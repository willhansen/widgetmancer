//! Visual debug tool for floating square rendering.
//!
//! Renders the same character picks the game uses
//! (`characters_for_full_square_with_2d_offset`, one world square = 2 terminal
//! columns) onto a checkerboard background with a dot marking each square
//! center, so sub-square offsets are easy to eyeball. All diagnostics
//! (snap family, snap error, coverage) describe the real render path via the
//! `#[doc(hidden)]` debug accessors in floating_square.rs and coverage.rs, so
//! the tool cannot drift from what the game draws and the coherence test
//! asserts.
//!
//! Modes:
//!   pos X Y       one square at world point (X, Y): frame with true-center
//!                 marker, snap-family diagnostics, and a sampled
//!                 actual-vs-ideal coverage view (the coherence test's oracle)
//!   families X Y  the same position rendered with each snap family forced,
//!                 side by side
//!   sweep         offset table over 0..=0.5 in 1/16 steps, each cell labeled
//!                 with the family that offset picks (a decision-boundary map)
//!   glyphs        reference table: every block character the renderer can
//!                 emit, each with an exact big-pixel zoom (8x24 pixels per
//!                 character, 1/16 x 1/24 world each) framed in box drawing
//!                 characters; plain text, so it can be redirected to a file
//!   animate (default)
//!                 square on the alternate screen (q quits); orbit,
//!                 arrow-key nudge, and line trajectories. A two-method
//!                 comparison: the in-use game path (family-snapped) and
//!                 a candidate replacement cycled with [ and ] (charwise;
//!                 charwise + protrusion, objective xor + 1.0·d; charwise
//!                 + protrusion², objective xor + 4.0·d² — progressive:
//!                 shallow overshoot nearly free, deep spikes hammered).
//!                 Each method row shows its large real-size grid (with
//!                 glyph legend and the error its own picker minimizes),
//!                 the zoomed render at native sampled resolution with
//!                 one palette color per glyph, and ONE error measurement
//!                 as a full-resolution colored pane with its numeric
//!                 value — cycled with , and . through center error
//!                 (silhouette + ideal outline + both centroids), area
//!                 error (signed: over red / under blue), per-char
//!                 coverage (half-cells heat-shaded by local error),
//!                 ideal square xor (any mismatch lit), jaggedness
//!                 (contour lit by local edge-step length), and
//!                 displacement sensitivity (what turns wrong under the
//!                 worst 1/16 nudge: bright yellow = newly wrong). Then a
//!                 common row with the ideal (true square) zoom, global
//!                 state, and controls. Left click/drag sets the orbit's
//!                 angular position (the angle from the top-row grid's
//!                 center to the mouse, at the fixed orbit radius); other
//!                 buttons place the square (drag to move it), and
//!                 holding shift/ctrl/alt while dragging (or pressing f)
//!                 switches placement to fine control, where large mouse
//!                 movements map to sub-cell square movements.
//!
//! Run via the top-level ./debug-floating-squares wrapper, or:
//!   cargo run -p terminal_rendering --bin floating_square_debug -- animate

use std::io::{stdin, stdout, IsTerminal, Write};
use std::sync::mpsc::channel;
use std::thread;
use std::time::Duration;

use rgb::RGB8;
use termion::event::{Event, Key, MouseButton, MouseEvent};
use termion::input::{MouseTerminal, TermReadEventsAndRaw};
use termion::raw::IntoRawMode;
use termion::screen::IntoAlternateScreen;

use terminal_rendering::coverage::{
    self, actual_sample, assign_colors, cell_bg, charwise_neighborhood,
    charwise_protrusion_squared_neighborhood, charwise_shaped_neighborhood, charwise_objective,
    coverage_error, displacement_sensitivity, fill_centroid, glyph_filled, glyph_pane,
    jaggedness, lerp,
    pane_from_colors, per_char_coverage_error, rendered_neighborhood, rendered_neighborhood_forced,
    ClassGrid, FillGrid, Metrics, BITMAP_W, PX_H, PX_W, CHARWISE_PROTRUSION_SQUARED_WEIGHT,
    CHARWISE_PROTRUSION_WEIGHT, DISPLACEMENT_DELTA,
};
use terminal_rendering::glyph_constants::named_colors::*;
use terminal_rendering::glyph_constants::SPACE;
use terminal_rendering::*;

const SQUARE_COLOR: RGB8 = RGB8::new(255, 165, 0); // orange
const CENTER_DOT_COLOR: RGB8 = RGB8::new(90, 90, 110);
const BG_DARK: RGB8 = RGB8::new(16, 16, 24);
const BG_LIGHT: RGB8 = RGB8::new(30, 30, 44);
const ORIGIN_COLOR: RGB8 = RGB8::new(0, 180, 180);
const TRUE_CENTER_COLOR: RGB8 = RGB8::new(120, 255, 255);

/// One display color per snap family, in SnapFamily::ALL / snap_family_names()
/// order (h-eighths, v-eighths, hextant, quadrant).
const FAMILY_COLORS: [RGB8; 4] = [
    RGB8::new(230, 230, 80),  // horizontal eighths: yellow
    RGB8::new(80, 210, 210),  // vertical eighths: cyan
    RGB8::new(100, 220, 100), // hextant: green
    RGB8::new(220, 120, 255), // quadrant: magenta
];
const FAMILY_LETTERS: [char; 4] = ['H', 'V', 'X', 'Q'];

fn family_index_of(name: &str) -> usize {
    snap_family_names().iter().position(|&n| n == name).unwrap()
}

/// "horizontal eighths (x: 1/16, y: row)" -> "horizontal eighths"
fn short_family_name(name: &str) -> &str {
    name.split(" (").next().unwrap()
}

/// Same checkerboard parity rule as Graphics::square_is_light.
fn bg_color_for(square: WorldSquare) -> RGB8 {
    if (square.x + square.y).rem_euclid(2) == 0 {
        BG_LIGHT
    } else {
        BG_DARK
    }
}

/// Blank frame covering `radius` squares around `origin_square`, with a dot at
/// each square center so sub-square offsets are visible against the grid.
fn grid_frame(radius: i32, origin_square: WorldSquare) -> Frame {
    let squares_wide = (2 * radius + 1) as usize;
    let mut frame = Frame::blank(squares_wide * 2, squares_wide);
    for dx in -radius..=radius {
        for dy in -radius..=radius {
            let square = euclid::point2(origin_square.x + dx, origin_square.y + dy);
            let bg = bg_color_for(square);
            let marker = if square == origin_square {
                ('+', ORIGIN_COLOR)
            } else {
                ('·', CENTER_DOT_COLOR)
            };
            let [row, wide_col] = frame_row_col(radius, origin_square, square);
            frame.set_by_double_wide_grid(
                row as usize,
                wide_col as usize,
                [
                    DrawableGlyph::new_colored(marker.0, marker.1, bg),
                    DrawableGlyph::new_colored(SPACE, BLACK, bg),
                ],
            );
        }
    }
    frame
}

/// Signed so off-grid squares (square near the animation grid's edge) can
/// be bounds-checked before casting; casting first would wrap to huge
/// values and overflow the width arithmetic in the check itself.
fn frame_row_col(radius: i32, origin_square: WorldSquare, square: WorldSquare) -> [i32; 2] {
    [
        radius - (square.y - origin_square.y),
        radius + (square.x - origin_square.x),
    ]
}

/// Mirrors OffsetSquareDrawable::drawables_for_floating_square_at_point:
/// only the 3x3 neighborhood of the rounded center square can be non-empty.
/// `forced_family` (index into snap_family_names()) overrides the automatic
/// family pick, for the `families` mode.
fn draw_floating_square(
    frame: &mut Frame,
    radius: i32,
    origin_square: WorldSquare,
    pos: WorldPoint,
    forced_family: Option<usize>,
) {
    let center = world_point_to_world_square(pos);
    for dx in -1..=1i32 {
        for dy in -1..=1i32 {
            let square = euclid::point2(center.x + dx, center.y + dy);
            let [row, wide_col] = frame_row_col(radius, origin_square, square);
            if row < 0
                || wide_col < 0
                || row as usize >= frame.height()
                || wide_col as usize * 2 + 1 >= frame.width()
            {
                continue;
            }
            let [row, wide_col] = [row as usize, wide_col as usize];
            let offset: WorldMove = pos - square.to_f32();
            let chars = match forced_family {
                Some(i) => characters_for_full_square_with_2d_offset_forced(offset, i),
                None => characters_for_full_square_with_2d_offset(offset),
            };
            if chars != [SPACE; 2] {
                let bg = bg_color_for(square);
                frame.set_by_double_wide_grid(
                    row,
                    wide_col,
                    chars.map(|c| DrawableGlyph::new_colored(c, SQUARE_COLOR, bg)),
                );
            }
        }
    }
}

fn pal(i: usize) -> RGB8 {
    let coverage::Rgb(r, g, b) = coverage::PALETTE[i % coverage::PALETTE.len()];
    RGB8::new(r, g, b)
}

/// Draws a precomputed glyph neighborhood with one palette color per
/// half-cell glyph (matching the zoomed panes and the legend), instead of
/// the uniform square color the diagnostic modes use.
fn draw_neighborhood_colored(
    frame: &mut Frame,
    radius: i32,
    origin_square: WorldSquare,
    center: WorldSquare,
    grid: &[[DoubleChar; 3]; 3],
    owners: &[[[Option<usize>; 2]; 3]; 3],
) {
    for dx in -1..=1i32 {
        for dy in -1..=1i32 {
            let square = euclid::point2(center.x + dx, center.y + dy);
            let [row, wide_col] = frame_row_col(radius, origin_square, square);
            if row < 0
                || wide_col < 0
                || row as usize >= frame.height()
                || wide_col as usize * 2 + 1 >= frame.width()
            {
                continue;
            }
            let chars = grid[(dx + 1) as usize][(dy + 1) as usize];
            if chars != [SPACE; 2] {
                let bg = bg_color_for(square);
                let glyph = |half: usize| {
                    let color = owners[(dx + 1) as usize][(dy + 1) as usize][half]
                        .map(pal)
                        .unwrap_or(SQUARE_COLOR);
                    DrawableGlyph::new_colored(chars[half], color, bg)
                };
                frame.set_by_double_wide_grid(
                    row as usize,
                    wide_col as usize,
                    [glyph(0), glyph(1)],
                );
            }
        }
    }
}

/// (text, visible width) legend mapping each rendered half-cell glyph to
/// its palette color, in assign_colors' scan order so legend colors match
/// the panes. A 1x1 square spans at most 8 half-cells, so the legend always
/// fits under the real-size grid.
fn glyph_legend(
    glyphs: &[[DoubleChar; 3]; 3],
    owners: &[[[Option<usize>; 2]; 3]; 3],
    style: &coverage::Style,
) -> (String, usize) {
    let mut legend = String::new();
    let mut w = 0usize;
    for dy in [1i32, 0, -1] {
        for dx in -1..=1i32 {
            for half in 0..2 {
                if let Some(idx) = owners[(dx + 1) as usize][(dy + 1) as usize][half] {
                    legend.push_str(&style.fg(coverage::PALETTE[idx % coverage::PALETTE.len()]));
                    legend.push(glyphs[(dx + 1) as usize][(dy + 1) as usize][half]);
                    legend.push(' ');
                    w += 2;
                }
            }
        }
    }
    legend.push_str(style.reset());
    (legend, w)
}

/// Strip ANSI CSI sequences (ESC [ ... final letter) to get a line's
/// visible width. Pane lines are full of color codes; the box borders need
/// printable widths to pad straight right edges.
fn visible_width(s: &str) -> usize {
    let mut w = 0;
    let mut chars = s.chars();
    while let Some(c) = chars.next() {
        if c == '\x1b' {
            for c in chars.by_ref() {
                if c.is_ascii_alphabetic() {
                    break;
                }
            }
        } else {
            w += 1;
        }
    }
    w
}

/// Visible width of a whole column of pre-styled lines.
fn visible_w(lines: &[String]) -> usize {
    lines.iter().map(|s| visible_width(s)).max().unwrap_or(0)
}

/// One titled, bordered row of side-by-side columns. Each column is its
/// lines plus its visible width; short columns are padded with spaces so
/// the right border stays a straight vertical line. Relies on the pane
/// lines being color-neutral at both ends (frame lines, bitmap panes, and
/// legends all reset their colors), so the padding and borders pick up no
/// dangling background.
fn boxed_row(title: &str, columns: &[(&[String], usize)]) -> Vec<String> {
    const GAP: usize = 2;
    let h = columns.iter().map(|c| c.0.len()).max().unwrap_or(0);
    let inner_w: usize =
        columns.iter().map(|c| c.1).sum::<usize>() + GAP * (columns.len() - 1) + 2;
    let label = format!("─ {title} ");
    let mut lines = vec![format!(
        "┌{label}{}┐",
        "─".repeat(inner_w - visible_width(&label))
    )];
    for row in 0..h {
        let mut line = String::from("│ ");
        for (i, (col, w)) in columns.iter().enumerate() {
            if i > 0 {
                line.push_str(&" ".repeat(GAP));
            }
            let cell = col.get(row).map(String::as_str).unwrap_or("");
            line.push_str(cell);
            line.push_str(&" ".repeat(w.saturating_sub(visible_width(cell))));
        }
        line.push_str(" │");
        lines.push(line);
    }
    lines.push(format!("└{}┘", "─".repeat(inner_w)));
    lines
}

/// One render method to compare: how to produce its glyph neighborhood.
type Neighborhood = fn(WorldPoint) -> ([[DoubleChar; 3]; 3], WorldSquare);

/// The game-facing render path — always the "in use" row.
const IN_USE: (&str, Neighborhood) = ("family-snapped", rendered_neighborhood);

/// Candidate replacements for the in-use method, cycled with [ and ].
const CANDIDATES: [(&str, Neighborhood); 3] = [
    ("charwise", charwise_neighborhood),
    ("charwise + protrusion", charwise_shaped_neighborhood),
    ("charwise + protrusion²", charwise_protrusion_squared_neighborhood),
];

/// The error measurements that can be cycled with , and . — one shown at a
/// time, as a full-resolution colored pane per method.
const METRICS: [&str; 6] = ["center", "area", "per-char", "xor", "jagged", "disp"];

/// The method's own objective, formatted for the stats column ("the error
/// used for rendering"). `index` 0 = in use, 1..=3 = CANDIDATES index + 1.
fn objective_lines(
    index: usize,
    glyphs: &[[DoubleChar; 3]; 3],
    owners: &[[[Option<usize>; 2]; 3]; 3],
    center: WorldSquare,
    pos: WorldPoint,
) -> Vec<String> {
    match index {
        // the family map is baked against the sampled ideal-square xor
        0 => vec![format!(
            "bake objective (xor)={:.3}",
            coverage_error(glyphs, owners, center, pos)
        )],
        1 => vec![format!("Σ cell xor={:.3}", charwise_objective(pos, 0.0, false))],
        2 => vec![format!(
            "xor+{:.2}·Σd={:.3}",
            CHARWISE_PROTRUSION_WEIGHT,
            charwise_objective(pos, CHARWISE_PROTRUSION_WEIGHT, false)
        )],
        _ => vec![format!(
            "xor+{:.2}·Σd²={:.3}",
            CHARWISE_PROTRUSION_SQUARED_WEIGHT,
            charwise_objective(pos, CHARWISE_PROTRUSION_SQUARED_WEIGHT, true)
        )],
    }
}

/// Arrow for the displacement metric's worst direction.
fn dir_arrow(d: WorldMove) -> char {
    if d.x > 0.0 {
        '→'
    } else if d.x < 0.0 {
        '←'
    } else if d.y > 0.0 {
        '↑'
    } else {
        '↓'
    }
}

/// One method's bordered section: large view (full animation grid, method
/// info, its own objective), zoomed render at native sampled resolution
/// with one palette color per glyph plus legend, and the currently selected
/// error measurement as a full-resolution colored pane with its value.
fn method_section(
    title: &str,
    nb: Neighborhood,
    objective_idx: usize,
    pos: WorldPoint,
    style: &coverage::Style,
    metric: usize,
    extra_info: &[String],
) -> Vec<String> {
    let (glyphs, center) = nb(pos);
    let owners = assign_colors(&glyphs);
    let sample_origin = euclid::point2(center.x as f32 - 1.5, center.y as f32 - 1.5);
    let actual = FillGrid::sample(sample_origin, |wx, wy| {
        actual_sample(&glyphs, &owners, center, wx, wy)
    });
    let class = ClassGrid::sample(sample_origin, |wx, wy| {
        ClassGrid::class_at(&glyphs, &owners, center, pos, wx, wy)
    });

    // zoomed render: native sampled pixel grid drawn from the glyphs'
    // exact geometry (see glyph_pane), one palette color per glyph
    let mut zoom_col: Vec<String> = glyph_pane(&glyphs, &owners, center, &coverage::PALETTE, style);
    let legend = glyph_legend(&glyphs, &owners, style);
    if legend.1 > 0 {
        zoom_col.push("glyph colors:".to_string());
        zoom_col.push(legend.0);
    }

    // large view: full animation grid, method info, then its objective —
    // the error the method's own picker minimizes
    let origin0 = euclid::point2(0, 0);
    let mut large = grid_frame(ANIMATE_GRID_RADIUS, origin0);
    draw_neighborhood_colored(&mut large, ANIMATE_GRID_RADIUS, origin0, center, &glyphs, &owners);
    let mut large_col: Vec<String> = large
        .string_for_regular_display()
        .lines()
        .map(String::from)
        .collect();
    large_col.extend(extra_info.iter().cloned());
    large_col.extend(objective_lines(objective_idx, &glyphs, &owners, center, pos));

    // the selected error measurement: full-res pane + numeric value
    let (pane, value): (Vec<String>, String) = match metric {
        0 => {
            let v = match fill_centroid(&actual) {
                Some(c) => format!("({:+.2}, {:+.2})", c.x - pos.x, c.y - pos.y),
                None => "n/a".to_string(),
            };
            (class.center_pane(&actual, pos, style), v)
        }
        1 => (
            class.signed_pane(style),
            format!("{:+.3}", class.signed_area_error()),
        ),
        2 => (
            ClassGrid::per_char_heat_pane(&glyphs, center, pos, style),
            format!("{:.3}", per_char_coverage_error(&glyphs, center, pos)),
        ),
        3 => (class.mismatch_pane(style), format!("{:.3}", class.xor_error())),
        4 => (
            ClassGrid::jaggedness_pane(&actual, style),
            format!("{:.2}", jaggedness(&actual)),
        ),
        _ => {
            let (gain, dir) = displacement_sensitivity(nb, pos, DISPLACEMENT_DELTA);
            let shifted_pos = pos + dir * DISPLACEMENT_DELTA;
            let (glyphs2, center2) = nb(shifted_pos);
            let owners2 = assign_colors(&glyphs2);
            let shifted = ClassGrid::sample(sample_origin, |wx, wy| {
                ClassGrid::class_at(&glyphs2, &owners2, center2, shifted_pos, wx, wy)
            });
            (
                ClassGrid::displacement_pane(&class, &shifted, style),
                format!("{:.3}{}", gain, dir_arrow(dir)),
            )
        }
    };
    let mut err_col: Vec<String> = vec![format!(
        "{:^BITMAP_W$}",
        format!("{} (, .)", METRICS[metric])
    )];
    err_col.extend(pane);
    err_col.push(format!("{:^BITMAP_W$}", value));

    let (large_w, zoom_w, err_w) = (
        visible_w(&large_col),
        visible_w(&zoom_col),
        visible_w(&err_col),
    );
    let cols = [
        (large_col, large_w),
        (zoom_col, zoom_w),
        (err_col, err_w),
    ];
    let refs: Vec<(&[String], usize)> =
        cols.iter().map(|(l, w)| (l.as_slice(), *w)).collect();
    boxed_row(title, &refs)
}

/// Marks the exact square center on the half-cell grid so the snapped-vs-true
/// offset (reported numerically in the text output) is also visible. Honest
/// to half a half-cell (1/4 world unit) of quantization; overwrites whatever
/// glyph the square put in that half-cell.
fn overlay_true_center(
    frame: &mut Frame,
    radius: i32,
    origin_square: WorldSquare,
    pos: WorldPoint,
) {
    // frame col 0 is the left half-cell edge of the leftmost square, i.e.
    // world x = origin.x - radius - 0.5; row 0 is the top edge of the
    // topmost square, world y = origin.y + radius + 0.5
    let col = ((pos.x - origin_square.x as f32 + radius as f32 + 0.5) * 2.0).floor() as i32;
    let row = (origin_square.y as f32 + radius as f32 + 0.5 - pos.y).floor() as i32;
    if row < 0 || col < 0 || row as usize >= frame.height() || col as usize >= frame.width() {
        return;
    }
    let bg = frame.grid[row as usize][col as usize].bg_color;
    frame.grid[row as usize][col as usize] = DrawableGlyph::new('+', Some(TRUE_CENTER_COLOR), bg);
}

/// The true square drawn analytically: each display pixel is exactly
/// 1/8 x 1/8 world units, so per-pixel ideal-coverage area is closed-form.
/// Shading by coverage shows the sub-pixel edge phase; the sampled
/// majority vote (whose 3-of-6 threshold produces corner divots at
/// fractional edge positions — an edge pixel holds 1 of 2 sample columns,
/// a corner pixel also only ≤2 of 3 rows) is what the metrics see, not
/// what the reference should look like.
fn ideal_pane(pos: WorldPoint, origin: WorldPoint, style: &coverage::Style) -> Vec<String> {
    let mut colors = vec![vec![None; PX_W]; PX_H];
    for py in 0..PX_H {
        for px in 0..PX_W {
            let x0 = origin.x + px as f32 / 8.0;
            let y1 = origin.y + 3.0 - py as f32 / 8.0;
            let ov_x = (x0 + 0.125).min(pos.x + 0.5) - x0.max(pos.x - 0.5);
            let ov_y = y1.min(pos.y + 0.5) - (y1 - 0.125).max(pos.y - 0.5);
            let frac = (ov_x.max(0.0) * ov_y.max(0.0)) * 64.0; // / (1/8)^2
            if frac > 0.0 {
                let bg = cell_bg(px / 4, py / 8);
                colors[py][px] = Some(lerp(bg, coverage::IDEAL_COLOR, frac));
            }
        }
    }
    pane_from_colors(style, &colors)
}

/// Sampled actual-vs-ideal coverage, using the same oracle the coherence
/// test asserts on (tests/floating_square_coherence.rs).
fn coverage_zoom_pane(pos: WorldPoint) -> String {
    let (grid, center) = rendered_neighborhood(pos);
    let owners = assign_colors(&grid);
    let origin = euclid::point2(center.x as f32 - 1.5, center.y as f32 - 1.5);
    let actual = FillGrid::sample(origin, |wx, wy| actual_sample(&grid, &owners, center, wx, wy));
    let style = coverage::Style::from_env();
    let actual_lines = glyph_pane(&grid, &owners, center, &coverage::PALETTE, &style);
    let ideal_lines = ideal_pane(pos, origin, &style);
    let mut out = format!(
        "actual: exact glyph geometry; ideal: analytic (1 text cell = 2 samples; background checkerboard = character cells):\n  {:BITMAP_W$}  {}\n",
        "actual", "ideal (true square)"
    );
    for row in 0..actual_lines.len() {
        out.push_str(&format!("  {}  {}\n", actual_lines[row], ideal_lines[row]));
    }
    out.push_str(&format!("  {}\n", Metrics::measure(&actual, pos).summary_line()));
    out
}

/// The offset the renderer's family decision actually sees: pos relative to
/// the rounded center square.
fn center_offset(pos: WorldPoint) -> WorldMove {
    pos - world_point_to_world_square(pos).to_f32()
}

fn print_family_diagnostics(pos: WorldPoint) {
    let offset = center_offset(pos);
    let info = snap_debug_info(offset);
    println!(
        "family: {}   snapped offset ({:+.4}, {:+.4})   snap err ({:+.4}, {:+.4})",
        info.family,
        info.snapped_offset.x,
        info.snapped_offset.y,
        info.snapped_offset.x - offset.x,
        info.snapped_offset.y - offset.y,
    );
    // candidates ranked by measured coverage error (the family map's
    // objective), not the center-snap proxy it used to report
    for (i, name) in snap_family_names().iter().enumerate() {
        let (grid, center) = rendered_neighborhood_forced(pos, i);
        let owners = assign_colors(&grid);
        let err = coverage_error(&grid, &owners, center, pos);
        let mark = if *name == info.family { '>' } else { ' ' };
        println!("  {mark} {name:<38} coverage err {err:.4}");
    }
}

fn show_position(pos: WorldPoint) {
    let frac = fraction_part(pos);
    println!(
        "pos=({:.3}, {:.3})  frac=({:+.3}, {:+.3})",
        pos.x, pos.y, frac.x, frac.y,
    );
    print_family_diagnostics(pos);
    let center = world_point_to_world_square(pos);
    let mut frame = grid_frame(3, center);
    draw_floating_square(&mut frame, 3, center, pos, None);
    overlay_true_center(&mut frame, 3, center, pos);
    println!("{frame}{}", Glyph::reset_colors());
    print!("{}", coverage_zoom_pane(pos));
}

/// The same position rendered with each snap family forced, side by side.
/// Explains the automatic pick: the winner is the family whose snapped
/// silhouette sits closest to the true square.
fn show_families(pos: WorldPoint) {
    let frac = fraction_part(pos);
    let info = snap_debug_info(center_offset(pos));
    println!(
        "pos=({:.3}, {:.3})  frac=({:+.3}, {:+.3})  auto-picked family: {}",
        pos.x, pos.y, frac.x, frac.y, info.family,
    );
    let center = world_point_to_world_square(pos);
    let names = snap_family_names();
    let err_of = |name: &str| {
        info.candidates
            .iter()
            .find(|(n, _)| *n == name)
            .map(|(_, e)| *e)
            .unwrap()
    };
    let radius = 2i32;
    let pane_w = ((2 * radius + 1) * 2) as usize; // 5 squares = 10 cols
    let header: Vec<String> = names
        .iter()
        .map(|n| {
            let mark = if *n == info.family { '>' } else { ' ' };
            format!("{mark}{:^w$}", short_family_name(n), w = pane_w - 1)
        })
        .collect();
    println!("{}", header.join("  "));
    let err_line: Vec<String> = names
        .iter()
        .map(|n| format!("{:^w$.4}", err_of(n), w = pane_w))
        .collect();
    println!("{}", err_line.join("  "));
    let pane_lines: Vec<Vec<String>> = (0..4)
        .map(|i| {
            let mut f = grid_frame(radius, center);
            draw_floating_square(&mut f, radius, center, pos, Some(i));
            overlay_true_center(&mut f, radius, center, pos);
            f.string_for_regular_display()
                .lines()
                .map(String::from)
                .collect()
        })
        .collect();
    for row in 0..pane_lines[0].len() {
        let line: Vec<&str> = pane_lines.iter().map(|l| l[row].as_str()).collect();
        println!("{}", line.join("  "));
    }
    println!("{}", Glyph::reset_colors());
}

/// Offset table over 0..=0.5 in 1/16 steps (the finest snap grid), each cell
/// labeled with the family that offset picks — a map of the family decision
/// boundaries. Only the positive quadrant is shown: every snap grid is
/// sign-symmetric, so the other quadrants are mirror images.
fn show_sweep() {
    let offsets: Vec<f32> = (0..=8).map(|i| i as f32 / 16.0).collect();
    let n = offsets.len();
    // cell = 3x3 squares (6 cols x 3 rows) + 1-col gap + 1 family-letter row
    let (cell_h, step_x, step_y) = (3usize, 7usize, 4usize);
    // width drops the trailing gap column; height keeps the last cell's letter row
    let mut big = Frame::blank(n * step_x - 1, n * step_y);
    for (yi, &y_off) in offsets.iter().enumerate() {
        for (xi, &x_off) in offsets.iter().enumerate() {
            let pos: WorldPoint = euclid::point2(x_off, y_off);
            let mut cell = grid_frame(1, euclid::point2(0, 0));
            draw_floating_square(&mut cell, 1, euclid::point2(0, 0), pos, None);
            let row0 = yi * step_y;
            let col0 = xi * step_x;
            big.blit(&cell, [row0 as i32, col0 as i32]);
            let idx = family_index_of(snap_debug_info(center_offset(pos)).family);
            big.grid[row0 + cell_h][col0 + 2] =
                DrawableGlyph::new(FAMILY_LETTERS[idx], Some(FAMILY_COLORS[idx]), None);
        }
    }
    let header: String = offsets
        .iter()
        .map(|x| format!("x={x:.2} "))
        .collect();
    println!("        {header}");
    for (row, line) in big.string_for_regular_display().lines().enumerate() {
        // both prefixes are exactly 8 chars so labeled and unlabeled rows align
        if row % step_y == 1 {
            println!("y={:<5.3} {line}", offsets[row / step_y]);
        } else {
            println!("        {line}");
        }
    }
    print!("{}", Glyph::reset_colors());
    println!("\nfamilies (auto-picked per offset):");
    for (i, name) in snap_family_names().iter().enumerate() {
        println!(
            "  {}{} = {}{}",
            termion::color::Fg(termion::color::Rgb(
                FAMILY_COLORS[i].r,
                FAMILY_COLORS[i].g,
                FAMILY_COLORS[i].b
            )),
            FAMILY_LETTERS[i],
            name,
            termion::style::Reset
        );
    }
}

/// Big-pixel grid per character cell in the glyph table: the exact union
/// lattice of the families' increments — 1/16 world horizontally (1/8-char
/// vertical strips), 1/24 world vertically (eighths/thirds union grid) —
/// so one character (0.5 x 1.0 world) is 8 x 24 big pixels and every glyph
/// edge lands exactly on a pixel boundary. No rounding anywhere.
const TABLE_PX_W: usize = 8;
const TABLE_PX_H: usize = 24;

/// Every block character the renderer can emit, by sweeping the four
/// family generators over their full input domains (deduped, SPACE
/// dropped). Calling the real generators means the list cannot drift
/// from the render vocabulary.
fn used_block_glyphs() -> Vec<char> {
    let mut glyphs: Vec<char> = Vec::new();
    let mut push = |c: char| {
        if c != SPACE && !glyphs.contains(&c) {
            glyphs.push(c);
        }
    };
    for &vertical in &[false, true] {
        for eighths in -8..=8 {
            push(character_for_half_square_with_1d_eighths_offset(vertical, eighths));
        }
    }
    for thirds in -3..=3 {
        push(character_for_half_square_with_vertical_thirds_offset(thirds));
    }
    for dy in -2..=2 {
        for dx in -2..=2 {
            push(quadrant_block_by_offset(euclid::vec2(dx, dy)));
        }
    }
    for dy in -3..=3 {
        for dx in -2..=2 {
            push(hextant_block_by_offset(euclid::vec2(dx, dy)));
        }
    }
    glyphs
}

/// The glyph table: one entry per block character (first column), its
/// exact 8x24 big-pixel zoom framed in box drawing characters (second
/// column), so each glyph's cell boundary is explicit. One big pixel =
/// one vertical half character: both=█ upper=▀ lower=▄ empty=·. Plain
/// text only (no ANSI) — the output is meant for a file (`glyphs > x.txt`).
fn print_glyph_table() {
    println!("block glyph reference - exact big-pixel zoom at the union lattice");
    println!("one character = one half-cell = 0.5 world wide x 1.0 world tall;");
    println!("one big pixel = 1/16 world wide x 1/24 world tall (the finest");
    println!("increments any snap family can express), so a character is 8x24");
    println!("big pixels and every glyph edge lies exactly on a pixel boundary.");
    println!("one big pixel = one vertical half character: both=█ upper=▀ lower=▄ empty=·");
    for &c in &used_block_glyphs() {
        println!();
        println!("{c} ┌{}┐", "─".repeat(TABLE_PX_W));
        for t in 0..TABLE_PX_H / 2 {
            // text row t stacks world pixel rows 23-2t (upper) and
            // 22-2t (lower), counted from the bottom (+y is up)
            let filled = |j: usize, i: usize| {
                glyph_filled(
                    c,
                    (i as f32 + 0.5) / TABLE_PX_W as f32,
                    (j as f32 + 0.5) / TABLE_PX_H as f32,
                )
            };
            let mut line = String::from("  │");
            for i in 0..TABLE_PX_W {
                let up = filled(TABLE_PX_H - 1 - 2 * t, i);
                let lo = filled(TABLE_PX_H - 2 - 2 * t, i);
                line.push(match (up, lo) {
                    (true, true) => '█',
                    (true, false) => '▀',
                    (false, true) => '▄',
                    (false, false) => '·',
                });
            }
            line.push('│');
            println!("{line}");
        }
        println!("  └{}┘", "─".repeat(TABLE_PX_W));
    }
}

const ORBIT_RADIUS: f32 = 2.5;
/// Radians per second. Matches the old 0.02 rad per 33ms frame.
const ORBIT_SPEED: f32 = 0.6;
/// Nudge step for the arrow keys: the finest snap grid (h-eighths x step),
/// so every nudge can cross at most one snap boundary.
const NUDGE: f32 = 1.0 / 16.0;
/// Per-second velocity of the line trajectory: 10x the coherence test's
/// (0.06, 0.03) per-sample step so motion is visible in real time.
const LINE_DIR: WorldMove = WorldMove::new(0.6, 0.3);
/// Half-width of the animation grid in squares; fixed so mouse cells can be
/// mapped back to world geometry.
const ANIMATE_GRID_RADIUS: i32 = 4;
/// Fine mouse-drag scale: world units per terminal cell of mouse travel,
/// vs. coarse mode's direct cell-to-grid mapping (0.5 in x, 1 in y). Large
/// mouse sweeps produce sub-cell square movements, which is what the
/// terminal grid's resolution otherwise forbids.
const FINE_SCALE: f32 = 1.0 / 32.0;

enum Motion {
    Orbit { theta: f32 },
    /// Parked at a spot (arrow-key nudge or mouse placement).
    Free { pos: WorldPoint },
    /// Straight line through `base`, t in [-2, 2], wrapping.
    Line { base: WorldPoint, dir: WorldMove, t: f32 },
}

impl Motion {
    fn pos(&self) -> WorldPoint {
        match *self {
            Motion::Orbit { theta } => {
                euclid::point2(theta.cos() * ORBIT_RADIUS, theta.sin() * ORBIT_RADIUS)
            }
            Motion::Free { pos } => pos,
            Motion::Line { base, dir, t } => base + dir * t,
        }
    }
    fn advance(&mut self, dt: f32, speed: f32) {
        match self {
            Motion::Orbit { theta } => {
                *theta = (*theta + ORBIT_SPEED * speed * dt).rem_euclid(std::f32::consts::TAU)
            }
            Motion::Free { .. } => {}
            Motion::Line { t, .. } => *t = (*t + speed * dt + 2.0).rem_euclid(4.0) - 2.0,
        }
    }
    fn name(&self) -> &'static str {
        match self {
            Motion::Orbit { .. } => "orbit",
            Motion::Free { .. } => "free",
            Motion::Line { .. } => "line",
        }
    }
}

/// What an in-progress mouse drag does. Hold events don't report which
/// button is down, so the press that starts the drag picks the mode once
/// and Holds stick with it for the whole drag.
enum DragMode {
    /// Left button: steer the orbit's angular position.
    Angle,
    /// Any other button (or left with a modifier held): place the square.
    Place,
}

struct AnimState {
    motion: Motion,
    paused: bool,
    speed: f32,
    anim_time: Duration,
    /// Family changes since start; each one is a potential visible pop.
    switches: u32,
    prev_family: Option<&'static str>,
    /// f-key fallback for terminals that don't report mouse modifiers:
    /// treats every drag as a fine drag.
    fine_drag: bool,
    /// Anchor for fine drags, which accumulate cell deltas relative to the
    /// previous event instead of mapping cells to absolute positions.
    last_mouse_cell: Option<(u16, u16)>,
    /// Mode of the drag in progress (None between drags).
    drag: Option<DragMode>,
    /// Candidate replacement method shown in the second row (index into
    /// CANDIDATES); the first row is always the in-use game path.
    candidate: usize,
    /// Which error measurement pane is displayed (index into METRICS).
    metric: usize,
}

impl AnimState {
    fn new() -> Self {
        AnimState {
            motion: Motion::Orbit { theta: 0.0 },
            paused: false,
            speed: 1.0,
            anim_time: Duration::ZERO,
            switches: 0,
            prev_family: None,
            fine_drag: false,
            last_mouse_cell: None,
            drag: None,
            candidate: 0,
            metric: 0,
        }
    }
}

/// Decode an SGR mouse sequence (ESC [ < Cb ; Cx ; Cy M/m) from raw input
/// bytes, recovering the modifier bits termion drops (shift=4, alt=8,
/// ctrl=16 — modified events otherwise surface as Event::Unsupported or
/// lose their modifier). Returns the event and whether any modifier was
/// held.
fn parse_sgr_mouse(raw: &[u8]) -> Option<(MouseEvent, bool)> {
    let body = raw.strip_prefix(b"\x1b[<")?;
    let (&final_byte, nums) = body.split_last()?;
    if final_byte != b'M' && final_byte != b'm' {
        return None;
    }
    let text = std::str::from_utf8(nums).ok()?;
    let mut fields = text.split(';');
    let cb: u16 = fields.next()?.parse().ok()?;
    let cx: u16 = fields.next()?.parse().ok()?;
    let cy: u16 = fields.next()?.parse().ok()?;
    let modified = cb & (4 | 8 | 16) != 0;
    let button = match cb & 3 {
        0 => MouseButton::Left,
        1 => MouseButton::Middle,
        _ => MouseButton::Right,
    };
    let event = if cb & 64 != 0 {
        // wheel events come only as presses
        if final_byte != b'M' {
            return None;
        }
        let button = if cb & 1 == 0 {
            MouseButton::WheelUp
        } else {
            MouseButton::WheelDown
        };
        MouseEvent::Press(button, cx, cy)
    } else if cb & 32 != 0 {
        MouseEvent::Hold(cx, cy)
    } else if final_byte == b'm' || cb & 3 == 3 {
        MouseEvent::Release(cx, cy)
    } else {
        MouseEvent::Press(button, cx, cy)
    };
    Some((event, modified))
}

/// 1-based terminal cell of the animation grid's top-left corner. The
/// large view is the second column of the first boxed row (first column is
/// the 6-wide small view, then the 2-cell gap, then "│ " prefix): 2 + 6 + 2.
/// If the boxed layout changes, this moves with it.
const GRID_SCREEN_ORIGIN: (u16, u16) = (10, 2);

/// World point under the (1-based) terminal cell, using the same grid
/// geometry as the frame: 2 columns per square, rows increase downward.
fn mouse_cell_point(col: u16, row: u16) -> WorldPoint {
    let r = ANIMATE_GRID_RADIUS as f32;
    // grid cell (0,0) sits at GRID_SCREEN_ORIGIN; within the grid the
    // origin square spans cols 2r..=2r+1 (center 2r+0.5), row r
    let dx_cells = col as f32 - GRID_SCREEN_ORIGIN.0 as f32 - (2.0 * r + 0.5);
    let dy_cells = row as f32 - GRID_SCREEN_ORIGIN.1 as f32 - r;
    euclid::point2(dx_cells * 0.5, -dy_cells)
}

/// Bearing of the mouse cell from the animation grid's center (the world
/// origin), for steering the orbit with the left button: clicking a
/// direction from the center moves the orbiting square to that angle,
/// keeping the orbit radius. Unlike placement this needs no clamping —
/// every cell has a well-defined angle.
fn mouse_cell_angle(col: u16, row: u16) -> f32 {
    let p = mouse_cell_point(col, row);
    p.y.atan2(p.x)
}

/// `raw_mode`: true when writing to a termion raw-mode terminal. Raw mode
/// disables ONLCR, so bare '\n' would stair-step the frame.
const FRAME_DT: Duration = Duration::from_millis(33);

fn render_animation_frame(out: &mut impl Write, state: &mut AnimState, raw_mode: bool) {
    let pos = state.motion.pos();
    let offset = center_offset(pos);
    let info = snap_debug_info(offset);
    let family_changed = state.prev_family.is_some_and(|p| p != info.family);
    if state.prev_family != Some(info.family) {
        if state.prev_family.is_some() {
            state.switches += 1;
        }
        state.prev_family = Some(info.family);
    }

    // invert the family on the frame it changed: family switches are where
    // the visible pops happen
    let family_display = if family_changed {
        format!(
            "{}{}{}",
            termion::style::Invert,
            short_family_name(info.family),
            termion::style::NoInvert
        )
    } else {
        short_family_name(info.family).to_string()
    };
    let style = coverage::Style::from_env();
    let in_use_info = vec![
        format!("family={family_display}"),
        format!(
            "snap err=({:+.3}, {:+.3})",
            info.snapped_offset.x - offset.x,
            info.snapped_offset.y - offset.y,
        ),
    ];
    let (cand_name, cand_nb) = CANDIDATES[state.candidate];
    let cand_info = match state.candidate {
        0 => vec![
            "per-character xor argmin,".to_string(),
            "no sibling awareness".to_string(),
        ],
        1 => vec![format!(
            "per-cell xor + {:.2}·d",
            CHARWISE_PROTRUSION_WEIGHT
        )],
        _ => vec![format!(
            "per-cell xor + {:.2}·d²",
            CHARWISE_PROTRUSION_SQUARED_WEIGHT
        )],
    };

    let mut text = String::new();
    for line in method_section(
        &format!("in use: {}", IN_USE.0),
        IN_USE.1,
        0,
        pos,
        &style,
        state.metric,
        &in_use_info,
    ) {
        text.push_str(&line);
        text.push('\n');
    }
    for line in method_section(
        &format!("candidate: {cand_name}  ([ ] cycle)"),
        cand_nb,
        state.candidate + 1,
        pos,
        &style,
        state.metric,
        &cand_info,
    ) {
        text.push_str(&line);
        text.push('\n');
    }

    // common box: ideal zoom (drawn analytically — see ideal_pane), global
    // state, controls — one column each
    let center = world_point_to_world_square(pos);
    let sample_origin = euclid::point2(center.x as f32 - 1.5, center.y as f32 - 1.5);
    let ideal_lines = ideal_pane(pos, sample_origin, &style);
    let frac = fraction_part(pos);

    let mut common_col = vec!["ideal (true square)".to_string()];
    common_col.extend(ideal_lines.iter().cloned());
    let global = vec![
        format!(
            "pos=({:6.3}, {:6.3})  frac=({:+.3}, {:+.3})",
            pos.x, pos.y, frac.x, frac.y,
        ),
        format!(
            "{} speed={:.2}x  switches={}  t={:.1}s{}{}",
            state.motion.name(),
            state.speed,
            state.switches,
            state.anim_time.as_secs_f32(),
            if state.paused { "  [paused]" } else { "" },
            if state.fine_drag { "  [fine-drag]" } else { "" },
        ),
    ];
    let controls: Vec<String> = [
        "controls:",
        "q/esc quit    space pause",
        "arrows nudge 1/16",
        "o orbit  l line",
        "+/- speed  f fine-drag",
        "[ ] candidate method",
        ", . error metric",
        "left drag: orbit angle",
        "mid/right drag: place",
        "shift/ctrl/alt-drag fine",
    ]
    .iter()
    .map(|s| s.to_string())
    .collect();
    let global_w = visible_w(&global);
    let controls_w = visible_w(&controls);
    let common_row = boxed_row(
        "common",
        &[
            (common_col.as_slice(), BITMAP_W),
            (global.as_slice(), global_w),
            (controls.as_slice(), controls_w),
        ],
    );
    text.push_str(&common_row.join("\n"));
    text.push('\n');

    if raw_mode {
        // raw mode disables ONLCR, so bare '\n' would stair-step; and erase
        // to end-of-line per line so shrinking values leave no leftovers.
        // UntilNewline clears from the cursor to end of line; CurrentLine
        // would wipe the line content just written.
        let eol = format!("{}\r\n", termion::clear::UntilNewline);
        write!(out, "{}{}", text.replace('\n', &eol), Glyph::reset_colors()).unwrap();
    } else {
        write!(out, "{}{}", text, Glyph::reset_colors()).unwrap();
    }
    out.flush().unwrap();
}

/// `frame_count`: None = run until quit (interactive) or a short default
/// (piped output).
fn run_animation(frame_count: Option<u32>) {
    let interactive = stdout().is_terminal();

    if !interactive {
        // Piped output (e.g. `animate 5 | less -R`): no raw mode available.
        // These frames render instantly, so simulate the animation clock.
        let mut state = AnimState::new();
        for _ in 0..frame_count.unwrap_or(8) {
            render_animation_frame(&mut stdout(), &mut state, false);
            println!();
            state.motion.advance(FRAME_DT.as_secs_f32(), state.speed);
            state.anim_time += FRAME_DT;
        }
        return;
    }

    let (tx, rx) = channel();
    thread::spawn(move || {
        for event in stdin().events_and_raw() {
            if tx.send(event.unwrap()).is_err() {
                break;
            }
        }
    });

    let mut screen = MouseTerminal::from(stdout().into_raw_mode().unwrap())
        .into_alternate_screen()
        .unwrap();

    let mut state = AnimState::new();
    let mut frames = 0u32;
    let mut dirty = true;
    loop {
        while let Ok((event, raw)) = rx.try_recv() {
            // recover the mouse modifier bits termion drops, so a held
            // shift/ctrl/alt can select fine dragging
            let (event, fine_mod) = match parse_sgr_mouse(&raw) {
                Some((mouse_event, modified)) => (Event::Mouse(mouse_event), modified),
                None => (event, false),
            };
            let fine = fine_mod || state.fine_drag;
            match event {
                Event::Key(Key::Char('q')) | Event::Key(Key::Esc) => return,
                Event::Key(Key::Char(' ')) => {
                    state.paused = !state.paused;
                    dirty = true;
                }
                // Arrow keys park the square and step it across snap
                // boundaries deterministically — an orbit sweeps past the
                // interesting crossings before you can see them.
                Event::Key(Key::Left) | Event::Key(Key::Right) | Event::Key(Key::Up)
                | Event::Key(Key::Down) => {
                    let step = match event {
                        Event::Key(Key::Left) => euclid::vec2(-NUDGE, 0.0),
                        Event::Key(Key::Right) => euclid::vec2(NUDGE, 0.0),
                        Event::Key(Key::Up) => euclid::vec2(0.0, NUDGE),
                        _ => euclid::vec2(0.0, -NUDGE),
                    };
                    state.motion = Motion::Free {
                        pos: state.motion.pos() + step,
                    };
                    dirty = true;
                }
                Event::Key(Key::Char('o')) => {
                    let p = state.motion.pos();
                    state.motion = Motion::Orbit {
                        theta: p.y.atan2(p.x),
                    };
                    state.paused = false;
                    dirty = true;
                }
                Event::Key(Key::Char('l')) => {
                    state.motion = Motion::Line {
                        base: state.motion.pos(),
                        dir: LINE_DIR,
                        t: 0.0,
                    };
                    state.paused = false;
                    dirty = true;
                }
                Event::Key(Key::Char('+') | Key::Char('=')) => {
                    state.speed = (state.speed * 2.0).min(8.0);
                    dirty = true;
                }
                Event::Key(Key::Char('-')) => {
                    state.speed = (state.speed / 2.0).max(0.125);
                    dirty = true;
                }
                // fallback fine-drag toggle for terminals that don't pass
                // mouse modifiers through
                Event::Key(Key::Char('f')) => {
                    state.fine_drag = !state.fine_drag;
                    dirty = true;
                }
                // two-button cycles: candidate method (second row) and
                // which error measurement pane is displayed
                Event::Key(Key::Char(']')) => {
                    state.candidate = (state.candidate + 1) % CANDIDATES.len();
                    dirty = true;
                }
                Event::Key(Key::Char('[')) => {
                    state.candidate = (state.candidate + CANDIDATES.len() - 1) % CANDIDATES.len();
                    dirty = true;
                }
                Event::Key(Key::Char('.')) => {
                    state.metric = (state.metric + 1) % METRICS.len();
                    dirty = true;
                }
                Event::Key(Key::Char(',')) => {
                    state.metric = (state.metric + METRICS.len() - 1) % METRICS.len();
                    dirty = true;
                }
                // Left click/drag steers the orbit: the angle from the
                // grid center to the mouse becomes the orbit's angular
                // position (orbit radius unchanged, so the square jumps to
                // that bearing). Other buttons place the square (drag to
                // move it), clamped to the visible grid; with a modifier
                // held (or fine_drag toggled) placement is relative
                // instead: cell deltas accumulate at FINE_SCALE, so large
                // mouse movements produce sub-cell square movements.
                // Press pauses so the placement sticks.
                Event::Mouse(MouseEvent::Press(_, x, y))
                | Event::Mouse(MouseEvent::Hold(x, y)) => {
                    if let Event::Mouse(MouseEvent::Press(button, _, _)) = &event {
                        state.drag = Some(if *button == MouseButton::Left && !fine {
                            DragMode::Angle
                        } else {
                            DragMode::Place
                        });
                    }
                    let limit = ANIMATE_GRID_RADIUS as f32 + 0.5;
                    let clamp = |p: WorldPoint| {
                        euclid::point2(p.x.clamp(-limit, limit), p.y.clamp(-limit, limit))
                    };
                    match state.drag {
                        Some(DragMode::Angle) => {
                            state.motion = Motion::Orbit {
                                theta: mouse_cell_angle(x, y),
                            };
                        }
                        _ => {
                            if fine {
                                if let Some((lx, ly)) = state.last_mouse_cell {
                                    let d: WorldMove = euclid::vec2(
                                        (x as f32 - lx as f32) * FINE_SCALE,
                                        (ly as f32 - y as f32) * FINE_SCALE,
                                    );
                                    state.motion = Motion::Free {
                                        pos: clamp(state.motion.pos() + d),
                                    };
                                }
                            } else {
                                state.motion = Motion::Free {
                                    pos: clamp(mouse_cell_point(x, y)),
                                };
                            }
                            // anchored on every event, so grabbing or
                            // releasing the modifier mid-drag never causes
                            // a jump
                            state.last_mouse_cell = Some((x, y));
                        }
                    }
                    if matches!(event, Event::Mouse(MouseEvent::Press(_, _, _))) {
                        state.paused = true;
                    }
                    dirty = true;
                }
                Event::Mouse(MouseEvent::Release(_, _)) => {
                    state.drag = None;
                    state.last_mouse_cell = None;
                }
                _ => {}
            }
        }

        if !state.paused {
            state.motion.advance(FRAME_DT.as_secs_f32(), state.speed);
            state.anim_time += FRAME_DT;
            dirty = true;
        }

        // Redraw only on state change: repainting the full screen every
        // 33ms would wipe any in-progress text selection.
        if dirty {
            write!(screen, "{}", termion::cursor::Goto(1, 1)).unwrap();
            render_animation_frame(&mut screen, &mut state, true);
            write!(
                screen,
                "q=quit space=pause arrows=nudge o=orbit l=line +/-=speed Ldrag=angle drag=place mod+drag=fine []=cand ,.=metric"
            )
            .unwrap();
            screen.flush().unwrap();
            dirty = false;
            frames += 1;
            if frame_count.is_some_and(|n| frames >= n) {
                return;
            }
        }
        thread::sleep(FRAME_DT);
    }
}

fn usage() {
    eprintln!(
        "usage: floating_square_debug <mode>\n\
         modes:\n  \
           pos X Y       single square at world point (X, Y), with snap-family\n  \
          \x20      diagnostics and a sampled coverage view\n  \
           families X Y  the same position with each snap family forced\n  \
           sweep         offset table over 0..=0.5 in 1/16 steps, labeled with\n  \
          \x20      the family each offset picks (decision-boundary map)\n  \
           glyphs        every block character the renderer can emit, each\n  \
          \x20      with an exact 8x24 big-pixel zoom (1/16 x 1/24 world per\n  \
          \x20      pixel), framed; plain text, redirect to a file\n  \
           animate [N]   orbiting square, two-method comparison: the in-use\n  \
          \x20      game path (family-snapped) and a candidate replacement\n  \
          \x20      cycled with [ and ] (charwise, charwise + protrusion,\n  \
          \x20      charwise + protrusion squared). Each row: the full grid\n  \
          \x20      with glyph legend and the method's own objective, the\n  \
          \x20      full-resolution zoomed render (one color per glyph), and\n  \
          \x20      ONE error pane cycled with , and . (center, area,\n  \
          \x20      per-char coverage, ideal xor, jaggedness, displacement\n  \
          \x20      sensitivity) with its numeric value; q quits, space\n  \
          \x20      pauses, arrows nudge, o resumes the orbit, l starts a\n  \
          \x20      line trajectory, +/- change speed, left click/drag sets\n  \
          \x20      the orbit's angular position (angle from the top-row\n  \
          \x20      grid's center to the mouse); other buttons place the\n  \
          \x20      square, and holding shift/ctrl/alt while dragging (or\n  \
          \x20      pressing f) gives fine control: large mouse movements\n  \
          \x20      map to sub-cell square movements. Optional frame count N\n  \
          \x20      runs a fixed number of frames, which is also the mode\n  \
          \x20      used when stdout is not a terminal.\n\
         default: animate (runs until q; fixed frame count when piped)"
    );
}

fn main() {
    let args: Vec<String> = std::env::args().skip(1).collect();
    let parse_xy = |i: usize| -> Option<(f32, f32)> {
        match (
            args.get(i).and_then(|s| s.parse().ok()),
            args.get(i + 1).and_then(|s| s.parse().ok()),
        ) {
            (Some(x), Some(y)) => Some((x, y)),
            _ => None,
        }
    };
    match args.first().map(String::as_str) {
        None => run_animation(None),
        Some("pos") => match parse_xy(1) {
            Some((x, y)) => show_position(euclid::point2(x, y)),
            _ => {
                usage();
                std::process::exit(2);
            }
        },
        Some("families") => match parse_xy(1) {
            Some((x, y)) => show_families(euclid::point2(x, y)),
            _ => {
                usage();
                std::process::exit(2);
            }
        },
        Some("sweep") => show_sweep(),
        Some("glyphs") => print_glyph_table(),
        Some("animate") => match args.get(1).map(|s| s.parse::<u32>()) {
            None => run_animation(None),
            Some(Ok(n)) => run_animation(Some(n)),
            Some(Err(_)) => {
                eprintln!("animate: frame count must be a non-negative integer");
                usage();
                std::process::exit(2);
            }
        },
        Some("--help" | "-h") => usage(),
        Some(other) => {
            eprintln!("unknown mode: {other}");
            usage();
            std::process::exit(2);
        }
    }
}
