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
//!                 emit with its Unicode name and an exact big-pixel zoom
//!                 (8x24 pixels per character, 1/16 x 1/24 world each)
//!                 framed with position rulers; plain text, redirect to a
//!                 file
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
//!                 the zoomed render as union-lattice big pixels (the
//!                 glyphs table's exact 1/16 x 1/24 world pixels, one
//!                 palette color per glyph), and ONE error measurement
//!                 as a colored pane on the same big-pixel lattice (one
//!                 big pixel per sample — the grid the numbers measure),
//!                 with its numeric value — cycled with , and . through center error
//!                 (silhouette + ideal outline + both centroids), area
//!                 error (signed: over red / under blue), per-char
//!                 coverage (half-cells heat-shaded by local error),
//!                 ideal square xor (any mismatch lit), jaggedness
//!                 (contour lit by local edge-step length), and
//!                 displacement sensitivity (what turns wrong under the
//!                 worst 1/16 nudge: bright yellow = newly wrong), and
//!                 frame diff (which samples changed against the previous
//!                 rendered frame — motion pops light up; n/a until one
//!                 frame old). Each method row also carries the selected
//!                 error's recent history as a block-character sparkline
//!                 (one 1/8th-increment block per frame, shared y-axis
//!                 across the method slots), sampled only on frames where
//!                 the square actually moves — manual mouse movement
//!                 counts. Then a common row with the ideal (true square)
//!                 zoom, global state, and controls. Left click/drag sets the orbit's
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
    self, actual_sample, assign_colors, big_pane_from_colors, big_pixel_pane, cell_bg,
    charwise_neighborhood, charwise_protrusion_squared_neighborhood, charwise_shaped_neighborhood,
    charwise_objective, coverage_error, displacement_sensitivity, fill_centroid, glyph_filled,
    glyph_pane, jaggedness, lerp, pane_from_colors, per_char_coverage_error,
    frame_diff_pane, frame_diff_xor, rendered_neighborhood, rendered_neighborhood_forced,
    ClassGrid, FillGrid, Metrics, BIG_PX_W, BITMAP_W, PX_H, PX_W,
    CHARWISE_PROTRUSION_SQUARED_WEIGHT, CHARWISE_PROTRUSION_WEIGHT, DISPLACEMENT_DELTA,
};
use terminal_rendering::glyph_constants::named_chars::EIGHTH_BLOCKS_FROM_BOTTOM;
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
const METRICS: [&str; 7] = ["center", "area", "per-char", "xor", "jagged", "disp", "frame"];

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

/// History depth per method row: one 1/8th-increment block per sampled
/// frame (~1.05s at the 33ms frame clock). Matches the zoom/error column
/// width so the boxed rows stay aligned.
const SPARKLINE_LEN: usize = 32;

/// One error measurement's full report for one method: the colored pane,
/// the value string printed under it, and the scalar the history buffers
/// sample (`None` = no data this frame — not appended, shown as n/a).
#[derive(Default)]
struct MetricReport {
    pane: Vec<String>,
    value: String,
    number: Option<f32>,
}

/// The selected error measurement for one method — the single source of
/// truth shared by the displayed pane/value (method_section) and the
/// per-slot history sparklines (render_animation_frame).
///
/// `number` is the history scalar: `center` = |centroid − pos| (the pane
/// shows the pair), `area` = |signed error| (the pane keeps over-red /
/// under-blue), everything else = the displayed value as-is; `frame` is
/// the diff against `prev`, `None` while no previous frame exists.
fn metric_report(
    nb: Neighborhood,
    glyphs: &[[DoubleChar; 3]; 3],
    center: WorldSquare,
    pos: WorldPoint,
    metric: usize,
    prev: Option<(&[[DoubleChar; 3]; 3], WorldSquare)>,
    style: &coverage::Style,
) -> MetricReport {
    let owners = assign_colors(glyphs);
    let sample_origin = euclid::point2(center.x as f32 - 1.5, center.y as f32 - 1.5);
    let class = || {
        ClassGrid::sample(sample_origin, |wx, wy| {
            ClassGrid::class_at(glyphs, &owners, center, pos, wx, wy)
        })
    };
    let actual = || {
        FillGrid::sample(sample_origin, |wx, wy| {
            actual_sample(glyphs, &owners, center, wx, wy)
        })
    };
    match metric {
        0 => {
            let actual = actual();
            let class = class();
            let c = fill_centroid(&actual);
            MetricReport {
                pane: class.center_pane(&actual, pos, style),
                value: match c {
                    Some(c) => format!("({:+.2}, {:+.2})", c.x - pos.x, c.y - pos.y),
                    None => "n/a".to_string(),
                },
                number: c.map(|c| {
                    ((c.x - pos.x) * (c.x - pos.x) + (c.y - pos.y) * (c.y - pos.y)).sqrt()
                }),
            }
        }
        1 => {
            let class = class();
            let signed = class.signed_area_error();
            MetricReport {
                pane: class.signed_pane(style),
                value: format!("{signed:+.3}"),
                number: Some(signed.abs()),
            }
        }
        2 => {
            let e = per_char_coverage_error(glyphs, center, pos);
            MetricReport {
                pane: ClassGrid::per_char_heat_pane(glyphs, center, pos, style),
                value: format!("{e:.3}"),
                number: Some(e),
            }
        }
        3 => {
            let class = class();
            let xor = class.xor_error();
            MetricReport {
                pane: class.mismatch_pane(style),
                value: format!("{xor:.3}"),
                number: Some(xor),
            }
        }
        4 => {
            let actual = actual();
            let jag = jaggedness(&actual);
            MetricReport {
                pane: ClassGrid::jaggedness_pane(&actual, style),
                value: format!("{jag:.2}"),
                number: Some(jag),
            }
        }
        5 => {
            let class = class();
            let (gain, dir) = displacement_sensitivity(nb, pos, DISPLACEMENT_DELTA);
            let shifted_pos = pos + dir * DISPLACEMENT_DELTA;
            let (glyphs2, center2) = nb(shifted_pos);
            let owners2 = assign_colors(&glyphs2);
            // sampled on the BASE frame's origin so base and shifted
            // grids align sample-for-sample for the pane comparison
            let shifted = ClassGrid::sample(sample_origin, |wx, wy| {
                ClassGrid::class_at(&glyphs2, &owners2, center2, shifted_pos, wx, wy)
            });
            MetricReport {
                pane: ClassGrid::displacement_pane(&class, &shifted, style),
                value: format!("{:.3}{}", gain, dir_arrow(dir)),
                number: Some(gain),
            }
        }
        _ => {
            let cur = (glyphs, center);
            let number = prev.map(|_| frame_diff_xor(prev, cur));
            MetricReport {
                pane: frame_diff_pane(prev, cur, style),
                value: match number {
                    Some(v) => format!("{v:.3}"),
                    None => "n/a".to_string(),
                },
                number,
            }
        }
    }
}

/// One method's recent history of the selected error as a single line of
/// 1/8th-increment vertical blocks: `level = round(v / scale * 8)`
/// clamped to 0..=8, `·` for 0. Oldest sample on the left, right-aligned,
/// `·`-padded until the buffer fills. `scale` is shared across all method
/// rows (max over every slot's history), so block heights compare between
/// methods.
fn sparkline_column(history: &[f32], scale: f32, style: &coverage::Style) -> String {
    let dim = style.fg(coverage::DOT_COLOR);
    let lit = style.fg(coverage::Rgb(190, 190, 200));
    let mut line = String::new();
    for _ in 0..SPARKLINE_LEN.saturating_sub(history.len()) {
        line.push_str(&dim);
        line.push('·');
    }
    for &v in history {
        let level = (v / scale * 8.0).round().clamp(0.0, 8.0) as usize;
        line.push_str(if level == 0 { &dim } else { &lit });
        line.push(if level == 0 {
            '·'
        } else {
            EIGHTH_BLOCKS_FROM_BOTTOM[level]
        });
    }
    line.push_str(style.reset());
    line
}

/// The 4th method-row column: the selected error's history sparkline plus
/// the shared scale legend. Samples are taken only on frames where the
/// square moved, so parked redraws don't dilute the window.
fn history_column(
    metric: usize,
    history: &[f32],
    scale: f32,
    style: &coverage::Style,
) -> Vec<String> {
    vec![
        format!("{:^SPARKLINE_LEN$}", format!("history: {}", METRICS[metric])),
        sparkline_column(history, scale, style),
        format!("{:^SPARKLINE_LEN$}", format!("0 ▁▂▃▄▅▆▇█ max={scale:.3}")),
    ]
}

/// One method's bordered section: large view (full animation grid, method
/// info, its own objective), zoomed render at native sampled resolution
/// with one palette color per glyph plus legend, the currently selected
/// error measurement as a full-resolution colored pane with its value
/// (precomputed — all four slots run `metric_report` for the histories,
/// the displayed rows reuse pane + value), and the error's recent history
/// as a sparkline with the shared scale legend.
fn method_section(
    title: &str,
    objective_idx: usize,
    glyphs: &[[DoubleChar; 3]; 3],
    center: WorldSquare,
    pos: WorldPoint,
    metric: usize,
    style: &coverage::Style,
    extra_info: &[String],
    report: MetricReport,
    history: &[f32],
    scale: f32,
) -> Vec<String> {
    let owners = assign_colors(glyphs);

    // zoomed render: union-lattice big pixels (see big_pixel_pane) — every
    // glyph edge lands on a pixel boundary, so the fill is exact, one
    // palette color per glyph
    let mut zoom_col: Vec<String> = vec![format!("{:^BIG_PX_W$}", "big pixels 1/16x1/24")];
    zoom_col.extend(big_pixel_pane(
        glyphs,
        &owners,
        center,
        &coverage::PALETTE,
        style,
    ));
    let legend = glyph_legend(glyphs, &owners, style);
    if legend.1 > 0 {
        zoom_col.push("glyph colors:".to_string());
        zoom_col.push(legend.0);
    }

    // large view: full animation grid, method info, then its objective —
    // the error the method's own picker minimizes
    let origin0 = euclid::point2(0, 0);
    let mut large = grid_frame(ANIMATE_GRID_RADIUS, origin0);
    draw_neighborhood_colored(&mut large, ANIMATE_GRID_RADIUS, origin0, center, glyphs, &owners);
    let mut large_col: Vec<String> = large
        .string_for_regular_display()
        .lines()
        .map(String::from)
        .collect();
    large_col.extend(extra_info.iter().cloned());
    large_col.extend(objective_lines(objective_idx, glyphs, &owners, center, pos));

    // the selected error measurement: precomputed pane + numeric value
    let mut err_col: Vec<String> = vec![format!(
        "{:^BIG_PX_W$}",
        format!("{} (, .)", METRICS[metric])
    )];
    err_col.extend(report.pane);
    err_col.push(format!("{:^BIG_PX_W$}", report.value));

    let spark_col = history_column(metric, history, scale, style);

    let (large_w, zoom_w, err_w, spark_w) = (
        visible_w(&large_col),
        visible_w(&zoom_col),
        visible_w(&err_col),
        visible_w(&spark_col),
    );
    let cols = [
        (large_col, large_w),
        (zoom_col, zoom_w),
        (err_col, err_w),
        (spark_col, spark_w),
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

/// The true square at the big-pixel union lattice over the same
/// 2x2-world window as `big_pixel_pane`, so the animate view's ideal
/// column lines up cell-for-cell with the actual zooms. True-square
/// edges fall between lattice points, so pixels keep fractional analytic
/// shading (the actual pane needs none — glyph edges are lattice-aligned).
fn ideal_big_pixel_pane(
    pos: WorldPoint,
    center: WorldSquare,
    style: &coverage::Style,
) -> Vec<String> {
    let (ox, oy) = (center.x as f32 - 1.0, center.y as f32 - 1.0);
    // Work in lattice units (1/16 wide, 1/24 tall) so pixel bounds are
    // exact integers; only the square's edges carry pos's own rounding.
    // Direct world-unit subtraction leaves ~1e-7 f32 noise that paints
    // epsilon slivers at exact edge alignments and off-by-one lerp
    // colors on full rows (1/24 is not dyadic, unlike the old 1/8 pane).
    let (sq_x0, sq_x1) = ((pos.x - 0.5 - ox) * 16.0, (pos.x + 0.5 - ox) * 16.0);
    let (sq_y0, sq_y1) = ((pos.y - 0.5 - oy) * 24.0, (pos.y + 0.5 - oy) * 24.0);
    let mut colors = vec![vec![None; BIG_PX_W]; coverage::BIG_PX_H];
    for py in 0..coverage::BIG_PX_H {
        // pixel py covers y-units [47-py, 48-py] from the window bottom;
        // one unit is one pixel, so the overlap is the coverage fraction
        let ov_y = (48.0 - py as f32).min(sq_y1) - (47.0 - py as f32).max(sq_y0);
        if ov_y <= 0.0 {
            continue;
        }
        for px in 0..BIG_PX_W {
            let ov_x = (px as f32 + 1.0).min(sq_x1) - (px as f32).max(sq_x0);
            let frac = ov_x.max(0.0) * ov_y.max(0.0);
            if frac > 0.0 {
                let bg = cell_bg(px / 8, py / 24);
                colors[py][px] = Some(lerp(bg, coverage::IDEAL_COLOR, frac));
            }
        }
    }
    big_pane_from_colors(style, &colors)
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

/// The glyph table: one entry per block character (first column) with its
/// official Unicode name (UCD), and its exact 8x24 big-pixel zoom framed
/// in box drawing characters, so each glyph's cell boundary is explicit.
/// Position rulers: digits above each grid index the big-pixel column
/// boundaries (1/16 world apart — every one a possible vertical cut,
/// h-eighths strips); digits left of each grid index the big-pixel row
/// boundary at the top of each half-row (even numbers — a half-row
/// spans 2 big pixels; the bottom frame line is position 24). One big
/// pixel = one vertical half character: both=█ upper=▀ lower=▄
/// empty=·. Plain text only (no ANSI) — the output is meant for a file
/// (`glyphs > x.txt`).
fn print_glyph_table() {
    println!("block glyph reference - exact big-pixel zoom at the union lattice");
    println!("one character = one half-cell = 0.5 world wide x 1.0 world tall;");
    println!("one big pixel = 1/16 world wide x 1/24 world tall (the finest");
    println!("increments any snap family can express), so a character is 8x24");
    println!("big pixels and every glyph edge lies exactly on a pixel boundary.");
    println!("one big pixel = one vertical half character: both=█ upper=▀ lower=▄ empty=·");
    println!("names are the official Unicode character names (UCD).");
    println!("x ruler (digits above each grid): big-pixel boundary index,");
    println!("  1/16 world apart; every one is a possible vertical cut");
    println!("  (h-eighths).");
    println!("y ruler (digits left of each grid, one per half-row):");
    println!("  big-pixel boundary index at the top of that half-row;");
    println!("  even numbers, because each half-row spans 2 big pixels.");
    println!("  The grid bottom edge is position 24.");
    let ruler: String = (0..=TABLE_PX_W)
        .map(|i| char::from(b'0' + i as u8))
        .collect();
    for &c in &used_block_glyphs() {
        println!();
        println!("{c} {}", char_name(c));
        println!("   {ruler}");
        println!("  ┌{}┐", "─".repeat(TABLE_PX_W));
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
            let mut line = String::new();
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
            println!("{:>2}│{line}│", 2 * t);
        }
        println!("{:>2}└{}┘", TABLE_PX_H, "─".repeat(TABLE_PX_W));
    }
}

/// Official Unicode name (UCD) of one table glyph. The fallback is the
/// bare code point; the test below fails if any used glyph reaches it,
/// so names cannot silently go missing when the vocabulary grows.
fn char_name(c: char) -> String {
    match c {
        '▏' => "LEFT ONE EIGHTH BLOCK",
        '▎' => "LEFT ONE QUARTER BLOCK",
        '▍' => "LEFT THREE EIGHTHS BLOCK",
        '▌' => "LEFT HALF BLOCK",
        '▋' => "LEFT FIVE EIGHTHS BLOCK",
        '▊' => "LEFT THREE QUARTERS BLOCK",
        '▉' => "LEFT SEVEN EIGHTHS BLOCK",
        '█' => "FULL BLOCK",
        '▕' => "RIGHT ONE EIGHTH BLOCK",
        '🮇' => "RIGHT ONE QUARTER BLOCK",
        '🮈' => "RIGHT THREE EIGHTHS BLOCK",
        '▐' => "RIGHT HALF BLOCK",
        '🮉' => "RIGHT FIVE EIGHTHS BLOCK",
        '🮊' => "RIGHT THREE QUARTERS BLOCK",
        '🮋' => "RIGHT SEVEN EIGHTHS BLOCK",
        '▁' => "LOWER ONE EIGHTH BLOCK",
        '▂' => "LOWER ONE QUARTER BLOCK",
        '▃' => "LOWER THREE EIGHTHS BLOCK",
        '▄' => "LOWER HALF BLOCK",
        '▅' => "LOWER FIVE EIGHTHS BLOCK",
        '▆' => "LOWER THREE QUARTERS BLOCK",
        '▇' => "LOWER SEVEN EIGHTHS BLOCK",
        '▔' => "UPPER ONE EIGHTH BLOCK",
        '🮂' => "UPPER ONE QUARTER BLOCK",
        '🮃' => "UPPER THREE EIGHTHS BLOCK",
        '▀' => "UPPER HALF BLOCK",
        '🮄' => "UPPER FIVE EIGHTHS BLOCK",
        '🮅' => "UPPER THREE QUARTERS BLOCK",
        '🮆' => "UPPER SEVEN EIGHTHS BLOCK",
        '🬂' => "BLOCK SEXTANT-12",
        '🬎' => "BLOCK SEXTANT-1234",
        '🬭' => "BLOCK SEXTANT-56",
        '🬹' => "BLOCK SEXTANT-3456",
        '▖' => "QUADRANT LOWER LEFT",
        '▗' => "QUADRANT LOWER RIGHT",
        '▘' => "QUADRANT UPPER LEFT",
        '▝' => "QUADRANT UPPER RIGHT",
        '🬀' => "BLOCK SEXTANT-1",
        '🬁' => "BLOCK SEXTANT-2",
        '🬄' => "BLOCK SEXTANT-13",
        '🬉' => "BLOCK SEXTANT-24",
        '🬏' => "BLOCK SEXTANT-5",
        '🬓' => "BLOCK SEXTANT-35",
        '🬞' => "BLOCK SEXTANT-6",
        '🬦' => "BLOCK SEXTANT-46",
        _ => return format!("U+{:04X}", c as u32),
    }
    .to_string()
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
    /// Previously rendered glyph neighborhoods per method slot (0 = the
    /// in-use row, 1..=3 = CANDIDATES index + 1), for the frame-diff
    /// metric. Updated after every rendered frame — moving or not — so
    /// frame diff always compares against the immediately previous render.
    prev_renders: [Option<([[DoubleChar; 3]; 3], WorldSquare)>; 4],
    /// Selected error over time per method slot, sampled on frames where
    /// the square actually moved. All four slots accumulate; `[`/`]` only
    /// changes which two are displayed.
    history: [Vec<f32>; 4],
    /// Which metric the history buffers hold; a change clears them all.
    history_metric: usize,
    /// Position at the last history sample: a frame appends only when pos
    /// differs (manual mouse movement and nudges count).
    last_sample_pos: Option<WorldPoint>,
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
            prev_renders: [None; 4],
            history: [Vec::new(), Vec::new(), Vec::new(), Vec::new()],
            history_metric: 0,
            last_sample_pos: None,
        }
    }

    /// Clear all history buffers when the metric cycled to a new one —
    /// different metrics have incomparable units.
    fn sync_history_metric(&mut self) {
        if self.history_metric != self.metric {
            self.history = [Vec::new(), Vec::new(), Vec::new(), Vec::new()];
            self.history_metric = self.metric;
        }
    }

    /// One frame's history sample: append each slot's number only when
    /// the square actually moved since the last sample — mouse drags and
    /// arrow nudges change pos too, so they count; parked redraws (metric
    /// or candidate switches, paused key presses) append nothing. `None`
    /// numbers (frame diff before its first prev) are skipped.
    fn record_history(&mut self, pos: WorldPoint, numbers: &[Option<f32>; 4]) {
        if self.last_sample_pos == Some(pos) {
            return;
        }
        self.last_sample_pos = Some(pos);
        for (slot, number) in numbers.iter().enumerate() {
            if let Some(number) = number {
                self.history[slot].push(*number);
                if self.history[slot].len() > SPARKLINE_LEN {
                    self.history[slot].remove(0);
                }
            }
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
    let cand_name = CANDIDATES[state.candidate].0;
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

    // All four method slots get a metric report every frame: the two
    // displayed rows reuse pane + value, all four numbers feed the
    // history sparklines — candidate switches only change which slots
    // render, so every slot's history stays continuous across them.
    state.sync_history_metric();
    let methods: [(&str, Neighborhood); 4] = [IN_USE, CANDIDATES[0], CANDIDATES[1], CANDIDATES[2]];
    let mut reports: Vec<MetricReport> = Vec::with_capacity(4);
    let mut renders: Vec<([[DoubleChar; 3]; 3], WorldSquare)> = Vec::with_capacity(4);
    let mut numbers: [Option<f32>; 4] = [None; 4];
    for slot in 0..4 {
        let (glyphs, center) = methods[slot].1(pos);
        let report = metric_report(
            methods[slot].1,
            &glyphs,
            center,
            pos,
            state.metric,
            state.prev_renders[slot].as_ref().map(|(g, c)| (g, *c)),
            &style,
        );
        numbers[slot] = report.number;
        renders.push((glyphs, center));
        reports.push(report);
    }
    state.record_history(pos, &numbers);
    // Shared y-axis: max over every live slot keeps block heights
    // comparable between rows and stable across candidate switches.
    let scale = state
        .history
        .iter()
        .flatten()
        .fold(1e-6f32, |acc, &v| acc.max(v));

    let mut text = String::new();
    for line in method_section(
        &format!("in use: {}", IN_USE.0),
        0,
        &renders[0].0,
        renders[0].1,
        pos,
        state.metric,
        &style,
        &in_use_info,
        std::mem::take(&mut reports[0]),
        &state.history[0],
        scale,
    ) {
        text.push_str(&line);
        text.push('\n');
    }
    let cand_slot = state.candidate + 1;
    for line in method_section(
        &format!("candidate: {cand_name}  ([ ] cycle)"),
        cand_slot,
        &renders[cand_slot].0,
        renders[cand_slot].1,
        pos,
        state.metric,
        &style,
        &cand_info,
        std::mem::take(&mut reports[cand_slot]),
        &state.history[cand_slot],
        scale,
    ) {
        text.push_str(&line);
        text.push('\n');
    }

    // common box: ideal zoom (drawn analytically — see ideal_big_pixel_pane),
    // global state, controls — one column each
    let center = world_point_to_world_square(pos);
    let ideal_lines = ideal_big_pixel_pane(pos, center, &style);
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
            (common_col.as_slice(), BIG_PX_W),
            (global.as_slice(), global_w),
            (controls.as_slice(), controls_w),
        ],
    );
    text.push_str(&common_row.join("\n"));
    text.push('\n');

    // Remember all four renders for the next frame's frame-diff metric —
    // every rendered frame, moving or not, so the diff is always against
    // the immediately previous render.
    for slot in 0..4 {
        state.prev_renders[slot] = Some(renders[slot]);
    }

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
          \x20      with its Unicode name and an exact 8x24 big-pixel zoom\n  \
          \x20      (1/16 x 1/24 world per pixel), framed with position\n  \
          \x20      rulers; plain text, redirect to a file\n  \
           animate [N]   orbiting square, two-method comparison: the in-use\n  \
          \x20      game path (family-snapped) and a candidate replacement\n  \
          \x20      cycled with [ and ] (charwise, charwise + protrusion,\n  \
          \x20      charwise + protrusion squared). Each row: the full grid\n  \
          \x20      with glyph legend and the method's own objective, the\n  \
          \x20      full-resolution zoomed render (one color per glyph), and\n  \
          \x20      ONE error pane cycled with , and . (center, area,\n  \
          \x20      per-char coverage, ideal xor, jaggedness, displacement\n  \
          \x20      sensitivity, frame diff) with its numeric value, plus\n  \
          \x20      the error's recent history as a block-character\n  \
          \x20      sparkline (shared y-axis); q quits, space\n  \
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

#[cfg(test)]
mod glyph_table_tests {
    use super::*;

    /// Every glyph the table renders must carry its real Unicode name: a
    /// new family glyph reaching char_name's code-point fallback fails
    /// here instead of shipping an anonymous table entry.
    #[test]
    fn every_used_glyph_is_named() {
        for c in used_block_glyphs() {
            let name = char_name(c);
            assert!(
                !name.starts_with("U+"),
                "{c} (U+{:04X}) needs a name arm in char_name",
                c as u32
            );
        }
    }
}

#[cfg(test)]
mod history_tests {
    use super::*;

    /// Sparkline maps the scale's zero to `·`, its max to the full block,
    /// half-scale to the half block, right-aligns, and left-pads with `·`.
    #[test]
    fn sparkline_maps_levels_and_aligns_right() {
        let style = coverage::Style { enabled: false };
        let chars: Vec<char> = sparkline_column(&[0.0, 2.0, 1.0], 2.0, &style).chars().collect();
        assert_eq!(chars.len(), SPARKLINE_LEN);
        let pad = SPARKLINE_LEN - 3;
        assert!(chars[..pad].iter().all(|&c| c == '·'));
        assert_eq!(chars[pad], '·'); // v = 0 → level 0
        assert_eq!(chars[pad + 1], '█'); // v = scale → level 8
        assert_eq!(chars[pad + 2], '▄'); // v = scale/2 → level 4
    }

    /// History semantics: only moving frames append (manual movement
    /// counts), all four slots accumulate independently, `None` numbers
    /// are skipped, the buffer caps at SPARKLINE_LEN keeping the newest,
    /// a metric change clears everything, and candidate switches change
    /// nothing.
    #[test]
    fn history_appends_only_on_motion_and_caps() {
        let mut state = AnimState::new();
        state.sync_history_metric();
        let p0 = euclid::point2(0.0, 0.0);
        state.record_history(p0, &[Some(1.0), Some(2.0), Some(3.0), Some(4.0)]);
        // parked redraw at the same pos: nothing, even with new numbers
        state.record_history(p0, &[Some(9.0); 4]);
        assert_eq!(state.history[0], vec![1.0]);
        assert_eq!(state.history[3], vec![4.0]);
        // motion appends on all slots; None skipped (frame metric's first)
        state.record_history(euclid::point2(0.25, 0.0), &[Some(5.0), None, Some(7.0), Some(8.0)]);
        assert_eq!(state.history[0], vec![1.0, 5.0]);
        assert_eq!(state.history[1], vec![2.0]);
        assert_eq!(state.history[2], vec![3.0, 7.0]);
        // cap: only the newest SPARKLINE_LEN values survive
        for k in 0..(SPARKLINE_LEN + 6) {
            state.record_history(
                euclid::point2(k as f32 * 0.25, 0.5),
                &[Some(k as f32), None, None, None],
            );
        }
        assert_eq!(state.history[0].len(), SPARKLINE_LEN);
        assert_eq!(state.history[0][0], 6.0);
        assert_eq!(state.history[0][SPARKLINE_LEN - 1], (SPARKLINE_LEN + 5) as f32);
        // metric change clears all four
        state.metric = 3;
        state.sync_history_metric();
        assert!(state.history.iter().all(|h| h.is_empty()));
        // candidate switch: purely a display change — history keeps
        state.candidate = 2;
        state.record_history(euclid::point2(9.0, 9.0), &[Some(1.0); 4]);
        assert_eq!(state.history[0], vec![1.0]);
        assert_eq!(state.history[3], vec![1.0]);
    }

    /// MetricReport's history scalar: center = |centroid − pos|, area =
    /// |signed error|, every metric reports Some except frame before its
    /// first prev (n/a), and frame with a prev is the true diff.
    #[test]
    fn metric_report_history_scalars() {
        let style = coverage::Style::from_env();
        let pos = euclid::point2(0.3, -0.7);
        let (glyphs, center) = rendered_neighborhood(pos);
        let frame = METRICS.len() - 1;
        for metric in 0..METRICS.len() {
            let r =
                metric_report(rendered_neighborhood, &glyphs, center, pos, metric, None, &style);
            if metric == frame {
                assert_eq!(r.number, None);
                assert_eq!(r.value, "n/a");
            } else {
                assert!(r.number.expect("history scalar") >= 0.0, "{metric} is a magnitude");
            }
            assert_eq!(r.pane.len(), coverage::BIG_TEXT_ROWS);
        }
        let owners = assign_colors(&glyphs);
        let origin = euclid::point2(center.x as f32 - 1.5, center.y as f32 - 1.5);
        // center: number is the Euclidean |centroid − pos|
        let r = metric_report(rendered_neighborhood, &glyphs, center, pos, 0, None, &style);
        let actual =
            FillGrid::sample(origin, |wx, wy| actual_sample(&glyphs, &owners, center, wx, wy));
        let c = fill_centroid(&actual).unwrap();
        let d = (c.x - pos.x).hypot(c.y - pos.y);
        assert!((r.number.unwrap() - d).abs() < 1e-5);
        // area: number is |signed|
        let r = metric_report(rendered_neighborhood, &glyphs, center, pos, 1, None, &style);
        let class = ClassGrid::sample(origin, |wx, wy| {
            ClassGrid::class_at(&glyphs, &owners, center, pos, wx, wy)
        });
        assert_eq!(r.number, Some(class.signed_area_error().abs()));
        // frame with a previous render: the real diff
        let (pg, pc) = rendered_neighborhood(euclid::point2(0.55, -0.7));
        let r =
            metric_report(rendered_neighborhood, &glyphs, center, pos, frame, Some((&pg, pc)), &style);
        assert!(r.number.unwrap() > 0.0);
    }
}
