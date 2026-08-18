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
//!   animate       square on the alternate screen (q quits); orbit,
//!                 arrow-key nudge, and line trajectories. Shows a zoomed
//!                 sampled-coverage view (actual vs ideal, one color per
//!                 glyph) and one line per error metric. Click/drag places
//!                 the square; holding shift/ctrl/alt while dragging (or
//!                 pressing f) switches to fine control, where large mouse
//!                 movements map to sub-cell square movements.
//!
//! Run via scripts/debug-floating-squares.sh or:
//!   cargo run -p terminal_rendering --bin floating_square_debug -- pos 1.3 0.7

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
    self, actual_sample, assign_colors, coverage_error, rendered_neighborhood,
    rendered_neighborhood_forced, FillGrid, Metrics, BITMAP_W, NX, NY,
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

/// Sampled actual-vs-ideal coverage, using the same oracle the coherence
/// test asserts on (tests/floating_square_coherence.rs).
fn coverage_zoom_pane(pos: WorldPoint) -> String {
    let (grid, center) = rendered_neighborhood(pos);
    let owners = assign_colors(&grid);
    let origin = euclid::point2(center.x as f32 - 1.5, center.y as f32 - 1.5);
    let actual = FillGrid::sample(origin, |wx, wy| actual_sample(&grid, &owners, center, wx, wy));
    let ideal = FillGrid::sample(origin, |wx, wy| {
        (
            (wx - pos.x).abs() <= 0.5 && (wy - pos.y).abs() <= 0.5,
            Some(0),
        )
    });
    let style = coverage::Style::from_env();
    let actual_lines = actual.bitmap_pane(&coverage::PALETTE, &style);
    let ideal_lines = ideal.bitmap_pane(&[coverage::IDEAL_COLOR], &style);
    let mut out = format!(
        "sampled coverage (1 text cell = 2 samples; background checkerboard = character cells):\n  {:BITMAP_W$}  {}\n",
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

/// World point under the (1-based) terminal cell, using the same grid
/// geometry as the frame: 2 columns per square, rows increase downward.
fn mouse_cell_point(col: u16, row: u16) -> WorldPoint {
    let r = ANIMATE_GRID_RADIUS as f32;
    // The origin square spans cols 2r+1..=2r+2 (center 2r+1.5), row r+1.
    let dx_cells = col as f32 - (2.0 * r + 1.5);
    let dy_cells = row as f32 - (r + 1.0);
    euclid::point2(dx_cells * 0.5, -dy_cells)
}

/// `raw_mode`: true when writing to a termion raw-mode terminal. Raw mode
/// disables ONLCR, so bare '\n' would stair-step the frame.
const FRAME_DT: Duration = Duration::from_millis(33);

/// Centroid of the rendered fill, for the measured center error: how far
/// the silhouette's actual middle sits from the true square center.
fn measured_center(actual: &FillGrid) -> Option<WorldPoint> {
    let (mut sx, mut sy, mut n) = (0.0f32, 0.0f32, 0usize);
    for j in 0..NX {
        for i in 0..NY {
            if actual.filled(j, i) {
                sx += actual.wx(j);
                sy += actual.wy(i);
                n += 1;
            }
        }
    }
    (n > 0).then(|| euclid::point2(sx / n as f32, sy / n as f32))
}

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

    // sampled-coverage oracle (the same one the coherence test asserts on):
    // both the zoomed view and the error metrics derive from it
    let (glyphs, center) = rendered_neighborhood(pos);
    let owners = assign_colors(&glyphs);
    let sample_origin = euclid::point2(center.x as f32 - 1.5, center.y as f32 - 1.5);
    let actual = FillGrid::sample(sample_origin, |wx, wy| {
        actual_sample(&glyphs, &owners, center, wx, wy)
    });
    let ideal = FillGrid::sample(sample_origin, |wx, wy| {
        (
            (wx - pos.x).abs() <= 0.5 && (wy - pos.y).abs() <= 0.5,
            Some(0),
        )
    });
    let metrics = Metrics::measure(&actual, pos);
    let coverage_err = coverage_error(&glyphs, &owners, center, pos);
    let style = coverage::Style::from_env();
    let actual_lines = actual.bitmap_pane(&coverage::PALETTE, &style);
    let ideal_lines = ideal.bitmap_pane(&[coverage::IDEAL_COLOR], &style);

    let origin = euclid::point2(0, 0);
    let mut frame = grid_frame(ANIMATE_GRID_RADIUS, origin);
    draw_floating_square(&mut frame, ANIMATE_GRID_RADIUS, origin, pos, None);
    overlay_true_center(&mut frame, ANIMATE_GRID_RADIUS, origin, pos);
    let frame_lines: Vec<String> = frame
        .string_for_regular_display()
        .lines()
        .map(String::from)
        .collect();
    let frame_w = frame.width();

    // glyph legend in the same scan order assign_colors used, so legend
    // colors match the zoomed view's pixels. A 1x1 square spans at most 8
    // half-cells, so the legend always fits under the real-size grid.
    let mut legend = String::new();
    let mut legend_w = 0usize;
    for dy in [1i32, 0, -1] {
        for dx in -1..=1i32 {
            for half in 0..2 {
                if let Some(idx) = owners[(dx + 1) as usize][(dy + 1) as usize][half] {
                    legend.push_str(&style.fg(coverage::PALETTE[idx % coverage::PALETTE.len()]));
                    legend.push(glyphs[(dx + 1) as usize][(dy + 1) as usize][half]);
                    legend.push(' ');
                    legend_w += 2;
                }
            }
        }
    }
    legend.push_str(style.reset());
    // the real-size grid is shorter than the zoomed panes; the leftover
    // rows under it hold the legend. (text, visible width)
    let under_grid: Vec<(String, usize)> = if legend_w > 0 {
        vec![("glyph colors:".to_string(), 13), (legend, legend_w)]
    } else {
        Vec::new()
    };

    // side-by-side: real-size grid left, zoomed actual + ideal coverage
    // right. Frame rows always render full-width (and color-neutral at both
    // ends), so plain concatenation keeps the panes aligned.
    let mut text = String::new();
    for row in 0..actual_lines.len() {
        match frame_lines.get(row) {
            Some(line) => {
                text.push_str(line);
                text.push_str("  ");
            }
            None => {
                let (s, w) = under_grid
                    .get(row - frame_lines.len())
                    .cloned()
                    .unwrap_or_default();
                text.push_str(&s);
                text.push_str(&" ".repeat(frame_w + 2 - w));
            }
        }
        text.push_str(&actual_lines[row]);
        text.push_str("  ");
        text.push_str(&ideal_lines[row]);
        text.push('\n');
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
    let frac = fraction_part(pos);
    let center_err = match measured_center(&actual) {
        Some(c) => {
            let (dx, dy) = (c.x - pos.x, c.y - pos.y);
            format!(
                "center err=({:+.3}, {:+.3})  |center err|={:.3}",
                dx,
                dy,
                dx.hypot(dy)
            )
        }
        None => "center err=n/a (nothing rendered)".to_string(),
    };
    // one line per metric, so no value is crowded out when others grow
    text.push_str(&format!(
        "pos=({:6.3}, {:6.3})  frac=({:+.3}, {:+.3})  family={}\n",
        pos.x, pos.y, frac.x, frac.y, family_display,
    ));
    text.push_str(&format!(
        "snap=({:+.3}, {:+.3})  snap err=({:+.3}, {:+.3})\n",
        info.snapped_offset.x,
        info.snapped_offset.y,
        info.snapped_offset.x - offset.x,
        info.snapped_offset.y - offset.y,
    ));
    text.push_str(&center_err);
    text.push('\n');
    text.push_str(&format!(
        "area err={:+.3}  (area {:.3})\n",
        metrics.area - 1.0,
        metrics.area,
    ));
    text.push_str(&format!("coverage err={coverage_err:.4}\n"));
    text.push_str(&format!("top spread={:.3}\n", metrics.top_spread));
    text.push_str(&format!("bottom spread={:.3}\n", metrics.bottom_spread));
    text.push_str(&format!("left spread={:.3}\n", metrics.left_spread));
    text.push_str(&format!("right spread={:.3}\n", metrics.right_spread));
    text.push_str(&format!("holes={}\n", metrics.holes));
    text.push_str(&format!(
        "{} speed={:.2}x  switches={}  t={:.1}s{}{}\n",
        state.motion.name(),
        state.speed,
        state.switches,
        state.anim_time.as_secs_f32(),
        if state.paused { "  [paused]" } else { "" },
        if state.fine_drag { "  [fine-drag]" } else { "" },
    ));

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
                // Click places the square and pauses so the placement sticks;
                // drag moves it. Clamped to the visible grid. With a
                // modifier held (or fine_drag toggled), movement is relative
                // instead: cell deltas accumulate at FINE_SCALE, so large
                // mouse movements produce sub-cell square movements.
                Event::Mouse(MouseEvent::Press(_, x, y))
                | Event::Mouse(MouseEvent::Hold(x, y)) => {
                    let limit = ANIMATE_GRID_RADIUS as f32 + 0.5;
                    let clamp = |p: WorldPoint| {
                        euclid::point2(p.x.clamp(-limit, limit), p.y.clamp(-limit, limit))
                    };
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
                    // anchored on every event, so grabbing or releasing the
                    // modifier mid-drag never causes a jump
                    state.last_mouse_cell = Some((x, y));
                    if matches!(event, Event::Mouse(MouseEvent::Press(_, _, _))) {
                        state.paused = true;
                    }
                    dirty = true;
                }
                Event::Mouse(MouseEvent::Release(_, _)) => {
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
                "q=quit space=pause arrows=nudge o=orbit l=line +/-=speed drag=place mod+drag=fine f=toggle-fine"
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
           animate [N]   orbiting square with a zoomed sampled-coverage view\n  \
          \x20      (actual vs ideal, one color per glyph) and one line per\n  \
          \x20      error metric; q quits, space pauses, arrows nudge, o resumes\n  \
          \x20      the orbit, l starts a line trajectory, +/- change speed,\n  \
          \x20      click/drag places the square, holding shift/ctrl/alt while\n  \
          \x20      dragging (or pressing f) gives fine control: large mouse\n  \
          \x20      movements map to sub-cell square movements. Optional frame\n  \
          \x20      count N runs a fixed number of frames, which is also the\n  \
          \x20      mode used when stdout is not a terminal.\n\
         default: pos 0.3 0.7"
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
        None => show_position(euclid::point2(0.3, 0.7)),
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
