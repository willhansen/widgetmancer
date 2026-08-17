//! Visual coherence tests for the floating square renderer.
//!
//! The game draws a floating unit square by picking a block glyph per
//! terminal half-cell (`characters_for_full_square_with_2d_offset`, reached
//! via `OffsetSquareDrawable`). Each half-cell is snapped independently, so
//! the cells of one square can disagree about where the square's edges are,
//! tearing the silhouette (seen at e.g. pos=(2.363, -0.816)).
//!
//! These tests render the square at evenly spaced positions along a line
//! (the reported position is the middle sample) and assert edge coherence:
//! a square's top/bottom/left/right edges must be straight (same position
//! in every column/row) and its fill must be hole-free. Any single glyph
//! family (eighths, hextants, quadrants) applied consistently satisfies
//! this, so a failure means the renderer mixed families within one square.
//!
//! Report layout: first a horizontal strip of the small (character-grid)
//! views of the square at every position, without colors; below it the
//! same strip with each half-cell glyph in its own color; below that the
//! correct rendering (the true square glyphized coherently via hextants)
//! at the same zoom, colored per piece. Columns (positions) with errors
//! are marked with `^^^`, and each failed position gets a zoomed-in row
//! (sampled bitmaps of actual vs ideal coverage) at the bottom. A
//! dark-grey background checkerboard marks the character cells.
//! Set NO_COLOR=1 for plain output.
//!
//! Run with output visible:
//!   cargo test -p terminal_rendering --test floating_square_coherence -- --nocapture

use euclid::vec2;
use terminal_rendering::glyph_constants::*;
use terminal_rendering::hextant_blocks::{
    hextant_array_to_char, hextant_character_to_binary, FIRST_HEXTANT, LAST_HEXTANT,
};
use terminal_rendering::*;
use utility::coordinate_frame_conversions::{WorldMove, WorldPoint, WorldSquare};

// Samples per world unit. X needs 8 per half-cell for eighth blocks; Y must
// divide both eighths and thirds (hextants), hence 24.
const SX: usize = 16;
const SY: usize = 24;
// Render window: the 3x3 world squares around the rounded center, which is
// the full set `OffsetSquareDrawable::drawables_for_floating_square_at_point`
// can emit into.
const NX: usize = 3 * SX;
const NY: usize = 3 * SY;

// Display geometry of the zoomed views: 2x3 samples per pixel, 2 pixels
// per text row.
const PX_W: usize = NX / 2; // 24 pixels wide (4 per half-cell)
const PX_H: usize = NY / 3; // 24 pixels tall (8 per world square)
const TEXT_ROWS: usize = PX_H / 2; // 12 content rows
const BITMAP_W: usize = PX_W;
const GLYPH_W: usize = 6; // small views: 3 world squares = 6 half-cell chars

/// One color per half-cell glyph of the square (max 4 columns x 2 rows).
const PALETTE: [(u8, u8, u8); 8] = [
    (255, 100, 100),
    (255, 180, 60),
    (230, 230, 80),
    (100, 220, 100),
    (80, 210, 210),
    (120, 150, 255),
    (220, 120, 255),
    (255, 130, 180),
];
const IDEAL_COLOR: (u8, u8, u8) = (170, 170, 180);
// background checkerboard marking the character cells (one glyph per cell)
const CELL_DARK: (u8, u8, u8) = (26, 26, 34);
const CELL_LIGHT: (u8, u8, u8) = (42, 42, 54);
const DOT_COLOR: (u8, u8, u8) = (70, 70, 80);
const MARKER_COLOR: (u8, u8, u8) = (255, 220, 80);

/// Checkerboard shade for a character cell, by half-cell column / cell row.
fn cell_bg(half_cell_col: usize, cell_row: usize) -> (u8, u8, u8) {
    if (half_cell_col + cell_row) % 2 == 0 {
        CELL_LIGHT
    } else {
        CELL_DARK
    }
}

struct Style {
    enabled: bool,
}

impl Style {
    fn from_env() -> Self {
        Style {
            enabled: std::env::var_os("NO_COLOR").is_none(),
        }
    }
    fn fg(&self, (r, g, b): (u8, u8, u8)) -> String {
        if self.enabled {
            format!("\x1b[38;2;{r};{g};{b}m")
        } else {
            String::new()
        }
    }
    fn bg(&self, (r, g, b): (u8, u8, u8)) -> String {
        if self.enabled {
            format!("\x1b[48;2;{r};{g};{b}m")
        } else {
            String::new()
        }
    }
    fn reset(&self) -> &'static str {
        if self.enabled {
            "\x1b[0m"
        } else {
            ""
        }
    }
}

/// Exact coverage model for every glyph the renderer can emit.
/// `fx`, `fy` are in [0, 1) within the character cell; `fy` is measured
/// from the bottom (world +y is up).
fn glyph_filled(c: char, fx: f32, fy: f32) -> bool {
    if c == SPACE {
        return false;
    }
    if c == FULL_BLOCK {
        return true;
    }
    // eighth blocks, all four orientations (partials only; the arrays also
    // contain SPACE at 0 and FULL_BLOCK at 8, handled above)
    for k in 1..8usize {
        let k = k as f32;
        if c == EIGHTH_BLOCKS_FROM_LEFT[k as usize] {
            return fx * 8.0 < k;
        }
        if c == EIGHTH_BLOCKS_FROM_RIGHT[k as usize] {
            return (1.0 - fx) * 8.0 < k;
        }
        if c == EIGHTH_BLOCKS_FROM_BOTTOM[k as usize] {
            return fy * 8.0 < k;
        }
        if c == EIGHTH_BLOCKS_FROM_TOP[k as usize] {
            return (1.0 - fy) * 8.0 < k;
        }
    }
    match c {
        UPPER_ONE_THIRD_BLOCK => return fy > 2.0 / 3.0,
        UPPER_TWO_THIRD_BLOCK => return fy > 1.0 / 3.0,
        LOWER_ONE_THIRD_BLOCK => return fy < 1.0 / 3.0,
        LOWER_TWO_THIRD_BLOCK => return fy < 2.0 / 3.0,
        // quadrant blocks and half blocks
        '▖' => return fx < 0.5 && fy < 0.5,
        '▗' => return fx >= 0.5 && fy < 0.5,
        '▘' => return fx < 0.5 && fy >= 0.5,
        '▝' => return fx >= 0.5 && fy >= 0.5,
        '▌' => return fx < 0.5,
        '▐' => return fx >= 0.5,
        '▄' => return fy < 0.5,
        '▀' => return fy >= 0.5,
        _ => {}
    }
    if (FIRST_HEXTANT..=LAST_HEXTANT).contains(&c) {
        // sextant bits: bit = row * 2 + col, row 0 = top, col 0 = left
        let bits = hextant_character_to_binary(c);
        let col = if fx < 0.5 { 0 } else { 1 };
        let row = if fy >= 2.0 / 3.0 {
            0
        } else if fy >= 1.0 / 3.0 {
            1
        } else {
            2
        };
        return bits & (1 << (row * 2 + col)) != 0;
    }
    panic!("no coverage model for glyph {c:?} (U+{:04X})", c as u32);
}

/// Mirror of `OffsetSquareDrawable::drawables_for_floating_square_at_point`:
/// the 3x3 neighborhood of the rounded center square. Indexed [dx+1][dy+1].
fn rendered_neighborhood(pos: WorldPoint) -> ([[DoubleChar; 3]; 3], WorldSquare) {
    let center = world_point_to_world_square(pos);
    let mut grid = [[[' '; 2]; 3]; 3];
    for dx in -1..=1i32 {
        for dy in -1..=1i32 {
            let square = center + vec2(dx, dy);
            let offset: WorldMove = pos - square.to_f32();
            grid[(dx + 1) as usize][(dy + 1) as usize] =
                characters_for_full_square_with_2d_offset(offset);
        }
    }
    (grid, center)
}

/// Assign each non-space half-cell glyph a PALETTE index, scanning
/// top-to-bottom, left-to-right so colors are stable within one report.
/// Indexed [dx+1][dy+1][half].
fn assign_colors(grid: &[[DoubleChar; 3]; 3]) -> [[[Option<usize>; 2]; 3]; 3] {
    let mut owners = [[[None; 2]; 3]; 3];
    let mut next = 0;
    for dy in [1i32, 0, -1] {
        for dx in -1..=1i32 {
            for half in 0..2 {
                if grid[(dx + 1) as usize][(dy + 1) as usize][half] != SPACE {
                    owners[(dx + 1) as usize][(dy + 1) as usize][half] = Some(next);
                    next += 1;
                }
            }
        }
    }
    owners
}

/// (filled, color index) of the rendered output at a world point.
fn actual_sample(
    grid: &[[DoubleChar; 3]; 3],
    owners: &[[[Option<usize>; 2]; 3]; 3],
    center: WorldSquare,
    wx: f32,
    wy: f32,
) -> (bool, Option<usize>) {
    let sx = (wx + 0.5).floor() as i32;
    let sy = (wy + 0.5).floor() as i32;
    let (dx, dy) = (sx - center.x, sy - center.y);
    if !(-1..=1).contains(&dx) || !(-1..=1).contains(&dy) {
        return (false, None);
    }
    let half = if wx < sx as f32 { 0 } else { 1 };
    let c = grid[(dx + 1) as usize][(dy + 1) as usize][half];
    let cell_left = sx as f32 - 0.5 + 0.5 * half as f32;
    let fx = (wx - cell_left) * 2.0;
    let fy = wy - (sy as f32 - 0.5);
    (
        glyph_filled(c, fx, fy),
        owners[(dx + 1) as usize][(dy + 1) as usize][half],
    )
}

/// Sample grid over the render window. Indexed [x][y], y from the bottom.
struct FillGrid {
    cells: Vec<Vec<bool>>,
    owners: Vec<Vec<Option<usize>>>,
    origin: WorldPoint, // world coords of the window's bottom-left corner
}

impl FillGrid {
    fn sample(origin: WorldPoint, mut f: impl FnMut(f32, f32) -> (bool, Option<usize>)) -> Self {
        let mut cells = Vec::new();
        let mut owners = Vec::new();
        for j in 0..NX {
            let mut col = Vec::new();
            let mut owner_col = Vec::new();
            for i in 0..NY {
                let wx = origin.x + (j as f32 + 0.5) / SX as f32;
                let wy = origin.y + (i as f32 + 0.5) / SY as f32;
                let (filled, owner) = f(wx, wy);
                col.push(filled);
                owner_col.push(owner);
            }
            cells.push(col);
            owners.push(owner_col);
        }
        FillGrid {
            cells,
            owners,
            origin,
        }
    }

    fn filled(&self, j: usize, i: usize) -> bool {
        self.cells[j][i]
    }

    fn wy(&self, i: usize) -> f32 {
        self.origin.y + (i as f32 + 0.5) / SY as f32
    }

    fn wx(&self, j: usize) -> f32 {
        self.origin.x + (j as f32 + 0.5) / SX as f32
    }

    /// (filled, owner) of one display pixel by majority vote. The pixel
    /// grid aligns exactly with the half-cell grid (4x8 pixels per
    /// half-cell), so the owner is unambiguous.
    fn pixel(&self, px: usize, py: usize) -> Option<Option<usize>> {
        // py 0 = top pixel row
        let mut count = 0;
        let mut owner = None;
        for dj in 0..2 {
            for di in 0..3 {
                let j = px * 2 + dj;
                let i = NY - 1 - (py * 3 + di);
                if self.cells[j][i] {
                    count += 1;
                    owner = self.owners[j][i];
                }
            }
        }
        if count >= 3 {
            Some(owner)
        } else {
            None
        }
    }

    /// Zoomed view as styled text rows over a dark-grey checkerboard that
    /// marks the character cells (4x8 pixels per half-cell). Filled pixels
    /// take their glyph's color; a text cell covering two differently-owned
    /// pixels shows the upper pixel as fg and the lower as bg.
    fn bitmap_pane(&self, palette: &[(u8, u8, u8)], style: &Style) -> Vec<String> {
        let color_of = |owner: Option<usize>| palette[owner.unwrap_or(0) % palette.len()];
        let cell = |t: usize, px: usize| -> String {
            let bg = style.bg(cell_bg(px / 4, t / 4));
            let up = self.pixel(px, 2 * t);
            let lo = self.pixel(px, 2 * t + 1);
            match (up, lo) {
                (Some(ou), Some(ol)) if ou == ol => {
                    format!("{}{}█", bg, style.fg(color_of(ou)))
                }
                // bg carries the lower pixel's glyph color, not the cell shade
                (Some(ou), Some(ol)) => {
                    format!("{}{}▀", style.bg(color_of(ol)), style.fg(color_of(ou)))
                }
                (Some(ou), None) => format!("{}{}▀", bg, style.fg(color_of(ou))),
                (None, Some(ol)) => format!("{}{}▄", bg, style.fg(color_of(ol))),
                // a shaded space would be invisible whenever the bg colors
                // don't survive (no truecolor, pipes, copy-paste)
                (None, None) => format!("{}{}·", bg, style.fg(DOT_COLOR)),
            }
        };
        (0..TEXT_ROWS)
            .map(|t| {
                let mut line: String = (0..PX_W).map(|px| cell(t, px)).collect();
                line.push_str(style.reset());
                line
            })
            .collect()
    }
}

#[derive(Default)]
struct Metrics {
    top_spread: f32,
    bottom_spread: f32,
    left_spread: f32,
    right_spread: f32,
    holes: usize,
    area: f32,
    /// per display-column flag: top or bottom edge deviates >1/8 from ideal
    ragged_columns: Vec<bool>,
}

impl Metrics {
    fn measure(actual: &FillGrid, pos: WorldPoint) -> Self {
        let mut m = Metrics::default();
        let mut tops = Vec::new();
        let mut bottoms = Vec::new();
        let mut column_info = Vec::new(); // (top, bottom) per sample column
        for j in 0..NX {
            let filled_rows: Vec<usize> = (0..NY).filter(|&i| actual.filled(j, i)).collect();
            if filled_rows.is_empty() {
                column_info.push(None);
                continue;
            }
            let (lo, hi) = (*filled_rows.first().unwrap(), *filled_rows.last().unwrap());
            m.holes += (hi - lo + 1) - filled_rows.len();
            let (top, bottom) = (actual.wy(hi), actual.wy(lo));
            tops.push(top);
            bottoms.push(bottom);
            column_info.push(Some((top, bottom)));
        }
        let spread = |v: &Vec<f32>| {
            v.iter().cloned().fold(f32::NEG_INFINITY, f32::max)
                - v.iter().cloned().fold(f32::INFINITY, f32::min)
        };
        m.top_spread = spread(&tops);
        m.bottom_spread = spread(&bottoms);

        let mut lefts = Vec::new();
        let mut rights = Vec::new();
        for i in 0..NY {
            let filled_cols: Vec<usize> = (0..NX).filter(|&j| actual.filled(j, i)).collect();
            if filled_cols.is_empty() {
                continue;
            }
            lefts.push(actual.wx(*filled_cols.first().unwrap()));
            rights.push(actual.wx(*filled_cols.last().unwrap()));
        }
        m.left_spread = spread(&lefts);
        m.right_spread = spread(&rights);

        let filled_count: usize = actual.cells.iter().flatten().filter(|&&b| b).count();
        m.area = filled_count as f32 / (SX * SY) as f32;

        let (ideal_top, ideal_bottom) = (pos.y + 0.5, pos.y - 0.5);
        m.ragged_columns = (0..PX_W)
            .map(|px| {
                [2 * px, 2 * px + 1].iter().any(|&j| {
                    column_info[j].is_some_and(|(top, bottom)| {
                        (top - ideal_top).abs() > 0.125 || (bottom - ideal_bottom).abs() > 0.125
                    })
                })
            })
            .collect();
        m
    }

    fn failures(&self) -> Vec<String> {
        let mut out = Vec::new();
        for (name, spread) in [
            ("top", self.top_spread),
            ("bottom", self.bottom_spread),
            ("left", self.left_spread),
            ("right", self.right_spread),
        ] {
            if spread > 1e-6 {
                out.push(format!("{name}-edge spread {spread:.3}"));
            }
        }
        if self.holes > 0 {
            out.push(format!("{} hole(s) in the fill", self.holes));
        }
        out
    }
}

struct PositionAnalysis {
    pos: WorldPoint,
    grid: [[DoubleChar; 3]; 3],
    owners: [[[Option<usize>; 2]; 3]; 3],
    center: WorldSquare,
    actual: FillGrid,
    ideal: FillGrid,
    metrics: Metrics,
    failures: Vec<String>,
}

fn analyze(pos: WorldPoint) -> PositionAnalysis {
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
    let metrics = Metrics::measure(&actual, pos);
    let failures = metrics.failures();
    PositionAnalysis {
        pos,
        grid,
        owners,
        center,
        actual,
        ideal,
        metrics,
        failures,
    }
}

/// Small view, monochrome: the 3x3 world squares as 6 half-cell chars,
/// the square in uniform grey over the cell checkerboard.
fn plain_view_lines(grid: &[[DoubleChar; 3]; 3], style: &Style) -> Vec<String> {
    [1i32, 0, -1]
        .iter()
        .map(|&dy| {
            let mut line = String::new();
            for dx in -1..=1i32 {
                for half in 0..2 {
                    line.push_str(&style.bg(cell_bg((dx + 1) as usize * 2 + half, (1 - dy) as usize)));
                    let c = grid[(dx + 1) as usize][(dy + 1) as usize][half];
                    if c == SPACE {
                        line.push_str(&format!("{}·", style.fg(DOT_COLOR)));
                    } else {
                        line.push_str(&format!("{}{c}", style.fg(IDEAL_COLOR)));
                    }
                }
            }
            line.push_str(style.reset());
            line
        })
        .collect()
}

/// Small view, colored: each glyph in its assigned color over the cell
/// checkerboard.
fn colored_view_lines(
    grid: &[[DoubleChar; 3]; 3],
    owners: &[[[Option<usize>; 2]; 3]; 3],
    style: &Style,
) -> Vec<String> {
    [1i32, 0, -1]
        .iter()
        .map(|&dy| {
            let mut line = String::new();
            for dx in -1..=1i32 {
                for half in 0..2 {
                    line.push_str(&style.bg(cell_bg((dx + 1) as usize * 2 + half, (1 - dy) as usize)));
                    let c = grid[(dx + 1) as usize][(dy + 1) as usize][half];
                    match owners[(dx + 1) as usize][(dy + 1) as usize][half] {
                        Some(idx) => line.push_str(&format!("{}{c}", style.fg(PALETTE[idx]))),
                        None => line.push_str(&format!("{}·", style.fg(DOT_COLOR))),
                    }
                }
            }
            line.push_str(style.reset());
            line
        })
        .collect()
}

/// The correct rendering of one half-cell: the true unit square's coverage
/// of the cell, glyphized coherently as a hextant (2x3 sub-cell majority).
fn correct_view_char(pos: WorldPoint, square: WorldSquare, half: usize) -> Option<char> {
    let cell_left = square.x as f32 - 0.5 + 0.5 * half as f32;
    let cell_bottom = square.y as f32 - 0.5;
    let mut array = [[false; 2]; 3];
    let mut any = false;
    for row in 0..3 {
        for col in 0..2 {
            let mut count = 0;
            for sy in 0..4 {
                for sx in 0..4 {
                    let fx = (col as f32 + (sx as f32 + 0.5) / 4.0) / 2.0;
                    let fy = ((2 - row) as f32 + (sy as f32 + 0.5) / 4.0) / 3.0;
                    let wx = cell_left + fx * 0.5;
                    let wy = cell_bottom + fy;
                    if (wx - pos.x).abs() <= 0.5 && (wy - pos.y).abs() <= 0.5 {
                        count += 1;
                    }
                }
            }
            array[row][col] = count >= 8;
            any |= array[row][col];
        }
    }
    any.then(|| hextant_array_to_char(array))
}

/// Small view of the correct rendering, colored per piece. Pieces keep the
/// color of the same cell in the actual view; pieces the actual render is
/// missing entirely are gray.
fn correct_view_lines(a: &PositionAnalysis, style: &Style) -> Vec<String> {
    [1i32, 0, -1]
        .iter()
        .map(|&dy| {
            let mut line = String::new();
            for dx in -1..=1i32 {
                for half in 0..2 {
                    line.push_str(&style.bg(cell_bg((dx + 1) as usize * 2 + half, (1 - dy) as usize)));
                    let square = a.center + vec2(dx, dy);
                    match correct_view_char(a.pos, square, half) {
                        Some(c) => {
                            let color = a.owners[(dx + 1) as usize][(dy + 1) as usize][half]
                                .map(|idx| PALETTE[idx])
                                .unwrap_or(IDEAL_COLOR);
                            line.push_str(&format!("{}{c}", style.fg(color)));
                        }
                        None => line.push_str(&format!("{}·", style.fg(DOT_COLOR))),
                    }
                }
            }
            line.push_str(style.reset());
            line
        })
        .collect()
}

/// Pad a possibly ANSI-styled line to a visible width.
fn pad(line: &str, visible_len: usize, width: usize) -> String {
    format!("{}{}", line, " ".repeat(width.saturating_sub(visible_len)))
}

/// Zoomed-in row for one position: glyph view, sampled actual coverage,
/// sampled ideal coverage, deviation markers, metrics.
fn zoomed_report(idx: usize, count: usize, a: &PositionAnalysis, style: &Style) -> String {
    let frac = fraction_part(a.pos);
    let status = if a.failures.is_empty() { "pass" } else { "FAIL" };
    let mut out = format!(
        "── [{idx}/{count}] pos=({:.3}, {:.3})  frac=({:+.3}, {:+.3})  ── {status} ──\n",
        a.pos.x, a.pos.y, frac.x, frac.y
    );
    out.push_str(&format!(
        "  char grid: world squares x={}..={}, y={}..={}   (background checkerboard = character cells)\n",
        a.center.x - 1,
        a.center.x + 1,
        a.center.y - 1,
        a.center.y + 1
    ));
    out.push_str(&format!(
        "  {:GLYPH_W$}  {:BITMAP_W$}  {}\n",
        "glyphs", "actual coverage", "ideal (true square)"
    ));

    let glyphs = colored_view_lines(&a.grid, &a.owners, style);
    let actual_lines = a.actual.bitmap_pane(&PALETTE, style);
    let ideal_lines = a.ideal.bitmap_pane(&[IDEAL_COLOR], style);
    let height = actual_lines.len().max(glyphs.len());
    for row in 0..height {
        let g = glyphs.get(row).map(String::as_str).unwrap_or("");
        let ac = actual_lines.get(row).map(String::as_str).unwrap_or("");
        let id = ideal_lines.get(row).map(String::as_str).unwrap_or("");
        out.push_str(&format!(
            "  {}  {}  {}\n",
            pad(g, if row < glyphs.len() { GLYPH_W } else { 0 }, GLYPH_W),
            pad(ac, if row < actual_lines.len() { BITMAP_W } else { 0 }, BITMAP_W),
            id
        ));
    }

    let markers: String = a
        .metrics
        .ragged_columns
        .iter()
        .map(|&ragged| if ragged { '^' } else { ' ' })
        .collect();
    if markers.trim().len() > 0 {
        out.push_str(&format!(
            "  {:GLYPH_W$}  {}{markers}{}  <-- columns >1/8 off the true edge\n",
            "",
            style.fg(MARKER_COLOR),
            style.reset()
        ));
    }
    out.push_str(&format!(
        "  edge spreads: top {:.3}  bottom {:.3}  left {:.3}  right {:.3}   holes: {}   area {:.3} (err {:+.3})\n",
        a.metrics.top_spread,
        a.metrics.bottom_spread,
        a.metrics.left_spread,
        a.metrics.right_spread,
        a.metrics.holes,
        a.metrics.area,
        a.metrics.area - 1.0,
    ));
    out
}

/// Renders the square along a line through the reported tearing position
/// (2.363, -0.816) and asserts the silhouette stays rectangular: straight
/// edges, no holes. Currently FAILS at several positions — that is the
/// demonstration of the bug, and this test should pass once the renderer
/// picks one glyph family per square instead of per half-cell.
#[test]
fn test_square_silhouette_stays_rectangular_along_motion_line() {
    let base = euclid::point2(2.363f32, -0.816f32);
    let step = vec2(0.06f32, 0.03f32);
    let positions: Vec<WorldPoint> = (-4..=4)
        .map(|i| base + step * i as f32)
        .collect();
    let analyses: Vec<PositionAnalysis> = positions.iter().map(|&p| analyze(p)).collect();
    let style = Style::from_env();

    let mut report = String::from(
        "\nfloating square along a line; the reported case is [5].\n\
         A square must have a straight top/bottom/left/right edge and no holes.\n\n",
    );

    // strips of small views, one 6-wide view per position
    let n = analyses.len();
    report.push_str("          ");
    for k in 0..n {
        report.push_str(&format!("{:^7}", format!("[{}]", k + 1)));
    }
    report.push('\n');
    let strips: Vec<(&str, Vec<Vec<String>>)> = vec![
        (
            "plain",
            analyses
                .iter()
                .map(|a| plain_view_lines(&a.grid, &style))
                .collect(),
        ),
        (
            "colored",
            analyses
                .iter()
                .map(|a| colored_view_lines(&a.grid, &a.owners, &style))
                .collect(),
        ),
        (
            "correct",
            analyses.iter().map(|a| correct_view_lines(a, &style)).collect(),
        ),
    ];
    for (label, views) in &strips {
        for row in 0..3 {
            let label = if row == 1 { *label } else { "" };
            report.push_str(&format!("  {:6}  ", label));
            report.push_str(
                &views
                    .iter()
                    .map(|v| v[row].as_str())
                    .collect::<Vec<_>>()
                    .join(" "),
            );
            report.push('\n');
        }
        report.push('\n');
    }

    // mark the columns (positions) with errors
    report.push_str("          ");
    for a in &analyses {
        if a.failures.is_empty() {
            report.push_str("       ");
        } else {
            report.push_str(&format!("{} ^^^^  {}", style.fg(MARKER_COLOR), style.reset()));
        }
    }
    report.push_str("\n\n");

    // legend
    for (k, a) in analyses.iter().enumerate() {
        let frac = fraction_part(a.pos);
        let status = if a.failures.is_empty() {
            "pass".to_string()
        } else {
            format!("FAIL: {}", a.failures.join(", "))
        };
        report.push_str(&format!(
            "  [{}] pos=({:.3}, {:.3})  frac=({:+.3}, {:+.3})  {status}\n",
            k + 1,
            a.pos.x,
            a.pos.y,
            frac.x,
            frac.y
        ));
    }

    // zoomed-in rows for the failed positions only
    let failed: Vec<(usize, &PositionAnalysis)> = analyses
        .iter()
        .enumerate()
        .filter(|(_, a)| !a.failures.is_empty())
        .collect();
    if !failed.is_empty() {
        report.push_str("\nzoomed views of failed positions:\n\n");
        for (k, a) in &failed {
            report.push_str(&zoomed_report(k + 1, n, a, &style));
            report.push('\n');
        }
    }

    println!("{report}");
    assert!(
        failed.is_empty(),
        "square silhouette is not rectangular at {}/{} sampled positions:\n{}\n\nfull report:\n{}",
        failed.len(),
        n,
        failed
            .iter()
            .map(|(k, a)| format!(
                "  [{}/{}] pos=({:.3}, {:.3}): {}",
                k + 1,
                n,
                a.pos.x,
                a.pos.y,
                a.failures.join(", ")
            ))
            .collect::<Vec<_>>()
            .join("\n"),
        report
    );
}

/// The 1D path (`characters_for_full_square_with_looping_1d_offset`, used by
/// shockwave animations) should move the rendered region monotonically with
/// the offset. This one is expected to pass already; it guards the path the
/// coherence fix must not regress.
#[test]
fn test_1d_offset_rendering_moves_monotonically() {
    // vertical: as the square moves up, the top edge inside the cell sinks
    let mut prev_top: Option<f32> = None;
    for step in 0..=99 {
        let t = step as f32 / 100.0;
        let chars = characters_for_full_square_with_looping_1d_offset(STEP_UP.into(), t);
        let top = (0..SY)
            .filter(|&i| glyph_filled(chars[0], 0.5, (i as f32 + 0.5) / SY as f32))
            .map(|i| (i as f32 + 0.5) / SY as f32)
            .last();
        if let (Some(prev), Some(top)) = (prev_top, top) {
            assert!(
                top <= prev + 1e-6,
                "vertical top edge jumped up: t={t:.2} edge {top:.3} after {prev:.3}"
            );
        }
        prev_top = top.or(prev_top);
    }

    // horizontal: as the square moves right, both edges move right
    let mut prev_edges: Option<(f32, f32)> = None;
    for step in 0..=99 {
        let t = step as f32 / 100.0;
        let chars = characters_for_full_square_with_looping_1d_offset(STEP_RIGHT.into(), t);
        let filled_x: Vec<f32> = (0..2 * SX)
            .map(|k| -0.5 + (k as f32 + 0.5) / (2 * SX) as f32)
            .filter(|&wx| {
                let half = if wx < 0.0 { 0 } else { 1 };
                let fx = (wx - (-0.5 + 0.5 * half as f32)) * 2.0;
                glyph_filled(chars[half], fx, 0.5)
            })
            .collect();
        if filled_x.is_empty() {
            continue;
        }
        let edges = (*filled_x.first().unwrap(), *filled_x.last().unwrap());
        if let Some(prev) = prev_edges {
            assert!(
                edges.0 >= prev.0 - 1e-6 && edges.1 >= prev.1 - 1e-6,
                "horizontal edges moved left: t={t:.2} {edges:?} after {prev:?}"
            );
        }
        prev_edges = Some(edges);
    }
}
