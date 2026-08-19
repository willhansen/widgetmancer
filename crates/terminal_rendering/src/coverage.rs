//! Sampled-coverage oracle for the floating square renderer, shared by
//! tests/floating_square_coherence.rs (which asserts edge coherence with it)
//! and the floating_square_debug tool (which displays it). Keeping one
//! oracle means the debug tool can never drift from what the test asserts.
//!
//! Not game-facing API.

use std::sync::OnceLock;

use euclid::vec2;

use crate::glyph_constants::*;
use crate::hextant_blocks::{hextant_character_to_binary, FIRST_HEXTANT, LAST_HEXTANT};
use crate::{
    characters_for_full_square_with_2d_offset, characters_for_full_square_with_2d_offset_forced,
    DoubleChar,
};
use utility::coordinate_frame_conversions::{
    world_point_to_world_square, WorldMove, WorldPoint, WorldSquare,
};

// Samples per world unit. X needs 8 per half-cell for eighth blocks; Y must
// divide both eighths and thirds (hextants), hence 24.
pub const SX: usize = 16;
pub const SY: usize = 24;
// Render window: the 3x3 world squares around the rounded center, which is
// the full set `OffsetSquareDrawable::drawables_for_floating_square_at_point`
// can emit into.
pub const NX: usize = 3 * SX;
pub const NY: usize = 3 * SY;

// Display geometry of the zoomed views: 2x3 samples per pixel, 2 pixels
// per text row.
pub const PX_W: usize = NX / 2; // 24 pixels wide (4 per half-cell)
pub const PX_H: usize = NY / 3; // 24 pixels tall (8 per world square)
pub const TEXT_ROWS: usize = PX_H / 2; // 12 content rows
pub const BITMAP_W: usize = PX_W;

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Rgb(pub u8, pub u8, pub u8);

/// One color per half-cell glyph of the square (max 4 columns x 2 rows).
pub const PALETTE: [Rgb; 8] = [
    Rgb(255, 100, 100),
    Rgb(255, 180, 60),
    Rgb(230, 230, 80),
    Rgb(100, 220, 100),
    Rgb(80, 210, 210),
    Rgb(120, 150, 255),
    Rgb(220, 120, 255),
    Rgb(255, 130, 180),
];
pub const IDEAL_COLOR: Rgb = Rgb(170, 170, 180);
// background checkerboard marking the character cells (one glyph per cell)
pub const CELL_DARK: Rgb = Rgb(26, 26, 34);
pub const CELL_LIGHT: Rgb = Rgb(42, 42, 54);
pub const DOT_COLOR: Rgb = Rgb(70, 70, 80);

/// Checkerboard shade for a character cell, by half-cell column / cell row.
pub fn cell_bg(half_cell_col: usize, cell_row: usize) -> Rgb {
    if (half_cell_col + cell_row) % 2 == 0 {
        CELL_LIGHT
    } else {
        CELL_DARK
    }
}

pub struct Style {
    pub enabled: bool,
}

impl Style {
    pub fn from_env() -> Self {
        let disabled = std::env::var_os("NO_COLOR").is_some()
            || std::env::var_os("CLICOLOR").is_some_and(|v| v == "0")
            || std::env::var_os("TERM").is_some_and(|v| v == "dumb");
        Style {
            enabled: !disabled,
        }
    }
    pub fn fg(&self, Rgb(r, g, b): Rgb) -> String {
        if self.enabled {
            format!("\x1b[38;2;{r};{g};{b}m")
        } else {
            String::new()
        }
    }
    pub fn bg(&self, Rgb(r, g, b): Rgb) -> String {
        if self.enabled {
            format!("\x1b[48;2;{r};{g};{b}m")
        } else {
            String::new()
        }
    }
    pub fn reset(&self) -> &'static str {
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
pub fn glyph_filled(c: char, fx: f32, fy: f32) -> bool {
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
pub fn rendered_neighborhood(pos: WorldPoint) -> ([[DoubleChar; 3]; 3], WorldSquare) {
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

/// Same as `rendered_neighborhood` but with the snap family forced, for
/// scoring "what would family X have done" (family-map baking, debug tool).
#[doc(hidden)]
pub fn rendered_neighborhood_forced(
    pos: WorldPoint,
    family_index: usize,
) -> ([[DoubleChar; 3]; 3], WorldSquare) {
    let center = world_point_to_world_square(pos);
    let mut grid = [[[' '; 2]; 3]; 3];
    for dx in -1..=1i32 {
        for dy in -1..=1i32 {
            let square = center + vec2(dx, dy);
            let offset: WorldMove = pos - square.to_f32();
            grid[(dx + 1) as usize][(dy + 1) as usize] =
                characters_for_full_square_with_2d_offset_forced(offset, family_index);
        }
    }
    (grid, center)
}

/// Symmetric-difference area (in world square units) between the rendered
/// glyph grid and the true 1x1 square at `pos`, on the sampling lattice.
/// This is the objective the family map is baked against.
#[doc(hidden)]
pub fn coverage_error(
    grid: &[[DoubleChar; 3]; 3],
    owners: &[[[Option<usize>; 2]; 3]; 3],
    center: WorldSquare,
    pos: WorldPoint,
) -> f32 {
    let origin: WorldPoint = euclid::point2(center.x as f32 - 1.5, center.y as f32 - 1.5);
    let mut mismatches = 0usize;
    for j in 0..NX {
        for i in 0..NY {
            let wx = origin.x + (j as f32 + 0.5) / SX as f32;
            let wy = origin.y + (i as f32 + 0.5) / SY as f32;
            let ideal = (wx - pos.x).abs() <= 0.5 && (wy - pos.y).abs() <= 0.5;
            if actual_sample(grid, owners, center, wx, wy).0 != ideal {
                mismatches += 1;
            }
        }
    }
    mismatches as f32 / (SX * SY) as f32
}

/// Samples across a half-cell's width (half a world square).
const HX: usize = SX / 2;

/// One candidate glyph's coverage over a half-cell's sample lattice, plus
/// its filled count (for per-character coverage error).
struct GlyphFit {
    c: char,
    bits: [u64; HX * SY / 64],
    count: u32,
}

/// Every glyph the coverage model knows, as a best-fit candidate table.
fn glyph_fits() -> &'static [GlyphFit] {
    static FITS: OnceLock<Vec<GlyphFit>> = OnceLock::new();
    FITS.get_or_init(|| {
        let mut candidates: Vec<char> = vec![SPACE, FULL_BLOCK];
        for blocks in [
            EIGHTH_BLOCKS_FROM_LEFT,
            EIGHTH_BLOCKS_FROM_BOTTOM,
            EIGHTH_BLOCKS_FROM_RIGHT,
            EIGHTH_BLOCKS_FROM_TOP,
        ] {
            // indices 0 and 8 are SPACE and FULL_BLOCK, already listed
            candidates.extend_from_slice(&blocks[1..8]);
        }
        candidates.extend([
            UPPER_ONE_THIRD_BLOCK,
            UPPER_TWO_THIRD_BLOCK,
            LOWER_ONE_THIRD_BLOCK,
            LOWER_TWO_THIRD_BLOCK,
            // quadrant blocks and half blocks
            '\u{2598}', '\u{259D}', '\u{2596}', '\u{2597}', '\u{258C}', '\u{2590}', '\u{2584}', '\u{2580}',
        ]);
        candidates.extend(FIRST_HEXTANT..=LAST_HEXTANT);
        candidates
            .into_iter()
            .map(|c| {
                let mut fit = GlyphFit {
                    c,
                    bits: [0; HX * SY / 64],
                    count: 0,
                };
                for i in 0..SY {
                    for j in 0..HX {
                        // same lattice and fx/fy mapping as actual_sample
                        if glyph_filled(c, (j as f32 + 0.5) / HX as f32, (i as f32 + 0.5) / SY as f32)
                        {
                            let bit = i * HX + j;
                            fit.bits[bit / 64] |= 1 << (bit % 64);
                            fit.count += 1;
                        }
                    }
                }
                fit
            })
            .collect()
    })
}

/// Ideal coverage of the true square over one half-cell's sample lattice:
/// (bitmap, filled count).
fn half_cell_ideal(
    square: WorldSquare,
    half: usize,
    pos: WorldPoint,
) -> ([u64; HX * SY / 64], u32) {
    let half_left = square.x as f32 - 0.5 + 0.5 * half as f32;
    let bottom = square.y as f32 - 0.5;
    let mut bits = [0u64; HX * SY / 64];
    let mut count = 0u32;
    for i in 0..SY {
        for j in 0..HX {
            let wx = half_left + 0.5 * (j as f32 + 0.5) / HX as f32;
            let wy = bottom + (i as f32 + 0.5) / SY as f32;
            if (wx - pos.x).abs() <= 0.5 && (wy - pos.y).abs() <= 0.5 {
                let bit = i * HX + j;
                bits[bit / 64] |= 1 << (bit % 64);
                count += 1;
            }
        }
    }
    (bits, count)
}

/// Mirror of `rendered_neighborhood` with no snap-family restriction and no
/// silhouette-coherence constraint: each half-cell independently takes the
/// glyph (from the whole coverage-modelled set) with the lowest sampled
/// coverage error against the true square, ties broken by filled-area
/// match. This is a candidate rendering *approach*, evaluated by the
/// comparison metrics below (`per_char_coverage_error`, `jaggedness`,
/// area error) exactly like the family-snapped approach is — it does no
/// fitting against those metrics itself.
#[doc(hidden)]
pub fn unrestricted_neighborhood(pos: WorldPoint) -> ([[DoubleChar; 3]; 3], WorldSquare) {
    let center = world_point_to_world_square(pos);
    let mut grid = [[[' '; 2]; 3]; 3];
    for dx in -1..=1i32 {
        for dy in -1..=1i32 {
            let square = center + vec2(dx, dy);
            for half in 0..2 {
                let (ideal, ideal_count) = half_cell_ideal(square, half, pos);
                let key = |fit: &GlyphFit| {
                    let mismatches: u32 = (0..HX * SY / 64)
                        .map(|w| (fit.bits[w] ^ ideal[w]).count_ones())
                        .sum();
                    (mismatches, fit.count.abs_diff(ideal_count))
                };
                grid[(dx + 1) as usize][(dy + 1) as usize][half] = glyph_fits()
                    .iter()
                    .min_by(|a, b| key(a).partial_cmp(&key(b)).unwrap())
                    .unwrap()
                    .c;
            }
        }
    }
    (grid, center)
}

/// Per-character coverage error: for each character half-cell, the absolute
/// difference between the rendered glyph's filled area and the ideal
/// square's filled area within that half-cell, summed (in world square
/// units). Coarser than `coverage_error` (which compares sample by
/// sample): it only asks each half-cell to contain the right *amount* of
/// ink, regardless of where in the cell it sits.
#[doc(hidden)]
pub fn per_char_coverage_error(
    grid: &[[DoubleChar; 3]; 3],
    center: WorldSquare,
    pos: WorldPoint,
) -> f32 {
    let mut total = 0u32;
    for dx in -1..=1i32 {
        for dy in -1..=1i32 {
            let square = center + vec2(dx, dy);
            for half in 0..2 {
                let (_, ideal_count) = half_cell_ideal(square, half, pos);
                let c = grid[(dx + 1) as usize][(dy + 1) as usize][half];
                let rendered = glyph_fits().iter().find(|f| f.c == c).unwrap().count;
                total += rendered.abs_diff(ideal_count);
            }
        }
    }
    total as f32 / (HX * SY) as f32
}

/// Jaggedness of a rendered silhouette: the sum, along each of the four
/// edges, of the perpendicular step lengths between consecutive sample
/// columns/rows — each edge contour's total variation, in world units.
/// A clean rectangle measures 0. Columns/rows with no fill (holes) break
/// the contour and contribute no step.
#[doc(hidden)]
pub fn jaggedness(actual: &FillGrid) -> f32 {
    let mut steps = 0.0f32;
    // top and bottom edges: extreme filled row per sample column
    let col_edges: Vec<Option<(f32, f32)>> = (0..NX)
        .map(|j| {
            let rows: Vec<usize> = (0..NY).filter(|&i| actual.filled(j, i)).collect();
            rows.first()
                .map(|&lo| (actual.wy(lo), actual.wy(*rows.last().unwrap())))
        })
        .collect();
    for pair in col_edges.windows(2) {
        if let (Some((bot0, top0)), Some((bot1, top1))) = (pair[0], pair[1]) {
            steps += (top1 - top0).abs() + (bot1 - bot0).abs();
        }
    }
    // left and right edges: extreme filled column per sample row
    let row_edges: Vec<Option<(f32, f32)>> = (0..NY)
        .map(|i| {
            let cols: Vec<usize> = (0..NX).filter(|&j| actual.filled(j, i)).collect();
            cols.first()
                .map(|&lo| (actual.wx(lo), actual.wx(*cols.last().unwrap())))
        })
        .collect();
    for pair in row_edges.windows(2) {
        if let (Some((left0, right0)), Some((left1, right1))) = (pair[0], pair[1]) {
            steps += (left1 - left0).abs() + (right1 - right0).abs();
        }
    }
    steps
}

/// Assign each non-space half-cell glyph a PALETTE index, scanning
/// top-to-bottom, left-to-right so colors are stable within one report.
/// Indexed [dx+1][dy+1][half].
pub fn assign_colors(grid: &[[DoubleChar; 3]; 3]) -> [[[Option<usize>; 2]; 3]; 3] {
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
///
/// Square lookup uses the same half-up rounding as
/// `world_point_to_world_square` so samples land in the same square the
/// renderer would draw into.
pub fn actual_sample(
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

/// Centroid of the rendered fill, for the measured center error: how far
/// the silhouette's actual middle sits from the true square center.
#[doc(hidden)]
pub fn fill_centroid(actual: &FillGrid) -> Option<WorldPoint> {
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

/// Silhouette metrics over a sampled actual-coverage grid, shared by the
/// coherence test (which asserts on them) and the floating_square_debug
/// tool (which prints them). Not game-facing API.
#[doc(hidden)]
#[derive(Default)]
pub struct Metrics {
    pub top_spread: f32,
    pub bottom_spread: f32,
    pub left_spread: f32,
    pub right_spread: f32,
    pub holes: usize,
    pub area: f32,
    /// per display-column flag: top or bottom edge deviates >1/8 from ideal
    pub ragged_columns: Vec<bool>,
}

impl Metrics {
    pub fn measure(actual: &FillGrid, pos: WorldPoint) -> Self {
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

    pub fn failures(&self) -> Vec<String> {
        let mut out = Vec::new();
        // without this the spread checks below pass vacuously on an empty
        // or degenerate render (no filled columns -> no spread)
        if self.area == 0.0 {
            out.push("no fill at all".to_string());
        } else if (self.area - 1.0).abs() > 0.3 {
            // 0.3 exceeds the worst-case quantization error of any single
            // glyph family (~0.23: x edges to 1/16, y edges to 1/6)
            out.push(format!("area {:.3} too far from 1.0", self.area));
        }
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

    /// One-line summary, as printed by the debug tool and test reports.
    pub fn summary_line(&self) -> String {
        format!(
            "edge spreads: top {:.3}  bottom {:.3}  left {:.3}  right {:.3}   holes: {}   area {:.3} (err {:+.3})",
            self.top_spread,
            self.bottom_spread,
            self.left_spread,
            self.right_spread,
            self.holes,
            self.area,
            self.area - 1.0,
        )
    }
}

/// Sample grid over the render window. Indexed [x][y], y from the bottom.
///
/// Sampling note: sample points sit at half-sample offsets
/// ((j+0.5)/16, (i+0.5)/24), which can never coincide with the
/// half/third/eighth glyph boundaries, so coverage metrics never alias.
pub struct FillGrid {
    pub cells: Vec<Vec<bool>>,
    owners: Vec<Vec<Option<usize>>>,
    origin: WorldPoint, // world coords of the window's bottom-left corner
}

impl FillGrid {
    pub fn sample(origin: WorldPoint, mut f: impl FnMut(f32, f32) -> (bool, Option<usize>)) -> Self {
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

    pub fn filled(&self, j: usize, i: usize) -> bool {
        self.cells[j][i]
    }

    pub fn wy(&self, i: usize) -> f32 {
        self.origin.y + (i as f32 + 0.5) / SY as f32
    }

    pub fn wx(&self, j: usize) -> f32 {
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
    pub fn bitmap_pane(&self, palette: &[Rgb], style: &Style) -> Vec<String> {
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
