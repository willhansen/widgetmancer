//! Sampled-coverage oracle for the floating square renderer, shared by
//! tests/floating_square_coherence.rs (which asserts edge coherence with it)
//! and the floating_square_debug tool (which displays it). Keeping one
//! oracle means the debug tool can never drift from what the test asserts.
//!
//! Not game-facing API.

use std::sync::OnceLock;

use euclid::vec2;
use ordered_float::OrderedFloat;

use crate::glyph_constants::*;
use crate::hextant_blocks::{
    hextant_array_to_char, hextant_character_to_binary, FIRST_HEXTANT, LAST_HEXTANT,
};
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

/// The "charwise" approach: for each character (half-cell) the true
/// square overlaps, independently pick the glyph whose filled region best
/// fits the square's overlap with that cell — exact, analytic, and with no
/// regard for sibling characters, so silhouettes are jagged by design (the
/// family-snapped render path is the coherent one). A comparison candidate
/// for the debug tool and tests, not a game-facing render path.
///
/// Geometry: a character is a half-cell (0.5 world units wide, 1 row tall)
/// and the square is exactly 2 half-cells by 1 row, so its overlap with a
/// character is always an *anchored* rectangle — on each axis either the
/// full cell or flush against exactly one cell edge; it can never float in
/// the middle. Every glyph's filled region is anchored the same way
/// (strips flush to one edge, quadrant blocks and hextants to a corner),
/// so ideal and candidate always share an anchor corner.
#[doc(hidden)]
pub fn charwise_neighborhood(pos: WorldPoint) -> ([[DoubleChar; 3]; 3], WorldSquare) {
    charwise_neighborhood_weighted(pos, 0.0)
}

/// Extra cost per cell-coordinate unit of protrusion in the linear shaped
/// charwise variant (see `charwise_shaped_neighborhood`). 1.0 means a
/// glyph sticking 0.2 cells past the true edge pays as much as mis-covering
/// 0.2 cells of area — enough that thin spikes lose to evenly distributed
/// error, without drowning the xor term.
pub const CHARWISE_PROTRUSION_WEIGHT: f32 = 1.0;

/// Weight of the squared-protrusion variant (`charwise_protrusion_squared_neighborhood`).
/// Since cell distances are < 1, `W·d²` only bites harder than linear `d`
/// once d > 1/W: with 4.0 the quadratic discouragement kicks in past 0.25
/// cells of protrusion — shallow overshoot is nearly free, deep spikes are
/// hammered quadratically.
pub const CHARWISE_PROTRUSION_SQUARED_WEIGHT: f32 = 4.0;

/// The shaped charwise variant: like `charwise_neighborhood`, but each
/// cell's pick minimizes xor area plus `CHARWISE_PROTRUSION_WEIGHT` times
/// how far the glyph sticks out past the true square's overlap with the
/// cell. A long thin protrusion has tiny xor but large max distance, so it
/// is heavily disincentivized; a small overextension along the true edge
/// has small max distance and pays mostly its (small) xor. Still per-cell
/// and analytic — no sibling awareness.
#[doc(hidden)]
pub fn charwise_shaped_neighborhood(pos: WorldPoint) -> ([[DoubleChar; 3]; 3], WorldSquare) {
    charwise_neighborhood_penalty(pos, CHARWISE_PROTRUSION_WEIGHT, false)
}

/// The squared-protrusion variant: cost = xor + weight × d². Progressive
/// instead of flat: shallow overshoot is cheaper than under the linear
/// penalty (d² < d for d < 1), deep protrusion far more expensive.
#[doc(hidden)]
pub fn charwise_protrusion_squared_neighborhood(
    pos: WorldPoint,
) -> ([[DoubleChar; 3]; 3], WorldSquare) {
    charwise_neighborhood_penalty(pos, CHARWISE_PROTRUSION_SQUARED_WEIGHT, true)
}

fn charwise_neighborhood_penalty(
    pos: WorldPoint,
    weight: f32,
    squared: bool,
) -> ([[DoubleChar; 3]; 3], WorldSquare) {
    let center = world_point_to_world_square(pos);
    let mut grid = [[[' '; 2]; 3]; 3];
    for dx in -1..=1i32 {
        for dy in -1..=1i32 {
            let square = center + vec2(dx, dy);
            for half in 0..2 {
                grid[(dx + 1) as usize][(dy + 1) as usize][half] =
                    charwise_glyph_parts(pos, square, half, weight, squared).0;
            }
        }
    }
    (grid, center)
}

fn charwise_neighborhood_weighted(
    pos: WorldPoint,
    weight: f32,
) -> ([[DoubleChar; 3]; 3], WorldSquare) {
    charwise_neighborhood_penalty(pos, weight, false)
}

/// The method's own objective summed over the 3x3 neighborhood: per-cell
/// xor area plus the protrusion penalty in the method's shape (linear d or
/// squared d²). This is the number the picker minimizes cell by cell —
/// "the error used for rendering" in the debug tool. Cell units (a
/// half-cell is 0.5 x 1), not world square units.
#[doc(hidden)]
pub fn charwise_objective(pos: WorldPoint, weight: f32, squared: bool) -> f32 {
    let center = world_point_to_world_square(pos);
    let mut total = 0.0f32;
    for dx in -1..=1i32 {
        for dy in -1..=1i32 {
            let square = center + vec2(dx, dy);
            for half in 0..2 {
                let (_, err, d) = charwise_glyph_parts(pos, square, half, weight, squared);
                total += err + weight * if squared { d * d } else { d };
            }
        }
    }
    total
}

/// Distance from an anchor-frame point (u, v) to the ideal anchored
/// rectangle [0, w] x [0, h]. Distance to a rectangle is convex and this
/// function is non-decreasing in u and v, so the farthest point of any
/// candidate's filled rectangle or sextant from the ideal is its corner
/// farthest from the anchor — one formula covers every candidate class.
///
/// Cell coordinates are anisotropic (u spans the half-cell width, v the
/// full row height), so this is not a true world distance; acceptable for
/// a shape heuristic, and it never feeds the game render path.
fn protrusion(u: f32, v: f32, w: f32, h: f32) -> f32 {
    (u - w).max(0.0).hypot((v - h).max(0.0))
}

/// One character's glyph pick plus its objective parts: the winning
/// candidate's xor area (cell units) and protrusion distance. `weight`/
/// `squared` shape the comparison key (see `charwise_objective`); weight 0
/// reproduces the plain xor argmin exactly, since err + 0.0·penalty == err
/// and all tie orderings are unchanged.
///
/// Each geometry class contributes its provably best member: for strips
/// the xor is monotone in the strip size on either side of the ideal's,
/// so the optimum is one of the two grid neighbors; the xor-optimal
/// hextant is the per-sextant majority rule (sextants are disjoint, so
/// each decides independently). The overall winner is the minimum over
/// the four class candidates.
fn charwise_glyph_parts(
    pos: WorldPoint,
    square: WorldSquare,
    half: usize,
    weight: f32,
    squared: bool,
) -> (char, f32, f32) {
    // overlap of the true square with this half-cell in cell coordinates:
    // x in [0, 1] across the half-cell, y in [0, 1] up the row
    let cell_left = square.x as f32 - 0.5 + 0.5 * half as f32;
    let cell_bottom = square.y as f32 - 0.5;
    let (x0, x1) = (
        ((pos.x - 0.5 - cell_left) * 2.0).clamp(0.0, 1.0),
        ((pos.x + 0.5 - cell_left) * 2.0).clamp(0.0, 1.0),
    );
    let (y0, y1) = (
        (pos.y - 0.5 - cell_bottom).clamp(0.0, 1.0),
        (pos.y + 0.5 - cell_bottom).clamp(0.0, 1.0),
    );
    if x1 <= x0 || y1 <= y0 {
        return (SPACE, 0.0, 0.0);
    }
    if x0 <= 0.0 && x1 >= 1.0 && y0 <= 0.0 && y1 >= 1.0 {
        return (FULL_BLOCK, 0.0, 0.0);
    }
    // w and h are the overlap extents measured from the touched edges;
    // the ideal is then the corner-anchored rectangle [0, w] x [0, h]
    debug_assert!(x0 <= 0.0 || x1 >= 1.0, "overlap floats in x");
    debug_assert!(y0 <= 0.0 || y1 >= 1.0, "overlap floats in y");
    let from_left = x0 <= 0.0;
    let from_bottom = y0 <= 0.0;
    let (w, h) = (
        if from_left { x1 } else { 1.0 - x0 },
        if from_bottom { y1 } else { 1.0 - y0 },
    );

    // symmetric-difference area between the ideal [0, w] x [0, h] and a
    // candidate anchored rectangle [0, a] x [0, b] (strips are the a=1
    // or b=1 cases, the quadrant block a=b=1/2)
    let xor = |a: f32, b: f32| w.max(a) * h.max(b) - w.min(a) * h.min(b);
    // the penalized comparison key: at weight 0 this is plain xor, and
    // every `+ 0.0 * d` is exact, so picks match the plain method bit for
    // bit and all tie orderings below are unchanged
    let cost = |err: f32, d: f32| err + weight * if squared { d * d } else { d };

    // vertical strip (eighth blocks): k/8 wide, full height. xor is
    // monotone in the width on either side of w, so the optimum is one
    // of the two eighth-grid neighbors. Candidates carry (k, err, d);
    // comparisons use the penalized cost, so at weight 0 they reduce to
    // the xor comparisons below.
    let kf = (w * 8.0).floor();
    let (x_k, x_err, x_d) = {
        let floor = (kf, xor(kf / 8.0, 1.0), protrusion(kf / 8.0, 1.0, w, h));
        let ceil = (
            kf + 1.0,
            xor((kf + 1.0) / 8.0, 1.0),
            protrusion((kf + 1.0) / 8.0, 1.0, w, h),
        );
        if cost(ceil.1, ceil.2) < cost(floor.1, floor.2) {
            ceil
        } else {
            floor
        }
    };
    let x_strip = (
        if from_left {
            EIGHTH_BLOCKS_FROM_LEFT[x_k as usize]
        } else {
            EIGHTH_BLOCKS_FROM_RIGHT[x_k as usize]
        },
        x_err,
        x_d,
    );

    // horizontal strip (eighth and third blocks): full width, k/8 or k/3
    // tall; the best over the union of the two grids is the better of
    // each grid's own best
    let kf_y = (h * 8.0).floor();
    let (eighth_k, eighth_err, eighth_d) = {
        let floor = (kf_y, xor(1.0, kf_y / 8.0), protrusion(1.0, kf_y / 8.0, w, h));
        let ceil = (
            kf_y + 1.0,
            xor(1.0, (kf_y + 1.0) / 8.0),
            protrusion(1.0, (kf_y + 1.0) / 8.0, w, h),
        );
        if cost(ceil.1, ceil.2) < cost(floor.1, floor.2) {
            ceil
        } else {
            floor
        }
    };
    let (third_k, third_err, third_d) = {
        let low = (1.0, xor(1.0, 1.0 / 3.0), protrusion(1.0, 1.0 / 3.0, w, h));
        let high = (2.0, xor(1.0, 2.0 / 3.0), protrusion(1.0, 2.0 / 3.0, w, h));
        if cost(high.1, high.2) < cost(low.1, low.2) {
            high
        } else {
            low
        }
    };
    let y_strip = if cost(eighth_err, eighth_d) <= cost(third_err, third_d) {
        (
            if from_bottom {
                EIGHTH_BLOCKS_FROM_BOTTOM[eighth_k as usize]
            } else {
                EIGHTH_BLOCKS_FROM_TOP[eighth_k as usize]
            },
            eighth_err,
            eighth_d,
        )
    } else if from_bottom {
        (
            [LOWER_ONE_THIRD_BLOCK, LOWER_TWO_THIRD_BLOCK][third_k as usize - 1],
            third_err,
            third_d,
        )
    } else {
        (
            [UPPER_ONE_THIRD_BLOCK, UPPER_TWO_THIRD_BLOCK][third_k as usize - 1],
            third_err,
            third_d,
        )
    };

    // quadrant block at the anchor corner
    let quadrant = (
        match (from_left, from_bottom) {
            (true, true) => '▖',
            (false, true) => '▗',
            (true, false) => '▘',
            (false, false) => '▝',
        },
        xor(0.5, 0.5),
        protrusion(0.5, 0.5, w, h),
    );

    // hextant. At weight 0 the per-sextant majority rule is the exact xor
    // optimum (sextants are disjoint). Under a protrusion penalty the fill
    // decisions couple through max(d), so instead brute-force all 64 fill
    // patterns — cheap, and exact for any penalty. Bit = row*2+col, row 0
    // = top, matching hextant_character_to_binary; strict < keeps the
    // lowest bits on ties, deterministic.
    let hextant = if weight == 0.0 {
        let mut array = [[false; 2]; 3]; // row 0 = top, col 0 = left
        let mut err = 0.0;
        for col in 0..2 {
            for row in 0..3 {
                // anchor coordinates: u from the touched x edge, v from
                // the touched y edge; the ideal is [0, w] x [0, h]
                let u_ov = ((col + 1) as f32 / 2.0).min(w) - (col as f32 / 2.0).min(w);
                let v_ov = ((row + 1) as f32 / 3.0).min(h) - (row as f32 / 3.0).min(h);
                let ov = u_ov * v_ov;
                // strict >: a half-covered sextant is xor-equal either
                // way; leaving it unfilled keeps the pick deterministic
                let filled = 2.0 * ov > 1.0 / 6.0;
                if filled {
                    array[if from_bottom { 2 - row } else { row }]
                        [if from_left { col } else { 1 - col }] = true;
                }
                err += if filled { 1.0 / 6.0 - ov } else { ov };
            }
        }
        (hextant_array_to_char(array), err, 0.0)
    } else {
        let mut best: Option<(f32, u32, f32, f32)> = None; // (cost, bits, err, d)
        for bits in 0..64u32 {
            let mut err = 0.0f32;
            let mut d = 0.0f32;
            for row in 0..3usize {
                for col in 0..2usize {
                    // display array coords -> anchor frame sextant span
                    let a_col = if from_left { col } else { 1 - col };
                    let a_row = if from_bottom { 2 - row } else { row };
                    let (u1, v1) = ((a_col + 1) as f32 / 2.0, (a_row + 1) as f32 / 3.0);
                    let ov = (u1.min(w) - a_col as f32 / 2.0).max(0.0)
                        * (v1.min(h) - a_row as f32 / 3.0).max(0.0);
                    let filled = bits & (1 << (row * 2 + col)) != 0;
                    err += if filled { 1.0 / 6.0 - ov } else { ov };
                    if filled {
                        // the distance function is monotone in u and v, so
                        // the far corner is the sextant's farthest point
                        d = d.max(protrusion(u1, v1, w, h));
                    }
                }
            }
            let c = cost(err, d);
            if best.is_none() || c < best.unwrap().0 {
                best = Some((c, bits, err, d));
            }
        }
        let (_, bits, err, d) = best.unwrap();
        let mut array = [[false; 2]; 3];
        for row in 0..3usize {
            for col in 0..2usize {
                array[row][col] = bits & (1 << (row * 2 + col)) != 0;
            }
        }
        (hextant_array_to_char(array), err, d)
    };

    // minimum cost wins; on a tie the earlier candidate (strips over
    // corner geometry) keeps the silhouette closer to a rectangle
    [x_strip, y_strip, quadrant, hextant]
        .into_iter()
        .min_by_key(|&(_, err, d)| OrderedFloat(cost(err, d)))
        .unwrap()
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

/// One glyph's filled count over a half-cell's sample lattice (for
/// per-character coverage error).
struct GlyphFit {
    c: char,
    count: u32,
}

/// Every glyph the coverage model knows, with its filled sample count.
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
                let mut count = 0u32;
                for i in 0..SY {
                    for j in 0..HX {
                        // same lattice and fx/fy mapping as actual_sample
                        if glyph_filled(
                            c,
                            (j as f32 + 0.5) / HX as f32,
                            (i as f32 + 0.5) / SY as f32,
                        ) {
                            count += 1;
                        }
                    }
                }
                GlyphFit { c, count }
            })
            .collect()
    })
}

/// Ideal filled count of the true square over one half-cell's sample
/// lattice (for per-character coverage error).
fn half_cell_ideal(square: WorldSquare, half: usize, pos: WorldPoint) -> u32 {
    let half_left = square.x as f32 - 0.5 + 0.5 * half as f32;
    let bottom = square.y as f32 - 0.5;
    let mut count = 0u32;
    for i in 0..SY {
        for j in 0..HX {
            let wx = half_left + 0.5 * (j as f32 + 0.5) / HX as f32;
            let wy = bottom + (i as f32 + 0.5) / SY as f32;
            if (wx - pos.x).abs() <= 0.5 && (wy - pos.y).abs() <= 0.5 {
                count += 1;
            }
        }
    }
    count
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
                let ideal_count = half_cell_ideal(square, half, pos);
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

// --- error-visualization panes -------------------------------------------------
//
// Full-resolution zoomed views (24x12 text cells = the native sampled
// pixel grid, 2x3 samples per pixel, two pixels stacked per text cell)
// that color one error metric per pane.

/// Rendered-vs-ideal classification of one sample point. Match is split
/// into filled/empty so panes can show the silhouette without a FillGrid.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum SampleClass {
    MatchFilled = 0,
    MatchEmpty = 1,
    /// rendered filled, ideal empty
    Over = 2,
    /// ideal filled, rendered empty
    Under = 3,
}

const OVER_COLOR: Rgb = Rgb(255, 90, 90);
const UNDER_COLOR: Rgb = Rgb(90, 160, 255);
const XOR_COLOR: Rgb = Rgb(255, 140, 40);

/// One text cell stacking two pixels vertically: same color → full block;
/// different colors → upper as fg half-block over lower as bg half-block
/// (the bg half-block carries the lower pixel's color, not the cell
/// shade); one pixel → its half-block over the cell background; none →
/// dark dot. Shared by the render zoom (`bitmap_pane`) and error panes.
fn two_tone_cell(style: &Style, cell_bg: Rgb, up: Option<Rgb>, lo: Option<Rgb>) -> String {
    match (up, lo) {
        (Some(cu), Some(cl)) if cu == cl => format!("{}{}█", style.bg(cell_bg), style.fg(cu)),
        (Some(cu), Some(cl)) => format!("{}{}▀", style.bg(cl), style.fg(cu)),
        (Some(cu), None) => format!("{}{}▀", style.bg(cell_bg), style.fg(cu)),
        (None, Some(cl)) => format!("{}{}▄", style.bg(cell_bg), style.fg(cl)),
        (None, None) => format!("{}{}·", style.bg(cell_bg), style.fg(DOT_COLOR)),
    }
}

/// Render a full-resolution pane from a per-pixel color grid (top row
/// first), composing each text cell from its two stacked pixels.
pub fn pane_from_colors(style: &Style, colors: &[Vec<Option<Rgb>>]) -> Vec<String> {
    (0..TEXT_ROWS)
        .map(|t| {
            let mut line: String = (0..PX_W)
                .map(|px| {
                    two_tone_cell(
                        style,
                        cell_bg(px, t / 4),
                        colors[2 * t][px],
                        colors[2 * t + 1][px],
                    )
                })
                .collect();
            line.push_str(style.reset());
            line
        })
        .collect()
}

pub fn lerp(a: Rgb, b: Rgb, t: f32) -> Rgb {
    let t = t.clamp(0.0, 1.0);
    Rgb(
        (a.0 as f32 + (b.0 as f32 - a.0 as f32) * t) as u8,
        (a.1 as f32 + (b.1 as f32 - a.1 as f32) * t) as u8,
        (a.2 as f32 + (b.2 as f32 - a.2 as f32) * t) as u8,
    )
}

/// 3/4-state sample grid over the render window (same lattice and
/// half-sample offsets as FillGrid, so the two never disagree on fill).
pub struct ClassGrid {
    pub cells: Vec<Vec<SampleClass>>,
    pub origin: WorldPoint, // world coords of the window's bottom-left corner
}

/// Sample counts within one display pixel (2x3 samples). `inside`/
/// `outside` partition by the ideal square, `filled` by the render.
#[derive(Default)]
struct PixelStats {
    filled: usize,
    over: usize,
    under: usize,
    inside: usize,  // ideal-inside samples = match_filled + under
    outside: usize, // ideal-outside samples = match_empty + over
}

impl ClassGrid {
    pub fn sample(origin: WorldPoint, mut f: impl FnMut(f32, f32) -> SampleClass) -> Self {
        let mut cells = Vec::new();
        for j in 0..NX {
            let mut col = Vec::new();
            for i in 0..NY {
                let wx = origin.x + (j as f32 + 0.5) / SX as f32;
                let wy = origin.y + (i as f32 + 0.5) / SY as f32;
                col.push(f(wx, wy));
            }
            cells.push(col);
        }
        ClassGrid { cells, origin }
    }

    /// Rendered-vs-ideal class at one world point (same square/half-cell
    /// lookup as `actual_sample`).
    pub fn class_at(
        grid: &[[DoubleChar; 3]; 3],
        owners: &[[[Option<usize>; 2]; 3]; 3],
        center: WorldSquare,
        pos: WorldPoint,
        wx: f32,
        wy: f32,
    ) -> SampleClass {
        let ideal = (wx - pos.x).abs() <= 0.5 && (wy - pos.y).abs() <= 0.5;
        let filled = actual_sample(grid, owners, center, wx, wy).0;
        match (filled, ideal) {
            (true, true) => SampleClass::MatchFilled,
            (false, false) => SampleClass::MatchEmpty,
            (true, false) => SampleClass::Over,
            (false, true) => SampleClass::Under,
        }
    }

    fn pixel(&self, px: usize, py: usize) -> PixelStats {
        // py 0 = top pixel row; same 2x3 sample block as FillGrid::pixel
        let mut s = PixelStats::default();
        for j in px * 2..px * 2 + 2 {
            for i in NY - (py + 1) * 3..NY - py * 3 {
                match self.cells[j][i] {
                    SampleClass::MatchFilled => {
                        s.filled += 1;
                        s.inside += 1;
                    }
                    SampleClass::MatchEmpty => s.outside += 1,
                    SampleClass::Over => {
                        s.filled += 1;
                        s.over += 1;
                        s.outside += 1;
                    }
                    SampleClass::Under => {
                        s.under += 1;
                        s.inside += 1;
                    }
                }
            }
        }
        s
    }

    /// Full-resolution pane: each text cell stacks two pixels (upper as
    /// fg half-block, lower as bg half-block) exactly like `bitmap_pane`,
    /// so every metric renders at the native sampled resolution.
    pub fn full_pane(
        &self,
        style: &Style,
        color_of: impl Fn(usize, usize, &PixelStats) -> Option<Rgb>,
    ) -> Vec<String> {
        let color = |px: usize, py: usize| color_of(px, py, &self.pixel(px, py));
        (0..TEXT_ROWS)
            .map(|t| {
                let mut line: String = (0..PX_W)
                    .map(|px| {
                        two_tone_cell(
                            style,
                            cell_bg(px, t / 4),
                            color(px, 2 * t),
                            color(px, 2 * t + 1),
                        )
                    })
                    .collect();
                line.push_str(style.reset());
                line
            })
            .collect()
    }

    /// Ideal-square xor: all mismatched samples over the window, in world
    /// square units (the family map's objective).
    pub fn xor_error(&self) -> f32 {
        let n: usize = self
            .cells
            .iter()
            .flatten()
            .filter(|&&c| matches!(c, SampleClass::Over | SampleClass::Under))
            .count();
        n as f32 / (SX * SY) as f32
    }

    /// Rendered area minus ideal area, in world square units (signed).
    /// Equal to FillGrid area − 1: over-coverage minus under-coverage.
    pub fn signed_area_error(&self) -> f32 {
        let (over, under): (usize, usize) = self
            .cells
            .iter()
            .flatten()
            .fold((0, 0), |(o, u), &c| match c {
                SampleClass::Over => (o + 1, u),
                SampleClass::Under => (o, u + 1),
                _ => (o, u),
            });
        (over as f32 - under as f32) / (SX * SY) as f32
    }

    /// Ideal-square-xor pane: any mismatched sample lights the pixel.
    pub fn mismatch_pane(&self, style: &Style) -> Vec<String> {
        self.full_pane(style, |_, _, s| (s.over + s.under > 0).then_some(XOR_COLOR))
    }

    /// Signed area pane: over-coverage red, under-coverage blue.
    pub fn signed_pane(&self, style: &Style) -> Vec<String> {
        self.full_pane(style, |_, _, s| {
            if s.over > s.under {
                Some(OVER_COLOR)
            } else if s.under > s.over {
                Some(UNDER_COLOR)
            } else if s.over > 0 {
                Some(XOR_COLOR) // split pixel: both directions in 2x3 samples
            } else {
                None
            }
        })
    }

    /// Center-error pane: dim rendered silhouette, ideal outline, and both
    /// centroids marked — '×' actual, '+' ideal. The value line carries the
    /// numbers; this shows *where* the silhouette's middle sits.
    pub fn center_pane(&self, actual: &FillGrid, pos: WorldPoint, style: &Style) -> Vec<String> {
        // world point -> text cell; window is 3 world units = 24 cols / 12 rows
        let pane_of = |wx: f32, wy: f32| -> (usize, usize) {
            let c = ((wx - self.origin.x) * 8.0).floor().clamp(0.0, 23.0) as usize;
            let r = ((self.origin.y + 3.0 - wy) * 4.0).floor().clamp(0.0, 11.0) as usize;
            (c, r)
        };
        let mut marks = vec![vec![(' ', Rgb(0, 0, 0)); PX_W]; TEXT_ROWS];
        if let Some(c) = fill_centroid(actual) {
            let p = pane_of(c.x, c.y);
            marks[p.1][p.0] = ('\u{00d7}', Rgb(120, 255, 255));
        }
        let p = pane_of(pos.x, pos.y);
        marks[p.1][p.0] = ('+', Rgb(235, 235, 235));
        self.full_pane(style, |px, py, s| {
            // a marker replaces the whole text cell it lands in: the pane
            // colors per pixel, so mark both pixels of that cell
            if marks[py / 2][px].0 != ' ' {
                return Some(marks[py / 2][px].1);
            }
            if s.inside > 0 && s.outside > 0 {
                Some(IDEAL_COLOR) // straddles the ideal boundary: outline
            } else if s.filled > 0 {
                Some(Rgb(64, 64, 80)) // dim rendered fill
            } else if s.under > 0 {
                Some(Rgb(48, 58, 92)) // faint under-coverage tint
            } else {
                None
            }
        })
    }

    /// Per-character coverage pane: each half-cell shaded by its local
    /// |rendered − ideal| filled area (the `per_char_coverage_error`
    /// contribution), from dark (0) to hot (0.25 of the half-cell).
    pub fn per_char_heat_pane(
        grid: &[[DoubleChar; 3]; 3],
        center: WorldSquare,
        pos: WorldPoint,
        style: &Style,
    ) -> Vec<String> {
        let mut heat = [[[0.0f32; 2]; 3]; 3];
        for dx in -1..=1i32 {
            for dy in -1..=1i32 {
                let square = center + vec2(dx, dy);
                for half in 0..2 {
                    let ideal = half_cell_ideal(square, half, pos);
                    let c = grid[(dx + 1) as usize][(dy + 1) as usize][half];
                    let rendered = glyph_fits().iter().find(|f| f.c == c).unwrap().count;
                    heat[(dx + 1) as usize][(dy + 1) as usize][half] =
                        rendered.abs_diff(ideal) as f32 / (HX * SY) as f32;
                }
            }
        }
        // window: 6 half-cells wide (4 px each), 3 half-cell rows tall
        // (8 px each, top pane rows = +y)
        let mut colors = vec![vec![None; PX_W]; PX_H];
        for py in 0..PX_H {
            for px in 0..PX_W {
                let h = px / 4;
                let dx = (h / 2) as i32 - 1;
                let dy = 1 - (py / 8) as i32;
                let v = heat[(dx + 1) as usize][(dy + 1) as usize][h % 2];
                colors[py][px] =
                    (v > 0.0).then(|| lerp(Rgb(70, 60, 30), Rgb(255, 200, 60), v / 0.25));
            }
        }
        pane_from_colors(style, &colors)
    }

    /// Jaggedness pane: the silhouette dim, with contour pixels lit by the
    /// local edge-step length (dark = straight, bright = a big jump).
    pub fn jaggedness_pane(actual: &FillGrid, style: &Style) -> Vec<String> {
        // per sample column: (top, bottom) filled sample indices
        let col_contour: Vec<Option<(usize, usize)>> = (0..NX)
            .map(|j| {
                let rows: Vec<usize> = (0..NY).filter(|&i| actual.filled(j, i)).collect();
                rows.first().map(|&lo| (lo, *rows.last().unwrap()))
            })
            .collect();
        let row_contour: Vec<Option<(usize, usize)>> = (0..NY)
            .map(|i| {
                let cols: Vec<usize> = (0..NX).filter(|&j| actual.filled(j, i)).collect();
                cols.first().map(|&lo| (lo, *cols.last().unwrap()))
            })
            .collect();
        let mut colors = vec![vec![None; PX_W]; PX_H];
        // dim fill base
        for py in 0..PX_H {
            for px in 0..PX_W {
                let filled = (px * 2..px * 2 + 2)
                    .any(|j| (NY - (py + 1) * 3..NY - py * 3).any(|i| actual.filled(j, i)));
                if filled {
                    colors[py][px] = Some(Rgb(56, 56, 70));
                }
            }
        }
        // top/bottom contours: brightest step within each pixel column
        for px in 0..PX_W {
            let mut best = (0.0f32, None, None); // (step, top sample, bottom sample)
            for j in px * 2..px * 2 + 2 {
                if j == 0 {
                    continue;
                }
                if let (Some((t0, b0)), Some((t1, b1))) = (col_contour[j - 1], col_contour[j]) {
                    let step = (actual.wy(t1) - actual.wy(t0)).abs()
                        + (actual.wy(b1) - actual.wy(b0)).abs();
                    if step >= best.0 {
                        best = (step, Some(t1), Some(b1));
                    }
                }
            }
            let t = (best.0 * 8.0).clamp(0.0, 1.0);
            if t > 0.0 {
                let c = lerp(Rgb(60, 60, 40), Rgb(160, 255, 80), t);
                for s in [best.1, best.2].into_iter().flatten() {
                    colors[(NY - 1 - s) / 3][px] = Some(c);
                }
            }
        }
        // left/right contours: brightest step within each pixel row
        for py in 0..PX_H {
            let mut best = (0.0f32, None, None);
            for i in NY - (py + 1) * 3..NY - py * 3 {
                if i == 0 {
                    continue;
                }
                if let (Some((l0, r0)), Some((l1, r1))) = (row_contour[i - 1], row_contour[i]) {
                    let step = (actual.wx(l1) - actual.wx(l0)).abs()
                        + (actual.wx(r1) - actual.wx(r0)).abs();
                    if step >= best.0 {
                        best = (step, Some(l1), Some(r1));
                    }
                }
            }
            let t = (best.0 * 8.0).clamp(0.0, 1.0);
            if t > 0.0 {
                let c = lerp(Rgb(60, 60, 40), Rgb(160, 255, 80), t);
                for s in [best.1, best.2].into_iter().flatten() {
                    colors[py][s / 2] = Some(c);
                }
            }
        }
        pane_from_colors(style, &colors)
    }

    /// Displacement pane: which mismatched samples appear when the square
    /// is nudged by `delta` in the worst direction. Bright yellow = newly
    /// wrong (the pop), dim red = still wrong, dim blue = recovered.
    pub fn displacement_pane(base: &ClassGrid, shifted: &ClassGrid, style: &Style) -> Vec<String> {
        let is_match = |c: SampleClass| matches!(c, SampleClass::MatchFilled | SampleClass::MatchEmpty);
        let class_of = |px: usize, py: usize| -> Option<Rgb> {
            let mut counts = [0usize; 3]; // [newly wrong, still wrong, recovered]
            for j in px * 2..px * 2 + 2 {
                for i in NY - (py + 1) * 3..NY - py * 3 {
                    let (a, b) = (base.cells[j][i], shifted.cells[j][i]);
                    match (is_match(a), is_match(b)) {
                        (true, false) => counts[0] += 1,
                        (false, false) => counts[1] += 1,
                        (false, true) => counts[2] += 1,
                        _ => {}
                    }
                }
            }
            if counts[0] > 0 {
                Some(Rgb(255, 230, 80))
            } else if counts[1] > 0 {
                Some(Rgb(150, 70, 70))
            } else if counts[2] > 0 {
                Some(Rgb(70, 110, 150))
            } else {
                None
            }
        };
        (0..TEXT_ROWS)
            .map(|t| {
                let mut line: String = (0..PX_W)
                    .map(|px| {
                        two_tone_cell(
                            style,
                            cell_bg(px, t / 4),
                            class_of(px, 2 * t),
                            class_of(px, 2 * t + 1),
                        )
                    })
                    .collect();
                line.push_str(style.reset());
                line
            })
            .collect()
    }
}

/// Small-displacement step for `displacement_sensitivity`: the nudge scale,
/// so one step can cross a real glyph-pick or snap-family boundary.
pub const DISPLACEMENT_DELTA: f32 = 1.0 / 16.0;

/// Worst-case xor gained per small displacement: for each axis direction,
/// how much the ideal-square xor of the method's own render grows when the
/// square (and its ideal) move by `delta`. Piecewise-constant glyph picks
/// make this 0 most of the time with jumps at pick boundaries — a direct
/// measure of pop sensitivity. Returns (worst gain, worst direction).
pub fn displacement_sensitivity(
    neighborhood: impl Fn(WorldPoint) -> ([[DoubleChar; 3]; 3], WorldSquare),
    pos: WorldPoint,
    delta: f32,
) -> (f32, WorldMove) {
    let error_at = |p: WorldPoint| {
        let (grid, center) = neighborhood(p);
        let owners = assign_colors(&grid);
        coverage_error(&grid, &owners, center, p)
    };
    let base = error_at(pos);
    let mut best = (0.0f32, WorldMove::new(1.0, 0.0));
    for d in [(1.0, 0.0), (-1.0, 0.0), (0.0, 1.0), (0.0, -1.0)] {
        let dir: WorldMove = WorldMove::new(d.0, d.1);
        let gain = error_at(pos + dir * delta) - base;
        if gain > best.0 {
            best = (gain, dir);
        }
    }
    best
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
        (0..TEXT_ROWS)
            .map(|t| {
                let mut line: String = (0..PX_W)
                    .map(|px| {
                        two_tone_cell(
                            style,
                            cell_bg(px / 4, t / 4),
                            self.pixel(px, 2 * t).map(|o| color_of(o)),
                            self.pixel(px, 2 * t + 1).map(|o| color_of(o)),
                        )
                    })
                    .collect();
                line.push_str(style.reset());
                line
            })
            .collect()
    }
}

#[cfg(test)]
mod charwise_tests {
    use super::*;

    fn cell(grid: &[[DoubleChar; 3]; 3], dx: i32, dy: i32) -> DoubleChar {
        grid[(dx + 1) as usize][(dy + 1) as usize]
    }

    #[test]
    fn test_aligned_square() {
        let (grid, center) = charwise_neighborhood(euclid::point2(0.0, 0.0));
        assert_eq!(center, euclid::point2(0, 0));
        for dx in -1..=1 {
            for dy in -1..=1 {
                let expected = if (dx, dy) == (0, 0) {
                    [FULL_BLOCK; 2]
                } else {
                    [SPACE; 2]
                };
                assert_eq!(cell(&grid, dx, dy), expected);
            }
        }
    }

    #[test]
    fn test_half_cell_x_shift() {
        // square spans x in [-0.25, 0.75]: the center square keeps a full
        // right half-cell, its right neighbor gets a left half-block
        let (grid, _) = charwise_neighborhood(euclid::point2(0.25, 0.0));
        assert_eq!(cell(&grid, 0, 0), [RIGHT_HALF_BLOCK, FULL_BLOCK]);
        assert_eq!(cell(&grid, 1, 0), [LEFT_HALF_BLOCK, SPACE]);
        assert_eq!(cell(&grid, -1, 0), [SPACE; 2]);
    }

    #[test]
    fn test_row_straddle_picks_vertical_strips() {
        // the square at y=0.4 straddles both rows: the center row keeps a
        // top-anchored 5/8 of the square (0.6 is nearest the eighth grid),
        // the row above gets the bottom 3/8; thirds are farther
        let (grid, _) = charwise_neighborhood(euclid::point2(0.0, 0.4));
        assert_eq!(cell(&grid, 0, 0), [EIGHTH_BLOCKS_FROM_TOP[5]; 2]);
        assert_eq!(cell(&grid, 0, 1), [EIGHTH_BLOCKS_FROM_BOTTOM[3]; 2]);
        assert_eq!(cell(&grid, 0, -1), [SPACE; 2]);
    }

    #[test]
    fn test_diagonal_corner_picks_hextant() {
        // the up-right diagonal cell sees a 0.6 x 0.3 corner overlap; a
        // single bottom-left sextant beats every strip and the quadrant
        // (xor ~0.047 vs 0.15 for the runner-up)
        let (grid, _) = charwise_neighborhood(euclid::point2(0.3, 0.3));
        let bottom_left_sextant =
            hextant_array_to_char([[false, false], [false, false], [true, false]]);
        assert_eq!(cell(&grid, 1, 1), [bottom_left_sextant, SPACE]);
    }

    #[test]
    fn test_weight_zero_matches_plain_charwise() {
        // the weighted path at weight 0 must be the plain xor argmin,
        // bit for bit, over the whole positive-quadrant offset lattice
        for xi in 0..=16 {
            for yi in 0..=16 {
                let pos = euclid::point2(xi as f32 / 16.0, yi as f32 / 16.0);
                assert_eq!(
                    charwise_neighborhood(pos),
                    charwise_neighborhood_weighted(pos, 0.0),
                    "weight-0 divergence at ({xi}, {yi})/16"
                );
            }
        }
    }

    #[test]
    fn test_protrusion_penalty_trades_spike_for_even_error() {
        // the center square's right half-cell sees a full-width, bottom-
        // anchored 0.3 overlap. The plain xor argmin is the lower-third
        // block (xor 1/30): it spikes 1/30 above the square across the
        // whole half-cell. The shaped pick takes the 2/8 block instead:
        // xor 0.05 but zero protrusion, spreading the error under the
        // true edge rather than sticking out past it.
        // (pos rounds to square (0, -1); the sliver is in the square
        // above it, grid cell (0, 1).)
        let pos = euclid::point2(0.25, -0.7);
        let (plain, _) = charwise_neighborhood(pos);
        let (shaped, _) = charwise_shaped_neighborhood(pos);
        assert_eq!(cell(&plain, 0, 1)[1], LOWER_ONE_THIRD_BLOCK);
        assert_eq!(cell(&shaped, 0, 1)[1], EIGHTH_BLOCKS_FROM_BOTTOM[2]);
    }

    #[test]
    fn test_shaped_variant_changes_something() {
        // guard against the penalty silently decaying to a no-op: over a
        // lattice spanning both sign quadrants the shaped grid must
        // differ from the plain one somewhere
        let mut diffs = 0usize;
        for xi in -16..=16 {
            for yi in -16..=16 {
                let pos = euclid::point2(xi as f32 / 16.0, yi as f32 / 16.0);
                if charwise_neighborhood(pos) != charwise_shaped_neighborhood(pos) {
                    diffs += 1;
                }
            }
        }
        assert!(diffs > 0, "shaped pick never diverges from plain charwise");
    }

    #[test]
    fn test_squared_penalty_kickin_threshold() {
        // the squared penalty only bites past d > 1/W2 = 0.25 cells. At
        // (0.25, -0.7) the spike's protrusion is d = 1/30, so the squared
        // variant keeps the plain spiky pick that the linear variant
        // refuses (see test_protrusion_penalty_trades_spike_for_even_error)
        let pos = euclid::point2(0.25, -0.7);
        let (plain, _) = charwise_neighborhood(pos);
        let (lin, _) = charwise_shaped_neighborhood(pos);
        let (sq, _) = charwise_protrusion_squared_neighborhood(pos);
        assert_eq!(cell(&plain, 0, 1)[1], LOWER_ONE_THIRD_BLOCK);
        assert_eq!(cell(&lin, 0, 1)[1], EIGHTH_BLOCKS_FROM_BOTTOM[2]);
        assert_eq!(cell(&sq, 0, 1)[1], LOWER_ONE_THIRD_BLOCK);
    }

    #[test]
    fn test_squared_variant_changes_deep_protrusions() {
        // guard against the quadratic silently decaying to the linear
        // pick: past the 1/W2 kick-in it must diverge somewhere on a
        // lattice spanning both sign quadrants
        let mut diffs = 0usize;
        for xi in -16..=16 {
            for yi in -16..=16 {
                let pos = euclid::point2(xi as f32 / 16.0, yi as f32 / 16.0);
                if charwise_shaped_neighborhood(pos)
                    != charwise_protrusion_squared_neighborhood(pos)
                {
                    diffs += 1;
                }
            }
        }
        assert!(diffs > 0, "squared pick never diverges from linear");
    }
}
