//! Sampled-coverage oracle for the floating square renderer, shared by
//! tests/floating_square_coherence.rs (which asserts edge coherence with it)
//! and the floating_square_debug tool (which displays it). Keeping one
//! oracle means the debug tool can never drift from what the test asserts.
//!
//! Not game-facing API.

use euclid::vec2;

use crate::glyph_constants::*;
use crate::hextant_blocks::{hextant_character_to_binary, FIRST_HEXTANT, LAST_HEXTANT};
use crate::{characters_for_full_square_with_2d_offset, DoubleChar};
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
