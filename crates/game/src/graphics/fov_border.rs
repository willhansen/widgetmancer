//! Static decorative border around the player's field of view.
//!
//! Drawn one square *outside* the FOV square (`D = player_sight_radius + 1`),
//! i.e. in the black ring the player cannot see, so it never covers board
//! contents. It is a square frame: full-block capitals at the four corners
//! (the top/bottom of the side "pillars"), inner-half shafts down the sides,
//! and an inner-half top/bottom beam with a centred outer-half run on the top
//! edge. Two colors only: dodgerblue blocks on black.
//!
//! Everything is a pure function of `(player_square, radius, screen geometry)`,
//! so repeated draws of the same moment are byte-identical.

use terminal_rendering::glyph::glyph_constants::named_chars::{
    FULL_BLOCK, LEFT_HALF_BLOCK, LOWER_HALF_BLOCK, RIGHT_HALF_BLOCK, UPPER_HALF_BLOCK,
};
use terminal_rendering::glyph::glyph_constants::named_colors::{BLACK, DODGER_BLUE};
use terminal_rendering::glyph::Glyph;
use terminal_rendering::*;
use utility::coordinate_frame_conversions::{WorldSquare, WorldStep};

/// Length of the centred outer-half run on the top edge, ≈ 1/3 of the FOV
/// diameter (`2r+1`), forced odd so it is symmetric about the centre square.
fn top_center_outer_run(radius: u32) -> i32 {
    2 * (radius / 3) as i32 + 1
}

/// Stateless FOV border renderer.
#[derive(Default, Clone, Copy)]
pub struct FovBorder;

impl FovBorder {
    pub fn new() -> Self {
        FovBorder
    }

    /// Paint the border for a player at `player_square` with sight `radius`.
    pub fn draw(&self, screen: &mut Screen, player_square: WorldSquare, radius: u32) {
        let d = radius as i32 + 1;
        let outer_half_run = top_center_outer_run(radius);

        for dy in -d..=d {
            for dx in -d..=d {
                // Square ring: keep the outermost Chebyshev shell only.
                if dx.abs() != d && dy.abs() != d {
                    continue;
                }

                let character = if dx.abs() == d && dy.abs() == d {
                    // Capitals/bases at the top and bottom of the side pillars.
                    FULL_BLOCK
                } else if dy == d {
                    // Top beam: outer (up) near the centre, inner elsewhere.
                    if dx.abs() <= (outer_half_run - 1) / 2 {
                        UPPER_HALF_BLOCK
                    } else {
                        LOWER_HALF_BLOCK
                    }
                } else if dy == -d {
                    // Bottom beam: all inner (up).
                    UPPER_HALF_BLOCK
                } else if dx == -d {
                    // Left pillar shaft: inner (right) half.
                    RIGHT_HALF_BLOCK
                } else {
                    // Right pillar shaft: inner (left) half.
                    LEFT_HALF_BLOCK
                };

                let world = player_square + WorldStep::new(dx, dy);
                let screen_square = screen.world_square_to_screen_buffer_square(world);
                let glyphs = [Glyph::new(character, DODGER_BLUE, BLACK); 2];
                screen.draw_glyphs_straight_to_screen_square(glyphs, screen_square);
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use euclid::point2;

    fn drawn_glyph_at(
        screen: &Screen,
        player: WorldSquare,
        dx: i32,
        dy: i32,
    ) -> Glyph {
        let world = player + WorldStep::new(dx, dy);
        let square = screen.world_square_to_screen_buffer_square(world);
        screen.get_glyphs_at_screen_square(square)[0]
    }

    #[test]
    fn draws_the_square_frame_with_expected_blocks_and_colors() {
        let player = point2(0, 0);
        let radius = 3u32;
        let mut screen = Screen::new(60, 30);
        screen.set_screen_center_by_world_square(player);
        FovBorder::new().draw(&mut screen, player, radius);

        let d = radius as i32 + 1;
        let outer_run = top_center_outer_run(radius);

        // Corners: full block capitals/bases.
        for (dx, dy) in [(-d, d), (d, d), (-d, -d), (d, -d)] {
            let g = drawn_glyph_at(&screen, player, dx, dy);
            assert_eq!(g.character, FULL_BLOCK, "corner ({dx},{dy})");
            assert_eq!(g.fg_color, DODGER_BLUE);
            assert_eq!(g.bg_color, BLACK);
        }

        // Top edge: outer half within the centre run, inner half outside.
        let half = (outer_run - 1) / 2;
        for dx in (-d + 1)..=(d - 1) {
            let expected = if dx.abs() <= half {
                UPPER_HALF_BLOCK
            } else {
                LOWER_HALF_BLOCK
            };
            assert_eq!(
                drawn_glyph_at(&screen, player, dx, d).character,
                expected,
                "top dx={dx}"
            );
        }

        // Bottom edge: all inner (upper half).
        for dx in (-d + 1)..=(d - 1) {
            assert_eq!(
                drawn_glyph_at(&screen, player, dx, -d).character,
                UPPER_HALF_BLOCK,
                "bottom dx={dx}"
            );
        }

        // Sides: inner-half shafts.
        for dy in (-d + 1)..=(d - 1) {
            assert_eq!(
                drawn_glyph_at(&screen, player, -d, dy).character,
                RIGHT_HALF_BLOCK,
                "left dy={dy}"
            );
            assert_eq!(
                drawn_glyph_at(&screen, player, d, dy).character,
                LEFT_HALF_BLOCK,
                "right dy={dy}"
            );
        }
    }

    #[test]
    fn center_outer_run_is_about_one_third_of_the_diameter_and_odd() {
        for radius in 1..=32u32 {
            let run = top_center_outer_run(radius);
            assert!(run % 2 == 1, "radius {radius} run {run} must be odd");
            let target = (2 * radius + 1) as f32 / 3.0;
            assert!(
                (run as f32 - target).abs() <= 1.0,
                "radius {radius}: run {run} not ~1/3 of diameter ({target})"
            );
        }
    }

    #[test]
    fn border_is_symmetric_across_the_axes() {
        let player = point2(0, 0);
        let mut screen = Screen::new(60, 30);
        screen.set_screen_center_by_world_square(player);
        FovBorder::new().draw(&mut screen, player, 4);

        // Mirror left/right: (dx,dy) and (-dx,dy) use swapped side/top halves.
        // Check the top edge is symmetric about dx=0.
        let d = 5;
        for dx in 1..d {
            assert_eq!(
                drawn_glyph_at(&screen, player, dx, d).character,
                drawn_glyph_at(&screen, player, -dx, d).character,
                "top symmetry dx={dx}"
            );
        }
    }
}
