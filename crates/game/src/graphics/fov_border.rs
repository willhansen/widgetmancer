//! Static decorative border around the player's field of view.
//!
//! Drawn one square *outside* the FOV square (`D = player_sight_radius + 1`),
//! i.e. in the black ring the player cannot see, so it never covers board
//! contents. It is a square frame: full-block capitals at the four corners
//! (the top/bottom of the side "pillars"), inner-half shafts down the sides,
//! and an inner-half top/bottom beam with a centred outer-half run on the top
//! edge. Two colors only: dodgerblue blocks on black.
//!
//! It is painted into the screen-space [`UiLayer`], not the world: the screen
//! is consulted only to locate the player's screen square, so the frame does
//! not turn with the camera. Everything is a pure function of
//! `(player_square, radius, screen geometry)`, so repeated draws of the same
//! moment are byte-identical.

use terminal_rendering::glyph::glyph_constants::named_chars::{
    FULL_BLOCK, LEFT_HALF_BLOCK, LOWER_HALF_BLOCK, RIGHT_HALF_BLOCK, UPPER_HALF_BLOCK,
};
use terminal_rendering::glyph::glyph_constants::named_colors::{BLACK, DODGER_BLUE};
use terminal_rendering::glyph::Glyph;
use terminal_rendering::*;
use utility::coordinate_frame_conversions::WorldSquare;

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
    ///
    /// Drawn into the screen-space UI layer, so it is anchored to the screen
    /// rather than the world: it does not turn with the camera. `screen` is used
    /// only to locate the player's screen square (the screen centre at the draw
    /// site); `screen.rotation()` is deliberately ignored.
    pub fn draw(
        &self,
        ui: &mut UiLayer,
        screen: &Screen,
        player_square: WorldSquare,
        radius: u32,
    ) {
        let d = radius as i32 + 1;
        let outer_half_run = top_center_outer_run(radius);
        let center = screen.world_square_to_screen_buffer_square(player_square);

        for sy in -d..=d {
            for sx in -d..=d {
                // Square ring: keep the outermost Chebyshev shell only.
                if sx.abs() != d && sy.abs() != d {
                    continue;
                }

                // Screen space is y-down, so the top edge is sy == -d.
                let character = if sx.abs() == d && sy.abs() == d {
                    // Capitals/bases at the top and bottom of the side pillars.
                    FULL_BLOCK
                } else if sy == -d {
                    // Top beam: outer (up) near the centre, inner elsewhere.
                    if sx.abs() <= (outer_half_run - 1) / 2 {
                        UPPER_HALF_BLOCK
                    } else {
                        LOWER_HALF_BLOCK
                    }
                } else if sy == d {
                    // Bottom beam: all inner (up).
                    UPPER_HALF_BLOCK
                } else if sx == -d {
                    // Left pillar shaft: inner (right) half.
                    RIGHT_HALF_BLOCK
                } else {
                    // Right pillar shaft: inner (left) half.
                    LEFT_HALF_BLOCK
                };

                let screen_square = center + ScreenBufferStep::new(sx, sy);
                let glyphs = [Glyph::new(character, DODGER_BLUE, BLACK); 2];
                ui.draw_double_glyph(screen_square, glyphs);
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use euclid::point2;

    /// Draw the border at a given camera rotation, returning the screen (to
    /// locate the centre) and the UI layer it was written into.
    fn draw_border(radius: u32, rotation: QuarterTurnsAnticlockwise) -> (Screen, UiLayer) {
        let player = point2(0, 0);
        let mut screen = Screen::new(60, 30);
        screen.set_screen_center_by_world_square(player);
        screen.set_rotation(rotation);
        let mut ui = UiLayer::new(60, 30);
        FovBorder::new().draw(&mut ui, &screen, player, radius);
        (screen, ui)
    }

    /// Read the border glyph at a screen-square offset from the centre. FovBorder
    /// ignores camera rotation, so offsets are screen space (y down).
    fn glyph_at(ui: &UiLayer, screen: &Screen, sx: i32, sy: i32) -> Glyph {
        let square = screen.screen_center_as_screen_buffer_square() + ScreenBufferStep::new(sx, sy);
        ui.glyph_at(point2(square.x * 2, square.y))
    }

    #[test]
    fn draws_the_square_frame_with_expected_blocks_and_colors() {
        let radius = 3u32;
        let (screen, ui) = draw_border(radius, QuarterTurnsAnticlockwise::default());

        let d = radius as i32 + 1;
        let outer_run = top_center_outer_run(radius);

        // Corners: full block capitals/bases.
        for (sx, sy) in [(-d, -d), (d, -d), (-d, d), (d, d)] {
            let g = glyph_at(&ui, &screen, sx, sy);
            assert_eq!(g.character, FULL_BLOCK, "corner ({sx},{sy})");
            assert_eq!(g.fg_color, DODGER_BLUE);
            assert_eq!(g.bg_color, BLACK);
        }

        // Top edge (screen sy == -d): outer half within the centre run, inner
        // half outside.
        let half = (outer_run - 1) / 2;
        for sx in (-d + 1)..=(d - 1) {
            let expected = if sx.abs() <= half {
                UPPER_HALF_BLOCK
            } else {
                LOWER_HALF_BLOCK
            };
            assert_eq!(
                glyph_at(&ui, &screen, sx, -d).character,
                expected,
                "top sx={sx}"
            );
        }

        // Bottom edge (screen sy == d): all inner (upper half).
        for sx in (-d + 1)..=(d - 1) {
            assert_eq!(
                glyph_at(&ui, &screen, sx, d).character,
                UPPER_HALF_BLOCK,
                "bottom sx={sx}"
            );
        }

        // Sides: inner-half shafts.
        for sy in (-d + 1)..=(d - 1) {
            assert_eq!(
                glyph_at(&ui, &screen, -d, sy).character,
                RIGHT_HALF_BLOCK,
                "left sy={sy}"
            );
            assert_eq!(
                glyph_at(&ui, &screen, d, sy).character,
                LEFT_HALF_BLOCK,
                "right sy={sy}"
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
        let (screen, ui) = draw_border(4, QuarterTurnsAnticlockwise::default());

        // The top edge is symmetric about sx = 0.
        let d = 5;
        for sx in 1..d {
            assert_eq!(
                glyph_at(&ui, &screen, sx, -d).character,
                glyph_at(&ui, &screen, -sx, -d).character,
                "top symmetry sx={sx}"
            );
        }
    }

    #[test]
    fn border_is_independent_of_camera_rotation() {
        // The bug: the border used to be drawn in world space, so the decorated
        // top edge swung round with the view. It must now be screen-fixed.
        let (reference_screen, reference) = draw_border(4, QuarterTurnsAnticlockwise::default());

        for turns in 1..4 {
            let rotation = QuarterTurnsAnticlockwise::new(turns);
            let (screen, ui) = draw_border(4, rotation);

            // The player is still the screen centre, so the ring sits in the
            // same place; every cell must match the unrotated layer.
            assert_eq!(
                reference_screen.screen_center_as_screen_buffer_square(),
                screen.screen_center_as_screen_buffer_square()
            );
            for x in 0..ui.width() as i32 {
                for y in 0..ui.height() as i32 {
                    assert_eq!(
                        reference.glyph_at(point2(x, y)),
                        ui.glyph_at(point2(x, y)),
                        "cell ({x},{y}) changed at rotation {turns}"
                    );
                }
            }

            // The decorated run stays on the screen's top edge.
            assert_eq!(
                glyph_at(&ui, &screen, 0, -5).character,
                UPPER_HALF_BLOCK,
                "top centre moved at rotation {turns}"
            );
        }
    }
}
