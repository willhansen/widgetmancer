//! Procedural starfield for the off-board area.
//!
//! The board edge used to sit on flat black. This paints the void beyond it
//! with a sparse field of stars. Depth is faked with parallax: each layer is
//! anchored at a fraction of the camera's motion, so near layers slide further
//! than far ones as the player moves. A slow linear drift keeps the field alive
//! while the player stands still.
//!
//! Everything is a pure function of `(screen, board_size, time)`: there is no
//! per-frame state, so repeated draws of the same moment produce byte-identical
//! buffers (see `test_headless_frames_are_byte_identical`). The star lattice is
//! conceptually infinite — moving or drifting just reveals new cells — so no
//! bounding region needs generating or recycling.

use std::f32::consts::TAU;

use euclid::{vec2, Vector2D};
use rgb::RGB8;

use terminal_rendering::glyph::glyph_constants::BLACK;
use terminal_rendering::*;

use crate::fov_stuff::FieldOfViewResult;
use crate::LogicalTime;

/// Plain screen/world-offset vector (no unit tag): star math is scalars until
/// the final round to a character cell.
type Vec2 = Vector2D<f32, euclid::UnknownUnit>;

/// Far, mid, near. Drawn in that order so nearer layers land on top.
struct Layer {
    /// Fraction of the camera's motion this layer follows. Smaller = farther.
    parallax: f32,
    /// Anchor-space drift in world squares per second, `(x, y)`.
    drift: (f32, f32),
    /// Lattice cell size in world squares. Larger = sparser stars.
    cell: f32,
    /// Probability that a lattice cell contains a star.
    density: f32,
    /// Twinkle angular-rate range in radians per second.
    twinkle_rate: (f32, f32),
    palette: &'static [(char, RGB8)],
}

const FAR_PALETTE: &[(char, RGB8)] = &[
    ('·', RGB8::new(74, 84, 112)),
    ('.', RGB8::new(92, 102, 138)),
];
const MID_PALETTE: &[(char, RGB8)] = &[
    ('·', RGB8::new(112, 122, 152)),
    ('.', RGB8::new(140, 150, 184)),
    ('+', RGB8::new(126, 138, 170)),
];
const NEAR_PALETTE: &[(char, RGB8)] = &[
    ('.', RGB8::new(188, 196, 220)),
    ('+', RGB8::new(170, 182, 214)),
    ('*', RGB8::new(210, 216, 236)),
    ('✦', RGB8::new(236, 240, 255)),
];

const LAYERS: [Layer; 3] = [
    Layer {
        parallax: 0.10,
        drift: (0.020, 0.008),
        cell: 2.4,
        density: 0.50,
        twinkle_rate: (0.25, 0.55),
        palette: FAR_PALETTE,
    },
    Layer {
        parallax: 0.30,
        drift: (-0.014, 0.026),
        cell: 2.8,
        density: 0.40,
        twinkle_rate: (0.4, 0.9),
        palette: MID_PALETTE,
    },
    Layer {
        parallax: 0.60,
        drift: (0.011, -0.019),
        cell: 3.2,
        density: 0.30,
        twinkle_rate: (0.6, 1.4),
        palette: NEAR_PALETTE,
    },
];

/// Stateless starfield renderer. See the module docs for the determinism
/// contract.
#[derive(Default, Clone, Copy)]
pub struct Starfield;

impl Starfield {
    pub fn new() -> Self {
        Starfield
    }

    /// Paint stars into the off-board cells of `screen.screen_buffer`.
    ///
    /// On-board squares are left untouched, so this composes after the FOV
    /// pass without disturbing the board or anything the player can see. Cells
    /// whose player-relative square is in the `fov` are also skipped: a portal
    /// view can map an off-board *screen* cell to visible floor, and the void
    /// beyond the sight radius must stay unrendered.
    pub fn draw(
        &self,
        screen: &mut Screen,
        board_size: BoardSize,
        time: LogicalTime,
        fov: Option<&FieldOfViewResult>,
    ) {
        let time_secs = time.as_secs_f32();
        let camera = vec2(
            screen.screen_center_as_world_square().x as f32,
            screen.screen_center_as_world_square().y as f32,
        );
        let center = screen.screen_center_as_screen_buffer_character_square();
        let char_half = vec2(
            screen.terminal_width() as f32 / 2.0,
            screen.terminal_height() as f32 / 2.0,
        );

        for (layer_index, layer) in LAYERS.iter().enumerate() {
            let translation = layer_translation(camera, time_secs, layer);

            let (anchor_min, anchor_max) = visible_anchor_bounds(screen, char_half, translation);
            let i0 = (anchor_min.x / layer.cell).floor() as i64;
            let i1 = (anchor_max.x / layer.cell).ceil() as i64;
            let j0 = (anchor_min.y / layer.cell).floor() as i64;
            let j1 = (anchor_max.y / layer.cell).ceil() as i64;

            for i in i0..=i1 {
                for j in j0..=j1 {
                    let h = hash_cell(layer_index as u64, i as u64, j as u64);
                    if rand01(h) >= layer.density {
                        continue;
                    }

                    let anchor = vec2(
                        (i as f32 + rand01(h ^ HASH_X)) * layer.cell,
                        (j as f32 + rand01(h ^ HASH_Y)) * layer.cell,
                    );
                    let screen_world = anchor - translation;
                    let char_off = world_offset_to_char_offset(screen, screen_world);

                    let pos_x = center.x + char_off.x.round() as i32;
                    let pos_y = center.y + char_off.y.round() as i32;
                    if pos_x < 0
                        || pos_y < 0
                        || pos_x >= screen.terminal_width()
                        || pos_y >= screen.terminal_height()
                    {
                        continue;
                    }

                    let world_square = screen.screen_buffer_character_square_to_world_square(
                        ScreenBufferCharacterSquare::new(pos_x, pos_y),
                    );
                    if is_on_board(world_square, board_size) {
                        continue;
                    }
                    if fov.is_some_and(|fov| {
                        fov.can_see_relative_square(world_square - fov.root_square())
                    }) {
                        continue;
                    }

                    let palette_index = ((rand01(h ^ HASH_GLYPH) * layer.palette.len() as f32)
                        as usize)
                        .min(layer.palette.len() - 1);
                    let (character, base_color) = layer.palette[palette_index];

                    let phase = rand01(h ^ HASH_PHASE) * TAU;
                    let rate = layer.twinkle_rate.0
                        + rand01(h ^ HASH_RATE) * (layer.twinkle_rate.1 - layer.twinkle_rate.0);
                    let brightness = 0.45 + 0.55 * (0.5 + 0.5 * (time_secs * rate + phase).sin());

                    screen.screen_buffer[pos_x as usize][pos_y as usize] =
                        Glyph::new(character, scale_color(base_color, brightness), BLACK);
                }
            }
        }
    }
}

/// Camera-space translation for a layer: a star's screen offset is
/// `anchor - translation`. Subtracting the scaled camera is what produces
/// parallax; subtracting the drift is what makes the field move over time.
fn layer_translation(camera: Vec2, time_secs: f32, layer: &Layer) -> Vec2 {
    camera * layer.parallax - vec2(layer.drift.0 * time_secs, layer.drift.1 * time_secs)
}

/// World-space offset pole of the visible screen rectangle, translated into
/// anchor space so the lattice walk covers everything on screen.
fn visible_anchor_bounds(screen: &Screen, char_half: Vec2, translation: Vec2) -> (Vec2, Vec2) {
    let corners = [
        vec2(-char_half.x, -char_half.y),
        vec2(char_half.x, -char_half.y),
        vec2(-char_half.x, char_half.y),
        vec2(char_half.x, char_half.y),
    ];
    let mut min = vec2(f32::INFINITY, f32::INFINITY);
    let mut max = vec2(f32::NEG_INFINITY, f32::NEG_INFINITY);
    for corner in corners {
        let world = char_offset_to_world_offset(screen, corner);
        min = vec2(min.x.min(world.x), min.y.min(world.y));
        max = vec2(max.x.max(world.x), max.y.max(world.y));
    }
    (min + translation, max + translation)
}

/// World offset (in world squares) -> offset in terminal character cells.
/// Mirrors `Screen::world_step_to_screen_step`, but in `f32` so sub-square
/// parallax survives until the final rounding.
fn world_offset_to_char_offset(screen: &Screen, world_offset: Vec2) -> Vec2 {
    let rotated = (-screen.rotation()).rotate_vector(world_offset);
    let flipped = flip_y(rotated);
    vec2(flipped.x * 2.0, flipped.y)
}

/// Inverse of [`world_offset_to_char_offset`].
fn char_offset_to_world_offset(screen: &Screen, char_offset: Vec2) -> Vec2 {
    let screen_square = vec2(char_offset.x / 2.0, char_offset.y);
    let flipped = flip_y(screen_square);
    screen.rotation().rotate_vector(flipped)
}

fn is_on_board(square: WorldSquare, board_size: BoardSize) -> bool {
    square.x >= 0
        && square.x < board_size.width as i32
        && square.y >= 0
        && square.y < board_size.height as i32
}

fn scale_color(color: RGB8, factor: f32) -> RGB8 {
    let f = factor.clamp(0.0, 1.0);
    RGB8::new(
        (color.r as f32 * f).round() as u8,
        (color.g as f32 * f).round() as u8,
        (color.b as f32 * f).round() as u8,
    )
}

const HASH_X: u64 = 0x632B_E5C1_9F37_9E21;
const HASH_Y: u64 = 0x2C1B_3C6E_F372_FE1D;
const HASH_GLYPH: u64 = 0x94D0_49BB_1331_11EB;
const HASH_PHASE: u64 = 0xBF58_476D_1CE4_E5B9;
const HASH_RATE: u64 = 0x2545_F491_4F6C_DD1D;

fn splitmix64(mut x: u64) -> u64 {
    x = x.wrapping_add(0x9E37_79B9_7F4A_7C15);
    let mut z = x;
    z = (z ^ (z >> 30)).wrapping_mul(0xBF58_476D_1CE4_E5B9);
    z = (z ^ (z >> 27)).wrapping_mul(0x94D0_49BB_1331_11EB);
    z ^ (z >> 31)
}

fn hash_cell(layer: u64, i: u64, j: u64) -> u64 {
    splitmix64(
        i.wrapping_mul(0x9E37_79B9_7F4A_7C15)
            ^ splitmix64(j ^ splitmix64(layer.wrapping_add(HASH_X))),
    )
}

fn rand01(seed: u64) -> f32 {
    (splitmix64(seed) >> 40) as f32 / (1u64 << 24) as f32
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::fov_stuff::portal_aware_field_of_view_from_square;
    use euclid::point2;

    fn test_screen() -> Screen {
        Screen::new(80, 24)
    }

    fn board_with_visible_void() -> BoardSize {
        BoardSize::new(10, 10)
    }

    fn count_star_cells(screen: &Screen) -> usize {
        screen
            .screen_buffer
            .iter()
            .flatten()
            .filter(|glyph| glyph.character != ' ')
            .count()
    }

    #[test]
    fn draws_nothing_when_the_board_fills_the_screen() {
        let mut screen = Screen::new(20, 10);
        screen.set_screen_center_by_world_square(point2(5, 5));
        let before = screen.screen_buffer.clone();

        Starfield::new().draw(&mut screen, BoardSize::new(20, 20), LogicalTime::ZERO, None);

        assert_eq!(screen.screen_buffer, before);
    }

    #[test]
    fn draws_stars_in_the_void_and_only_there() {
        let mut screen = test_screen();
        screen.set_screen_center_by_world_square(point2(5, 5));
        let board_size = board_with_visible_void();

        Starfield::new().draw(&mut screen, board_size, LogicalTime::ZERO, None);

        assert!(count_star_cells(&screen) > 0, "expected some stars");
        for x in 0..screen.terminal_width() {
            for y in 0..screen.terminal_height() {
                let glyph = screen.screen_buffer[x as usize][y as usize];
                if glyph.character == ' ' {
                    continue;
                }
                let square = screen.screen_buffer_character_square_to_world_square(
                    ScreenBufferCharacterSquare::new(x, y),
                );
                assert!(
                    !is_on_board(square, board_size),
                    "star drawn on board square {square:?}"
                );
            }
        }
    }

    #[test]
    fn no_stars_inside_the_field_of_view() {
        // A small board so plenty of off-board squares fall inside the FOV.
        let board_size = BoardSize::new(4, 4);
        let center = point2(2, 2);
        let mut screen = test_screen();
        screen.set_screen_center_by_world_square(center);
        let fov = portal_aware_field_of_view_from_square(
            center,
            3,
            &Default::default(),
            &Default::default(),
        );

        Starfield::new().draw(&mut screen, board_size, LogicalTime::ZERO, Some(&fov));

        assert!(count_star_cells(&screen) > 0, "expected some stars");
        for x in 0..screen.terminal_width() {
            for y in 0..screen.terminal_height() {
                let glyph = screen.screen_buffer[x as usize][y as usize];
                if glyph.character == ' ' {
                    continue;
                }
                let square = screen.screen_buffer_character_square_to_world_square(
                    ScreenBufferCharacterSquare::new(x, y),
                );
                let relative = square - fov.root_square();
                assert!(
                    !fov.can_see_relative_square(relative),
                    "star drawn on a visible off-board square {square:?} (rel {relative:?})"
                );
            }
        }
    }

    #[test]
    fn same_time_and_camera_are_byte_identical() {
        let board_size = board_with_visible_void();

        let render = || {
            let mut screen = test_screen();
            screen.set_screen_center_by_world_square(point2(5, 5));
            Starfield::new().draw(&mut screen, board_size, LogicalTime::from_secs_f32(3.0), None);
            screen.screen_buffer
        };

        assert_eq!(render(), render());
    }

    #[test]
    fn time_drift_moves_the_stars() {
        let board_size = board_with_visible_void();

        let render = |time| {
            let mut screen = test_screen();
            screen.set_screen_center_by_world_square(point2(5, 5));
            Starfield::new().draw(&mut screen, board_size, LogicalTime::from_secs_f32(time), None);
            screen.screen_buffer
        };

        assert_ne!(render(0.0), render(5.0));
    }

    #[test]
    fn char_offset_round_trips_under_rotation() {
        for quarter_turns in 0..4 {
            let mut screen = test_screen();
            screen.set_rotation(QuarterTurnsAnticlockwise::new(quarter_turns));
            let offset = vec2(3.7, -1.4);
            let round_tripped =
                char_offset_to_world_offset(&screen, world_offset_to_char_offset(&screen, offset));
            assert!((round_tripped.x - offset.x).abs() < 1e-4);
            assert!((round_tripped.y - offset.y).abs() < 1e-4);
        }
    }

    #[test]
    fn nearer_layers_track_more_camera_motion() {
        let still = vec2(0.0, 0.0);
        let moved = vec2(8.0, 0.0);

        let shift = |layer: &Layer| {
            (layer_translation(moved, 0.0, layer) - layer_translation(still, 0.0, layer)).x
        };

        let far = shift(&LAYERS[0]);
        let near = shift(&LAYERS[LAYERS.len() - 1]);
        assert!(
            near > far,
            "near layer should shift further: {near} vs {far}"
        );
        assert!((far - 8.0 * LAYERS[0].parallax).abs() < 1e-4);
    }
}
