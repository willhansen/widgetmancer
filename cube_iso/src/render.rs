//! Rasterizes the voxel world into a terminal [`Frame`] using projection P.
//!
//! Painter's algorithm: columns are visited north-to-south (larger `y` first)
//! so nearer, southern geometry overwrites the far geometry behind it. Each
//! column draws its lit top face, then its south wall when the square to the
//! south is not at the same altitude (edge of a cube, or across a gap).

use crate::physics::Player;
use crate::project::{project, project_double, Camera};
use crate::world::World;
use rgb::RGB8;
use terminal_rendering::glyph_constants::{BLACK, FULL_BLOCK};
use terminal_rendering::{DrawableGlyph, Frame};

const TOP_LIGHT: RGB8 = RGB8::new(150, 150, 162);
const TOP_DARK: RGB8 = RGB8::new(112, 112, 124);
const WALL_TOP: RGB8 = RGB8::new(92, 92, 122);
const WALL_BOTTOM: RGB8 = RGB8::new(32, 32, 56);
const PLAYER_COLOR: RGB8 = RGB8::new(255, 214, 40);
const TRAIL_COLOR: RGB8 = RGB8::new(120, 86, 30);
const STAR_DIM: RGB8 = RGB8::new(64, 64, 88);
const STAR_BRIGHT: RGB8 = RGB8::new(150, 150, 195);

/// A solid cell: the fill is carried by both fg and bg so the shape is visible
/// even in an uncolored dump, while the terminal still renders it flat.
fn solid(character: char, color: RGB8) -> DrawableGlyph {
    DrawableGlyph::new_colored(character, color, color)
}

/// Render one frame at `time` (reserved for future twinkle/drift).
pub fn render_frame(
    world: &World,
    player: &Player,
    _time: f32,
    width: usize,
    height: usize,
) -> Frame {
    let mut frame = Frame::solid_color(width, height, BLACK);
    draw_stars(&mut frame);

    let cam = Camera::at(player.x.round(), player.y.round());

    // Far (north, high y) to near (south, low y).
    let mut squares = world.all_top_squares();
    squares.sort_by(|a, b| b.1.cmp(&a.1).then(a.0.cmp(&b.0)));
    for (x, y) in squares {
        draw_column(&mut frame, world, &cam, width, height, x, y);
    }

    draw_trail(&mut frame, &cam, width, height, player);
    draw_player(&mut frame, &cam, width, height, player);
    frame
}

fn draw_column(
    frame: &mut Frame,
    world: &World,
    cam: &Camera,
    width: usize,
    height: usize,
    x: i32,
    y: i32,
) {
    let top_z = world.top_height(x, y).expect("top squares are solid");
    let (left, right, row) = project_double(cam, width, height, x as f32, y as f32, top_z as f32);
    let top_color = if (x + y).rem_euclid(2) == 0 {
        TOP_LIGHT
    } else {
        TOP_DARK
    };
    put(frame, left, row, solid(FULL_BLOCK, top_color));
    put(frame, right, row, solid(FULL_BLOCK, top_color));

    // Only the south face is visible in projection P, and only where the
    // surface drops away.
    let south_z = world.top_height(x, y - 1);
    if south_z == Some(top_z) {
        return;
    }
    for z in (0..top_z).rev() {
        let (wl, wr, wrow) = project_double(cam, width, height, x as f32, y as f32, z as f32);
        let t = if top_z > 1 {
            z as f32 / (top_z - 1) as f32
        } else {
            0.0
        };
        let color = lerp_rgb(WALL_BOTTOM, WALL_TOP, t);
        put(frame, wl, wrow, solid('▒', color));
        put(frame, wr, wrow, solid('▒', color));
    }
}

fn draw_trail(frame: &mut Frame, cam: &Camera, width: usize, height: usize, player: &Player) {
    for &(x, y, z) in &player.trail {
        let (col, row) = project(cam, width, height, x, y, z);
        put(
            frame,
            col,
            row,
            DrawableGlyph::new_colored('·', TRAIL_COLOR, BLACK),
        );
    }
}

fn draw_player(frame: &mut Frame, cam: &Camera, width: usize, height: usize, player: &Player) {
    let (col, row) = project(cam, width, height, player.x, player.y, player.z);
    let marker = DrawableGlyph::new_colored('@', BLACK, PLAYER_COLOR);
    put(frame, col, row, marker);
    if !player.is_smooth() {
        put(frame, col + 1, row, marker);
    }
}

fn draw_stars(frame: &mut Frame) {
    let (width, height) = (frame.width(), frame.height());
    for row in 0..height {
        for col in 0..width {
            let h = hash(col as i32, row as i32);
            let glyph = if h % 41 == 0 {
                DrawableGlyph::new_colored('+', STAR_BRIGHT, BLACK)
            } else if h % 13 == 0 {
                DrawableGlyph::new_colored('·', STAR_DIM, BLACK)
            } else {
                continue;
            };
            frame.grid[row][col] = glyph;
        }
    }
}

fn put(frame: &mut Frame, col: i32, row: i32, glyph: DrawableGlyph) {
    if col < 0 || row < 0 {
        return;
    }
    let (col, row) = (col as usize, row as usize);
    if row < frame.height() && col < frame.width() {
        frame.grid[row][col] = glyph;
    }
}

fn hash(a: i32, b: i32) -> u64 {
    let mut h = (a as u64).wrapping_mul(0x9E37_79B9_7F4A_7C15)
        ^ (b as u64).wrapping_mul(0xC2B2_AE3D_27D4_EB4F);
    h ^= h >> 29;
    h = h.wrapping_mul(0xBF58_476D_1CE4_E5B9);
    h ^= h >> 32;
    h
}

fn lerp_rgb(a: RGB8, b: RGB8, t: f32) -> RGB8 {
    let t = t.clamp(0.0, 1.0);
    let mix = |x: u8, y: u8| (x as f32 * (1.0 - t) + y as f32 * t).round() as u8;
    RGB8::new(mix(a.r, b.r), mix(a.g, b.g), mix(a.b, b.b))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::physics::{Intent, PhysicsMode};

    fn painted(frame: &Frame) -> Vec<RGB8> {
        frame
            .glyphs()
            .flat_map(|g| [g.fg_color, g.bg_color])
            .flatten()
            .collect()
    }

    #[test]
    fn frame_has_requested_dimensions() {
        let world = World::four_cubes();
        let player = Player::new(PhysicsMode::GridMoveGated);
        let frame = render_frame(&world, &player, 0.0, 60, 20);
        assert_eq!(frame.size_rows_cols(), [20, 60]);
    }

    #[test]
    fn scene_contains_top_faces_walls_and_the_player() {
        let world = World::four_cubes();
        let player = Player::new(PhysicsMode::GridMoveGated);
        let frame = render_frame(&world, &player, 0.0, 80, 30);
        let colors = painted(&frame);
        assert!(colors.contains(&TOP_LIGHT) || colors.contains(&TOP_DARK));
        assert!(colors.contains(&WALL_BOTTOM) || colors.contains(&WALL_TOP));
        assert!(frame.glyphs().any(|g| g.bg_color == Some(PLAYER_COLOR)));
    }

    #[test]
    fn player_is_horizontally_centered_under_camera_follow() {
        let world = World::four_cubes();
        let mut player = Player::new(PhysicsMode::GridMoveGated);
        player.apply_intent(&world, Intent::East);
        let frame = render_frame(&world, &player, 0.0, 80, 30);
        let player_cells: Vec<usize> = frame
            .grid
            .iter()
            .flat_map(|row| row.iter().enumerate())
            .filter(|(_, g)| g.bg_color == Some(PLAYER_COLOR))
            .map(|(col, _)| col)
            .collect();
        assert!(!player_cells.is_empty());
        let min = *player_cells.iter().min().unwrap();
        let max = *player_cells.iter().max().unwrap();
        assert!(
            min >= 38 && max <= 41,
            "player columns {min}..={max} near center"
        );
    }

    #[test]
    fn stepping_into_void_renders_a_fall_trail() {
        let world = World::four_cubes();
        let mut player = Player::new(PhysicsMode::GridMoveGated);
        for _ in 0..crate::world::CUBE_SIZE {
            player.apply_intent(&world, Intent::West);
        }
        assert!(player.z < crate::world::CUBE_HEIGHT as f32, "player fell");
        let frame = render_frame(&world, &player, 0.0, 80, 30);
        assert!(
            painted(&frame).contains(&TRAIL_COLOR),
            "trail should be drawn"
        );
        assert!(frame.glyphs().any(|g| g.bg_color == Some(PLAYER_COLOR)));
    }
}
