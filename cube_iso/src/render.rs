//! Rasterizes the voxel world into a terminal [`Frame`] using projection P.
//!
//! Painter's algorithm: columns are visited far-to-near in the camera's
//! rotated frame (largest depth first) so nearer geometry overwrites the far
//! geometry behind it.
//!
//! Readability, all through color and checkerboard (no projection change):
//! - **Material**: cool cube faces (block-checkered top, smooth wall, bright
//!   front rim) versus warm ledge faces.
//! - **Standoff hue**: a ledge's distance from the nearest cube is mapped to a
//!   warm hue ramp, which is the one channel projection P leaves free for the
//!   depth it collapsed into the vertical axis.
//! - **Block checker**: a 3-square checker on exposed top faces gives the
//!   surface grid (and a coarse phase backup for depth).
//! - **Depth fog**: distant columns are dimmed so only near geometry competes.
//!
//! All of these are anchored in world space, so they rotate correctly with the
//! view; only which faces are drawn and the painter order depend on
//! [`Camera::rotation`].

use crate::physics::Player;
use crate::project::Camera;
use crate::world::World;
use rgb::RGB8;
use terminal_rendering::glyph_constants::{BLACK, FULL_BLOCK};
use terminal_rendering::{DrawableGlyph, Frame};

const TOP_LIGHT: RGB8 = RGB8::new(150, 150, 162);
const TOP_DARK: RGB8 = RGB8::new(112, 112, 124);
const WALL_TOP: RGB8 = RGB8::new(92, 92, 122);
const WALL_BOTTOM: RGB8 = RGB8::new(32, 32, 56);

/// Bright cool line where a cube top falls away to its wall.
const RIM: RGB8 = RGB8::new(214, 214, 228);

/// Warm standoff ramp: 1 square out = amber, further = hotter/cooler.
const STANDOFF_HUE: [RGB8; 4] = [
    RGB8::new(240, 176, 64), // 1 out: amber
    RGB8::new(232, 120, 44), // 2 out: orange
    RGB8::new(214, 64, 72),  // 3 out: crimson
    RGB8::new(176, 64, 156), // 4+ out: violet
];

const PLAYER_COLOR: RGB8 = RGB8::new(255, 214, 40);
const TRAIL_COLOR: RGB8 = RGB8::new(120, 86, 30);
const STAR_DIM: RGB8 = RGB8::new(64, 64, 88);
const STAR_BRIGHT: RGB8 = RGB8::new(150, 150, 195);

/// Checker block size, in world squares (3 matches the game's floor pattern).
const CHECKER_BLOCK: i32 = 3;
/// Camera distance (in world squares, depth weighted 1x and horizontal 0.5x)
/// at which fog reaches its floor.
const FOG_SPAN: f32 = 18.0;
/// Fog never goes fully black, so far geometry stays a readable silhouette.
const FOG_MIN: f32 = 0.2;

/// A solid cell: the fill is carried by both fg and bg so the shape is visible
/// even in an uncolored dump, while the terminal still renders it flat.
fn solid(character: char, color: RGB8) -> DrawableGlyph {
    DrawableGlyph::new_colored(character, color, color)
}

/// Render one frame at `time` (reserved for future twinkle/drift) viewed from
/// `rotation` quarter turns counter-clockwise.
pub fn render_frame(
    world: &World,
    player: &Player,
    _time: f32,
    width: usize,
    height: usize,
    rotation: u8,
) -> Frame {
    let mut frame = Frame::solid_color(width, height, BLACK);
    draw_stars(&mut frame);

    let cam = Camera::with_rotation(player.x.round(), player.y.round(), rotation);
    let cube_columns = world.cube_columns();

    // Far to near in the rotated frame. Sort by the *signed* forward distance,
    // not the absolute depth: nearer (southern, at rotation 0) geometry must be
    // drawn last so it overwrites the wall behind it.
    let mut columns = world.columns();
    columns.sort_by(|a, b| {
        cam.forward(b.0 as f32, b.1 as f32)
            .partial_cmp(&cam.forward(a.0 as f32, a.1 as f32))
            .expect("forward is finite")
    });
    for (x, y) in columns {
        draw_column(&mut frame, world, &cube_columns, &cam, width, height, x, y);
    }

    draw_trail(&mut frame, &cam, width, height, player);
    draw_player(&mut frame, &cam, width, height, player);
    frame
}

/// Draw the exposed faces of every solid voxel in one column. A voxel shows a
/// lit top face when nothing is above it, and a shaded face toward the camera
/// when the neighboring column on that side has no solid at the same altitude.
fn draw_column(
    frame: &mut Frame,
    world: &World,
    cube_columns: &[(i32, i32)],
    cam: &Camera,
    width: usize,
    height: usize,
    x: i32,
    y: i32,
) {
    let zmax = world
        .top_voxel(x, y)
        .expect("columns are built from solid voxels");
    let fog = fog_factor(cam, x, y);
    let is_cube = world.is_cube_top_column(x, y);
    let standoff = if is_cube {
        0
    } else {
        nearest_cube_distance(cube_columns, x, y)
    };
    let toward = cam.toward_camera_step();
    let left = cam.screen_left_step();
    let right = cam.screen_right_step();

    for z in (0..=zmax).rev() {
        if !world.is_solid(x, y, z) {
            continue;
        }
        if !world.is_solid(x + toward.0, y + toward.1, z) {
            let (wl, wr, row) = cam.project_double(width, height, x as f32, y as f32, z as f32);
            let color = side_face_color(z, zmax, is_cube, standoff);
            let color = apply_fog(color, fog);
            put(frame, wl, row, solid('▒', color));
            put(frame, wr, row, solid('▒', color));
        }
        if !world.is_solid(x, y, z + 1) {
            let (tl, tr, row) =
                cam.project_double(width, height, x as f32, y as f32, (z + 1) as f32);
            let color =
                top_face_color(world, x, y, z, zmax, is_cube, standoff, toward, left, right);
            let color = apply_fog(color, fog);
            put(frame, tl, row, solid(FULL_BLOCK, color));
            put(frame, tr, row, solid(FULL_BLOCK, color));
        }
    }
}

/// Color for an exposed top face: a cool checker for cube tops (with a bright
/// rim where the top falls toward the camera), or the warm standoff hue for
/// ledges (with bright end caps and a darker checker shade).
#[allow(clippy::too_many_arguments)]
fn top_face_color(
    world: &World,
    x: i32,
    y: i32,
    z: i32,
    zmax: i32,
    is_cube: bool,
    standoff: i32,
    toward: (i32, i32),
    left: (i32, i32),
    right: (i32, i32),
) -> RGB8 {
    if is_cube && z == zmax {
        if !world.is_solid(x + toward.0, y + toward.1, z) {
            return RIM;
        }
        return if checker_light(x, y, z) {
            TOP_LIGHT
        } else {
            TOP_DARK
        };
    }

    let hue = standoff_hue(standoff);
    let end_cap =
        !world.is_solid(x + left.0, y + left.1, z) || !world.is_solid(x + right.0, y + right.1, z);
    if end_cap {
        scale_rgb(hue, 1.35)
    } else if checker_light(x, y, z) {
        hue
    } else {
        scale_rgb(hue, 0.72)
    }
}

/// Color for an exposed camera-facing side face. A cube's wall is the cool
/// gradient; a ledge's front is a darker shade of its standoff hue, so the
/// whole ledge reads as one warm object.
fn side_face_color(z: i32, zmax: i32, is_cube: bool, standoff: i32) -> RGB8 {
    if is_cube {
        let t = if zmax > 0 {
            z as f32 / zmax as f32
        } else {
            1.0
        };
        lerp_rgb(WALL_BOTTOM, WALL_TOP, t)
    } else {
        scale_rgb(standoff_hue(standoff), 0.55)
    }
}

/// Standoff hue ramp, indexed 1-based (0 is treated as 1).
fn standoff_hue(distance: i32) -> RGB8 {
    STANDOFF_HUE[(distance.clamp(1, STANDOFF_HUE.len() as i32) - 1) as usize]
}

/// 3-square block checker parity; `true` selects the light shade.
fn checker_light(x: i32, y: i32, z: i32) -> bool {
    let block = |v: i32| v.div_euclid(CHECKER_BLOCK);
    (block(x) + block(y) + block(z)).rem_euclid(2) == 0
}

/// Manhattan distance to the nearest full-height cube column.
fn nearest_cube_distance(cube_columns: &[(i32, i32)], x: i32, y: i32) -> i32 {
    cube_columns
        .iter()
        .map(|(cx, cy)| (cx - x).abs() + (cy - y).abs())
        .min()
        .unwrap_or(i32::MAX)
}

/// Dimming factor by depth in the rotated frame.
fn fog_factor(cam: &Camera, x: i32, y: i32) -> f32 {
    let distance = cam.depth(x as f32, y as f32);
    (1.0 - distance / FOG_SPAN).clamp(FOG_MIN, 1.0)
}

fn apply_fog(color: RGB8, fog: f32) -> RGB8 {
    scale_rgb(color, fog)
}

fn scale_rgb(color: RGB8, factor: f32) -> RGB8 {
    let scale = |v: u8| ((v as f32) * factor).round().clamp(0.0, 255.0) as u8;
    RGB8::new(scale(color.r), scale(color.g), scale(color.b))
}

fn draw_trail(frame: &mut Frame, cam: &Camera, width: usize, height: usize, player: &Player) {
    for &(x, y, z) in &player.trail {
        let (col, row) = cam.project(width, height, x, y, z);
        put(
            frame,
            col,
            row,
            DrawableGlyph::new_colored('│', TRAIL_COLOR, BLACK),
        );
    }
}

fn draw_player(frame: &mut Frame, cam: &Camera, width: usize, height: usize, player: &Player) {
    let (col, row) = cam.project(width, height, player.x, player.y, player.z);
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
            let glyph = if h % 211 == 0 {
                DrawableGlyph::new_colored('+', STAR_BRIGHT, BLACK)
            } else if h % 61 == 0 {
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
        let frame = render_frame(&world, &player, 0.0, 60, 20, 0);
        assert_eq!(frame.size_rows_cols(), [20, 60]);
    }

    #[test]
    fn fog_factor_shrinks_with_distance_and_floors() {
        let cam = Camera::with_rotation(0.0, 0.0, 0);
        assert!(fog_factor(&cam, 0, 0) > fog_factor(&cam, 0, -8));
        assert!((fog_factor(&cam, 0, -100) - FOG_MIN).abs() < 1e-6);
        assert_eq!(fog_factor(&cam, 0, 0), 1.0);
    }

    #[test]
    fn standoff_hues_are_all_distinct() {
        let hues: Vec<RGB8> = (1..=4).map(standoff_hue).collect();
        for i in 0..hues.len() {
            for j in i + 1..hues.len() {
                assert_ne!(hues[i], hues[j], "standoff {i} and {j} share a hue");
            }
        }
        assert_eq!(standoff_hue(0), standoff_hue(1));
        assert_eq!(standoff_hue(9), standoff_hue(4));
    }

    #[test]
    fn checker_advances_every_three_squares() {
        assert_eq!(checker_light(0, 0, 0), checker_light(2, 0, 0));
        assert_ne!(checker_light(0, 0, 0), checker_light(3, 0, 0));
        assert_ne!(checker_light(0, 0, 0), checker_light(0, 3, 0));
    }

    #[test]
    fn scene_has_cool_cube_warm_ledge_and_player() {
        let world = World::four_cubes();
        let mut player = Player::new(PhysicsMode::GridMoveGated);
        // Walk off the south edge onto the warm staircase.
        for _ in 0..7 {
            player.apply_intent(&world, Intent::South);
        }
        let frame = render_frame(&world, &player, 0.0, 100, 48, 0);
        let colors = painted(&frame);
        let warm = colors
            .iter()
            .any(|c| c.r > 150 && c.r > c.b + 40 && c.g < c.r);
        let cool = colors.iter().any(|c| c.b >= c.r && c.r > 20);
        assert!(warm, "a warm ledge hue should be visible");
        assert!(cool, "cool cube/wall geometry should be visible");
        assert!(frame.glyphs().any(|g| g.bg_color == Some(PLAYER_COLOR)));
    }

    #[test]
    fn rotating_the_view_rearranges_the_frame() {
        let world = World::four_cubes();
        let mut player = Player::new(PhysicsMode::GridMoveGated);
        for _ in 0..7 {
            player.apply_intent(&world, Intent::South);
        }
        let upright = render_frame(&world, &player, 0.0, 100, 48, 0);
        let turned = render_frame(&world, &player, 0.0, 100, 48, 1);
        assert_ne!(
            upright.uncolored_regular_string(),
            turned.uncolored_regular_string(),
            "a quarter turn must change the rendered frame"
        );
        // The staircase ledges stay warm and the player stays visible.
        let warm = painted(&turned)
            .iter()
            .any(|c| c.r > 150 && c.r > c.b + 40 && c.g < c.r);
        assert!(warm, "warm ledges should still render after rotating");
        assert!(turned.glyphs().any(|g| g.bg_color == Some(PLAYER_COLOR)));
    }

    #[test]
    fn staircase_ledges_are_not_overwritten_by_the_cube_wall() {
        // Regression: painter order must use signed forward distance, or the
        // cube wall (nearer in screen rows but smaller |forward|) is drawn after
        // the southern staircase and erases it.
        let world = World::four_cubes();
        let player = Player::new(PhysicsMode::GridMoveGated);
        let (w, h) = (110usize, 50usize);
        let frame = render_frame(&world, &player, 0.0, w, h, 0);
        let cam = Camera::with_rotation(player.x.round(), player.y.round(), 0);
        // South staircase step 2 top: world (5, -2), surface altitude 6.
        let (col, row) = cam.project(w, h, 5.0, -2.0, 6.0);
        let color = frame.grid[row as usize][col as usize]
            .bg_color
            .expect("staircase cell should be painted");
        assert!(
            color.r > color.b + 25 && color.g < color.r,
            "expected a warm staircase ledge at ({col},{row}), got {color:?}"
        );
        // Its front face (one row below the top) is warm too, not cool wall.
        let (fcol, frow) = cam.project(w, h, 5.0, -2.0, 5.0);
        let front = frame.grid[frow as usize][fcol as usize]
            .bg_color
            .expect("staircase front should be painted");
        assert!(
            front.r > front.b + 15 && front.g < front.r,
            "expected a warm staircase front at ({fcol},{frow}), got {front:?}"
        );
    }

    #[test]
    fn player_is_horizontally_centered_under_camera_follow() {
        let world = World::four_cubes();
        let mut player = Player::new(PhysicsMode::GridMoveGated);
        player.apply_intent(&world, Intent::East);
        let frame = render_frame(&world, &player, 0.0, 80, 30, 0);
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
        let frame = render_frame(&world, &player, 0.0, 80, 30, 0);
        assert!(
            painted(&frame).contains(&TRAIL_COLOR),
            "trail should be drawn"
        );
        assert!(frame.glyphs().any(|g| g.bg_color == Some(PLAYER_COLOR)));
    }
}
