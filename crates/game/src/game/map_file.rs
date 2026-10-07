//! JSON map definitions: an ordered list of setup operations applied to a
//! `Game`, plus its board size and player start. Unlike a snapshot (which
//! records exact state), a map file is a *recipe* — maps own their board size
//! and are independent of the terminal.
//!
//! Files live in the repo's `maps/` directory as `<name>.json` and are selected
//! with `--map <name>` (see `set_up_map_by_name`).

use std::path::PathBuf;

use euclid::point2;
use rgb::RGB8;
use serde::Deserialize;

use utility::coordinate_frame_conversions::{BoardSize, WorldSquare, WorldStep, WorldVoxel};
use utility::{SquareWithOrthogonalDir, STEP_DOWN, STEP_LEFT, STEP_RIGHT, STEP_UP};

use super::{Game, TerrainMaterial, SLAB_VOXEL_Z};

/// Side length of a `space-cubes` cube, in world squares (the demo's constant).
const CUBE_SIZE: i32 = 10;

/// Warm ledge tints, approximating the demo's standoff ramp (amber → orange →
/// crimson → violet). Index 0 is reserved for the innermost ring.
const LEDGE_RAMP: [RGB8; 4] = [
    RGB8::new(240, 176, 64),
    RGB8::new(232, 120, 44),
    RGB8::new(214, 64, 72),
    RGB8::new(176, 64, 156),
];

/// The repo's `maps/` directory, anchored to the crate manifest.
pub fn maps_dir() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../../maps")
}

/// Load `maps/<name>.json`, if it exists and parses.
pub fn load_map_file(name: &str) -> Option<MapFile> {
    let path = maps_dir().join(format!("{name}.json"));
    let contents = std::fs::read_to_string(&path).ok()?;
    match serde_json::from_str(&contents) {
        Ok(map) => Some(map),
        Err(error) => panic!("Could not parse map file {}: {error}", path.display()),
    }
}

/// A map recipe.
#[derive(Deserialize, Debug, Clone)]
pub struct MapFile {
    /// The board's [width, height] in squares. Independent of the terminal.
    pub board: Option<[u32; 2]>,
    /// Remove the default full-board floor before running `ops`, for maps with
    /// real void gaps.
    #[serde(default)]
    pub clear_floor: bool,
    #[serde(default)]
    pub ops: Vec<MapOp>,
    /// Optional player sight radius for this map.
    pub sight_radius: Option<u32>,
    /// Player start square; applied after `ops` so it sees the built terrain.
    pub player: Option<[i32; 2]>,
}

/// One setup operation. `{"op": "cuboid", ...}`.
#[derive(Deserialize, Debug, Clone)]
#[serde(tag = "op", rename_all = "snake_case")]
pub enum MapOp {
    FillFloorRect { x: i32, y: i32, width: i32, height: i32 },
    ClearFloor,
    Cuboid { x: i32, y: i32, z: i32, width: i32, depth: i32, height: i32, tint: Option<[u8; 3]> },
    Column { x: i32, y: i32, height: u32, tint: Option<[u8; 3]> },
    Voxel { x: i32, y: i32, z: i32, tint: Option<[u8; 3]> },
    /// The demo's cube side platforms (south staircase + east/west ledges),
    /// anchored at the cube's lower-left `(x, y)`.
    CubeSidePlatforms { x: i32, y: i32 },
    /// A two-way, double-sided portal: entering `entrance` moving
    /// `entrance_dir` emerges at `exit` moving `exit_dir`, and the reverse
    /// faces exist too (see `Game::place_double_sided_two_way_portal`).
    DoubleSidedTwoWayPortal {
        entrance: [i32; 2],
        entrance_dir: MapDir,
        exit: [i32; 2],
        exit_dir: MapDir,
    },
    /// A stationary death-cube turret.
    DeathTurret { x: i32, y: i32 },
}

/// A world-space orthogonal direction, named as it reads on screen. `up` is
/// `+y` in world squares.
#[derive(Deserialize, Debug, Clone, Copy)]
#[serde(rename_all = "snake_case")]
pub enum MapDir {
    Up,
    Down,
    Left,
    Right,
}

impl MapDir {
    fn to_step(self) -> WorldStep {
        match self {
            MapDir::Up => STEP_UP,
            MapDir::Down => STEP_DOWN,
            MapDir::Left => STEP_LEFT,
            MapDir::Right => STEP_RIGHT,
        }
    }
}

fn tint_of(tint: Option<[u8; 3]>) -> Option<RGB8> {
    tint.map(|[r, g, b]| RGB8::new(r, g, b))
}

impl Game {
    /// Apply a map recipe: set the board, optionally clear the default floor,
    /// run the ops in order, then place the player.
    pub fn apply_map_file(&mut self, map: &MapFile) {
        if let Some([width, height]) = map.board {
            self.board_size = BoardSize::new(width, height);
            // Recipes own their board; derive the default floor from it rather
            // than inheriting the terminal-sized slab from `Game::new`.
            self.seed_board_floor_for_current_board();
        }
        if map.clear_floor {
            self.terrain.clear_floor();
        }
        if let Some(radius) = map.sight_radius {
            self.set_player_sight_radius(radius);
        }
        for op in &map.ops {
            self.apply_map_op(op);
        }
        if let Some([x, y]) = map.player {
            self.place_player(point2(x, y));
        }
    }

    fn apply_map_op(&mut self, op: &MapOp) {
        match *op {
            MapOp::FillFloorRect { x, y, width, height } => {
                for yy in y..y + height {
                    for xx in x..x + width {
                        self.place_voxel(WorldVoxel::new(xx, yy, SLAB_VOXEL_Z));
                    }
                }
            }
            MapOp::ClearFloor => self.terrain.clear_floor(),
            MapOp::Cuboid { x, y, z, width, depth, height, tint } => {
                for zz in z..z + height {
                    for yy in y..y + depth {
                        for xx in x..x + width {
                            self.place_voxel(WorldVoxel::new(xx, yy, zz));
                        }
                    }
                }
                if let Some(tint) = tint_of(tint) {
                    for yy in y..y + depth {
                        for xx in x..x + width {
                            self.set_terrain_material(
                                point2(xx, yy),
                                TerrainMaterial::Tint(tint),
                            );
                        }
                    }
                }
            }
            MapOp::Column { x, y, height, tint } => {
                let square = point2(x, y);
                match tint_of(tint) {
                    Some(tint) => self.place_solid_column_with_material(
                        square,
                        height,
                        TerrainMaterial::Tint(tint),
                    ),
                    None => self.place_solid_column(square, height),
                }
            }
            MapOp::Voxel { x, y, z, tint } => {
                self.place_voxel(WorldVoxel::new(x, y, z));
                if let Some(tint) = tint_of(tint) {
                    self.set_terrain_material(point2(x, y), TerrainMaterial::Tint(tint));
                }
            }
            MapOp::CubeSidePlatforms { x, y } => self.place_cube_side_platforms(point2(x, y)),
            MapOp::DoubleSidedTwoWayPortal {
                entrance,
                entrance_dir,
                exit,
                exit_dir,
            } => self.place_double_sided_two_way_portal(
                SquareWithOrthogonalDir::from_square_and_worldstep(
                    point2(entrance[0], entrance[1]),
                    entrance_dir.to_step(),
                ),
                SquareWithOrthogonalDir::from_square_and_worldstep(
                    point2(exit[0], exit[1]),
                    exit_dir.to_step(),
                ),
            ),
            MapOp::DeathTurret { x, y } => self.place_death_turret(point2(x, y)),
        }
    }

    /// The demo's per-cube side platforms: a descending south staircase and
    /// east/west ledges, as thin single-voxel slabs. Warm tints approximate the
    /// demo's standoff ramp.
    fn place_cube_side_platforms(&mut self, origin: WorldSquare) {
        let (cx, cy) = (origin.x, origin.y);
        // South staircase: (dy south of the south edge, x start offset, slab z).
        for (dy, x_start, z) in [(1, 3, 7), (2, 4, 5), (3, 5, 3), (4, 6, 1)] {
            let tint = LEDGE_RAMP[(dy - 1) as usize];
            let y = cy - dy;
            for x in cx + x_start..cx + x_start + 3 {
                self.place_voxel(WorldVoxel::new(x, y, z));
                self.set_terrain_material(point2(x, y), TerrainMaterial::Tint(tint));
            }
        }
        // East and west pairs: (offset from the face, y start, slab z, x length).
        for (offset, y_start, z, length) in [(0, 2, 6, 3), (1, 3, 3, 3)] {
            let tint = LEDGE_RAMP[offset as usize];
            let east_start = cx + CUBE_SIZE + offset;
            let west_start = cx - 1 - offset - (length - 1);
            for step in 0..length {
                for y in cy + y_start..cy + y_start + 5 {
                    for x in [east_start + step, west_start + step] {
                        self.place_voxel(WorldVoxel::new(x, y, z));
                        self.set_terrain_material(point2(x, y), TerrainMaterial::Tint(tint));
                    }
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use euclid::point2;

    #[test]
    fn map_file_parses_and_applies_ops() {
        let json = r#"{
            "board": [10, 10],
            "clear_floor": true,
            "ops": [
                { "op": "column", "x": 2, "y": 2, "height": 3 },
                { "op": "voxel", "x": 4, "y": 4, "z": 6 }
            ],
            "player": [2, 2]
        }"#;
        let map: MapFile = serde_json::from_str(json).expect("parse map");
        let mut game = Game::new(20, 10, crate::LogicalTime::ZERO);
        game.apply_map_file(&map);

        assert_eq!(game.board_size(), BoardSize::new(10, 10));
        assert_eq!(game.height_at(point2(2, 2)), Some(3));
        assert_eq!(game.height_at(point2(4, 4)), Some(7), "floating voxel");
        assert_eq!(game.height_at(point2(0, 0)), None, "floor cleared");
        assert_eq!(game.player_square(), point2(2, 2));
    }

    #[test]
    fn space_cubes_file_has_the_demo_layout() {
        // Skip silently if the file is absent (e.g. a packaged build).
        let Some(map) = load_map_file("space-cubes") else {
            return;
        };
        assert_eq!(map.board, Some([42, 38]));
        assert!(map.clear_floor);
        assert_eq!(map.player, Some([9, 9]));
    }
}
