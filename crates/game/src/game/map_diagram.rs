//! Colorless ASCII dump of a map's placement (portals, blocks, entities),
//! independent of the renderer. Exists to answer "is the map wrong or is the
//! rendering wrong?" — a coherent diagram next to a broken-looking hall means
//! the bug is in `graphics`, not in `portal_geometry`.

use std::collections::{HashMap, HashSet};

use euclid::point2;

use utility::coordinate_frame_conversions::{WorldPoint, WorldSquare, WorldStep};
use utility::{STEP_DOWN, STEP_LEFT, STEP_RIGHT, STEP_UP};

use super::Game;

/// One portal face as it actually registers in `PortalGeometry`, i.e.
/// including the reverse/back faces that two-way and double-sided placements
/// add. Printing the raw faces (rather than a tidied pair) is the point: the
/// extra faces are what distinguish the three portal types.
struct DiagramPortal {
    entrance_square: WorldSquare,
    entrance_dir: WorldStep,
    exit_square: WorldSquare,
}

impl Game {
    pub fn ascii_diagram(&self) -> String {
        let portals = self.diagram_portals();
        let blocks = &self.blocks.blocks;
        let cubes = self
            .death_cubes
            .iter()
            .map(|cube| world_point_to_square(cube.position))
            .collect::<Vec<_>>();
        let player_square = self.try_get_player_square();

        let mut entrance_arrows: HashMap<WorldSquare, char> = HashMap::new();
        let mut exit_squares: HashSet<WorldSquare> = HashSet::new();
        for portal in &portals {
            let glyph = arrow_for_step(portal.entrance_dir);
            entrance_arrows
                .entry(portal.entrance_square)
                .and_modify(|existing| {
                    if *existing != glyph {
                        // Two different entries share a square; the arrow
                        // would lie either way.
                        *existing = '*';
                    }
                })
                .or_insert(glyph);
            exit_squares.insert(portal.exit_square);
        }

        let (min_x, max_x, min_y, max_y) = self.content_bounds(&portals, &cubes, player_square);

        let mut out = String::new();
        out.push_str(&format!(
            "Map diagram  x {}..{}  y {}..{}  (y increases upward)\n",
            min_x, max_x, min_y, max_y
        ));

        out.push_str("     ");
        for x in min_x..=max_x {
            out.push_str(&format!("{:>2} ", x.rem_euclid(100)));
        }
        out.push('\n');

        for y in (min_y..=max_y).rev() {
            out.push_str(&format!("{:>3}  ", y));
            for x in min_x..=max_x {
                let square = point2(x, y);
                let glyph = if let Some(&arrow) = entrance_arrows.get(&square) {
                    arrow
                } else if player_square == Some(square) {
                    '@'
                } else if cubes.contains(&square) {
                    '*'
                } else if exit_squares.contains(&square) {
                    'o'
                } else if blocks.contains(&square) {
                    '#'
                } else {
                    '.'
                };
                out.push_str(&format!(" {} ", glyph));
            }
            out.push('\n');
        }

        out.push_str("\nLegend\n");
        out.push_str("  @ player   * death cube   # block   o portal exit (enter-only face)\n");
        out.push_str("  portal entrances, by direction of travel through them:\n");
        for portal in &portals {
            out.push_str(&format!(
                "  {:>3} {} ({}, {}) -> ({}, {})\n",
                arrow_for_step(portal.entrance_dir),
                dir_name(portal.entrance_dir),
                portal.entrance_square.x,
                portal.entrance_square.y,
                portal.exit_square.x,
                portal.exit_square.y,
            ));
        }
        out
    }

    fn diagram_portals(&self) -> Vec<DiagramPortal> {
        let mut portals: Vec<DiagramPortal> = self
            .portal_geometry
            .iter_portals()
            .map(|portal| DiagramPortal {
                entrance_square: portal.entrance().square(),
                entrance_dir: portal.entrance().direction().step(),
                exit_square: portal.exit().square(),
            })
            .collect();
        portals.sort_by_key(|portal| {
            (
                portal.entrance_square.y,
                portal.entrance_square.x,
                portal.entrance_dir.x,
                portal.entrance_dir.y,
            )
        });
        portals
    }

    fn content_bounds(
        &self,
        portals: &[DiagramPortal],
        cubes: &[WorldSquare],
        player_square: Option<WorldSquare>,
    ) -> (i32, i32, i32, i32) {
        let mut squares: Vec<WorldSquare> = Vec::new();
        squares.extend(self.blocks.blocks.iter().copied());
        squares.extend(self.pieces.keys().copied());
        squares.extend(cubes.iter().copied());
        squares.extend(player_square);
        for portal in portals {
            squares.push(portal.entrance_square);
            squares.push(portal.exit_square);
        }
        assert!(!squares.is_empty(), "map has no content to diagram");

        let min_x = squares.iter().map(|s| s.x).min().unwrap() - 1;
        let max_x = squares.iter().map(|s| s.x).max().unwrap() + 1;
        let min_y = squares.iter().map(|s| s.y).min().unwrap() - 1;
        let max_y = squares.iter().map(|s| s.y).max().unwrap() + 1;
        (min_x, max_x, min_y, max_y)
    }
}

fn world_point_to_square(point: WorldPoint) -> WorldSquare {
    point2(point.x.round() as i32, point.y.round() as i32)
}

fn arrow_for_step(step: WorldStep) -> char {
    if step == STEP_RIGHT {
        '>'
    } else if step == STEP_LEFT {
        '<'
    } else if step == STEP_UP {
        '^'
    } else if step == STEP_DOWN {
        'v'
    } else {
        '?'
    }
}

fn dir_name(step: WorldStep) -> &'static str {
    if step == STEP_RIGHT {
        "right"
    } else if step == STEP_LEFT {
        "left"
    } else if step == STEP_UP {
        "up"
    } else if step == STEP_DOWN {
        "down"
    } else {
        "?"
    }
}
