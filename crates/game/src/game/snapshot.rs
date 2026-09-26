//! Runtime debug dumps: the full game state, the rendered screen, and the
//! input history as of a moment during play. Triggered by pressing 'p' so a
//! transient rendering bug can be captured from a live session.

use std::path::PathBuf;

use termion::event::{Event, Key, MouseButton, MouseEvent};

use crate::game::Game;
use crate::piece::{Faction, Piece};
use terminal_rendering::glyph::Glyph;
use utility::coordinate_frame_conversions::{WorldMove, WorldPoint, WorldSquare, WorldStep};

pub const SNAPSHOT_KEY: Key = Key::Char('p');

pub struct InputRecord {
    pub millis_from_start: u128,
    pub event: Event,
}

/// The repo root's `snapshot/` directory. Anchored to the crate manifest so
/// it does not depend on the working directory the game was launched from.
pub fn snapshot_dir() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../../snapshot")
}

pub fn write_snapshot(game: &Game, input_history: &[InputRecord], map_name: Option<&str>) {
    let dir = snapshot_dir();
    if let Err(error) = std::fs::create_dir_all(&dir) {
        eprintln!("Could not create snapshot directory {}: {error}", dir.display());
        return;
    }

    write_file(&dir.join("screen.txt"), &screen_text(game));
    write_file(
        &dir.join("game_state.json"),
        &game_state_json(game, map_name),
    );
    write_file(
        &dir.join("input_history.json"),
        &input_history_json(input_history),
    );

    eprintln!("Wrote debug snapshot to {}", dir.display());
}

fn write_file(path: &std::path::Path, contents: &str) {
    if let Err(error) = std::fs::write(path, contents) {
        eprintln!("Could not write {}: {error}", path.display());
    }
}

/// The rendered character grid as ANSI-colored text, one line per terminal
/// row — the same encoding the blessed rendering tests use.
fn screen_text(game: &Game) -> String {
    let screen = &game.graphics.screen;
    let width = screen.terminal_width as usize;
    let height = screen.terminal_height as usize;
    let mut out = String::new();
    for y in 0..height {
        for x in 0..width {
            out.push_str(&screen.screen_buffer[x][y].to_string());
        }
        out.push_str(&Glyph::reset_colors());
        out.push('\n');
    }
    out
}

fn game_state_json(game: &Game, map_name: Option<&str>) -> String {
    let mut fields = vec![
        ("map", opt_string_json(map_name)),
        ("running", game.running.to_string()),
        ("board_width", game.board_size.width.to_string()),
        ("board_height", game.board_size.height.to_string()),
        ("turn_count", game.turn_count.to_string()),
        (
            "world_time_seconds",
            game.world_time_since_start().as_secs_f32().to_string(),
        ),
        ("player", player_json(game)),
        ("pieces", json_sorted_array(game.pieces.iter().map(|(&square, &piece)| piece_json(square, &piece)))),
        ("blocks", json_sorted_array(game.blocks.blocks.iter().map(|&square| square_json(square)))),
        (
            "upgrades",
            json_sorted_array(game.blocks.upgrades.iter().map(|(&square, &upgrade)| {
                json_object([
                    ("square", square_json(square)),
                    ("type", string_json(&upgrade.to_string())),
                ])
            })),
        ),
        (
            "conveyor_belts",
            json_sorted_array(
                game.blocks
                    .conveyor_belts
                    .iter()
                    .map(|(&square, step)| directed_square_json(square, step.step())),
            ),
        ),
        (
            "floor_push_arrows",
            json_sorted_array(
                game.floor_push_arrows
                    .iter()
                    .map(|(&square, step)| directed_square_json(square, step.step())),
            ),
        ),
        (
            "widgets",
            json_sorted_array(game.widgets.iter().map(|(&square, widget)| {
                json_object([
                    ("square", square_json(square)),
                    ("val", widget.val().to_string()),
                ])
            })),
        ),
        (
            "incubating_pawns",
            json_sorted_array(game.incubating_pawns.iter().map(|(&square, pawn)| {
                json_object([
                    ("square", square_json(square)),
                    ("age_in_turns", pawn.age_in_turns.to_string()),
                    ("faction", faction_json(pawn.faction)),
                ])
            })),
        ),
        ("death_cubes", death_cubes_json(game)),
        ("floating_hunter_drones", hunter_drones_json(game)),
        ("portals", portals_json(game)),
        ("screen", screen_state_json(game)),
    ];

    // Stable output makes snapshots diffable across runs.
    fields.sort_by_key(|(name, _)| *name);

    json_object(fields)
}

fn player_json(game: &Game) -> String {
    match &game.player_optional {
        None => "null".to_string(),
        Some(player) => json_object([
            ("position", square_json(player.position)),
            ("faced_direction", step_json(player.faced_direction.step())),
            ("blink_range", player.blink_range.to_string()),
        ]),
    }
}

fn piece_json(square: WorldSquare, piece: &Piece) -> String {
    let faced_direction = if piece.can_turn() {
        step_json(piece.faced_direction().step())
    } else {
        "null".to_string()
    };
    json_object([
        ("square", square_json(square)),
        ("type", string_json(&piece.piece_type.to_string())),
        ("faction", faction_json(piece.faction)),
        ("faced_direction", faced_direction),
    ])
}

fn death_cubes_json(game: &Game) -> String {
    json_sorted_array(game.death_cubes.iter().map(|cube| {
        json_object([
            ("id", cube.id.0.to_string()),
            ("position", point_json(cube.position)),
            ("velocity", move_json(cube.velocity)),
        ])
    }))
}

fn hunter_drones_json(game: &Game) -> String {
    json_sorted_array(game.floating_hunter_drones.iter().map(|drone| {
        json_object([
            ("id", drone.id.0.to_string()),
            ("position", point_json(drone.position)),
            ("velocity", move_json(drone.velocity)),
            (
                "sight_direction_degrees",
                drone.sight_direction.radians.to_degrees().to_string(),
            ),
        ])
    }))
}

fn portals_json(game: &Game) -> String {
    let mut portals: Vec<_> = game.portal_geometry.iter_portals().collect();
    portals.sort_by_key(|portal| {
        let entrance = portal.entrance().square();
        let exit = portal.exit().square();
        (entrance.x, entrance.y, exit.x, exit.y)
    });
    json_sorted_array(portals.into_iter().map(|portal| {
        json_object([
            ("entrance", pose_json(portal.entrance())),
            ("exit", pose_json(portal.exit())),
        ])
    }))
}

fn screen_state_json(game: &Game) -> String {
    let screen = &game.graphics.screen;
    json_object([
        ("terminal_width", screen.terminal_width.to_string()),
        ("terminal_height", screen.terminal_height.to_string()),
        (
            "rotation_quarter_turns",
            screen.rotation().quarter_turns().to_string(),
        ),
        ("origin", square_json(screen.screen_origin_as_world_square())),
        ("center", square_json(screen.screen_center_as_world_square())),
    ])
}

fn input_history_json(input_history: &[InputRecord]) -> String {
    json_array(input_history.iter().map(|record| {
        json_object([
            ("t_ms", record.millis_from_start.to_string()),
            ("event", event_json(&record.event)),
        ])
    }))
}

fn event_json(event: &Event) -> String {
    match event {
        Event::Key(key) => json_object([
            ("kind", string_json("key")),
            ("key", key_json(*key)),
        ]),
        Event::Mouse(mouse) => json_object([
            ("kind", string_json("mouse")),
            ("mouse", mouse_json(*mouse)),
        ]),
        Event::Unsupported(bytes) => json_object([
            ("kind", string_json("unsupported")),
            ("byte_count", bytes.len().to_string()),
        ]),
    }
}

fn key_json(key: Key) -> String {
    match key {
        Key::Char(c) => named_value_json("char", &c.to_string()),
        Key::Alt(c) => named_value_json("alt", &c.to_string()),
        Key::Ctrl(c) => named_value_json("ctrl", &c.to_string()),
        Key::F(n) => named_value_json("function", &n.to_string()),
        named => json_object([("type", string_json(&format!("{named:?}").to_lowercase()))]),
    }
}

fn mouse_json(mouse: MouseEvent) -> String {
    match mouse {
        MouseEvent::Press(button, x, y) => json_object([
            ("action", string_json("press")),
            ("button", string_json(&button_name(button))),
            ("x", x.to_string()),
            ("y", y.to_string()),
        ]),
        MouseEvent::Release(x, y) => json_object([
            ("action", string_json("release")),
            ("x", x.to_string()),
            ("y", y.to_string()),
        ]),
        MouseEvent::Hold(x, y) => json_object([
            ("action", string_json("hold")),
            ("x", x.to_string()),
            ("y", y.to_string()),
        ]),
    }
}

fn button_name(button: MouseButton) -> String {
    format!("{button:?}").to_lowercase()
}

fn named_value_json(name: &str, value: &str) -> String {
    json_object([
        ("type", string_json(name)),
        ("value", string_json(value)),
    ])
}

fn faction_json(faction: Faction) -> String {
    match faction {
        Faction::Unaligned => string_json("unaligned"),
        Faction::RedPawn => string_json("red_pawn"),
        Faction::DeathCube => string_json("death_cube"),
        Faction::Enemy(id) => json_object([("enemy", id.to_string())]),
    }
}

fn directed_square_json(square: WorldSquare, step: WorldStep) -> String {
    json_object([
        ("square", square_json(square)),
        ("direction", step_json(step)),
    ])
}

fn pose_json(pose: utility::SquareWithOrthogonalDir) -> String {
    json_object([
        ("square", square_json(pose.square())),
        ("direction", step_json(pose.direction().step())),
    ])
}

fn square_json(square: WorldSquare) -> String {
    format!("[{}, {}]", square.x, square.y)
}

fn point_json(point: WorldPoint) -> String {
    format!("[{}, {}]", point.x, point.y)
}

fn step_json(step: WorldStep) -> String {
    format!("[{}, {}]", step.x, step.y)
}

fn move_json(movement: WorldMove) -> String {
    format!("[{}, {}]", movement.x, movement.y)
}

fn opt_string_json(value: Option<&str>) -> String {
    value.map(string_json).unwrap_or_else(|| "null".to_string())
}

fn json_array(items: impl IntoIterator<Item = String>) -> String {
    let items: Vec<String> = items.into_iter().collect();
    format!("[{}]", items.join(", "))
}

/// Like `json_array`, but sorts elements so snapshots of unordered
/// collections (HashMaps, HashSets) are diffable across runs.
fn json_sorted_array(items: impl IntoIterator<Item = String>) -> String {
    let mut items: Vec<String> = items.into_iter().collect();
    items.sort();
    json_array(items)
}

fn json_object(fields: impl IntoIterator<Item = (&'static str, String)>) -> String {
    let fields: Vec<String> = fields
        .into_iter()
        .map(|(name, value)| format!("{}: {}", string_json(name), value))
        .collect();
    format!("{{{}}}", fields.join(", "))
}

fn string_json(value: &str) -> String {
    let mut out = String::with_capacity(value.len() + 2);
    out.push('"');
    for c in value.chars() {
        match c {
            '"' => out.push_str("\\\""),
            '\\' => out.push_str("\\\\"),
            '\n' => out.push_str("\\n"),
            '\r' => out.push_str("\\r"),
            '\t' => out.push_str("\\t"),
            c if (c as u32) < 0x20 => out.push_str(&format!("\\u{:04x}", c as u32)),
            c => out.push(c),
        }
    }
    out.push('"');
    out
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::utils_for_tests::set_up_game_with_player;
    use utility::STEP_RIGHT;

    #[test]
    fn game_state_json_includes_world_features() {
        let mut game = set_up_game_with_player();
        game.set_up_simple_portal_map();
        game.place_piece(Piece::pawn(), game.player_square() + STEP_RIGHT);

        let json = game_state_json(&game, Some("test"));

        assert!(json.starts_with('{') && json.ends_with('}'));
        assert!(json.contains("\"map\": \"test\""));
        assert!(json.contains("\"turn_count\": 0"));
        assert!(json.contains("\"player\": {"));
        assert!(json.contains("\"pieces\": ["));
        assert!(json.contains("\"portals\": ["));
        assert!(json.contains("\"type\": \"OmniDirectionalPawn\""));
    }

    #[test]
    fn screen_text_has_one_line_per_terminal_row() {
        let mut game = set_up_game_with_player();
        game.draw_headless_now();

        let text = screen_text(&game);

        assert_eq!(
            text.lines().count(),
            game.graphics.screen.terminal_height as usize
        );
    }

    #[test]
    fn input_history_json_records_keys_and_mouse() {
        let history = vec![
            InputRecord {
                millis_from_start: 5,
                event: Event::Key(Key::Char('q')),
            },
            InputRecord {
                millis_from_start: 9,
                event: Event::Mouse(MouseEvent::Press(MouseButton::Left, 3, 4)),
            },
        ];

        let json = input_history_json(&history);

        assert!(json.contains("\"t_ms\": 5"));
        assert!(json.contains("\"type\": \"char\""));
        assert!(json.contains("\"value\": \"q\""));
        assert!(json.contains("\"action\": \"press\""));
        assert!(json.contains("\"x\": 3"));
    }
}
