//! Runtime debug dumps: the full game state, the rendered screen, and the
//! input history as of a moment during play. Triggered by pressing 'p' so a
//! transient rendering bug can be captured from a live session.

use std::path::{Path, PathBuf};
use std::str::FromStr;
use std::time::{Duration, Instant};

use euclid::Angle;
use serde::Deserialize;
use termion::event::{Event, Key, MouseButton, MouseEvent};

use crate::game::{DeathCube, FloatingEntityId, FloatingHunterDrone, Game, IncubatingPawn, Player, Widget};
use crate::piece::{Faction, Piece, PieceType, Upgrade};
use terminal_rendering::glyph::Glyph;
use utility::coordinate_frame_conversions::{
    BoardSize, WorldMove, WorldPoint, WorldSquare, WorldStep,
};
use utility::{KingWorldStep, QuarterTurnsAnticlockwise, SquareWithOrthogonalDir};

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

// --- Loading -----------------------------------------------------------------
//
// The writer above emits a flat, JSON-friendly projection of `Game`. Loading
// mirrors that projection with a DTO whose fields are only plain JSON shapes
// (arrays, numbers, strings); no internal game type derives serde. The DTO is
// then applied through the same `place_*` builders the map set-up code uses.

#[derive(Deserialize)]
struct SnapshotData {
    #[serde(default)]
    running: bool,
    board_width: u32,
    board_height: u32,
    turn_count: u32,
    world_time_seconds: f32,
    player: Option<PlayerDto>,
    #[serde(default)]
    pieces: Vec<PieceDto>,
    #[serde(default)]
    blocks: Vec<[i32; 2]>,
    #[serde(default)]
    upgrades: Vec<UpgradeDto>,
    #[serde(default)]
    conveyor_belts: Vec<DirectedDto>,
    #[serde(default)]
    floor_push_arrows: Vec<DirectedDto>,
    #[serde(default)]
    widgets: Vec<WidgetDto>,
    #[serde(default)]
    incubating_pawns: Vec<IncubatingDto>,
    #[serde(default)]
    death_cubes: Vec<CubeDto>,
    #[serde(default)]
    floating_hunter_drones: Vec<DroneDto>,
    #[serde(default)]
    portals: Vec<PortalDto>,
    screen: ScreenDto,
}

#[derive(Deserialize)]
struct PlayerDto {
    position: [i32; 2],
    faced_direction: [i32; 2],
    blink_range: u32,
}

#[derive(Deserialize)]
struct PieceDto {
    square: [i32; 2],
    #[serde(rename = "type")]
    piece_type: String,
    faction: FactionDto,
    faced_direction: Option<[i32; 2]>,
}

#[derive(Deserialize)]
struct UpgradeDto {
    square: [i32; 2],
    #[serde(rename = "type")]
    upgrade_type: String,
}

#[derive(Deserialize)]
struct DirectedDto {
    square: [i32; 2],
    direction: [i32; 2],
}

#[derive(Deserialize)]
struct WidgetDto {
    square: [i32; 2],
    val: u32,
}

#[derive(Deserialize)]
struct IncubatingDto {
    square: [i32; 2],
    age_in_turns: u32,
    faction: FactionDto,
}

#[derive(Deserialize)]
struct CubeDto {
    id: u64,
    position: [f32; 2],
    velocity: [f32; 2],
}

#[derive(Deserialize)]
struct DroneDto {
    id: u64,
    position: [f32; 2],
    velocity: [f32; 2],
    sight_direction_degrees: f32,
}

#[derive(Deserialize)]
struct PortalDto {
    entrance: PoseDto,
    exit: PoseDto,
}

#[derive(Deserialize)]
struct PoseDto {
    square: [i32; 2],
    direction: [i32; 2],
}

#[derive(Deserialize)]
struct ScreenDto {
    terminal_width: u16,
    terminal_height: u16,
    rotation_quarter_turns: i32,
}

/// Factions serialize either as a bare name (`"red_pawn"`) or as an enemy
/// object (`{"enemy": 0}`), so serde needs an untagged union here.
#[derive(Deserialize)]
#[serde(untagged)]
enum FactionDto {
    Name(String),
    Enemy { enemy: u32 },
}

impl FactionDto {
    fn to_faction(&self) -> Faction {
        match self {
            FactionDto::Name(name) => match name.as_str() {
                "unaligned" => Faction::Unaligned,
                "red_pawn" => Faction::RedPawn,
                "death_cube" => Faction::DeathCube,
                other => panic!("Unknown faction '{other}' in snapshot"),
            },
            FactionDto::Enemy { enemy } => Faction::Enemy(*enemy),
        }
    }

    fn enemy_id(&self) -> Option<u32> {
        match self {
            FactionDto::Enemy { enemy } => Some(*enemy),
            FactionDto::Name(_) => None,
        }
    }
}

fn square_from_array(value: [i32; 2]) -> WorldSquare {
    WorldSquare::new(value[0], value[1])
}

fn step_from_array(value: [i32; 2]) -> WorldStep {
    WorldStep::new(value[0], value[1])
}

fn point_from_array(value: [f32; 2]) -> WorldPoint {
    WorldPoint::new(value[0], value[1])
}

fn move_from_array(value: [f32; 2]) -> WorldMove {
    WorldMove::new(value[0], value[1])
}

fn pose_from_dto(pose: &PoseDto) -> SquareWithOrthogonalDir {
    SquareWithOrthogonalDir::from_square_and_step(
        square_from_array(pose.square),
        step_from_array(pose.direction),
    )
}

/// Read a snapshot directory's `game_state.json` and rebuild the `Game` it
/// describes. `start_time` anchors the reconstructed world clock, so passing
/// `Instant::now()` resumes realtime effects from the captured elapsed time.
pub(crate) fn load_snapshot_game(dir: &Path) -> Result<Game, String> {
    let path = dir.join("game_state.json");
    let contents = std::fs::read_to_string(&path)
        .map_err(|error| format!("Could not read {}: {error}", path.display()))?;
    let data: SnapshotData = serde_json::from_str(&contents)
        .map_err(|error| format!("Could not parse {}: {error}", path.display()))?;
    Ok(Game::from_snapshot(data, Instant::now()))
}

impl Game {
    /// Rebuild a game from the DTO emitted by `game_state_json`. Transient
    /// visual state (in-flight animations, selectors, mouse smoothing) is
    /// intentionally not restored; only persistent model state is.
    fn from_snapshot(data: SnapshotData, start_time: Instant) -> Game {
        let mut game =
            Game::new(data.screen.terminal_width, data.screen.terminal_height, start_time);

        // The terminal-derived board size can differ from the captured one
        // (maps like `racetrack` clamp the terminal), so trust the snapshot.
        game.board_size = BoardSize::new(data.board_width, data.board_height);
        game.running = data.running;
        game.turn_count = data.turn_count;
        game.world_start_time = start_time;
        game.world_time = start_time + Duration::from_secs_f32(data.world_time_seconds.max(0.0));
        game.graphics
            .screen
            .set_rotation(QuarterTurnsAnticlockwise::new(data.screen.rotation_quarter_turns));

        if let Some(player) = data.player {
            game.player_optional = Some(Player {
                position: square_from_array(player.position),
                faced_direction: KingWorldStep::new(step_from_array(player.faced_direction)),
                blink_range: player.blink_range,
            });
        }

        // Non-overlapping by construction, but order matches the emptiness
        // guards on `place_block`/`place_upgrade`/`place_piece`.
        for &square in &data.blocks {
            game.place_block(square_from_array(square));
        }
        for upgrade in &data.upgrades {
            let upgrade_type = Upgrade::from_str(&upgrade.upgrade_type)
                .unwrap_or_else(|_| panic!("Unknown upgrade '{}' in snapshot", upgrade.upgrade_type));
            game.place_upgrade(upgrade_type, square_from_array(upgrade.square));
        }
        for piece in &data.pieces {
            let piece_type = PieceType::from_str(&piece.piece_type)
                .unwrap_or_else(|_| panic!("Unknown piece type '{}' in snapshot", piece.piece_type));
            let mut new_piece = Piece::new(piece_type, piece.faction.to_faction());
            if let Some(direction) = piece.faced_direction {
                new_piece.set_faced_direction(KingWorldStep::new(step_from_array(direction)));
            }
            game.place_piece(new_piece, square_from_array(piece.square));
        }
        for belt in &data.conveyor_belts {
            game.place_conveyor_belt(
                square_from_array(belt.square),
                step_from_array(belt.direction),
            );
        }
        for arrow in &data.floor_push_arrows {
            game.place_floor_push_arrow(
                square_from_array(arrow.square),
                step_from_array(arrow.direction),
            );
        }
        for widget in &data.widgets {
            game.place_widget(Widget::new(widget.val), square_from_array(widget.square));
        }
        for pawn in &data.incubating_pawns {
            game.incubating_pawns.insert(
                square_from_array(pawn.square),
                IncubatingPawn {
                    age_in_turns: pawn.age_in_turns,
                    faction: pawn.faction.to_faction(),
                },
            );
        }
        for cube in &data.death_cubes {
            game.death_cubes.push(DeathCube::new(
                FloatingEntityId(cube.id),
                point_from_array(cube.position),
                move_from_array(cube.velocity),
            ));
        }
        for drone in &data.floating_hunter_drones {
            game.floating_hunter_drones.push(FloatingHunterDrone::new(
                FloatingEntityId(drone.id),
                point_from_array(drone.position),
                move_from_array(drone.velocity),
                Angle::degrees(drone.sight_direction_degrees),
            ));
        }
        // Every registered portal face is dumped, so replaying each as an
        // entrance reconstructs the exact entrance->exit map.
        for portal in &data.portals {
            game.portal_geometry
                .create_portal(pose_from_dto(&portal.entrance), pose_from_dto(&portal.exit));
        }

        // Counters aren't serialized; recover them from the highest id in use
        // so future spawns don't collide with loaded entities/factions.
        let next_floating_entity_id = data
            .death_cubes
            .iter()
            .map(|cube| cube.id)
            .chain(data.floating_hunter_drones.iter().map(|drone| drone.id))
            .max();
        game.next_floating_entity_id = next_floating_entity_id.map_or(0, |id| id + 1);

        let next_faction_id = data
            .pieces
            .iter()
            .map(|piece| piece.faction.enemy_id())
            .chain(
                data.incubating_pawns
                    .iter()
                    .map(|pawn| pawn.faction.enemy_id()),
            )
            .flatten()
            .max();
        if let Some(id) = next_faction_id {
            game.faction_factory.ensure_id_at_least(id + 1);
        }

        game
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::utils_for_tests::set_up_game_with_player;
    use euclid::vec2;
    use utility::{STEP_DOWN, STEP_LEFT, STEP_RIGHT, STEP_UP};

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

    #[test]
    fn snapshot_round_trips_through_serialize_and_load() {
        let mut game = set_up_game_with_player();
        game.set_up_simple_portal_map();
        let base = game.player_square();
        game.place_piece(Piece::pawn(), base + STEP_RIGHT);
        game.place_piece(Piece::arrow(STEP_UP.into()), base + STEP_LEFT);
        game.place_block(base + STEP_DOWN);
        game.place_upgrade(Upgrade::BlinkRange, base + STEP_DOWN * 2);
        game.place_conveyor_belt(base + STEP_DOWN * 3, STEP_RIGHT);
        game.place_floor_push_arrow(base + STEP_DOWN * 4, STEP_LEFT);
        game.place_widget(Widget::new(3), base + STEP_DOWN * 5);
        game.place_linear_death_cube(base.to_f32() + vec2(0.5, 0.5), vec2(1.0, 0.0));
        game.place_floating_hunter_drone(
            base.to_f32() + vec2(1.5, 1.5),
            vec2(0.0, 1.0),
            Angle::radians(0.5),
        );
        game.incubating_pawns.insert(
            base + STEP_RIGHT * 2,
            IncubatingPawn {
                age_in_turns: 4,
                faction: Faction::RedPawn,
            },
        );
        // Zero the clock so the two snapshots have nothing to drift on.
        game.world_time = game.world_start_time;

        let json = game_state_json(&game, Some("test"));
        let data: SnapshotData = serde_json::from_str(&json).expect("parse snapshot");
        let loaded = Game::from_snapshot(data, game.graphics().start_time());

        assert_eq!(game_state_json(&loaded, Some("test")), json);
    }

    #[test]
    fn loaded_snapshot_reproduces_rendered_screen() {
        let mut game = set_up_game_with_player();
        game.set_up_simple_portal_map();
        game.place_piece(Piece::pawn(), game.player_square() + STEP_RIGHT);
        game.world_time = game.world_start_time;

        let start_time = game.graphics().start_time();
        game.draw_headless_at_duration_from_start(std::time::Duration::ZERO);
        let before = screen_text(&game);
        let json = game_state_json(&game, Some("test"));

        let data: SnapshotData = serde_json::from_str(&json).expect("parse snapshot");
        let mut loaded = Game::from_snapshot(data, start_time);
        loaded.draw_headless_at_duration_from_start(std::time::Duration::ZERO);

        assert_eq!(screen_text(&loaded), before);
    }
}

/// Headless snapshot tooling, enabled with the `debug-tools` feature (see
/// `crates/game/src/bin/snapshot_tool.rs`). Lives in a descendant module of
/// `snapshot`, so it can reach the private DTO/loader without widening the
/// crate's normal public surface.
#[cfg(feature = "debug-tools")]
pub mod debug {
    use std::path::{Path, PathBuf};
    use std::time::{Duration, Instant};

    use regex::Regex;
    use rgb::RGB8;

    use super::{
        game_state_json, load_snapshot_game, screen_text, snapshot_dir, Game, SnapshotData,
    };

    /// Render `dir/game_state.json` headless at the captured world time and
    /// return the ANSI grid. Rendering at the captured elapsed time is what
    /// makes time-dependent visuals (e.g. death-cube technicolor) reproduce.
    pub fn render_snapshot_headless(dir: &Path) -> Result<String, String> {
        let path = dir.join("game_state.json");
        let contents = std::fs::read_to_string(&path)
            .map_err(|error| format!("Could not read {}: {error}", path.display()))?;
        let data: SnapshotData = serde_json::from_str(&contents)
            .map_err(|error| format!("Could not parse {}: {error}", path.display()))?;
        let elapsed = Duration::from_secs_f32(data.world_time_seconds.max(0.0));
        let mut game = Game::from_snapshot(data, Instant::now());
        game.draw_headless_at_duration_from_start(elapsed);
        Ok(screen_text(&game))
    }

    /// The repo-root `snapshot/` directory.
    pub fn default_snapshot_dir() -> PathBuf {
        snapshot_dir()
    }

    /// Re-serialize a loaded game's persistent state (for round-trip checks).
    pub fn game_state_json_of(game: &Game, map_name: Option<&str>) -> String {
        game_state_json(game, map_name)
    }

    /// Load a snapshot directory into a live `Game` (resumes realtime effects).
    pub fn load_snapshot_dir(dir: &Path) -> Result<Game, String> {
        load_snapshot_game(dir)
    }

    /// One terminal cell as encoded in `screen.txt`: character + fg/bg RGB.
    #[derive(Clone, Copy, Debug, PartialEq, Eq)]
    pub struct Cell {
        pub character: char,
        pub fg: RGB8,
        pub bg: RGB8,
    }

    /// Parse the ANSI stream written by `screen_text` back into a row-major
    /// grid. `Glyph::bg_transparent` is not encoded in the text, so it is not
    /// represented here (and is ignored by all comparisons).
    pub fn parse_screen_text(text: &str) -> Result<Vec<Vec<Cell>>, String> {
        let cell_re = Regex::new(
            r"\x1b\[48;2;(\d+);(\d+);(\d+)m\x1b\[38;2;(\d+);(\d+);(\d+)m(.)\x1b\[49m\x1b\[39m",
        )
        .map_err(|error| format!("bad cell regex: {error}"))?;

        let mut grid = Vec::new();
        for (row, line) in text.split('\n').enumerate() {
            if line.is_empty() {
                continue;
            }
            let mut cells = Vec::new();
            for caps in cell_re.captures_iter(line) {
                let num =
                    |i: usize| caps[i].parse::<u8>().map_err(|e| format!("bad color component: {e}"));
                let bg = RGB8::new(num(1)?, num(2)?, num(3)?);
                let fg = RGB8::new(num(4)?, num(5)?, num(6)?);
                let character = caps[7]
                    .chars()
                    .next()
                    .ok_or_else(|| format!("row {row}: empty glyph capture"))?;
                cells.push(Cell {
                    character,
                    fg,
                    bg,
                });
            }
            if cells.is_empty() {
                return Err(format!(
                    "row {row}: no cells parsed (not a screen_text stream?)"
                ));
            }
            grid.push(cells);
        }
        Ok(grid)
    }

    pub fn parse_screen_text_file(path: &Path) -> Result<Vec<Vec<Cell>>, String> {
        let text = std::fs::read_to_string(path)
            .map_err(|error| format!("Could not read {}: {error}", path.display()))?;
        parse_screen_text(&text)
    }

    #[derive(Clone, Copy, Debug)]
    pub struct CellDiff {
        pub row: usize,
        pub col: usize,
        pub expected: Cell,
        pub actual: Cell,
    }

    /// Cells that differ, in row-major order.
    pub fn diff_cells(expected: &[Vec<Cell>], actual: &[Vec<Cell>]) -> Vec<CellDiff> {
        let mut diffs = Vec::new();
        for (row, (exp_row, act_row)) in expected.iter().zip(actual.iter()).enumerate() {
            for (col, (exp, act)) in exp_row.iter().zip(act_row.iter()).enumerate() {
                if exp != act {
                    diffs.push(CellDiff {
                        row,
                        col,
                        expected: *exp,
                        actual: *act,
                    });
                }
            }
        }
        diffs
    }

    fn show_cell(cell: &Cell) -> String {
        format!(
            "{:?} fg({},{},{}) bg({},{},{})",
            cell.character, cell.fg.r, cell.fg.g, cell.fg.b, cell.bg.r, cell.bg.g, cell.bg.b
        )
    }

    /// Human-readable diff report: per-row character overlay (first `MAX_ROWS`)
    /// plus explicit cell diffs (first `MAX_CELLS`).
    pub fn render_diff_report(expected: &[Vec<Cell>], actual: &[Vec<Cell>]) -> String {
        const MAX_ROWS: usize = 60;
        const MAX_CELLS: usize = 200;

        let mut out = String::new();
        if expected.len() != actual.len() {
            out.push_str(&format!(
                "row count differs: expected {} got {}\n",
                expected.len(),
                actual.len()
            ));
        }
        let width = expected.first().map_or(0, Vec::len);
        if actual.first().map_or(0, Vec::len) != width {
            out.push_str(&format!(
                "col count differs: expected {} got {}\n",
                width,
                actual.first().map_or(0, Vec::len)
            ));
        }

        let diffs = diff_cells(expected, actual);
        out.push_str(&format!("{} differing cells\n", diffs.len()));

        let mut rows_with_diffs: Vec<usize> = diffs.iter().map(|d| d.row).collect();
        rows_with_diffs.dedup();
        for row in rows_with_diffs.iter().take(MAX_ROWS) {
            if let (Some(exp), Some(act)) = (expected.get(*row), actual.get(*row)) {
                let chars =
                    |cells: &[Cell]| cells.iter().map(|c| c.character).collect::<String>();
                let markers: String = (0..width)
                    .map(|col| {
                        let same = exp
                            .get(col)
                            .zip(act.get(col))
                            .map_or(true, |(a, b)| a == b);
                        if same {
                            ' '
                        } else {
                            '^'
                        }
                    })
                    .collect();
                out.push_str(&format!("row {row}\n"));
                out.push_str(&format!("  exp {}\n", chars(exp)));
                out.push_str(&format!("  got {}\n", chars(act)));
                out.push_str(&format!("      {markers}\n"));
            }
        }

        for diff in diffs.iter().take(MAX_CELLS) {
            out.push_str(&format!(
                "  ({row}, {col}) exp: {exp}\n           got: {act}\n",
                row = diff.row,
                col = diff.col,
                exp = show_cell(&diff.expected),
                act = show_cell(&diff.actual),
            ));
        }
        if diffs.len() > MAX_CELLS {
            out.push_str(&format!("  ... {} more\n", diffs.len() - MAX_CELLS));
        }
        out
    }

    /// Compare a headless render against `dir/screen.txt`.
    pub fn diff_snapshot(dir: &Path) -> Result<(bool, String), String> {
        let rendered = render_snapshot_headless(dir)?;
        let expected = parse_screen_text_file(&dir.join("screen.txt"))?;
        let actual = parse_screen_text(&rendered)?;
        let matches = expected == actual;
        Ok((matches, render_diff_report(&expected, &actual)))
    }

    /// Overwrite `dir/screen.txt` with a headless render (canonization).
    pub fn bless_snapshot(dir: &Path) -> Result<PathBuf, String> {
        let rendered = render_snapshot_headless(dir)?;
        let path = dir.join("screen.txt");
        std::fs::write(&path, rendered)
            .map_err(|error| format!("Could not write {}: {error}", path.display()))?;
        Ok(path)
    }

    #[cfg(test)]
    mod tests {
        use super::*;
        use crate::utils_for_tests::set_up_game_with_player;

        /// The parser must recover exactly the character/fg/bg trio the screen
        /// buffer held (transparency is not encoded, and is not compared).
        #[test]
        fn screen_text_round_trips_through_parser() {
            let mut game = set_up_game_with_player();
            game.draw_headless_at_duration_from_start(Duration::ZERO);

            let text = screen_text(&game);
            let parsed = parse_screen_text(&text).expect("parse screen_text");

            let screen = &game.graphics().screen;
            let width = screen.terminal_width as usize;
            let height = screen.terminal_height as usize;
            let expected: Vec<Vec<Cell>> = (0..height)
                .map(|y| {
                    (0..width)
                        .map(|x| {
                            let glyph = screen.screen_buffer[x][y];
                            Cell {
                                character: glyph.character,
                                fg: glyph.fg_color,
                                bg: glyph.bg_color,
                            }
                        })
                        .collect()
                })
                .collect();

            assert_eq!(parsed, expected);
        }

        #[test]
        fn diff_report_is_empty_for_identical_grids() {
            let grid = parse_screen_text(
                "\x1b[48;2;0;0;0m\x1b[38;2;255;255;255m \x1b[49m\x1b[39m",
            )
            .expect("parse single cell");
            assert!(diff_cells(&grid, &grid).is_empty());
            assert_eq!(render_diff_report(&grid, &grid), "0 differing cells\n");
        }

        /// A headless render must not depend on wall-clock reads: two loads of
        /// the same state, rendered at the captured time, must be byte-equal.
        /// Guards against re-introducing `Instant::now()` in the render path.
        #[test]
        fn headless_render_is_deterministic_across_loads() {
            use euclid::vec2;
            use utility::coordinate_frame_conversions::WorldPoint;

            let mut game = set_up_game_with_player();
            game.place_linear_death_cube(WorldPoint::new(5.5, 4.5), vec2(1.0, 0.0));
            game.world_time = game.world_start_time;

            let json = game_state_json(&game, Some("test"));
            let dir = std::env::temp_dir().join(format!(
                "widgetmancer_snapshot_determinism_{}",
                std::process::id()
            ));
            std::fs::create_dir_all(&dir).expect("create temp snapshot dir");
            std::fs::write(dir.join("game_state.json"), json).expect("write game_state.json");

            let first = render_snapshot_headless(&dir).expect("first render");
            let second = render_snapshot_headless(&dir).expect("second render");
            std::fs::remove_dir_all(&dir).ok();

            assert_eq!(first, second);
        }
    }
}
