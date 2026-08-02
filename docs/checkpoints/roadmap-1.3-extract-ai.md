# Checkpoint — Roadmap 1.3: Extract `game/ai.rs`

Sub-step of [ROADMAP.md](../ROADMAP.md) item 1 ("Split the `game.rs` god module"),
plan step 3: move enemy pathfinding/decision logic out of the god module.

**Status:** DONE — `cargo build` green, `cargo nextest run`: 470 passed,
11 skipped (identical to the 1.2 baseline). Public `Game` API unchanged.

Notes on how it landed:

- `game/ai.rs` is a single `impl Game` block in a child module — no `Game`
  fields moved. It holds faction/movement decision logic:
  `non_arrow_piece_squares`, `move_non_arrow_factions`, `get_enemy_factions`,
  `squares_of_pieces_in_faction`, `move_faction`,
  `square_of_closest_piece_to_player_in_faction`, `move_piece`, `slide_cast`,
  `square_to_move_toward_player_for_piece_at`, `piece_can_capture_player`,
  `highest_priority_capture_square_for_piece_at`,
  `allies_within_radius_excluding_center`, `protection_strengths_from_given_pawns`,
  `orthogonal_adjacency_from_given_squares`, `move_red_pawn_at`,
  `move_piece_at_square_and_return_end_position_if_moved`,
  `turn_piece_toward_player`, `move_options_for_piece_at`,
  `on_board_move_or_capture_squares_for_piece_at`,
  `on_board_capture_squares_for_piece_at`, `on_board_move_squares_for_piece_at`,
  `move_squares_for_all_pieces`, `squares_threatened_by_any_piece`,
  `guarded_squares_for_piece_at`, `find_king_path` (A*-ish king pathfinding via
  `DoublePriorityQueue`), `capture_options_for_piece_at`.
- Previously-`fn` methods are now `pub(crate)`: private items of a child module
  aren't visible to the parent, and `mod.rs` still calls some of them
  (`move_all_pieces` → `move_non_arrow_factions`; `convert_orphaned_pieces` →
  `get_enemy_factions`) and inline tests exercise others. Existing `pub`
  methods kept `pub` so external callers are untouched.
- Reverse direction needs no special casing: `ai.rs` calls `mod.rs`-private
  methods (`kill_player`, `square_is_empty`, `multiple_portal_aware_steps`,
  …) directly, because child modules can access ancestors' private items.
- Imports: `ai.rs` got a minimal explicit set (`HashMap`/`HashSet`,
  `Itertools`, `OrderedFloat`, `DoublePriorityQueue`, `euclid::{vec2, Angle}`,
  `piece::*`, `crate::*`, `terminal_rendering::*`, `utility::*`,
  `super::Game`) rather than a copy of `mod.rs`'s header.
- `get_new_faction` (faction factory, not AI) stayed in `mod.rs` — it's used
  by `Game::new` and spawning code that belongs to step 4.
- `move_all_pieces` stayed in `mod.rs` — it advances `turn_count` and calls
  `tick_projectile_arrows`, so it belongs to step 5 (turns).

Result: `mod.rs` 4856 → 4361 lines; `ai.rs` is 516 lines.

## Follow-ups (rest of roadmap item 1, in order)

4. `game/spawning.rs` — pawn/drone spawn logic (`tick_pawn_incubation`,
   `place_*`, `set_up_*` map builders — decide the line between "spawning"
   and "test/demo map setup" when carving)
5. `game/turns.rs` — `move_all_pieces`, `on_turn_end`, `convert_orphaned_pieces`,
   game-over handling
6. Move inline `#[cfg(test)]` tests to `game/tests.rs` or `crates/game/tests/`
