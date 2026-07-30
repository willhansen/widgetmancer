# Checkpoint — Roadmap 1.7: Extract `game/combat.rs` and `game/realtime.rs`

Sub-step of [ROADMAP.md](../../ROADMAP.md) item 1 ("Split the `game.rs` god module"),
added step 7: get `mod.rs` to "primarily the `Game` struct and its core
accessors" by extracting combat and real-time effects.

**Status:** DONE — `cargo build --tests` green, `cargo nextest run`: 470
passed, 11 skipped (identical to the 1.6 baseline). Public `Game` API
unchanged.

Notes on how it landed:

- `game/combat.rs` (176 lines): `do_player_radial_attack`,
  `do_player_spear_attack`, `do_player_shoot_arrow`,
  `do_player_shoot_shotgun`, `do_player_shoot_sniper`,
  `smite_selected_square`, `smite`, `apply_upgrade`, `capture_piece_at`,
  `try_capture_piece_at`.
- `game/realtime.rs` (192 lines): `tick_realtime_effects`,
  `world_time_since_start`, `tick_realtime_turrets`, `tick_death_cubes`,
  `kill_along_line`, `tick_hunter_drones`,
  `slide_floating_entity_with_portal_awareness`, `reflect_off_board_edges`,
  `tick_conveyor_belts` (moved here from `mod.rs`, resolving the open
  question from 1.5), `remove_death_cubes_that_are_off_board`.
- The line drawn: `raycast` stayed in `mod.rs` (it's geometry/query, used
  by rendering too); `place_*` builders, `set_up_*` map scripting, portal
  placement, selection, and all rendering glue stayed.
- Visibility bumps (all `pub(crate)`, same convention as earlier steps):
  `reflect_off_board_edges` (called by tests), `world_time_since_start`
  (called by conveyor-belt visual code in `mod.rs`),
  `slide_floating_entity_with_portal_awareness` (called by floating-entity
  push code in `mod.rs`).
- `realtime.rs` imports the private `GridEntity` enum and the
  `floating_entities`/`blocks` re-exports via `super::` — child modules can
  see parent privates, so no visibility changes were needed for those.
- Both new files needed their own `use` sets (globs don't propagate to
  child modules): combat needs `euclid::{vec2, Angle}` and
  `Upgrade::BlinkRange`; realtime needs `Duration`, `HashMap`, `HashSet`,
  `Angle`, `num::clamp`, and `super::conveyor_period_just_elapsed`.

Result: `mod.rs` 1544 → 1219 lines; largest remaining items are the `Game`
struct, accessors, portal placement, rendering glue, and `set_up_*` map
builders.

## Item 1 "done when" — MET

- No non-test module in `game/` exceeds ~1.5k LOC (largest is `ai.rs` at
  512; `tests.rs` at 2.5k is test-only).
- `mod.rs` is now primarily the `Game` struct, core accessors, map
  construction (`place_*`/`set_up_*`), and rendering — the mechanics all
  live in named submodules (`blocks`, `floating_entities`, `ai`,
  `spawning`, `turns`, `combat`, `realtime`).

Cumulative: `game.rs` went from ~4,900 LOC / 121 `pub fn`s to a 1,219-line
`mod.rs` + 7 focused submodules.
