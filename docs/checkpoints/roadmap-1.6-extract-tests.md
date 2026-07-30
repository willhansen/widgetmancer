# Checkpoint — Roadmap 1.6: Extract inline tests to `game/tests.rs`

Sub-step of [ROADMAP.md](../../ROADMAP.md) item 1 ("Split the `game.rs` god module"),
plan step 6: move the inline `#[cfg(test)]` tests out of the god module.

**Status:** DONE — `cargo build --tests` green, `cargo nextest run`: 470 passed,
11 skipped (identical to the 1.5 baseline, with identical `game::tests::*`
test paths). No `#[cfg(test)]` remains in `mod.rs` except the module
declaration.

Notes on how it landed:

- `game/tests.rs` holds the entire former test module body: 116 test fns
  (~2,500 lines), declared in `mod.rs` as `#[cfg(test)] mod tests;`.
- Chosen target was a **child module**, not `crates/game/tests/` integration
  tests: the tests access private fields (`game.pieces`, `game.blocks`, …)
  and private methods (`reflect_off_board_edges`, `square_is_empty`,
  `raw_set_player_faced_direction`, `get_new_faction`, …) in dozens of
  places. A child module keeps all of that via `use super::*;` with zero
  visibility changes; integration tests would have forced ~20 items into
  the public API.
- The move was a straight cut/paste: the test module was the last thing in
  `mod.rs` (lines 1543–4039), and its imports (`use super::*;` plus
  fov_stuff/graphics/piece/`utils_for_tests`/glyph constants) all resolved
  unchanged — sibling-module items (`DeathCube`, `IncubatingPawn`,
  `TURNS_TO_SPAWN_PAWN`, …) still reach the tests through `mod.rs`'s
  existing re-exports. No import fixes were needed; the build was clean on
  the first try.
- The `#[ignore = "TODO"]` tests moved verbatim — resolving them is roadmap
  item 3, deliberately not conflated with this move.
- All `set_up_*` map builders and helpers stayed in `mod.rs` (they're
  non-test map scripting used by demos too); tests call them through the
  unchanged `impl Game` surface.

Result: `mod.rs` 4039 → 1544 lines; `tests.rs` is 2,500 lines.

## Item 1 "done when" assessment

- "No module in `game/` exceeds ~1.5k LOC": `mod.rs` is at 1,544 — right at
  the threshold. `tests.rs` at 2.5k is test-only and excludable in spirit,
  but worth noting.
- "`game.rs` is primarily the `Game` struct and its core accessors": **not
  yet** — `mod.rs` still holds combat (`do_player_*`, captures, smite),
  portal placement/geometry, rendering glue, real-time ticks
  (`tick_conveyor_belts`, `tick_death_cubes`), and the `set_up_*` map
  builders. If further shrinkage is wanted, the natural next sub-steps are
  a `game/combat.rs` and a `game/realtime.rs`; otherwise item 1 can be
  called done here.
