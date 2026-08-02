# Checkpoint — Roadmap 1.5: Extract `game/turns.rs`

Sub-step of [ROADMAP.md](../ROADMAP.md) item 1 ("Split the `game.rs` god module"),
plan step 5: move turn advancement and game-over handling out of the god module.

**Status:** DONE — `cargo build` green, `cargo nextest run`: 470 passed,
11 skipped (identical to the 1.4 baseline). Public `Game` API unchanged.

Notes on how it landed:

- `game/turns.rs` holds an `impl Game` block with:
  - turn advancement: `tick_game_logic`, `on_turn_end`,
    `convert_orphaned_pieces`, `move_all_pieces`
  - per-turn mechanics driven by `tick_game_logic`: `tick_floor_push_arrows`,
    `tick_projectile_arrows`, `drain_arrows`/`set_arrows`, and the two
    `simultaneously_push_*` helpers
  - game-over handling: `kill_player` (moved from `ai.rs`, where it only
    lived because the AI called it), `quit`, `running`
- The line drawn for "turns" vs. the rest: `tick_conveyor_belts` stayed in
  `mod.rs` because it is real-time (`Duration`-based), not turn-based — a
  future real-time-tick extraction can take it. Because it calls the two
  `simultaneously_push_*` helpers, those are `pub(crate)` (same convention
  as `ai.rs`: private-in-spirit, visible to `mod.rs` orchestration).
  Combat (`do_player_*` attacks, `try_capture_piece_at`/`capture_piece_at`/
  `smite`) and death-cube logic stayed — candidates for a future combat
  module / `floating_entities` extension respectively.
- The `running: bool` and `turn_count` fields (and the `turn_count`
  accessor) stayed in `mod.rs` with the `Game` struct; only methods moved.
- No re-exports were needed from `mod.rs` — unlike 1.4, everything moved is
  a method called through `Game`, not a named type/const.
- All moved methods kept their existing visibility (`pub` or `pub(crate)`),
  so external callers (`lib.rs`: `tick_game_logic`, `running`, `quit`,
  `move_all_pieces`; `utils_for_tests.rs`; inline tests calling
  `on_turn_end`) are untouched.
- `turns.rs` needed `std::collections::{HashMap, HashSet}` and
  `itertools::Itertools` (for `all_equal` in `convert_orphaned_pieces`) —
  the glob imports in `mod.rs` don't propagate to child modules.

Result: `mod.rs` 4197 → 4039 lines; `turns.rs` is 186 lines.

## Follow-ups (rest of roadmap item 1)

6. Move inline `#[cfg(test)]` tests to `game/tests.rs` or
   `crates/game/tests/` (this is the big one: ~2500 lines of the remaining
   ~4000). Modest further shrinks possible: combat (`do_player_*`,
   captures, smite), death cubes, real-time ticks (`tick_conveyor_belts`,
   `tick_death_cubes`).
