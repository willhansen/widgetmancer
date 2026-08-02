# Checkpoint — Roadmap 1.4: Extract `game/spawning.rs`

Sub-step of [ROADMAP.md](../ROADMAP.md) item 1 ("Split the `game.rs` god module"),
plan step 4: move pawn/drone spawn logic out of the god module.

**Status:** DONE — `cargo build` green, `cargo nextest run`: 470 passed,
11 skipped (identical to the 1.3 baseline). Public `Game` API unchanged.

Notes on how it landed:

- `game/spawning.rs` holds the `IncubatingPawn` struct, the
  `TURNS_TO_SPAWN_PAWN` const, and an `impl Game` block with:
  `place_new_king_pawn_faction`, `place_random_3x3_faction`,
  `place_linear_death_cube`, `place_piece`, `place_red_pawn`,
  `place_death_turret`, `place_floating_hunter_drone`, `place_upgrade`,
  `tick_pawn_incubation`, `empty_squares_surrounded_by_pawns_of_one_faction`,
  `random_empty_square`, `place_piece_randomly`, `place_block_randomly`,
  `get_new_faction`.
- The line drawn for "spawning" vs. the rest: piece/pawn/drone/incubation and
  random-placement helpers moved; generic map geometry (`place_block`,
  `place_arrow`, portals, conveyor belts, floor push arrows, widgets) and all
  `set_up_*` test/demo map builders stayed in `mod.rs` — the `set_up_*`
  functions are map scripting, not spawn mechanics, and they call the moved
  methods through the unchanged `pub` API.
- `mod.rs` re-exports `pub use spawning::IncubatingPawn;` (it's a field type
  of `Game` and part of the existing public surface) and
  `pub(crate) use spawning::TURNS_TO_SPAWN_PAWN;` (inline tests in `mod.rs`
  loop over it).
- All moved methods were already `pub`, so no visibility changes were needed
  and external callers (`lib.rs`, `utils_for_tests.rs`) are untouched.
- `spawning.rs` imports `DeathCube`/`FloatingHunterDrone` via `super::`
  (sibling module `floating_entities`) and needed `itertools::Itertools`,
  `rand::Rng`, `euclid::{vec2, Angle}`, `std::collections::HashMap` — the
  glob imports in `mod.rs` don't propagate to child modules.

Result: `mod.rs` 4361 → 4197 lines; `spawning.rs` is 189 lines.

## Follow-ups (rest of roadmap item 1, in order)

5. `game/turns.rs` — `move_all_pieces`, `on_turn_end`, `convert_orphaned_pieces`,
   game-over handling (`kill_player`, `quit`)
6. Move inline `#[cfg(test)]` tests to `game/tests.rs` or `crates/game/tests/`
   (this is the big one: ~2500 lines of the remaining ~4200)
