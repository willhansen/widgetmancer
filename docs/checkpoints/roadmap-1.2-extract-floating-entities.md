# Checkpoint — Roadmap 1.2: Extract `game/floating_entities.rs`

Sub-step of [ROADMAP.md](../../ROADMAP.md) item 1 ("Split the `game.rs` god module"),
plan step 2: move `DeathCube`, `FloatingHunterDrone`, and the ambassador-based
`FloatingEntityTrait` machinery out of the god module.

**Status:** DONE — `cargo build` green, `cargo nextest run`: 470 passed,
11 skipped (identical to the 1.1 baseline). Public `Game` API unchanged.

Notes on how it landed:

- `game/floating_entities.rs` holds `FloatingEntityTrait`
  (`#[delegatable_trait]`), `FloatingEntityEnum` (`pub(crate)`,
  `#[derive(From, Delegate)]`), `DeathCube` + trait impl, `FloatingHunterDrone`
  + trait impl + `new()`, and `HUNTER_DRONE_SIGHT_RANGE`.
- `mod.rs` re-exports `pub use floating_entities::{DeathCube,
  FloatingEntityTrait, FloatingHunterDrone, HUNTER_DRONE_SIGHT_RANGE};`, so
  `graphics.rs`'s `use crate::game::{...}` and the inline tests keep working
  unchanged. The `ambassador` import moved to the new module (no longer in
  `mod.rs`).
- Fields of both structs are `pub(crate)` — `mod.rs` previously accessed them
  directly from the same module (e.g. `clone_drone.velocity`,
  `death_cube.position`, `sight_direction +=`); marking fields `pub(crate)`
  preserved that behavior verbatim instead of rewriting ~20 access sites.
- `DeathCube` gained a `new(position, velocity)` constructor because
  `shoot_death_cube` built it with a struct literal (now invalid across module
  boundary); the call site changed to `DeathCube::new(...)`.

**Stays in `mod.rs` (for now):**

- `Game` fields `death_cubes`, `floating_hunter_drones`, `death_cube_faction` —
  per-tick simulation state; grouping them into a storage struct like `Blocks`
  is possible but the tick methods are more entangled than blocks were.
- Orchestration methods needing `Game`'s other subsystems:
  `tick_death_cubes`, hunter-drone scan/steer logic,
  `slide_floating_entity_with_portal_awareness` (generic over the trait),
  the `floating_entities_at_start` / `floating_entities_in_square` split-join
  helpers, and all `draw_*` wrappers.

## Follow-ups (rest of roadmap item 1, in order)

3. `game/ai.rs` — enemy pathfinding/decisions
4. `game/spawning.rs` — pawn/drone spawn logic
5. `game/turns.rs` — turn advancement, game-over
6. Move inline `#[cfg(test)]` tests to `game/tests.rs` or `crates/game/tests/`
