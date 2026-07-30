# Checkpoint — Roadmap 1.1: Extract `game/blocks.rs`

Sub-step of [ROADMAP.md](../../ROADMAP.md) item 1 ("Split the `game.rs` god module"),
plan step 1: move block/wall/conveyor/upgrade types out of the god module.

**Status:** DONE — Steps 1–4 complete. `cargo build` green, `cargo nextest run`:
470 passed, 11 skipped. Public `Game` API unchanged (thin delegating shims).

Notes on how it landed:
- `blocks.rs` holds `Blocks` storage struct (public fields `upgrades`, `blocks`,
  `conveyor_belts`), `FloorFeature`, both `CONVEYOR_BELT_*` constants, free fn
  `conveyor_belt_speed()`, and `conveyor_period_just_elapsed(prev_time, delta)`.
- `mod.rs` re-exports all of the above (`pub use blocks::{...}`), so tests and
  `graphics.rs` keep working via `crate::game::...`.
- `Game` gained a single `blocks: Blocks` field; `place_block`, `is_block_at`,
  `place_conveyor_belt`, `place_upgrade` (assert kept on `Game`),
  `is_upgrade_at`, and `Game::conveyor_belt_speed()` are thin delegations.
- `tick_conveyor_belts` uses `conveyor_period_just_elapsed` verbatim (float math
  unchanged); the test in `game::tests` that touched `game.upgrades` now goes
  through `game.blocks.upgrades`.

---

## Goal

Peel the self-contained "placed world features" (solid blocks, conveyor belts,
upgrades) out of `game.rs` into `game/blocks.rs`, keeping the build green and
the public `Game` API unchanged throughout. These come first because they are
three storage fields, two constants, and ~15 methods with no entanglement with
combat or AI.

## Step 1 — Mechanical rename (DONE, uncommitted)

`game.rs` becomes a directory module so submodules can be peeled off one at a
time while everything compiles:

```
crates/game/src/game.rs  →  crates/game/src/game/mod.rs
```

`lib.rs` is untouched; `mod game;` resolves to `game/mod.rs`.

> **Resume note:** the rename was done with plain `mv` (this checkout is not a
> git repository — `.git/` is empty — so `git mv` and history-following are
> unavailable). The verifying `cargo build` after the rename was aborted before
> running. **First action when resuming: run `cargo build` and `cargo nextest
> run` to confirm the rename alone is green.**

## Step 2 — Create `crates/game/src/game/blocks.rs`

Move in:

- Constants `CONVEYOR_BELT_MOVEMENT_PERIOD`, `CONVEYOR_BELT_VISUAL_PERIOD`
  (currently `mod.rs` lines ~112–113).
- `FloorFeature` enum (`PushArrow`, `ConveyorBelt`) — lines ~123–127.
- A new `Blocks` storage struct owning the three collections currently inline
  in `Game` (lines ~179–183):
  - `upgrades: HashMap<WorldSquare, Upgrade>`
  - `blocks: HashSet<WorldSquare>`
  - `conveyor_belts: HashMap<WorldSquare, OrthogonalWorldStep>`
- Pure accessors/mutators as `impl Blocks`: `place_block`, `is_block_at`,
  `place_conveyor_belt`, `place_upgrade`, `is_upgrade_at`, plus
  `conveyor_belt_speed()` as a free function and a
  `conveyor_period_just_elapsed(prev_time, delta)` helper holding the
  period-boundary math from `tick_conveyor_belts` (lines ~950–957).

See the design sketch in the conversation that produced this document for the
full `blocks.rs` outline.

**Stays in `mod.rs` (for now):**

- `GridEntity::Block` — the enum mixes `Player`/`Widget`, so it belongs with
  piece handling, not blocks.
- Methods with real orchestration logic, which keep needing `Game`'s other
  subsystems but will operate on `self.blocks.<field>`:
  - `tick_conveyor_belts` (needs `simultaneously_push_*`)
  - `place_upgrade` on `Game` (has the `square_is_empty` assert)
  - `place_block_randomly` (needs `random_empty_square`)
  - `draw_blocks` / `draw_conveyor_belts` / upgrade drawing wrappers
    (need `self.graphics`)

## Step 3 — Wire into `Game` (`game/mod.rs`)

- `mod blocks;` + `pub use blocks::{FloorFeature, CONVEYOR_BELT_*};`
  (re-export keeps existing references, including tests, working).
- Replace the three `Game` fields with `blocks: Blocks`; fix the constructor
  (lines ~208–212).
- Keep the existing public `Game` methods as thin delegations
  (`self.blocks.place_block(square)` etc.) so **no call sites change**.
  Known direct field accesses to update (from a grep of the pre-split file):
  lines ~344, 512, 516–518, 612, 621, 635–637, 961, 1114, 1211, 2040,
  2047, 2050 (approximate, pre-split numbering).

## Step 4 — Verify

```
cargo build && cargo nextest run
```

Green = done. Then check off the step in this document and note it in
ROADMAP.md item 1's progress.

## Known risks / gotchas

- **Not a git repo:** no version-control safety net; keep changes small and
  verify after each step. Consider `git init` or making a backup copy before
  Step 3.
- `tick_conveyor_belts` math is float-based (`as_secs_f32` floor comparison) —
  move it verbatim into `conveyor_period_just_elapsed`; do not "improve" it in
  the same pass.
- `CONVEYOR_BELT_VISUAL_PERIOD` is used by `graphics.rs` (drawing) — the
  re-export path must stay `crate::game::CONVEYOR_BELT_VISUAL_PERIOD`-compatible.

## Follow-ups (rest of roadmap item 1, in order)

2. `game/floating_entities.rs` — `DeathCube`, `FloatingHunterDrone`,
   `FloatingEntityTrait` (self-contained, uses `ambassador` delegation)
3. `game/ai.rs` — enemy pathfinding/decisions
4. `game/spawning.rs` — pawn/drone spawn logic
5. `game/turns.rs` — turn advancement, game-over
6. Move inline `#[cfg(test)]` tests to `game/tests.rs` or `crates/game/tests/`

Each step reuses the delegating-shim pattern so the public `Game` API never
breaks mid-refactor; shims get inlined at the end.
