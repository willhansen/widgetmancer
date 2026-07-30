# Roadmap — Architecture Improvements

Tracked recommendations from the architecture review (see [ARCHITECTURE.md](ARCHITECTURE.md)).

Check off items in the same commit that completes them, then move them to
**Done** with a date. Keep evidence (file names, LOC counts, test locations)
with each item so the context doesn't have to be re-discovered later.

---

## Open

### 1. Split the `game.rs` god module
- **Evidence:** `crates/game/src/game.rs` is ~4,900 LOC with 121 `pub fn`s and ~59 `unwrap()`s.
  It contains board state, turn handling, combat, enemy AI, spawning, block types,
  floating entities, and inline tests.
- **Plan:** extract submodules incrementally, keeping each step compiling:
  1. `game/blocks.rs` — block/wall/conveyor/upgrade block types **[DONE]** — see
     `docs/checkpoints/roadmap-1.1-extract-blocks.md`
  2. `game/floating_entities.rs` — `DeathCube`, `FloatingHunterDrone`, `FloatingEntityTrait` **[DONE]** — see
     `docs/checkpoints/roadmap-1.2-extract-floating-entities.md`
  3. `game/ai.rs` — enemy pathfinding/decision logic **[DONE]** — see
     `docs/checkpoints/roadmap-1.3-extract-ai.md`
  4. `game/spawning.rs` — pawn/drone spawn logic **[DONE]** — see
     `docs/checkpoints/roadmap-1.4-extract-spawning.md`
  5. `game/turns.rs` — turn advancement and game-over handling **[DONE]** — see
     `docs/checkpoints/roadmap-1.5-extract-turns.md`
  6. Move inline `#[cfg(test)]` tests into `crates/game/tests/` or `game/tests.rs`
- **Done when:** no module in `game/` exceeds ~1.5k LOC; `game.rs` is primarily
  the `Game` struct and its core accessors.

### 2. Remove globally suppressed warnings
- **Evidence:** `#![allow(warnings)]` in `crates/game/src/lib.rs`;
  `#![allow(dead_code)]` + `#![allow(deprecated)]` in `crates/utility/src/lib.rs`.
- **Plan:** remove the blanket allows one crate at a time (`utility` first —
  it has the fewest deps), fixing lints per-category rather than re-adding
  narrower allows. Add `cargo clippy` to the workflow once clean.
- **Done when:** workspace builds warning-free on stable, no crate-root
  `#![allow(warnings)]` remains.

### 3. Resolve ignored tests on core mechanics
- **Evidence:** at least 5 `#[ignore = "TODO"]` tests in `crates/game/src/game.rs`
  (lines ~3882, 3977, 4725, 4893, 4912), plus open correctness TODOs in
  `fov_stuff.rs` (sorting ambiguity at line ~702) and `portal_geometry.rs`
  (second-portal handling at line ~242).
- **Plan:** for each ignored test: either fix the underlying behavior, fix the
  test's assumptions, or delete it with a comment explaining why it's not
  testable. Priority order: portal FOV > pathfinding determinism > the rest.
- **Progress:**
  - FIXED: `portal_playground::test_render_with_center_offset` — root cause was
    in `fov_stuff.rs`: `OctantFOVSquareSequenceIter` partitioned squares by the
    static integer octant wedge, so with a fractional `center_offset`, squares
    whose angular extent straddles an octant boundary were only enumerated in one
    octant; the other octant's visible sliver was silently dropped, producing
    spurious partial visibility on an empty board. Fix: each octant ring now also
    yields the one-square band just past its diagonal (straddlers are filtered by
    the existing arc-overlap check; complementary partials then combine into full
    visibility). Also fixed two center-convention inconsistencies unmasked along
    the way: `portal_aware_field_of_view_from_point` now picks the center square
    with `round_ties_even` (keeping `center_offset` within the asserted
    [-0.5, 0.5]; ties break consistently when the view point is exactly on a
    square boundary), and `portal_playground::render_camera` positions the fov
    rect using the same convention instead of flooring the unadjusted center.
- **Done when:** `cargo nextest run` runs the full suite with zero ignored
  tests (or only ignored tests with documented justification).

### 4. Replace glob imports across crate boundaries
- **Evidence:** `use utility::*` in `crates/game/src/lib.rs` and
  `crates/game/src/game.rs`; `terminal_rendering` re-exports `utility::*`
  (`crates/terminal_rendering/src/lib.rs`), blurring the crate layering.
- **Plan:** switch to explicit imports, then remove the `pub use utility::*`
  re-export from `terminal_rendering` so `game` depends on `utility` directly
  for utility types.
- **Done when:** no `use utility::*` or `use terminal_rendering::*` globs
  remain outside test code; layering is visible from imports alone.

### 5. Harden hot paths against panics
- **Evidence:** ~180 `unwrap()` calls in non-test source, concentrated in
  `game.rs` (59), `fov_stuff.rs` (48), `drawable_glyph.rs` (32).
  The panic hook restores the terminal, but the game still crashes.
- **Plan:** audit `unwrap()`s in FOV and rendering first. Replace with
  `Result` propagation where recovery is possible; where the invariant is
  real, convert to `expect("<why this can't fail>")` so failures are
  self-documenting.
- **Done when:** every remaining `unwrap()`/`expect()` in `fov_stuff.rs` and
  `terminal_rendering` has a stated invariant or is gone.

### 6. Record user input + timing for deterministic crash reproduction
- **Evidence:** input already arrives as `(Instant, Event)` pairs via
  `set_up_input_thread` in `crates/game/src/lib.rs` (~line 51), and
  `Game::new` takes a start `Instant` — so the full input stream is
  capturable at one seam. But there is currently no logging/replay:
  a crash report can't be reproduced from what the user actually did.
- **Plan:**
  1. Log the input stream (event + timestamp offset from game start, plus
     the initial seed/start `Instant` and terminal size) to a rolling file
     (e.g. `~/.local/share/<game>/replays/last_session.input`).
  2. Verify determinism first: audit `Game`/`inputmap.rs` for wall-clock
     reads (`Instant::now()` outside the input seam) and RNG without a
     seeded source; route both through injectable clock/RNG.
  3. Add a headless replay mode (e.g. `--replay <file>` or a test harness
     in `crates/game/tests/`) that feeds recorded events at recorded times
     (or turn indices) and asserts identical final state.
  4. On panic, leave the replay file intact and print its path in the
     crash message.
- **Done when:** crashing a live session, then replaying the recorded
  input file, reproduces the same panic/final state; a regression test
  replays a canned recording.

### 7. Fix error display truncation on crashes
- **Evidence:** the panic hook in `crates/game/src/lib.rs` (~line 44, and
  a copy in `crates/game/src/bin/portal_playground.rs` ~line 47) writes
  `{:?}` of `PanicInfo` straight to stdout after switching to the main
  screen. With no scrollback handling/wrapping, long panic messages and
  backtraces run past the terminal height and the top of the message
  (often the actual error) is lost.
- **Plan:** in the hook, format the message + location + optional
  backtrace, wrap to terminal width, and either print the tail (most
  relevant lines last) or page it; dedupe the hook into one shared
  function used by both `lib.rs` and `portal_playground.rs`. Also write
  the full crash text to a log file and print its path so nothing is
  ever lost to truncation.
- **Done when:** panicking with a multi-screen message leaves the error
  message and location readable on screen (or in a pager), and the full
  text is on disk; covered by a test that panics in a small terminal.

---

## Done

_(nothing yet — move completed items here with a date)_
