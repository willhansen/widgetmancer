# Roadmap — Architecture Improvements

Tracked recommendations from the architecture review (see [ARCHITECTURE.md](ARCHITECTURE.md)).

Check off items in the same commit that completes them, then move them to
**Done** with a date. Keep evidence (file names, LOC counts, test locations)
with each item so the context doesn't have to be re-discovered later.

---

## Deferred: voxel-world vertical gameplay (from the `cube_iso` port)

The z/altitude rendering port (`docs/CUBE_ISO_PORT.md`) made terrain a voxel
grid and let the player walk its top surfaces, but deliberately stopped short of
vertical physics. Open follow-ups:

- **Gravity / falling.** Walking off an edge currently does nothing: void and
  step-ups are blocked, and the player's altitude is derived from the column it
  stands on (`Game::player_altitude`), so it can only move level or *down*.
  The demo (`cube_iso/src/physics.rs`, deleted) dropped `z` one step at a time
  to the support below, marked `lost` past `FALL_LIMIT`, and respawned.
- **Fall trail.** The demo drew the path of the fall; the game has no such
  overlay (its `Fall trail` inventory row is `deferred`).
- **Step-down animation.** Descents (e.g. the `space-cubes` staircase) jump the
  player glyph down instantly rather than sliding.
- **Smooth / grid-realtime / move-gated modes.** The demo's three physics
  schemes are not ported.
- **Player altitude as state.** Today it is derived from position; real falling
  needs a stored `z`, `vz`, and tick integration.

Sight and blocking are already altitude-aware (`Game::fov_blockers`): a column
blocks only when its top rises above the player, which is what lets the player
see across a cube top it is standing on.

---

## Debug tooling wishlist — rendering & FOV

Captured while designing tooling for the portal-depth partial-visibility
artifact (`issues/solved/black-block-deep-in-portal/PORTAL_FOV_DEPTH_ARTIFACT.md`),
whose write-up names its own blocker: "no supported way to dump FOV internals
for a loaded snapshot." Ordered by leverage, not implementation order. This is
a wishlist; promote entries into numbered `Open` items (or fold into item 6)
when scheduled.

Status (2026-10-20): **W.B, W.C, W.D, W.E and W.F have landed** (each entry
keeps its own landed notes below). Still open: **W.A** (virtual clock /
deterministic replay) and **W.G** (live overlay). Item 16 (animation capture)
is the next tooling item and reuses the W.F-style capture seam.

Motivating failure mode: not a crash, but an emergent property of
(1) portal-recursive FOV with accumulated frame transforms/view arcs
(`crates/game/src/fov_stuff.rs`), (2) a layered draw buffer composited through
visibility/tint (`graphics.rs`, `graphics/drawable.rs`), (3) floating-point
positions and unbiased glyph picks (`crates/terminal_rendering`), and
(4) ambient nondeterminism. So the ideal tooling is introspective,
deterministic, and provenance-oriented — not a step debugger (none is
installed anyway: no gdb/lldb/rr/perf/valgrind).

### W.A. Virtual clock + deterministic replay
- **What:** a single injectable logical clock for sim + render, plus a recorded
  tick schedule. `Game::draw(time)` already threads a frame time, but it is not
  the only clock a frame depends on.
- **Leaks (evidence):** sim time = wall time: loop measures real deltas
  (`lib.rs:140-145,165`) → `tick_realtime_effects(delta)` → `world_time += delta`
  (`game/realtime.rs:24`); `Game::new` ignores its `start_time` for the world
  clock (`game/mod.rs:139-140` vs `:122`); render reads wall clock —
  `draw_death_cube` calls `technicolor_at_time(Instant::now())`
  (`graphics.rs:399`, defeating the `time` argument at `:435`); animations
  anchor `start_time: Instant::now()` at construction (~9 files,
  `piece_death_animation.rs:18`), and `static_floor::start_time()` returns
  `Instant::now()` per call (`static_floor.rs:23`); `--load` rebases to
  `Instant::now()` (`game/snapshot.rs:553`).
- **Ideal:** `LogicalTime(Duration)` newtype (serializable/constructible, unlike
  `std::time::Instant`) for `world_time`, `Graphics::start_time`, and animation
  start times; one `now` per tick feeding both sim and draw; wall clock read
  only at the driver seam in `lib.rs` and the input thread. Snapshot serializes
  absolute logical time so load restores the clock.
- **Replay promise:** `(game_state.json + input_history.json + tick schedule)`
  → byte-identical ANSI frame at any index.
- **Overlap:** item 6 step 2 already calls for auditing wall-clock reads; W.A is
  that audit generalized to the render path and made load-safe.
- **Landed (2026-09):** `LogicalTime(Duration)` newtype (`crates/game/src/logical_time.rs`)
  replaces `std::time::Instant` across `Game`/`Graphics`/all 12 animations;
  `Game::draw` stamps spawned animations with the frame's logical time via
  `Graphics::set_current_time`, and the driver reads the wall clock once
  (`lib.rs` epoch) to derive each tick. `Game::new` seeds the world clock from
  its `start_time`; `draw_death_cube` takes the frame time; snapshot load no
  longer rebases to `Instant::now()`. Gameplay randomness is a seeded
  `ChaCha8Rng` on `Game` (turret fire, shotgun spread, random subordinate
  spawns) whose state is serialized into `game_state.json` (`rng_state`), so a
  loaded game resumes the exact stream. A test asserts no `Instant::now()`
  survives outside the driver seam. Full suite green (235 lib tests).
  The pre-fix checked-in snapshot still differs on 30 death-cube cells (the old
  wall-clock skew); new captures are self-consistent.

### W.B. "Explain this cell" provenance query — LANDED (was highest leverage)
- **What:** given a screen-buffer cell (e.g. the issue's `(49,37)`), return the
  ordered contributions: drawable, absolute world square,
  `PositionedSquareVisibilityInFov`/`portal_depth`, transform/rotation, tint
  alpha (`0.1 * depth`, `fov_stuff.rs:849`), and why that glyph/color won.
- **Anchors:** `SquareVisibility::as_string` (`fov_stuff.rs:131`);
  `PositionedSquareVisibilityInFov` is `pub` + `Debug` (`:215`);
  `screen_text`/`graphics.screen` (`game/snapshot.rs:62`); the issue's observed
  cell table is exactly what this query would produce mechanically.
- **Answers:** "why does it look like that once loaded?" in one query.
- **Landed (2026-09):** `Game::explain_screen_cell(square_x, y)` (debug-tools)
  reports the final glyph/colors, the world square and FOV-relative square, and
  every `PositionedSquareVisibilityInFov` contributing (absolute square, portal
  depth, rotation, absolute/relative `as_string`, draw-buffer drawable).
  `snapshot_tool explain <dir> X Y` exposes it. On the black-block snapshot,
  `explain <dir> 49 37` reproduces the issue's table exactly: depth 3,
  abs(37,45), abs_vis `🬭🬭`, final fg(165,89,89) bg(77,0,0), draw-buffer floor
  GREY. Test added.

### W.C. Portal-space / FOV tracing — LANDED
- **What:** dump the FOV recursion as a tree — per depth: incoming `view_arc`,
  `transformed_center`, portal transform, `visible_arc_of_portal`, resulting
  visibilities. Plus a "hall of mirrors" unfolded map showing the view arc and
  shadow half-plane per depth, and a depth heatmap.
- **Anchors:** recursion at `fov_stuff.rs:1109`, portal block `:1195-1285`
  (`transform_arc(view_arc.intersection(portal_view_arc))` at `:1229-1231`,
  `transformed_center` at `:1224`); depth is bounded by `radius + 1` (see
  `docs/PERFORMANCE.md` "Depth bound"); `player_field_of_view` private
  (`game/mod.rs:1428`); `load_snapshot_game` `pub(crate)` (`snapshot.rs:547`) —
  reachable from an in-crate `#[cfg(test)]` dump without API changes.
- **Tests the issue's hypothesis:** the depth-3 black-block artifact (since
  fixed by arc unioning); depth itself turned out to be bounded, so the
  frame-time concern is branching, not missing-depth-cap.
- **Landed (2026-09):** `FovTrace`/`FovTraceNode` (`fov_stuff.rs`) with
  `single_octant_field_of_view_traced`, `portal_aware_field_of_view_from_square_traced`,
  and `Game::player_field_of_view_traced` (debug-tools). Threads `depth` +
  `Option<&mut FovTrace>` through the recursion; records one node per portal
  crossing (child depth, incoming transformed arc, parent-frame portal arc,
  transformed center, rotation). `snapshot_tool fov-trace[-json] <dir>` prints
  it; `to_tree_string`/`to_json` on `FovTrace`. Trace test added. Against the
  black-block snapshot it shows the depth-3 arcs narrowing and splitting
  (`depth1 [0..6.340] → depth2 [0..3.013] & [1.975..3.013]`), direct evidence
  for the arc-accumulation hypothesis.

### W.D. Differential + invariant oracles — LANDED
- **What:** a reference FOV (ray-cast in unwrapped portal coordinates) to diff
  against the recursive one, and per-frame debug invariants, e.g. "no
  `OUT_OF_SIGHT`-background partial where a lower depth treats the same absolute
  square as fully visible," plus FOV monotonicity.
- **Landed (invariant half, 2026-09):** `fov_visibility_consistency_violations`
  (`fov_stuff.rs`) groups every relative square's visibilities by absolute
  square and flags any square that is fully visible via one portal
  depth/relative square but partially visible via another — exactly the
  black-block signature. `Game::fov_invariant_violations` +
  `snapshot_tool invariants <dir>` expose it; against the black-block snapshot
  it reports 115 violations including the known `rel(15,1) abs(37,45)`. Test:
  invariant holds on a portal-free FOV. `visibility-pairs` (same year) adds the
  per-cell complementarity check; `verify-issues` is the two-sided fixture
  oracle for solved captures. **Remaining:** the ray-cast reference oracle
  (differential) and reducing false positives by only flagging partials whose
  underlying drawable is plain floor at a deeper depth than a full view.

### W.E. Automated snapshot minimizer — LANDED
- **What:** shrink `snapshot/` state (drop entities/blocks, narrow player
  position) while preserving the artifact. Turns the 70×78 map into a minimal
  regression repro.
- **Landed (2026-09):** `debug::minimize_snapshot` greedily removes entity
  collection entries (pieces, blocks, upgrades, belts, arrows, widgets,
  incubating pawns, death cubes, drones) and portal entries while the
  `OUT_OF_SIGHT`-tinted partial persists; the player is preserved. The
  artifact is anchored to the player (relative square + portal depth + absolute
  square) so it survives a resize. An **early virtual-screen crop** shrinks
  `screen.terminal_width/height` to the `{player, artifact}` bounding box plus
  margin (default 4), touching nothing else; rejected if the artifact would
  leave the buffer. `snapshot_tool minimize <dir> X Y [out]` with
  `--review[=<path>]` (screen-by-screen transcript: frame → one-line change →
  next frame), `--review-explain`, `--review-plain`, `--crop-margin`,
  `--no-screen-crop`, `--keep-portals`. Portals are minimized too by default
  (the strict anchor predicate keeps the artifact's chain); `--keep-portals`
  opts out. On the black-block snapshot: 141×78 → **73×11**, then all entities
  and 19 of 21 portals removed → **player + 2 portals**, artifact intact.
  Tests: crop geometry, anchor-derivation error, review-step formatting,
  no-artifact error. Used 2026-10-20 to reduce issues/0020's capture to the
  0029 repro (141x78 -> 17x12, 50 renders).
  **Remaining:** extent/coordinate shrink (deliberately deferred — translation
  is near a visual no-op, floor pattern is mod-6, and it risks `place_piece`
  panics), and a chunked (delta-debugging) search for large maps.

### W.F. Headless golden/diff harness — LANDED
- **What:** `--load <dir> --render-headless --diff snapshot/screen.txt` with
  per-cell glyph/fg/bg diffs and `--bless`; promote
  `loaded_snapshot_reproduces_rendered_screen` (`snapshot.rs:771`) into a
  reusable CLI. Headless only; no TTY needed
  (`draw_headless_at_duration_from_start`, `game/mod.rs:490`).
- **Landed (2026-09):** `game` gains a default-off `debug-tools` feature and
  `pub mod game::snapshot::debug` (descendant module, so it reaches the private
  DTO/loader without widening the crate's API). New bin
  `crates/game/src/bin/snapshot_tool.rs`:
  `render <dir>` / `diff <dir> [ref]` / `bless <dir>` / `cells <dir>`, with an
  ANSI `screen_text` parser and per-cell diff report. Manifest switches to
  explicit `[[bin]]` entries + `autobins = false`; `snapshot_tool` has
  `required-features = ["debug-tools"]`. Tests: parser round-trip against the
  screen buffer; a cross-load render determinism guard. The repo snapshot now
  matches the headless render except the retired wall-clock skew (W.A).
  Extended since with `bless`, `explain`, `screen-explain`, `explain-diff`,
  `invariants`, `visibility-pairs`, `render` toggles, `render-at`, `simulate`,
  `verify-issues` and `minimize`.
  **Gate:** run
  `cargo run -p game --features debug-tools --bin snapshot_tool -- diff snapshot/`.

### W.G. Live overlay — nice-to-have
- Toggle FOV arcs, portal-depth heatmap, screen-center markers, draw order, and
  ideal-vs-actual coverage in the running TUI; hover a cell for W.B. Extends the
  existing `floating-square-debug` tool (`floating-square-debug/README.md`).
- **Landed (minimal, 2026-09):** `Graphics.debug_overlay: DebugOverlayFlags`
  (`screen_center`, `screen_origin`) applied by `draw_debug_overlays` after the
  FOV composite; `Game::set_debug_overlay` toggles it. Test covers the center
  marker. **Remaining:** keybindings/`InputMap` toggles, depth heatmap,
  draw-order readout, and mouse-hover → `explain_screen_cell`.

### Mapping to `black-block-deep-in-portal`
| Item | Role |
|---|---|
| W.A | prove load == capture; rule out nondeterminism before blaming FOV |
| W.B + W.C | test the no-depth-cap / arc-accumulation hypothesis |
| W.D | encode the fix as an invariant |
| W.E | minimal regression state |
| W.F | lock it in as a golden test |

---

## Open

### 15. Complementary direct/portal visibilities draw only one glyph
- **Evidence:** `./snapshot-tool visibility-pairs` shows that at every relative
  cell with more than one visibility the direct and portal half-planes are
  **exact complements** (0020 115/116, 0022 121/121, 0024 112/112,
  0025 112/112, 0026 172/172). The FOV model is correct; the renderer is not.
  `drawable_at_relative_square` composites the stack with
  `PartialVisibilityDrawable::drawn_over`, which paints the deeper (portal)
  glyph and reduces the shallower (direct) half to a flat colour
  (`color_if_backgroundified`). A terminal cell carries a single glyph, so the
  direct cube's top/edge cannot be shown alongside the portal content — "can't
  see the cube column" (`issues/0029`, class A; minimized repro in
  `issues/0029/snapshot`, original reports 0020/0025).
- **Scope:** sub-cell compositing. Options: per-character compositing of the two
  visibilities (each clipped to its half-plane) rather than one reduced glyph; or
  a model where a complementary pair that covers the whole cell is represented
  once with both contents. Neither is a `combined_*` merge problem (the merge is
  never applied across depths).
- **Related:** item 14 (the opaque shadow half); issue 0029 (class A).

### 17. Portal wall projection vs height occlusion
- **Evidence:** the raised-terrain forward pass (`graphics.rs`,
  `load_screen_buffer_from_terrain`) gates each column's z-shifted wall by the
  *view frame of the destination cell* (deepest visibility first, then the
  suppression-only void-view rule added for 0020/0026). Two different absolute
  squares in one frame can share a screen ray, so the gate fails in both
  directions:
  - **Missing side** (`issues/0029` class B, reports 0022/0024): the direct
    column's wall is dropped where the cell resolves to another frame or to
    void; the reporter's expected cube side is simply not drawn.
  - **Extra side** (`issues/0027`): a depth-1 portal column's wall is painted
    onto a cell whose FOV resolves to void (an on-board gap).
- **Why the cheap fixes are out:** flipping draw order breaks all 18 portal
  fixtures; blanket-suppressing walls that land on void cells drops 646
  legitimate cliff cells in `solved/0009`; the 2026-10-12 experiment that
  relaxed the frame gate added walls (0007 +37, 0008 +60, 0012 +49, 0017 +78,
  0009 +19 cells). The current gate is a frame heuristic, not an occlusion
  test.
- **Plan:** decide each wall voxel from its own geometry — is any part of that
  voxel visible along the screen ray that reaches the cell (depth order, portal
  frames, and the column height above the camera)? Then the gate becomes "draw
  the wall where the wall is visible" and both polarities follow. Encode the
  invariant as a test: *no side glyph in a cell the FOV resolves to void unless
  a wall voxel is visible on that ray*.
- **Fixtures:** 0029 minimized repro (class A/B), 0027 box, `solved/0009` cliff
  sides as the anti-regression, plus the re-blessed 0007/0008/0010-0017 set.
- **Related:** item 15 (class A shares the same cells); issues 0022, 0024,
  0027, 0029.

### 16. Capture and review animation/video issues
- **Evidence:** `issues/0023` — conveyor belts seen through a portal as partial
  tiles look static, because a partial tile is one glyph in one `fg_color`
  (`PartialVisibilityDrawable::to_glyphs`), so the belt's two-colour animation
  phase is discarded. A single static "after" picture cannot show it, and the
  reviewer noted (2026-10-16) "can't be fairly represented with just one
  picture". The reporter's suggestion (average the tile's colours) would still
  convey no motion; motion needs per-character compositing (item 15) *and* a
  review medium that shows time.
- **Scope (capture):**
  - Keep a rolling ring buffer of the last ~2-3 s of frames in the render loop
    (~47 fps), not a full recording — a filed issue carries at most that window.
  - Write it beside the snapshot as compact per-frame deltas (changed cells
    only). A raw ANSI frame is ~0.5 MB, so a naive recording is useless; the
    target is tens of KB per capture, enforced by a stated size bound.
  - Format lives in `snapshot/frames.*` and is written by
    `game::snapshot` (same seam as `screen.txt`); buffer capture point is the
    draw in `crates/game/src/lib.rs`, where the report overlay already freezes
    the frame.
- **Scope (review):** `issue_review` gains playback. `IssueView.before` is
  already a `Vec<Vec<Glyph>>` drawn by a generic `put_pane`, so the work is an
  extra pane, a frame clock/step tick, and a frameable before-source
  (`cells_to_buffer` today takes the single `screen.txt`). Keys: play/pause,
  step, scrub. First cut plays captured **before** frames only — it must not
  block on W.A/item 6 (deterministic replay for an animated "after" is the
  follow-up).
- **Acceptance:** filing an animation issue writes a playable sequence;
  `verify-issues` is unaffected (no new blessed fixture format); the capture
  size bound is enforced and tested; `issue_review` playback has a pty test
  like the note editor.
- **Related:** 0023, `docs/vision/ideas.md` ("Issue review: animation"), item 6
  (W.A), item 15.

### 14. Partial shadows over void suppress the starfield
- **Evidence:** `issues/solved/0017/` (and its `screen.pre-fix.txt`): a portal
  view of a raised cube partially covers an off-board void square. The
  `PartialVisibilityDrawable` shadow half carries an opaque `OUT_OF_SIGHT`
  background, and the cell is in the FOV pass's `drawn` set, so
  `Starfield::draw` skips it — the void that should show stars renders black,
  and stepping flips the cell to a direct/void resolution ("more outer space
  abruptly visible").
- **Scope:** sub-cell. The shadow half of a partial whose underlying square is
  off-board void should let the starfield through (transparent shadow, or
  exclude such cells from `drawn` and let the starfield paint the visible void
  portion). The material-base fix (`draw_surface_base`) resolved the
  floor-colored visible half; this is the remaining half.
- **Related:** roadmap W.D (portal-depth partial-visibility artifact).

### 3. Resolve ignored tests on core mechanics
- **Evidence (2026-10-20):** 9 `#[ignore]`d tests across the tree:
  `crates/game/src/game/tests.rs:1480`, `:1575`, `:2611` (`"TODO"`), `:1367`
  ("May or may not want this"); `crates/game/src/fov_stuff.rs:3038` ("not a
  priority for the time being"); `crates/terminal_rendering/src/frame.rs:588`;
  `crates/utility/src/angle_interval.rs:740` ("moving away from angle sets");
  `crates/game/src/graphics/animations.rs:174` (visual debugging);
  `crates/game/src/bin/portal_playground.rs:1592`. Open correctness TODOs
  remain in `fov_stuff.rs` and `portal_geometry.rs` (second-portal handling at
  line ~246).
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
- **Evidence (re-checked 2026-10-20):** `use utility::*` in
  `game/src/{lib,piece}.rs`, `game/src/game/{floor_features,floating_entities}.rs`,
  `game/src/graphics/animations.rs` (plus a test module in `graphics.rs`), and
  `terminal_rendering/src/{braille,glyph,floating_square,drawable_glyph}.rs`.
  `use terminal_rendering::*` in `game/src/{fov_stuff,review_ui,report,graphics}.rs`,
  `game/src/game/mod.rs`,
  `game/src/graphics/{animations,drawable,fov_border,starfield}.rs`, and
  `game/src/bin/portal_playground.rs`. `blocks.rs` no longer globs. The
  `terminal_rendering` re-export of `utility::*`
  (`crates/terminal_rendering/src/lib.rs:46`) still blurs the layering.
- **Plan:** switch to explicit imports, then remove the `pub use utility::*`
  re-export from `terminal_rendering` so `game` depends on `utility` directly
  for utility types.
- **Done when:** no `use utility::*` or `use terminal_rendering::*` globs
  remain outside test code; layering is visible from imports alone.

### 5. Harden hot paths against panics
- **Evidence (re-checked 2026-10-20):** several hundred `unwrap()` calls
  across the crates' source (311 including test code), concentrated in
  `fov_stuff.rs` (45) and `terminal_rendering/src/drawable_glyph.rs` (28).
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
- **Evidence (re-checked 2026-10-20):** two panic hooks still exist — the one
  in `crates/game/src/lib.rs:58` (writes `{:?}` of the panic info) and a
  second in `crates/game/src/bin/portal_playground.rs:41` (a `{:#?}` copy with
  its own `\n`→`\n\r` fixup). Both write the panic straight to stdout after
  switching to the main screen, with no scrollback handling or wrapping, so
  long panic messages and backtraces run past the terminal height and the top
  of the message (often the actual error) is lost.
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

### 9. Fix floating-square silhouette tearing — 2026-10-20
- **Was:** at pos=(2.363, -0.816) the silhouette tore because each half-cell
  snapped across four glyph families independently.
- **Now:** the coherence suite passes, including the acceptance test
  `test_square_silhouette_stays_rectangular_along_motion_line`
  (`crates/terminal_rendering/tests/floating_square_coherence.rs`), and no
  `branch=` label remains in the tree (it never existed under that name in the
  current sources).

### 2. Remove globally suppressed warnings — 2026-10-20
- **Was:** `#![allow(warnings)]` in `crates/game/src/lib.rs` masked every lint;
  `utility`'s crate-root allows were already gone.
- **Now:** the blanket allow is gone; the only crate-level allow in `game` is
  `#![allow(non_snake_case)]`. A plain `cargo check -p game` reports 13
  warnings (unused imports/dependencies, dead code, a privacy lint, a
  `private item shadows public glob re-export` pair), so lints are visible
  again and can be fixed per category.
- **Remaining:** clearing those 13 warnings and adding `cargo clippy` to the
  workflow. No open item currently owns this; file one if the count grows.

### 13. Fix black diagonal partial-square seam through adjacent portals — 2026-09
- **Evidence:** `issues/solved/black-diagonal-portal-seam/` — standing on one of a
  stacked pair of east-facing portals rendered a diagonal of
  `OUT_OF_SIGHT`-background partials on the 45-degree seam between their
  openings (`rel(k,-k)`, `k=1..5` in the capture; longer in the minimized repro).
- **Root cause:** the item-12 merge carried a single `view_arc`. When same-root
  portal slices did not touch, `combined_with_unioning_arcs` kept one operand's
  arc but the other's squares; a later touching slice unioned only with the
  retained arc, so the dropped slice's squares were recomputed under an arc
  that no longer covered them. `combined_sub_fovs` reduces a `HashMap` group, so
  the loss was order-dependent.
- **Landed:** `FieldOfViewResult` carries `view_arcs: Vec<AngleInterval>`;
  `combined_main_view_only` merges touching/overlapping fragments
  (`merge_contiguous_arc_intervals`) and recomputes squares under the merged set
  (`visibility_of_square_under_arc_intervals`), leaving blocker-separated arcs
  distinct. Separate follow-up: the minimizer's virtual-screen crop returned an
  always-odd width, so the last (half-visible) column panicked when drawn; it
  now returns an even width and the renderer clips off-screen character columns
  instead of panicking (`Screen::draw_glyph_straight_to_screen_buffer`,
  `draw_string_to_screen`). Tests `test_stacked_portal_slices_union_to_full_visibility`,
  `stacked_portal_seam_has_no_out_of_sight_partial`,
  `drawing_an_odd_width_edge_square_clips_instead_of_panicking`; full suite
  green (536 passed / 9 skipped; 275 with `debug-tools`).

### 12. Fix portal-depth partial-visibility artifact — 2026-09
- **Evidence:** `issues/solved/black-block-deep-in-portal/` — a black
  `OUT_OF_SIGHT`-background partial at buffer `(49,37)` (rel `(15,1)`, depth 3)
  where depths 0–2 see the same absolute square `(37,45)` fully.
- **Root cause:** sub-FOVs reaching the same transformed root through adjacent
  portal-face slices were merged by combining per-square half-planes
  (`combined_increasing_visibility`), which can only express exact complements;
  the union of two non-complementary partials was lost, leaving a shadow that
  rendered as opaque `OUT_OF_SIGHT` black.
- **Landed:** `FieldOfViewResult` carries its `view_arc`; `combined_sub_fovs`
  merges same-root results with `combined_with_unioning_arcs`, which unions the
  cones and recomputes affected squares under the union. Top-level octant folds
  and blocker splits keep their arcs. Regression test
  `test_portal_slice_arcs_union_to_full_visibility`; full workspace suite green.
  Debug outputs regenerated under `issues/solved/black-block-deep-in-portal/debug/`
  (pre-fix minimization kept in `debug/pre-fix/`).

### 11. Floating squares travel through portals — 2026-09-23
- **Evidence:** `slide_floating_entity_with_portal_awareness` was a
  `// TODO: portal awareness` stub (realtime.rs); `tick_death_cubes`
  integrated naively and killed along the straight naive segment; two
  `#[ignore = "TODO"]` tests in game/tests.rs defined the wanted behavior
  (drone moves through portal; straddling drone pokes through visually).
- **Landed:** (1) `PortalGeometry::portal_aware_move` — continuous
  segment-based portal traversal for movers (reuses
  `first_inside_square_face_hit_by_ray` + `RigidTransform::transform_ray`,
  exact crossings without the ray draw-back epsilon, 16-crossing guard
  cap), returning end position, accumulated rotation, and traveled
  sub-paths; (2) the slide funnel applies it (position + velocity
  rotation), so drones, conveyor pushes, and floor-arrow pushes inherit
  portal awareness; (3) `tick_death_cubes` routes through the funnel and
  kills along each sub-path (stationary cubes still kill their own square);
  (4) render-side straddle remap: the beyond-face cell of a straddling
  square is moved through the entrance's portal transform
  (`remap_floating_square_drawables_through_portals`, drawable.rs) —
  an entrance-only rule that covers one-way, two-way, and double-sided
  portals with no inverse transforms or transit tracking. Both TODO tests
  un-ignored and passing. Suite: 509 passed / 9 skipped.

### 10. Improve floating-square rendering quality — 2026-08
- **Evidence:** post-item-9 evaluation found (a) a 0.25×1/6 silhouette notch
  from one wrong hand-written entry in `hextant_block_by_offset`
  (`(-1,-1) => '▖'` should be `🬓`), (b) family selection optimized a
  center-offset proxy rather than measured coverage error, (c) the
  motion-line test's trajectory missed whole offset regions, (d) family
  switches caused visible pops during motion.
- **Landed:** (1) hextant/quadrant glyph tables generated from
  square-overlap geometry, with table≡geometry probe tests; (2) family
  selection is a baked 24×24 map over the [0, 0.5)² fundamental domain
  scored by measured coverage error (`family_map.rs` + generated
  `family_map_table.rs`, FAMILY_MAP_BLESS regeneration + sparse live
  re-validation); (3) dense-sweep silhouette test over the offset plane at
  1/24 density (caught two real bugs: non-translation-invariant
  `f32::round` snapping, and float-ULP tie-break tearing at family
  decision boundaries) + `Metrics` surfaced in the debug tool; (5)
  family-switch hysteresis: `characters_for_full_square_with_2d_offset_biased`
  with `FAMILY_SWITCH_PENALTY`, renderer-owned memory keyed by
  `FloatingEntityId` (entities are pure model; `Graphics` caches and
  sweeps per frame), one biased pick per frame forced on all 9 cells
  (portal rotations re-derive — families aren't rotation-invariant). Hysteresis takes boundary flicker from 39 switches
  to 0 with silhouette metrics green. Suite: 469 passed / 0 failed.

### 8. Migrate animation/graphics API off the world character grid — 2026-08-02
- **Evidence:** `Animation::glyphs_at_time` returned `WorldCharacterSquareGlyphMap`
  across 12 impls in `game/src/graphics/animations/*`, immediately squashed to
  per-`WorldSquare` `DoubleGlyph` at the render boundary. Spawned from item 2
  Phase 2c.
- **Landed:** the `Animation` trait now has a single required glyph method,
  `double_glyphs_at_time -> HashMap<WorldSquare, DoubleGlyph>`; all 12 impls
  migrated (3 rename-only; 9 via new world-square-binned producers:
  `points_to_braille_double_arrays`, `Glyph::points_to_braille_double_glyphs`,
  `Glyph::double_glyphs_for_colored_braille_line`,
  `points_to_hextant_double_glyphs`). The entire world character grid API is
  deleted from `terminal_rendering` (`WorldCharacterSquare/Point/Step/Move`,
  `CharacterGridInWorldFrame`, the map aliases, 7 conversion fns,
  `pair_up_character_square_map`, `get_chars_for_braille_line`,
  `points_to_braille_chars`, `points_to_hextant_chars`,
  `character_map_for_full_square_at_point`, and the DrawableGlyph char-grid
  variants); dead `Graphics::draw_glyphs`/`draw_string_to_draw_buffer`
  deleted. Binning-equivalence property tests guarded the migration and
  caught two real bugs: euclid's `Point2D::round` is `(x+0.5).floor()` (not
  `f32::round`), and a pre-existing vertical mirroring of braille dots at
  negative y (`(pos.y % 4).abs()` → `rem_euclid` in
  `braille_square_to_dot_in_character`, behavior fix on old path until its
  deletion). Property tests retired with the old path; converted golden
  tests retain coverage. Workspace deprecation warnings: 59 → 0 (only 2
  intentional glob-shadowing warnings remain, item 4). Suite: 459 passed /
  11 skipped (14 tests of deleted APIs removed/converted).

### 1. Split the `game.rs` god module — 2026-07-30
- **Evidence:** `crates/game/src/game.rs` was ~4,900 LOC with 121 `pub fn`s and ~59 `unwrap()`s.
- **Landed:** extracted `game/blocks.rs`, `game/floating_entities.rs`, `game/ai.rs`,
  `game/spawning.rs`, `game/turns.rs`, `game/tests.rs`, `game/combat.rs`, and
  `game/realtime.rs` in seven compiling steps. `mod.rs` is now 1,219 lines —
  primarily the `Game` struct, core accessors, map construction, and rendering
  glue; no non-test module exceeds ~1.5k LOC. Test suite held at 470 passed /
  11 skipped throughout; public `Game` API unchanged.
