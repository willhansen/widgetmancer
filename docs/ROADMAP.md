# Roadmap — Architecture Improvements

Tracked recommendations from the architecture review (see [ARCHITECTURE.md](ARCHITECTURE.md)).

Check off items in the same commit that completes them, then move them to
**Done** with a date. Keep evidence (file names, LOC counts, test locations)
with each item so the context doesn't have to be re-discovered later.

---

## Debug tooling wishlist — rendering & FOV

Captured while designing tooling for the portal-depth partial-visibility
artifact (`issues/black-block-deep-in-portal/PORTAL_FOV_DEPTH_ARTIFACT.md`),
whose write-up names its own blocker: "no supported way to dump FOV internals
for a loaded snapshot." Ordered by leverage, not implementation order. This is
a wishlist; promote entries into numbered `Open` items (or fold into item 6)
when scheduled.

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
  `piece_death_animation.rs:18`), and `static_board::start_time()` returns
  `Instant::now()` per call (`static_board.rs:23`); `--load` rebases to
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

### W.B. "Explain this cell" provenance query — highest leverage
- **What:** given a screen-buffer cell (e.g. the issue's `(49,37)`), return the
  ordered contributions: drawable, absolute world square,
  `PositionedSquareVisibilityInFov`/`portal_depth`, transform/rotation, tint
  alpha (`0.1 * depth`, `fov_stuff.rs:760`), and why that glyph/color won.
- **Anchors:** `SquareVisibility::as_string` (`fov_stuff.rs:131`);
  `PositionedSquareVisibilityInFov` is `pub` + `Debug` (`:215`);
  `screen_text`/`graphics.screen` (`game/snapshot.rs:62`); the issue's observed
  cell table is exactly what this query would produce mechanically.
- **Answers:** "why does it look like that once loaded?" in one query.

### W.C. Portal-space / FOV tracing
- **What:** dump the FOV recursion as a tree — per depth: incoming `view_arc`,
  `transformed_center`, portal transform, `visible_arc_of_portal`, resulting
  visibilities. Plus a "hall of mirrors" unfolded map showing the view arc and
  shadow half-plane per depth, and a depth heatmap.
- **Anchors:** recursion at `fov_stuff.rs:~831`, portal block `:909-993`
  (`transform_arc(view_arc.intersection(portal_view_arc))`, `transformed_center`
  at `:938-943`); no depth cap; `player_field_of_view` private (`game/mod.rs:1371`);
  `load_snapshot_game` `pub(crate)` (`snapshot.rs:547`) — reachable from an
  in-crate `#[cfg(test)]` dump without API changes.
- **Tests the issue's hypothesis:** cumulative arc shrink / missing depth cap at
  depth 3.

### W.D. Differential + invariant oracles
- **What:** a reference FOV (ray-cast in unwrapped portal coordinates) to diff
  against the recursive one, and per-frame debug invariants, e.g. "no
  `OUT_OF_SIGHT`-background partial where a lower depth treats the same absolute
  square as fully visible," plus FOV monotonicity.

### W.E. Automated snapshot minimizer
- **What:** shrink `snapshot/` state (drop entities/blocks, narrow player
  position) while preserving the artifact. Turns the 70×78 map into a minimal
  regression repro.

### W.F. Headless golden/diff harness
- **What:** `--load <dir> --render-headless --diff snapshot/screen.txt` with
  per-cell glyph/fg/bg diffs and `--bless`; promote
  `loaded_snapshot_reproduces_rendered_screen` (`snapshot.rs:771`) into a
  reusable CLI. Headless only; no TTY needed
  (`draw_headless_at_duration_from_start`, `game/mod.rs:490`).

### W.G. Live overlay — nice-to-have
- Toggle FOV arcs, portal-depth heatmap, screen-center markers, draw order, and
  ideal-vs-actual coverage in the running TUI; hover a cell for W.B. Extends the
  existing `floating-square-debug` tool (`floating-square-debug/README.md`).

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

### 2. Remove globally suppressed warnings
- **Evidence:** `#![allow(warnings)]` in `crates/game/src/lib.rs`;
  `#![allow(dead_code)]` + `#![allow(deprecated)]` in `crates/utility/src/lib.rs`.
- **Plan:** remove the blanket allows one crate at a time (`utility` first —
  it has the fewest deps), fixing lints per-category rather than re-adding
  narrower allows. Add `cargo clippy` to the workflow once clean.
- **Progress:**
  - STALE EVIDENCE CORRECTED: `utility`'s crate-root allows are already gone;
    only `#![allow(warnings)]` at `crates/game/src/lib.rs:2` remains.
    Workspace build currently surfaces 164 warnings: ~60 are deprecated
    `screen::WorldCharacterSquare*` / `CharacterGridInWorldFrame` usage
    ("obsolete since screen rotation" migration); the rest are mechanical
    (7 `unused mut`, 5 ambiguous glob re-exports, unused imports/Results,
    elided-lifetime hiding, function-pointer comparisons, useless comparisons).
  - SKETCHED ATTACK PLAN:
    1. Phase 1 (mechanical, allow still in place): `cargo fix --workspace`
       for auto-fixable lints, then hand-fix the ~15 remaining non-deprecation
       warnings.
    2. Phase 2 (deprecation migration, ~60% of warnings): migrate call sites
       off the `screen::` deprecated aliases one alias at a time, in
       dependency order: `WorldCharacterPoint` → `WorldCharacterSquare` →
       `*ToCharMap`/`*GlyphMap` → `CharacterGridInWorldFrame`. When touching
       files with ambiguous glob re-exports, make imports explicit in the
       same commit (overlap with item 4).
    3. Phase 3: remove `#![allow(warnings)]` from `crates/game/src/lib.rs`,
       fix stragglers the allow was hiding from the build output, then add
       `cargo clippy --workspace -- -D warnings` to CI/bacon.
  - RISK: `CharacterGridInWorldFrame` migration (13 struct uses, not just the
    alias) may become a real refactor — if so, split it into its own roadmap
    item rather than letting item 2 balloon.
  - PHASE 1 COMPLETE (mechanical cleanup): `cargo fix --workspace` (also with
    `--tests`), then hand-fixes. All non-deprecation warnings eliminated
    except 2 intentional `private item shadows public glob re-export` at
    `crates/terminal_rendering/src/lib.rs:12,14` — `geometry2::FPoint`/`IPoint`
    are `[f32;2]`/`[i32;2]` arrays deliberately shadowing the euclid-based
    aliases in `pub use utility::*`; untangling is item 4 (NOTE comment left
    in code). Landed changes: removed 5 duplicate item definitions from
    `drawable_glyph.rs` (already in `glyph.rs`: `KNOWN_FG_ONLY_CHARS`,
    `KNOWN_BG_ONLY_CHARS`, `map_of_stringables_to_string`,
    `glyph_map_to_string`, `chars_for_square_walls`), renamed
    `Glyph::default_fg_color/default_bg_color` → `DEFAULT_FG_COLOR`/
    `DEFAULT_BG_COLOR`, dropped `PartialEq` derive on
    portal_playground `WorldState` (fn-pointer fields), `#[cfg(test)]`-gated
    test-only fns/imports, underscore-prefixed intentionally-unused bindings.
    Remaining: 67 deprecation warnings (Phase 2). Test suite: 470 passed /
    11 skipped, unchanged.
  - NEXT: Phase 2 (deprecation migration) in the order sketched above.
  - PHASE 2 SCOPED (next step, evidence gathered):
    - 67 deprecation warnings total, ALL visible ones are inside
      `terminal_rendering` itself: `screen.rs` 25, `glyph.rs` 13,
      `braille.rs` 12, `hextant_blocks.rs` 7, `floating_square.rs` 7,
      `drawable_glyph.rs` 3. By symbol: `WorldCharacterSquare` 20,
      `CharacterGridInWorldFrame` 15, `WorldCharacterSquareToCharMap` 6,
      `WorldCharacterSquareGlyphMap` 6, `WorldCharacterPoint` 6, the 4
      deprecated conversion fns 9, `WorldSquareGlyphMap` 1,
      `glyph::pair_up_character_square_map` 2.
    - `game` crate usage is HIDDEN by `#![allow(warnings)]`: 1
      `WorldCharacterSquare`, 22 other alias uses (Point/Step/Move/maps,
      mostly in `graphics/animations/*`), 9 deprecated fn calls, 0 direct
      `CharacterGridInWorldFrame` uses. Must be migrated before the aliases
      can be deleted, but yields no warnings until Phase 3.
    - REVISED ORDER (cheapest blast radius first):
      1. Step 2a — internal-only symbols: migrate `screen.rs`'s own 25
         uses + the 6 cross-file uses of `WorldSquareGlyphMap` /
         `pair_up_character_square_map` in terminal_rendering. No API
         change, game untouched.
      2. Step 2b — `game` crate call sites (32 uses): migrate the 9
         deprecated fn calls + 23 alias uses to their replacements
         (WorldPoint/WorldSquare-based or local-frame equivalents per
         call site); verify with a temporary `#![deny(deprecated)]`
         patch on `game/src/lib.rs` since the allow suppresses progress
         signal.
      3. Step 2c — `CharacterGridInWorldFrame` (15 uses, incl. 13 struct
         uses in `screen.rs` tests/fns): attempt migration; if it turns
         into a redesign of the screen-rotation frame stack, split into
         roadmap item 8 per the RISK note above.
    - EXIT CHECK for each step: warning count drops by the expected
      amount (`cargo build --workspace 2>&1 | grep -c "use of deprecated"`),
      test suite stays at 470 passed / 11 skipped.
    - REPLACEMENT MAPPING RESOLVED: the three deprecation notes
      ("Obselete/Invalidated since screen rotation", "World does not know
      about glyphs/characters") are one root cause — post-rotation there is
      no meaningful world-frame character grid. Replacements already exist:
      types → local character frame (`LocalCharacterSquare`/`Point`,
      screen.rs:563-566); conversions → `world_point_to_local_character_point`
      et al.; maps → `ScreenBufferGlyphMap` / `DrawableGlyphMap`.
    - DECISION (map migration): option 2 — NO intermediate
      `LocalCharacterSquareGlyphMap` alias. REVISED AFTER IMPLEMENTATION
      START: producers CANNOT emit `ScreenBufferGlyphMap` directly —
      braille/line producers take `WorldPoint` inputs and the world→buffer
      transform (camera origin + `rotation`) lives in `Screen`
      (screen.rs:43-44, `world_square_to_both_screen_buffer_character_squares`).
      Correct end state: world-side producers emit per-`WorldSquare`
      `[Glyph; 2]`/`DoubleChar` (pairing is already the terminal step —
      `Screen::draw_glyphs` immediately squashes char maps via
      `pair_up_character_square_map`, graphics.rs:146-150), and sub-square
      producers (braille) bin dots by world square directly instead of
      going through the world character grid.
    - STEP 2a PARTIAL LANDED: `pair_up_character_square_map` and
      `glyph_map_to_string` made generic over the euclid unit
      (`HashMap<Point2D<i32, U>, _>`); pairing math reimplemented without
      deprecated fns (`world_x = char_x.div_euclid(2)`,
      `index = char_x.rem_euclid(2)` — matches the `char_x = 2*world_x+0.5`
      convention and `round()` behavior of the old path); deprecated
      `#[deprecated]` attr + dead imports removed from glyph.rs.
      Warnings 67 → 59; terminal_rendering tests 139 pass.
    - RISK REALIZED: the remaining glyph.rs/braille.rs producers
      (`get_glyphs_for_colored_braille_line`, `points_to_braille_glyphs`,
      `character_world_pos_to_colored_braille_glyph`, glyph.rs:435-447;
      braille.rs `points_to_braille_chars` binning) are gated on the
      animation-API migration — `glyphs_at_time` returns
      `WorldCharacterSquareGlyphMap` across ~12 files in
      `game/src/graphics/animations/*`. Splitting that out as item 8
      rather than letting item 2 balloon.
  - PHASE 2c COMPLETE via item 8 (2026-08-02): the entire world character
    grid API is deleted; zero `WorldCharacterSquare*` references remain
    anywhere; terminal_rendering builds with zero deprecation warnings
    (lib + tests). Remaining for Phase 3 (game crate, hidden by
    `#![allow(warnings)]`, triaged via `deny(deprecated)` probe):
    `PartialVisibilityDrawable::from_square_visibility` x16 (mostly
    fov_stuff.rs tests), `Graphics::draw_glyphs_for_square_to_draw_buffer`
    x7 (itself `#[deprecated]` with live callers — needs
    un-deprecate-or-rename decision), `Graphics::square_is_white` x5,
    `Glyph::get_glyphs_for_player` x1. Then remove
    `#![allow(warnings)]` and add clippy to CI.
  - PHASE 3 STEP 1 COMPLETE (2026-08-07, trivial group): deleted dead
    `Graphics::off_board_color_at_square` and `checkerboard_square_function`
    (0 callers each); renamed `square_is_white` → `square_is_light` (only
    live caller `radial_shockwave.rs`, which needs the chessboard-light parity
    test); deleted deprecated `Glyph::get_glyphs_for_player` from
    `terminal_rendering` (single caller in `game/tests.rs:1681` now uses
    `ArrowDrawable::new(STEP_UP.into(), THICK_ARROWS, PLAYER_COLOR)`).
    Re-probe with `#![deny(deprecated)]` confirms only the two intended
    items remain in the game crate: `from_square_visibility` x16 and
    `draw_glyphs_for_square_to_draw_buffer` x8. Suite: 459 passed / 11
    ignored, unchanged.
  - PHASE 3 STEP 2 COMPLETE (2026-08-07): resolved the
    un-deprecate-or-rename decision for `draw_glyphs_for_square_to_draw_buffer`
    — deleted it (not renamed) and migrated its 8 callers (all internal to
    `graphics.rs` incl. 2 in `#[cfg(test)]`) to
    `draw_drawable_to_draw_buffer(square, &TextDrawable::from_glyphs(glyphs))`,
    which is byte-for-byte what the deprecated body did. Re-probe now shows a
    single remaining deprecation: `from_square_visibility` x16. Suite:
    459 passed / 11 ignored.
  - PHASE 3 STEP 3 COMPLETE (2026-08-07): migrated all 17
    `from_square_visibility` uses (all `#[cfg(test)]` — 15 in fov_stuff tests,
    1 in drawable.rs test, + the def) to
    `from_partially_visible_drawable(&SolidColorDrawable::new(GREEN), viz)`,
    preserving the old hard-coded `GREEN` fg. Deprecated fn deleted.
    `#![deny(deprecated)]` probe: 0 remaining. ALL deprecated symbols in the
    game crate are now resolved — next: remove `#![allow(warnings)]` from
    `game/src/lib.rs`, fix stragglers, add clippy to CI.
    Suite: 459 passed / 11 ignored.
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

### 9. Fix floating-square silhouette tearing
- **Evidence:** at pos=(2.363, -0.816) the floating square renders as a
  ragged blob. `character_for_half_square_with_2d_offset`
  (`crates/terminal_rendering/src/floating_square.rs:168`) snaps each
  half-cell independently across four glyph families (h-eighths, v-eighths,
  hextants, quadrants); sibling cells of one square drop different axes, so
  the edges land at different positions per column. Failing visual test:
  `crates/terminal_rendering/tests/floating_square_coherence.rs` (5/9
  sampled positions along a motion line fail edge coherence).
- **Plan:** pick the glyph family once per square (score families against
  the center offset, y weighted ~2x for cell aspect), then snap all
  half-cells within that family. Also fix the debug tool's misleading
  `branch=` label (it describes the unused `get_chars_for_floating_square`
  path, not the render path).
- **Done when:** `test_square_silhouette_stays_rectangular_along_motion_line`
  passes and the debug tool no longer attributes renders to dead code.

## Done

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
