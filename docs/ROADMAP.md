# Roadmap — Architecture Improvements

Tracked recommendations from the architecture review (see [ARCHITECTURE.md](ARCHITECTURE.md)).

Check off items in the same commit that completes them, then move them to
**Done** with a date. Keep evidence (file names, LOC counts, test locations)
with each item so the context doesn't have to be re-discovered later.

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


### 8. Migrate animation/graphics API off the world character grid
- **Evidence:** `Animation::glyphs_at_time` returns
  `WorldCharacterSquareGlyphMap` (`crates/game/src/graphics/animations.rs:41`)
  and is implemented in ~12 files under `game/src/graphics/animations/*`;
  `Screen::draw_glyphs` (graphics.rs:146) immediately squashes these to
  per-`WorldSquare` `DoubleGlyph` via `pair_up_character_square_map`.
  Spawned from item 2 Phase 2 (see RISK note there).
- **Plan:**
  1. Change the `Animation` trait to emit `HashMap<WorldSquare, DoubleGlyph>`
     (what `draw_glyphs_at_squares` already consumes); update
     `double_glyphs_at_time/duration` defaults and the ~12 impls.
  2. Migrate sub-square producers (braille/hextant) to bin by world square
     directly (`get_braille_arrays_for_braille_line` already proves the
     pattern — it pairs then converts; skip the pairing).
  3. Once no caller remains, delete the deprecated aliases/fns from
     `screen.rs` (completes item 2 Phase 2) — includes resolving the 25
     definition-site warnings in screen.rs itself.
- **Progress:**
  - SEAM LANDED (2026-08-02): rendering now consumes
    `Animation::double_glyphs_at_time()` and draws it directly via
    `draw_glyphs_at_squares()`, instead of consuming character-grid glyphs and
    pairing them at the render boundary. The double-glyph trait methods now
    expose `HashMap<WorldSquare, DoubleGlyph>` explicitly rather than the
    deprecated `WorldSquareGlyphMap` alias. `StaticBoard` is the first
    implementation migrated to emit double glyphs directly; its legacy
    character-grid method remains as a compatibility adapter until the trait
    is flipped. Added direct output coverage for `StaticBoard`.
    Suite: 471 passed / 11 skipped; workspace deprecation warnings remain 59.
  - NEXT STEP SKETCHED (2026-08-02): flip the trait (make
    `double_glyphs_at_time` required), migrate the 11 remaining impls in two
    groups (3 already world-square producers → rename-only; 8 braille/char-map
    producers → new world-square-binned APIs in braille.rs/hextant_blocks.rs
    with a binning-equivalence property test), delete `draw_string_to_draw_buffer`/
    `draw_glyphs` (no live callers), then delete the deprecated char-grid API
    (closes item 2 Phase 2c). Detailed step plan: `docs/checkpoints/roadmap-8-flip-animation-trait.md`.
- **Done when:** no `WorldCharacterSquare*` types in `game/src`; the
  deprecated items in `screen.rs` are deleted; suite still green.

---

## Done

### 1. Split the `game.rs` god module — 2026-07-30
- **Evidence:** `crates/game/src/game.rs` was ~4,900 LOC with 121 `pub fn`s and ~59 `unwrap()`s.
- **Landed:** extracted `game/blocks.rs`, `game/floating_entities.rs`, `game/ai.rs`,
  `game/spawning.rs`, `game/turns.rs`, `game/tests.rs`, `game/combat.rs`, and
  `game/realtime.rs` in seven compiling steps (checkpoints:
  `docs/checkpoints/roadmap-1.1-extract-blocks.md` through
  `roadmap-1.7-extract-combat-realtime.md`). `mod.rs` is now 1,219 lines —
  primarily the `Game` struct, core accessors, map construction, and rendering
  glue; no non-test module exceeds ~1.5k LOC. Test suite held at 470 passed /
  11 skipped throughout; public `Game` API unchanged.
