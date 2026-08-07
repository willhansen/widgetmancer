# Checkpoint — Roadmap 8: Flip the Animation trait & finish the world-square migration

Working state for [ROADMAP.md](../ROADMAP.md) item 8 (and the tail of item 2
Phase 2c), in case of interruption. **Not yet committed to ROADMAP.md** —
update it when the item completes.

## Where we are (verified 2026-08-02)

- Production rendering is fully on the new seam: `graphics.rs:584,601,604`
  call `animation.double_glyphs_at_time()`. **Zero production callers** of
  the legacy `glyphs_at_time` outside the trait itself (only the trait
  defaults at animations.rs:44,48 and 4 test call sites at
  animations.rs:143,176,190,198).
- `StaticBoard` is the migrated template: implements `double_glyphs_at_time`
  directly; `glyphs_at_time` is a converting adapter back to the char grid.
- The trait still requires `glyphs_at_time` (animations.rs:41) and *defaults*
  `double_glyphs_at_time` via `pair_up_character_square_map` — i.e. the
  seam landed but the direction is not yet flipped.
- 59 deprecation warnings remain, **all inside `terminal_rendering`**:
  screen.rs 25, braille.rs 10, hextant_blocks.rs 7, glyph.rs 7,
  floating_square.rs 7, drawable_glyph.rs 3. The `game` crate's uses are
  hidden by `#![allow(warnings)]` at `crates/game/src/lib.rs:2`.
- `WorldSquareGlyphMap = HashMap<WorldSquare, DoubleGlyph>` (screen.rs:451)
  is the **new** type — not deprecated. Everything named
  `*WorldCharacterSquare*` / `CharacterGridInWorldFrame` / the
  `world_*character*` conversion fns is the deprecated dead-end.

## Progress log

- GROUP A LANDED (2026-08-02): `radial_shockwave`, `recoiling_board`,
  `smite_from_above` now implement `double_glyphs_at_time` directly — the
  dead round-trip through `world_square_glyph_map_to_world_character_glyph_map`
  is gone from all three. **Sequencing deviation from the plan below**: 8a
  (trait flip) moved to LAST, because flipping first leaves the 8 unmigrated
  impls not implementing the required method → tree doesn't compile between
  steps. Instead, the trait now has a transition bridge: `glyphs_at_time`
  gained a default (un-pair via
  `world_square_glyph_map_to_world_character_glyph_map`) mirroring the
  existing `double_glyphs_at_time` default — each defaults in terms of the
  other, every impl overrides ≥1 (NOTE comment on the bridge in
  animations.rs). Flip becomes a ~10-line deletion once all 12 impls emit
  double glyphs. Suite: 471 passed / 11 skipped; deprecation warnings 59
  (bridge's deprecated-fn call is hidden by game's `#![allow(warnings)]`).
- 8c LANDED (2026-08-02): world-square-binned producers + property tests.
  - CHECKPOINT CORRECTION: `WorldSquareGlyphMap` (screen.rs:451) IS
    deprecated ("World does not know about glyphs") — the note below claiming
    otherwise is wrong. The 5 Group-A uses were replaced with explicit
    `HashMap<WorldSquare, DoubleGlyph>`.
  - braille.rs: `world_point_to_world_braille_point` rewired to skip the
    deprecated char-grid hop (`(4x+1.5, 4y+1.5)`, algebraically identical);
    new `points_to_braille_double_arrays(Vec<impl Into<WorldPoint>>) ->
    HashMap<WorldSquare, DoubleBrailleArray>` bins dots directly
    (`local = braille_square - 4*world_square`).
  - glyph.rs: new `Glyph::double_glyphs_for_colored_braille_line` (wraps
    `get_braille_arrays_for_braille_line`) and `Glyph::points_to_braille_double_glyphs`;
    both emit `HashMap<WorldSquare, DoubleGlyph>`, empty half ->
    `transparent_glyph()` to match old `pair_up(.., transparent_glyph())`.
  - hextant_blocks.rs: new `points_to_hextant_double_glyphs(points, color) ->
    HashMap<WorldSquare, DoubleGlyph>` (only consumer: blink_animation).
  - TWO REAL BUGS CAUGHT by the property tests (the safety net paid off):
    1. euclid's `Point2D::round` for floats is `(x+0.5).floor()` (half-UP),
       NOT `f32::round` (half away from zero) — they disagree at negative
       half-integers, flipping left/right char index. New hextant code uses
       `(v+0.5).floor()` to match the old path exactly.
    2. PRE-EXISTING BUG FIXED in `braille_square_to_dot_in_character`:
       `(pos.y % 4).abs()` vertically MIRRORED braille dots inside characters
       at negative y (e.g. by=-13 -> 1 instead of 3). Now `rem_euclid`.
       Changes behavior of the old (still live until 8d) line/point braille
       paths for negative coordinates — for the better. No golden test
       encoded the buggy output (suite untouched).
  - Property tests: `test_direct_braille_binning_matches_paired_char_grid`,
    `test_direct_hextant_binning_matches_paired_char_grid` — 0.125-step grid
    over [-4,4]^2 (f32-exact) + .5 ties + negatives + multi-point sets,
    `#[allow(deprecated)]` (dies with old path in 8f).
  - Suite: 473 passed / 11 skipped (471 + 2 new). Deprecation warnings: 59,
    all pre-existing old-path definitions; new code adds zero.
- NEXT: 8d (migrate 8 Group-B impls to the new producers + 4 test call
  sites in animations.rs; simple_laser/circle_attack are 1-liners, six
  Vec<WorldPoint> braille producers + blink's hextant), then flip + deletions
  per 8e–8g below.

## Original plan (superseded in order, not in content): flip the trait, migrate the 11 impls, then delete the char-grid API

### 8a. Flip the trait (animations.rs)

- Make `double_glyphs_at_time` the **required** method; demote
  `glyphs_at_time` to a defaulted compat adapter (reverse direction:
  un-pair the double-glyph map via a `[Glyph;2] → 2 char-grid entries`
  converter, or just keep `pair_up_character_square_map`'s inverse until
  the 4 test call sites migrate).
- This makes the 11 legacy impls fail to compile → forced, verifiable
  migration, no silent fallbacks.

### 8b. Group A — already world-square producers (3 files, trivial)

`radial_shockwave.rs:65-109`, `recoiling_board.rs:97-124`,
`smite_from_above.rs:32-51` already build `HashMap<WorldSquare, [Glyph;2]>`
and then **round-trip** through
`world_square_glyph_map_to_world_character_glyph_map` (e.g.
radial_shockwave.rs:106-109 inserts `[-1.0,1.0].map(...)` pairs, then
un-pairs them). Migration: rename `glyphs_at_time` → `double_glyphs_at_time`,
return the map directly, delete the converter call + import. Pure dead
round-trip removal — no behavior change.

### 8c. New world-square-binned producers in terminal_rendering

- **braille.rs**: `get_braille_arrays_for_braille_line` (line 309) already
  returns `HashMap<WorldSquare, DoubleBrailleArray>` — the proven pattern.
  Add `DoubleBrailleArray → DoubleGlyph` conversion; re-wrap as
  `get_glyphs_for_colored_braille_line` returning
  `HashMap<WorldSquare, DoubleGlyph>`.
- **braille.rs**: new `points_to_braille_double_arrays(points)` — mirror of
  `points_to_braille_chars` (line 322) but bin by `WorldSquare` directly
  (`point.round().to_i32()` on the `WorldPoint` — equivalent to the old
  `world_point_to_world_character_point` → `.round()` → `div_euclid(2)`
  chain, since char_x = 2*world_x + 0.5), then bin local braille dots into
  a `DoubleBrailleArray` reusing the pairing tail of
  `get_braille_arrays_for_braille_line`.
- **hextant_blocks.rs**: new world-square-binned variant of
  `points_to_hextant_chars` (line 200). Keep
  `world_point_to_local_character_point` (already the new local-frame API,
  not deprecated); drop the `world_point_to_world_character_point` hop.
- **Verification safety net**: a property test comparing old
  (char-grid + `pair_up_character_square_map`) vs new (direct bin) output
  maps across a grid of fractional offsets — this is the one place the
  migration can silently change binning at boundaries (round-ties, negative
  coords).

### 8d. Migrate Group B — braille/char-map animations (8 files)

`simple_laser` (1-liner: `Glyph::get_glyphs_for_colored_braille_line`),
`circle_attack` (1-liner: `Glyph::points_to_braille_glyphs`), and the six
`Vec<WorldPoint>` → `points_to_braille_glyphs` producers (blink,
burst_explosion, floaty_laser, piece_death, selector, spear_attack) all
switch to the 8c producers and retype to `double_glyphs_at_time`.

Then migrate the 4 test call sites in animations.rs — note
`test_recoil_animation_has_smooth_animation_at_start_of_recoil_left`
(animations.rs:143) asserts on `WorldCharacterSquare::new(1, board_length-1)`
→ becomes a `WorldSquare` key with a `[Glyph; 2]` value — and the
`glyph_map_to_string`-based tests (176, 190, 198) need a double-glyph map
stringifier or an un-pair before rendering. Delete `glyphs_at_time` +
`glyphs_at_duration` from the trait once migrated.

### 8e. Game-side leftovers (graphics.rs)

- `draw_string_to_draw_buffer` (graphics.rs:315) has **no live callers**
  (only commented-out test references at game/tests.rs:796-798,902-904,
  947-949) and its `world_square_to_left_world_character_square` (318) +
  `draw_glyphs` (146) are only used by it (graphics.rs:327). Delete all
  three outright — verify trait membership of `draw_glyphs` first.
- This also kills the last `pair_up_character_square_map` call sites in
  `game` (graphics.rs:39,148; animations.rs:44).

### 8f. Delete the deprecated char-grid API (closes item 2 Phase 2c)

Kills all 59 terminal_rendering warnings + game's hidden uses:
- **screen.rs**: `WorldCharacterSquare/Point/Step/Move`,
  `CharacterGridInWorldFrame`,
  `world_square_glyph_map_to_world_character_glyph_map`,
  `world_character_glyph_map_to_world_square_glyph_map`, the 4 conversion
  fns, `is_world_character_square_left/right_square_of_world_square`.
- **braille.rs**: `world_character_point_to_braille_point`,
  `braille_pos_to_character_world_pos`,
  `world_braille_point_to_world_character_point`,
  `world_braille_square_to_world_character_square`, `points_to_braille_chars`
  (and any `get_chars_for_braille_line` internals left without callers).
- **hextant_blocks.rs**: `points_to_hextant_chars`.
- **glyph.rs**: `get_glyphs_for_colored_braille_line`, `points_to_braille_glyphs`,
  `character_world_pos_to_colored_braille_glyph`, and
  `pair_up_character_square_map` once 8e removed its callers.
- **floating_square.rs / drawable_glyph.rs**: per the roadmap-2 checkpoint
  decision, `character_map_for_full_square_at_point` is un-deprecate-with-TODO
  (only its own tests call it) rather than deleted; the drawable_glyph.rs:15
  imports and :391 use of the char-grid types die with 8f.

### 8g. Close item 2 (Phase 3)

Remove `#![allow(warnings)]` from `crates/game/src/lib.rs:2`; workspace must
build warning-free with `--tests`; add `cargo clippy --workspace -- -D warnings`
to the bacon.toml workflow.

## Sequencing & exit checks

Order: 8a → 8b → 8c → 8d → 8e → 8f → 8g. Each step compiles and the suite
stays green (currently 471 passed / 11 skipped).

Per-step exit check:
```
cargo build --workspace 2>&1 | grep -c "use of deprecated"   # 59 → 0 by 8f
cargo nextest run                                             # green throughout
```

Risks:
- **8c binning equivalence** (boundary rounding) — mitigated by the property
  test in 8c.
- **8d test assertions** on exact chars (`glyph_map_to_string`) — may need a
  small double-glyph stringifier; do not weaken the assertions.
- **8e `draw_glyphs` trait membership** — verify before deleting; if it's a
  required trait method (not a default), delete the whole method from the
  trait.
