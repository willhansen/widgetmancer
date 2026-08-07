# Roadmap item 2 — Phase 3, Step 1: trivial deprecation cleanup

Date: 2026-08-07

## Scope
First chunk of Phase 3: drop dead code and retire the three simplest
deprecated symbols in the `game`/`terminal_rendering` crates. This is the
"smallest blast radius" slice; the two remaining deprecations
(`from_square_visibility`, `draw_glyphs_for_square_to_draw_buffer`) are
separate follow-up steps because they carry semantic changes.

## Changes landed

### `crates/game/src/graphics.rs`
- Renamed `Graphics::square_is_white` → `Graphics::square_is_light`, with a
  doc comment stating the invariant (`(x + y) % 2 == 0` = checkerboard light
  squares). Name now says *what* it tests.
- Deleted `checkerboard_square_function` — private, zero callers.
- Deleted `off_board_color_at_square` — `pub` but zero callers.

### `crates/game/src/graphics/animations/radial_shockwave.rs`
- Updated the two `square_is_white` references to `square_is_light`. This was
  the only live caller of the renamed fn.

### `crates/terminal_rendering/src/glyph.rs`
- Deleted `Glyph::get_glyphs_for_player` (deprecated, "Use ArrowDrawable
  instead"). Its only caller was a test. `THICK_ARROWS` (glyph.rs:321) and
  `extract_arrow_from_arrow_string` (glyph.rs:348) still have other callers so
  no import cleanup was needed.

### `crates/game/src/game/tests.rs`
- Replaced the one `Glyph::get_glyphs_for_player(STEP_UP.into())` call with
  `ArrowDrawable::new(STEP_UP.into(), THICK_ARROWS, PLAYER_COLOR)
  .to_glyphs().to_clean_string()` — `ArrowDrawable::new(KingWorldStep, &str,
  RGB8)` (drawable.rs:299) synthesizes the identical transparent-bg arrow pair
  the deleted helper built by hand.
- Added `ArrowDrawable` to the `crate::graphics::drawable` import and
  `THICK_ARROWS`, `PLAYER_COLOR` to the `glyph_constants` import.

## Verification
- Re-added `#![deny(deprecated)]` to `crates/game/src/lib.rs`:
  probe output now contains ONLY the two intended remaining deprecations —
  `PartialVisibilityDrawable::from_square_visibility` (16, incl. 1 in
  drawable.rs) and `Graphics::draw_glyphs_for_square_to_draw_buffer` (8).
  No trace of `square_is_white`/`off_board_color_at_square`/
  `get_glyphs_for_player`.
- Removed the probe (allow still in place pending the full Phase 3).
- Full workspace suite: **459 passed / 11 ignored**, unchanged from the
  roadmap baseline (203 + 127 + 103 + 26 passes; 8 + 1 + 1 + 1 ignored).

## Next
Step 2: `Graphics::draw_glyphs_for_square_to_draw_buffer` (8 uses, all
internal to `graphics.rs`) — migrate callers to
`draw_drawable_to_draw_buffer(.., &TextDrawable::from_glyphs(glyphs))` and
delete the method. Then Step 3: the `from_square_visibility` →
`from_partially_visible_drawable` sweep (semetic change, new `: &T` arg),
then remove `#![allow(warnings)]` + add clippy.
