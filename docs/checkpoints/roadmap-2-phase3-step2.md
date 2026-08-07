# Roadmap item 2 — Phase 3, Step 2: retire draw_glyphs_for_square_to_draw_buffer

Date: 2026-08-07

## Decision: un-deprecate-or-rename -> DELETE
The deprecated `Graphics::draw_glyphs_for_square_to_draw_buffer` added no
semantics on top of the replacement:

```rust
self.draw_above_square(&TextDrawable::from_glyphs(glyphs), world_square);
```

was byte-for-byte the body of `draw_drawable_to_draw_buffer(..)`. Renaming it
would just re-couple `Graphics` to glyphs under a helper name; the deprecation
note's intent ("Graphics should not know about glyphs") is best served by
deleting it and letting callers go through the drawable path.

## Changes landed (`crates/game/src/graphics.rs`)
- Deleted the `#[deprecated]` method.
- Migrated all 8 callers (5 internal methods + 2 in a `#[cfg(test)]` test) from
  `draw_glyphs_for_square_to_draw_buffer(s, glyphs)` to
  `draw_drawable_to_draw_buffer(s, &TextDrawable::from_glyphs(glyphs))`:
  - `draw_glyphs_at_squares`
  - `draw_piece_with_color`
  - `draw_upgrade`
  - `draw_arrow`
  - `draw_same_glyphs_at_squares`
  - `test_draw_buffer_to_screen_through_field_of_view` (x2)
- `TextDrawable` was already imported (line 31), so no import change.

## Verification
- Re-added `#![deny(deprecated)]` probe: only
  `PartialVisibilityDrawable::from_square_visibility` (x16) remains —
  `draw_glyphs_for_square_to_draw_buffer` gone entirely.
- Full workspace suite: **459 passed / 11 ignored**, unchanged.

## Next
Step 3 (last deprecation): `from_square_visibility` -> `from_partially_visible_drawable`
(semantic change — the replacement ctor takes `&T original_drawable` to derive
`fg_color` instead of hard-coding `GREEN`). 16 call sites in `fov_stuff.rs` +
1 in `drawable.rs`. After that, remove `#![allow(warnings)]` and add clippy.
