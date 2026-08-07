# Roadmap item 2 — Phase 3, Step 3: retire from_square_visibility

Date: 2026-08-07

## Scope
Last deprecation in the game crate. `PartialVisibilityDrawable` had a
deprecated `from_square_visibility(SquareVisibility)` in addition to the
non-deprecated `from_partially_visible_drawable(&T original_drawable,
SquareVisibility)`. The replacement differs only in that it takes the drawable
being shadowed so `fg_color` is derived from it, instead of hard-coding `GREEN`.
The old note's framing: "Shadows should be conceptualized as lack of visibility".

## The one wrinkle (and why it didn't bite)
The replacement ctor's new `&T` arg means the migration is not a rename — each
call site must nominate a drawable. But it turned out **all 17 uses were in
`#[cfg(test)]` code** (15 in `fov_stuff.rs`'s `mod tests`, 1 in
`graphics/drawable.rs`'s test `test_shadow_over_text`, plus the deprecated def
itself). The tests compare `to_glyphs().to_clean_string()` output (color-blind),
so the `fg_color` is irrelevant to their assertions — a
`&SolidColorDrawable::new(GREEN)` placeholder preserves the old hard-coded
default exactly.

I verified `SolidColorDrawable` (not `SolidColor`) is in scope: `fov_stuff.rs`
line 18 imports `SolidColorDrawable`, and its `mod tests` does `use super::*`
(line 1250); `GREEN` is imported in the tests module (line 1242). The
`&SolidColorDrawable::new(...)` pattern was already established in
`fov_stuff.rs` tests (lines 2380, 2494).

## Changes landed
- `crates/game/src/fov_stuff.rs`: 15 call sites rewritten
  `from_square_visibility(X)` -> `from_partially_visible_drawable(&SolidColorDrawable::new(GREEN), X)`.
- `crates/game/src/graphics/drawable.rs`: 2 substitutions — the one test use
  (which, reordered mentally, keeps `fg_color`=GREEN distinct from the text
  it's drawn over, preserving the `assert_ne!(fg, bg)`), plus the deprecated
  definition deleted.
- No import changes needed (both names already in scope in every touched
  module; `GREEN` in the non-test scope resolves via the `named_colors::*` glob
  in `drawable.rs`).

## Verification
- Re-added `#![deny(deprecated)]` probe: **0** `use of deprecated` errors
  (down from 16 before this step, from 24 at the start of Phase 3).
- Full workspace suite: **459 passed / 11 ignored**, unchanged.
- `cargo build -p game` warnings: only the 2 intentional terminal_rendering
  glob-shadowing notes (item 4), none from `game`.

## Next
All deprecations resolved. Remaining Phase 3 work: remove
`#![allow(warnings)]` from `crates/game/src/lib.rs`, fix any straggler lints it
was hiding, then add `cargo clippy --workspace -- -D warnings` to CI/bacon.
