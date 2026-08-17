# Checkpoint — Floating square silhouette tearing

Working state for the floating-square rendering-quality issue (ROADMAP.md
item 9), in case of interruption.

## The issue

Reported via `floating_square_debug`: at pos=(2.363, -0.816) the floating
square renders as a ragged blob (`🬞▁🬏` over `🮇🮆▊`), not a square.

**Root cause:** the game renders a floating square per terminal half-cell via
`characters_for_full_square_with_2d_offset` →
`character_for_half_square_with_2d_offset`
(`crates/terminal_rendering/src/floating_square.rs:168`). That function snaps
each half-cell **independently** to the nearest of ~60 candidate offsets from
four glyph families: horizontal eighths `(i/8, 0)`, vertical eighths
`(0, i/8)`, hextants `(halves, thirds)`, quadrants `(halves, halves)`. No
candidate has fine x *and* fine y (no such glyphs exist), so any cell with
both an x and y residual must drop one axis — and because the x-compensation
differs per half-cell, sibling cells drop *different* axes. At the reported
position the six half-cells pick three different families; the top edge lands
at five different heights across four columns.

**Red herring in the debug tool:** its `branch=` label and 3x3 dump describe
`get_chars_for_floating_square` (`floating_square.rs:24`), which the game
never calls — dead code kept alive by the debug bin and its own tests. The
rendered picture always comes from the 2d-offset path.

## Evidence (automated, visual)

`crates/terminal_rendering/tests/floating_square_coherence.rs`:

- `test_square_silhouette_stays_rectangular_along_motion_line` — **FAILS**
  (intentionally, demonstrating the bug). Renders the square at 9 evenly
  spaced positions along a line through (2.363, -0.816) (it is [5/9]).
  Per position it prints three panes: the glyphs as drawn, a sampled bitmap
  of those glyphs' exact coverage, and a bitmap of the true unit square;
  then asserts edge coherence (top/bottom/left/right edge spread == 0, no
  holes) — a bar any single glyph family applied per-square clears.
  Currently 5/9 positions fail; at [5/9] the top-edge spread is 0.333 and
  the bottom-edge spread is 1.000, with `^` markers under deviating columns.
  Run: `cargo test -p terminal_rendering --test floating_square_coherence -- --nocapture`
- `test_1d_offset_rendering_moves_monotonically` — passes; regression net
  for the live 1D path (`characters_for_full_square_with_looping_1d_offset`,
  used by shockwave animations at `crates/game/src/graphics/drawable.rs:383`).

Supporting change: `hextant_character_to_binary` in
`crates/terminal_rendering/src/hextant_blocks.rs` is now `pub` (the test's
coverage model needs sextant bit patterns).

The test suite is otherwise green (130 passed in `terminal_rendering`).
The coherence test is left failing on purpose; it should go green with the
fix. **Note: `cargo test` is red until the fix lands.**

## Proposed fixes (reviewed, NOT yet implemented — awaiting approval)

1. **Pick the glyph family once per square, not per half-cell**: in
   `characters_for_full_square_with_2d_offset`, score the four families
   against the center offset (weight y ~2x for terminal cell aspect — the
   current Euclidean metric in (half-cell, row) units under-penalizes
   vertical error ~2x, biasing toward h-eighth candidates that discard y),
   then snap all half-cells within the winning family. Edges become
   consistent by construction; worst case is a uniformly slightly-misplaced
   square instead of a torn blob. This is what makes the coherence test pass.
2. Fix the debug tool: label the actual render path; delete or clearly
   quarantine the unused `get_chars_for_floating_square` subtree (smooth
   horizontal/vertical + half-grid 3x3 + `square_with_half_step_offset`).
3. Optional: static snap-point table (the TODO'd duplicate removal at
   `floating_square.rs:211`); the function rebuilds a ~60-entry Vec per
   half-cell per frame. The vertical thirds-vs-eighths per-call choice in
   `character_for_half_square_with_1d_offset` causes glyph-family flicker on
   the live 1D path — make it hysteretic or family-fixed per call site.

## Status

- [x] Root cause identified and reviewed
- [x] Visual automated tests demonstrating the issue (failing as intended)
- [ ] Fix approved
- [ ] Fix implemented; coherence test green
- [ ] Debug tool label fixed / dead smooth path resolved
