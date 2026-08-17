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
  at 5/9 positions (demonstrating the bug), deliberately left red until
  the fix lands (user decision over `should_panic`/`#[ignore]`). The full
  visual report rides the panic payload; nothing is printed on pass.
  Renders the square at 9 evenly spaced positions along a line through
  (2.363, -0.816) (it is [5]). Asserts edge coherence
  (top/bottom/left/right edge spread == 0, no holes, non-empty fill with
  area within 0.3 of 1.0 — the area check closes the vacuous-pass hole
  for empty/degenerate renders) — a bar any single glyph family applied
  per-square clears. At [5] the top-edge spread is 0.333 and the
  bottom-edge spread is 1.000.
  Report layout: (1) a horizontal strip of the small 6x3 glyph views at
  all positions, monochrome (uniform grey square on the cell checkerboard);
  (2) the same strip with each half-cell glyph
  in its own ANSI truecolor; (3) the correct rendering at the same zoom
  (true square glyphized coherently via hextants, 2x3 sub-cell majority),
  colored per piece — pieces the actual render lacks are gray; (4) `^^^`
  markers under the failed columns; (5) a legend with per-position
  pos/frac/status; (6) one zoomed-in row per failed position (sampled
  bitmaps of actual vs ideal coverage over a dark-grey cell checkerboard,
  `^` markers under deviating columns, edge-spread metrics). A text cell
  straddling two glyphs shows upper=fg, lower=bg. NO_COLOR=1 disables
  colors (empty cells fall back to dots).
  Run: `cargo test -p terminal_rendering --test floating_square_coherence -- --nocapture`
- `test_glyph_filled_coverage_model` — pins the coverage oracle
  (`glyph_filled`) that all metrics flow through: half/quadrant/eighth/
  third blocks plus sextant bit order, cross-checked against
  `hextant_array_to_char`.
- `test_1d_offset_rendering_moves_monotonically` — passes; regression net
  for the live 1D path (`characters_for_full_square_with_looping_1d_offset`,
  used by shockwave animations at `crates/game/src/graphics/drawable.rs:383`).

Supporting change: `hextant_character_to_binary` in
`crates/terminal_rendering/src/hextant_blocks.rs` is now
`#[doc(hidden)] pub` (the test's coverage model needs sextant bit
patterns).

The rest of the workspace suite is green (467 passed). **Note: `cargo test`
is red until the fix lands** — the coherence test is the only failure.

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
