# Checkpoint — Floating square silhouette tearing

Resolved. Kept as a record of the issue and the fix (ROADMAP.md item 9).

## The issue (fixed)

Reported via `floating_square_debug`: at pos=(2.363, -0.816) the floating
square rendered as a ragged blob (`🬞▁🬏` over `🮇🮆▊`), not a square.

**Root cause:** the game rendered a floating square per terminal half-cell,
snapping each half-cell **independently** to the nearest of ~60 candidate
offsets from four glyph families. Sibling half-cells could pick different
families, tearing the silhouette.

**Fix (landed):** `SnapFamily` in
`crates/terminal_rendering/src/floating_square.rs` picks one glyph family
per *square* (`SnapFamily::for_offset`, min snap error over horizontal
eighths / vertical eighths / hextant / quadrant), then snaps all half-cells
within that family. The snap grids are integer-aligned, so every cell of a
square agrees on the family by construction. Worst case is now a uniformly
slightly-misplaced square instead of a torn blob.

## Tests (all green)

`crates/terminal_rendering/tests/floating_square_coherence.rs`:

- `test_square_silhouette_stays_rectangular_along_motion_line` — renders the
  square at 9 evenly spaced positions along a line through the reported
  position (2.363, -0.816) and asserts edge coherence (edge spread == 0, no
  holes, area within 0.3 of 1.0). Failed at 5/9 positions before the fix;
  passes since. The full visual report rides the panic payload on failure.
- `test_glyph_filled_coverage_model` — pins the coverage oracle
  (`coverage::glyph_filled`) all metrics flow through.
- `test_1d_offset_rendering_moves_monotonically` — regression net for the
  live 1D path (`characters_for_full_square_with_looping_1d_offset`, used by
  shockwave animations).

Run: `cargo test -p terminal_rendering --test floating_square_coherence -- --nocapture`

## Debug tool (rewired to the real render path)

`floating_square_debug` previously displayed diagnostics for
`get_chars_for_floating_square`, a dead code path the game never called (its
`branch=` label and 3x3 dump described nothing the renderer did). That
subtree is deleted; the tool now reports the real path via `#[doc(hidden)]`
accessors (`snap_debug_info`, `snap_family_names`,
`characters_for_full_square_with_2d_offset_forced` in floating_square.rs):

- `pos X Y` — frame with a bright `+` marking the true square center, the
  winning snap family with snapped offset and error, all four candidates
  ranked by error, and a sampled actual-vs-ideal coverage pane.
- `families X Y` — the position rendered with each family forced, side by
  side, with per-family snap errors.
- `sweep` — offsets 0..=0.5 in 1/16 steps, each cell labeled with the family
  it picks (a decision-boundary map; sign-symmetric, so one quadrant
  suffices).
- `animate [N]` — orbit / arrow-key nudge (1/16 steps) / line trajectory,
  with the live family in the status line (inverted on the frame it changes;
  `switches=N` counts family changes — the visible pops), speed controls,
  and click/drag placement.

The coverage oracle (`glyph_filled`, `FillGrid`, `bitmap_pane`, …) lives in
`crates/terminal_rendering/src/coverage.rs` (`#[doc(hidden)] pub`), shared
by the test and the tool so the two can never drift apart. `Style`/`Rgb` and
the palette constants moved there too.

## Remaining loose ends (not blocking)

- The y-error weighting idea from the original proposal (weight y ~2x for
  terminal cell aspect when scoring families) was not needed for coherence;
  revisit only if squares look vertically off.
- The vertical thirds-vs-eighths per-call choice in
  `character_for_half_square_with_1d_offset` can still cause glyph-family
  flicker on the live 1D path (shockwaves). Consider hysteresis if it ever
  shows.
