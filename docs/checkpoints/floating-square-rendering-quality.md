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
  and click/drag placement. Extended 2026-08 with:
  - a zoomed sampled-coverage view beside the real-size grid (actual vs
    ideal, one palette color per rendered half-cell glyph, checkerboard
    marking the character cells, glyph-color legend under the grid);
  - one line per error metric (later reworked into the per-approach
    comparison metrics described in the next section);
  - fine mouse control: holding shift/ctrl/alt while dragging (or pressing
    `f`, for terminals that don't pass mouse modifiers) switches from
    absolute cell-to-grid placement to relative movement at 1/32 world unit
    per cell, so large mouse movements produce sub-cell square movements.
    termion drops the xterm modifier bits, so the tool decodes them itself
    from the raw SGR mouse bytes (`events_and_raw`).

  Also fixed a pre-existing crash the finer control exposed: clicking near
  the animation grid's edge panicked with "attempt to multiply with
  overflow" — `frame_row_col` cast possibly-negative indices to usize
  before the bounds check, and the check's own `wide_col * 2` then
  overflowed. It now returns signed indices checked before casting.

## Debug tool: approach-comparison view + glyph colors — 2026-08 (continued)

More animate-mode refinements (the `pos`/`families`/`sweep` modes are
unchanged: they keep the true-center marker and uniform square color as
single-shot diagnostics):

- The true-center `+` overlay is gone from the animate view (it hid the
  glyphs under inspection; the center-err metric and ideal pane carry the
  same information).
- The real-size square is drawn in its glyph (palette) colors instead of
  uniform orange, so a glyph means the same color in the small view, the
  zoomed view, and the legend (`draw_neighborhood_colored`).
- Fine mouse control and per-line metrics from the previous round are
  unchanged.

**Layout: one row per rendering approach, with its stats.** Row 1: the
family-snapped approach — real-size grid, zoomed actual, zoomed *ideal*
(true square), and the globally applicable stats and notes (pos/frac,
family + snap err, motion status, pane notes). Row 2: the *unrestricted*
approach — real-size grid, zoomed actual, and its own stats. Glyph-color
legends sit under each real-size grid.

**Comparison metrics, measured identically for every approach**
(`coverage.rs`, `#[doc(hidden)]` so tool and test can't drift):

- *area error* — rendered total area vs the (sampled) ideal's;
- *center error* — rendered fill centroid vs the ideal's (`fill_centroid`);
- *per-character coverage error* (`per_char_coverage_error`) — for each
  character half-cell, |rendered filled area − ideal filled area|,
  summed. Coarser than the bitmap symmetric-difference `coverage_error`:
  it only asks each half-cell to contain the right *amount* of ink;
- *jaggedness* (`jaggedness`) — the sum, along each of the four edges, of
  the perpendicular step lengths between consecutive sample columns/rows
  (each edge contour's total variation). A clean rectangle measures 0.

**The unrestricted approach does no fitting to these metrics** (an
explicitly fitted earlier design was rejected: minimizing the metrics
inside the renderer is not the point — a good fitted rule would later be
hardcoded as its own approach). It is simply: per half-cell, over every
coverage-modelled glyph (all eighth/third/quadrant/half blocks, hextants),
lowest sampled coverage error, ties broken by filled-area match
(`unrestricted_neighborhood`).

Measured over a 16x16 offset grid (tests/unrestricted_rendering.rs prints
the table; mean / max):

| approach | area | center | per-char cov | jaggedness |
|---|---|---|---|---|
| family-snapped (auto) | .021/.042 | .079/.159 | .325/.667 | 0/0 |
| unrestricted | .058/.208 | .035/.081 | .225/.417 | 1.24/3.79 |
| forced h-eighths | .021/.042 | .250/.500 | 1.00/2.00 | 0/0 |
| forced v-eighths | .021/.042 | .136/.258 | .574/1.21 | 0/0 |
| forced hextant | .021/.042 | .114/.208 | .496/.938 | 0/0 |
| forced quadrant | .021/.042 | .151/.280 | .647/1.38 | 0/0 |

Reading: the family restriction buys perfectly straight edges (jaggedness
0 everywhere — the coherence property holds over the whole grid, and every
single-family render is coherent by construction) at the cost of ~2x the
center error and ~1.4x the per-character coverage error of the
unrestricted best-fit. The test asserts only loose sanity bounds (the
family-snapped jaggedness bound is the real guard); the printed table is
the comparison.

Gotchas encountered during this work (so nobody retries them):

- `glyph_filled`'s `fx` spans the *half-cell* (one terminal column), like
  `actual_sample`'s `* 2.0` — a first implementation of per-half-cell
  scoring forgot this and was caught by the scan test.
- Fitting picks to running *global* area/centroid residuals scrambles the
  silhouette: moment cancellation pulls fill into the wrong half-cells,
  giving perfect area/center metrics on a non-square (worst coverage err
  1.30 vs 0.36).
- At positions where the true square's boundary lands exactly on a sample
  point (e.g. y frac = 1/16), the *sampled* ideal area is not 1.0
  (inclusive boundaries count it twice), so renderer-vs-ideal comparisons
  must use the sampled ideal, not the continuous 1.0.

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
