# Full-resolution zoomed views + two-method interactive layout

**Status: COMPLETE.** All workspace tests pass; patch proposal generated.

## User request

1. Zoomed views at FULL resolution: every square-drawing character displayed
   natively, each rendering increment (glyph) potentially its own color.
2. Full-res panes × the 4-method × 6-metric grid wouldn't fit on screen, so:
   show only TWO methods at once — the in-use game path (family-snapped) and
   a candidate replacement cycled with two buttons.
3. Per method show only: real-size view, zoomed view, the error used for
   rendering, and ONE error component cycled with two buttons.

## What was built

### coverage.rs

- `SampleClass` became 4-state (MatchFilled/MatchEmpty/Over/Under) so panes
  can show the silhouette without a FillGrid; `class_at` updated.
- `charwise_glyph_penalty` refactored → `charwise_glyph_parts(pos, square,
  half, weight, squared) -> (char, err, d)`; candidates carry triples, all
  comparisons via cost(err,d). Weight-0 bit-exactness preserved (same tie
  orderings; weight-0 equivalence test still passes). Hextant brute force
  tracks (cost, bits, err, d); majority path returns d=0.
- New `#[doc(hidden)] pub fn charwise_objective(pos, weight, squared)` =
  Σ over the 3x3 neighborhood of err + weight·(d or d²) — the method's own
  picker objective, shown as "the error used for rendering".
- Half-res 12x6 pane builders replaced by full-res versions (same names):
  the pane grid is now 24x12 text cells = the native sampled pixel grid
  (each pixel = 2x3 samples, two pixels stacked per text cell via
  half-blocks). Shared helpers: `two_tone_cell` (bitmap_pane refactored onto
  it) and `pane_from_colors`. Pixel geometry: window 3 world units = 24 cols
  (8/unit) x 24 px rows (8/unit); half-cell = 4 px wide, 8 px tall
  (py/8, top pane rows = +y); PixelStats{filled, over, under, inside,
  outside} per pixel.
- Panes: center (dim silhouette + IDEAL_COLOR outline where a pixel
  straddles the ideal edge + '×' actual centroid / '+' ideal center
  markers), signed area (over red / under blue / split = xor color),
  per-char heat (half-cell |rendered−ideal| area, dark→hot ramp /0.25),
  xor (any mismatch lit), jaggedness (contour pixels lit by local edge-step
  length, ×8 normalized), displacement (newly-wrong yellow / still-wrong
  dim red / recovered dim blue under the worst 1/16 nudge).

### floating_square_debug.rs animate mode

- Two rows only: "in use: family-snapped" (fixed) and "candidate: {name}
  ([ ] cycle)". CANDIDATES = charwise, charwise + protrusion (xor+1.0·d),
  charwise + protrusion² (xor+4.0·d²). AnimState.candidate/.metric
  (defaults 0); keys `[`/`]` cycle candidate, `,`/`.` cycle metric.
- Per row: large 9x9 grid (first column — GRID_SCREEN_ORIGIN (2,2) mouse
  mapping unchanged) + method info + objective line ("bake objective (xor)",
  "Σ cell xor", "xor+1.00·Σd", "xor+4.00·Σd²"); full-res zoomed render with
  per-glyph palette colors + legend; selected metric's full-res pane with
  header "{metric} (, .)" and value. Displacement shifted-grid computed
  lazily only when metric == disp.
- Layout: ~125 cols x 48 lines piped; small 3x3 view dropped per request.
- usage()/module doc/bottom help updated ([]=cand ,.=metric).

## Verification

- `cargo test --workspace`: 480+ pass, 0 fail (incl. weight-0 equivalence
  over the lattice, kick-in threshold, coherence bounds, comparison table).
- Piped frames eyeballed at aligned and off-center positions: outline ring
  and centroid markers land on the ideal/render edges, heat cells track
  per-half-cell error, values align under panes.

## Interpretation note

"Error used for rendering" is per-method: family-snapped's is the sampled
xor the family map bakes against; the charwise variants sum their own
per-cell closed-form objective (cell units — a half-cell is 0.5x1 — so the
numbers are not directly comparable to the lattice-sample metrics in the
panes; each is the quantity its picker actually minimizes).

## Follow-up fix: ideal pane corner divots (COMPLETE)

User spotted corner divots in the "ideal (true square)" pane. Cause: the
ideal was *sampled* and pushed through the same majority-vote pixel
pipeline as the render (pixel lit at >=3 of 6 samples). Edge pixels are
partially covered by construction (an open 1.0-length interval contains
only 15 of 16 sample columns), and a corner pixel combines a half-filled
column (1 of 2) with a partially-covered row (<=2 of 3) -> vote <= 2 ->
unlit, while its neighbors pass. Systematic four-corner divots at any
zoom; not a resolution problem.

Fix: the ideal pane is now drawn analytically (`ideal_pane` in the tool):
each pixel is exactly 1/8 x 1/8 world units, so per-pixel ideal coverage
area is closed-form; pixels shade from the checkerboard bg to IDEAL_COLOR
by that fraction (sub-pixel edge phase visible). Used in both the animate
common row and coverage_zoom_pane (pos mode). Metrics unchanged — they
remain lattice-based; only the reference picture changed. `lerp` /
`pane_from_colors` made pub for the tool; note tool imports shadow the
glob-imported utility::lerp. Verified divots gone at aligned (0.25,-0.7)
and fractional (0.3,0.3) positions; all workspace tests pass.
