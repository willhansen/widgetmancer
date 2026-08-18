# Checkpoint — Floating square rendering improvements (items 1, 2, 3, 5)

In progress. Follow-up to
[floating-square-rendering-quality.md](floating-square-rendering-quality.md)
(silhouette tearing fix). Plan approved 2026-08; see ROADMAP.md item 10.

## Background

Post-tearing-fix evaluation (via `debug-floating-squares.sh pos 0.3 0.7` +
the `Metrics` oracle) found a residual defect: a 0.25 × 1/6 notch in the
top-right corner, caused by one wrong hand-written entry in
`hextant_block_by_offset` (`(-1,-1) => '▖'` — quadrant bottom-half — where
the geometry requires `🬓`, sextant left-column bottom-two-thirds). The
one-char fix is applied locally in
`crates/terminal_rendering/src/hextant_blocks.rs` and verified
(spreads 0, area exactly 1.0 at (0.3, 0.7)); item 1 subsumes it by
generating the table.

## Plan (implementation order)

### Step 0 (3a+3c): metrics plumbing
- Move `Metrics` + `Metrics::measure` from
  `tests/floating_square_coherence.rs` into `src/coverage.rs` as
  `#[doc(hidden)] pub` so the debug tool can use them.
- `floating_square_debug` `pos` mode prints edge spreads / holes / area
  under the coverage panes.

### Step 1 (item 1): generated glyph tables
- Replace the hand-written 15-entry match in
  `hextant_blocks::hextant_block_by_offset` with a generated rectangle of
  mini-blocks (column mask from x step ∈ {-1,0,1}, row mask from y step ∈
  {-2..=2}, product → `hextant_array_to_char`). Verified equivalents for
  all existing correct entries; yields `🬓` for `(-1,-1)`.
- Same treatment for `quadrant_block_by_offset` (2×2 grid in
  `floating_square.rs`).
- New test: for every reachable step combo, `glyph_filled` must agree with
  the analytic square∩half-cell overlap rectangle at all 2×3 (or 2×2)
  probe points.

### Step 2 (item 3b): dense-sweep silhouette test
- `test_silhouette_coherent_over_offset_plane`: offsets over [-0.5, 0.5)²
  at 1/24 steps (finer than the finest snap spacing, 1/16 x), asserting
  edge spreads == 0, no holes, area ≈ 1 per position. Density
  overridable via env var. Subsumes (but does not delete) the motion-line
  smoke test.

### Step 3 (item 2): baked family-selection map
- New `src/family_map.rs`: `FAMILY_BY_OFFSET: [[u8; 24]; 24]` over the
  fundamental domain [0, 0.5)² at 1/48 resolution (half the 1/24 sample
  step → alias-free), baked offline by a test using
  `characters_for_full_square_with_2d_offset_forced` + `FillGrid`
  symmetric-difference area per family; blessed-file regeneration pattern
  (`BLESS_TESTS`-style env var), coarse live re-validation in the normal
  test path.
- `SnapFamily::for_offset` becomes a lookup; `snapped_offset`/`snap_error`
  stay for diagnostics.
- Debug tool candidate list switches to coverage error so the printed
  ranking always justifies the pick.

### Step 4 (item 5): family-switch hysteresis
- Safety fact (verified in design): snap error is identical for all 9
  neighborhood cells (integer-aligned grids), so biased selection cannot
  tear the silhouette.
- `for_offset_biased(o, incumbent: Option<usize>)` with
  `FAMILY_SWITCH_PENALTY = 0.02` world units; new
  `characters_for_full_square_with_2d_offset_biased` returns the picked
  family index (reuses the `snap_family_names()` index convention —
  `SnapFamily` stays private).
- Game side: floating entities (death cubes, hunter drones in
  `crates/game/src/game/floating_entities.rs`) gain
  `family_memory: Cell<Option<usize>>` (draw path is `&self`).
- Tests: family-switch count over the line trajectory drops vs. unbiased;
  silhouette metrics still pass with hysteresis on; biased pick never
  exceeds `best_error + FAMILY_SWITCH_PENALTY`. Portal rotations drop the
  memory (family re-derived in rotated space) — acceptable, comment why.

## Progress log

- 2026-08: plan documented; `▖`→`🬓` one-char fix already applied locally
  (subsumed by Step 1). Starting Step 0.
- Step 0 DONE: `Metrics` moved to `src/coverage.rs` (`#[doc(hidden)]`,
  plus `summary_line()`); debug `pos` mode prints the metrics line;
  coherence test imports it from there.
- Step 1 DONE: `hextant_block_by_offset` and `quadrant_block_by_offset`
  are now generated (column mask × row mask → `hextant_array_to_char` /
  quadrant match), plus `test_hextant_table_matches_square_overlap_geometry`
  and the quadrant equivalent probing `glyph_filled` against the analytic
  overlap rectangle. 127 lib tests green.
- Step 2 DONE + TWO BUGS FOUND by the new sweep
  (`test_silhouette_coherent_over_offset_plane`, default 24/axis, env
  SWEEP_DENSITY; green at 96):
  1. `snap_to_nths` used `f32::round` (half away from zero), which is NOT
     translation-invariant at exact ties (round(0.5)=1 but
     round(-3.5)=-4 ≠ round(0.5)-4), breaking the integer-alignment
     invariant family coherence rests on. Fixed to half-up
     (`(x*n + 0.5).floor()`), matching euclid/`world_point_to_world_square`.
  2. Family selection tie-break incoherence: at exact decision boundaries
     (e.g. offset (1/8, 7/24), hextant/v-eighths exactly equidistant),
     float ULP noise in `snap_error` flipped the family pick between cells
     of one square, tearing the silhouette. Fixed with
     `SNAP_ERROR_TIE_EPSILON = 1e-3` + fixed priority order in
     `SnapFamily::for_offset`. Residual risk: a ~2e-6-wide shell around
     gap==EPSILON contours; accepted, watched by the sweep test.
- Next: Step 3 (baked family map), Step 4 (hysteresis).
