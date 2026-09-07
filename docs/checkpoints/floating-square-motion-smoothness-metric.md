# Floating square debug tool — motion smoothness metric + per-method error-over-time sparkline

**Status: COMPLETE — 2026-09-07.** Stage 1 (architecture + resolved
decisions) and Stage 2 (implementation) both landed, per plan above.

## Verification (2026-09-07)

- `cargo test -p terminal_rendering`: 154 passed / 0 failed — lib 141
  (was 139; +2 frame-diff pane tests), bin 4 (+3 history/sparkline/
  metric-report tests), integration 9.
- `cargo test --workspace`: 489 passed / 0 failed (game crate untouched;
  no change to game render paths).
- Piped `animate` frames: method rows are 132 visible columns exactly
  (98 + 2 gap + 32 sparkline); the sparkline grows one block per frame,
  right-aligned (frame k shows k value chars), and the shared scale
  legend tracks the running max (max=0.000 → 0.049 → 0.053 across six
  orbit frames). Frame-metric semantics (n/a before the first prev,
  pane ≡ number over the window crop, growth with displacement) are
  covered by unit tests.
- Other modes (pos / families / sweep) smoke-tested — unchanged.
- Surfaced along the way (pre-existing, NOT from this change): a fresh
  full recompile emits a `private_bounds` warning for the private
  `PixelStats` in `pub fn ClassGrid::full_pane`'s signature (coverage.rs),
  which stale incremental-compilation warning caches had been hiding —
  left for roadmap item 2's warning cleanup.

## User request

1. Add a frame-to-frame xor measurement: `xor(render(t-1), render(t))`, as a
   new selectable error metric in the existing error-measurement cycle.
2. Add a section on the right of each rendering-method row that tracks the
   currently-selected error over time, drawn with the 1/8th-increment
   vertical blocks (`▁▂▃▄▅▆▇█`).

## Plan

### New metric: `frame` (index 6 in the `,`/`.` cycle)

In `coverage.rs` (kept with the shared metrics machinery, `#[doc(hidden)]`):

- `frame_diff_xor(prev, cur) -> f32` — fraction of samples over the current
  frame's 3x3 window (same lattice and denominator `SX*SY` as
  `coverage_error`) where the two rendered fills differ. Units: world square
  units, directly comparable to the existing `xor` metric.
- `frame_diff_pane(prev, cur, style) -> Vec<String>` — big-pixel pane lighting
  every sample where the two renders differ (reuse `XOR_COLOR`, same 2x2 crop
  as the other metric panes).

Both take two `([[DoubleChar;3];3], WorldSquare)` pairs and use
`actual_sample` (owners via `assign_colors`, computed internally), so
"no previous frame" naturally renders as empty-everywhere. Interpretation:
frame-diff measures rendered *change* per frame — it scales with motion
speed, so it compares methods fairly only side by side under the same
motion (the shared-y-axis use), not as an absolute smoothness score.

In `floating_square_debug.rs`:

- `METRICS` grows from 6 to 7 entries (append `"frame"`).
- `AnimState.prev_renders: [Option<([[DoubleChar;3];3], WorldSquare)>; 4]`
  (slot 0 = in-use, 1..=3 = candidates), updated at the end of each rendered
  frame, for **all four slots** — moving or not — so the frame metric always
  diffs against the immediately previous render. Never reset (candidate
  switches don't touch it); `None` only before the first frame.
- `frame` shows `n/a` only while `prev_renders[slot]` is `None` (process
  start); a parked square shows the true diff (renders identical →
  `0.000`) — honest, and needs no special casing.

### Sparkline column (per method row)

In `AnimState`:

- `history: [Vec<f32>; 4]` (same slot indexing as `prev_renders`) +
  `history_metric: usize` (which metric the buffers hold) +
  `last_sample_pos: Option<WorldPoint>` (pos of the last appended frame).
- Histories reset when the metric changes (`history_metric` mismatch).
- All four histories are appended on every frame in which the square
  actually moves — pos differs from `last_sample_pos` (manual mouse
  drags/angle-drags and arrow nudges change pos, so they count; parked
  redraws like metric or candidate switches append nothing). The frame
  metric's very first sample has no prev → skipped.
- Candidate switches reset **nothing**: all four slots keep accumulating,
  `[`/`]` only changes which two are displayed, and the shared y-axis
  stays stable across switches.
- Consequence: the loop computes all four neighborhoods + MetricReports
  per moving frame and discards the panes of the two non-displayed slots
  (one code path; a debug tool at 30fps can afford the extra pane work).

Rendering:

- New const `SPARKLINE_LEN: usize = 32` (tunable; matches the existing
  zoom/error column width).
- `sparkline_column(history, scale, style)` maps each value to
  `level = round(value / scale * 8)` clamped to 0..8: `·` for 0,
  `EIGHTH_BLOCKS_FROM_BOTTOM[level]` for 1..8. Oldest on the left,
  right-aligned, padded with `·` until the buffer fills.
- **Shared y-axis**: `scale` = max over all four method histories for the
  selected metric (floor ~1e-6) — meaningful now that all four slots stay
  live, and it keeps the axis stable across candidate switches. All
  history scalars are magnitudes (≥ 0; center/area by abs above), so
  `level = round(value / scale * 8)` needs no sign handling. Scale printed
  as a legend (`0 ▁▂▃▄▅▆▇█ max=…`).
- `method_section` gains a 4th column (title + sparkline + scale legend)
  passed to `boxed_row`.

### Refactor (single source of truth for metric computation)

The metric computation currently lives inline in `method_section`'s
`match metric` block. Extract:

- `metric_report(nb, glyphs, center, pos, metric, prev, style) -> MetricReport`
  where `MetricReport { pane: Vec<String>, value: String, number: Option<f32> }`.
  `number` is the history scalar: `center` = |centroid − pos| (Euclidean;
  `None` when the fill has no centroid), `area` = |signed error| (the pane
  keeps the red/blue sign), the rest = the displayed value as-is; `frame`
  = the diff, `None` only while `prev` is `None`.

`method_section` uses it for the pane + value string; `render_animation_frame`
uses it to get `number` for the history **before** building the rows (so the
current frame is the newest sparkline slot — no one-frame lag).

`method_section` will take precomputed `(glyphs, center)`, the `MetricReport`,
the method's `history` slice, and the shared `scale`, instead of calling
`nb(pos)` internally.

### Terminal width

Measured piped output: method rows are 98 visible columns today (the
"large" column is ~24 wide — the info/objective text lines, not the
18-wide grid), the common row 108. The 4th column (+32, +2 gap) brings
method rows to ~132 — the new view width. This assumes a wide terminal;
`SPARKLINE_LEN` can drop to 16–24 if that is a problem. Mouse mapping
(`GRID_SCREEN_ORIGIN`) is untouched: the new column goes on the right, so
column 0 doesn't move (its stale comment is a known leftover, separate
fix if ever).

## Resolved decisions

1. Y-axis: shared auto-scale — max over all four slot histories, floor
   ~1e-6; stable across candidate switches.
2. `SPARKLINE_LEN = 32` (~1.05s of history at the 33ms frame clock).
3. `center` history scalar: absolute distance |centroid − pos|.
4. `area` history scalar: |signed error| (the pane keeps the sign).
5. Append history only when the square actually moved (pos changed since
   the last append); manual mouse movement counts.
6. All four slots keep history across candidate switches; `[`/`]` only
   switches the display.

## Files to touch

- `crates/terminal_rendering/src/coverage.rs` — `frame_diff_xor`,
  `frame_diff_pane`, unit tests.
- `crates/terminal_rendering/src/bin/floating_square_debug.rs` — `METRICS`,
  `MetricReport`/`metric_report`, `sparkline_column`, `AnimState` fields,
  `method_section`, `render_animation_frame`, event handlers, `usage()`,
  and the `//!` module doc's animate-mode description.

## Tests

- `coverage.rs`: `frame_diff_xor` unit tests — identical renders → 0.0; a
  small move → > 0 and monotone in displacement direction.
- `coverage.rs` `pane_tests` convention: `frame_diff_pane` is
  BIG_TEXT_ROWS x BIG_PX_W and lights exactly the differing samples of
  the window crop (pane ≡ number).
- Binary: level→char mapping (0 → `·`, 8 → `█`) and right-alignment/padding
  behavior.
- Binary: history semantics — append only when pos changed; metric change
  resets all four buffers; candidate switch preserves them (no re-scale).
- Existing `charwise_rendering` + `floating_square_coherence` tests must stay
  green (no change to game render paths).

## Verification

- `cargo test -p terminal_rendering` (unit + integration).
- Piped animate frames eyeballed: frame-diff pane lights changed samples
  only; sparkline grows rightward, resets on metric change only, stays
  put (and unscaled) across candidate switches, and in-use vs candidate
  share one y-axis.
