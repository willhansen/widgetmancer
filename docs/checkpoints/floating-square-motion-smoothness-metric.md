# Floating square debug tool — motion smoothness metric + per-method error-over-time sparkline

**Status: COMPLETE — 2026-09-07.** Stage 1 (architecture + resolved
decisions), Stage 2 (implementation), and two same-day follow-ups
(five-row graph + in-use baseline overlay; marker priority rules) all
landed — see the follow-up sections at the end.

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

### History column (per method row)

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

Rendering (as landed after the follow-ups — see below for the evolution):

- `SPARKLINE_LEN: usize = 32` (tunable; matches the zoom/error column
  width), `SPARKLINE_ROWS = 5` → 40 vertical levels against the scale.
- `sparkline_graph(history, baseline, scale, style)` — bars per frame
  (full rows `█`, top row a bottom-up 1/8th block), `·` zero-axis row,
  spaces above, oldest left, right-aligned.
- **Shared y-axis**: `scale` = max over all four method histories for the
  selected metric (floor ~1e-6) — meaningful since all four slots stay
  live, and it keeps the axis stable across candidate switches. All
  history scalars are magnitudes (≥ 0; center/area by abs), so the level
  mapping needs no sign handling. Scale printed in the legend.
- `method_section` gains a 4th column (title + graph rows + scale
  legend) passed to `boxed_row`; the candidate row's graph additionally
  marks the in-use (baseline) error — see the follow-ups.

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
current frame is the newest graph column — no one-frame lag).

`method_section` takes precomputed `(glyphs, center)`, the `MetricReport`,
the method's `history` slice, the optional `baseline`, and the shared
`scale`, instead of calling `nb(pos)` internally.

### Terminal width

Measured piped output: method rows were 98 visible columns before the
graph (the "large" column is ~24 wide — the info/objective text lines,
not the 18-wide grid), the common row 108. The 4th column (+32, +2 gap)
brings method rows to ~132 — the new view width. This assumes a wide
terminal; `SPARKLINE_LEN` can drop to 16–24 if that is a problem. Mouse
mapping (`GRID_SCREEN_ORIGIN`) is untouched: the new column goes on the
right, so column 0 doesn't move (its stale comment is a known leftover,
separate fix if ever).

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
  `MetricReport`/`metric_report`, the history graph, `AnimState` fields,
  `method_section`, `render_animation_frame`, event handlers, `usage()`,
  and the `//!` module doc's animate-mode description.

## Tests

- `coverage.rs`: `frame_diff_xor` unit tests — identical renders → 0.0; a
  small move → > 0 and monotone in displacement direction.
- `coverage.rs` `pane_tests` convention: `frame_diff_pane` is
  BIG_TEXT_ROWS x BIG_PX_W and lights exactly the differing samples of
  the window crop (pane ≡ number).
- Binary: graph level mapping, right-alignment/padding, baseline landing
  positions, and marker priority rules.
- Binary: history semantics — append only when pos changed; metric change
  resets all four buffers; candidate switch preserves them (no re-scale).
- Existing `charwise_rendering` + `floating_square_coherence` tests must stay
  green (no change to game render paths).

## Landing verification — 2026-09-07

- `cargo test -p terminal_rendering`: 154 passed / 0 failed — lib 141
  (was 139; +2 frame-diff pane tests), bin 4 (+3 history/sparkline/
  metric-report tests), integration 9.
- `cargo test --workspace`: 489 passed / 0 failed (game crate untouched;
  no change to game render paths).
- Piped `animate` frames: method rows are 132 visible columns exactly
  (98 + 2 gap + 32 sparkline); the history grows one column per frame,
  right-aligned, and the shared scale legend tracks the running max
  (max=0.000 → 0.049 → 0.053 across six orbit frames). Frame-metric
  semantics (n/a before the first prev, pane ≡ number over the window
  crop, growth with displacement) are covered by unit tests.
- Other modes (pos / families / sweep) smoke-tested — unchanged.
- Surfaced along the way (pre-existing, NOT from this change): a fresh
  full recompile emits a `private_bounds` warning for the private
  `PixelStats` in `pub fn ClassGrid::full_pane`'s signature (coverage.rs),
  which stale incremental-compilation warning caches had been hiding —
  left for roadmap item 2's warning cleanup.

## Follow-up: five-row graph + in-use baseline overlay — 2026-09-07

User request: more vertical resolution for the history graph (5 text
rows → 40 levels against the shared scale), and show where the in-use
(baseline) method's errors sit by overlaying a 1/8-tall horizontal-line
character positioned accordingly.

Research (user-requested): the marker chars are U+1FB76..=U+1FB7B,
HORIZONTAL ONE EIGHTH BLOCK-2..-7 (Unicode 13.0, Symbols for Legacy
Computing — the same U+1FB00–1FBFF chart as the sextants/right-blocks
the renderer already emits, so no new font requirement). The official
suffix counts from the TOP (the family's -1/-8 ends are ▔ U+2594 /
▁ U+2581), so in bottom-up order the six new chars appear in DESCENDING
code-point order: 1→▁ 2→🭻 3→🭺 4→🭹 5→🭸 6→🭷 7→🭶 8→▔. Verified three
ways: the official names list (user paste of all six lines), the chart's
`→ 2594 ▔` cross-reference, and pixel measurement of the rendered block
chart (calibrated by BLOCK SEXTANT-1 = top-left fill, matching the repo's
own hextant bit model). Guard test pins the order.

Landed: `SPARKLINE_ROWS = 5`, level = round(v/scale·40) clamped 0..=40;
bars (█ rows + bottom-up eighth-block tops), '·' zero-axis row, spaces
above; baseline overlay on the candidate row only (the in-use row IS
the baseline), boundary-exact levels draw ▔ on the row seam, level 0 is
▁ on the axis, level 40 is ▔ at the graph top; candidate legend names
the marker (🭸=in-use). Column width unchanged (32) — method rows stay
132 visible columns; the graph grows the column from 3 to 7 lines, still
shorter than the zoom column. Applied with two corrections to the
presented diff: the v=scale test assertion is unconditional '█' (level
40 reaches the top row at exactly p=8), and the SPARKLINE_LEN doc now
says "graph column" (a frame is a column, not a single block).

Verification: bin tests 4 → 6 (level test replaced; baseline landing +
family-order guard added); `cargo test -p terminal_rendering` 156 / 0
(lib 141, bin 6, integration 9); workspace 491 / 0. Piped `animate 8`:
width still 132; in-use row = bars only; candidate row = bars + 8
baseline marks (family chars, cyan), floating above bars and on the
axis; legends "0 ▁▂▃▄▅▆▇█ max=…" / "🭸=in-use ▁▂▃▄▅▆▇█ max=…".

## Follow-up: marker priority rules — the baseline never covers the graph — 2026-09-07

User feedback after eyeballing piped frames: the baseline marker replaced
the bar char in its cell, so a line inside the bar CUT it (`████🭻██`
read as a hole in the bar). New rules per column (line level L_l vs bar
level L_b), via `MarkerMode` + `GRAPH_COLOR` (the bars' gray, also the
OnGraph cell background):

- inside the bar (L_l < L_b, including L_l = 0 under a bar): the line
  char rides the graph color as its cell background — the bar continues
  behind the marker, solid rows render seamlessly;
- same character as the bar's top (L_l == L_b > 0): the graph wins — the
  bar's top block is drawn, but in the marker (cyan) color, flagging the
  coincidence instead of silently dropping the marker;
- above the graph (L_l > L_b, or on the zero axis): floats on the default
  background, as before.

Known accepted costs (the literal reading of the rules): a line inside
the bar's TOP row below its edge fills that cell with the graph color
(the bar's partial edge within the row is subsumed); a line in the bar's
top row above its edge replaces the bar's tip char (the bar reads as
topping out at the row below).

Verification: bin tests 6 → 7 (`baseline_marker_never_covers_the_graph`:
mode table, coincidence coloring, ride-the-graph-color escape pattern,
float-leaves-the-tip-intact). `cargo test -p terminal_rendering` 157 / 0;
workspace 492 / 0. Piped `animate 10` mode distribution: 41 Coincides
(cyan bar tops, ~4 per frame — frequent and visible), 4 Float, 10 axis
markers, 0 OnGraph occurrences in this orbit/metric (that mode is pinned
by the unit test's escape assertion); no cut cells. Two test-side slips
during application, both mine: level 20 is position 4 (🭹, not 🭸), and
char-position assertions must use the plain style since escape codes
count as chars.
