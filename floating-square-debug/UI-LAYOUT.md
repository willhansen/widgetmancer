# UI layout — floating square debug tool

On-screen hierarchy of the tool's terminal UI. For how to run the tool and
what it's for, see `README.md`; this document describes what you see and
where each panel is built in `src/main.rs`.

Everything drawn derives from the sampled-coverage oracle in
`terminal_rendering::coverage` (shared with the `floating_square_coherence`
test), so the tool cannot drift from what the game draws.

## Conventions

- Every panel is a titled box-drawing frame built by `boxed_row`
  (`main.rs:317`): a `┌─ title ─┐` style frame around a row of columns.
- Columns inside a box are separated by a 2-space gutter; each column is a
  list of equal-width lines.
- **Big-pixel lattice**: the shared zoom coordinate system — one "big
  pixel" = 1/16 × 1/24 world units; coverage zoom and error panes are
  `BIG_PX_W` columns of big pixels. One world square = 2 terminal
  columns, so an 8×24-pixel block character zooms to exactly one big
  pixel per pixel.
- Snap-family legend (`FAMILY_COLORS` / `FAMILY_LETTERS`,
  `main.rs:108-114`): **H** horizontal eighths (yellow), **V** vertical
  eighths (cyan), **X** hextant (green), **Q** quadrant (magenta).
- Error metrics (`METRICS`, `main.rs:348-359`), cycled with `,` and `.`:
  center, area, per-char, xor, jagged, disp, frame (details below).

## `animate` mode (default, interactive)

Whole screen is a vertical stack of titled boxes, composed by
`render_animation_frame` (`main.rs:1419`):

```
screen  (alternate screen + raw mode + mouse; run_animation, main.rs:1611)
├── box "in use: family-snapped"             (method_section, :1502)
├── box "candidate: <name>  ([ ] cycle)"      (method_section, :1520)
│      └─ interact: the "([ ] cycle)" in the title is an affordance
│         label — the actual input is the [ / ] keys
├── box "common"                              (boxed_row,     :1578)
└── bottom hint line                          (:1797-1801)
       └─ interact: display-only key summary; q and esc quit anywhere
```

The two method rows ("in use" = the game's real render path; "candidate" =
a replacement method cycled with `[` / `]`) share the same four-column
layout, built by `method_section` (`main.rs:709-780`). `interact:` notes
say how you can affect each element:

```
box "in use: …" / "candidate: …"
├── col 1: real-size grid
│   ├── checkerboard background with a dot at each square center
│   │   (grid_frame, :136) — sub-square offsets are eyeball-able
│   ├── drawn neighborhood (radius 4): the actual glyphs at real size
│   │   interact: this is the square you steer — arrows nudge it by
│   │   1/16, o/l switch trajectory, mouse drags (screen-wide, see
│   │   below) place/steer it on this grid
│   ├── method info lines — in-use row: family (inverted on switch
│   │   frames, where visible pops happen) + snap error; candidate
│   │   row: its objective ("per-cell xor + 1.0·d", etc.)
│   │   interact: display-only; the family line updates as you steer
│   │   the square across snap boundaries
├── col 2: big-pixel zoom pane
│   ├── the render as union-lattice big pixels (oracle big_pixel_pane)
│   └── glyph-color legend (one palette color per glyph)
│       interact: display-only; follows the square's position
├── col 3: selected error-metric pane
│   ├── the error drawn as colors on the same big-pixel lattice —
│   │   one big pixel per sample, the grid the numbers measure
│   └── the numeric value (metric_report, :434)
│       interact: `,` / `.` swap which metric this pane shows
│       (both method rows change together)
└── col 4: error-history sparkline (sparkline_graph, :608)
    ├── 5 rows × 32 columns, one column per frame, shared y-axis
    │   (max over all four method slots — block heights stay
    │   comparable across rows and candidate switches)
    ├── sampled only on frames where the square actually moves
    │   (manual mouse movement counts)
    │   interact: indirectly live — any square movement you cause
    │   (keys or mouse) appends a column here
    └── candidate row only: marks each frame's in-use error with a
        1/8-tall horizontal-line block character, drawn so it never
        covers the graph (rides the bar color inside, floats above)
        interact: display-only
```

The `common` box (`main.rs:1544-1586`) has three columns:

```
box "common"
├── col 1: "ideal (true square)" — the ideal zoom drawn analytically
│   (ideal_big_pixel_pane), width BIG_PX_W
│   interact: display-only; the reference both rows are judged against
├── col 2: global state — pos / frac, motion name, speed, family
│   switches, elapsed time, [paused] / [fine-drag] flags
│   interact: display-only; mirrors your input live (space toggles
│   [paused], +/- change speed=, f or a held modifier shows
│   [fine-drag], and every family crossing you steer past increments
│   switches=)
└── col 3: controls legend (the key list below)
    interact: display-only — a reminder of the keys, not clickable
    text; the terminal has no clickable regions
```

### Controls (event loop, `main.rs:1644-1784`)

| Input | Action |
| --- | --- |
| `q` / `esc` | quit |
| `space` | pause / resume |
| arrows | nudge the square by 1/16 |
| `o` | orbit trajectory |
| `l` | line trajectory |
| `+` / `-` | animation speed |
| `f` | toggle fine-drag mode |
| `[` / `]` | cycle candidate method |
| `,` / `.` | cycle error metric |
| left drag | set orbit angle (angle from the grid center to the mouse) |
| mid/right drag | place the square (drag to move) |
| shift/ctrl/alt-drag | fine placement (large mouse moves → sub-cell moves) |

**How mouse input maps to the UI.** There are no clickable regions: mouse
coordinates are converted from terminal cells to world-grid coordinates
(`mouse_cell_point` / `mouse_cell_angle`) and act on the square wherever
the pointer is on screen — hovering a pane does not target that pane.
Placement is clamped to the visible animation grid's bounds (radius 4 +
half a square). Any mouse press also pauses, so a placement sticks; drag
mode is chosen by button (left = angle, others = place) and is locked
until release. A held shift/ctrl/alt (parsed from the raw SGR mouse
sequence, since termion drops the modifier bits) or fine-drag mode makes
placement relative: cell deltas accumulate at `FINE_SCALE`, so large
mouse moves produce sub-cell movements.

The screen only redraws on a state change (the `dirty` flag), not every
frame tick — full-screen repaints every 33 ms would wipe any in-progress
terminal text selection.

Interaction state lives in `AnimState` (`main.rs:1257`) with
`DragMode::Angle | Place` (`main.rs:1250`). All four method slots get a
metric report every frame so history stays continuous across candidate
switches; the frame-diff metric compares against the immediately
previous render (`main.rs:1589-1594`).

### The error metrics (col 3, cycled with `,` / `.`)

All seven metrics measure the same render on the same **sample lattice**:
48 × 72 sample points over the 3×3-world render window (16 × 24 per world
square, at half-sample offsets so samples never coincide with half/third/
eighth glyph boundaries — coverage never aliases). Every number divides
by `SX * SY = 384`, one square's sample count, so values are in **world
square units**. The colored pane is the exact same grid (a 2×2-world
crop, one big pixel per lattice sample), so each pane shows pixel-for-
pixel what its number counts — not a downsampled view.

The value line under each pane also feeds the col-4 sparkline, with
history scalars `center` = Euclidean distance and `area` = |signed|
(`metric_report` doc, main.rs:440-447).

#### `center` — silhouette position

- **Measures** how far the rendered silhouette's actual middle (centroid
  of its filled samples) sits from the true square's center.
- **How** unweighted mean of filled-sample positions over the full
  lattice (`fill_centroid`, coverage.rs:781); value = per-axis offset,
  history scalar = ‖centroid − pos‖.
- **Displayed** `({:+.2}, {:+.2})` signed x/y; pane: dim silhouette,
  grey ideal outline, '×' actual vs '+' ideal centroid marks
  (coverage.rs:1130-1185).
- **Why** catches lopsided glyph picks — plausible ink distribution
  that's nevertheless off-center, which xor can trade off against
  shape error.

#### `area` — ink amount, signed

- **Measures** rendered area minus ideal area (ideal = 1 square),
  signed: positive = bloated, negative = shrunken.
- **How** `(over − under) / 384` over the full lattice — over =
  rendered-filled/ideal-empty samples, under the reverse
  (`signed_area_error`, coverage.rs:1095).
- **Displayed** `{:+.3}`; pane: over-coverage red, under-coverage blue
  (coverage.rs:1115-1128).
- **Why** the signed catch-all for systematic over/undershoot; xor
  alone can't tell which direction. Deviations past ~0.23 exceed any
  single family's quantization error — genuine failure, not rounding.

#### `per-char` — per-half-cell ink amount

- **Measures** for each of the 18 character half-cells, |rendered
  glyph fill − ideal fill| within that half-cell, summed.
- **How** each half-cell sampled on its own 8×24 = 192-point lattice;
  `Σ |rendered − ideal| / 192` (`per_char_coverage_error`,
  coverage.rs:635-660). Coarser than xor: only the right *amount* of
  ink per cell, regardless of where it sits.
- **Displayed** `{:.3}`; pane: each half-cell heat-shaded dark → hot
  amber at 0.25 of the cell mis-inked (coverage.rs:1187-1225).
- **Why** localizes *which* cells pick badly, and separates "right ink
  in the wrong spot" from real area error — charwise candidates score
  well here even when jagged.

#### `xor` — shape fidelity (the objective)

- **Measures** symmetric difference between render and the true 1×1
  square.
- **How** mismatched-sample count over the lattice, `/ 384`
  (`coverage_error`, coverage.rs:537). **This is the objective the
  snap-family map was baked against** and the coherence test asserts
  on.
- **Displayed** `{:.3}`; pane: any mismatched sample lights orange
  (coverage.rs:1111).
- **Why** the ground-truth single number for shape fidelity — but
  sample-by-sample strictness means it says nothing about edge
  smoothness (jagged) or nearby cliffs (disp).

#### `jagged` — staircase edges

- **Measures** total variation of the silhouette's four edge contours.
- **How** along each sample column/row, the perpendicular step between
  consecutive filled extents, Σ|steps| in world units; a clean
  rectangle measures 0 (`jaggedness`, coverage.rs:662-697).
- **Displayed** `{:.2}`; pane: dim silhouette with contour pixels lit
  by local step size — dark olive (straight) → bright green (a jump of
  1/8 world is full-scale) (coverage.rs:1227-1298).
- **Why** stair-stepping is the visual artifact charwise methods
  produce by design; the family-snapped path's main payoff is straight
  edges (jagged = 0).

#### `disp` — pop sensitivity

- **Measures** how much the method's own xor grows under the worst
  small nudge of the square.
- **How** re-evaluate `coverage_error` at pos ± 1/16 on each of the 4
  axes (`DISPLACEMENT_DELTA` = 1/16, matching the finest snap grid and
  the arrow-key nudge); value = max gain + worst direction
  (`displacement_sensitivity`, coverage.rs:1570-1595).
- **Displayed** `{:.3}` plus arrow (→ ← ↑ ↓); pane: the shifted render
  re-sampled on the base frame's origin so they align sample-for-
  sample — bright yellow = newly wrong (the pop), dim red = still
  wrong, dim blue = recovered (coverage.rs:1300-1320).
- **Why** glyph picks are piecewise-constant: a render can sit at zero
  xor and still be one pick-boundary away from a large visible pop.
  This finds that boundary and shows which samples would pop.

#### `frame` — rendered change per frame (temporal)

- **Measures** how much the rendered fill changed against the
  immediately previous frame — vs. the previous *render*, not the
  ideal.
- **How** differing-sample count between the two frames' fills on the
  current lattice, `/ 384` — same denominator as xor, so directly
  comparable (`frame_diff_xor`, coverage.rs:699-731). n/a until one
  frame old (sparkline skips it).
- **Displayed** `{:.3}` / `n/a`; pane: every changed sample lights
  orange, same window crop as the other panes (coverage.rs:1323-1355).
- **Why** under identical motion, the method whose frame diff stays
  low looks smoother on screen — the metric for comparing candidates
  during animation, not at a static position. Scales with motion
  speed, so it's a comparison tool, not an absolute smoothness score.

## Non-interactive modes

All four modes below are read-only snapshots: no keyboard or mouse
interaction, argument positions chosen once on the command line.

### `pos X Y` (`main.rs:916`)

```
pos output
├── snap-family diagnostics header
├── framed real-size grid with a true-center marker
└── sampled actual-vs-ideal coverage zoom (the coherence test's oracle)
```

### `families X Y` (`main.rs:934`)

```
families output
└── four side-by-side panes, one per snap family forced
    (auto-picked winner marked — explains why the pick won)
```

### `sweep` (`main.rs:987`)

```
sweep output
└── 9×9 offset table over 0..=0.5 in 1/16 steps
    (each cell a mini-grid labeled with its auto-picked family
     letter H/V/X/Q — reads as a decision-boundary map)
```

### `glyphs` (`main.rs:1088`)

```
glyphs output
└── reference table: every block character the renderer can emit,
    with its Unicode name and an exact big-pixel zoom
    (8×24 pixels per character, framed with position rulers)
```

Plain text — redirect to a file.

## Code pointer map

Line numbers refer to `src/main.rs` at time of writing and may drift.

| What | Function | Line |
| --- | --- | ---: |
| sampling lattice / denominators | `SX`, `SY`, `NX`, `NY` | coverage.rs:25-33 |
| titled box container | `boxed_row` | 317 |
| checkerboard grid frame | `grid_frame` | 136 |
| method rows / metric list | `IN_USE`, `CANDIDATES`, `METRICS` | 348 |
| error pane + value | `metric_report` | 434 |
| history sparkline | `sparkline_graph` | 608 |
| method row layout (4 columns) | `method_section` | 709 |
| interaction state | `DragMode`, `AnimState` | 1250 |
| full-screen composer | `render_animation_frame` | 1419 |
| event loop, alt screen | `run_animation` | 1611 |
| mode dispatch | `main` | 1851 |
