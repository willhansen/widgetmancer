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

### Metric panes (col 3, cycled with `,` / `.`)

- **center** — silhouette + ideal outline + both centroids
- **area** — signed: over red / under blue
- **per-char** — coverage, half-cells heat-shaded by local error
- **xor** — ideal-square xor, any mismatch lit
- **jagged** — contour lit by local edge-step length
- **disp** — displacement sensitivity: what turns wrong under the worst
  1/16 nudge (bright yellow = newly wrong)
- **frame** — frame diff: which samples changed against the previous
  rendered frame (motion pops light up; n/a until one frame old)

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
