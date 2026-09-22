# Floating square debug tool — UX fixes (errors, size warning, drag hint, presets, metric help)

**Status: COMPLETE — 2026-09-22.** Five ranked UX fixes from an evaluation
of the tool against its "single developer building trust in rendering
decisions" job. One commit each, in dependency order.

## User request

Evaluate the floating square debug tool's user experience, then implement
the top five fixes:

1. No terminal-size detection — the fixed ~132×86 layout wrapped and the
   hardcoded mouse→world anchor silently misread clicks on smaller
   terminals.
2. Interpretation was doc-dependent — nothing on screen explained the
   seven error metrics.
3. Mouse semantics (left-drag = orbit angle, mid/right = place, global
   effect regardless of pane) were only discoverable via a 117-char
   abbrevi-hint.
4. No reset for session counters and no way to jump to interesting
   positions without quitting to the CLI.
5. Error handling: `pos nan nan` rendered garbage frames; terminal-setup
   failures panicked on `unwrap()`.

## What landed

### Fix 5 — arg + terminal-setup errors (b708bf6)

`parse_xy` became a `Result` function (present, numeric, **finite** — bare
`f32::parse` accepts NaN/inf); each rejection names its mode (`pos: X and
Y must be finite (got NaN, NaN)`) and exits 2 with usage. `run_animation`
returns `Result<(), String>`: raw-mode/alternate-screen failures map to
messages naming `$TERM` and exit 1 via `die` (no usage dump — not the
arguments' fault).

### Fix 1 — measured size warning + mouse-anchor correction (b5026dc)

- `probe_frame()` renders one frame from a fresh default state into a
  buffer; `animate_min_size()` measures its widest line and line count
  (+1 for the hint row). Measuring instead of hardcoding means the number
  tracks the layout automatically.
- When `terminal_size()` reports less than that, the bottom hint line is
  **replaced** by a warning naming both sizes (a row swap, not an added
  row — no scroll churn); re-checked per redraw, so enlarging the terminal
  restores the hint. An unreadable size is skipped.
- **Real bug found and fixed:** `GRID_SCREEN_ORIGIN` was `(10, 2)` with a
  comment describing a removed layout ("first column is the 6-wide small
  view"). The large grid is now the *first* boxed column: measured at
  **col 3, row 2**. Every mouse placement/angle click had been offset by
  7 terminal columns = **3.5 world units**.
- Drift guard: `grid_screen_origin_matches_the_rendered_frame` renders a
  probe frame and asserts the first checkerboard mark sits at the
  constant's column (char position, not byte offset — the glyphs are
  multi-byte UTF-8; `str::find`'s byte index was the first draft's bug).

### Fix 3 — contextual drag hint (22868db)

`hint_line(&state)`: while a drag is active, the bottom line names the
drag's mode *and its alternative* ("drag: setting orbit angle
(mid/right-drag places the square)"; coarse/fine place variants name
their toggles) — the one moment attention is on mouse semantics, so the
other button teaches itself. No-drag shows the full key summary. Left
drag stays angle-steering (user decision).

### Fix 4 — preset jumps + reset (4e2c3bd)

- `PRESETS[0..=9]` (const — `Point2D::new` is `const fn` in euclid
  0.22.11): boundary-heavy offsets (quarter interior, half-cell corner,
  1/16 finest offset, eighth boundaries, mixed boundary, negative half,
  cell crossing, three-quarter corner) plus origin center and the
  roadmap #9 tear corner (2.363, −0.816). Digit keys jump **paused**
  (same contract as mouse placement); the global-state column shows
  `preset: <name>`, cleared by any manual steering (arrows/o/l/mouse).
- `r` → `AnimState::reset_counters()`: clears histories, `switches=`,
  preset label, `last_sample_pos` — but **not** `prev_renders`, because
  frame diff is defined against the immediately previous render; clearing
  it would fake an `n/a` gap.

### Fix 2 — `?` metric explainer box (c807c7d)

`METRIC_HELP`: one aligned line per metric, condensed from UI-LAYOUT.md's
per-metric "measures/why" sections. `?` toggles a `boxed_row("metric
help (? hides)")` below the common row; the selected metric is prefixed
`> ` and follows `,`/`.` cycling. A drift test pins METRIC_HELP to METRICS
(length + name prefix); a render test pins that the box appears and marks
the selection.

## Tests (15 total, all in-file mods)

- `arg_tests::parse_xy_accepts_finite_pairs_rejects_the_rest`
- `layout_tests::grid_screen_origin_matches_the_rendered_frame`
- `layout_tests::animate_min_size_exceeds_a_default_terminal`
- `hint_tests::hint_line_follows_drag_context`
- `preset_tests::presets_are_finite_and_on_the_grid`
- `preset_tests::reset_counters_clears_history_but_keeps_prev_renders`
- `help_tests::metric_help_covers_every_metric`
- `help_tests::help_box_renders_and_marks_the_selected_metric`
- (plus the 7 pre-existing glyph/history/metric tests, unchanged)

Run: `cargo test -p floating_square_debug`

## Decisions recorded

- Warning **replaces** the hint line (user choice: warn, don't exit) —
  no added row, no scroll churn, self-clears on enlarge.
- Left-drag stays angle-steering (user choice: keep semantics, live hint
  only).
- Presets are curated, not savable (user choice: simpler; CLI `pos X Y`
  covers arbitrary coordinates).
- `refactor`: `visible_width` now delegates to a new `strip_ansi` (same
  scan logic, reused by the origin test and probes).

## Known limits

- On a terminal that fits exactly, toggling `?` adds ~12 rows and scrolls
  the top box off — transient, user-requested, not worth adaptive layout.
- The banner can't be exercised in this sandbox (no tty); its inputs
  (`animate_min_size`, the anchor) are covered by tests instead.
