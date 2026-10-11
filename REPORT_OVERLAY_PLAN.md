# Issue-report overlay — resumption plan

Status: planning (2026-10-11). Supersedes the phase-2 PTY editor pane.

Related: `issues/0021/` (feature request), `issues/0020/` (deferred portal-leak
bug), commits `d1d4265` (phase 1: overlay + flagging + filing), `6f57d1e`
(phase 2: PTY editor pane), `f9444a5` (removed the PTY pane for a built-in
editor).

## Goal

Make the in-game report overlay (`Ctrl+P`) pleasant to use: no flicker, a
re-centered game view, live drag feedback, tinted marks, and numbered labels.

## The model (from the reporter)

On `Ctrl+P`, freeze and capture the rendered frame as a **static image**. While
the report is open, draw *that image* repositioned into the non-editor region
(so the player is centered there), and **record the reposition** so mouse input
still maps back to the real frame cells / world squares. The live game is never
re-rendered while the overlay is open.

## Current state

- `crates/game/src/report.rs`: overlay session, square flagging, an in-memory
  multi-line description buffer, and `finish_issue` -> `issue.md` +
  `flags.json` + the un-altered `snapshot/screen.txt`.
- Driver wiring in `crates/game/src/lib.rs`; `report` feature (default-on,
  implies `debug-tools`).
- The overlay draws **directly to the terminal** (never into `screen_buffer`),
  which is what keeps `snapshot/screen.txt` un-altered and `verify-issues`
  green.
- `screen_buffer` is `Vec<Vec<Glyph>>` indexed `[x][y]`; `Glyph` is
  `Copy + PartialEq + Eq`. `tint_color(original, tint, strength)` is in
  `crates/utility/src/lib.rs`.

## Diagnosis

1. **Flicker on typing.** `draw` restores every cell it previously wrote
   (re-emitting the frame there) and then redraws the whole editor region on
   each keystroke — a restore→redraw flash plus a full-pane rewrite.
2. **Play area not re-centered.** Nothing repositions the frame; the game
   renders full-screen and the editor overlays a third, so the player stays at
   the full-screen center, not the game region's center.
3. **Drag lag.** No feedback during the drag, and each `Hold` repaints the whole
   overlay, so queued motion events back up before `Release`.
4. **No drag rectangle.** `Hold` only moves the keyboard cursor.
5. **Marking replaces the cell.** `paint_square` overwrites `bg_color`, hiding
   the underlying glyph instead of tinting it.
6. **No labels.** Never rendered.

## Plan

### A. Static frame + reposition (fixes 2)

- On open, capture `frame: Vec<Vec<Glyph>>` = `screen.screen_buffer` clone
  (indexed `[x][y]`).
- Compute `offset` (in terminal cells) so the player's frame cell lands at the
  game-region center: shift by half the editor's width (side-by-side layout) or
  half its height (stacked layout) toward the game region. The player is at the
  frame center, so e.g. `dx = -(editor_width / 2)`, `dy = 0` for side-by-side.
- Blit the frame into the game region at `offset`, clipped; the editor occupies
  the rest. Game region = complement of the editor rect (aspect-ratio split, as
  today).
- Mouse mapping: `frame_cell = term_cell - offset`; then
  `screen.screen_buffer_square_to_world_square(frame_cell / 2, frame_cell.y)`.
  Keyboard cursor likewise maps through the offset.
- On close, `force_redraw` restores the full-screen game.

### B. Cell-diff drawing (fixes 1 and 3)

- Keep `last: HashMap<(u16, u16), Glyph>` of the overlay cells from the last
  draw.
- Each dirty draw, build the desired overlay (editor + flags + labels + drag
  rect + help), then:
  - write only cells whose glyph differs from `last`;
  - for cells in `last` but not desired, restore the underlying glyph from the
    (repositioned) frame, or black if outside it.
- Blit the static frame once (on open / on offset change), not every keystroke.
- Result: typing touches ~a few cells; no full-pane rewrite; no flicker.

### C. Tinted marking (fixes 5)

- Instead of replacing `bg_color`, blend
  `tint_color(glyph.fg_color, FLAG_TINT, s)` and
  `tint_color(glyph.bg_color, FLAG_TINT, s)` so the underlying glyph shows
  through. Proposed `FLAG_TINT` = magenta, `s` ~ 0.45; drag preview a distinct
  color.

### D. Numbered labels (fixes 6)

- Draw the area's 1-based index just above it (one row up, clamped to the top
  edge). Label color distinct from the tint.

### E. Drag rectangle (fixes 4)

- Track `drag_anchor` + `drag_current` (world squares). While dragging, render
  the rectangle outline (tinted) plus the tentative region and its label, so
  feedback is immediate.

### F. Motion coalescing (fixes 3)

- When draining events in the driver loop, keep only the latest mouse-motion
  position (drop intermediate `Hold`s) so the queue can't back up before
  `Release`. (termion enables `?1000`/`?1002`/`?1006`, so press/release and
  drag-motion are reported.)

## Open questions (unanswered — ask before/while implementing)

1. **Selected-area model.** Is a drag one numbered *region* (one label above the
   box, one entry), or does each square get its own number? Lean: region-per-drag,
   a click is a 1x1 region.
2. **Region summary.** For a box, does `issue.md`/`flags.json` record the bounds
   plus the `explain` of the region's center square, or an `explain` per square?
3. **Frame reposition style.** Translate + clip (frame stays full size; edges
   under/off the game region), or scale the frame down to fit the game region
   whole? Lean: translate + clip (matches "repositioned").
4. **Tint color.** Confirm magenta (~0.45) for selected areas and the drag
   preview color.

## Files to touch

- `crates/game/src/report.rs` — frame capture + offset, diff drawing, tinted
  marks, labels, drag rectangle, region model (pending Q1/Q2).
- `crates/game/src/lib.rs` — mouse-motion coalescing; pass the editor layout to
  the session (or keep it in the session).
- `docs/ARCHITECTURE.md`, `issues/0021/issue.md`, `docs/CHANGELOG.md` — update.

## Verification

- Unit tests: frame-offset math; diff writes only changed cells (idle draw
  emits nothing — already covered by `session_draw_is_dirty_gated`); tint does
  not replace the underlying glyph; label placement; drag-rect rendering; mouse
  -> world mapping through the offset.
- `./run-tests`; `./snapshot-tool diff snapshot/`; `./snapshot-tool
  verify-issues` (must stay byte-identical — the overlay never touches
  `screen_buffer`).
- Manual pass: `cargo run -p game -- --map cubes`, `Ctrl+P`, drag a box, type,
  `Tab`, `Enter`.
