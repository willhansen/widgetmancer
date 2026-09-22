# Floating square debug tool

Visual debugger for floating-square rendering: renders the same glyph picks
the game uses (one world square = 2 terminal columns) on a checkerboard of
square centers, plus sampled actual-vs-ideal coverage panes and per-method
error metrics. All diagnostics go through the sampled-coverage oracle in
`terminal_rendering` (shared with the `floating_square_coherence` test), so
the tool cannot drift from what the game draws.

## Run

From the repo root:

    ./floating-square-debug/debug-floating-squares

or directly:

    cargo run -p floating_square_debug -- <mode>

## Modes

- `pos X Y` — one square: snap-family diagnostics, true-center marker, and
  the sampled actual-vs-ideal coverage zoom (the coherence test's oracle).
- `families X Y` — the same position with each snap family forced, side by
  side; explains why the automatic pick won.
- `sweep` — offset table over 0..=0.5 in 1/16 steps, each cell labeled with
  its auto-picked family: a decision-boundary map.
- `glyphs` — reference table: every block character the renderer can emit
  with an exact big-pixel zoom. Plain text; redirect to a file.
- `animate` (default) — square on the alternate screen (q quits): orbit,
  arrow-key nudge, line trajectories; two-method comparison with cycled
  candidates and error panes.

## Layout

- `src/main.rs` — the tool itself.
- On-screen UI hierarchy: see UI-LAYOUT.md.
- The sampled-coverage oracle (`terminal_rendering::coverage`) stays in
  `crates/terminal_rendering` because the coherence tests there share it.

## Docs

- Vision: `docs/vision/floating-square-debug-tool.md`
- Rendering background: `docs/FLOATING_BLOCKS.md`
- History: `docs/checkpoints/floating-square-*.md`
