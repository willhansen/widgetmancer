# Floating square debug tool

Visual debugger for floating-square rendering: renders the same glyph picks
the game uses (one world square = 2 terminal columns) on a checkerboard of
square centers, plus sampled actual-vs-ideal coverage panes and per-method
error metrics. All diagnostics go through the sampled-coverage oracle in
`terminal_rendering` (shared with the `floating_square_coherence` test), so
the tool cannot drift from what the game draws. The `pixels` mode is the
exception by design: it rasterizes a real font file, for glyphs the analytic
oracle does not model.

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
- `pixels [--font P] [--size N] <chars…>` — the *actual* pixels of arbitrary
  characters, rasterized from a real font file (`--font`, else `$GLYPH_FONT`,
  else a system font), with an `Emoji_Presentation` warning and, when the
  oracle models the glyph, the analytic view beside it. This is how to see
  glyphs the oracle doesn't know (geometric shapes such as `⬤ ● • ·`) and to
  catch codepoints that default to the color-emoji font. Args may be single
  characters, runs, or `U+25FE` / `0x25FE` code points.
- `which-font [--dir D]… <chars…>` — scan the system font directories (or
  `--dir`s) and list which fonts contain the requested characters, sorted by
  coverage. Use it to find the terminal's fallback for a glyph the configured
  font lacks; on Linux `fc-match -s ':charset=<U+XXXX>'` gives the OS's first
  pick.
- `animate` (default) — square on the alternate screen (q quits): orbit,
  arrow-key nudge, line trajectories, preset jumps (`0`–`9`, paused — the
  roadmap's tear corner among them), `r` to reset histories and switches,
  `?` for an on-screen metric explainer; two-method comparison with cycled
  candidates and error panes. The hint line names the active drag's mode
  and alternative while dragging. Needs a large terminal (measured at
  runtime, ~132×87); smaller ones get a size warning in place of the key
  hint.

## Layout

- `src/main.rs` — the tool itself.
- On-screen UI hierarchy: see UI-LAYOUT.md.
- The sampled-coverage oracle (`terminal_rendering::coverage`) stays in
  `crates/terminal_rendering` because the coherence tests there share it.

## Docs

- Vision: `docs/vision/floating-square-debug-tool.md`
- Rendering background: `docs/FLOATING_BLOCKS.md`
