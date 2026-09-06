# Glyph table subcommand (`floating_square_debug glyphs`) — 2026-09

## Context

The vision doc (docs/vision/floating-square-debug-tool.md) specifies the
zoomed-in scale's big pixels as vertical half characters, with the actual
zoomed view mapping the smallest rendering increment to one big pixel.
Spelling out the glyph vocabulary's edge lattice (see the
"exact vs 24x24" discussion that preceded this):

- x: h-eighths strips cut at 1/8 character = 1/16 world; hextant/quadrant
  columns at 1/4 world; v-eighths at half-cell boundaries. Union: 1/16.
- y: v-eighths strips at 1/8 world = 3/24; hextant rows at 1/3 world =
  8/24; quadrants at 1/2. Union: 1/24 (tightest gap 5/8 ↔ 2/3 = 1/24,
  the vision doc's own example).

So one character cell (0.5 x 1.0 world) is exactly 8 x 24 big pixels on
the union lattice, and every glyph edge lands exactly on a pixel
boundary — this is the same lattice as coverage.rs's sample grid
(SX=16, SY=24), by construction.

## What landed

`glyphs` subcommand on floating_square_debug (also via the top-level
./debug-floating-squares wrapper): prints a plain-text reference table to
stdout — one entry per block character the renderer can emit (first
column: the character; second column: its exact 8x24 big-pixel zoom,
framed in box drawing characters; one big pixel = one vertical half
character: both=█ upper=▀ lower=▄ empty=·). Redirect to a file for the
generated artifact; no separate script.

- Glyph set is enumerated by sweeping the four family generators
  (1d-eighths, vertical-thirds, quadrant_block_by_offset,
  hextant_block_by_offset) over their full input domains, deduped,
  SPACE dropped — it cannot drift from the render vocabulary because it
  calls the same functions the renderer does. 45 glyphs total.
- Notable dedup: the third-block constants (🬂 🬎 🬭 🬹) are the same
  codepoints as the full-width sextants (both-columns row strips), so the
  hextant sweep contributes 8 (not 12) new glyphs. Halves (▌▐▄▀) dedup
  into the eighth-strip sweep.
- Fill decisions use coverage::glyph_filled at pixel centers — exact,
  because pixel edges align with every glyph edge (no rounding, no
  coverage lerp; contrast glyph_pane, which anti-aliases).
- Plain text only (no ANSI) so the output files cleanly.

Verified: ▍ renders as exactly 3 of 8 columns; 🬀 as left half of
exactly the top third; 🬦 as right half of the bottom two-thirds. All
workspace tests pass (481 total).

## Open (separate decision)

The zoom panes (glyph_pane / ideal_pane / error panes) still run at
24x24 (1/8-world pixels) with glyph_pane anti-aliasing edges. Migrating
them to this exact 48x72-per-window union lattice (one big pixel per
sample) is pending: it buys exactness but costs a ~1.5x vertical stretch
(big pixels are visually square half-chars but cover 1/16 x 1/24 world).
The glyph table doubles as an eyeball test of that stretch.
