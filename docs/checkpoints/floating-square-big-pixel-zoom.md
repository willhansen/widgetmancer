# Animate-mode zoom at the union lattice (`big_pixel_pane`) — 2026-09

## Follow-up: error panes on the same grid — 2026-09

The err-column metric panes (center / area / per-char / xor / jagged /
disp, cycled with , and .) moved off the old 24x24 display grid
(2x3 samples per pixel over the 3x3 window) onto the zooms' 32x48
big-pixel grid: one big pixel per lattice sample over the 2x2-world
window, composed via `big_pane_from_colors`. Key fact that made this
safe: all numeric metrics (coverage_error, xor_error, signed_area_error,
jaggedness, per-char, displacement gain) were already computed on the
native NX x NY sample grid — only the panes downsampled. So the panes
now show pixel-for-pixel what the numbers measure.

Landed (coverage.rs):
- `ClassGrid::full_pane` builds per-sample `PixelStats` over the window
  crop (`sample_at`; the old 2x3 `pixel` aggregation deleted);
  `mismatch_pane`/`signed_pane` unchanged logic per sample (the split-
  pixel XOR_COLOR case is structurally impossible now and was removed);
- `center_pane`: ideal outline becomes 1-sample-thick adjacency (inside
  sample with an outside 4-neighbor) since the 2x3 straddle rule is
  gone; centroid marks still claim a whole text cell;
- `jaggedness_pane`/`displacement_pane`: per-sample; contour steps use
  neighboring samples that may sit just outside the 2x2 window but
  inside the sampled 3x3 grid, so no edge information is lost at the
  crop (out-of-crop contour writes bounds-checked);
- `per_char_heat_pane`: half-cell heat lookup from world coords per big
  pixel, same rounding as `big_pixel_pane`.
Bin: err header/value width BIG_PX_W; docs. Numeric metrics,
`FillGrid::bitmap_pane` (coherence-test failure reports), and `pos`
mode all untouched.

New tests in coverage.rs `pane_tests`: every metric pane must be
BIG_TEXT_ROWS x BIG_PX_W (grid-size regression guard), and the
mismatch pane's lit big-pixel count must equal the Over/Under sample
count of the window crop exactly (pane ≡ numeric, not a downsampled
view). Suite: 139 passed / 0 failed in terminal_rendering lib.
Frame height unchanged; method rows ~8 cols wider (common row still
widest).

## Context

The vision doc (docs/vision/floating-square-debug-tool.md, "zoomed in"
scale) says the actual zoomed view should map the smallest rendering
increment to one big pixel. The `glyphs` subcommand already does this
(see [floating-square-glyph-table.md](floating-square-glyph-table.md)):
8x24 big pixels per character on the snap families' union lattice
(1/16 x 1/24 world, = coverage.rs's SX x SY sample grid), so every glyph
edge lands exactly on a pixel boundary. The animate mode's zoom
(`glyph_pane`) did not: it rendered the 3x3 window at 1/8 x 1/8 world
per pixel with fractional coverage shading, which hid odd 1/16
horizontal cuts (each pixel spans two of them) and anti-aliased every
vertical cut that isn't a multiple of 1/8.

## What landed

- `coverage.rs`: `BIG_PX_W/BIG_PX_H/BIG_TEXT_ROWS` (2x2-world window
  around the center square => 32x48 big pixels => 32 cols x 24 text
  rows), `big_pane_from_colors` (pane_from_colors' composition at the
  union lattice: 8 big px per half-cell column, 12 text rows per
  character cell), and `big_pixel_pane` — same inputs as `glyph_pane`
  but sampling `glyph_filled` at pixel centers: exact fill, no
  coverage shading, one palette color per owning glyph. All
  `#[doc(hidden)]` debug surface, like `glyph_pane`.
- `floating_square_debug.rs` (animate mode): both method rows' zoom
  column is now `big_pixel_pane` with a "big pixels 1/16x1/24" caption;
  the common row's ideal pane is a new `ideal_big_pixel_pane` over the
  same 2x2 window/lattice (analytic fractional shading retained — true
  square edges fall between lattice points), so actual and ideal line
  up cell-for-cell.
- Frame grows ~52 -> ~74 rows; width unchanged (~100 cols, the common
  row was already widest). `pos` mode and all metric panes unchanged
  (metrics stay on the 2x3-samples-per-pixel oracle the coherence test
  asserts on).

## Verification

- Geometry checked against hand-computed expectations at two orbit
  positions: theta=0 (pos=(2.5, 0)) renders exactly 16x24 big px in both
  actual and ideal; theta~=0.139 (pos=(2.476, 0.345), family=hextant,
  snap err (+0.024, -0.012)) renders the snapped square x∈[2,3],
  y∈[-1/6, 5/6] as exactly py rows 4..26, cols 16..31 — pixel-exact
  match to the snapped geometry; the 56-px delta vs ideal is the snap
  error the tool is meant to show.
- f32 pitfall found and fixed during verification: computing the ideal
  pane's overlap in world units leaves ~1e-7 noise (1/24 is not dyadic),
  painting epsilon slivers at exact edge alignments and off-by-one-u8
  lerp colors that `two_tone_cell` renders as spurious ▀/▄. Fix:
  compute overlaps in integer lattice units so pixel bounds are exact;
  only the square's own edges carry pos's rounding. The old 1/8-lattice
  `ideal_pane` (pos mode) is dyadic and never had this problem.
- Suite: `cargo test -p terminal_rendering` green (469 passed).

## Known leftovers (not done here)

- `pane_from_colors`, `ClassGrid::full_pane`, and the 3-class pane pass
  raw pixel indices to `cell_bg` (`cell_bg(px, t/4)` instead of
  `px / 4`), so those panes' checkerboard flips every 2 px instead of
  per character cell like `bitmap_pane`/`glyph_pane`/`big_pixel_pane`.
  Latent pre-existing inconsistency; 3-line fix if wanted.
- GRID_SCREEN_ORIGIN's comment in the bin ("first column is the 6-wide
  small view", 2+6+2=10) does not match the current layout (large view
  is the first column); mouse mapping may be off. Untouched.
