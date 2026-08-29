# Floating Blocks: Sub-Square Square Rendering

How entities positioned at `f32` world points (death cubes, hunter drones)
get rendered as a 1×1 world square whose center need not align with the
character grid. This is the deepest sub-character path in the renderer; for
the overall pipeline see [RENDERING.md](RENDERING.md), for the coordinate
frames see [COORDINATE_FRAMES.md](COORDINATE_FRAMES.md).

## Pipeline

```
WorldPoint (f32)                              game side
  │  OffsetSquareDrawable::drawables_for_floating_square_at_point
  ▼  (crates/game/src/graphics/drawable.rs)
HashMap<WorldSquare, OffsetSquareDrawable>   one entry per covered square,
  │                                          empty squares omitted
  ▼  Drawable::to_glyphs, per square
characters_for_full_square_with_2d_offset(WorldMove) -> DoubleChar
  │  (crates/terminal_rendering/src/floating_square.rs)
  ▼
two block characters per world square, composited like any other drawable
```

A floating square can straddle up to 3×3 world squares (its center lands
within half a square of the rounded center, and it extends half a square in
each direction). `drawables_for_floating_square_at_point` visits all nine
neighbors, computes each one's offset from the true point, and keeps only
the squares whose glyph pair is non-empty. From there the drawable flows
through the normal draw-buffer/FOV/screen machinery — portals, rotation,
and compositing included (`OffsetSquareDrawable::rotated` just rotates the
stored offset vector).

## The core problem: no glyph has fine resolution in both axes

Block-element glyphs each commit their resolution to one axis or a coarse
grid, so a sub-cell offset can only be approximated. `floating_square.rs`
defines four **snap families** (`SnapFamily`), each a grid the square's
center can snap to:

| Family              | x resolution        | y resolution   | Glyph set                |
|---------------------|---------------------|----------------|--------------------------|
| Horizontal eighths  | 1/16 world          | the row        | `▏▎▍▌▋▊▉█`               |
| Vertical eighths    | the half-cell (1/2) | 1/8 world      | `▁▂▃▄▅▆▇█`               |
| Hextant             | 1/4 world           | 1/3 world      | sextants `🬀`–`🬻` + thirds |
| Quadrant            | 1/4 world           | 1/2 world      | `▖▗▘▝▌▐▄▀█`              |

(One world square = two terminal columns, one row; the half-cell is the
horizontal quantum.)

## The coherence invariant: one family per square

Each of the up-to-9 neighborhood cells sees a *different* offset from the
floating point. If every cell independently picked its best-fitting glyph,
neighboring cells could pick different families — the square's silhouette
tears (a sextant top edge against an eighth-block bottom edge, etc.). This
was a real bug; `tests/floating_square_coherence.rs` guards against it.

The fix has two parts:

1. **Every snap grid is integer-aligned.** Each family's grid is a subgrid
   of the world grid shifted only by whole squares, so snapping preserves a
   cell's integer offset from the square's center. Snapping cell A's offset
   and cell B's offset therefore lands on the same relative grid point, and
   `SnapFamily::for_offset` (minimum snap error) returns the same family
   for every cell of the square. The unit test sweeps neighborhoods of
   several offsets asserting exactly this agreement.

2. **Selection happens once per square.** `for_offset` compares the
   euclidean distance (in world units, which are already visually
   isotropic: 1 unit = 2 columns wide, 1 row ≈ 2 column-widths tall)
   between the true offset and each family's snapped offset, and takes the
   minimum. All nine cells then render with that family.

### Splitting the snapped offset across a cell's two half-cells

A 1-world-unit square is two half-cells wide; snapped to a quarter-cell x
grid it can occupy *three* half-cells. `characters_in_family` splits the
snapped x offset `s.x` (in world units) into per-half-cell remainders: the
half-cell the offset points away from gets `2·s.x` clamped to one half-cell
of magnitude (`(|2·s.x| − 1).max(0)·sign(2·s.x)`), the other gets the rest.
The result is that each half-cell's local offset always lands exactly on
the family grid, so `SnapFamily::character_for_half_square` only ever sees
offsets it has an exact glyph for.

### Glyph tables must encode square-overlap geometry

The family glyph functions (`hextant_block_by_offset`,
`quadrant_block_by_offset`, the eighth-block arrays) answer "which glyph
shows the part of a 1×1 square, offset by (x, y), that overlaps this
half-cell?". Every table entry must match that geometry exactly. A single
wrong entry carves a notch out of the silhouette — e.g. a hextant entry
holding a quadrant block (bottom *half*) where the geometry calls for the
corresponding sextant (bottom *two thirds*) produced a 0.25 × 1/6 missing
corner, detectable as a top-edge spread of 1/6 and a right-edge spread of
1/4. Symmetry between mirror entries is a useful audit heuristic.

## The coverage oracle

`crates/terminal_rendering/src/coverage.rs` measures what the renderer
actually draws, independent of the glyph-picking code:

- The 3×3 neighborhood is point-sampled at **16×24 samples per world
  square** — x must resolve eighth-blocks (8 per half-cell), y must divide
  both eighths and thirds (hence 24 = lcm(8, 3)). Sample points sit at
  half-sample offsets so they never alias a glyph boundary.
- `glyph_filled` is an exact per-glyph coverage model for every character
  the renderer can emit (space/full, all eighth blocks, thirds, quadrants,
  sextant bit patterns).
- `Metrics` (in the coherence test) derives silhouette properties from the
  sample grid: per-edge spread (must be 0 — straight edges), holes (must
  be 0), and filled area (must be ≈ 1 world square; worst-case family
  quantization is ~0.23).

The same oracle drives both consumers, so they can never drift apart:

- `tests/floating_square_coherence.rs` — sweeps a square along motion
  lines and asserts the silhouette stays rectangular throughout.
- The debug tool's actual-vs-ideal zoom view (below).

## Debug tooling

The top-level `./debug-floating-squares` wrapper (for
`cargo run -p terminal_rendering --bin floating_square_debug`) renders the
real glyph picks on a checkerboard of square centers:

- `pos X Y` — one square: family diagnostics (snapped offset, per-family
  snap errors), a frame with the true center marked `+` (note: the marker
  *overwrites* one half-cell glyph — check the coverage view, not the
  frame, for shape bugs), and the sampled actual-vs-ideal coverage zoom.
- `families X Y` — the same position with each family forced, side by
  side; explains why the automatic pick won.
- `sweep` — offset table over the positive quadrant in 1/16 steps, each
  cell labeled with its auto-picked family: a decision-boundary map.
- `animate` — orbiting square on the alternate screen; arrow keys nudge
  by 1/16 (the finest snap grid, so one keypress crosses at most one snap
  boundary), family switches are highlighted since that is where visible
  pops happen. This is the default when no mode is given.

## Related but separate: the 1D-offset path

`characters_for_full_square_with_1d_offset` and its looping variant snap a
square offset along a *single axis* (used by `ConveyorBeltDrawable` for its
phase animation). It is older, simpler, and has no family-coherence
concerns (one axis, one cell row of glyphs). Vertical offsets mix thirds
and eighths by per-axis least error; that mixing is deliberately **not**
used in the 2D path, where it would break family purity.
