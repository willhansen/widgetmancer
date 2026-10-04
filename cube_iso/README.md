# cube_iso

An interactive **visual prototype** of four large cubes floating in space,
rendered entirely in the terminal. It exists to answer one question: how does a
*small character walking on top of large cubes* read in a terminal, and what
does it look like when the character walks off an edge and falls?

This is intentionally separate from the game. It owns its own voxel model and
its own [`Frame`](../crates/terminal_rendering/src/frame.rs); it does **not**
touch `Game`, `Graphics`, `Screen`, or the FOV/portal machinery, all of which
are hard-wired to a flat 2D top-down world. The initial design is recorded in
[PLAN.md](PLAN.md).

## Running

```sh
cargo run -p cube_iso                 # interactive, alternate screen
cargo run -p cube_iso -- --dump /tmp/frame.txt   # headless single frame
```

Options: `--dump FILE`, `--width N`, `--height N`,
`--mode smooth|grid|gated`, `--scenario top|stairs|fall|east|west`, `--help`.

`--scenario` jumps the player to a starting situation, which is handy for a
single headless dump:

```sh
cargo run -p cube_iso -- --dump /tmp/frame.txt --scenario stairs --height 48
```

- `top` — standing on a cube top (default).
- `stairs` — walked off the south edge onto the side staircase.
- `fall` — overshot the staircase and is falling into open space (leaves a
  `│` trail).
- `east` / `west` — walked off that face onto the protruding x-facing ledges.

The dump writes the ANSI frame to `FILE` and echoes an uncolored character
view to stdout, so the shape is readable in a log or test output.

## Controls

| Key | Action |
|-----|--------|
| arrows / WASD / hjkl | move |
| `g` | cycle the physics scheme |
| `r` | respawn |
| `q` / `Esc` | quit |

## Projection P — shared-vertical pseudo-isometric

Top faces are drawn top-down exactly like the flat board: one world square is
**two terminal columns** wide and **one row** tall, because terminal cells are
about twice as tall as they are wide. A cube's **south wall** is a vertical
extrusion below its top face. The defining rule is:

```
col = 2 * (x - cam.x)          + width/2
row =     (cam.y - y) - z      + height/2
```

So **one terminal row is either one world step north-south or one step of
altitude**. North (`+y`) and up (`+z`) both move up the screen; east/west move
across. The camera follows the player's `x`/`y` but deliberately not `z`, which
is what makes a fall a visible slide down the screen.

Properties of this projection (inherent, not bugs):

- Only **south walls** are visible; east/west faces are edge-on, so a cube reads
  as "top-down plan + front elevation." A true 2:1 isometric projection (top
  faces as rhombi, depth drawn diagonally) would show two faces but abandons the
  existing top-down square scale and the FOV/portal fit. See [PLAN.md](PLAN.md).
- `CUBE_GAP` must be `>= CUBE_HEIGHT` so a cube's wall does not overlap the cube
  to its south. Both are constants in [`src/world.rs`](src/world.rs); the gap is
  wider than the height so a band of void is visible.

Shading does the 3D work: lit top faces (two-tone checker), a south wall that
darkens toward its base, and a procedural starfield in the void. Solid surfaces
use block characters (`█` top, `▒` wall) whose fill is carried by both fg and bg,
so they render flat in color but stay readable in an uncolored dump.

### Readability (color + checkerboard only)

Projection P collapses north-south distance and altitude into one vertical axis,
so a ledge's *screen row* alone cannot say how far it is from the cube. The
renderer recovers the lost depth with channels P leaves free, without changing
the projection:

- **Material**: cube faces are cool (block-checkered top, smooth wall, a bright
  front rim), ledges are warm. A warm shelf on a cool wall reads immediately.
- **Standoff hue**: each ledge's distance to the nearest cube maps to a warm hue
  ramp — amber (1 square out) → orange → crimson → violet (4+). This is the exact
  depth channel; `standoff` is measured from the ledge column to the nearest
  full-height cube column.
- **Block checker**: a 3-square checker on exposed top faces gives the surface
  grid and a coarse phase backup for depth. Walls stay a smooth gradient so
  ledges stand out.
- **End caps**: the left/right ends of a ledge run are drawn in a bright shade of
  its hue, so the shelf's extent is legible.
- **Depth fog**: columns dim with camera distance (`1 − d/FOG_SPAN`, floored at
  `FOG_MIN`), so far cubes fall back to a silhouette and only near geometry
  competes.

The channels are separate: hue = standoff, checker/shade = grid, brightness
(with fog) = distance, so they don't fight.

## World model

Four equal `CUBE_SIZE x CUBE_SIZE` footprints (10x10) in a 2x2 arrangement,
each `CUBE_HEIGHT` (10) tall, separated by `CUBE_GAP` (14). The world is a set
of solid **voxels**, so columns can have overhangs: a voxel shows a lit top face
where nothing sits above it and a shaded south face where the column to its
south has no solid at that altitude.

Each cube carries **side platforms** for testing the sidescroller:

- a descending **south staircase** (thin one-voxel slabs at altitudes 8, 6, 4,
  2 drifting east) so a player who walks off the south edge can step down it;
- **east and west ledges** (three squares of horizontal protrusion at altitudes
  7 and 4). These read unambiguously in projection P, because x is the screen's
  horizontal axis — unlike the south staircase, whose standoff is recovered via
  the standoff hue.

There is no floor plane, so the cubes genuinely float and the gaps are
bottomless: falling past the platforms marks the player lost until `r`. See
[`src/world.rs`](src/world.rs).

## Physics schemes

One continuous `(x, y, z)` position; the scheme only changes how intent and
gravity are applied (cycle with `g`):

1. **smooth + realtime** — continuous horizontal velocity, gravity on the
   realtime clock, single-character-wide body.
2. **grid + realtime gravity** — integer horizontal steps, gravity still
   integrated on the realtime clock.
3. **grid + move-gated gravity** (default) — fully discrete: gravity is resolved
   immediately after an intentional move, and the resolved fall is recorded as a
   trail of altitudes drawn during and after the drop. This is the scheme that
   best matches a turn-based roguelike.

See [`src/physics.rs`](src/physics.rs).

## Files

```
src/main.rs      terminal loop, input, HUD, --dump
src/world.rs     voxel model + four-cube layout
src/project.rs   projection P math
src/render.rs    top faces, walls, starfield, player, fall trail
src/physics.rs   Player + the three schemes
```

## Tests

```sh
cargo test -p cube_iso
```

Covers the projection identities (including "one altitude step == one N-S step"
and square aspect), the world layout/gap invariant and platform slabs, all three
physics schemes, and the readability channels (distinct standoff hues, fog
monotonicity, 3-square checker parity, warm-ledge/cool-cube presence) plus
structural render checks (player centering, fall trail).

## Limitations / next ideas

- Projection Q (true 2:1 isometric) is not implemented; P was chosen for engine
  compatibility.
- Smooth mode has no horizontal collision and glides via velocity damping.
- Only south walls are drawn; east/west bevels would need projection Q.
- Platforms are one-voxel-thick slabs; the player is supported by the top
  surface only, so head-bonking on undersides is not modeled.
- Portals, FOV, and the turn engine are out of scope until the look is settled.
