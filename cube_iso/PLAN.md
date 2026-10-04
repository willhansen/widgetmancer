# cube_iso — initial plan

This is the design that `cube_iso` was built from. It is kept as a record of
the decisions and the intended next steps; see [README.md](README.md) for how to
run the prototype as it stands.

## The idea

The game's setting is a small character walking on top of large cubes floating
in space. Falling off the side becomes a platforming sidescroller. How can that
be represented while keeping terminal rendering?

## Constraints found in the existing renderer

- One world square always renders as **two terminal characters**
  (`DoubleGlyph`), so squares look visually square.
- The whole pipeline (FOV, portals, camera rotation) assumes an **axis-aligned
  top-down grid**. True 3D perspective is out.
- The off-board **starfield** already fills the void, and
  `falling_box_animation.rs` already has a visual language for things falling
  off the board.
- Sub-cell glyph vocabulary available: half/eighth blocks, quadrants, hextants,
  angled blocks, and braille — enough to shape bevels, limbs, and motion arcs.

## Decisions

| Question | Decision |
|----------|----------|
| Deliverable | Prototype the visuals (standalone binary) |
| Projection | **P: shared-vertical** top-down top faces + vertical south walls |
| Physics | All three schemes behind a toggle; default **#3 grid + move-gated gravity** |
| Location | New standalone `cube_iso` package; no changes to `game`/`Screen`/`Graphics` |
| Cube data | 4 equal cubes in a 2x2 with a gap; 3D voxel model |

The "shared-vertical" rule came from the brief: *one vertical character on the
screen is equivalent to one step north-south or also up-down.* In projection P
that is literally the mapping: `row = (cam.y - y) - z`, so north and altitude
share the screen's vertical axis.

## Planned steps (and status)

- [x] Scaffold package + workspace member; alternate-screen loop, input thread,
      panic hook; `--dump` headless frame.
- [x] `world.rs`: voxel/column model, 2x2 four-cube generator with tunable
      `CUBE_HEIGHT`/`CUBE_GAP` (gap > height so void is visible between cubes).
- [x] `project.rs`: coordinate math, painter's-algorithm ordering, off-screen
      cull; tests for round-trip and ordering.
- [x] `render.rs`: top faces, south walls + shading, void/starfield, player
      marker, fall trail.
- [x] `physics.rs`: driver layer + #3 default; #1 and #2 wired; `g` toggles.
- [x] Interactive controls: arrows/WASD move, `g` cycle scheme, `r` respawn,
      `q` quit; HUD line.
- [x] Headless structural + projection + physics tests.
- [x] `README.md` and this plan.

## Projection P details

```
col = 2 * (x - cam.x) + width/2
row =     (cam.y - y) - z + height/2
```

Painter's algorithm visits columns north-to-south (larger `y` first) so nearer
geometry overwrites. Each column draws its lit top face, then its south wall
where the square to the south is not at the same altitude (cube edge or gap).

Accepted trade-offs:

- Only south walls are visible; east/west faces are edge-on.
- `CUBE_GAP >= CUBE_HEIGHT` is required to avoid wall/top overlap.

## Deferred

- **Projection Q (true 2:1 isometric)**: rhombus top faces
  (`x -> (+2,-1)`, `y -> (-2,-1)`, `z -> (0,-1)`), two visible faces. Looks more
  cube-like, but abandons the existing top-down square scale and FOV/portal fit.
  Kept as a future toggle.
- Horizontal collision for smooth mode; velocity smoothing for grid modes.
- Reconciling with the game: a real 3D board would need a voxel/height layer,
  per-column solidity, a level-aware camera, and (eventually) 3D portal
  transforms. The prototype deliberately avoids all of that to settle the look
  first.
