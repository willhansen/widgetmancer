# Porting `cube_iso` into the game

The `cube_iso/` package is a standalone prototype of the z/altitude rendering
the game is absorbing. This doc records the plan and inventories everything the
demo demonstrates, so the game can take it over incrementally and `cube_iso/`
can be deleted once nothing is left to port.

**Rule:** flip a feature's status in the *same commit* that ports it (mirroring
`ROADMAP.md`'s "check off in the same commit"). `docs/CHANGELOG.md` still gets
the per-commit entry per `AGENTS.md`.

## Status legend

- `native` — the game already has an equivalent; nothing to port.
- `pending` — in the demo, still to port.
- `ported` — now lives in the game.
- `testbed-only` — demo scaffolding we deliberately will NOT port (the game has
  better tooling); dropped with the demo.
- `deferred` — gameplay feature, outside the current "rendering z first" scope.

## Decisions

These were settled while planning the port (see the conversation that produced
this doc):

- **Scope:** rendering z first. Add height/altitude and render the
  top-face + wall look; floating entities gain a static altitude; gameplay
  stays 2D on the top surface.
- **Substrate:** extend the game's native `Glyph` / `Screen` path (explicit
  colors). Do **not** migrate the game loop to the `Frame` / `DrawableGlyph`
  stack, which has an `Option`-color bleed in `Frame::raw_string` (the bug that
  made the demo's HUD letter show as a solid block).
- **FOV / portals:** keep them exactly as-is on the top surface; walls are
  visual only for now. Full 3D occlusion/portals is a separate, larger effort.
- **View rotation:** wire `q`/`e` to the game's existing `Screen::rotation`
  (`QuarterTurnsAnticlockwise`); movement is already screen-relative.
- **Terrain model:** a voxel set (`HashSet<WorldVoxel>`), not a per-column
  height map. Existing block placement becomes the special case of a
  single-height column.
- **Board edge:** render the flat board as a floating slab with a visible edge
  wall (the demo's "slab in space" look).
- **Heights source:** a new terrain placement API plus a demo map; block
  placement is the trivial case.
- **Floating entities:** static altitude per entity (rendering only), no
  vertical physics yet.
- **`cube_iso`:** kept as a fast projection/material testbed until the port is
  complete; its HUD is not ported.

## Phased plan

1. **Data model** — `WorldVoxel`/`WorldPoint3` types; `Game.voxels` +
   `column_tops`; `place_voxel` / `place_solid_column` / `height_at` /
   `is_solid_at`; `place_block` as a single-height column; board slab; snapshot
   voxels (both directions, sorted).
2. **z projection + forward column pass** — `Screen` z-forward mapping and a
   multi-row write path; replace the board part of
   `load_screen_buffer_from_fov` with a painter-sorted forward column pass
   (top face + wall rows + slab edge), gated so an all-zero-height board takes
   the existing path byte-for-byte.
3. **Materials** — cool cube vs warm ledge, standoff hue, 3-square block
   checker, bright rim, depth fog, emitting explicit `Glyph` colors.
4. **Floating entities** — static `z` on `DeathCube`/`FloatingHunterDrone` and
   `OffsetSquareDrawable`; render through the z projection; snapshot field.
5. **View rotation** — bind `q`/`e` to `Screen::rotation`; show facing via
   `DebugOverlayFlags`.
6. **Tooling / testbed** — extend `snapshot_tool` with a height/column view and
   `map_diagram` to print heights; drop the demo HUD; verify headlessly.

Highest-risk step is Phase 2 (swapping the inverse FOV composite for a forward
pass); the flat gate plus golden diffs contain it.

## A. Terrain / world model (`cube_iso/src/world.rs`)

| Feature | Game target | Status |
|---|---|---|
| Voxel set + per-column top cache | `Game.voxels` + `column_tops` | pending |
| Exposed-face semantics (top face; camera-facing side face) | forward column pass | pending |
| `is_cube_top_column` / `nearest_cube_distance` (standoff source) | material helper | pending |
| Four-cube 2x2 demo layout, `CUBE_SIZE/HEIGHT/GAP`, `GAP >= HEIGHT` invariant | demo-only layout; keep invariant doc | testbed-only |
| Side platforms: south staircase + east/west ledges | demo map content | testbed-only |

## B. Projection / camera (`cube_iso/src/project.rs`)

| Feature | Game target | Status |
|---|---|---|
| Projection P (`col = 2(x-cam.x)+w/2`, `row = (cam.y-y)-z+h/2`) | `Screen` z-forward mapping | pending |
| Camera follows x,y but not z | game camera | pending |
| 90-degree view rotation (0..=3 CCW) | `Screen::rotation` + `q`/`e` | pending |
| Direction helpers (`forward`/`toward_camera`/`screen_left`/`screen_right`, `ScreenDir`) | input/camera helpers | pending |
| Signed `forward` (painter key) vs absolute `depth` (fog) | column pass + fog | pending |

## C. Rendering (`cube_iso/src/render.rs`)

| Feature | Game target | Status |
|---|---|---|
| Painter's algorithm by signed forward depth (far -> near) | forward column pass | pending |
| Top faces `#` / side faces `%` with material colors | face drawables | pending |
| Cool cube material: block-checkered top, smooth wall gradient, bright rim | material helper | pending |
| Warm ledge material: standoff hue ramp, darker front, end caps | material helper | pending |
| 3-square block checker (x,y,z parity) | material helper | pending |
| Depth fog (`FOG_SPAN`, `FOG_MIN`) | material helper | pending |
| Board-as-floating-slab edge wall | forward column pass | pending |
| Player marker (width by mode) | existing `ArrowDrawable` | pending |
| Fall trail | deferred with gravity | deferred |
| Starfield in the void | already in game | native |
| Flat solid cells (fg=bg) for uncolored dumps | `Glyph` already explicit | native |

## D. Physics (`cube_iso/src/physics.rs`)

| Feature | Game target | Status |
|---|---|---|
| Three modes (smooth / grid+realtime gravity / grid+move-gated) | gameplay | deferred |
| Gravity, `FALL_LIMIT`, `lost`, respawn | gameplay | deferred |
| Move-gated gravity + fall trail | gameplay | deferred |
| Smooth (single-char) vs grid (double-char) body width | gameplay | deferred |

## E. App scaffolding (`cube_iso/src/main.rs`)

| Feature | Game target | Status |
|---|---|---|
| Alt-screen loop, input thread, panic hook | already in game | native |
| View-relative movement keys | `InputMap` | pending |
| `q`/`e` rotate, `Esc`/`Ctrl-C` quit, `g`, `r` | game input mapping | pending |
| HUD (mode/facing/pos + help) | use `DebugOverlayFlags` / `map_diagram` | testbed-only |
| CLI (`--dump/--width/--height/--mode/--rotate/--scenario`) | use `snapshot_tool` + map args | testbed-only |
| Scenarios (`top/stairs/stairs-side/fall/east/west`) | use map builders / snapshots | testbed-only |

## F. Verification (`cube_iso` tests)

| Feature | Game target | Status |
|---|---|---|
| Projection identities, direction steps, view-relative movement | port as game tests | pending |
| Fog monotonicity, hue distinctness, checker parity, layout/gap invariants | port | pending |
| Painter-order regression (staircase not overwritten) | port | pending |
| Physics-mode tests | deferred with physics | deferred |
| Material/color dump scripts | replace with `snapshot_tool` | testbed-only |

## Deletion gate

Delete `cube_iso/` (and remove it from the workspace `members`) only when **no
`pending` items remain** and every `testbed-only`/`deferred` item is explicitly
acknowledged here. Then delete this doc or mark it **Done** with a date
(`ROADMAP.md` convention).
