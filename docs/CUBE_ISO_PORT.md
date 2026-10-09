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

## Environment model (what voxels own)

"The board" in this game is not one flat grid of solid/empty cells; it is four
subsystems, and altitude only enters the first. Keeping the split explicit stops
later phases from assuming altitude is gameplay-addressable:

| Layer | Where | Does the voxel model own it? |
|---|---|---|
| Terrain shape / solidity / altitude | `game/terrain.rs` | **yes** — voxel set + per-column top; board slab; blocks are single-height columns |
| Board containment + void | `square_is_on_board`, `graphics/starfield.rs` | no — a rectangle, unchanged |
| Portal topology | `portal_geometry.rs` | no — 2D rigid transforms with **no `z`** |
| Portal-aware FOV | `fov_stuff.rs` | no — 2D shadowcasting over a `SquareSet` of blockers |
| Gameplay addressing | pieces, widgets, belts, push chains | no — one square (2D) on the top surface |
| Render compositor | `graphics.rs` / `load_screen_buffer_from_fov` | partly — Phase 2 adds the forward column pass |

Invariants that hold until full 3D occlusion is tackled (a separate, larger
effort):

- Altitude is **visual-only** for now; gameplay stays 2D on the top surface.
- A portal transform ignores `z`; portals exist at every height.
- `is_solid_at(x, y, 0)` / `block_squares()` (altitude 0) remains the
  gameplay/LOS query; taller voxels do not affect movement or FOV yet.
- `height_at` is the single source for the Phase 2 column pass and Phase 4
  floating-entity altitude.

## Phased plan

1. **Data model** — `WorldVoxel`/`WorldPoint3` types; `Game.voxels` +
   `column_tops`; `place_voxel` / `place_solid_column` / `height_at` /
   `is_solid_at`; `place_block` as a single-height column; board slab; snapshot
   voxels (both directions, sorted). **Done 2026-10-04** — terrain lives in
   `game/terrain.rs`; the board is a materialized slab at voxel `-1`; blocks
   were folded into the voxel set (`Blocks.blocks` removed) with `block_squares`
   (= solid at altitude 0) as the gameplay/LOS query; snapshots emit `voxels`
   (slab regenerated on load) and still read the legacy `blocks` field.
2. **z projection + forward column pass** — `Screen` z-forward mapping and a
   multi-row write path; replace the board part of
   `load_screen_buffer_from_fov` with a painter-sorted forward column pass
   (top face + wall rows + slab edge), gated so an all-zero-height board takes
   the existing path byte-for-byte. **Done 2026-10-04** — `Screen::
   world_square_and_altitude_to_screen_buffer_square` shifts a square up one row
   per voxel (rotation-independent); `Graphics::load_screen_buffer_from_terrain`
   walks columns far-to-near, drawing top faces from the FOV/draw-buffer lookup
   (so visibility, partial shadows, and entity overlays still apply), solid
   placeholder walls, and the slab edge. Gate: `Terrain::max_top_altitude() > 1`
   (taller than one voxel), so flat/block-only boards keep the legacy path
   byte-for-byte. On a raised board the legacy inverse composite still runs
   *underneath* the pass, so portal views of the floor and entities (including
   its per-cell `drawn_over` compositing) are preserved; only the raised
   geometry itself is not yet re-projected through portals (made portal-aware
   on 2026-10-08 — see the post-port note below). Materials and real
   wall colors are Phase 3.
3. **Materials** — cool cube vs warm ledge, standoff hue, 3-square block
   checker, bright rim, depth fog, emitting explicit `Glyph` colors.
   **Done 2026-10-04** — terrain carries a per-column `TerrainMaterial`
   (`Floor` or `Tint`), chosen when the column is created; `Floor` resolves to
   the existing board pattern so flat boards are unchanged. The forward pass
   recolors only the top's **background** (glyphs — `#`, pieces, player — stay
   visible), uses the 3-square `(x,y,z)` checker for tints, draws a wall
   gradient, and fogs terrain and walls. The demo's cube-vs-ledge split and
   standoff-hue ramp are intentionally dropped (a tint is chosen at creation
   instead); the bright rim is not ported. Non-default materials are
   snapshotted.
4. **Floating entities** — static `z` per entity; render through the z
   projection; snapshot field.
   **Done 2026-10-04** — both entities carry an `altitude` (voxels, default 0),
   snapshotted. A non-zero-altitude entity is held out of the planar draw
   buffer and composited by `Graphics::overlay_floating_entities_at_altitude` at
   its shifted screen square, FOV-gated, so there is no ground-level ghost.
   Altitude 0 keeps the legacy path byte-for-byte. (Altitude is tracked in
   `Graphics`, not `OffsetSquareDrawable`.)
5. **View rotation** — bind `q`/`e` to `Screen::rotation`; show facing via
   `DebugOverlayFlags`. **Done 2026-10-04** — `q`/`e` rotate the view via
   `Game::rotate_view`; quit moved to `Esc`/`Ctrl-C`; movement is already
   screen-relative so it follows automatically. The demo's facing/HUD text is
   testbed-only; the game's `DebugOverlayFlags`/`map_diagram` cover debugging.
6. **Tooling / testbed** — extend `snapshot_tool` with a height/column view and
   `map_diagram` to print heights; drop the demo HUD; verify headlessly.
   **Done 2026-10-04** — `map_diagram` prints `.`/`#`/height digits;
   `snapshot_tool heights <dir>` prints the loaded map's per-column top
   altitude. The demo HUD is dropped with `cube_iso/`.

All phases are complete; `cube_iso/` is deleted. Remaining statuses are
`native`, `deferred` (gameplay physics), or `testbed-only` (demo scaffolding).

## A. Terrain / world model (`cube_iso/src/world.rs`)

| Feature | Game target | Status |
|---|---|---|
| Voxel set + per-column top cache | `Game.voxels` + `column_tops` | ported |
| Exposed-face semantics (top face; camera-facing side face) | forward column pass | ported |
| `is_cube_top_column` / `nearest_cube_distance` (standoff source) | material helper | testbed-only (replaced by explicit per-column tint) |
| Four-cube 2x2 demo layout, `CUBE_SIZE/HEIGHT/GAP`, `GAP >= HEIGHT` invariant | demo-only layout; keep invariant doc | testbed-only |
| Side platforms: south staircase + east/west ledges | demo map content | testbed-only |

## B. Projection / camera (`cube_iso/src/project.rs`)

| Feature | Game target | Status |
|---|---|---|
| Projection P (`col = 2(x-cam.x)+w/2`, `row = (cam.y-y)-z+h/2`) | `Screen` z-forward mapping | ported |
| Camera follows x,y but not z | game camera | native |
| 90-degree view rotation (0..=3 CCW) | `Screen::rotation` + `q`/`e` | ported |
| Direction helpers (`forward`/`toward_camera`/`screen_left`/`screen_right`, `ScreenDir`) | input/camera helpers | native |
| Signed `forward` (painter key) vs absolute `depth` (fog) | column pass (fog removed) | ported |

## C. Rendering (`cube_iso/src/render.rs`)

| Feature | Game target | Status |
|---|---|---|
| Painter's algorithm by signed forward depth (far -> near) | forward column pass | ported |
| Top faces `#` / side faces `%` with material colors | face drawables | ported |
| Cool cube material: block-checkered top, smooth wall gradient, bright rim | material helper | ported (no rim) |
| Warm ledge material: standoff hue ramp, darker front, end caps | material helper | testbed-only (per-column tint instead) |
| 3-square block checker (x,y,z parity) | material helper | ported |
| Depth fog (`FOG_SPAN`, `FOG_MIN`) | material helper | removed (read as a spotlight on the cubes map) |
| Board-as-floating-slab edge wall | forward column pass | ported |
| Player marker (width by mode) | existing `ArrowDrawable` | deferred (with physics modes) |
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
| View-relative movement keys | `InputMap` | native |
| `q`/`e` rotate, `Esc`/`Ctrl-C` quit, `g`, `r` | game input mapping | ported |
| HUD (mode/facing/pos + help) | use `DebugOverlayFlags` / `map_diagram` | testbed-only |
| CLI (`--dump/--width/--height/--mode/--rotate/--scenario`) | use `snapshot_tool` + map args | testbed-only |
| Scenarios (`top/stairs/stairs-side/fall/east/west`) | use map builders / snapshots | testbed-only |

## F. Verification (`cube_iso` tests)

| Feature | Game target | Status |
|---|---|---|
| Projection identities, direction steps, view-relative movement | port as game tests | ported |
| Fog monotonicity, hue distinctness, checker parity, layout/gap invariants | port | ported (fog/checker; hue dropped with warm ledge) |
| Painter-order regression (staircase not overwritten) | port | ported |
| Physics-mode tests | deferred with physics | deferred |
| Material/color dump scripts | replace with `snapshot_tool` | testbed-only |

## Deletion gate

**Done 2026-10-04.** `cube_iso/` was deleted and removed from the workspace
`members`. No `pending` items remain: every ported feature is in the game
(phases 1-6), gameplay physics is `deferred`, per-column materials replaced the
cube/ledge + standoff-hue system, and the demo HUD/CLI/scenarios are
`testbed-only` (the game has `snapshot_tool`, `map_diagram`, and
`DebugOverlayFlags` instead). This doc is kept as the record.

## Post-port: `space-cubes` map (2026-10-04)

The demo scene is recreated as the `space-cubes` map, defined by data rather
than Rust: `maps/space-cubes.json` is a recipe of setup ops applied to a `Game`
(board size, `clear_floor`, four `cuboid`s at the demo's `(0,0),(24,0),(0,24),
(24,24)` + `+4,+4` offset, four `cube_side_platforms`, player on the first cube
top). `maps/space-cubes.sh` launches it.

This required two changes beyond the original port:

- **The board is an explicit voxel grid, not a rectangle-with-floor.** The
  `z = -1` slab is a convenience a map opts into (`fill_floor_rect` /
  `seed_board_slab`); `clear_floor` gives real void, which renders as starfield
  and blocks movement. Snapshots record the floor (`floor_cells`) only when it
  isn't the default full rect.
- **Maps own their size.** Board size and player start come from the map, not
  the terminal; `do_everything` no longer clamps the terminal per map.
  `Game::new`'s terminal-derived board is only a fallback for map-less/test
  games.

Walkability follows the player's surface altitude: a square is enterable if it
has ground and is not *higher* than where the player stands (level or step down;
void and step-ups are blocked). This keeps single-voxel blocks as walls on floor
maps while letting the player descend the demo's staircases. Sight blockers are
likewise altitude-aware: a column blocks only if its top rises above the player.
See `ROADMAP.md` for deferred gravity/falling.

The execution status of this work (and its follow-ups) is tracked in
[`VOXEL_WORLD_PLAN.md`](VOXEL_WORLD_PLAN.md).

## Post-port: portal-aware raised terrain (2026-10-08)

The Phase 2 caveat — "only the raised geometry is not yet re-projected through
portals" — is closed. `Graphics::load_screen_buffer_from_terrain` no longer
projects columns by their absolute world→screen square; it builds, per absolute
column, the shallowest-portal `PositionedSquareVisibilityInFov` from the FOV and
draws the top face and camera-facing wall at that visibility's *relative* screen
cell. Portal views therefore get the column's material, the `0.1 * depth` red
tint, and the correct camera-facing side (the wall direction is rotated by
`portal_rotation_from_relative_to_absolute`). Writes are clamped to the
`sight_radius + 1` FOV frame, so the camera's `camera_altitude` shift can no
longer push a wall below the frame. Fixes issues 0007, 0008, and 0009.

Two gaps in that first pass were closed on 2026-10-09 (issues 0010/0011/0012).
The occlusion check ran only at a column's *top* cell, while the wall is written
up to `camera_altitude - z` rows below it; a wall write now also requires the
wall cell itself to resolve to the same view frame
(`absolute_fov_center_square`) as the column, so a directly visible cube's wall
cannot paint into cells the portal resolves to void/another region. And the
top-face background recolor now skips a `ConveyorBelt`, compositing it over a
material base so its own black background survives.

Issue 0013 closed the remaining collapse: the pass had picked each absolute
column's single shallowest-portal view, so a column visible both directly (at an
off-terminal relative cell) and through a portal was drawn only for the direct
view and the portal cell kept the flat board checkerboard. The pass now walks
the FOV's relative cells and draws the terrain column named by each cell's
`resolved_visibility`, so raised geometry appears in every view it is visible
through.

The wall gradient's top color was the material tint itself, so a cube's side
matched its top's light checker and a step read as walkable (issue 0015). The
gradient endpoint is now a darkened shade of the material
(`WALL_TINT_SHADE`), keeping every column's exposed side clearly darker than
its top.
