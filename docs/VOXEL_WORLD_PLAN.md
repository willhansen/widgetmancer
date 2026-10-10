# Voxel-world / `space-cubes` work plan (status)

Working plan for turning the board into an explicit voxel grid and recreating
the deleted `cube_iso` demo as the `space-cubes` map. Kept current so the work
can be resumed after an interruption. The port itself is recorded in
[`CUBE_ISO_PORT.md`](CUBE_ISO_PORT.md); per-commit narrative in
[`CHANGELOG.md`](CHANGELOG.md).

**Status: complete and verified.** Remaining items are deliberate follow-ups,
not blockers.

## Locked decisions

- **The world is an explicit voxel grid.** `VoxelGrid`'s voxel set is the
  source of truth and owns the horizontal `extent` that defines "on the map";
  the `z = -1` floor layer is a convenience a map opts into. Void is real
  (renders as starfield, blocks movement). There is no separate board.
- **Walkability follows surface altitude.** A square is enterable iff it has
  ground and is not *higher* than the player's current surface (derived from the
  voxel stack). Level and step **down** allowed; step **up** and void blocked.
  This keeps single-voxel blocks as walls on floor maps and lets the player
  descend a staircase.
- **Altitude-aware sight.** A square blocks only when its top rises above the
  player (`Game::fov_blockers`).
- **Maps own their extent.** Grid extent and player start come from the map, not
  the terminal. `Game::new`'s terminal-derived extent is only a fallback for
  map-less/test games.
- **Maps are data.** `maps/<name>.json` is a recipe of setup ops; the built-in
  maps remain as a fallback.
- **Gravity/falling is deferred** (see `ROADMAP.md` → "Deferred: voxel-world
  vertical gameplay").

## Steps

- [x] **0. Reconcile refactor.** `Player.altitude` dropped (altitude is derived);
  `snapshot` player-altitude field reverted; starfield/floor call sites fixed.
- [x] **1. Voxel-grid floor model.** `voxel_grid.rs`: `fill_floor_rect`,
  `clear_floor`, `floor_squares`, `occupied_squares`, `floor_voxels`;
  `seed_floor` = clear + full-rect fill.
- [x] **2. Rendering gaps.** Floor drawn per occupied square
  (`draw_floor_pattern`), starfield keyed on occupancy
  (`starfield::draw(occupied, …)`), walls FOV-gated in
  `load_screen_buffer_from_terrain`.
- [x] **3. Walkability.** `player_altitude` / `player_can_stand_at`;
  `try_set_player_position` uses them instead of `is_block_at`.
- [x] **4. Snapshot floor.** `floor_cells` written only when the floor layer
  differs from the full extent rect; loader clears + re-lays it. Legacy
  snapshots keep the default full floor.
- [x] **5. JSON map recipes.** `game/map_file.rs` (`MapFile`, `MapOp`,
  `load_map_file`, `maps_dir`, `Game::apply_map_file`); `set_up_map_by_name`
  prefers a recipe; ops: `fill_floor_rect`, `clear_floor`, `cuboid`, `column`,
  `voxel`, `cube_side_platforms`.
- [x] **6. `space-cubes` map.** `maps/space-cubes.json` (extent 42×38,
  `clear_floor`, four 10×10×10 cubes, side platforms, player `[9,9]`,
  `sight_radius 24`), `maps/space-cubes.sh`, `main.rs` usage.
- [x] **7. Terminal-independent built-ins.** `demo` (40×24 @ 20,12), `racetrack`
  (48×26 @ 24,13), `hallways` (48×26 @ 24,13); `numbered-boxes` already fixed.
  `do_everything` clamp removed; `map_diagram` no longer sizes to a map.
- [x] **8. Tests.** Voxel floor/void; walkability (down ok, up/void blocked);
  map-file parse/apply; snapshot `floor_cells` round-trip; `space-cubes`
  invariants; added to the frame-reproducibility loop.
- [x] **9. Docs.** `CUBE_ISO_PORT.md` post-port section; `ROADMAP.md` gravity
  entry; `CHANGELOG.md`.
- [x] **10. Grid vocabulary.** Board/slab/column vocabulary removed in favor of
  the voxel grid (2026-10-10): `BoardSize`→`GridExtent`, `Terrain`→`VoxelGrid`
  (file `voxel_grid.rs`), `Blocks`→`FloorFeatures` (file `floor_features.rs`),
  `seed_board_slab`→`seed_floor`, `height_at`→`surface_at`,
  `place_solid_column`→`fill_column`, `SLAB_*`→`FLOOR_*`, `TerrainMaterial`→
  `VoxelMaterial`. The grid now owns the extent; `Game` no longer has a
  `board_size`. Serialized/JSON names (`board_width`, `floor_cells`, the
  `board` map key) are unchanged.

## Where things live

| Concern | Location |
|---|---|
| Floor / void / voxels / extent | `crates/game/src/game/voxel_grid.rs` |
| Walkability, FOV blockers, player | `crates/game/src/game/mod.rs` |
| Forward voxel pass, starfield, walls | `crates/game/src/graphics.rs`, `graphics/starfield.rs` |
| Snapshot floor + player | `crates/game/src/game/snapshot.rs` |
| Map recipes | `crates/game/src/game/map_file.rs`, `maps/*.json` |
| Map dispatch / launch | `crates/game/src/lib.rs`, `crates/game/src/main.rs` |

## Verify

```sh
cargo test --workspace --features game/debug-tools
cargo run -p game --features debug-tools --bin snapshot_tool -- diff snapshot/   # expect OK
./maps/space-cubes.sh            # or: ./play-game --map space-cubes
cargo run -p game --bin map_diagram -- space-cubes
```

## Follow-ups (not blockers)

- Gravity/falling, fall trail, step-down animation, physics modes, stored
  player `z` — documented in `ROADMAP.md`.
- Generic repetition ops (`repeat` / `for`) for map recipes, so patterns can be
  built from primitives without a bespoke Rust op.
- Warm-ledge standoff-hue ramp and bright rim remain intentionally dropped.
- `map_diagram` prints height 10 as `0` (height mod 10); cosmetic.
