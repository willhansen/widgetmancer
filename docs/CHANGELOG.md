# Changelog

Narrative record of work done in the sandbox, where commits are ephemeral.
Each entry corresponds to one focused commit; the bodies below are the commit
messages that would otherwise live only in the (throwaway) sandbox git history.
Hashes are sandbox-local and may not exist outside the container.

Newest first.

---

## 2026-10-09 — Debug-tooling hardening from the 0013–0016 session

### tooling: reproducible captures, snapshot-tool wrapper, and review commands

Follow-ups to the debugging friction hit while fixing 0013–0016.

- **Never trust a stale binary.** New repo-root `./snapshot-tool` wrapper runs
  `cargo run -p game --features debug-tools --bin snapshot_tool --`, so the
  binary always matches the sources (a stale `target/**/snapshot_tool` silently
  reported wrong diffs and nearly caused a wrong conclusion). `AGENTS.md`
  documents: use the wrapper, never the prebuilt path.
- **Captures byte-reproduce.** The snapshot now records
  `logical_time_seconds` — the draw clock (`graphics.current_time - start`),
  which the starfield/animations read and which differs from `world_time`. The
  three headless render paths draw at it (old captures fall back to
  `world_time_seconds`), and `from_snapshot` restores it, so future captures
  diff with no time-dependent star churn. Test:
  `snapshot_round_trips_the_draw_clock`.
- **Diff categories.** `render_diff_report` now prints a
  `char / fg-only / bg-only` split, so time-dependent starfield churn no longer
  drowns a real glyph change.
- **Frame labeling.** `explain` prints `screen-square (X,Y) = char cols ..
  ..`, and takes `--char-col` to accept a terminal character column.
- **Reproduce a transition.** `render-at <dir> [--player X Y] [--rotate N]`
  renders a capture with the player moved/rotated (e.g. the step 0013
  described); `simulate <dir> --keys <chars> [--render-each]` steps a capture
  forward. Replaying `input_history` was rejected: a snapshot is the state
  *after* the inputs, so replay would double-apply.
- **Mechanical re-bless review.** `explain-diff <dir> <ref>` prints each changed
  cell's old/new glyphs plus how the new render resolved it.
- **Two-sided fixtures.** `verify-issues` walks `issues/solved/*` and requires
  each render to match `screen.txt` and differ from `screen.pre-fix.txt`; a
  fixture that matches both proves nothing. It immediately caught a stale
  `solved/0004` render (pre-existing), now re-blessed. `AGENTS.md` records the
  rule: a regression test must be shown to fail on the pre-fix revision.
- `QuarterTurnsAnticlockwise`'s `AddAssign` no longer bypasses normalization
  (the derived impl accumulated unbounded, yielding snapshot values like 131);
  it now routes through `Add` so rotation stays 0..=3. Test:
  `quarter_turns_add_assign_stays_normalized`.

Verified: `cargo test --workspace` green (331 game lib tests); `cargo test -p
game --features debug-tools --lib` green (341); `./snapshot-tool verify-issues`
all 15 fixtures OK; flat `snapshot/` unchanged.

---

## 2026-10-09 — Per-belt and per-segment conveyor speeds (fixes 0016)

### game: give each conveyor-belt square its own movement period

Issue 0016: "each conveyor belt going a different speed", and one belt whose
segments ramp slow → fastest → slow. Belts were direction-only over one global
2s movement period, which drove all grid-entity steps, all floating-entity push
distances, and one global visual phase.

Each belt square now owns a `ConveyorBelt { direction, movement_period }`
(shorter = faster). `tick_conveyor_belts` steps a grid entity only when *that*
belt's period boundary is crossed and pushes floating entities by `speed *
delta`, each square independently; `draw_conveyor_belts` runs the visual phase
on each belt's own `2 * period`. Default-period belts are byte-identical.

- `MapOp::ConveyorBelt` gains an optional `speed` multiplier (1.0 = default);
  `Game::place_conveyor_belt_with_speed` / `Blocks::place_conveyor_belt_with_speed`
  author it.
- Snapshot round-trips a non-default `period_millis`; older captures and
  default belts omit it and load at the original speed.
- `maps/cubes.json` gives the runs different speeds (west 0.5, east 2.0,
  top-right 0.75) and ramps the vertical run's segments (0.25, 0.5, 0.75, 1.5,
  1.5, 0.75, 0.5, 0.25).
- Tests: `faster_conveyor_belt_steps_grid_entities_on_its_own_period`,
  `faster_conveyor_belt_pushes_floating_entities_farther`,
  `conveyor_belt_visual_phase_follows_its_own_period`,
  `map_file_parses_and_applies_ops` (a speed multiplier), the cubes recipe ramp
  assertions, and the snapshot round-trip with a non-default-speed belt.
- Captures: `issues/0016` moved to `issues/solved/0016` with
  `screen.pre-fix.txt` and a re-blessed `screen.txt`. The other captures are
  byte-identical (default belts render as before).
- Docs: `RENDERING.md` layer bullet updated.

Verified: `cargo test --workspace` green (330 game lib tests); `snapshot_tool
diff` OK for `snapshot/` and `solved/0003/0005/0006/0007`–`0016`.

---

## 2026-10-09 — Voxel walls are a distinct shade from voxel tops (fixes 0015)

### game: darken a tinted column's wall gradient endpoint

Issue 0015 (cubes map): a cube's side was the same color as its top — "it looks
like I should be able to step down here, but I can't." The wall gradient ran
from `TERRAIN_WALL_BASE` toward `terrain_tint(material)`, and for a `Tint`
material that returned the material color itself, so the wall reached exactly
the top face's light checker color at the top voxel.

`terrain_tint` now returns a darkened shade of the material (`WALL_TINT_SHADE`,
0.5) for `Tint`; `Floor` is unchanged. A column's exposed side is now clearly
darker than its top.

- Test: `raised_terrain_shifts_the_top_face_and_draws_a_wall` additionally
  asserts the wall color differs from the top color (fails before the change).
- Captures: `issues/0015` moved to `issues/solved/0015` with
  `screen.pre-fix.txt` and a re-blessed `screen.txt`; the other raised-terrain
  captures (`solved/0003`, `0007`–`0014`) re-blessed. Flat `snapshot/`,
  `solved/0005`, `0006` unchanged.
- Docs: `CUBE_ISO_PORT.md` post-port note updated.

Verified: `cargo test --workspace` green (327 game lib tests); `snapshot_tool
diff` OK for `snapshot/` and `solved/0003/0005/0006/0007`–`0015`.

---

## 2026-10-09 — Keep floor features layered so they rotate with content (fixes 0014)

### game: a conveyor belt under content rotates with the view

Issue 0014 (cubes map): with the view rotated 180°, the right glyph of the
player's square showed the belt going *up* while the belt run beside it went
down. The player is drawn over the belt via `Graphics::draw_above_square`, which
composited the belt's half-glyph into the arrow's `TextDrawable` immediately (in
world orientation). The FOV/terrain pass then rotates the composite for the
view, but `ArrowDrawable::rotated` only re-derives the arrow glyph, so the baked
belt half stayed unrotated.

`draw_above_square` now keeps a conveyor belt (or an existing stack) under new
content as a separate layer: a new `LayeredDrawable { under, over }` whose
`rotated` rotates both layers before compositing, while `to_glyphs` reuses the
same `drawn_over` composition (preserving the `PartialVisibility` /
`OffsetSquare` special cases). The belt half under the player now matches the
rotated run.

- Tests: `belt_under_the_player_rotates_with_the_view` (game; fails on the baked
  composite) and `belt_beneath_content_rotates_with_the_composite` (drawable).
- Captures: `issues/0014` moved to `issues/solved/0014` with
  `screen.pre-fix.txt` and a re-blessed `screen.txt`. The other captures are
  byte-identical.
- Docs: `RENDERING.md` drawable-abstraction section documents
  `LayeredDrawable`.

Verified: `cargo test --workspace` green (327 game lib tests); `snapshot_tool
diff` OK for `snapshot/` and `solved/0003/0005/0006/0007`–`0014`.

---

## 2026-10-09 — Portal-aware terrain draws every view a column is visible through (fixes 0013)

### game: draw raised terrain per relative cell, not per absolute column

Issue 0013 (cubes map): standing just inside a portal, the view "changed way too
much" when stepping through — the portal showed the floor's checkerboard and
cube sides/edges came and went. The forward terrain pass
(`Graphics::load_screen_buffer_from_terrain`) collapsed each absolute column to
its single *shallowest*-portal visibility and drew only there. On this map the
portal looks into cube clusters whose columns are also directly visible, but at
relative cells off the 35-row terminal; collapsing to the direct view drew those
columns nowhere, so the portal cell kept the flat composite's base board
checkerboard (red-tinted by portal depth — the "off" portal edge color). Making
those cells direct on the next step swapped the checker for real terrain.

The pass now walks the FOV's relative cells and, for each, draws the terrain
column named by `resolved_visibility` (the topmost view — exactly what the flat
composite paints) with that view's rotation and tint. A column is therefore
raised at *every* cell it is visible through, direct and portal alike.

- Test: `raised_column_visible_directly_and_through_a_portal_is_drawn_at_both_cells`
  (fails on the per-absolute collapse: the column is directly visible off-frame
  and through a portal on-frame, and the portal-cell rim was missing).
- Captures: `issues/0013` moved to `issues/solved/0013` with
  `screen.pre-fix.txt` and a re-blessed `screen.txt`; `solved/0007`–`0012`
  re-blessed (they gain the previously-missing portal terrain). Flat
  `snapshot/` and the non-cubes captures are unchanged.
- Docs: `RENDERING.md` FOV-compositing and `CUBE_ISO_PORT.md` post-port notes
  updated.

Verified: `cargo test -p game --lib` green (325 tests); `snapshot_tool diff`
OK for `snapshot/`, `solved/0007`–`0013`, `solved/0003/0005/0006`,
`touching-floating-square-background`.

---

## 2026-10-09 — Terrain walls gate on their own cell's view frame; belts keep their colors (fixes 0010/0011/0012)

### game: gate terrain walls by the wall cell's view frame; don't clobber a belt's background

Issues 0010/0011 (cubes map) reported a cube side visible through the portal
that vanished abruptly when the player stepped toward it; 0012 added that the
conveyor belts' non-white parts were missing. Both came from the forward terrain
column pass (`Graphics::load_screen_buffer_from_terrain`) added in `c3ba607`
(the 0007/0008/0009 fix), which was still incomplete in two ways:

- **Wall cells were unchecked.** A column's occlusion was validated only at its
  *top* relative cell (`resolved_visibility(visibility.relative_square())`), but
  the camera-facing wall is written up to `camera_altitude - z` rows below that
  cell (`apparent_cell + vec2(0, -(z - camera_altitude))`). On the raised cubes
  map those shifted cells can resolve through the portal to off-board void, so a
  directly visible cube's wall painted there anyway — the 0009 artifact class,
  surviving because the check never looked at the wall's own cell. Stepping
  toward the portal flipped the cells' resolution and the patch winked out.
  Each wall write now also requires the wall cell's topmost visibility to be in
  the *same view frame* as the column
  (`resolved_visibility(relative + toward_camera * (camera_altitude - z))`
  compared by `absolute_fov_center_square()`); a cell resolved to `None`
  (void/off-board) or to the same frame is still allowed, so a wall over the
  void beyond the board keeps rendering.
- **Belt backgrounds were clobbered.** The top-face branch set `bg_color = cube
  material` on every non-partial drawable, wiping a conveyor belt's intentional
  black background (`ConveyorBeltDrawable` colors `[WHITE, BLACK]`). A
  `ConveyorBelt` is now composited over a solid material base
  (`DoubleGlyph::drawn_over`) so its own colors win; plain tops and entities are
  unchanged.

- Tests: `terrain_walls_do_not_paint_through_a_portal_view` (loads the cubes
  recipe at player `(4,18)`, asserts the formerly-spurious portal cells carry no
  wall) and `conveyor_belt_on_a_raised_column_keeps_its_own_colors` (belt cell
  keeps `fg WHITE`/`bg BLACK`). Both fail against `c3ba607`.
- Captures: `issues/0010` (duplicate of 0011), `0011`, `0012` moved to
  `issues/solved/` with `screen.pre-fix.txt` and a re-blessed `screen.txt`;
  `solved/0007`, `0008`, `0009` re-blessed (the wall frame gate removes a few
  more spurious walls). Flat `snapshot/`, `solved/0003`, `0005`, `0006` are
  byte-identical.
- Docs: `CUBE_ISO_PORT.md` post-port note and `RENDERING.md` FOV-compositing
  section updated.

Verified: `cargo test --workspace` green (324 game lib tests); `snapshot_tool
diff` OK for `snapshot/` and every `solved/` capture.

---

## 2026-10-08 — Portal-aware raised terrain (fixes 0007/0008/0009)

### game: re-project raised terrain through portals and clip it to the FOV frame

The forward terrain column pass assumed flat, portal-free geometry in two ways,
both newly exposed once the default `cubes` map got portals (`ff23cf9`):

- **Not portal-aware.** `Graphics::load_screen_buffer_from_terrain` projected
  every column at its *absolute* `world_square_and_altitude_to_screen_buffer_square`
  and gated walls with `can_see_relative_square(column - root)`, which the
  portal recursion also satisfies. Raised geometry was thus painted at its true
  location even when that cell resolves through the portal to a different
  square — overwriting the red-tinted portal composite with untinted material
  and near-black walls (0008: "no red tint", "untinted blue floor", the
  white↔black edge diagonal) and showing nearby cube sides where the portal
  resolves to off-board void (0009).
- **Not frame-bounded.** The camera follows the player's surface (issue 0003),
  so a wall voxel at `z` is projected down by `camera_altitude - z` rows. The
  pass wrote those cells regardless, pushing walls up to 11 rows below the FOV
  frame (0007).

`load_screen_buffer_from_terrain` now takes `sight_radius` and, in
`graphics.rs`:

- builds `absolute square -> shallowest-portal PositionedSquareVisibilityInFov`
  from `at_least_partially_visible_relative_squares_including_subviews` (sorted
  for determinism) and draws each column's top and camera-facing wall at that
  visibility's *relative* screen cell, skipping a column that another
  (shallower) view wins at the same cell, matching the flat composite;
- applies the `0.1 * portal_depth` red tint (`tint_color`) to tops and walls,
  matching the flat composite; the wall's camera-facing direction is rotated by
  `portal_rotation_from_relative_to_absolute`;
- drops any write outside the `sight_radius + 1` FOV frame, leaving it to the
  starfield.

Call site (`game/mod.rs`) passes `self.player_sight_radius`.

- Tests: `raised_column_seen_only_through_a_portal_is_drawn_at_the_apparent_cell`
  (fails on the old absolute projection), `raised_terrain_is_clipped_to_the_fov_frame`.
  The existing raised-terrain/painter-order/frame-centre tests still pass.
- Captures: `issues/0007`, `0008`, `0009` moved to `issues/solved/` with
  `screen.pre-fix.txt` (original) and a re-blessed `screen.txt`;
  `issues/solved/0003/snapshot/screen.txt` re-blessed (portal-free board; only
  the frame clip changes it). Repo-root `snapshot/` is flat (`voxels == 0`) and
  stays byte-identical.
- Docs: `CUBE_ISO_PORT.md` Phase 2 caveat closed with a post-port note;
  `RENDERING.md` FOV-compositing section updated.

---

## 2026-10-08 — Portals and a belt-through-portal on the default cubes map

### game: three portal pairs on `cubes`; belt transport through a portal

The default scene gains three `double_sided_two_way_portal` ops (data only —
no new ops needed) plus a conveyor that rides through one:

- **P1 (belt-through, rotates right→up).** The east belt run (x20–27) ends on
  its entrance `(27,18)`; the widget emerges at `(33,30)` facing up onto a new
  `(33,30)…(33,34)` belt run. A rider widget (value 6) starts at `(20,18)` and
  visibly travels east, through the portal, then up.
- **P2** links the center cube `(19,21)` to the bottom-left top `(6,8)`.
- **P3** links the top-middle `(18,32)` to the middle-left `(6,18)`.
- Every one of the 12 faces (and its ±1 neighbors) lands on a solid cube top, so
  no reverse face emerges over void.

**Belt-push fix (`turns.rs`).** `simultaneously_push_several_grid_entities`
inserted a belt's end square into `push_end_squares` even when nothing moved
there. An *empty* belt processed before a following belt would reserve that
following square and skip it, stalling a rider depending on `HashMap` iteration
order (observed as intermittent stalls on the new belt run). The reservation now
happens only when `try_push_grid_entity` actually returns `Ok`.

- Tests: `default_cubes_map_belt_carries_a_widget_through_a_portal` ticks the
  east run to the entrance (7 ticks), crosses on the 8th, and continues up on
  the 9th; `default_map_is_the_cubes_recipe` now expects 6 widgets and asserts a
  belt sits on both the portal entrance and its exit.
- Verified with `map_diagram cubes` (all 12 faces listed) and
  `cargo test --workspace` green.

---

## 2026-10-08 — Widgets push and the player can blink on the cubes map

### game: classify grid blockers relative to the player's altitude

On the raised `cubes` map the player could not push widgets or blink. Both
symptoms had one cause: the grid-entity and empty-square checks used
`is_block_at`, the *flat* solidity test (`solid at z = SLAB_TOP`). Every cube
column is solid at z=0, so once the player stood on a cube top:

- `get_grid_entity_at_square` returned `Block` for the widget's square, so
  `try_push_grid_entity` refused it (`mod.rs:315`).
- `square_is_empty` returned false for every cube-top square, so `player_blink`
  never advanced past the start (`mod.rs:785`).

`player_can_stand_at` already encodes the right rule for the player (a
destination is standable when its surface is not above the player's). New
`square_blocks_player` is its negation (void included), i.e. the player-relative
form of `is_block_at`; `get_grid_entity_at_square` and `square_is_empty` now use
it. On flat maps (player altitude 0) this reduces to the old `is_block_at`, so
existing gameplay is unchanged.

- Demo tweak: the center-cube widget moved from `(17,18)` (on a belt) to
  `(17,17)` so it stays put until the player pushes it onto the belt line.
- Tests: `default_cubes_map_widget_can_be_pushed` walks the player beside the
  center cube's widget and asserts it slides one square; 
  `default_cubes_map_player_can_blink_on_a_cube_top` blinks the full range across
  the top. Both fail against the `is_block_at` check.

Verified: `cargo test --workspace` green (319 game lib tests).

---

## 2026-10-08 — Default cubes map gets bridges, widgets, and belts; depth fog removed

### game: bridge/widget/belt map ops; drop terrain fog

Three requested changes to the default `cubes` scene:

- **Bridges.** `maps/cubes.json` now places twelve 3-wide, 1-voxel-thick bridge
  decks (`z=9`, so their tops sit at altitude 10, level with the cube tops)
  across the 2-square void gaps, joining all nine cubes into a walkable grid.
  No code was needed — the existing `cuboid` op expresses them.
- **Widgets & belts on the recipe.** `MapOp` gains `Widget { x, y, value }` and
  `ConveyorBelt { x, y, dir }` (`map_file.rs`), delegating to
  `place_widget` / `place_conveyor_belt`. The default map now seeds five
  pushable widgets across five cube tops and three conveyor runs (center cube
  east/west toward the bridges, and a bottom-left→middle-left run) so the
  machines are reachable without any Rust-authoring. Snapshot serialization
  already covered both, so no snapshot change.
- **Fog removed.** The raised-terrain pass tinted walls and tops by distance
  from the screen center (`terrain_fog`, `FOG_SPAN`, `FOG_MIN`,
  `terrain_apply_fog`), which on a void map read as a spotlight centered on the
  player. `load_screen_buffer_from_terrain` now uses the raw wall gradient and
  material top color; the helpers and their test are gone. `wall_gradient_t`
  and `scale_rgb` (checker shade) remain.
- **Tests.** `map_file_parses_and_applies_ops` also parses a widget and a belt;
  `default_map_is_the_cubes_recipe` checks bridge heights, that a gap corner
  stays void, the occupied-column count (`9*100 + 12*6`), and the widget/belt
  counts.
- `issues/solved/0003/snapshot/screen.txt` re-blessed to the fog-free render
  (the cubes map is the raised-board capture); `docs/CUBE_ISO_PORT.md` fog rows
  marked removed.

Verified: `cargo test --workspace` green (317 game lib tests); `snapshot_tool
diff` OK for `snapshot/`, `issues/solved/0003`, `0005`, and `0006`.

---

## 2026-10-07 — Starfield parallax turns with the portal frame (0005 follow-up)

### game: apply the view frame's rotation to the starfield (issue 0005)

The first per-frame starfield pass (entry below) anchored each portal frame's
stars at that frame's root but placed them at `frame_offset + (root - player)`
with no rotation. The FOV's actual promise is
`abs = root + rotate_q(main_relative)`, i.e.
`main_relative = rotate_{-q}(frame_offset)` — a pure rotation, no translation
(a q=1 frame maps frame offset `(0,15)` to primary `(15,0)`; verified against
`snapshot_tool explain`). So the first pass mislocated every portal-frame star
and dropped the ones whose shifted cell left the sight radius. Parallax
*orientation* matters, not just position.

- **FOV.** `view_frame_roots()` becomes `view_frames() -> Vec<(WorldSquare,
  QuarterTurnsAnticlockwise)>`, composing `view_transform_to(..).rotation()`
  recursively. The gate now matches the cell's topmost frame root *and* its
  `portal_rotation_from_relative_to_absolute`.
- **Starfield.** New `frame_to_primary_offset(frame_offset, rotation) =
  (-rotation).rotate_vector(frame_offset)`; `visible_anchor_bounds` rotates the
  screen corners by `+rotation`; the `(root - player)` shift is gone. q=0 is
  the identity, so portal-free output stays byte-identical.
- **Tests.** `frame_rotation_maps_parallax_back_to_primary_space` (exact unit
  test); `stars_seen_through_a_portal_use_the_destination_frames_camera` now
  checks *both* a translating (q=0) and a rotating (q=1) frame at screen
  rotations 0 and 1 — a portal frame's stars must match the direct view from its
  root at `rotate_q(primary relative)`, skipping board-edge cells where the
  occupancy rounding legitimately differs. Fails with either the old player
  camera or a rotation-less mapping.
- **Issue 0006.** The rotation-less intermediate pass also produced issue 0006
  (moving left/right moved southern stars up/down). The capture matches the old
  render to within 4 cells and the corrected render differs by 129; archived to
  `issues/solved/0006` with its pre-fix render.
- `snapshot/`, `issues/solved/0005/snapshot` and `issues/solved/0006/snapshot`
  re-blessed; `docs/RENDERING.md` updated.

Verified: `cargo test --workspace` green; `snapshot_tool diff` OK for
`snapshot/`, `issues/solved/0003/snapshot` (portal-free),
`issues/solved/0005/snapshot`, and `issues/solved/0006/snapshot`.

---

## 2026-10-07 — Starfield camera follows the portal frame; fix 0005

### game: paint the starfield once per FOV frame (issue 0005)

Issue 0005: stepping right through the demo's portal bank made the stars off to
the right abruptly shift. The starfield anchored its parallax camera on the
player's absolute square (`screen_center_as_world_square`). The board is
composited through the portal-aware FOV, so a portal crossing keeps the view
continuous, but the player's absolute square jumps by the portal displacement
(`(23,12)` -> `(28,17)`, a `(5,5)` step). The star lattice therefore moved by
`parallax * displacement` (up to ~3 world squares on the near layer) instead of
the one apparent step.

- **FOV.** `FieldOfViewResult::resolved_visibility` returns the topmost
  visibility (and the frame it was reached through);
  `resolved_absolute_square` is now a thin wrapper. `view_frame_roots()`
  collects every frame root (primary + recursive sub-views), sorted and deduped.
  (Superseded by `view_frames`, see the entry above.)
- **Starfield.** `Starfield::draw` paints once per view frame with that frame's
  root as the camera, and keeps a star only when the cell's topmost visibility
  frame root matches — so the sky is anchored where it is actually seen. No
  portals => a single frame rooted at the player => byte-identical output.
  (The `(frame_root - player)` translation from this first pass was wrong; see
  the entry above.)
- **Tests.** `stars_seen_through_a_portal_use_the_destination_frames_camera`
  and `issues/solved/0003/snapshot` (portal-free) diffs clean.
- `issues/0005` archived to `issues/solved/0005` with its pre-fix render kept as
  `screen.pre-fix.txt`; `docs/RENDERING.md` starfield section updated.

Verified: `cargo test --workspace` green; `snapshot_tool diff` OK for
`snapshot/`, `issues/solved/0003/snapshot`, and `issues/solved/0005/snapshot`.

---

## 2026-10-07 — Re-bless the 0004 and touching captures to their fixed renders

### issues: bless resolved captures; keep the pre-fix render beside them

The `0004` (stars through a portal) and `touching-floating-square-background`
(shape-preserving composite) fixes changed their headless renders, so their
`solved/` captures were blessed to match. The originals are preserved as
`screen.pre-fix.txt` (missing stars; flooded left half). `0003` was blessed the
same way when it was fixed.

The `black-*` and `player-border-rotation` captures predate the current renderer
and are kept as design records rather than re-blessed.

---

## 2026-10-07 — Retire the leftover-portal artifact and archive the resolved captures

### issues: archive 0003/0004/touching, retire leftover-portal

All four captures now in `issues/` are resolved, so they move to
`issues/solved/` (numbers stay reserved per `issues/README.md`):

- `0003` — cubes framing + edges fixed by the camera-altitude/rim change and
  the `sight_radius` round-trip.
- `0004` — stars through a portal fixed by the FOV-resolved occupancy test.
- `touching-floating-square-background` — fixed by the shape-preserving
  compositing.
- `leftover-portal-rendering-artifact-after-jump` — retired as
  non-reproducible: no headless code path produces the red-tinted floor, the
  starfield cannot flicker (pure function of screen/board/time), and the two
  deterministic paths that could have were already fixed. Documented in its
  note; a live pty capture would be needed to chase the terminal-side case.

`issue_number_names` still scans both directories; the next number stays 5.

---

## 2026-10-07 — Camera follows the player's surface altitude; cube tops get a drop-off rim

### game: player-relative camera altitude and a terrain rim (issue 0003)

The `cubes` map's big cubes projected out of the top of the frame because the
camera ignored the player's altitude. The player stands on a 10-tall cube, but
`update_screen_from_draw_buffer` centred the camera on the player's *ground*
square while terrain is projected upward by `-altitude`: the player glyph sat 10
rows above the frame centre and the north cubes left the top of the border.

- **Camera altitude.** `Screen` gains `camera_altitude`;
  `world_square_and_altitude_to_screen_buffer_square` subtracts
  `altitude - camera_altitude`. It is set to `player_altitude()` each draw, so
  the player's surface lands at the frame centre. Flat boards (altitude 0) are
  byte-for-byte unchanged (`snapshot/` diff still OK).
- **Rim.** Exposed top faces get a bright `UPPER_HALF_BLOCK` rim on their far
  (screen-up) drop-off edge (`terrain_rim_color`), so a raised cube reads as a
  cube rather than a flat dark patch. Only plain tops (no glyph of their own)
  are rimmed.
- Tests: `player_renders_at_the_frame_center_on_a_raised_column`; the
  raised-terrain top-face test now expects the rim half-block.

The `issues/0003` capture was re-blessed to the fixed render (original kept at
`screen.pre-fix.txt`).

---

## 2026-10-07 — Snapshots round-trip the player sight radius

### game: serialize/restore `sight_radius` so map overrides survive a capture

Issue 0003 (map `cubes`) re-rendered 1488 cells wrong because the headless
loader fell back to `PLAYER_SIGHT_RADIUS = 16` while the map overrides the
radius to 24 (`maps/cubes.json`). The snapshot had no field for it, so the FOV
border and starfield were eight squares too small on every side.

- `game_state_json` emits `sight_radius`; `SnapshotData` reads it as an
  `Option<u32>` (absent in older captures → default), and `from_snapshot`
  calls `set_player_sight_radius`.
- Backfilled `issues/0003/snapshot/game_state.json` with `"sight_radius": 24`
  (the value the live run used, confirmed by the captured border at radius 25);
  `snapshot_tool diff issues/0003/snapshot` is now `OK`.
- Regression test `snapshot_round_trips_sight_radius`.

The committed `snapshot/` fixture predates the field and still loads on the
default-16 fallback, so it doubles as the backward-compat case.

---

## 2026-10-07 — Opaque glyphs no longer flood a partial below shape with its ink color

### game/terminal_rendering: preserve a content drawable's background under an opaque glyph

Issue `touching-floating-square-background`: the player arrow over a remapped
death cube rendered its left half entirely in the death color and let a lower
block through on the right. In `Glyph::drawn_over`
(`crates/terminal_rendering/src/glyph.rs`), when the top glyph had ink and a
transparent background but could not be combined with the below character, the
background was filled with the below glyph's **ink** color, discarding the
below shape. For the player over the death cube that flooded the half.

- `Glyph::drawn_over_preserving_below_shape` (and the `DoubleGlyphFunctions`
  counterpart) keep the below shape's *background* when the top is transparent,
  falling back to the ink color only when the below glyph is itself
  background-transparent.
- `TextDrawable::drawn_over` routes through it only when the below drawable is
  content — `OffsetSquareDrawable` or `PartialVisibilityDrawable`. UI markers
  (danger/move squares) keep the old recolor-the-cell compositing, so
  `test_protected_piece_has_fully_colored_background` and
  `test_portal_drawn_in_correct_order_over_partially_visible_block` still hold.
- Regression test `test_text_over_partial_shape_keeps_below_background` pins the
  failing direction (it fails against the old `bottom.fg_color` fallback).

The right half still shows the death cube's actual lower-block coverage; only
the spurious whole-half fill is gone.

---

## 2026-10-07 — Stars through portals: resolve board-vs-void at the square actually seen

### game: starfield decides void from the FOV-resolved square, not the naive projection

Issue 0004: looking north through the demo's portal bank, the void beyond the
board edge showed no stars. `Starfield::draw` tested occupancy with
`screen_buffer_character_square_to_world_square`, which knows nothing about
portals. For a cell whose content comes through a portal, that naive square is
on-board and occupied, so `occupied.contains(&world_square)` culled the star
even though the FOV resolved the cell to an off-board square
(`graphics/starfield.rs:163`).

- **Model.** Added `FieldOfViewResult::resolved_absolute_square(relative)`,
  returning the absolute square of the topmost visibility in draw order (the
  same order the renderer uses), or `None` when the relative square is unseen.
- **Starfield.** The occupancy gate now runs on the resolved square (falling
  back to the naive square when there is no visibility), while the FOV-visibility
  and `drawn` gates are unchanged, so board floor is still never overwritten.
- **Tests.** `stars_are_drawn_through_a_portal_over_off_board_void` builds a
  small board with a double-sided north portal and asserts a star renders in a
  cell that is off-board only through the portal. All existing starfield tests
  and the blessed `snapshot/` diff stay green.

`snapshot_tool diff issues/0004/snapshot` grows by the previously-missing stars;
the remaining delta is transient animation/selector state the loader does not
restore.

---

## 2026-10-07 — Archive the three fixed issues; retire the rotated-border lost-look note

### issues: move black-block, black-diagonal-seam, player-border-rotation to solved/

`issues/` is meant to hold only open captures (`issues/README.md`), but three
entries there were already fixed:

- `black-block-deep-in-portal` and `black-diagonal-portal-seam` carry
  "Status: fixed" and their regression tests
  (`test_portal_slice_arcs_union_to_full_visibility`,
  `test_stacked_portal_slices_union_to_full_visibility`,
  `stacked_portal_seam_has_no_out_of_sight_partial`).
- `player-border-rotation` was fixed by the screen-space `UiLayer`
  (`border_is_independent_of_camera_rotation`) but its note was never updated.

Moved all three under `issues/solved/` (numbers stay reserved per
`issues/README.md`). Updated the stale `player-border-rotation` note with its
resolution, recorded its "save the rotated frame as reference" wish in
`docs/vision/ideas.md`, and repointed the `docs/ROADMAP.md` Done-section links
and two code comments at the new `issues/solved/…` paths.

Verified: `issue_number_names` still scans both directories (next number
stays 5); full suite green.

---

## 2026-10-07 — Default map becomes a data-defined `cubes` recipe; the old demo moves to `portals-and-death-cubes-demo`

### game: `maps/cubes.json` default; port the demo to a recipe and add portal/turret ops

The no-`--map` default used to be `Game::set_up_demo_map` in Rust. The default
is now the data-defined `maps/cubes.json` (a 3×3 grid of 10×10×10 cubes over
void, one voxel apart), and the old demo scene moved to the recipe
`maps/portals-and-death-cubes-demo.json`.

- **New recipe ops.** `MapOp` gains `double_sided_two_way_portal`
  (`entrance`/`entrance_dir` + `exit`/`exit_dir`, with a serde `MapDir`) and
  `death_turret` (`x`/`y`), so recipes can express portal geometry and turrets
  that previously required Rust.
- **Demo ported.** `set_up_demo_map` is deleted; its 40×24 floor, 11
  double-sided two-way portals, and death turret at `(6,12)` are transcribed
  into `maps/portals-and-death-cubes-demo.json`. A golden test pins the exact
  44 portal-entrance `sort_key`s and the turret square, so the recipe stays
  byte-faithful to the deleted built-in.
- **Default.** `set_up_map_by_name` resolves `None` to `DEFAULT_MAP_NAME`
  (`"cubes"`), which loads like any other recipe; the built-in `demo` arm is
  gone. `--map demo` is replaced by
  `--map portals-and-death-cubes-demo`.
- **Tests.** `built_in_maps_seed_the_floor_to_their_board` drops the now-recipe
  demo; `demo_map_has_no_floor_past_its_edges` →
  `demo_recipe_has_no_floor_past_its_edges`;
  `flat_built_in_map_is_the_default_floor` → `flat_recipe_map_is_the_default_floor`;
  new `default_map_is_the_cubes_recipe` and
  `portals_and_death_cubes_demo_recipe_matches_the_golden_layout`.
- **CLI/docs/scripts.** Usage strings in `main.rs`/`map_diagram.rs` and the
  snapshot issue label list the new names; `maps/cubes.sh` and
  `maps/portals-and-death-cubes-demo.sh` launch the two new maps.

Verified: `cargo test` workspace green (311 lib tests among them);
`map_diagram cubes` and `map_diagram portals-and-death-cubes-demo` render.

---



### issues: restore solved 0001/0002, move the reused issue to 0003

The two issues solved by the built-in-map floor fix were deleted rather than
archived, so the next capture reused number 0001. Now that numbers are never
reused, restore the record and fix the collision:

- `issues/solved/0001/` and `issues/solved/0002/` recreated from the
  `docs/CHANGELOG.md` descriptions (demo top-edge `(19, 23)` could not move up;
  right-edge `(39, 13)` could not move right). Their snapshots were lost with
  the deletion, so only the notes are restored — the numbers are what matter for
  non-reuse.
- The reused capture is renumbered `issues/0001` → `issues/0003` (its `issue.md`
  header updated), the next number once 0001/0002 are reserved.

Verified: `issue_number_names` now scans to next = 4.

---

## 2026-10-06 — Solved issues are archived; issue numbers are never reused

### game: reserve issue numbers across the `issues/solved/` archive

A capture after deleting a solved issue reused its number: `next_issue_number`
was one past the highest name *currently* in `issues/`, so removing the
highest-numbered (or only) issue made the next capture take that number again.
Issue numbers are references — `docs/CHANGELOG.md`, tests, and notes point at
them — so reusing one silently repoints those references.

- **Policy.** Solved issues are now moved to `issues/solved/` instead of
  deleted, so the record of an assigned number survives. `issues/README.md`
  updated; `issues/solved/README.md` documents the archive.
- **Numbering.** New `issue_number_names(&issues)` gathers candidate names from
  both `issues/` and `issues/solved/`; `create_new_issue` feeds them to
  `next_issue_number`, which stays `max + 1`. `next_issue_number`'s doc now
  states the caller must include archived numbers, and its old "smallest unused"
  wording is corrected to "one past the highest".
- **Tests.** `archived_issue_numbers_are_still_reserved` builds `issues/0001`
  plus `issues/solved/0002` and asserts the next number is `3`; it fails against
  the previous top-level-only scan (which returned the reused `2`).

Verified: `cargo test -p game --lib` 309 passed / 6 ignored.

---

## 2026-10-06 — Re-seed each built-in map's floor to its own board (issues 0001/0002)

### game: built-in maps re-derive their floor slab; AGENTS debugging discipline

Issues 0001 and 0002 were the same bug at different edges: the player walked to
the demo board's top edge (`(19, 23)`) and couldn't move up, and to the right
edge (`(39, 13)`) and couldn't move right, with no visible reason.

`Game::new` seeds the floor slab for the *terminal-derived* board
(`terminal_width / 2 × terminal_height`). Built-in maps then override
`board_size` (demo 40×24, racetrack/hallways 48×26, numbered-boxes 20×20)
without re-deriving the slab, so on a wide terminal (the captures were
353×62) the floor stayed 176×62. The renderer paints the floor for every
occupied terrain column, while movement is gated by `square_is_on_board(board_size)`
— so walkable-looking floor was drawn past the edge and then silently refused.
On a narrow terminal the mismatch inverts: part of the board is void.

- **Re-seed on map setup.** Added `Game::seed_board_floor_for_current_board`
  and call it right after each built-in map sets its `board_size`, so the slab
  always matches the board. `seed_board_slab` only rebuilds the `z = -1` layer,
  so placed geometry is untouched.
- **Harden recipes.** `apply_map_file` now seeds the default floor for a
  recipe's board before honoring `clear_floor`, so a recipe that shrinks the
  board can't inherit the terminal slab either.
- **Snapshot.** A bare map's slab is now the full board rect, so snapshots omit
  the (previously always-present) `floor_cells` override.
- **Tests.** `built_in_maps_seed_the_floor_to_their_board` checks every
  built-in across a mismatched terminal; `demo_map_has_no_floor_past_its_edges`
  pins the two reported coordinates; `flat_built_in_map_is_the_default_floor`
  asserts the compact serialization.
- **AGENTS.md.** Added a repo "Debugging discipline" section: data before
  code, reconnoiter `git diff`/CHANGELOG and fixtures first, state one
  falsifiable invariant, and group numbered reports.
- Deleted the solved `issues/0001` and `issues/0002`; re-blessed the local
  (gitignored) `snapshot/` fixture now that its floor is board-sized.

Verified: `cargo test -p game` green; `snapshot_tool diff snapshot/` OK.

---

## 2026-10-06 — Fix one-keypress input lag from the pausable reader

### game: read input unbuffered so `poll` and `read` agree

Follow-up to the Ctrl-P capture work. Holding an arrow and then switching
direction applied the *previous* direction one step late.

`PausableInput::read` polled the tty fd for readability but read through the
buffered `std::io::stdin()`. Termion parses a multi-byte key in two reads (2
bytes, then the tail), and `Stdin`'s `BufReader` had already drained the tail
into user space, so `poll` saw an empty kernel buffer and the loop never drained
the buffer. The sequence completed only when the next key made the fd readable —
delaying every arrow/function/mouse sequence by one keypress.

Read directly from the fd with `libc::read` (retrying `EINTR`, `Ok(0)` on EOF)
so `poll` and the read observe the same buffer. New regression test writes a
whole `ESC [ A` "Up" sequence into a pipe and asserts termion yields `Key::Up`
without a follow-up key; it fails against the buffered read.

Verified: `./run-tests` 613 passed / 9 skipped.

---

## 2026-10-06 — Ctrl-P files live snapshots as numbered issues with a typed note

### game: bind capture to Ctrl-P; auto-create a numbered issue and edit its note

`p` was too easy to fat-finger, and a capture still required exiting the game,
making an issue directory, copying the snapshot in, and hand-writing a note.

- **Hotkey.** `SNAPSHOT_KEY` is now `Key::Ctrl('p')`.
- **Issue per capture.** `create_new_issue` scans `issues/` for purely numeric
  names, takes max+1, and creates `issues/NNNN/snapshot/` plus a templated
  `issue.md` (number, map, UTC date, Description section). `write_snapshot_to`
  writes the three snapshot files into a given directory; `next_issue_number`,
  `civil_from_days`, and the template are unit-tested.
- **Editor.** On capture the game pauses, drops its raw-mode/alternate-screen
  writer to restore the terminal, runs `$VISUAL`/`$EDITOR` (fallback `vi`) on
  the note, then recreates the terminal, forces a full repaint, and resumes.
  The suspended wall time is added to the logical-time epoch so the world clock
  and animations stay continuous instead of jumping.
- **Input reader parking.** Spawning an editor while the background input
  thread is blocked reading the tty would split keystrokes between them. The
  reader (`PausableInput`) now polls stdin with `libc::poll` (new `libc`
  dependency) and parks on a `Condvar` while `InputPause` holds it, with a
  parked-handshake before the editor starts; `Screen::force_redraw` repaints
  after the terminal is rebuilt.
- README snapshot section updated; the repo-root `snapshot/` directory remains
  for `snapshot_tool` and manual dumps.

Verified: `./run-tests` 611 passed / 9 skipped.

---

## 2026-10-06 — Screen-space UI layer; FOV border stops rotating with the view

### game: add a screen-space `UiLayer` and move the FOV border into it

The FOV border rotated with the player's view because it was authored in
player-relative world squares and projected through `Screen`'s camera
(`rotation`). There was no UI layer to put it in — one `Screen` owned both the
framebuffer and the camera. This adds the missing layer.

- **`terminal_rendering::ui_layer::UiLayer`.** A character-resolution glyph
  grid in the same frame as `Screen`'s buffer (origin top-left, y down) with no
  camera attached. Cells start `Glyph::transparent_glyph()`; `composite_onto`
  skips transparent cells and `drawn_over`s the rest, so opaque UI overwrites
  and transparent UI leaves the world pass byte-for-byte untouched. Draw
  helpers (`draw_glyph`, `draw_double_glyph`, `draw_string`) take screen
  character/square coordinates and clip off-screen.
- **Frame order.** `Graphics` owns the layer (`clear_ui` / `ui_layer` /
  `composite_ui`). `update_screen_from_draw_buffer` now fills the world pass,
  clears and draws UI, composites it over `screen_buffer`, then draws the
  debug overlays (still topmost) and `display`s.
- **FOV border.** `graphics/fov_border.rs` paints into the UI layer, positioned
  by screen offset from the player's screen square, choosing glyphs from screen
  axes and ignoring `screen.rotation()`. Output is unchanged at rotation 0 and
  no longer turns under q/e. New `border_is_independent_of_camera_rotation`
  test compares the layer byte-for-byte across all four rotations.
- `snapshot/` re-blessed: it was captured at `rotation_quarter_turns: 3`, where
  the old border had its decorated top edge on a side.

Full suite green: game 300 passed / 6 ignored (26 in the playground bin),
terminal_rendering 153 passed / 1 ignored; `snapshot_tool diff snapshot/` OK.

---

## 2026-10-04 — Voxel-grid board, data-defined maps, and the `space-cubes` demo map

### game: voxel-grid board + JSON map recipes + `space-cubes` map

Turns the board into an explicit voxel grid and recreates the deleted
`cube_iso` demo as a data-defined map.

- **Voxel-grid board.** `Terrain` gains `fill_floor_rect` / `clear_floor` /
  `floor_squares` / `occupied_squares` / `slab_voxels`; the `z = -1` floor is a
  convenience a map opts into. Void renders as starfield and blocks movement.
  Starfield now paints by occupancy, not the board rectangle.
- **Walkability by surface altitude.** A move is allowed if the destination has
  ground and is not *higher* than the player's current surface (derived from the
  column, so it follows the player down steps). On floor maps a block is a step
  up → still a wall, so existing gameplay is unchanged. Void is blocked.
- **Altitude-aware sight.** `Game::fov_blockers` only counts columns whose top
  rises above the player, so the player can see across a cube top it is on.
- **Snapshot.** `floor_cells` records the `z = -1` floor only when it differs
  from the default full rect; legacy snapshots load the full floor.
- **JSON map recipes.** `game/map_file.rs`: `maps/<name>.json` is a list of
  `cuboid` / `column` / `voxel` / `cube_side_platforms` / floor ops plus board
  size, sight radius, and player start; `set_up_map_by_name` prefers a recipe
  and falls back to the built-ins. Board size is now owned by the map, not the
  terminal; `do_everything`'s per-map terminal clamp is gone and the built-in
  maps (`demo`, `racetrack`, `hallways`) set explicit boards.
- **`space-cubes` map.** `maps/space-cubes.json` + `maps/space-cubes.sh`
  recreate the demo: four 10×10×10 cubes, their south staircases and east/west
  ledges, player on the first cube top, real void between.
- Deferred gravity/falling documented in `ROADMAP.md`.
- `docs/VOXEL_WORLD_PLAN.md` records the work plan and status so it can be
  resumed after an interruption.

Verified: workspace tests green (new terrain/walkability/map-file/snapshot/
space-cubes tests); `snapshot_tool diff snapshot/` still matches.

---

## 2026-10-04 — Finish the `cube_iso` port (phases 4/6, verify) and delete the demo

### game: floating-entity altitude, height tooling, ported tests; remove cube_iso

Completes `docs/CUBE_ISO_PORT.md`:

- **Phase 4 — floating-entity altitude.** `DeathCube`/`FloatingHunterDrone`
  gain a static `altitude` (voxels, default 0), snapshotted as `altitude`.
  A non-zero-altitude entity is kept out of the planar draw buffer and
  composited by `Graphics::overlay_floating_entities_at_altitude` at its
  shifted screen square (FOV-gated, no ground-level ghost). Altitude 0 is
  byte-identical to before.
- **Phase 6 — tooling.** `snapshot_tool heights <dir>` prints a loaded map's
  per-column top altitude; `map_diagram` already prints heights. Demo HUD/CLI
  dropped with the crate.
- **Verification.** Ported tests: `checker_light` 3-square parity,
  `terrain_fog` monotonicity/floor, painter-order regression (a nearer column
  overwriting a farther wall), and the projection/altitude tests added earlier.
- **Deletion gate.** Removed `cube_iso/` from disk and from the workspace
  `members`; statuses in the port doc are now `ported`/`native`/`deferred`/
  `testbed-only` with no `pending` left. The doc is marked Done.

Verified: full workspace tests green; `snapshot_tool diff snapshot/` still
matches.

---

## 2026-10-04 — Port phase 3: terrain materials (`cube_iso`)

### game: per-column terrain materials, checker tops, wall gradient, fog (cube_iso port phase 3)

- `TerrainMaterial { Floor, Tint(RGB8) }` stored per column in `Terrain`
  (overrides only; defaults derived from whether a column is built up).
  `place_solid_column_with_material` / `set_terrain_material` choose a tint at
  creation; `place_block` keeps its legacy look. `set_up_terrain_demo` uses
  several tints.
- `Graphics::load_screen_buffer_from_terrain`: fully-visible top faces get the
  material as their **background** only (block/piece/player glyphs untouched);
  `Floor` resolves to the existing board pattern (flat boards unchanged), a
  tint uses the 3-square `(x,y,z)` checker. Walls use a base→tint gradient.
  Terrain and walls are depth-fogged; `Floor` is not, so the board reads as
  before. Partially-visible tops keep their existing shadow rendering.
- Only exactly-one-voxel columns draw the flat block glyph now
  (`single_height_block_squares`); taller columns render as material, so their
  tops are no longer hidden by the block fill.
- Snapshot: `materials` (`[x, y, r, g, b]`, `Floor` encoded as `-1` color),
  defaults re-derived on load.
- The demo's warm-ledge/standoff-hue system is intentionally dropped (a tint is
  chosen at creation); the bright rim is not ported.
- Tests: default material derivation, override serialization, red-tint top, and
  the existing raised-terrain/flat-gate/snapshot round-trip tests.

Verified: workspace tests green; `snapshot_tool diff snapshot/` still matches.

---

## 2026-10-04 — Port phase 5: view rotation (`q`/`e`) (`cube_iso`)

### game: bind q/e to view rotation; quit on Esc/Ctrl-C (cube_iso port phase 5)

- `Game::rotate_view(quarter_turns)` rotates `Screen::rotation`; the FOV cache
  is rotation-independent, so no invalidation needed.
- `InputMap`: `q`/`e` rotate counter-clockwise/clockwise; quit moved from `q` to
  `Esc`/`Ctrl-C`. Movement is already screen-relative, so it follows the view.
- Test: `q` then `e` returns to the starting quarter turn; `Esc` quits.
- Docs updated: phase 5 + projection/rotation inventory rows marked ported.
  The optional `DebugOverlayFlags` facing marker is not added.

---

## 2026-10-04 — Port phase 2: z projection + forward terrain column pass (`cube_iso`)

### game: render raised terrain with a z-forward column pass (cube_iso port phase 2)

Second step of the `cube_iso` port (`docs/CUBE_ISO_PORT.md`). Adds the visual
altitude layer while keeping the flat renderer byte-for-byte:

- `Screen::world_square_and_altitude_to_screen_buffer_square` — altitude shifts
  a square up one row per voxel, independent of view rotation (projection P's z
  term). Unit-tested upright and under rotation.
- `Terrain::columns` / `max_top_altitude` expose the render walk and the gate.
- `Graphics::load_screen_buffer_from_terrain` — a painter-sorted (far-to-near)
  forward column pass drawing top faces, camera-facing walls, and the slab edge.
  Top faces reuse the FOV/draw-buffer lookup, so visibility, partial shadows,
  and entity overlays still apply; walls use a solid placeholder color until the
  material phase.
- `Game::update_screen_from_draw_buffer` picks the pass only when
  `max_top_altitude() > 1`, so flat/block-only boards keep the legacy inverse-FOV
  path. On a raised board the legacy inverse composite still runs underneath the
  pass, preserving portal views of the floor and entities (its per-cell
  `drawn_over` compositing) and the starfield `drawn` contract; only the raised
  geometry is not yet re-projected through portals.
- Tests: raised column draws its top three rows up with a wall below; a
  single-height block still renders at its flat square.
- `map_diagram` now prints terrain height: `.` bare board/void, `#`
  single-voxel block, digits for taller columns (mod 10).

Verified: workspace tests green; `snapshot_tool diff snapshot/` still matches.

---

## 2026-10-04 — Port phase 1: terrain/voxel data model (`cube_iso`)

### game: add voxel/altitude terrain model (cube_iso port phase 1)

First step of the `cube_iso` port (`docs/CUBE_ISO_PORT.md`), data model only —
no rendering change, so the flat gate holds.

- New `utility` types `WorldVoxel`/`WorldPoint3`/`VoxelSet`.
- New `game/terrain.rs`: a `VoxelSet` + per-column top cache with
  `place_voxel` / `place_solid_column` / `is_solid_at` / `height_at` and a
  **materialized board slab** (one voxel per on-board square at index `-1`, top
  surface altitude 0). The slab is regenerated from the board size, not
  serialized.
- Folded blocks into the voxel set: removed `Blocks.blocks`; `place_block` is
  now a one-voxel column and `Game::block_squares()` (solid at altitude 0) is
  the gameplay/LOS query. All render/FOV/diagram call sites use it, so flat
  output is byte-identical.
- Snapshot: emit sorted `voxels` (`[x,y,z]`, slab excluded); still read the
  legacy `blocks` field and load each as a single-height column. Loader re-lays
  the slab for the captured board size.
- Added `Game::set_up_terrain_demo` and terrain/snapshot round-trip + legacy
  migration tests.
- `CUBE_ISO_PORT.md` gained an "Environment model" section recording what the
  voxel layer owns (terrain solidity/altitude) versus what it does not (portal
  topology, 2D FOV, single-surface gameplay), plus the invariants that hold
  until full 3D occlusion.

Verified: workspace tests green; `snapshot_tool diff snapshot/` still matches.

---

## 2026-10-04 — Record the `cube_iso` porting checklist and plan

### docs: add `CUBE_ISO_PORT.md` (inventory + phased plan + deletion gate)

The `cube_iso/` prototype has reached the point where its z/altitude rendering
should move into the main game rather than keep growing its own HUD and debug
surface. New `docs/CUBE_ISO_PORT.md` is the hub for that:

- **Decisions** captured from planning: rendering z first; extend the native
  `Glyph`/`Screen` path (not the `Frame`/`DrawableGlyph` stack, which has an
  `Option`-color bleed in `Frame::raw_string`); FOV/portals stay 2D on the top
  surface; `q`/`e` reuse `Screen::rotation`; voxel-set terrain with blocks as
  single-height columns; flat board rendered as a floating slab; static entity
  altitude; `cube_iso` kept as a testbed until the port completes.
- **Phased plan:** data model -> z projection + forward column pass (flat-gated)
  -> materials -> floating-entity altitude -> view rotation -> tooling.
- **Inventory (A–F):** every demo feature with a status (`native`, `pending`,
  `ported`, `testbed-only`, `deferred`) and its game target, so progress is
  visible and the demo can be deleted once no `pending` rows remain.
- **Deletion gate:** remove `cube_iso/` only when nothing is left to port and
  the intentionally-dropped/deferred items are acknowledged.

Rule recorded in the doc: flip a status in the same commit that ports it, with
the usual `CHANGELOG.md` entry.

---

## 2026-09-29 — Bundle the game's fonts locally and verify coverage

### game: collect-fonts.sh bundles the fallback chain; cover mode verifies it

The game reaches into several fallback fonts. New tooling collects exactly the
fonts it needs into a gitignored local dir and checks nothing is missing:

- `floating_square_debug cover --dir DIR <chars…>` loads the fonts under DIR in
  filename order and reports, per glyph, the first font that contains it — or
  `MISSING` (fontdue `has_glyph`). TSV on stdout, summary on stderr. This is the
  missing-render check.
- `scripts/collect-fonts.sh` resolves the ordered fontconfig chain
  (`fc-match -s '<family>'`), truncates it at the last font that renders any
  glyph in the set (default; `--chain=full` copies the whole chain,
  `--chain=used` only the renderers), copies the files into `local-fonts/` as
  `NNN-<family>-<name>` (order-preserving, content-deduped), writes
  `MANIFEST.tsv`, then runs `cover` over the copy and exits non-zero if any
  glyph has no renderer. Extra future symbols via args or `--glyphs-file`;
  `--dry-run`/`--clean`; `--family` default `CaskaydiaMono Nerd Font`.
  `local-fonts/` is gitignored (fonts are copied for local use only).

Verified with a fake `fc-match` shim over real fonts: needed/full/used modes,
missing-glyph detection (exit 1), and dry-run.

Full workspace suite green.

---

## 2026-09-29 — `glyph-fonts.sh` queries fontconfig with the terminal family

### scripts: make glyph-fonts.sh family-aware

The report asked `fc-match ':charset=<U+XXXX>'` *without* the configured
family, so fontconfig answered with its default (sans) and credited DejaVu /
Noto Sans for glyphs the terminal's own font actually renders (e.g. block
elements, braille, ASCII pieces). Now it resolves the primary family once
(`fc-match -f '%{family}' '<family>'`) and queries
`'<family>:charset=<hex>'`, exactly as a fontconfig-based terminal does:
a result whose family differs from the primary is marked `fallback`, and the
summary splits configured-font vs fallback glyphs. New flags: `--family`
(default `CaskaydiaMono Nerd Font`, the author's terminal), `--files` (font
path), `--all` (ranked candidates). The no-`fc-match` path still uses
`floating_square_debug which-font` and marks status unknown.

Verified with a fake `fc-match` shim: ASCII -> primary, `❶` -> DejaVu
(fallback), `◾` -> Noto Color Emoji, and both `--files`/`--all`.

Full workspace suite green.

---

## 2026-09-29 — `scripts/glyph-fonts.sh`: the font behind every game glyph

### game: glyph vocabulary + `scripts/glyph-fonts.sh` font-fallback report

The game draws many glyphs its configured font lacks (e.g. `❶`, `◾`), so the
terminal renders them from a fallback. New tooling shows exactly which font
wins for each:

- `terminal_rendering::renderable_block_glyphs()` — the block-family render
  vocabulary, moved out of the debug tool so it has one source of truth.
- `game::glyph_vocabulary::game_glyph_vocabulary()` (behind `debug-tools`)
  gathers every character the game can draw from the real sources: block
  families, braille, angled blocks, arrows, move markers, chess pieces, widget
  digits, floor arrows, starfield/debug markers. `glyph_vocabulary` is a new
  debug binary that prints it (`U+XXXX <char>`); it also accepts chars /
  `U+XXXX` args.
- `scripts/glyph-fonts.sh` feeds those codepoints to the local font stack
  (`fc-match ':charset=<U+XXXX>'` on Linux; else `floating_square_debug
  which-font`), lists each glyph's font, and summarizes by font.

On this machine it confirms the earlier finding: the block/braille/arrow
vocabulary resolves to DejaVu Sans, and the two-digit enclosed digits resolve
to a color-emoji fallback.

Full workspace suite green: game lib 270 passed / 6 ignored, terminal_rendering
145 passed / 1 ignored, utility 103 passed / 1 ignored.

---

## 2026-09-29 — Keep demo widgets inside the fallback-covered 1–10

### game: keep set_up_test_map widgets within 1-10; note fallback coverage

fontconfig picks DejaVu Sans as the fallback for the negative circled digits,
and DejaVu covers 1-10 (`❶`–`❿`) but not 11-20 (`⓫`–`⓴`) or zero. The
`set_up_test_map` demo placed `Widget(13)` (`⓭`), which would tofu there, so it
now uses `Widget(8)`. `Widget::new`'s comment records the coverage and points
at `floating_square_debug which-font`; the `numbered-boxes` map already uses
1-10 only.

Full workspace suite green.

---

## 2026-09-29 — `which-font` finds the terminal's glyph fallback

### floating-square-debug: add a which-font mode

Scan font directories (`--dir`, else per-OS defaults) and list which installed
fonts contain the requested characters, sorted by coverage; `.ttc` collections
are resolved by trying face indices. Complements `pixels`: when the configured
font lacks a glyph the terminal draws it from a fallback font, and this finds
the candidates (on Linux, `fc-match -s ':charset=<U+XXXX>'` gives the OS's
first pick). Confirmed against the provided Cascadia Code: it carries
`⬤ ● • ·` but not `❶ ①`, which DejaVu Sans does.

Full workspace suite green: game lib 270 passed / 6 ignored, terminal_rendering
145 passed / 1 ignored, utility 103 passed / 1 ignored.

---

## 2026-09-29 — Analytic braille dots get gaps; Cascadia Code coverage noted

### terminal_rendering: braille dots with gaps; record Cascadia Code coverage

The coverage oracle modelled each braille dot as its whole slot, so a full
`⣿` rendered as solid vertical bars in the analytic panes. Dots now use a
smaller vertical extent (half-height 0.1, chosen never to land on the HX×SY
sample lattice), so the analytic view shows the real 2×4 dot grid; the
rects-vs-filled parity test stays green.

A Cascadia Code copy became available, and
`floating_square_debug pixels --font .../CascadiaCode-Regular.ttf` shows it
contains the falling-circle sequence (`⬤ ● • ·`), block elements, and braille,
but **not** the enclosed digits (`①` U+2460, `❶` U+2776, `⓫` U+24EB,
`⓿` U+24FF): those are drawn by the terminal's fallback font, which is how a
geometric codepoint can come back as a color-emoji square. The falling
animation deliberately uses only glyphs Cascadia itself carries.

Full workspace suite green: game lib 270 passed / 6 ignored, terminal_rendering
145 passed / 1 ignored, utility 103 passed / 1 ignored.

---

## 2026-09-29 — Widgets use filled circled digits

### game: render widgets with negative (filled) circled digits

The widgets (pushable numbered boxes) used the positive circled digits
(`①`, `②`, …) — an outline circle with a hole. They now use the Unicode
**negative/filled** circled digits, a solid disc with the numeral knocked out:

- `⓿` zero (U+24FF);
- `❶`–`❿` 1–10 (U+2776–277F, Dingbat set);
- `⓫`–`⓴` 11–20 (U+24EB–24F4, Enclosed Alphanumerics set).

All are `Emoji_Presentation=No` (checked against Unicode 16.0 `emoji-data.txt`
and pinned by a test), so they stay monochrome text. Note the 11–20 set is not
in every font (DejaVuSans lacks it; the `numbered-boxes` map only uses 1–10);
verify a font's coverage with
`floating_square_debug pixels --font <path> U+24EB`.

Full workspace suite green: game lib 270 passed / 6 ignored, terminal_rendering
145 passed / 1 ignored, utility 103 passed / 1 ignored.

---

## 2026-09-29 — Filled-circle falls, and in-repo glyph-pixel debugging

### game: fall as filled circles into the void; add glyph-pixel debugging tools

The first cut of the falling-box animation used braille and was tuned blind:
the coverage oracle panics on braille and geometric shapes, and
`floating-square-debug` could not draw either, so there was no way to see the
actual glyphs. Fixed by changing the animation and building the missing
debugging ability.

**Animation.** `FallingBoxAnimation` now steps through the text-presentation
filled circles `⬤ ● • ·` (U+2B24/25CF/2022/00B7) over 600 ms, fading to black,
and plays at the **off-board square** the box would have landed on — so the box
behind it slides into the edge square with no overlap, and it reads as dropping
into the void. The glyph sequence and color are pinned by tests.

**Debug ability (in-repo):**
- `terminal_rendering::emoji_presentation` embeds the Unicode 16.0
  `Emoji_Presentation` ranges with `char_is_emoji_presentation`. This is the
  bug class that made `◾` (U+25FE) render as a gray emoji square; the falling
  glyphs are now asserted text-presentation.
- The coverage oracle (`glyph_filled`/`glyph_rects`) now models braille, so the
  `glyphs` table and panes no longer panic on the line-drawing vocabulary.
- `floating-square-debug pixels [--font P] [--size N] <chars…>` rasterizes
  arbitrary characters from a real font file (new `fontdue` dependency, dev
  tool only) and prints the true pixel shape beside the analytic view, with an
  `Emoji_Presentation` warning. This is how glyphs the oracle does not model
  are inspected, using the caller's own font.

Full workspace suite green: game lib 269 passed / 6 ignored, terminal_rendering
145 passed / 1 ignored, utility 103 passed / 1 ignored.

---

## 2026-09-29 — `numbered-boxes` map, capped push chains, boxes fall off the edge

### game: add the numbered-boxes map with pushable boxes that fall off the edge

New map `--map numbered-boxes` (also `maps/numbered-boxes.sh`): a fixed 20×20
board with the player at `(10,10)` and ten pushable numbered widgets scattered
around — including a vertical triple that is exactly the longest chain the
player can shove. `do_everything` clamps the terminal to ≥40×20 for it, like
`racetrack`.

The widget push machinery (`try_push_grid_entity`) gained two rules:

- **Max three in a row.** The recursive push now carries a budget
  (`MAX_GRID_ENTITIES_IN_A_PUSH_CHAIN = 3`); an occupied square with no budget
  left refuses the push, so a fourth box in the row blocks the step. Because
  moves happen on the unwind, a refused push leaves the board untouched.
- **Falling off the edge.** When the destination is off-board and the pushee
  is a widget, it is removed and a `FallingBoxAnimation` starts on the edge
  square it left; the rest of the chain then shifts forward and the player
  advances. (Previously the widget was silently deleted with no animation.)
  The animation draws a solid braille square shrinking toward a dot over
  700 ms, so it reads as dropping away into the distance.

Widgets keep their existing enclosed-digit rendering; no new entity type or
snapshot fields were needed.

Full workspace suite green: game lib 266 passed / 6 ignored, terminal_rendering
142 passed / 1 ignored, utility 103 passed / 1 ignored.

---

## 2026-09-28 — Static dodgerblue FOV border

### game: draw a decorative border one square outside the player FOV

The FOV edge to the rest of the screen is now framed by a static border
(`crates/game/src/graphics/fov_border.rs`), drawn in the black ring at Chebyshev
distance `player_sight_radius + 1` so it never covers board contents:

- full-block capitals/bases at the four corners (top and bottom of the side
  pillars);
- inner-half shafts down the left/right sides (`▐` / `▌`);
- inner-half top/bottom beams (`▄` / `▀`), with a centred outer-half run on the
  top edge of length `2·(radius/3)+1` (≈ 1/3 of the FOV diameter, odd so it is
  symmetric);
- dodgerblue (`named_colors::DODGER_BLUE = (30,144,255)`) on black only.

Pure function of `(player square, radius, camera)` → no animation/scrolling, and
`test_headless_frames_are_byte_identical` stays green. Drawn after the FOV
composite and starfield, only while the player is alive. Tests cover the ring
geometry/colors, the 1/3 centre run, and axis symmetry. Verified on the live
`snapshot/`: corners `█`, top centre `▀` / flanks `▄`, bottom `▀`, sides
`▐`/`▌`, all fg(30,144,255) bg(0,0,0).

Full suite green (556 passed / 9 skipped; 296 with debug-tools).

---

## 2026-09-28 — Minimizer writes a final screen.txt for visual review

### snapshot_tool: emit `game_state.json` + `screen.txt` beside the minimized JSON

`minimize <dir> X Y <out.json>` now also writes a loadable snapshot directory at
`<out-dir>` (the out path with `.json` stripped): `game_state.json` and
`screen.txt`, the final render at the captured world time. So the result can be
eyeballed (`cat`/open `screen.txt`) or re-loaded with `render`/`explain`/`diff`
without any manual copying. For the touching-death-square issue this regenerates
`issues/touching-floating-square-background/minimized/` (`game_state.json` +
`screen.txt`; `snapshot_tool diff` confirms they match). New test
`minimize_writes_screen_txt_next_to_the_json`.

Full suite green (553 passed / 9 skipped; 293 with debug-tools).

---

## 2026-09-28 — Generalize the snapshot minimizer; minimize the touching-death-square bug

### snapshot_tool: anchor the minimizer on a rendered cell, not only FOV partials

The minimizer (`snapshot_tool minimize`) could only anchor on an
`OUT_OF_SIGHT`-partial FOV square, so it refused the touching-death-square
artifact (a fully-visible player cell made wrong by a remapped floating square).
`ArtifactAnchor` now records the captured rendered cell — both halves'
characters and colors — plus the FOV visibility identity when one exists;
`artifact_present_at` requires the rendered cell to match exactly (and the FOV
identity to survive for partial anchors). `derive_artifact_anchor` no longer
requires a partial; it errors only for out-of-FOV/out-of-bounds cells, and
`anchor_for_relative_square` captures from the current render.

Result: the touching issue reproduces and minimizes to **one death cube + one
portal**. `issues/touching-floating-square-background/minimized/` (+
`minimized.json`): player (47,29), zero-velocity cube (53.2727,17), portal
`(53,17) E → (47,29) N`. The portal remaps the cube's drawable onto the player's
square; the arrow `drawn_over` then fills the left half's transparent bg with
the below drawable's solid color (left char bg = death color, right char a
death lower-block). Root cause recorded in the issue: `TextDrawable` over
`PartialVisibilityDrawable` compositing. Tests updated for the new anchor
(`derive_anchor_captures_fully_visible_cell`,
`minimize_errors_on_out_of_bounds_cell`).

Also corrected the issue's reproduction note: the earlier "not reproducible"
claim was wrong — the death cube is adjacent *through portal geometry*, not in
absolute space.

Full suite green (553 passed / 9 skipped; 292 with debug-tools).

---

## 2026-09-28 — Starfield inside the FOV; doc hygiene; repro attempts

### game: starfield draws only FOV-visible off-board void; doc cleanup

**Starfield mask was inverted.** `Starfield::draw` skipped off-board cells whose
relative square was in the FOV, so stars appeared *outside* the sight radius and
the void *inside* the FOV stayed black — the opposite of
`issues/starfield-visibility.md`. The discriminator is not visibility but
whether the FOV composite *drew* the cell. Now
`Graphics::load_screen_buffer_from_fov` returns the set of screen squares it
filled, `draw_starfield` takes that set plus the FOV, and a star is drawn only
for an off-board cell that is FOV-visible and undrawn. Portal-view floor (drawn
into off-board screen cells) is still left alone; `fov == None` (dead player)
decorates the whole void. Verified on `leftover-portal-.../snapshot`: all stars
are off-board-in-FOV, none outside. Tests replaced
(`stars_only_inside_the_fov_over_undrawn_void`,
`stars_are_not_drawn_where_the_fov_composite_drew`).

**Doc hygiene.** Deleted the resolved issue records per `issues/README.md`:
`starfield-visibility.md`, `starfield-on-top-of-portal-view/`,
`fov-budget-optimization-cant-see-through-portal/` (budget removed).
`black-*` kept as design records (referenced by ROADMAP). Fixed stale anchors in
`docs/PERFORMANCE.md`/`docs/ROADMAP.md` and the outdated "no depth cap" note.
Softened the depth bound to "≤ radius + 1 in every configuration tested" and
added a randomized check over 100 portal layouts at radius 2..6
(`test_portal_recursion_depth_is_bounded_by_sight_radius`).

**Reproduction attempts** recorded in the two remaining bug issues:
`touching-floating-square-background` is absent from its snapshot (nearest death
cube 6.03 away); a synthetic overlapping-cube repro shows the whole left
character background taking the death color. `leftover-portal-...` is absent
from its snapshot (0 red-tinted cells up-right), matching the issue's own note.

Full suite green (553 passed / 9 skipped).

---

## 2026-09-28 — Remove the cumulative budget; the sight radius is the one dial

### fov: delete FovOptions/cumulative_radius_budget; add a sight-radius override

The budget was meant to be the radius, and after the depth-bound and artifact
corrections it had become a second radius plus a per-hop crossing gate. Its
default (`PLAYER_SIGHT_RADIUS`) was a no-op, and its only remaining effect was to
shrink the view while running an artifact-prone gate. Removed:

- `FovOptions`, `cumulative_radius_budget`, the `remaining_radius`/`spent`
  accumulator and the gate; `max_extent` is now always `radius`.
- The `_with_options` FOV entry points and `Game::set_fov_cumulative_radius`.
- `--fov-budget` / `--no-fov-budget` (`FovToggles::cumulative_radius_budget`).

Replaced by a real radius override: `Game::set_player_sight_radius` /
`player_sight_radius` (clears the FOV cache), `--fov-radius <n>` on the binary,
and `--radius=<n>` on the profiling harness. Default behavior is unchanged,
because the old default was already the no-op case. Racetrack FOV with the dial:
0.60 / 2.3 / 8.6 ms at R=4 / 8 / 16.

Tests: dropped the budget-specific regressions
(`test_cumulative_budget_keeps_a_distant_portal_window_open`,
`test_budget_does_not_black_out_the_outer_fov_through_a_portal`,
`test_budget_at_sight_radius_does_not_black_out_a_portal_hallway`,
`test_fov_cumulative_budget_is_a_fidelity_dial`), kept
`test_portal_recursion_depth_stays_near_the_sight_radius` on the plain API, and
added `test_fov_sight_radius_override_changes_the_view_and_clears_the_cache`.
Full suite green (551 passed / 9 skipped; 290 with debug-tools). PERFORMANCE.md
records the removal.

---

## 2026-09-28 — Portal recursion depth is bounded; cost is exponential in the radius

### docs: correct the FOV recursion depth/work framing in PERFORMANCE.md

Measured the recursion depth directly with `FovTrace` and found the earlier
"unbounded / only arc narrowing terminates" framing was wrong. Along a path the
apparent image strictly advances (a deeper frame inherits the parent's
`starting_step_in_fov_sequence` and starts past the near field) and each frame
scans only `radius` squares, so **depth ≤ radius + 1**; a portal on the player's
own square supplies the extra "zeroth" hop. At radius 5: three north portals to
one common exit → depth 1, a portal north → 5, a portal on the player's own
square → 6.

So the frame-time driver is the number of **paths**, not depth. Racetrack FOV
scales ~4.5x per +4 radius (0.42/1.8/8.2 ms at 4/8/16), i.e. exponential in the
radius. Two consequences recorded in `docs/PERFORMANCE.md`:

- Depth caps are not a useful perf lever; path/node count is.
- The relative-radius budget is a *work* cap, and because the gate only fires
  when `budget < radius`, the default `budget == radius` is a no-op. Dominance
  pruning (approach C) is now framed as the preferred cap-free fix: it attacks
  the exponential path count with no fidelity loss.

New regression `test_portal_recursion_depth_stays_near_the_sight_radius` pins
the measured depths. Full suite green (554 passed / 9 skipped).

---

## 2026-09-28 — Budget must not prune inside the sight radius

### fov: only gate portal recursion when the budget is below the sight radius

Follow-up to the two FOV fixes above, from the live `snapshot/` infinite portal
hallway (player (18,20)). The cumulative accumulator is a per-hop *path* cost,
not apparent distance: through the hallway's parallel portals it grows about
twice as fast as the apparent image. Gating crossings at `budget == radius`
therefore exhausted the 16-square budget after ~half the apparent distance —
rel 7..16 rendered black while rel 4..6 and the adjacent column stayed visible.

The gate is now active only when the budget is tighter than the sight radius
(`budget < radius`). At or above the radius the sub-view is legacy, because
`max_extent = min(radius, budget)` already caps apparent reach; the default
budget (16) therefore no longer changes rendering at all. The budget stays a
fidelity/perf dial for smaller values (8 → 1.8 ms/FOV, 4 → 0.42 ms, vs 8.3
unbudgeted on racetrack).

`test_fov_cumulative_budget_is_a_fidelity_dial` updated: budget ≥ radius equals
unbounded; 8 and 4 differ. New regression
`test_budget_at_sight_radius_does_not_black_out_a_portal_hallway` builds the
snapshot's portal window (the live `snapshot/` is gitignored) and asserts the
budgeted view matches legacy at every apparent distance inside the radius; it
fails on the old gate at rel(0,7). Full suite green (553 passed / 9 skipped).

---

## 2026-09-28 — Fix through-portal FOV clipping and clip the starfield to the FOV

### fov: keep the through-portal window open; clip starfield to FOV

Two related "player field of view" defects, both reproducible from the planted
issue snapshots.

**Through-portal sight.** `issues/fov-budget-optimization-cant-see-through-portal/`
(snapshot_2: player at (61,42), portal directly north at (61,50)/(61,51)) showed
a portal you can enter but not see through. The cumulative-radius budget
computed each sub-view's extent as `remaining_radius - spent`, but a portal
sub-view is centered on the viewer's *virtual* image (the transformed player),
so the exit lands at child-relative radius `spent` and child-relative distance
already equals cumulative apparent distance. Subtracting `spent` again clipped
the window to exactly the portal plane; for a portal more than half the sight
radius away nothing remained.

The recursion now gives every level the same apparent extent,
`min(radius, budget)`, while `remaining_radius` only gates further crossings.
Without a budget the extent is the legacy full `radius`. `explain` on
snapshot_2 rel(0,9) now reports the depth-1 image of abs(82,53) instead of a
black cell; regression `test_cumulative_budget_keeps_a_distant_portal_window_open`
fails on the old metric and passes on the new one.

**Debug tooling.** `player_field_of_view_traced` (used by
`fov-trace`/`explain`/`invariants`) ignored the game's `FovOptions`, so it
described the unbudgeted view rather than the rendered one. It now passes
`self.fov_options`; this is what made the clipping visible in `explain`.

**Starfield.** `Starfield::draw` only skipped on-board squares, so stars painted
the off-board void regardless of the FOV and overwrote portal-view floor whose
screen cell maps to an off-board world square
(`starfield-visibility.md`, `starfield-on-top-of-portal-view/`). It now also
skips any cell whose player-relative square is in the FOV (via an `Option<&fov>`,
`None` for the dead player who renders the whole board).

Perf: budget 4/8/16 → 0.43/1.8/7.5 ms/FOV on racetrack, versus 8.1 ms
unbudgeted. The previous 24 ms at budget 16 came from the over-shrinking bug;
the corrected metric keeps a full window through each portal, so the default 16
is a milder ~7% saving. Measurements and narrative updated in
`docs/PERFORMANCE.md`.

Tests: full suite green (552 passed / 9 skipped); added
`test_cumulative_budget_keeps_a_distant_portal_window_open`,
`test_budget_does_not_black_out_the_outer_fov_through_a_portal`, and
`no_stars_inside_the_field_of_view`.

---

## 2026-09-28 — Don't black out the outer FOV through a portal corridor

### fov: sub-view extent is the budget, not the remaining budget

Follow-up to the through-portal fix above. The first version made each
sub-view's apparent extent the *remaining* budget (one hop behind), which
shrinks with depth. In a portal corridor that leaves a black band *inside* the
sight radius: the direct view is blocked by the portal wall and only the
sub-view can fill the outer apparent squares, but the sub-view stops short of
them.

Repro from the live `snapshot/` (player (69,47) facing into a hall of portals,
rotation makes world-east screen-up): rel 13–16 of the sight radius render black
at budget 16, while rel 17 (outside the radius) is correctly black. The
sub-view's extent is now the shared `min(radius, budget)` at every level; the
budget still bounds apparent reach and gates deeper crossings via
`remaining_radius`. `explain` now fills rel 1–16 and goes black at 17.

Regression: `test_budget_does_not_black_out_the_outer_fov_through_a_portal`
fails on the shrinking extent and passes on the constant one.

---

## 2026-09-27 — Stop paying for gprof instrumentation on every test run

### test: make gprof instrumentation opt-in; trim charwise sampling

The test suite was ~30s (the issue reported 40s). Root cause: `.cargo/config.toml`
put `-Zinstrument-mcount` (gprof call-count instrumentation) in `[build].rustflags`,
so every target — all test binaries included — carried an `mcount` call at every
function entry. Measured by varying only `RUSTFLAGS`: full suite **30.1s → 7.3s**.

- `.cargo/config.toml` now keeps only `-Cforce-frame-pointers=yes` by default.
  The mcount flags moved to `scripts/profile.sh`, which sets `RUSTFLAGS`
  (replacing, not appending to, the config flags — so it repeats the frame
  pointer flag) and runs whatever command you give it. `RUSTFLAGS` is also what
  previously made this invisible: `cargo nextest` inherited the instrumentation.
- Remaining hotspot was `terminal_rendering::charwise_rendering
  test_approach_comparison_metrics` (5.8s, 75% of the un-instrumented suite):
  a characterization/print test whose assertions are explicitly loose. Its
  offset grid went 16x16 → 8x8 at odd 1/16 steps (avoiding eighth-block-aligned
  offsets, which zeroed the family-snapped area error). Bounds still pass with
  margin, `displacement_sensitivity` still probes the omitted midpoints.
- Full suite: **30.1s → 5.2s**; no single test over ~2s. nextest's `slow-timeout`
  comment updated to match.

Docs: ARCHITECTURE.md "Profiling" and PERFORMANCE.md now say to run through
`scripts/profile.sh`; resolved and removed `issues/tests-take-too-long.md`.

---

## 2026-09-27 — Default the FOV cache and cumulative-radius budget on

### perf: default FOV cache and cumulative-radius budget on
The two racetrack FOV prototypes (docs/PERFORMANCE.md) are now the default for
`Game` instead of opt-in flags:

- `Game::new` sets `fov_cache_enabled: true` and initializes
  `FovOptions::cumulative_radius_budget` to `Some(PLAYER_SIGHT_RADIUS as f32)`
  (16), so the cumulative relative-radius budget equals the player's view
  radius. `PLAYER_SIGHT_RADIUS` is now `pub`.
- Cache invalidation: the cache key is only the player square, so `place_block`
  and the four public portal-placement methods now clear `fov_cache`. Previously
  they were assumed immutable after setup; defaulting the cache on makes a
  mid-game map mutation able to serve a stale view, which the new
  `test_fov_cache_invalidates_when_map_changes` guards.
- CLI: `FovToggles::default()` is now on, and the game binary gains
  `--no-fov-cache` / `--no-fov-budget` opt-outs (`--fov-cache` /
  `--fov-budget <n>` still override). The profiling harness keeps its own
  default-off toggles, so baseline profiling is unchanged.
- Tests: added `test_fov_performance_defaults_are_on` and the invalidation test;
  reworked `test_fov_cumulative_budget_is_a_fidelity_dial` to take an explicit
  `None` unbounded reference. Full suite green (254 lib tests + 26 playground).

Docs: updated the PERFORMANCE.md prototype section and map-mutation note.

---

## 2026-09-27 — Off-board starfield with parallax and drift

### game: procedural starfield for the off-board void
The area beyond the board edge was flat black. It now renders a sparse
starfield (`crates/game/src/graphics/starfield.rs`).

- Three depth layers, each anchored at a fraction of the camera's motion, so
  near layers slide further than far ones as the player moves (parallax). Each
  layer also drifts slowly in a fixed direction so the field stays alive while
  the player stands still.
- Stars live on an infinite hashed lattice: moving/drifting reveals new cells,
  so nothing is generated or recycled and there is no bounding region to
  maintain.
- `Starfield::draw` is a pure function of `(screen, board_size, time)` with no
  per-frame state, preserving `test_headless_frames_are_byte_identical` and the
  screen-stability tests.
- Drawn after the FOV composite and only into off-board cells, so the board and
  visible contents are never touched. Wired via `Graphics::draw_starfield` from
  `Game::update_screen_from_draw_buffer`.
- Tests cover: off-board-only, determinism, time drift, rotation round-trip,
  and parallax ordering (near shifts more than far).

---

## 2026-09 — Deterministic portal ordering, relative-radius cap, test timeout

### game: deterministic portal order; relative-radius FOV cap; test timeout
Three changes:

1. **Deterministic portal ordering.** Portal iteration fed FOV sub-view order
   from hash maps. `portals_entering_from_square` and `iter_portals` now sort by
   `SquareWithOrthogonalDir::sort_key()` (square, then step), the FOV keeps
   portal arcs in an order-preserving `Vec` instead of a `HashMap`, and
   `combined_sub_fovs` groups by root through a `BTreeMap`. Sorting is the cheap
   fix because at most two portals are visible per square. New test
   `test_portal_iteration_is_deterministic`.
2. **B is now a cumulative *relative-radius* cap.** The budget charged each
   portal crossing its absolute entrance↔exit jump, so an adjacent portal that
   led far away was hidden (issue
   `issues/fov-budget-optimization-cant-see-through-portal/`). It now spends how
   far sight travelled in the current frame to reach the portal, and the frame
   extent is `min(radius, remaining)`. Renamed
   `FovOptions::cumulative_radius_budget` / `set_fov_cumulative_radius`. Also
   clamped the top-level extent to `radius`: a large budget had made the octant
   iterator sweep thousands of squares (the test-suite hang). Verified against
   the issue snapshot that the adjacent-portal view is preserved at budget 16.
3. **Default test timeout.** Added `.config/nextest.toml` with
   `slow-timeout = { period = "60s", terminate-after = 2 }`, so a runaway test
   fails after 120s instead of hanging the suite. This would have caught change
   2's hang.

Full suite green (541 tests).

---

## 2026-09 — Byte-identical frames: make the FOV draw order total

### game: make FOV draw order total so frames are deterministic
Frames were not byte-identical. The FOV's sub-view order is non-deterministic
(visible portals are iterated from a `HashMap` to build `transformed_sub_fovs`),
and `FieldOfViewResult::sorted_by_draw_order` only broke ties by portal depth
(a stable sort). Equal-depth visibilities therefore fell back to HashMap order,
and `drawable_at_relative_square` picked different glyphs frame-to-frame — the
"ambiguity (and thus flashing)" its old TODO warned about, defeating roadmap
W.A's byte-identical-frame promise.

`sorted_by_draw_order` is now a total order on `(portal_depth,
absolute_square.x, absolute_square.y, rotation)`. New regression test
`test_headless_frames_are_byte_identical` renders each tick twice on racetrack
and hallways and asserts identical screen buffers; all goldens still pass. FOV
*construction* order remains HashMap-dependent (affects debug FOV trace/JSON
ordering, not frames).

Also recorded the map-mutation cache-invalidation TODO on the FOV cache fields
(`game/mod.rs`) and in `docs/PERFORMANCE.md`, left for later by request.

Full suite green (540 tests).

---

## 2026-09 — Test the FOV prototypes and fix the budget's initialization

### perf: test FOV cache/budget, fix budget initialization
Added three tests that enable the prototypes directly (they were default-off and
untested): `test_fov_cache_matches_fresh_computation`,
`test_fov_cache_invalidates_when_player_moves`, and
`test_fov_cumulative_budget_is_a_fidelity_dial`.

Fixed a bug in prototype B: `remaining_distance` was initialized to `radius`,
so the budget's magnitude was ignored — every budget ≥ radius behaved
identically (that is why budgets 8–48 had looked flat). It now initializes from
the budget, and the per-hop extent stays `radius` (gate-only, never shrinking a
hop's own view). B is now a real speed/fidelity dial: budget 4/8/16/32/1000 →
13.7/16.1/32.1/43.0/46.6 ms/FOV on racetrack; A + B(8) = 7.7 ms/frame.

The tests compare an order-insensitive FOV signature rather than screen
buffers, because of a side finding: the FOV's sub-view order is
non-deterministic (visible portals are collected into a `HashMap` and iterated
to build `transformed_sub_fovs`), and the renderer picks glyphs first-match, so
frames are not byte-identical. This is unrelated to the cache but defeats
roadmap W.A's "byte-identical frame" promise; recorded in
`docs/PERFORMANCE.md`.

Full suite green (539 tests).

---

## 2026-09 — Expose the FOV prototypes on the game binary

### game: add --fov-cache / --fov-budget CLI flags
The FOV prototypes were only reachable from the profiling example. Added
`FovToggles` and a third `do_everything` argument, applied to the game after
map setup or snapshot load, plus `--fov-cache` and `--fov-budget <n>` (or
`--fov-budget=<n>`) in `main.rs`'s arg parser and usage text.

Both default off, so normal play is unchanged. The `maps/*.sh` launchers now
forward extra args, so `./maps/racetrack.sh --fov-cache --fov-budget 16` works;
`./play-game --map racetrack --fov-cache` works directly. Verified the game
starts and renders under a pty with both flags set.

---

## 2026-09 — Prototype FOV fixes for the racetrack frame time

### perf: prototype FOV cache and cumulative distance budget behind flags
Implemented prototypes A and B from `docs/PERFORMANCE.md` behind default-off
runtime toggles on `Game`:

- **A** (`set_fov_cache_enabled`): cache the player FOV per player square. The
  FOV is render-only and a pure function of the player square plus the static
  map (blocks/portals never change at runtime, facing is irrelevant), so it
  only recomputes when the player moves. Served at the draw seam by
  `player_fov_for_draw`.
- **B** (`set_fov_cumulative_distance_budget`): thread `FovOptions` +
  `remaining_distance` through the FOV recursion
  (`field_of_view_within_arc_in_single_octant_impl`). Each portal crossing
  spends the straight-line separation between sub-view centers, and a crossing
  that overruns the budget is skipped instead of recursed. This is a distance
  budget, not a depth cap. Public wrappers keep legacy per-hop-only behavior
  via `FovOptions::default()`.

The harness gains `--cache`, `--budget=<n>`, and a coarse `vis` mode.
Racetrack, release: 71.2 → 23.6 ms/frame (A), 28.0 (B at budget 16), 10.8
(A+B). B's curve is flat for budgets 8–48. Caveat: B changes visibility (a
coarse 11x7 probe goes from 14 to 35 invisible squares), so it must be tuned
and validated against goldens before enabling. Default behavior is unchanged;
all 267 game tests pass.

Recorded the results, the float-key memoization caveat, and the staged plan in
`docs/PERFORMANCE.md`.

---

## 2026-09 — Profile the racetrack map; FOV recursion is the hotspot

### perf: add racetrack profiling harness; FOV recursion dominates
Added `crates/game/examples/profile_racetrack.rs`, a headless harness that
replays the real loop (`tick_realtime_effects` + a headless draw per frame) and
can time the player FOV alone. Used it with `gprof` to rank hotspots.

Racetrack renders at 71 ms/frame (release) against the 21 ms budget — ~2.2x
slower than demo/hallways (29/33 ms). All time is in `Game::draw`. The driver
is the portal-recursive FOV (`field_of_view_within_arc_in_single_octant`):
~545k recursive calls/frame vs 72–135k on other maps, and a single player-FOV
computation costs 47 ms. The FOV is recomputed from scratch every draw
(`game/mod.rs:601`) with no memoization and no recursion depth cap or
visited-portal set; the racetrack's 19-face L portal and 3-wide two-way corners
give it many branching crossings. Secondary costs are the angle-interval/trig
math feeding the recursion, angled-block glyph mapping over point tuples, and
general `WorldSquare` SipHash.

Wrote `docs/PERFORMANCE.md` with the numbers and method, and linked it from the
ARCHITECTURE profiling section. Root cause overlaps roadmap W.C ("no depth
cap"); this quantifies its frame-time impact.

---

## 2026-09 — Document which profilers actually run in the sandbox

### docs: note available profiling tooling in ARCHITECTURE
The `flake.nix` dev shell lists `cargo-flamegraph`, `cargo-profiler` and
`uftrace`, but in this sandbox only some resolve. Replaced the stale
`flamegraph.svg` mention in "Testing & Tooling" with a table of what actually
runs: `uftrace` (verified via dynamic tracing of `map_diagram racetrack`),
`gprof` (sampling from `gmon.out`), and `cargo-nextest` work; `cargo-flamegraph`
and `cargo-profiler` do not, because `perf` and `valgrind` are absent (as are
gdb/lldb/rr). Noted that `.cargo/config.toml` already builds with
`-Zinstrument-mcount`, and that the TTY game loop must be profiled through a
headless harness (`map_diagram`, `draw_headless_now`, or a frame-loop test).

---

## 2026-09 — Build wide racetrack portal bands from one transform

### game: derive every wide-band strip from one portal transform
Replaced the map's hand-laid multi-square portals with a single helper,
`place_wide_portal_from_transform(entrance, exit, width)`: caller gives the
center strip and the helper derives every other strip's exit from that
portal's rigid transform. The corners (width 3), both shuttle flips (width
3) and the 19-wide L portal now all use it, so a band is one coherent window
by construction — including turns (180° flips, 90° corners) that reverse the
wall's lateral direction. This supersedes the manual `-d` mirror and the
manually reversed L exit ordering.

Removed `test_every_map_has_coherent_portal_bands` and the L-specific
coherence test: asserting coherent bands as an invariant is too strong, since
unorthodox portal arrangements may be intentional. Coherence now comes from
the construction helper; behavior stays covered by the lap, shuttle and
L-render tests.

`place_wide_corner_portal` is gone (folded into the new helper).

---

## 2026-09 — Racetrack shuttle band sheared; audit all map portal bands

### game: mirror the racetrack shuttle's 3-wide exit band
The same shear as the L portal, one square over: a 180° flip reverses a
wall's lateral direction, so a 3-wide entrance band's exit band must be its
mirror. The shuttle offset both ends of each flip by `+d`, so the three
strips each got a different transform and the window twisted. Exit offsets
now use `-d` for both shuttle walls.

### game: assert every map's portal bands share one transform
Added `test_every_map_has_coherent_portal_bands`, which walks demo,
racetrack, hallways and test_map and asserts that entrance strips laid side
by side (adjacent along the axis perpendicular to their shared entrance
direction) are `is_coherent_with` each other. It fails on the shuttle before
the mirror fix (`racetrack: portal band shears: (19,10) -> (19,14) vs
(20,10) -> (20,14)`) and passes after. Adjacency along the travel axis is
deliberately excluded — those are separate windows, not one band. Demo,
hallways and test_map were already coherent.

---

## 2026-09 — Racetrack L portal sheared across its strips

### game: make the racetrack's big L portal one coherent window
The "L" exhibit places a 19-face vertical entrance wall (x = base.x+22,
facing right) turning 90° onto a 19-face horizontal exit wall at
y = base.y+7 (facing up). The turn is a +90° (anticlockwise) rotation, so
the entrance wall's "up" tangent maps to "left": the exit wall must run
right-to-left as the entrance runs bottom-to-top. The code walked the exit
wall left-to-right (`base.x + 2 + i`), so each of the 19 strips got its own
rigid transform (rotation centres stepping along a diagonal) instead of one
shared transform. The wall was therefore a glide-reflection, not a portal —
it rendered with the "weird turns and breaks" of a sheared surface, and the
per-strip exits were mirrored. Fixed to `base.x + 20 - i` (exit x = 44 down
to 26 as the entrance climbs y = 4..22).

New regression test `test_racetrack_l_portal_strips_are_one_coherent_portal`
asserts every strip `is_coherent_with` the first (it failed before the fix).
Updated `test_racetrack_stationary_cubes_render_at_l_portal_wall`: the
bottom entrance's sliver now emerges at the right end of the exit wall
`(44,20)` instead of the mirrored `(26,20)`.

---

## 2026-09 — Racetrack bottom-left corner portal placement

### game: fix bottom-left racetrack corner exit off-by-one
The racetrack's four 3-wide turning portals must each exit two squares past
their corner square: at one square the 3-wide exit band overlaps the 3-wide
entrance band (sharing a corner square), and the resulting lap is asymmetric.
The bottom-left corner alone used `bottom_row_y + 1`, so its exit row (y=12)
met its entrance column (x=28) at `(28,12)`, and the left straightaway ran a
square longer than the right. Corrected to `+ 2`, giving a symmetric 9x5 path
with the exit bands clear of the entrance bands.

The unintuitive placement was masked by tests that had been written to the
asymmetric lap: it also made the lap length 29 rather than the true 28. Updated
`test_racetrack_map_cubes_loop_back_after_one_lap` (7.25s -> 7.0s),
`test_racetrack_map_cubes_survive_frame_rate_ticks` (345 -> 333 frames), the
`test_portal_aware_move_racetrack_lap_closes` geometry (bottom-left exit
`(10,6)` -> `(10,7)`, move 29 -> 28), and the map's doc comment (lap 29 -> 28,
plus the lap direction: up-right-down-left is clockwise, not counter-).

---

## 2026-09 — Black diagonal portal-seam artifact

### fov: merge adjacent portal-slice view cones as a set (fixes black diagonal seam)
Root cause of `issues/black-diagonal-portal-seam/`: standing on one of a
stacked pair of east-facing portals, a square on the 45-degree seam between
their openings had only one partial visibility and rendered as an
`OUT_OF_SIGHT` black diagonal. The item-12 merge (`combined_with_unioning_arcs`)
carried a single `view_arc`; when two same-root slices did not touch it kept one
arc but the other's squares, so a later touching slice unioned only with the
retained arc and the dropped slice's squares were recomputed under an arc that
no longer covered them (order-dependent, since `combined_sub_fovs` reduces a
`HashMap` group). `FieldOfViewResult` now carries `view_arcs:
Vec<AngleInterval>`; `combined_main_view_only` merges touching/overlapping
fragments (`merge_contiguous_arc_intervals`) and recomputes squares under the
merged set (`visibility_of_square_under_arc_intervals`), while
blocker-separated arcs stay distinct. All seam cells `rel(k,-k)` are now fully
visible. Regression tests `test_stacked_portal_slices_union_to_full_visibility`
and `stacked_portal_seam_has_no_out_of_sight_partial`; full suite green
(536 passed / 9 skipped).

### game: issue capture + minimized repro for the black diagonal seam
New `issues/black-diagonal-portal-seam/` with the snapshot, a write-up
(symptom, observed cells, minimal repro, mechanism), and `debug/` outputs from
`snapshot_tool` (render/diff/cells/fov-trace/explain/invariants). The minimizer
reduced the capture to **player + 2 portals**. The repro was first written with
`--no-screen-crop` because the crop candidate crashed on an odd-width bug (see
next entry); that workaround and its false diagnosis ("halls of mirrors") were
later corrected.

### game: fix odd-width screen crop + clip off-screen draws
The virtual-screen crop panicked (`Tried to draw character off screen`) because
`screen_crop_candidate` returned `2 * (dx + margin) + 1` — always an odd width.
One world square is two char columns addressed by even left columns, so an odd
width's last column is a half-visible square whose right half is off-screen;
`Screen::all_screen_squares` yielded it and `draw_glyphs_straight_to_screen_square`
panicked. Large crops only appeared to work because their last column fell
outside the sight radius. Fix: `screen_crop_candidate` now returns an even
width, and off-screen character columns are clipped instead of panicking
(`Screen::draw_glyph_straight_to_screen_buffer`, `draw_string_to_screen`;
roadmap item 5). The minimized state crops to 22x13. Tests:
`drawing_an_odd_width_edge_square_clips_instead_of_panicking` and the
even-width assertion in `screen_crop_candidate_bounds_artifact_with_margin`.
Superseded/reverted the earlier false fix (portal-square bounding,
`catch_unwind`, late crop).

---

## 2026-09 — Debug tooling wishlist (W.A–W.G)

Implemented the debug-tooling wishlist from the front of
[ROADMAP.md](ROADMAP.md), motivated by the portal-depth partial-visibility
artifact in `issues/black-block-deep-in-portal/`. All workspace tests green.

### fov: union portal-slice view cones before merging (fixes black-block artifact)
Root cause: sub-FOVs reaching the same transformed root through adjacent
portal-face slices were merged by combining their per-square half-planes
(`combined_increasing_visibility`), which cannot represent the union of two
non-complementary partials; the uncovered part rendered as an `OUT_OF_SIGHT`
(black) partial. Carry the view cone on `FieldOfViewResult::view_arc` and, when
`combined_sub_fovs` merges same-root results, union the arcs and recompute
affected squares under the unioned cone (`combined_with_unioning_arcs`); the
top-level octant fold and blocker splits keep their arcs. The artifact cell
`(49,37)` now renders as a fully-visible tinted floor. Added regression test
`test_portal_slice_arcs_union_to_full_visibility`; full workspace suite green
(238 game lib). Regenerated the issue debug outputs (analysis post-fix;
minimizer outputs moved to `debug/pre-fix/`).

### game: minimize portals too, with --keep-portals opt-out (W.E)
The minimizer previously protected all portals, leaving 21 in the black-block
repro. It now removes portal entries by default; the strict anchor predicate
(same relative square + depth + absolute square) keeps only the artifact's own
chain, so the repro drops to **2 portals** (`[41,44]`/`[41,45]`, both
`dir[1,0] -> [37,44/45]`). `MinimizeOptions.keep_portals` /
`snapshot_tool minimize --keep-portals` restores the old behavior (21 portals).
Regenerated the issue debug outputs, added a `--keep-portals` variant, and
updated `debug/README.md`. Tests green (238 lib).

### docs: add post-minimization renders to the black-block debug outputs
The first pass rendered only the original snapshot; the minimized state existed
only embedded in the review transcripts. Materialized `debug/minimized/` and
`debug/minimized-nocrop/` as full snapshot dirs (`game_state.json` +
`screen.txt`) and added `render-minimized.txt`, `render-minimized-nocrop.txt`,
`cells-minimized.txt`, `explain-minimized.txt` (artifact moves to buffer
`(32,4)` in the 73×11 frame), and `diff-minimized.txt`. Verified the review
transcript's final frame equals the standalone minimized render.

### docs: capture all snapshot_tool debug outputs for the black-block issue
Generated `issues/black-block-deep-in-portal/debug/` from the issue snapshot:
`render`, `diff`, `cells`, `fov-trace` (+JSON), `explain 49 37`, `invariants`,
and the minimizer with each review variant (`--review`, `--review-plain`,
`--review-explain`, `--no-screen-crop`) plus the minimized JSONs. Added a
`debug/README.md` index with the regeneration commands.

### game: virtual-screen crop + review transcript for the minimizer (W.E)
The minimizer now anchors the artifact to the player (relative square + portal
depth + absolute square) instead of a fixed buffer cell, so it survives a
resize. New early step crops the virtual screen to the `{player, artifact}`
bounding box plus a margin (default 4 squares), changing only
`screen.terminal_width/height` (board and world coordinates untouched; rejected
if the artifact would leave the buffer). Entity removal then runs as before.
`snapshot_tool minimize` gains `--review[=<path>]` (screen-by-screen transcript:
frame, then a one-line change summary, then the next frame), `--review-explain`
(embed the full explain block per step), `--review-plain` (strip ANSI), and
`--crop-margin`/`--no-screen-crop`. On the black-block snapshot: 141×78 → 73×11,
then the three death cubes removed, artifact intact throughout. Tests:
crop geometry, anchor derivation error, review-step formatting; full game suite
green (238 lib tests).

### docs: drop export guidance from AGENTS.md
Removed the extra review-pause export note from `AGENTS.md` and the matching
changelog reference.

### docs: changelog/AGENTS cleanup
Final wording pass so the changelog no longer quotes the removed guidance.

### docs: add repo AGENTS.md changelog policy
Added `AGENTS.md` noting that sandbox commits are ephemeral and that every
commit's subject/body must be mirrored into `docs/CHANGELOG.md`.

### game: add minimal live debug overlay (W.G)
`Graphics.debug_overlay` (`screen_center`, `screen_origin`) draws magenta
markers after the FOV composite; `Game::set_debug_overlay` toggles it.
Test covers the center marker.
_Deferred:_ keybindings, depth heatmap, draw-order readout, mouse-hover →
`explain_screen_cell`.

### game: add snapshot minimizer (W.E)
`debug::minimize_snapshot` greedily removes entity entries while the
`OUT_OF_SIGHT`-tinted partial persists at a target cell, preserving portals
and player. `snapshot_tool minimize <dir> X Y [out]`. Reduces the black-block
snapshot to player + 21 portals with the artifact intact.
_Deferred:_ coordinate/board shrink and chunked delta-debugging.

### game: add FOV visibility-consistency invariant (W.D)
`fov_visibility_consistency_violations` groups visibilities by absolute square
across relative squares and flags a square that is fully visible via one portal
depth but partially visible via another — the black-block signature. Exposed via
`Game::fov_invariant_violations` and `snapshot_tool invariants`. Detects
`rel(15,1) abs(37,45)` on the issue snapshot.
_Deferred:_ the ray-cast differential reference oracle.

### game: add explain_screen_cell provenance query (W.B)
`Game::explain_screen_cell` reports the final glyph, the world/FOV-relative
square, and every FOV visibility (absolute square, portal depth, rotation,
absolute/relative mask, draw-buffer drawable) that could contribute to a screen
cell. `snapshot_tool` gains an `explain <dir> X Y` mode. On the black-block
snapshot it reproduces the issue's observed cell exactly (depth 3, abs(37,45),
final fg(165,89,89) bg(77,0,0)). Test added.

### game: add portal-recursion FOV trace (W.C)
Thread `depth` + an optional `FovTrace` through
`field_of_view_within_arc_in_single_octant` and expose traced entry points
(`single_octant_field_of_view_traced`,
`portal_aware_field_of_view_from_square_traced`,
`Game::player_field_of_view_traced`). Each node records the crossed portal
square, child incoming arc, parent-frame arc, transformed center, and rotation.
`snapshot_tool` gains `fov-trace` and `fov-trace-json` modes. 236 lib tests
green.

### game: inject LogicalTime + seeded serialized RNG (W.A)
Replace `std::time::Instant` with a serializable `LogicalTime(Duration)`
throughout `Game`/`Graphics`/animations, stamp spawned animations from the
frame's logical time, and confine wall-clock reads to the driver seam.
`Game::new` seeds the world clock from its `start_time`; snapshot load no longer
rebases to `Instant::now()`.

Gameplay randomness (turret fire, shotgun spread, random subordinate spawns)
now draws from a `ChaCha8Rng` on `Game` whose state is serialized in
`game_state.json` (`rng_state`), so loading resumes the exact stream.

Adds a test asserting no `Instant::now()` survives outside the driver seam.
game/utility/terminal_rendering suites green (235 lib tests).

### docs: record W.F landed and W.A partial in roadmap wishlist
Updated the roadmap wishlist statuses.

### game: seed world clock from start_time; stabilize StaticBoard time (W.A)
`Game::new` set `world_start_time`/`world_time` to independent `Instant::now()`
reads instead of the caller-provided anchor. `StaticBoard::start_time` returned
a fresh `Instant::now()` per call. Also drops the dead
`Graphics::time_since_start` and adds a headless-render determinism test.

### game: thread frame time into death-cube draw (W.A leak fix)
`draw_death_cube` used `Instant::now()` for technicolor, ignoring the frame time
threaded through `draw`/`populate_draw_buffer`. Headless `snapshot_tool` now
renders at the captured `world_time_seconds`; the only remaining diff against
the pre-fix checked-in snapshot is the ~2 ms wall-clock skew the old code baked
into the capture.

### game: add feature-gated headless snapshot_tool (W.F)
Added a default-off `debug-tools` feature, `pub mod game::snapshot::debug`
(descendant module so it reaches the private DTO/loader without widening the
crate API), and `crates/game/src/bin/snapshot_tool.rs` with
`render`/`diff`/`bless`/`cells` subcommands, an ANSI `screen_text` parser, and a
per-cell diff report. Manifest switched to explicit `[[bin]]` entries +
`autobins = false`; `snapshot_tool` has `required-features = ["debug-tools"]`.
Tests: parser round-trip against the screen buffer; cross-load render
determinism guard.

### docs: add debug tooling wishlist to roadmap front
Inserted the W.A–W.G wishlist section before `## Open` in
[ROADMAP.md](ROADMAP.md), grounded in the black-block issue.
