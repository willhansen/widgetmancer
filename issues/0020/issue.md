# Issue 0020

Map: cubes
Captured: 2026-10-11

## Description

The column downward one left of the player. Why does the portal tint outside its edge, and why can't I see the cube column

## Hypothesis (2026-10-11) — DEFERRED

Both symptoms are the portal's depth-1 sub-view leaking onto the two squares
*behind* the portal (`world(4,8)`, `(3,8)`), where it paints the red tint and
overwrites the directly-visible cube (top and edge).

Evidence from `snapshot_tool fov-trace-json issues/0020/snapshot`:

- The portal crossing at `abs(6,8)` reports
  `visible_arc_of_portal = [149.04, 168.69]`.
- That is the **-x face at x = 5.5** (corners `(5.5,7.5)`, `(5.5,8.5)`), i.e.
  the far side from the player at `(8,7)`. The face the player actually sees is
  the **+x face at x = 6.5**, arc `[135, 161.57]`.
- The computed arc `[149.04, 168.69]` covers the directions to `(6,8)` 153.4,
  `(5,8)` 161.6, `(4,8)` 166.0, `(3,8)` 168.7; the +x face arc covers only
  `(6,8)` and `(5,8)`.
- `fov_stuff.rs:1255` builds that arc via
  `AngleInterval::from_square_face_and_center_offset(relative_square,
  portal.entrance().direction(), ..)`, and `fov_stuff.rs:1342` reuses it as the
  view-**blocking** arc, so the direct view of `(4,8)`/`(3,8)` is shadowed and
  the portal view fills it. `explain` confirms each cell has a `depth 0` direct
  cube (partial) *and* a `depth 1` portal view, with the deeper view on top.

Leading cause: the entrance face is taken on the wrong side (`entrance_dir` is
the movement-*into* direction per the map docs, but
`from_square_face_and_center_offset` treats it as the outward face normal),
shifting the portal's aperture/occlusion arc one square.

Ruled out: flipping the draw order in `drawable_at_relative_square`
(deeper-on-top -> shallower-on-top) breaks all 18 portal fixtures, so the fix
must target the portal's arc, not the global compositing order.

Not yet confirmed: the face-normal convention (`MapDir::to_step` Right = `+x`
vs. the stored `Portal::entrance().direction()`), and whether the intended
behavior is that a free-standing portal's view is confined to the portal's own
square(s). **Deferred** until the square-indicator UI (issue 0021) lets the
reporter name the exact squares.

## Root cause (2026-10-12) — same defect class as 0022/0024/0025

Confirmed with the new `snapshot_tool screen-explain` full-frame dump and the
`render --no-portal-tint` / `--no-terrain-pass` toggles.

Both symptoms are the depth-1 portal sub-view being composited and tinted on
cells the portal does not fully cover, plus the terrain pass skipping the
direct cube where the portal view wins the cell:

- `screen-explain issues/0020/snapshot | grep 'abs(4,8)'` shows the flagged
  cells with `d0 abs(4,8) abs_vis="🬭🬭" db=solid` (a direct partial cube top)
  **and** `d1 abs(19,20) … db=solid` (the portal sub-view), depth-1 on top.
- `fov_stuff.rs:900-906` and the terrain pass (`graphics.rs:481`) tint by
  `portal_depth()` alone, with no aperture-coverage test, and
  `PartialVisibilityDrawable::tinted` (`graphics/drawable.rs:232-239`) tints the
  `OUT_OF_SIGHT_COLOR` **shadow** background red. That is the red wash on the
  half of the cell outside the portal — "tint appears outside its edge".
- `load_screen_buffer_from_terrain` uses `resolved_visibility` (topmost =
  deepest) for the whole column and only draws the wall when `wall_frame_matches
  && within_frame` (`graphics.rs:499-530`), so a column whose cell is won by the
  portal view gets no direct top/wall — "can't see the cube column".

Model-level corroboration: `snapshot_tool invariants` reports 101
visibility-consistency violations on this capture (the same absolute square
fully visible via one path, partially via another). This is the same class as
`solved/black-block-deep-in-portal`, whose fix (`combined_with_unioning_arcs`)
covered only co-root portal *slices*; main-view/sub-view and the top-level fold
still merge with `combined_with` (`union_arcs=false`) via
`combined_increasing_visibility` (`fov_stuff.rs:105-130,1370,1499`).

Fix (a) — stop tinting the shadow background in
`PartialVisibilityDrawable::tinted` — is **applied** (2026-10-12; see 0025);
it removes the red wash from the shadow half. The portal sub-view still
overwrites the direct cube at aperture-edge cells (fix (b), separate).

The "entrance face taken on the wrong side" lead above is **not** supported: the
aperture arc `[149.04, 168.69]` is the -x face, which is the aperture the
entrance direction describes; the defect is downstream of the arc.

**Leading hypothesis for the remaining defect (fix (b), unconfirmed).** The FOV
assigns each relative cell to exactly one topmost view (`resolved_visibility`,
deepest wins) and the terrain pass draws that view's column there
(`graphics.rs:436-462`). At a portal's aperture edge the same cell can be
partially covered by both the direct view and the depth-1 portal view (the 101
`invariants` violations). The leading hypothesis is that the correct model is
*coverage-aware*: each relative cell should be owned by the nearest view, with
partial coverage only where geometry actually occludes, and the terrain pass
should draw the direct column's top/wall on the part of the cell the direct view
owns, falling back to the portal view only where the direct view does not reach.
A compositing-only variant was prototyped and rejected (the wall comes from the
terrain pass, not the drawable compositor). Confirming this requires changing the
FOV visibility merge (`fov_stuff.rs`: the `SquareVisibility` half-plane /
`combined_*` path) and re-blessing the `cubes` fixtures. See 0022/0024/0026.

