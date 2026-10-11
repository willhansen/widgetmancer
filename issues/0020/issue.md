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
