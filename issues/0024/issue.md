# Issue 0024

Map: cubes
Captured: 2026-10-12

## Description

I would expect to see the side of the cube here.  Why can't I?

## Root cause (2026-10-12)

Same defect as 0020/0022/0025 (one root cause for the cluster).

- Every flagged cell resolves to depth-0 *void* (`screen-explain`:
  `d0 abs(41,26) abs_vis="  " db=<none>`); the cube side the reporter expects is
  drawn one screen square to the side by the raised-terrain forward pass, and
  the cells they flagged are left to the flat composite / starfield.
- `render issues/0024/snapshot --no-terrain-pass` drops the entire `▒` wall
  strip, confirming the forward pass draws it; `--no-portal-tint` drops the red
  wash that covers the boundary cells.
- `snapshot_tool invariants` reports 46 visibility-consistency violations (the
  same absolute square fully visible via one path, partially via another).

The portal sub-view at the aperture edge is composited (and tinted) over cells
the portal only partly covers, and the forward pass does not redraw the direct
cube there, so the side reads as absent.

**Leading hypothesis for the remaining defect (fix (b), unconfirmed).** The FOV
assigns each relative cell to exactly one topmost view (`resolved_visibility`,
deepest wins) and the terrain pass draws that view's column there
(`graphics.rs:436-462`). At a portal's aperture edge the same cell can be
partially covered by both the direct view and the depth-1 portal view (the
`invariants` violations). The leading hypothesis is that the correct model is
*coverage-aware*: each relative cell should be owned by the nearest view, with
partial coverage only where geometry actually occludes, and the terrain pass
should draw the direct column's top/wall on the part of the cell the direct view
owns. A compositing-only variant was prototyped and rejected (the wall comes from
the terrain pass, not the drawable compositor). Confirming this requires changing
the FOV visibility merge (`fov_stuff.rs`) and re-blessing the `cubes` fixtures.
See 0022/0026.

## Flagged regions

1. world(41, 26)..(45, 29) — screen(47, 40)..(50, 44)
