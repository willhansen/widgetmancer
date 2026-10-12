# Issue 0026

Map: cubes
Captured: 2026-10-12

## Description

Square 1 seems to be tinted outside the portal.

Section 2 seems to be not as red-tinted as I would exped.

## Root cause (2026-10-12) — same cluster as 0020/0022/0024/0025

Both complaints are the portal sub-view owning the wrong cells (the FOV
aperture-coverage problem), not the tint strength:

- **Region 1** `world(16,28)` / `screen(44,21)`:
  `screen-explain` reports `d0 abs(16,28) abs_vis="█🭐" db=solid` (a direct
  partial cube) **and** `d1 abs(12,11) rot=3 abs_vis="🭈🬭" db=<none>`. The final
  glyph is a `▒` wall tinted `(67,44,55)` — i.e. the depth-1 portal sub-view's
  wall is drawn (and tinted) on a cell the reporter reads as outside the portal.
- **Region 2** `world(14,36)..(15,44)` / `screen(42,5)..(43,13)`: every cell is a
  fully-visible depth-1 portal view of **void** (`abs(20..28,12..13)`, the gap
  row between cubes) with `db=<none>`, so there is nothing to tint and the
  starfield shows through — "not as red as I would expect."

So the tint appears on cells the portal does not cover (region 1) and is absent
from cells it does cover but that have no drawable (region 2). `snapshot_tool
invariants` reports 141 visibility-consistency violations on this capture.

## Status

Not addressed by fix (a) (0025), which only stopped tinting the `OUT_OF_SIGHT`
shadow half.

**Leading hypothesis for the remaining defect (fix (b), unconfirmed).** The FOV
assigns each relative cell to exactly one topmost view (`resolved_visibility`,
deepest wins) and the terrain pass draws that view's column there
(`graphics.rs:436-462`). At a portal's aperture edge the same cell can be
partially covered by both the direct view and the depth-1 portal view (the 141
`invariants` violations). The leading hypothesis is that the correct model is
*coverage-aware*: each relative cell should be owned by the nearest view, with
partial coverage only where geometry actually occludes, and the terrain pass
should draw the direct column's top/wall on the part of the cell the direct view
owns, falling back to the portal view only where the direct view does not reach.
A compositing-only variant was prototyped and rejected (the wall comes from the
terrain pass, not the drawable compositor). Confirming this requires changing the
FOV visibility merge (`fov_stuff.rs`: the `SquareVisibility` half-plane /
`combined_*` path) and re-blessing the `cubes` fixtures. See 0022/0024.

## Flagged regions

1. world(16, 28)..(16, 28) — screen(44, 21)..(44, 21)
2. world(14, 36)..(15, 44) — screen(42, 5)..(43, 13)
