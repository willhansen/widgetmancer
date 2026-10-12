# Issue 0022

Map: cubes
Captured: 2026-10-11

## Description

Why can't I see cube sides at this region?

## Root cause (2026-10-12)

Same defect as 0020/0024/0025 (this cluster shares one root cause).

`screen-explain` over the flagged band shows the cube's wall is drawn by the
raised-terrain forward pass (`load_screen_buffer_from_terrain`), but the
surrounding cells the reporter expects the side in are won by a depth-1 portal
sub-view:

- `render issues/0022/snapshot --no-terrain-pass` removes every `▒`/`█` wall in
  the region, so the forward pass draws them; `--no-portal-tint` removes the red
  `R` wash but leaves the geometry.
- The remaining cells carry `abs_vis` partials with `bg` = tinted
  `OUT_OF_SIGHT_COLOR`, i.e. the portal sub-view's shadow half is tinted red and
  its topmost compositing suppresses the direct cube edge
  (`fov_stuff.rs:900-906`, `graphics/drawable.rs:232-239`).
- `snapshot_tool invariants` reports 87 visibility-consistency violations.

So "can't see the cube side" is not a missing wall in isolation: the portal
sub-view is composited over the cells at the portal/aperture edge where the
direct cube side should show, and the forward pass leaves those cells to the
portal composite.

**Leading hypothesis for the remaining defect (fix (b), unconfirmed).** The FOV
assigns each relative cell to exactly one topmost view (`resolved_visibility`,
deepest wins) and the terrain pass draws that view's column there
(`graphics.rs:436-462`). At a portal's aperture edge the same cell can be
partially covered by both the direct view and the depth-1 portal view (the
`invariants` violations). The leading hypothesis is that the correct model is
*coverage-aware*: each relative cell should be owned by the nearest view, with
partial coverage only where geometry actually occludes, and the terrain pass
should draw the direct column's top/wall on the part of the cell the direct view
owns, falling back to the portal view only where the direct view does not reach.
A compositing-only variant (render the layer behind a partial in its shadow
half) was prototyped and rejected: it changed renders but did not restore the
missing side, because the wall comes from the terrain pass, not the drawable
compositor. Confirming this requires changing the FOV visibility merge
(`fov_stuff.rs`: the `SquareVisibility` half-plane / `combined_*` path) and
re-blessing the `cubes` fixtures. See 0026.

## Flagged squares

### 1. region world(-8, 9)..(-5, 11) — screen(46, 46)..(48, 49)

world(-8, 9) — screen(48, 49):

```
cell screen-square (48, 49) = char cols 96..97 (row 49); world(-8,9) rel(-18,4) fov_center(10,5):
  final: ' ' fg(255,255,255) bg(0,0,0)
  fov visibilities: 1
    depth 0 abs(-8,9) rot 0 abs_vis "  " rel_vis "  "
```

world(-7, 9) — screen(48, 48):

```
cell screen-square (48, 48) = char cols 96..97 (row 48); world(-7,9) rel(-17,4) fov_center(10,5):
  final: ' ' fg(255,255,255) bg(0,0,0)
  fov visibilities: 1
    depth 0 abs(-7,9) rot 0 abs_vis "  " rel_vis "  "
```

world(-6, 9) — screen(48, 47):

```
cell screen-square (48, 47) = char cols 96..97 (row 47); world(-6,9) rel(-16,4) fov_center(10,5):
  final: ' ' fg(255,255,255) bg(0,0,0)
  fov visibilities: 1
    depth 0 abs(-6,9) rot 0 abs_vis "  " rel_vis "  "
```

world(-5, 9) — screen(48, 46):

```
cell screen-square (48, 46) = char cols 96..97 (row 46); world(-5,9) rel(-15,4) fov_center(10,5):
  final: ' ' fg(255,255,255) bg(0,0,0)
  fov visibilities: 1
    depth 0 abs(-5,9) rot 0 abs_vis "  " rel_vis "  "
```

world(-8, 10) — screen(47, 49):

```
cell screen-square (47, 49) = char cols 94..95 (row 49); world(-8,10) rel(-18,5) fov_center(10,5):
  final: ' ' fg(255,255,255) bg(0,0,0)
  fov visibilities: 1
    depth 0 abs(-8,10) rot 0 abs_vis "  " rel_vis "  "
```

world(-7, 10) — screen(47, 48):

```
cell screen-square (47, 48) = char cols 94..95 (row 48); world(-7,10) rel(-17,5) fov_center(10,5):
  final: ' ' fg(255,255,255) bg(0,0,0)
  fov visibilities: 1
    depth 0 abs(-7,10) rot 0 abs_vis "  " rel_vis "  "
```

world(-6, 10) — screen(47, 47):

```
cell screen-square (47, 47) = char cols 94..95 (row 47); world(-6,10) rel(-16,5) fov_center(10,5):
  final: ' ' fg(255,255,255) bg(0,0,0)
  fov visibilities: 1
    depth 0 abs(-6,10) rot 0 abs_vis "  " rel_vis "  "
```

world(-5, 10) — screen(47, 46):

```
cell screen-square (47, 46) = char cols 94..95 (row 46); world(-5,10) rel(-15,5) fov_center(10,5):
  final: ' ' fg(255,255,255) bg(0,0,0)
  fov visibilities: 1
    depth 0 abs(-5,10) rot 0 abs_vis "  " rel_vis "  "
```

world(-8, 11) — screen(46, 49):

```
cell screen-square (46, 49) = char cols 92..93 (row 49); world(-8,11) rel(-18,6) fov_center(10,5):
  final: ' ' fg(255,255,255) bg(0,0,0)
  fov visibilities: 1
    depth 0 abs(-8,11) rot 0 abs_vis "  " rel_vis "  "
```

world(-7, 11) — screen(46, 48):

```
cell screen-square (46, 48) = char cols 92..93 (row 48); world(-7,11) rel(-17,6) fov_center(10,5):
  final: ' ' fg(255,255,255) bg(0,0,0)
  fov visibilities: 1
    depth 0 abs(-7,11) rot 0 abs_vis "  " rel_vis "  "
```

world(-6, 11) — screen(46, 47):

```
cell screen-square (46, 47) = char cols 92..93 (row 47); world(-6,11) rel(-16,6) fov_center(10,5):
  final: ' ' fg(255,255,255) bg(0,0,0)
  fov visibilities: 1
    depth 0 abs(-6,11) rot 0 abs_vis "  " rel_vis "  "
```

world(-5, 11) — screen(46, 46):

```
cell screen-square (46, 46) = char cols 92..93 (row 46); world(-5,11) rel(-15,6) fov_center(10,5):
  final: ' ' fg(255,255,255) bg(0,0,0)
  fov visibilities: 1
    depth 0 abs(-5,11) rot 0 abs_vis "  " rel_vis "  "
```

