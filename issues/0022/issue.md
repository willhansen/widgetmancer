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


## Update (2026-10-12) — leading hypothesis refuted; over-painting bug fixed

The **coverage-aware FOV merge / nearest-view ownership** hypothesis above is
**refuted**. `snapshot_tool visibility-pairs` (new: `Game`'s
`fov_visibility_pairs_report`, wired into `bin/snapshot_tool.rs`) checks every
relative cell with more than one visibility and finds the direct and portal
half-planes are **exact complements**:

| capture | multi-visibility cells | exact complements |
|---|---|---|
| 0020 | 116 | 115 |
| 0022 | 121 | 121 |
| 0024 | 112 | 112 |
| 0025 | 112 | 112 |
| 0026 | 172 | 172 |

So the FOV already partitions each cell correctly; `combined_*` is never involved
across depths, and one `SquareVisibility` cannot carry two contents anyway.

The confirmed defect is the terrain forward pass's wall-frame gate
(`graphics.rs::load_screen_buffer_from_terrain`): it compared the **deepest**
visibility's frame, but the flat composite paints the topmost visibility *that
has a drawable*. When the deepest view is a portal view of void (no drawable), a
portal column could paint its wall over a directly-drawn cube. Fixed by also
dropping a wall whose own cell resolves to a void view while a different frame
paints it (suppression-only); regression test
`portal_void_wall_does_not_overpaint_a_direct_cube` (shown to fail pre-fix).
Re-blesses portal fixtures 0007/0008/0010-0014/0016/0017.

The flagged region (`world(-8,9)..(-5,11)`) is fully-visible depth-0 **void**
(no portal sub-view competes), so it is not the over-painting class. The cube
side the reporter expects is simply not projected into those cells; issue stays
open. 0023 (conveyor animation) is unrelated.

## Investigation (2026-10-12) — the remainder is a model-level limitation

`SNAP_DEBUG_SKIP` instrumentation of the forward pass, plus
`visibility-pairs`, show the remaining symptom is not a wrong frame-gate
decision but one of two model limitations:

1. **Complementary-pair compositing.** Where a direct view and a portal view
   split a cell into complementary half-planes, `drawable_at_relative_square`
   draws the deeper (portal) drawable's glyph and reduces the shallower (direct)
   half to a flat colour. One terminal cell carries one glyph, so the direct
   cube's shape/edge cannot appear beside the portal content. (ROADMAP item 15.)
2. **Wall projection vs height occlusion.** The forward pass gates each
   column's z-shifted wall by the view frame of its own cell. Relaxing it so a
   portal column may draw its wall over a void cell re-adds walls that the
   0008/0017 fixes deemed spurious across seven fixtures (0007 +37, 0008 +60,
   0012 +49, 0017 +78, 0009 +19 …), so it is not safe without a real occlusion
   model.

Applied here: the flagged cells `(46,46..49)` are depth-0 **void** with a
gate-dropped depth-1 portal wall (`col abs(16,14)`). Drawing it is limitation 2.
