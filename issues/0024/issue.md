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

The flagged region (`world(41,26)..(45,29)`) is depth-0 **void**; the cube side
is drawn one screen square away by the forward pass. Two nearby cells are fixed
by the over-painting change, but the flagged region is unaffected; issue stays
open.

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

Applied here: the flagged region is depth-0 fully-visible **void**; no wall is
projected into it (no skipped wall lands there), so the side the reporter
expects is simply one screen square away. Needs a fresh read of the intended
geometry rather than a code fix.

## Appended note (2026-10-16)

Issue remains.

## Appended note (2026-10-17)

Still valid, though the highlighted region doesn't look quite right.  Possible the highlighted region itself has rendering issues.
