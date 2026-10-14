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

**Region 1 fixed.** `world(16,28)` at screen (44,21): the depth-1 portal sub-view
of void (`abs(12,11)`, no drawable) used to paint its wall (`▒`) over the direct
cube top; it now shows the direct cube. **Region 2 remains**: the strip is a
depth-1 portal view of *void* with no drawable, so there is nothing to tint
("not as red as expected") — a separate tint-on-void/coverage concern, not
over-painting.

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

Region 2 (the portal view of the gap row) is a depth-1 view of **void** with no
drawable, so there is nothing to tint; tinting a portal-visible void is a
design choice (limitation 1 does not apply). Region 1 stays fixed.

## Region 2 fixed (2026-10-13)

The portal-depth tint is now applied to the aperture's *empty* cells too:
`Graphics::load_screen_buffer_from_fov` paints a `tint_color(OUT_OF_SIGHT, RED,
0.1 * depth)` background wherever the FOV resolves a cell to a depth>0 portal
view whose absolute square is an **on-board gap** (no drawable), so the whole
opening reads as one red window instead of red cubes with black holes. Off-board
void keeps the starfield (space seen through a portal is meant to show stars;
see the `portal_void_stars` test). Regression test
`portal_view_of_an_on_board_gap_is_tinted` (shown to fail pre-fix: `(42,5)` bg
`(0,0,0)`). Region 2's cells now read `bg=(26,0,0)`. Re-blessed fixtures
`solved/0007`–`0019`. Issue closed.
