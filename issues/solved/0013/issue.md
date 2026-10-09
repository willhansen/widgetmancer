# Issue 0013

Map: cubes
Captured: 2026-10-09

## Description

Way too much changes when the player steps left through this portal. Edge highlighting, cube sides, voxel checkerboard of the floor.

Also the portal edge coloring seems off.

## Resolution (2026-10-09)

The forward terrain pass collapsed each absolute column to its single
shallowest-portal view. The portal here looks into another cube cluster whose
columns are also directly visible, but at relative cells off the (35-row)
terminal; collapsing to the direct view meant those columns were drawn nowhere,
so the portal cell kept the flat composite's base board checkerboard, red-tinted
by portal depth — the "voxel checkerboard of the floor" and the "off" portal
edge color. Stepping through made those cells direct, swapping the checker for
real terrain ("way too much changes").

`Graphics::load_screen_buffer_from_terrain` now draws per *relative cell* from
`resolved_visibility` — the topmost view, matching the flat composite — so a
column is raised at every cell it is visible through, direct and portal alike.

Regression test: `raised_column_visible_directly_and_through_a_portal_is_drawn_at_both_cells`.
`screen.pre-fix.txt` is the original capture; `screen.txt` is the fixed render.
The cubes-map captures `solved/0007`–`0012` were re-blessed (they gain the
previously-missing portal terrain).
