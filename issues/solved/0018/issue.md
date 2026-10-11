# Issue 0018

Map: cubes
Captured: 2026-10-09
Solved: 2026-10-10

## Description

The tinting of portals on this map doesn't seem quite right at portal edges.  eg one step up left of the player.

## Resolution (2026-10-10)

Same root cause as 0017/0019. The player stands on a 10-tall cube, so the cells
at the portal edge are *partially* visible top faces. The draw buffer's base
under every occupied square was the flat floor pattern, and the forward voxel
pass recolors raised tops only when they are fully visible. A partially-visible
top was therefore rendered from the flat composite's
`PartialVisibilityDrawable`, whose colors come from the floor draw buffer, so
the portal edge showed the floor pattern (and floor tint) instead of the cube
material.

Confirmed with `snapshot_tool explain` at the reported cell (one step up-left,
buffer `(54,30)`): the resolved squares `abs(5,7)` and `abs(20,21)` are both
cubes (top voxel 9), yet the partial rendered `fg(140,114,114)` /
`bg(127,127,127)` — floor colors — while the fully-visible neighbour showed the
cube material `(96,104,128)`.

Fixed by giving each occupied square its own material as the draw-buffer base
(`Graphics::draw_surface_base`). The cell now renders `fg(112,94,115)` /
`bg(96,104,128)` — the cube material tinted by portal depth.

Regression test `partial_top_on_a_raised_column_uses_the_material_not_the_floor`
(fails pre-fix). `screen.pre-fix.txt` is the original capture; `screen.txt` is
the fixed render.
