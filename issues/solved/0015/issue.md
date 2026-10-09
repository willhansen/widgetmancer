# Issue 0015

Map: cubes
Captured: 2026-10-09

## Description

voxel sides should be a different color from voxel tops. Because it looks like I should be able to step down here, but I can't

## Resolution (2026-10-09)

The wall gradient ran from `TERRAIN_WALL_BASE` toward `terrain_tint(material)`,
and for a `Tint` material that is the material color itself — so the wall
reached exactly the top face's light checker color at the top voxel. The cube's
side was indistinguishable from its top, so a step read as a walkable surface.

`terrain_tint` now returns a darkened shade of the material (`WALL_TINT_SHADE`,
0.5) for `Tint`, so a column's exposed side is distinctly darker than its top.
`Floor` is unchanged.

Test: `raised_terrain_shifts_the_top_face_and_draws_a_wall` now also asserts the
wall color differs from the top color (fails before the change).
`screen.pre-fix.txt` is the original capture; `screen.txt` is the fixed render.
The other raised-terrain captures were re-blessed.

