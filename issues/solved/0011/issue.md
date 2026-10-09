# Issue 0011

Map: cubes
Captured: 2026-10-09

## Description

can see cube sides through portal to right. If player takes one step further to the right, large parts of the cube sides (but not all of them) abruptly vanish.

## Resolution (2026-10-09)

The forward terrain column pass checked occlusion only at each column's *top*
relative cell, then wrote the camera-facing wall up to `camera_altitude - z`
rows below it with no FOV check at the wall's own cell. On the raised `cubes`
map the z-shifted wall of a directly visible cube therefore painted into cells
the portal recursion resolves to off-board void — the same artifact class as
0009, which the top-cell check alone missed. Stepping toward the portal flipped
those cells to a direct/void resolution, so the cube side appeared to vanish
abruptly.

`Graphics::load_screen_buffer_from_terrain` now also requires a wall's cell to
resolve to the same view frame as its column (`absolute_fov_center_square`)
before writing it; a cell resolved to a different portal frame leaves the wall
unpainted. Regression test
`terrain_walls_do_not_paint_through_a_portal_view`.

`screen.txt` re-blessed; `screen.pre-fix.txt` is the original capture.
