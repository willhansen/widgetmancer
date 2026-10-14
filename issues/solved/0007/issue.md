# Issue 0007

Map: cubes
Captured: 2026-10-08

## Description

Big cube walls are sticking off the bottom of the fov frame

## Resolution (2026-10-08)

The camera follows the player's surface altitude (issue 0003), so the forward
terrain pass projects a wall voxel at `z` down by `camera_altitude - z` rows
(`Screen::world_square_and_altitude_to_screen_buffer_square`). On a 10-tall cube
that is up to 11 rows below the column's base, and the pass wrote those cells
with no viewport bound: on this capture 300 wall cells landed at screen rows
59-63 while the FOV frame's bottom is row 58.

Fixed by clamping the forward pass to the FOV frame
(`sight_radius + 1`): `Graphics::load_screen_buffer_from_terrain` now drops any
wall or top write outside that Chebyshev ring, leaving those cells to the
starfield. Regression test `raised_terrain_is_clipped_to_the_fov_frame`.

`screen.txt` was re-blessed to the fixed render; `screen.pre-fix.txt` is the
original capture (walls spilling below the frame).

## Post-fix re-bless (2026-10-09)

The wall-frame gate added for 0010/0011 also drops walls that the frame clip
alone let through (z-shifted walls landing on cells resolved to a different view
frame). `screen.txt` re-blessed; `screen.pre-fix.txt` is unchanged (the
2026-10-08 original).


## Post-fix re-bless (2026-10-12)

The forward pass's wall-frame gate now also drops a column wall whose cell is
*resolved to a void portal view* while a different view frame actually paints it
(the deepest visibility has no drawable, so the flat composite falls through to a
shallower frame). This is the over-painting bug behind issues 0020/0026: a portal
sub-view of void painted its cube side over a directly-drawn cube. `screen.txt`
re-blessed; `screen.pre-fix.txt` unchanged.
