# Issue 0009

Map: cubes
Captured: 2026-10-08

## Description

Why can I see cube sides through the portal just to the north of the player?

## Resolution (2026-10-08)

Same root cause as 0008: `Graphics::load_screen_buffer_from_terrain` projected
each raised column at its absolute world→screen square and gated walls with
`can_see_relative_square(column - root)`, which the portal recursion also
satisfies. A nearby cube's side wall was therefore painted where its absolute
projection lands, even when that cell is resolved through the portal to
off-board void — `snapshot_tool explain` showed e.g. cell `(75,15)` resolving
only to `depth 1 abs(18,50)` (no drawable) yet carrying a terrain wall.

Fixed by making the forward pass portal-aware: the FOV's
`PositionedSquareVisibilityInFov` (absolute square + relative square + portal
depth + rotation) places each column at the cell it is actually seen at, applies
the portal red tint, and maps the camera-facing wall direction through the
portal rotation. Writes are also clamped to the FOV frame (0007). Regression
tests `raised_column_seen_only_through_a_portal_is_drawn_at_the_apparent_cell`
and `raised_terrain_is_clipped_to_the_fov_frame`; `snapshot_tool diff` on this
capture, `0007`, and `0008` now matches.

`screen.txt` was re-blessed to the fixed render; `screen.pre-fix.txt` is the
original capture (cube sides through the portal).

## Post-fix re-bless (2026-10-09)

Issues 0010/0011 showed the top-cell occlusion check was incomplete: a column's
z-shifted wall could still paint into cells that resolve through the portal to a
different view frame. Walls are now additionally gated by the frame of their own
cell, which removes the remaining spurious walls from this capture. `screen.txt`
re-blessed; `screen.pre-fix.txt` is unchanged (the 2026-10-08 original).


