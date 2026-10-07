# Issue 0003

Map: cubes
Captured: 2026-10-07

## Description

the big cubes are going out of the frame at the top.

It is also difficult to see the edges of the cube.

## Resolution (2026-10-07)

Two independent causes.

**1. The cubes were above the frame because the camera ignored the player's
altitude.** The player stands on a 10-tall cube (`player_altitude() == 10`),
but `update_screen_from_draw_buffer` centred the camera on the player's *ground*
square while the projection lifts terrain by `-altitude` (`Screen::
world_square_and_altitude_to_screen_buffer_square`). The player glyph therefore
rendered 10 rows above the frame centre and the north cubes projected out the
top. `Screen` now carries a `camera_altitude` (the player's surface height) and
the projection subtracts `altitude - camera_altitude`, so the player renders at
the frame centre and cubes stay inside the border. Flat boards (altitude 0) are
byte-for-byte unchanged.

**2. Cube edges were hard to read.** Top faces are background-only fills and the
material tint + fog made them a flat dark patch against the void. Exposed top
faces now get a bright rim (`UPPER_HALF_BLOCK`, `terrain_rim_color`) on their
far drop-off edge, so the cube silhouette reads.

A separate loader bug made the headless render differ by 1488 cells: the
snapshot did not serialize the map's `sight_radius` (24 for `cubes`), so the
loader fell back to `PLAYER_SIGHT_RADIUS = 16`. Fixed by round-tripping
`sight_radius`; the capture was backfilled with the live value.

`issues/0003/snapshot/screen.pre-fix.txt` is the original capture (cubes poking
above the frame); `screen.txt` was re-blessed to the fixed render. Regression
tests: `player_renders_at_the_frame_center_on_a_raised_column`,
`raised_terrain_shifts_the_top_face_and_draws_a_wall` (rim),
`snapshot_round_trips_sight_radius`.
