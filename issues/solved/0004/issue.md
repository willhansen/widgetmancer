# Issue 0004

Map: portals-and-death-cubes-demo
Captured: 2026-10-07

## Description

I can't see the stars through the portal to the north

## Resolution (2026-10-07)

`Starfield::draw` decided board-vs-void from the portal-unaware
`screen_buffer_character_square_to_world_square` projection. For a screen cell
showing off-board void *through a portal*, that naive square is on-board and
occupied, so `occupied.contains(&world_square)` culled the star even though the
FOV resolved the cell to off-board void (`starfield.rs:163`).

Fixed by adding `FieldOfViewResult::resolved_absolute_square` and testing
occupancy at the square the FOV actually resolves the cell to
(`graphics/starfield.rs`, `fov_stuff.rs`). Regression test
`stars_are_drawn_through_a_portal_over_off_board_void`.

`snapshot_tool diff issues/0004/snapshot` now shows stars where the capture
(whose render also lacked them) had blank void; the remaining 5-cell delta is
transient animation/selector state the snapshot loader intentionally does not
restore.

`screen.txt` was re-blessed to the fixed render; `screen.pre-fix.txt` is the
original capture (missing stars).

