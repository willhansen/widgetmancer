# Issue 0005

Map: portals-and-death-cubes-demo
Captured: 2026-10-07

## Description

When the player takes a step to the right, through the portal, the stars off to the right abruptly shift.

## Resolution (2026-10-07)

The starfield anchored its parallax camera on the player's absolute square
(`screen_center_as_world_square`). The board is composited through the
portal-aware FOV, so a portal crossing keeps the view continuous, but the
player's absolute square jumps by the portal displacement (here `(23,12)` ->
`(28,17)`, a `(5,5)` step). The star lattice therefore shifted by
`parallax * displacement` (up to ~3 world squares on the near layer) instead of
the one apparent step.

Fixed by painting the starfield once per FOV view frame, with that frame's root
as the camera (`FieldOfViewResult::view_frame_roots`), and gating each star to
the frame its cell actually resolves to (topmost visibility). With no portals
there is a single frame rooted at the player, so the output is byte-identical to
before. `issues/solved/0003/snapshot` (portal-free) still diffs clean.

Regression test: `stars_seen_through_a_portal_use_the_destination_frames_camera`
(portal frame stars equal the translated direct view from the frame root; fails
against the old player-anchored camera).

`screen.pre-fix.txt` keeps the captured (buggy) render.
