# Issue 0006

Map: portals-and-death-cubes-demo
Captured: 2026-10-07

## Description

Moving left and right makes the stars visible south of the player visually move up and down.  This is not correct.

## Resolution (2026-10-07)

Regression from the first, rotation-less starfield pass for issue 0005: that
pass anchored each portal frame's stars at the frame root but placed them
without applying the frame's rotation, so a rotating portal's sky parallaxed
along the wrong axis (a horizontal move moved those stars vertically).

The follow-up fix ("Starfield parallax turns with the portal frame") maps a
frame offset back through the inverse rotation,
`primary = rotate_{-rotation}(frame_offset)`, in
`crates/game/src/graphics/starfield.rs::frame_to_primary_offset`. The capture
matches the old rotation-less render to within 4 cells; the corrected render
differs by 129, and `stars_seen_through_a_portal_use_the_destination_frames_camera`
now also checks a rotated *screen* (view rotation 1), which fails against the
rotation-less mapping.

`screen.pre-fix.txt` keeps the captured (wrong-axis) render.
