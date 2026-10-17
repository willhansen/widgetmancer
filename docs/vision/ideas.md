Entities can face in directions.  Floating point directions.

- After getting floating squares to go through portals, I realized there's no easy test map to prove that it's working with just a short visual inspection.

## Saved from solved/player-border-rotation

The FOV border used to rotate with the view when the player stepped through a
rotating portal (the border was authored in world squares and projected through
the camera). That was fixed, but the accidental rotated-frame geometry was
called "kind of interesting, and I'd like it to be saved as reference
somewhere." Consider re-creating it as a deliberate decorative variant (an
ornate frame that rotates with the view) rather than discarding the look.

## Issue review: animation

A single static "after" picture cannot represent a bug that only shows in motion
(e.g. issue 0023's conveyor-belt animation). Consider letting `issue_review`
play back a few seconds: step the capture's logical time (or `simulate_keys`)
and render N frames in sequence, so one pane is an animation rather than a
still.

