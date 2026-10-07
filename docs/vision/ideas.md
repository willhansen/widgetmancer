Entities can face in directions.  Floating point directions.

- After getting floating squares to go through portals, I realized there's no easy test map to prove that it's working with just a short visual inspection.

## Saved from solved/player-border-rotation

The FOV border used to rotate with the view when the player stepped through a
rotating portal (the border was authored in world squares and projected through
the camera). That was fixed, but the accidental rotated-frame geometry was
called "kind of interesting, and I'd like it to be saved as reference
somewhere." Consider re-creating it as a deliberate decorative variant (an
ornate frame that rotates with the view) rather than discarding the look.

