In the portal view to the right of the player, the starfield is drawn on top of the floor.  There shouldn't be stars, visible there.  It's floor.

## Resolution (2026-09-28)

Fixed with `starfield-visibility.md`: the starfield skips cells whose
player-relative square is in the FOV, which includes portal-view floor drawn
into off-board *screen* cells.
