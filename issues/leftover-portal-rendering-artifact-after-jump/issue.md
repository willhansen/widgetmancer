Up and to the right of the player (closer to the upper right corner than the player) there is a rendering artifact that looks like red-tinted floor.

Also, it looks like the starfield is flickering on the far right of the screen.

Note that the captured snapshot does not appear to display this behaviour, so it may be purely terminal rendering.

## Reproduction status (2026-09-28)

Not reproducible from `snapshot/`, matching the issue's own note: a scan of the
current headless render found **0** red-tinted cells in the "up and to the
right" quadrant (world `+x,+y` from the player). Needs a live run / fresh
capture; may indeed be terminal-side.

The starfield-flicker half is plausibly addressed by the FOV starfield-mask fix
(stars are now confined to off-board cells inside the FOV; see
`docs/CHANGELOG.md` 2026-09-28).
