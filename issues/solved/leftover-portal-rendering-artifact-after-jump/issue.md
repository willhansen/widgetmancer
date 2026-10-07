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

## Retirement (2026-10-07)

Retired as non-reproducible. No code path in the headless renderer produces a
red-tinted floor at the reported offset from `snapshot/`, and the starfield is a
pure function of `(screen, board, time)` (`graphics/starfield.rs`), so it cannot
flicker frame-to-frame in a headless render. The remaining candidate is
terminal-side redraw/scroll behavior that the snapshot format cannot capture.

The two deterministic rendering paths that could have produced red-tinted floor
near the player — a stale portal-view floor and the starfield mask — were both
already fixed (portal FOV arc union, 2026-09; starfield mask, 2026-09-28). A pty
live-capture harness would be needed to chase the terminal-side possibility; that
work is not worth keeping the issue open. If it recurs, re-capture with a live
`--load` session and file a fresh issue.
