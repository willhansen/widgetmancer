# Issue 0001

Map: demo
Captured: 2026-10-06
Solved: 2026-10-06

## Description

The player walked to the demo board's top edge, `(19, 23)`, and could not move
up, with no visible reason.

## Resolution

The floor slab was seeded for the terminal-derived board and not re-derived when
the demo map shrank the board to 40x24, so walkable-looking floor was drawn past
the edge while movement was refused. Fixed by re-seeding each built-in map's
floor to its own board (see `docs/CHANGELOG.md`). The snapshot was removed with
the fix; this note is retained so the number stays reserved.