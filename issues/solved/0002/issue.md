# Issue 0002

Map: demo
Captured: 2026-10-06
Solved: 2026-10-06

## Description

The player walked to the demo board's right edge, `(39, 13)`, and could not move
right, with no visible reason.

## Resolution

Same root cause as issue 0001: the floor slab was not re-derived when the demo
map set its own board, so floor past the edge was rendered but movement was
refused. Fixed by re-seeding each built-in map's floor to its own board (see
`docs/CHANGELOG.md`). The snapshot was removed with the fix; this note is
retained so the number stays reserved.