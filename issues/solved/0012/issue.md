# Issue 0012

Map: cubes
Captured: 2026-10-09

## Description

Can't see correct cube sides through portal.  also the non-white parts of the conveyor belts seem to be missing.

## Resolution (2026-10-09)

Two causes:

- **Cube sides.** Same root cause as 0011: the forward terrain pass validated
  occlusion only at a column's top cell, but wrote its z-shifted wall into
  cells that may resolve through the portal to a different view frame. The wall
  is now gated by the view frame of its own cell (see 0011).
- **Conveyor belts.** The top-face pass overwrote every top face's background
  with the cube material, which clobbered a conveyor belt's intentional black
  background (its drawable colors are `[WHITE, BLACK]`). The pass now composites
  a `ConveyorBelt` over the material base instead of recoloring it. Regression
  test `conveyor_belt_on_a_raised_column_keeps_its_own_colors`.

`screen.txt` re-blessed; `screen.pre-fix.txt` is the original capture.
