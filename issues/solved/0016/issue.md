# Issue 0016

Map: cubes
Captured: 2026-10-09

## Description

For this map, I want each conveyor belt going a different speed.

I also want one conveyor belt with each segment going a different speed.  slow at each end, linearly ramping to a maximum speed at the center, then ramping down again.

## Resolution (2026-10-09)

Belts were direction-only over a single global movement period (2s). Each belt
square now owns a `ConveyorBelt { direction, movement_period }`: a shorter
period is a faster belt. Movement steps a grid entity only when *that* belt's
period boundary is crossed, and pushes floating entities by `speed * delta`;
the visual phase runs on each belt's own `2 * period`.

`MapOp::ConveyorBelt` gains an optional `speed` multiplier (default 1.0), and
the snapshot round-trips a non-default `period_millis` (old captures default to
the original speed). `maps/cubes.json` now gives the runs different speeds
(west 0.5, east 2.0, top-right 0.75) and ramps the vertical run's segments slow
at the ends to fastest in the middle (0.25, 0.5, 0.75, 1.5, 1.5, 0.75, 0.5,
0.25).

Tests: `faster_conveyor_belt_steps_grid_entities_on_its_own_period`,
`faster_conveyor_belt_pushes_floating_entities_farther`,
`conveyor_belt_visual_phase_follows_its_own_period`,
`map_file_parses_and_applies_ops` (speed), and the snapshot round-trip now
includes a non-default-speed belt.

