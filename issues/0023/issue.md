# Issue 0023

Map: cubes
Captured: 2026-10-12

## Description

When looking at conveyor belts through a portal, so the belts are
partial tiles, they look like they aren't moving. This is less than
ideal. Better to show average color of the tile rather than just
use the single dominant color? Maybe do some font analysis to do a
pixel perfect averaging? Worth it?

## Note (2026-10-12)

Separate from the 0020/0022/0024/0025 cluster (which is the portal sub-view
tint/coverage defect). A partial tile is rendered by
`PartialVisibilityDrawable::to_glyphs` (`graphics/drawable.rs:192-216`) as a
single angled-block glyph in one `fg_color`; a conveyor belt's animated phase
lives in its two colors / half-glyph, which a partial tile discards. So "belts
look static through a portal" is expected under the current model, and the
description's own suggestion (average the tile's color) is an enhancement, not a
regression. `snapshot_tool invariants` reports 210 violations, but those are the
cluster defect, not this one.

## Flagged regions

1. world(26, 17)..(32, 19) — screen(56, 31)..(62, 33)

## Assessment (2026-10-12) — enhancement, deferred

A partial tile renders as a single angled block in one `fg_color`
(`PartialVisibilityDrawable::to_glyphs`, `graphics/drawable.rs`), so a conveyor
belt's two-colour animation phase is discarded and the belt reads as static.
Averaging the tile's colours (the report's suggestion) would remove the
half-and-half pattern but still not convey motion. Showing motion through a
partial would need per-character compositing (ROADMAP item 15). Deferred as an
enhancement, not a regression.
