# Issue 0014

Map: cubes
Captured: 2026-10-09

## Description

The right glyph of the player square seems to show a conveyor belt going up rather than down as would be correct.

## Resolution (2026-10-09)

The player is drawn over the belt via `draw_above_square`, which composited the
belt's half-glyph into the arrow's `TextDrawable` immediately (in world
orientation). The FOV/terrain pass then rotates the composite for the view, but
`ArrowDrawable::rotated` only re-derives the arrow glyph — the baked belt half
stayed unrotated, so under the 180° view it read as going up while the belt run
beside it went down.

Floor features are now kept as their own layer: `draw_above_square` wraps a
conveyor belt under content in a `LayeredDrawable`, whose `rotated` rotates both
layers before compositing (and `to_glyphs` reuses the same `drawn_over` logic).
The belt half under the player now matches the rotated run.

Tests: `belt_under_the_player_rotates_with_the_view` (game) and
`belt_beneath_content_rotates_with_the_composite` (drawable). `screen.pre-fix.txt`
is the original capture; `screen.txt` is the fixed render.

