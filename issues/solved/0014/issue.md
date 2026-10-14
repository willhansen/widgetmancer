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


## Post-fix re-bless (2026-10-12)

The forward pass's wall-frame gate now also drops a column wall whose cell is
*resolved to a void portal view* while a different view frame actually paints it
(the deepest visibility has no drawable, so the flat composite falls through to a
shallower frame). This is the over-painting bug behind issues 0020/0026: a portal
sub-view of void painted its cube side over a directly-drawn cube. `screen.txt`
re-blessed; `screen.pre-fix.txt` unchanged.
