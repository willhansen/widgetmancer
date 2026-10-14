# Issue 0017

Map: cubes
Captured: 2026-10-09
Solved: 2026-10-10

## Description

Through the portal to the left, you can see another portal. as soon as the player takes one step left, more outer space becomes abruptly visible through that second portal. enough that it becomes clear there should be more visible before taking that step.

## Resolution (2026-10-10)

Same root cause as 0018/0019. The player stands on a 10-tall cube; the cells
around the portal edge are *partially* visible top faces. The draw buffer's base
under every occupied square was the flat floor pattern, and the forward voxel
pass recolors raised tops only when they are fully visible
(`graphics.rs`, `if !is_partial`). Partially-visible tops were therefore
rendered from the flat composite's `PartialVisibilityDrawable`, whose colors
come from the floor draw buffer: the visible half carried the floor pattern, and
a shadow half over off-board void carried an opaque `OUT_OF_SIGHT` background
that sits in the `drawn` set and suppresses the starfield there.

On this capture, stepping left flips a large region from those wrong
portal-edge partials to full raised tops, so the "outer space" (void through the
nested portal) appears to pop in.

Fixed by giving each occupied square its own material as the draw-buffer base
(`Graphics::draw_surface_base`, called from `Game::populate_draw_buffer`): on a
raised map a built-up square's base is its material checker, so portal views and
partial shadows carry the material color; a flat map keeps the floor pattern
byte-for-byte. The forward-pass recolor is left in place (now redundant).

Regression test `partial_top_on_a_raised_column_uses_the_material_not_the_floor`
(fails pre-fix: the partial's visible half reads `(191,191,191)`, the floor
pattern). `screen.pre-fix.txt` is the original capture; `screen.txt` is the
fixed render.

The remaining void-through-portal shadow (the starfield half of this report) is
still a separate, sub-cell concern; see `docs/ROADMAP.md`.

## Post-fix re-bless (2026-10-12)

The forward pass's wall-frame gate now also drops a column wall whose cell is
*resolved to a void portal view* while a different view frame actually paints it
(the deepest visibility has no drawable, so the flat composite falls through to a
shallower frame). This is the over-painting bug behind issues 0020/0026: a portal
sub-view of void painted its cube side over a directly-drawn cube. `screen.txt`
re-blessed; `screen.pre-fix.txt` unchanged.
