# Issue 0019

Map: cubes
Captured: 2026-10-10
Solved: 2026-10-10

## Description

The portal border diagonal down left of the player has incorrect tinting on and off the big cube

## Resolution (2026-10-10)

Same root cause as 0017/0018. The player stands on a 10-tall cube at a portal
entrance, so the diagonal portal border is a run of *partially* visible top
faces. The draw buffer's base under every occupied square was the flat floor
pattern, and the forward voxel pass recolors raised tops only when they are
fully visible (`graphics.rs`, `if !is_partial`), leaving partial tops with the
flat composite's floor-derived colors.

Confirmed with `snapshot_tool explain` at buffer `(53,32)`: the resolved squares
`abs(32,30)` and `abs(28,19)` are both cubes (top voxel 9), yet the partial
rendered `fg(140,114,114)` / `bg(191,191,191)` — floor colors — while the
fully-visible neighbour showed the cube material `(96,104,128)`.

Fixed by giving each occupied square its own material as the draw-buffer base
(`Graphics::draw_surface_base`). The diagonal now renders the cube material
tinted by portal depth (`fg(112,94,115)` / `bg(69,75,92)`), matching the cube on
both sides of the border.

Regression test `partial_top_on_a_raised_column_uses_the_material_not_the_floor`
(fails pre-fix). `screen.pre-fix.txt` is the original capture; `screen.txt` is
the fixed render.
