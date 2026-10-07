When the player steps through a rotating portal, all the glyphs in the border of the player fov appear to rotate. This is not the intended behaviour.  The frame is supposed to be a static ui element.

Note that the way the frame turns out after rotation is actually kind of interesting, and I'd like it to be saved as reference somewhere

## Resolution (2026-10-06)

Fixed by the screen-space `UiLayer`. The border was authored in player-relative
world squares and projected through `Screen`'s camera, so `q`/`e` view rotation
turned it. `crates/game/src/graphics/fov_border.rs` now paints into a
`terminal_rendering::ui_layer::UiLayer` (no camera) positioned by screen offset
from the player's screen square, so it is a static UI element again.
Regression test: `border_is_independent_of_camera_rotation` compares the layer
across all four rotations.

The "interesting rotated frame as reference" wish is recorded in
`docs/vision/ideas.md`; the buggy output itself was not saved.
