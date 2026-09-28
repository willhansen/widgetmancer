The starfield is visible off the game board, with no regard to the player's field of view.  This is incorrect.  The field of view determines what the player can see.  If off-the-board is not in fov, it should not be rendered.

## Resolution (2026-09-28)

`Starfield::draw` now also skips any off-board cell whose player-relative
square is in the FOV (passed as `Option<&FieldOfViewResult>`; `None` when the
player is dead and the whole board is rendered). Regression:
`no_stars_inside_the_field_of_view`.
