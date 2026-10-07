The player is touching the edge of a death square. Visually, the background of the player doesn't quite make sense. I would expect the very rightmost edge of the player's right character to be colored the same as the death square, but instead the entire of the player's left character background and the *bottom* of the player's right character is colored as the death square. This does not seem correct.

## Reproduction (2026-09-28)

Reproduces from `snapshot/`. The player is adjacent to a death cube *through a
portal*: the cube sits on the far side of a portal whose exit is the player's own
square, so the cube's remapped drawable lands on the player cell.

`snapshot_tool explain snapshot 31 21` (the player's cell, world (47,29)):

```
final: '🢃' fg(20,52,164) bg(9,0,255)        <- left char bg = death-cube color
  draw_buffer: Arrow(... TextDrawable { glyphs: [
    char 🢀 fg(20,52,164) bg(9,0,255) bg_transparent false,
    char ▂ fg(9,0,255)   bg(191,191,191) bg_transparent false ] })
```

So the player's left-half background is wholly the death color and the right half
is a death-colored lower block — matching the report.

### Minimal example

`minimized/` (and `minimized.json`): one zero-velocity death cube + one portal,
screen cropped to 18x9.

- player (47,29)
- death cube `(53.2727, 17)` (id 7)
- portal entrance `(53,17) dir E` → exit `(47,29) dir N`

`snapshot_tool explain minimized 4 4` reproduces the same cell. The portal
remaps the cube's drawable onto the player's square; only ~the north 77% of the
player's square is actually covered, but the arrow `drawn_over` composite fills
the left half's transparent background with the below drawable's solid color and
lets a lower block through on the right half.

Root cause is in the player/`TextDrawable` over `PartialVisibilityDrawable`
compositing (`crates/game/src/graphics/drawable.rs` /
`DoubleGlyph::drawn_over`): the below drawable's per-half shape is not preserved
under an opaque glyph.

## Resolution (2026-10-07)

The flattening is in `Glyph::drawn_over` (`crates/terminal_rendering/src/glyph.rs`):
when the top glyph has ink and a transparent background but cannot be combined
with the below character, it filled the whole half's background with the below
glyph's *ink* color, discarding the below shape. For the player arrow over the
remapped death cube that flooded the left half with the death color.

Fix: `TextDrawable::drawn_over` now detects content drawables
(`OffsetSquareDrawable`, `PartialVisibilityDrawable`) and composites through
`Glyph::drawn_over_preserving_below_shape`, which keeps the below shape's
background where the top is transparent instead of flooding it. UI overlays
(danger/move markers) keep the old recolor-the-cell compositing, so
`test_protected_piece_has_fully_colored_background` still holds.

Regression test: `test_text_over_partial_shape_keeps_below_background`.
After the fix, `explain minimized 4 4` reports the player's left half as
arrow-on-floor (`bg(191,191,191)`) rather than death-flooded; the right half
keeps the death cube's actual lower-block coverage.

`snapshot/screen.txt` and `minimized/screen.txt` were re-blessed to the fixed
render; the `screen.pre-fix.txt` files are the originals (flooded left half).


