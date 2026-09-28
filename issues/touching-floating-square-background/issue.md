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

