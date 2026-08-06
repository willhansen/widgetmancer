# Coordinate Frames & Reference Frames

All positions in the codebase are **euclid-typed** (`Point2D<T, Unit>`,
`Vector2D<T, Unit>`, …) where the `Unit` parameter is a zero-sized marker
struct naming the *reference frame* the value lives in. Mixing frames is a
compile error; conversions are explicit functions. This file is the map of
those frames.

See [RENDERING.md](RENDERING.md) for how these frames are used during a
frame render.

## Naming conventions

Within one frame, four type families recur:

| Suffix   | euclid type      | Meaning                          |
|----------|------------------|----------------------------------|
| `Square` | `Point2D<i32>`   | integer grid cell                |
| `Point`  | `Point2D<f32>`   | continuous position              |
| `Step`   | `Vector2D<i32>`  | integer offset between squares   |
| `Move`   | `Vector2D<f32>`  | continuous offset                |

Example: `WorldSquare`, `WorldPoint`, `WorldStep`, `WorldMove` all live in
`SquareGridInWorldFrame`.

## Frame inventory

### 1. `SquareGridInWorldFrame` — the world

Defined in `crates/utility/src/coordinate_frame_conversions.rs`. The one
"real" frame: board squares, piece positions, FOV, portals, animations.

- `WorldSquare` / `WorldPoint` / `WorldStep` / `WorldMove`,
  `WorldSquareRect`, `BoardSize`
- **y-axis points up** (`STEP_UP = vec2(0, 1)`), matching math convention.
- A square's center is at its integer coordinates, and its extent is ±0.5;
  `world_point_to_world_square` is therefore `round()`, not `floor()`.
- Step constants (`STEP_RIGHT`, `STEP_UP_LEFT`, …) and their `KingWorldStep`
  / `OrthogonalWorldStep` wrappers live in `utility/src/lib.rs`.

### 2. `SquareGridInLocalSquareFrame` — square-relative

Same module. `LocalSquare` / `LocalSquarePoint`: coordinates relative to a
chosen reference `WorldSquare`, used whenever geometry is easier to express
"within a square":

- FOV shadows: `SquareVisibility` stores its visible portion as a
  `LocalSquareHalfPlane = HalfPlane<f32, SquareGridInLocalSquareFrame>`
  (`crates/game/src/fov_stuff.rs`) — "which half of *this* square is lit".
- Conversions: `world_point_to_local_square_point` /
  `local_square_point_to_world_point`, plus
  `world_half_plane_to_local_square_half_plane`.

### 3. `CharacterGridInScreenBufferFrame` — terminal cells

`crates/terminal_rendering/src/screen.rs`. `ScreenBufferCharacterSquare` /
`ScreenBufferCharacterStep`: one unit = one terminal character cell in the
`Screen`'s buffers.

- **y-axis points down** (`SCREEN_STEP_UP = vec2(0, -1)`), matching terminal
  row order — the flip from world space happens in
  `Screen::world_step_to_screen_step` (`flip_y` + rotation).
- Raw indices into `Screen::screen_buffer[x][y]`.

### 4. `SquareGridInScreenBufferFrame` — on-screen squares

Same module. `ScreenBufferSquare` / `ScreenBufferStep`: the two-cell-wide
"square" units of the screen buffer. Because terminal cells are ~half as
wide as tall, one world square maps to **two horizontally adjacent
character cells** (`DoubleGlyph`); this frame is the screen-side unit in
which that pairing is one element.

- Character ↔ square conversions are pure scale:
  `point2(x / 2, y)` and `point2(x * 2, y)`, plus
  `..._to_both_screen_buffer_character_squares` for the pair.
- World ↔ screen-buffer-square goes through the camera: subtract the screen
  center (in the source frame), apply inverse screen rotation + `flip_y`,
  re-anchor at the center in the target frame
  (`world_square_to_screen_buffer_square` and inverse).

### 5. `CharacterGridInWorldFrame` — **deprecated**

World-space character grid (`WorldCharacterSquare`, `WorldCharacterPoint`,
…): a fixed "2 characters per world square, x' = 2x + 0.5" world-space
character frame. **Obsolete since screen rotation** — with a rotated screen
there is no rotation-invariant world character grid, so world→character
mapping must go through the screen (frames 3–4) instead. The aliases and
conversion fns remain (deprecated) while call sites are migrated; this is
the bulk of roadmap item 2's warning cleanup. Character-level maps
(`WorldCharacterSquareGlyphMap`) still appear in the `Animation` trait
interface for the same legacy reason.

### 6. `CharacterGridInLocalCharacterFrame` — half-square-local

`LocalCharacterSquare` / `LocalCharacterPoint`: character-cell coordinates
relative to one character half of one square. Used to split a square's
visibility shadow into left/right character shadows
(`fov_stuff.rs::CharacterShadow`) so `PartialVisibilityDrawable` can pick an
angled-block character per half-cell. Conversions:
`local_square_point_to_local_character_point`,
`local_square_half_plane_to_local_character_half_plane`,
`world_half_plane_to_local_character_half_plane`.

## The conversion chain

```
WorldPoint ──round()──► WorldSquare ─┐
   (y-up, float)      (y-up, int)    │
                                     ▼ camera: −center, rotate⁻¹, flip y
        ScreenBufferCharacterSquare ◄──×2── ScreenBufferSquare
              (y-down, 1 unit = 1 cell)     (y-down, 1 unit = 2 cells)
```

Side views into a square (FOV/shadow work) instead route through the local
frames: world → local-square → local-character.

## Relative vs. absolute in FOV

Portals make "where a square appears" differ from "where it is".
`PositionedSquareVisibilityInFov` (`fov_stuff.rs`) therefore carries both
frames plus the transform between them:

- `relative_square: WorldStep` — offset from the viewer where the square
  *appears* (what the render loop iterates over)
- `absolute_square: WorldSquare` — where the square *is* (what the draw
  buffer is keyed by)
- `quarter_turns_ccw_from_relative_to_absolute: QuarterTurnsAnticlockwise`
  — accumulated portal rotation; drawables are `rotated()` by its inverse
  before display so contents look correct through the portal
- visibility stored in both frames
  (`square_visibility_in_absolute_frame` / `..._relative_frame`)

`QuarterTurnsAnticlockwise` itself is defined in `utility/src/lib.rs` and
also drives `Screen::rotation` (camera rotation).

## Gotchas

- **Two `IPoint`/`FPoint` families.** `utility/src/lib.rs` aliases them to
  euclid `default::Point2D` (unit-less); `utility/src/geometry2.rs` aliases
  them to plain `[i32; 2]` / `[f32; 2]` arrays. `terminal_rendering`
  deliberately shadows the former with the latter (NOTE in its `lib.rs`);
  untangling is roadmap item 4. Prefer the frame-typed aliases
  (`WorldSquare`, …) in new code.
- **`cast_unit()` is a blind reinterpret.** Several conversions (e.g.
  screen↔world steps after rotate+flip) end in `.cast_unit()` — correct
  only because the numeric transform was done explicitly first.
- **y-axis flips at the screen boundary only.** World and local frames are
  y-up; both screen-buffer frames are y-down. If a sign bug appears only
  when the camera rotates, suspect a missing `flip_y` or a deprecated
  `WorldCharacter*` conversion bypassing the screen transform.
