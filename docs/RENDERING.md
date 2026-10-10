# Rendering Pipeline

How a frame goes from game state to terminal escape codes. For the broader
crate layout see [ARCHITECTURE.md](ARCHITECTURE.md); for the coordinate
frames named below see [COORDINATE_FRAMES.md](COORDINATE_FRAMES.md).

## Overview

```
Game state ──► draw buffer (WorldSquare → DrawableEnum)
                 │
                 ▼  per screen square, through portal-aware FOV
           screen buffer (terminal cells → Glyph)
                 │
                 ▼  cell-by-cell diff against previous frame
           termion escape codes ──► terminal
```

One **world square** always renders as **two adjacent terminal characters**
(a `DoubleGlyph = [Glyph; 2]`), because terminal cells are ~twice as tall as
they are wide; two characters per square keeps world geometry visually square.

## Per-frame sequence

The main loop (`crates/game/src/lib.rs::do_everything`) drains input events,
ticks logic, then calls `Game::draw(writer, time)`
(`crates/game/src/game/mod.rs`). Draw has two phases:

### Phase 1 — populate the draw buffer

`Game::populate_draw_buffer` layers drawables into
`Graphics::draw_buffer: HashMap<WorldSquare, DrawableEnum>` in back-to-front
order:

1. Static floor (`SolidColorDrawable`, from the floor-color function —
   default is a 3×3-tile checkerboard, `Graphics::big_chess_pattern`)
2. Floor-wide animation if active (recoiling floor, radial shockwave)
3. Floor push arrows, conveyor belts (each belt's own speed drives its phase,
   offset by square parity)
4. Move/capture marker squares
5. Blocks, pieces (chess-character glyphs), upgrades
6. Floating entities — death cubes and hunter drones are positioned at
   `f32` world points and drawn with sub-square resolution:
   `OffsetSquareDrawable` for the body, braille lines for drone sight
7. Widgets
8. Non-floor animations (lasers, explosions, smites, selectors, …) after
   `remove_finished_animations(time)`
9. The player (an `ArrowDrawable` showing faced direction)

Every draw call funnels into `Graphics::draw_above_square`, which composes
with whatever is already at that square via `Drawable::drawn_over`. Layering
is therefore order-independent-ish: later draws land *on top* of earlier ones,
and transparency (`bg_transparent`) lets lower layers show through.

### Phase 2 — resolve FOV and flush to screen

`Game::update_screen_from_draw_buffer`:

1. Fill `Screen::screen_buffer` with black.
2. If the player is alive: center the screen on the player and call
   `Graphics::load_screen_buffer_from_fov(player_field_of_view())`.
   Otherwise fall back to
   `load_screen_buffer_from_absolute_positions_in_draw_buffer` (direct
   world→screen mapping, no visibility shading — used on the death screen).
3. `Graphics::draw_starfield` paints the off-grid void (see below).
4. `Graphics::clear_ui`, then draw UI content into the screen-space
   `UiLayer` (the FOV border among it), then `Graphics::composite_ui` blends
   it over the screen buffer (see [UI layer](#ui-layer-screen-space)).
5. `Graphics::draw_debug_overlays` (no-op unless enabled).
6. `Graphics::display(writer)` diffs and writes (see below).

Headless variants (`draw_headless_now`, `display_headless`) run the same
pipeline with `writer = None`; tests inspect the buffers instead of a
terminal.

## Off-grid starfield

The black beyond the grid's extent is filled by a procedural starfield
(`crates/game/src/graphics/starfield.rs`). It only writes cells whose world
square is off-grid, so it composes after the FOV pass without touching the
grid. There are three depth layers; each is anchored at a fraction
(`parallax`) of the **view frame's** motion, so near layers slide further than
far ones as the player moves, plus a slow linear drift so the field keeps moving
while standing still. Stars come from an infinite hashed lattice — moving or
drifting simply reveals new cells, so nothing is generated or recycled.

The camera is the root of the FOV frame a cell is actually seen through, not the
player's absolute square: the field is painted once per `view_frames()` entry
(root + rotation), each star gated to the frame its cell resolves to
(`resolved_visibility`). A portal frame's stars are anchored at that frame's root
and projected back to primary space through the frame's inverse rotation
(`frame_to_primary_offset`), so both the sky's *position* and the direction it
parallaxes turn with the frame. This keeps the sky continuous when the player
steps through a portal (the view is already portal-continuous); a
portal-free scene has a single frame rooted at the player, so the output is
byte-for-byte the pre-portal-aware render.

`Starfield::draw` is a pure function of `(screen, fov, extent, time)`: it
carries no per-frame state, which is what keeps repeated draws of the same
moment byte-identical (`test_headless_frames_are_byte_identical`).

## UI layer (screen space)

`terminal_rendering::ui_layer::UiLayer` is a character-resolution glyph grid
in the same frame as `Screen`'s buffer (origin top-left, y down), but with no
camera attached. It is composited over the world pass and under the debug
overlays, and it exists so terminal-space content does not turn with the view.

- Cells start as `Glyph::transparent_glyph()`; `composite_onto` skips
  transparent cells and `drawn_over`s the rest, so opaque UI overwrites and
  transparent UI leaves the world pass byte-for-byte untouched.
- Draw helpers (`draw_glyph`, `draw_double_glyph`, `draw_string`) take
  `ScreenBufferCharacterSquare` / `ScreenBufferSquare` and clip off-screen.
- `Graphics` owns the layer (`clear_ui` / `ui_layer` / `composite_ui`). The FOV
  border (`graphics/fov_border.rs`) is the first client: it used to be authored
  in player-relative world squares, so the decorated top edge swung round with
  the camera; it now paints into the UI layer, positioned by screen offset from
  the player's screen square.

## FOV-aware compositing (the portal part)

`FieldOfViewResult::drawable_at_relative_square`
(`crates/game/src/fov_stuff.rs`) answers "what does the player see at this
offset?" — possibly *several* things, because portals let the same relative
square show different absolute squares:

- All `PositionedSquareVisibilityInFov` entries for the relative square are
  collected and sorted back-to-front (`sorted_by_draw_order`).
- Each entry's drawable is fetched from the draw buffer by its **absolute**
  square, then `rotated()` by that view's portal rotation so geometry appears
  correct through the portal.
- A partially visible square is wrapped in a `PartialVisibilityDrawable`,
  which renders the shadow boundary as an **angled block character**
  (`half_plane_to_angled_block_character` in `angled_blocks.rs`) — a
  half-cell triangle approximating the visible half-plane.
- With `tint_portals` enabled, views through portals get a color tint so the
  player can tell which squares are seen through a portal.

The resulting `DrawableEnum` is rotated once more by the screen's own
rotation, converted `to_glyphs()`, and written straight into the screen
buffer.

On a raised grid, the forward voxel pass
(`Graphics::load_screen_buffer_from_terrain`) uses the same resolved
visibilities, per relative cell: each cell's topmost view
(`resolved_visibility`) names the absolute square it shows, and if that square
holds a voxel stack it is drawn there, with the portal rotation and tint applied
and the camera-facing wall direction rotated through the portal, so raised
geometry is re-projected through portals just like flat floor and entities.
Drawing per cell (not per absolute stack) keeps a stack in *every* view it is
visible through — a cube seen both directly and through a portal is raised at
both cells, instead of collapsing to the direct view and leaving the portal cell
with the flat checkerboard (issue 0013). Writes are clamped to the FOV frame
(`sight_radius + 1`).

A wall voxel is written up to `camera_altitude - z` rows below the stack's
occlusion-checked top cell, so that wall cell is checked separately: it must
resolve to the *same view frame* as the stack (`absolute_fov_center_square`)
before the wall is drawn, otherwise a directly visible cube's wall would paint
into cells a portal resolves to void or another region (issues 0010/0011). A
`ConveyorBelt` top face is composited over the material base rather than
background-recolored, so its own black background is preserved (issue 0012).

## The Drawable abstraction

`crates/game/src/graphics/drawable.rs` defines:

```rust
pub trait Drawable: Clone + Debug {
    fn rotated(&self, quarter_rotations_anticlockwise: i32) -> DrawableEnum;
    fn to_glyphs(&self) -> DoubleGlyph;
    fn drawn_over<T: Drawable>(&self, other: &T) -> DrawableEnum;
    fn color_if_backgroundified(&self) -> RGB8;
    fn to_enum(&self) -> DrawableEnum;
    fn tinted(&self, color: RGB8, strength: f32) -> DrawableEnum;
}
```

`DrawableEnum` (via `ambassador` delegation) covers: `TextDrawable`,
`SolidColorDrawable`, `PartialVisibilityDrawable`, `BrailleDrawable`,
`ArrowDrawable`, `ConveyorBeltDrawable`, `OffsetSquareDrawable`, and
`LayeredDrawable`.

`LayeredDrawable` holds two drawables (`under`, `over`) uncomposited. It exists
because `drawn_over` bakes the lower glyphs immediately: a *directional* lower
layer (a conveyor belt) then loses its orientation when the composite is rotated
for the view (issue 0014 — the belt half under the player). `LayeredDrawable`
rotates each layer before compositing, and `to_glyphs` still routes through the
same `drawn_over` logic, so its special cases (`PartialVisibility`,
`OffsetSquare`) are preserved. `Graphics::draw_above_square` wraps a conveyor
belt (or an existing stack) under new content in a `LayeredDrawable`.

Glyphs themselves are composable: `DoubleGlyphFunctions::drawn_over`
(`terminal_rendering/src/glyph.rs`) does the low-level over-blending,
respecting `bg_transparent` and fullwidth characters.

## Sub-character rendering

Effects finer than one terminal cell are encoded in the *choice of
character*, all in `crates/terminal_rendering/`:

| Module              | Resolution trick                                   | Used for                        |
|---------------------|----------------------------------------------------|---------------------------------|
| `braille.rs`        | 2×4 dot matrix per character                       | lasers, sight lines, smooth curves |
| `angled_blocks.rs`  | half-cell triangles at arbitrary angles            | FOV shadow edges                |
| `hextant_blocks.rs` | 2×3 block sextants                                 | filled sub-cell shapes          |
| `floating_square.rs`| snap-family block combos                           | sub-square-positioned entities  |

The floating-square path (snap families, the coherence invariant, the
coverage oracle, and its debug tooling) has its own document:
[FLOATING_BLOCKS.md](FLOATING_BLOCKS.md).

A `BrailleDrawable::line(start, end, color)` rasterizes a world-space line
into a `HashMap<WorldSquare, BrailleDrawable>`, so smooth lines compose with
the same per-square machinery as everything else.

## Animations

`graphics/animations.rs` defines the time-based `Animation` trait:

```rust
pub trait Animation: Clone {
    fn start_time(&self) -> LogicalTime;
    fn set_start_time(&mut self, time: LogicalTime);
    fn duration(&self) -> Duration;
    fn double_glyphs_at_time(&self, time: LogicalTime) -> HashMap<WorldSquare, DoubleGlyph>;
    // + default: double_glyphs_with_transparency_at_time, fraction_done_at_time, finished_at_time, ...
}
```

Animations are pure functions of logical time — same `LogicalTime` in, same
glyphs out — which makes them deterministic and testable
(`draw_headless_at_duration_from_start`; the injectable clock is
`game/src/logical_time.rs`, roadmap W.A). `AnimationEnum` delegates over
lasers (simple/floaty), explosions, smites, blink teleports, spear/circle
attacks, piece deaths, selectors, and the two **floor animations**
(`RecoilingFloorAnimation`, `RadialShockwave`) stored separately in
`Graphics::floor_animation` because they replace the floor layer rather than
drawing over it.

## Screen output and diffing

`Screen` (`terminal_rendering/src/screen.rs`) owns:

- `screen_buffer` — the glyph grid being built this frame
- `current_screen_state` — what the terminal is believed to show
- `screen_origin` + `rotation` — the world→screen transform (camera)

Coordinate frames are distinct euclid-typed grids, converted in
`screen.rs`: world square ↔ screen-buffer square (2 chars wide) ↔
screen-buffer character square (individual terminal cells), all under
arbitrary quarter-turn rotation. Screen steps are **y-down**, world steps
**y-up** (hence the `SCREEN_STEP_*` constants and `flip_y`).

`Screen::update_screen(writer)` walks every cell, and only where
`screen_buffer` differs from `current_screen_state` emits
`termion::cursor::Goto` + the glyph's escape string (24-bit fg/bg color
codes + character). `Graphics::display` then snapshots
`screen_buffer` into `current_screen_state`.

## The two glyph stacks (migration in progress)

There are two parallel representations:

- **`Glyph`** (`glyph.rs`) — non-optional colors + `bg_transparent` flag.
  Used by the whole game-side pipeline and `Screen`.
- **`DrawableGlyph` / `Frame`** (`drawable_glyph.rs`, `frame.rs`) —
  `Option` colors, and a `Frame` framebuffer with blitting, cursor-optimized
  diff rendering (`string_for_raw_display_over`), display-string
  parsing/serialization, and blessed-snapshot test support. Currently used by
  the `portal_playground` binary and test utilities, **not** by the main
  game loop.

`screen.rs` carries `// TODO: replace with Frame`; unifying these is part of
the long-term cleanup (see [ROADMAP.md](ROADMAP.md)).

## Testing hooks

- `Game::draw_headless_at_duration_from_start` — render an exact moment.
- `Graphics::print_draw_buffer` — dump the draw buffer as text.
- `Frame::parse_regular_display_string` + blessed files
  (`assert_frame_same_as_past_fn`, `BLESS_TESTS` env var) — snapshot testing
  of rendered output.
- `Frame`'s `Debug` impl prints side-by-side decompositions (full / bg /
  fg / characters) to make color-vs-character bugs obvious.
