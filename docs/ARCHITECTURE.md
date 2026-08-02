# Widgetmancer — Architecture Overview

Widgetmancer is a turn-based terminal roguelike written in Rust, built around a
signature mechanic: **portals that alter line-of-sight and movement geometry**.
It renders entirely in the terminal using custom sub-character rendering
techniques (braille, half/quarter/hextant blocks) for smooth graphics.

Run with `cargo run --release`; test with `cargo nextest run`.

Known architectural issues and the plan to fix them are tracked in
[ROADMAP.md](ROADMAP.md).

## Workspace Layout

The project is a Cargo workspace (`resolver = "3"`) with three crates under `crates/`,
ordered from lowest-level to highest-level:

```
┌────────────────────────────────────────────────────────┐
│ game          – game logic, FOV, portals, rendering    │
│                 orchestration, input, animations       │
├────────────────────────────────────────────────────────┤
│ terminal_rendering – glyph/framebuffer abstraction     │
│                 over the terminal (termion)            │
├────────────────────────────────────────────────────────┤
│ utility       – geometry, coordinates, angles, math    │
└────────────────────────────────────────────────────────┘
```

Dependencies flow strictly downward: `game` → `terminal_rendering` → `utility`.

### 1. `utility` (~4.5k LOC)

Foundation math and geometry helpers, built on top of `euclid` typed 2D points/vectors.

- `lib.rs` — core type aliases (`IPoint`/`FPoint`/`IVector`/`FVector`, plus
  game-domain aliases like `WorldPoint`/`WorldStep`), orthogonal/diagonal step
  constants, line-of-sight and grid helpers, trait extensions.
- `geometry2.rs` — extension traits over euclid types (`IPointExt`, `FPointExt`,
  `IRectExt`): rotations, king moves, quadrant handling, etc.
- `angle_interval.rs` (~1.1k LOC) — circular angle intervals: containment,
  intersection, union; heavily used by the FOV system.
- `coordinate_frame_conversions.rs` — conversions between coordinate frames
  (world ↔ local/square-relative), essential for portal-transformed geometry.

### 2. `terminal_rendering` (~5.2k LOC)

A terminal "graphics engine": a double-glyph-per-character framebuffer with
sub-cell resolution.

- `glyph.rs` (~1.1k LOC) — the core `Glyph` type: a terminal cell with a
  character, foreground/background color, and combinators (over/under blending).
- `glyph_with_transparency.rs`, `drawable_glyph.rs` — alpha-aware glyphs and
  positioned glyph batches.
- `frame.rs` — a framebuffer of glyphs with a `Drawable` trait for compositing.
- `screen.rs` — screen buffer management, diffing, and output via `termion`.
- Sub-character renderers for high-resolution effects:
  - `braille.rs` — 2×4 dot braille rendering
  - `hextant_blocks.rs` — 2×3 block rendering
  - `angled_blocks.rs` — half-block triangles for angled lines
  - `floating_square.rs` — sub-cell positioned solid squares
- `glyph_constants.rs` — named characters and a named-color palette.

### 3. `game` (~10.5k LOC)

The actual game. Modules:

- `lib.rs` — entry point (`do_everything()`). Sets up the terminal (raw mode,
  alternate screen, mouse), a panic hook that restores the main screen, and a
  dedicated input thread that streams timestamped `termion` events over an
  mpsc channel. Runs the main loop.
- `main.rs` — thin binary calling `game::do_everything()`. A second binary,
  `bin/portal_playground.rs`, exists for experimenting with portal rendering.
- `game.rs` (~4.9k LOC) — the `Game` state and rules engine: board, turn
  handling, piece placement/movement/combat, block types (walls, conveyors,
  upgrades), enemy AI, and **floating entities** (`DeathCube`,
  `FloatingHunterDrone`) unified via a `FloatingEntityTrait` delegated with
  `ambassador`.
- `piece.rs` — pieces on the board: player, pawns, other enemies; `PieceType`
  and an `Upgrade` system.
- `fov_stuff.rs` (~2.5k LOC) — **portal-aware field of view**, the technical
  heart of the project. Produces `FieldOfViewResult` with per-square
  `SquareVisibility` (including partial visibility), casting sight through
  portals using angle intervals from `utility`.
- `portal_geometry.rs` — portal placement/orientation and the transforms
  mapping squares/rays across portal pairs.
- `graphics.rs` (~0.8k LOC) — bridges game state to `terminal_rendering`:
  builds drawables for the board, pieces, FOV shading, HUD, and animations.
- `graphics/drawable.rs` — game-side drawable implementations
  (`ArrowDrawable`, `BrailleDrawable`, `ConveyorBeltDrawable`,
  `PartialVisibilityDrawable`, `TextDrawable`, …) behind a `DrawableEnum`.
- `graphics/animations.rs` + `graphics/animations/*` — time-based animation
  system: lasers (simple/floaty), explosions, blinking, radial shockwaves,
  smites, spear/circle attacks, death animations, selector, and a recoiling
  board.
- `inputmap.rs` — maps `termion` key/mouse events to game commands.
- `utils_for_tests.rs` — test helpers (board setup, assertions).

## Runtime Model

```
 stdin thread ──(Instant, Event)──▶ main loop (lib.rs)
                                     │
                                     ├─ InputMap → player commands
                                     ├─ Game::... → rules, AI, portals, FOV
                                     └─ Graphics → Frame (glyphs) → Screen diff → termion
```

- **Input** is asynchronous: a spawned thread forwards timestamped events over a
  channel so the loop can animate at a fixed cadence regardless of input.
- **Rendering** is pull-based each tick: `Graphics` converts game state into
  drawables composited into a `Frame`, which the `Screen` writes to the
  terminal, minimizing escape-sequence output.
- **Panic safety**: a custom hook exits the alternate screen and prints the
  panic info so crashes don't corrupt the terminal.

## Key Dependencies

| Crate          | Role                                             |
|----------------|--------------------------------------------------|
| `euclid`       | typed 2D geometry (points/vectors with units)    |
| `termion`      | raw terminal I/O, alternate screen, input events |
| `ambassador`   | trait delegation for the floating-entity enum    |
| `derive_more`, `getset`, `shrinkwraprs` | boilerplate reduction     |
| `ordered-float`, `num`, `approx` | numeric helpers                  |
| `line_drawing` | supercover/Bresenham lines for grid ray casting  |
| `rand`         | spawning and procedural behavior                 |
| `priority-queue` | pathfinding/AI                                 |
| `rgb`, `color-hex` | color handling                               |

## Testing & Tooling

- Unit tests live alongside source (snapshot data in
  `crates/terminal_rendering/test_data/`); `tests/integration_tests.rs` covers
  end-to-end behavior. Recommended runner: `cargo nextest run`.
- `bacon.toml` — bacon watch config; `flake.nix` — Nix dev shell;
  `scripts/` — test recording/printing helpers; `flamegraph.svg` — a captured
  performance profile.
