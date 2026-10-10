# Widgetmancer — Architecture Overview

Widgetmancer is a turn-based terminal roguelike written in Rust, built around a
signature mechanic: **portals that alter line-of-sight and movement geometry**.
It renders entirely in the terminal using custom sub-character rendering
techniques (braille, half/quarter/hextant blocks) for smooth graphics.

Run with `cargo run --release`; test with `cargo nextest run`.

Known architectural issues and the plan to fix them are tracked in
[ROADMAP.md](ROADMAP.md). The frame-by-frame draw path is documented in
[RENDERING.md](RENDERING.md), the euclid-typed coordinate/reference
frames in [COORDINATE_FRAMES.md](COORDINATE_FRAMES.md), and the
sub-square floating-entity renderer in
[FLOATING_BLOCKS.md](FLOATING_BLOCKS.md).

## Workspace Layout

The project is a Cargo workspace (`resolver = "3"`) with three crates under `crates/`,
ordered from lowest-level to highest-level, plus a top-level
`floating-square-debug` bin for the sub-square renderer:

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

Line counts below are approximate source lines (tests included).

### 1. `utility` (~4.6k LOC)

Foundation math and geometry helpers, built on top of `euclid` typed 2D points/vectors.

- `lib.rs` (~2.7k LOC) — core type aliases (`IPoint`/`FPoint`/`IVector`/`FVector`, plus
  game-domain aliases like `WorldPoint`/`WorldStep`), orthogonal/diagonal step
  constants, line-of-sight and grid helpers, trait extensions.
- `angle_interval.rs` (~1.1k LOC) — circular angle intervals: containment,
  intersection, union; heavily used by the FOV system.
- `geometry2.rs` (~0.7k LOC) — extension traits over euclid types (`IPointExt`,
  `FPointExt`, `IRectExt`): rotations, king moves, quadrant handling, etc.
- `coordinate_frame_conversions.rs` — conversions between coordinate frames
  (world ↔ local/square-relative), essential for portal-transformed geometry.

### 2. `terminal_rendering` (~9.6k LOC)

A terminal "graphics engine": a double-glyph-per-character framebuffer with
sub-cell resolution.

- `glyph.rs` (~1.1k LOC) — the core `Glyph` type: a terminal cell with a
  character, foreground/background color, and combinators (over/under blending).
- `glyph_with_transparency.rs`, `drawable_glyph.rs` — alpha-aware glyphs and
  positioned glyph batches.
- `frame.rs` — a framebuffer of glyphs with a `Drawable` trait for compositing.
- `screen.rs` — screen buffer management, diffing, and output via `termion`.
- `ui_layer.rs` — the screen-space `UiLayer` (UI drawn in terminal space, so it
  does not rotate with the camera).
- `coverage.rs` (~2.1k LOC) — the sub-square coverage oracle used by the
  floating-square tests and debug tool.
- `family_map.rs` + `family_map_table.rs` — baked snap-family selection for the
  floating-square renderer.
- Sub-character renderers for high-resolution effects:
  - `braille.rs` — 2×4 dot braille rendering
  - `hextant_blocks.rs` — 2×3 block rendering
  - `angled_blocks.rs` — half-block triangles for angled lines
  - `floating_square.rs` — sub-cell positioned solid squares
- `glyph_constants.rs` — named characters and a named-color palette;
  `emoji_presentation.rs` — emoji-variation handling.

### 3. `game` (~22.8k LOC)

The actual game. Modules:

- `lib.rs` — entry point (`do_everything()`). Sets up the terminal (raw mode,
  alternate screen, mouse), a panic hook that restores the main screen, and a
  dedicated input thread that streams timestamped `termion` events over an
  mpsc channel. Runs the main loop.
- `main.rs` — thin binary calling `game::do_everything()`. Further binaries:
  `bin/portal_playground.rs` (portal-rendering experiments),
  `bin/map_diagram.rs` (ASCII map/height dumps), and the `debug-tools`-gated
  `bin/snapshot_tool.rs` (headless render/diff/explain) and
  `bin/glyph_vocabulary.rs`.
- `game/mod.rs` (~1.8k LOC) — the `Game` state, core accessors, map
  construction, grid geometry, and rendering glue. The rules engine was split
  into the modules below (roadmap item 1).
- `game/floor_features.rs`, `game/turns.rs`, `game/combat.rs`, `game/ai.rs`,
  `game/spawning.rs`, `game/floating_entities.rs`, `game/realtime.rs` —
  non-solid floor features (conveyors, upgrades), turn handling, piece
  placement/movement/combat, enemy AI, spawning, floating entities
  (`DeathCube`, `FloatingHunterDrone` unified via a `FloatingEntityTrait`
  delegated with `ambassador`), and the realtime/tick effects.
- `game/voxel_grid.rs` — the voxel grid: solid voxels, their horizontal
  `extent`, per-square surface heights, and materials. The single source of
  truth for geometry (safe floor, walls/stacks, void).
- `game/map_file.rs` — JSON map recipes (`maps/<name>.json`).
- `game/map_diagram.rs` — ASCII map/height rendering used by the
  `map_diagram` bin.
- `game/snapshot.rs` — game-state serialization/loading and the `debug-tools`
  snapshot helpers (render/diff/explain/minimize).
- `game/tests.rs` — the game-logic test suite.
- `piece.rs` — pieces on the grid: player, pawns, other enemies; `PieceType`
  and an `Upgrade` system.
- `logical_time.rs` — the injectable `LogicalTime(Duration)` clock (roadmap W.A)
  used by the sim and render paths instead of `std::time::Instant`.
- `fov_stuff.rs` (~3.2k LOC) — **portal-aware field of view**, the technical
  heart of the project. Produces `FieldOfViewResult` with per-square
  `SquareVisibility` (including partial visibility), casting sight through
  portals using angle intervals from `utility`.
- `portal_geometry.rs` — portal placement/orientation and the transforms
  mapping squares/rays across portal pairs.
- `graphics.rs` (~1.3k LOC) — bridges game state to `terminal_rendering`:
  builds drawables for the floor, pieces, FOV shading, HUD, and animations.
- `graphics/drawable.rs` — game-side drawable implementations
  (`ArrowDrawable`, `BrailleDrawable`, `ConveyorBeltDrawable`,
  `PartialVisibilityDrawable`, `LayeredDrawable`, `TextDrawable`, …) behind a
  `DrawableEnum`.
- `graphics/starfield.rs` — the off-grid procedural starfield.
- `graphics/fov_border.rs` — the screen-space FOV border (a `UiLayer` client).
- `graphics/animations.rs` + `graphics/animations/*` — time-based animation
  system: lasers (simple/floaty), explosions, blinking, radial shockwaves,
  smites, spear/circle attacks, falling boxes, death animations, selector, and
  a recoiling floor.
- `inputmap.rs` — maps `termion` key/mouse events to game commands.
- `utils_for_tests.rs` — test helpers (grid setup, assertions).

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
  drawables composited into the `Screen`'s glyph buffer, which `Screen` diffs
  against the previous frame and writes to the terminal, minimizing
  escape-sequence output.
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
| `rand_chacha`  | seeded, serializable gameplay RNG (roadmap W.A)  |
| `serde`, `serde_json` | map recipes and snapshot serialization    |
| `libc`         | raw-fd input polling (`poll`/`read`)             |
| `itertools`    | collection helpers in the rules engine           |
| `priority-queue` | pathfinding/AI                                 |
| `rgb`, `color-hex` | color handling                               |

## Testing & Tooling

- Unit tests live alongside source (snapshot data in
  `crates/terminal_rendering/test_data/`); `tests/integration_tests.rs` covers
  end-to-end behavior. Recommended runner: `cargo nextest run`.
- `.config/nextest.toml` sets a default per-test timeout
  (`slow-timeout = { period = "60s", terminate-after = 2 }`): a test is marked
  slow after 60s and terminated after 120s, so a runaway test fails fast
  instead of hanging the suite. Override per test with
  `[[profile.default.overrides]]` if something is legitimately slower.
- `game` has a default-off `debug-tools` feature gating the headless
  `snapshot_tool` / `glyph_vocabulary` bins and `game/snapshot.rs`'s
  introspection helpers. Run it through the repo-root `./snapshot-tool` wrapper
  (which does `cargo run -p game --features debug-tools --bin snapshot_tool --`)
  so the binary always matches the sources; `snapshot_tool` can `diff`/`bless`
  captures, `explain` a cell, dump `fov-trace`, `heights`, `invariants`, and
  `minimize` a repro.
- Live bug captures are the numbered `issues/NNNN/` directories (solved ones
  under `issues/solved/`), each a snapshot plus a note; the `verify-issues`
  subcommand checks every solved capture against its blessed `screen.txt` and
  pre-fix render.
- `bacon.toml` — bacon watch config (incl. a `clippy` job); `flake.nix` — Nix
  dev shell; `scripts/` — test recording/printing and profiling helpers.

### Profiling (what actually runs in this sandbox)

`flake.nix` lists more profilers than resolve here, so don't trust it as an
inventory. `.cargo/config.toml` keeps frame pointers on, but
`-Zinstrument-mcount` is **opt-in** via `scripts/profile.sh`: leaving it in
`[build].rustflags` instruments every function of every target (tests
included) and roughly quadruples the test suite. Run any command through the
script when you need `gmon.out` call counts or uftrace's mcount hooks, e.g.
`scripts/profile.sh cargo run --release -p game --example profile_racetrack`.
Note `RUSTFLAGS` replaces the config flags, so the script repeats the frame
pointer flag.

| Tool | Status | Use |
|------|--------|-----|
| `uftrace` 0.19 | works | function tracing / `report` / `graph` / `--flame-graph`; run against `target/debug/*` (verified on `map_diagram racetrack`) |
| `gprof` | works | sampling profile from a run's `gmon.out`; no `perf` needed |
| `cargo-nextest` | works | test runner (README) |
| `cargo-flamegraph` | broken | installed, but its Linux backend is `perf`, which is absent |
| `cargo-profiler` | broken | wraps valgrind callgrind/cachegrind; valgrind absent |

No `perf`, `valgrind`, `gdb`, `lldb`, or `rr`. `flamegraph.svg` at the repo
root is stale (pre-sandbox). The live game loop (`lib.rs`) is a TTY
alternate-screen loop, so profile headlessly instead: the `map_diagram` bin,
`Game::draw_headless_now`, or a frame-loop test such as
`test_racetrack_map_cubes_survive_frame_rate_ticks`.

`crates/game/examples/profile_racetrack.rs` is a ready-made headless harness
(`-- <map> <frames>` or `-- fov <map> <iters>`). Findings from it are in
[PERFORMANCE.md](PERFORMANCE.md).
