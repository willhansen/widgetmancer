# Changelog

Narrative record of work done in the sandbox, where commits are ephemeral.
Each entry corresponds to one focused commit; the bodies below are the commit
messages that would otherwise live only in the (throwaway) sandbox git history.
Hashes are sandbox-local and may not exist outside the container.

Newest first.

---

## 2026-09 — Debug tooling wishlist (W.A–W.G)

Implemented the debug-tooling wishlist from the front of
[ROADMAP.md](ROADMAP.md), motivated by the portal-depth partial-visibility
artifact in `issues/black-block-deep-in-portal/`. All workspace tests green.

### game: minimize portals too, with --keep-portals opt-out (W.E)
The minimizer previously protected all portals, leaving 21 in the black-block
repro. It now removes portal entries by default; the strict anchor predicate
(same relative square + depth + absolute square) keeps only the artifact's own
chain, so the repro drops to **2 portals** (`[41,44]`/`[41,45]`, both
`dir[1,0] -> [37,44/45]`). `MinimizeOptions.keep_portals` /
`snapshot_tool minimize --keep-portals` restores the old behavior (21 portals).
Regenerated the issue debug outputs, added a `--keep-portals` variant, and
updated `debug/README.md`. Tests green (238 lib).

### docs: add post-minimization renders to the black-block debug outputs
The first pass rendered only the original snapshot; the minimized state existed
only embedded in the review transcripts. Materialized `debug/minimized/` and
`debug/minimized-nocrop/` as full snapshot dirs (`game_state.json` +
`screen.txt`) and added `render-minimized.txt`, `render-minimized-nocrop.txt`,
`cells-minimized.txt`, `explain-minimized.txt` (artifact moves to buffer
`(32,4)` in the 73×11 frame), and `diff-minimized.txt`. Verified the review
transcript's final frame equals the standalone minimized render.

### docs: capture all snapshot_tool debug outputs for the black-block issue
Generated `issues/black-block-deep-in-portal/debug/` from the issue snapshot:
`render`, `diff`, `cells`, `fov-trace` (+JSON), `explain 49 37`, `invariants`,
and the minimizer with each review variant (`--review`, `--review-plain`,
`--review-explain`, `--no-screen-crop`) plus the minimized JSONs. Added a
`debug/README.md` index with the regeneration commands.

### game: virtual-screen crop + review transcript for the minimizer (W.E)
The minimizer now anchors the artifact to the player (relative square + portal
depth + absolute square) instead of a fixed buffer cell, so it survives a
resize. New early step crops the virtual screen to the `{player, artifact}`
bounding box plus a margin (default 4 squares), changing only
`screen.terminal_width/height` (board and world coordinates untouched; rejected
if the artifact would leave the buffer). Entity removal then runs as before.
`snapshot_tool minimize` gains `--review[=<path>]` (screen-by-screen transcript:
frame, then a one-line change summary, then the next frame), `--review-explain`
(embed the full explain block per step), `--review-plain` (strip ANSI), and
`--crop-margin`/`--no-screen-crop`. On the black-block snapshot: 141×78 → 73×11,
then the three death cubes removed, artifact intact throughout. Tests:
crop geometry, anchor derivation error, review-step formatting; full game suite
green (238 lib tests).

### docs: drop export guidance from AGENTS.md
Removed the extra review-pause export note from `AGENTS.md` and the matching
changelog reference.

### docs: changelog/AGENTS cleanup
Final wording pass so the changelog no longer quotes the removed guidance.

### docs: add repo AGENTS.md changelog policy
Added `AGENTS.md` noting that sandbox commits are ephemeral and that every
commit's subject/body must be mirrored into `docs/CHANGELOG.md`.

### game: add minimal live debug overlay (W.G)
`Graphics.debug_overlay` (`screen_center`, `screen_origin`) draws magenta
markers after the FOV composite; `Game::set_debug_overlay` toggles it.
Test covers the center marker.
_Deferred:_ keybindings, depth heatmap, draw-order readout, mouse-hover →
`explain_screen_cell`.

### game: add snapshot minimizer (W.E)
`debug::minimize_snapshot` greedily removes entity entries while the
`OUT_OF_SIGHT`-tinted partial persists at a target cell, preserving portals
and player. `snapshot_tool minimize <dir> X Y [out]`. Reduces the black-block
snapshot to player + 21 portals with the artifact intact.
_Deferred:_ coordinate/board shrink and chunked delta-debugging.

### game: add FOV visibility-consistency invariant (W.D)
`fov_visibility_consistency_violations` groups visibilities by absolute square
across relative squares and flags a square that is fully visible via one portal
depth but partially visible via another — the black-block signature. Exposed via
`Game::fov_invariant_violations` and `snapshot_tool invariants`. Detects
`rel(15,1) abs(37,45)` on the issue snapshot.
_Deferred:_ the ray-cast differential reference oracle.

### game: add explain_screen_cell provenance query (W.B)
`Game::explain_screen_cell` reports the final glyph, the world/FOV-relative
square, and every FOV visibility (absolute square, portal depth, rotation,
absolute/relative mask, draw-buffer drawable) that could contribute to a screen
cell. `snapshot_tool` gains an `explain <dir> X Y` mode. On the black-block
snapshot it reproduces the issue's observed cell exactly (depth 3, abs(37,45),
final fg(165,89,89) bg(77,0,0)). Test added.

### game: add portal-recursion FOV trace (W.C)
Thread `depth` + an optional `FovTrace` through
`field_of_view_within_arc_in_single_octant` and expose traced entry points
(`single_octant_field_of_view_traced`,
`portal_aware_field_of_view_from_square_traced`,
`Game::player_field_of_view_traced`). Each node records the crossed portal
square, child incoming arc, parent-frame arc, transformed center, and rotation.
`snapshot_tool` gains `fov-trace` and `fov-trace-json` modes. 236 lib tests
green.

### game: inject LogicalTime + seeded serialized RNG (W.A)
Replace `std::time::Instant` with a serializable `LogicalTime(Duration)`
throughout `Game`/`Graphics`/animations, stamp spawned animations from the
frame's logical time, and confine wall-clock reads to the driver seam.
`Game::new` seeds the world clock from its `start_time`; snapshot load no longer
rebases to `Instant::now()`.

Gameplay randomness (turret fire, shotgun spread, random subordinate spawns)
now draws from a `ChaCha8Rng` on `Game` whose state is serialized in
`game_state.json` (`rng_state`), so loading resumes the exact stream.

Adds a test asserting no `Instant::now()` survives outside the driver seam.
game/utility/terminal_rendering suites green (235 lib tests).

### docs: record W.F landed and W.A partial in roadmap wishlist
Updated the roadmap wishlist statuses.

### game: seed world clock from start_time; stabilize StaticBoard time (W.A)
`Game::new` set `world_start_time`/`world_time` to independent `Instant::now()`
reads instead of the caller-provided anchor. `StaticBoard::start_time` returned
a fresh `Instant::now()` per call. Also drops the dead
`Graphics::time_since_start` and adds a headless-render determinism test.

### game: thread frame time into death-cube draw (W.A leak fix)
`draw_death_cube` used `Instant::now()` for technicolor, ignoring the frame time
threaded through `draw`/`populate_draw_buffer`. Headless `snapshot_tool` now
renders at the captured `world_time_seconds`; the only remaining diff against
the pre-fix checked-in snapshot is the ~2 ms wall-clock skew the old code baked
into the capture.

### game: add feature-gated headless snapshot_tool (W.F)
Added a default-off `debug-tools` feature, `pub mod game::snapshot::debug`
(descendant module so it reaches the private DTO/loader without widening the
crate API), and `crates/game/src/bin/snapshot_tool.rs` with
`render`/`diff`/`bless`/`cells` subcommands, an ANSI `screen_text` parser, and a
per-cell diff report. Manifest switched to explicit `[[bin]]` entries +
`autobins = false`; `snapshot_tool` has `required-features = ["debug-tools"]`.
Tests: parser round-trip against the screen buffer; cross-load render
determinism guard.

### docs: add debug tooling wishlist to roadmap front
Inserted the W.A–W.G wishlist section before `## Open` in
[ROADMAP.md](ROADMAP.md), grounded in the black-block issue.
