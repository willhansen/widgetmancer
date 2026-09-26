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
