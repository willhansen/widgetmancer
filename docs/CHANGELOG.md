# Changelog

Narrative record of work done in the sandbox, where commits are ephemeral.
Each entry corresponds to one focused commit; the bodies below are the commit
messages that would otherwise live only in the (throwaway) sandbox git history.
Hashes are sandbox-local and may not exist outside the container.

Newest first.

---

## 2026-09-28 — Remove the cumulative budget; the sight radius is the one dial

### fov: delete FovOptions/cumulative_radius_budget; add a sight-radius override

The budget was meant to be the radius, and after the depth-bound and artifact
corrections it had become a second radius plus a per-hop crossing gate. Its
default (`PLAYER_SIGHT_RADIUS`) was a no-op, and its only remaining effect was to
shrink the view while running an artifact-prone gate. Removed:

- `FovOptions`, `cumulative_radius_budget`, the `remaining_radius`/`spent`
  accumulator and the gate; `max_extent` is now always `radius`.
- The `_with_options` FOV entry points and `Game::set_fov_cumulative_radius`.
- `--fov-budget` / `--no-fov-budget` (`FovToggles::cumulative_radius_budget`).

Replaced by a real radius override: `Game::set_player_sight_radius` /
`player_sight_radius` (clears the FOV cache), `--fov-radius <n>` on the binary,
and `--radius=<n>` on the profiling harness. Default behavior is unchanged,
because the old default was already the no-op case. Racetrack FOV with the dial:
0.60 / 2.3 / 8.6 ms at R=4 / 8 / 16.

Tests: dropped the budget-specific regressions
(`test_cumulative_budget_keeps_a_distant_portal_window_open`,
`test_budget_does_not_black_out_the_outer_fov_through_a_portal`,
`test_budget_at_sight_radius_does_not_black_out_a_portal_hallway`,
`test_fov_cumulative_budget_is_a_fidelity_dial`), kept
`test_portal_recursion_depth_stays_near_the_sight_radius` on the plain API, and
added `test_fov_sight_radius_override_changes_the_view_and_clears_the_cache`.
Full suite green (551 passed / 9 skipped; 290 with debug-tools). PERFORMANCE.md
records the removal.

---

## 2026-09-28 — Portal recursion depth is bounded; cost is exponential in the radius

### docs: correct the FOV recursion depth/work framing in PERFORMANCE.md

Measured the recursion depth directly with `FovTrace` and found the earlier
"unbounded / only arc narrowing terminates" framing was wrong. Along a path the
apparent image strictly advances (a deeper frame inherits the parent's
`starting_step_in_fov_sequence` and starts past the near field) and each frame
scans only `radius` squares, so **depth ≤ radius + 1**; a portal on the player's
own square supplies the extra "zeroth" hop. At radius 5: three north portals to
one common exit → depth 1, a portal north → 5, a portal on the player's own
square → 6.

So the frame-time driver is the number of **paths**, not depth. Racetrack FOV
scales ~4.5x per +4 radius (0.42/1.8/8.2 ms at 4/8/16), i.e. exponential in the
radius. Two consequences recorded in `docs/PERFORMANCE.md`:

- Depth caps are not a useful perf lever; path/node count is.
- The relative-radius budget is a *work* cap, and because the gate only fires
  when `budget < radius`, the default `budget == radius` is a no-op. Dominance
  pruning (approach C) is now framed as the preferred cap-free fix: it attacks
  the exponential path count with no fidelity loss.

New regression `test_portal_recursion_depth_stays_near_the_sight_radius` pins
the measured depths. Full suite green (554 passed / 9 skipped).

---

## 2026-09-28 — Budget must not prune inside the sight radius

### fov: only gate portal recursion when the budget is below the sight radius

Follow-up to the two FOV fixes above, from the live `snapshot/` infinite portal
hallway (player (18,20)). The cumulative accumulator is a per-hop *path* cost,
not apparent distance: through the hallway's parallel portals it grows about
twice as fast as the apparent image. Gating crossings at `budget == radius`
therefore exhausted the 16-square budget after ~half the apparent distance —
rel 7..16 rendered black while rel 4..6 and the adjacent column stayed visible.

The gate is now active only when the budget is tighter than the sight radius
(`budget < radius`). At or above the radius the sub-view is legacy, because
`max_extent = min(radius, budget)` already caps apparent reach; the default
budget (16) therefore no longer changes rendering at all. The budget stays a
fidelity/perf dial for smaller values (8 → 1.8 ms/FOV, 4 → 0.42 ms, vs 8.3
unbudgeted on racetrack).

`test_fov_cumulative_budget_is_a_fidelity_dial` updated: budget ≥ radius equals
unbounded; 8 and 4 differ. New regression
`test_budget_at_sight_radius_does_not_black_out_a_portal_hallway` builds the
snapshot's portal window (the live `snapshot/` is gitignored) and asserts the
budgeted view matches legacy at every apparent distance inside the radius; it
fails on the old gate at rel(0,7). Full suite green (553 passed / 9 skipped).

---

## 2026-09-28 — Fix through-portal FOV clipping and clip the starfield to the FOV

### fov: keep the through-portal window open; clip starfield to FOV

Two related "player field of view" defects, both reproducible from the planted
issue snapshots.

**Through-portal sight.** `issues/fov-budget-optimization-cant-see-through-portal/`
(snapshot_2: player at (61,42), portal directly north at (61,50)/(61,51)) showed
a portal you can enter but not see through. The cumulative-radius budget
computed each sub-view's extent as `remaining_radius - spent`, but a portal
sub-view is centered on the viewer's *virtual* image (the transformed player),
so the exit lands at child-relative radius `spent` and child-relative distance
already equals cumulative apparent distance. Subtracting `spent` again clipped
the window to exactly the portal plane; for a portal more than half the sight
radius away nothing remained.

The recursion now gives every level the same apparent extent,
`min(radius, budget)`, while `remaining_radius` only gates further crossings.
Without a budget the extent is the legacy full `radius`. `explain` on
snapshot_2 rel(0,9) now reports the depth-1 image of abs(82,53) instead of a
black cell; regression `test_cumulative_budget_keeps_a_distant_portal_window_open`
fails on the old metric and passes on the new one.

**Debug tooling.** `player_field_of_view_traced` (used by
`fov-trace`/`explain`/`invariants`) ignored the game's `FovOptions`, so it
described the unbudgeted view rather than the rendered one. It now passes
`self.fov_options`; this is what made the clipping visible in `explain`.

**Starfield.** `Starfield::draw` only skipped on-board squares, so stars painted
the off-board void regardless of the FOV and overwrote portal-view floor whose
screen cell maps to an off-board world square
(`starfield-visibility.md`, `starfield-on-top-of-portal-view/`). It now also
skips any cell whose player-relative square is in the FOV (via an `Option<&fov>`,
`None` for the dead player who renders the whole board).

Perf: budget 4/8/16 → 0.43/1.8/7.5 ms/FOV on racetrack, versus 8.1 ms
unbudgeted. The previous 24 ms at budget 16 came from the over-shrinking bug;
the corrected metric keeps a full window through each portal, so the default 16
is a milder ~7% saving. Measurements and narrative updated in
`docs/PERFORMANCE.md`.

Tests: full suite green (552 passed / 9 skipped); added
`test_cumulative_budget_keeps_a_distant_portal_window_open`,
`test_budget_does_not_black_out_the_outer_fov_through_a_portal`, and
`no_stars_inside_the_field_of_view`.

---

## 2026-09-28 — Don't black out the outer FOV through a portal corridor

### fov: sub-view extent is the budget, not the remaining budget

Follow-up to the through-portal fix above. The first version made each
sub-view's apparent extent the *remaining* budget (one hop behind), which
shrinks with depth. In a portal corridor that leaves a black band *inside* the
sight radius: the direct view is blocked by the portal wall and only the
sub-view can fill the outer apparent squares, but the sub-view stops short of
them.

Repro from the live `snapshot/` (player (69,47) facing into a hall of portals,
rotation makes world-east screen-up): rel 13–16 of the sight radius render black
at budget 16, while rel 17 (outside the radius) is correctly black. The
sub-view's extent is now the shared `min(radius, budget)` at every level; the
budget still bounds apparent reach and gates deeper crossings via
`remaining_radius`. `explain` now fills rel 1–16 and goes black at 17.

Regression: `test_budget_does_not_black_out_the_outer_fov_through_a_portal`
fails on the shrinking extent and passes on the constant one.

---

## 2026-09-27 — Stop paying for gprof instrumentation on every test run

### test: make gprof instrumentation opt-in; trim charwise sampling

The test suite was ~30s (the issue reported 40s). Root cause: `.cargo/config.toml`
put `-Zinstrument-mcount` (gprof call-count instrumentation) in `[build].rustflags`,
so every target — all test binaries included — carried an `mcount` call at every
function entry. Measured by varying only `RUSTFLAGS`: full suite **30.1s → 7.3s**.

- `.cargo/config.toml` now keeps only `-Cforce-frame-pointers=yes` by default.
  The mcount flags moved to `scripts/profile.sh`, which sets `RUSTFLAGS`
  (replacing, not appending to, the config flags — so it repeats the frame
  pointer flag) and runs whatever command you give it. `RUSTFLAGS` is also what
  previously made this invisible: `cargo nextest` inherited the instrumentation.
- Remaining hotspot was `terminal_rendering::charwise_rendering
  test_approach_comparison_metrics` (5.8s, 75% of the un-instrumented suite):
  a characterization/print test whose assertions are explicitly loose. Its
  offset grid went 16x16 → 8x8 at odd 1/16 steps (avoiding eighth-block-aligned
  offsets, which zeroed the family-snapped area error). Bounds still pass with
  margin, `displacement_sensitivity` still probes the omitted midpoints.
- Full suite: **30.1s → 5.2s**; no single test over ~2s. nextest's `slow-timeout`
  comment updated to match.

Docs: ARCHITECTURE.md "Profiling" and PERFORMANCE.md now say to run through
`scripts/profile.sh`; resolved and removed `issues/tests-take-too-long.md`.

---

## 2026-09-27 — Default the FOV cache and cumulative-radius budget on

### perf: default FOV cache and cumulative-radius budget on
The two racetrack FOV prototypes (docs/PERFORMANCE.md) are now the default for
`Game` instead of opt-in flags:

- `Game::new` sets `fov_cache_enabled: true` and initializes
  `FovOptions::cumulative_radius_budget` to `Some(PLAYER_SIGHT_RADIUS as f32)`
  (16), so the cumulative relative-radius budget equals the player's view
  radius. `PLAYER_SIGHT_RADIUS` is now `pub`.
- Cache invalidation: the cache key is only the player square, so `place_block`
  and the four public portal-placement methods now clear `fov_cache`. Previously
  they were assumed immutable after setup; defaulting the cache on makes a
  mid-game map mutation able to serve a stale view, which the new
  `test_fov_cache_invalidates_when_map_changes` guards.
- CLI: `FovToggles::default()` is now on, and the game binary gains
  `--no-fov-cache` / `--no-fov-budget` opt-outs (`--fov-cache` /
  `--fov-budget <n>` still override). The profiling harness keeps its own
  default-off toggles, so baseline profiling is unchanged.
- Tests: added `test_fov_performance_defaults_are_on` and the invalidation test;
  reworked `test_fov_cumulative_budget_is_a_fidelity_dial` to take an explicit
  `None` unbounded reference. Full suite green (254 lib tests + 26 playground).

Docs: updated the PERFORMANCE.md prototype section and map-mutation note.

---

## 2026-09-27 — Off-board starfield with parallax and drift

### game: procedural starfield for the off-board void
The area beyond the board edge was flat black. It now renders a sparse
starfield (`crates/game/src/graphics/starfield.rs`).

- Three depth layers, each anchored at a fraction of the camera's motion, so
  near layers slide further than far ones as the player moves (parallax). Each
  layer also drifts slowly in a fixed direction so the field stays alive while
  the player stands still.
- Stars live on an infinite hashed lattice: moving/drifting reveals new cells,
  so nothing is generated or recycled and there is no bounding region to
  maintain.
- `Starfield::draw` is a pure function of `(screen, board_size, time)` with no
  per-frame state, preserving `test_headless_frames_are_byte_identical` and the
  screen-stability tests.
- Drawn after the FOV composite and only into off-board cells, so the board and
  visible contents are never touched. Wired via `Graphics::draw_starfield` from
  `Game::update_screen_from_draw_buffer`.
- Tests cover: off-board-only, determinism, time drift, rotation round-trip,
  and parallax ordering (near shifts more than far).

---

## 2026-09 — Deterministic portal ordering, relative-radius cap, test timeout

### game: deterministic portal order; relative-radius FOV cap; test timeout
Three changes:

1. **Deterministic portal ordering.** Portal iteration fed FOV sub-view order
   from hash maps. `portals_entering_from_square` and `iter_portals` now sort by
   `SquareWithOrthogonalDir::sort_key()` (square, then step), the FOV keeps
   portal arcs in an order-preserving `Vec` instead of a `HashMap`, and
   `combined_sub_fovs` groups by root through a `BTreeMap`. Sorting is the cheap
   fix because at most two portals are visible per square. New test
   `test_portal_iteration_is_deterministic`.
2. **B is now a cumulative *relative-radius* cap.** The budget charged each
   portal crossing its absolute entrance↔exit jump, so an adjacent portal that
   led far away was hidden (issue
   `issues/fov-budget-optimization-cant-see-through-portal/`). It now spends how
   far sight travelled in the current frame to reach the portal, and the frame
   extent is `min(radius, remaining)`. Renamed
   `FovOptions::cumulative_radius_budget` / `set_fov_cumulative_radius`. Also
   clamped the top-level extent to `radius`: a large budget had made the octant
   iterator sweep thousands of squares (the test-suite hang). Verified against
   the issue snapshot that the adjacent-portal view is preserved at budget 16.
3. **Default test timeout.** Added `.config/nextest.toml` with
   `slow-timeout = { period = "60s", terminate-after = 2 }`, so a runaway test
   fails after 120s instead of hanging the suite. This would have caught change
   2's hang.

Full suite green (541 tests).

---

## 2026-09 — Byte-identical frames: make the FOV draw order total

### game: make FOV draw order total so frames are deterministic
Frames were not byte-identical. The FOV's sub-view order is non-deterministic
(visible portals are iterated from a `HashMap` to build `transformed_sub_fovs`),
and `FieldOfViewResult::sorted_by_draw_order` only broke ties by portal depth
(a stable sort). Equal-depth visibilities therefore fell back to HashMap order,
and `drawable_at_relative_square` picked different glyphs frame-to-frame — the
"ambiguity (and thus flashing)" its old TODO warned about, defeating roadmap
W.A's byte-identical-frame promise.

`sorted_by_draw_order` is now a total order on `(portal_depth,
absolute_square.x, absolute_square.y, rotation)`. New regression test
`test_headless_frames_are_byte_identical` renders each tick twice on racetrack
and hallways and asserts identical screen buffers; all goldens still pass. FOV
*construction* order remains HashMap-dependent (affects debug FOV trace/JSON
ordering, not frames).

Also recorded the map-mutation cache-invalidation TODO on the FOV cache fields
(`game/mod.rs`) and in `docs/PERFORMANCE.md`, left for later by request.

Full suite green (540 tests).

---

## 2026-09 — Test the FOV prototypes and fix the budget's initialization

### perf: test FOV cache/budget, fix budget initialization
Added three tests that enable the prototypes directly (they were default-off and
untested): `test_fov_cache_matches_fresh_computation`,
`test_fov_cache_invalidates_when_player_moves`, and
`test_fov_cumulative_budget_is_a_fidelity_dial`.

Fixed a bug in prototype B: `remaining_distance` was initialized to `radius`,
so the budget's magnitude was ignored — every budget ≥ radius behaved
identically (that is why budgets 8–48 had looked flat). It now initializes from
the budget, and the per-hop extent stays `radius` (gate-only, never shrinking a
hop's own view). B is now a real speed/fidelity dial: budget 4/8/16/32/1000 →
13.7/16.1/32.1/43.0/46.6 ms/FOV on racetrack; A + B(8) = 7.7 ms/frame.

The tests compare an order-insensitive FOV signature rather than screen
buffers, because of a side finding: the FOV's sub-view order is
non-deterministic (visible portals are collected into a `HashMap` and iterated
to build `transformed_sub_fovs`), and the renderer picks glyphs first-match, so
frames are not byte-identical. This is unrelated to the cache but defeats
roadmap W.A's "byte-identical frame" promise; recorded in
`docs/PERFORMANCE.md`.

Full suite green (539 tests).

---

## 2026-09 — Expose the FOV prototypes on the game binary

### game: add --fov-cache / --fov-budget CLI flags
The FOV prototypes were only reachable from the profiling example. Added
`FovToggles` and a third `do_everything` argument, applied to the game after
map setup or snapshot load, plus `--fov-cache` and `--fov-budget <n>` (or
`--fov-budget=<n>`) in `main.rs`'s arg parser and usage text.

Both default off, so normal play is unchanged. The `maps/*.sh` launchers now
forward extra args, so `./maps/racetrack.sh --fov-cache --fov-budget 16` works;
`./play-game --map racetrack --fov-cache` works directly. Verified the game
starts and renders under a pty with both flags set.

---

## 2026-09 — Prototype FOV fixes for the racetrack frame time

### perf: prototype FOV cache and cumulative distance budget behind flags
Implemented prototypes A and B from `docs/PERFORMANCE.md` behind default-off
runtime toggles on `Game`:

- **A** (`set_fov_cache_enabled`): cache the player FOV per player square. The
  FOV is render-only and a pure function of the player square plus the static
  map (blocks/portals never change at runtime, facing is irrelevant), so it
  only recomputes when the player moves. Served at the draw seam by
  `player_fov_for_draw`.
- **B** (`set_fov_cumulative_distance_budget`): thread `FovOptions` +
  `remaining_distance` through the FOV recursion
  (`field_of_view_within_arc_in_single_octant_impl`). Each portal crossing
  spends the straight-line separation between sub-view centers, and a crossing
  that overruns the budget is skipped instead of recursed. This is a distance
  budget, not a depth cap. Public wrappers keep legacy per-hop-only behavior
  via `FovOptions::default()`.

The harness gains `--cache`, `--budget=<n>`, and a coarse `vis` mode.
Racetrack, release: 71.2 → 23.6 ms/frame (A), 28.0 (B at budget 16), 10.8
(A+B). B's curve is flat for budgets 8–48. Caveat: B changes visibility (a
coarse 11x7 probe goes from 14 to 35 invisible squares), so it must be tuned
and validated against goldens before enabling. Default behavior is unchanged;
all 267 game tests pass.

Recorded the results, the float-key memoization caveat, and the staged plan in
`docs/PERFORMANCE.md`.

---

## 2026-09 — Profile the racetrack map; FOV recursion is the hotspot

### perf: add racetrack profiling harness; FOV recursion dominates
Added `crates/game/examples/profile_racetrack.rs`, a headless harness that
replays the real loop (`tick_realtime_effects` + a headless draw per frame) and
can time the player FOV alone. Used it with `gprof` to rank hotspots.

Racetrack renders at 71 ms/frame (release) against the 21 ms budget — ~2.2x
slower than demo/hallways (29/33 ms). All time is in `Game::draw`. The driver
is the portal-recursive FOV (`field_of_view_within_arc_in_single_octant`):
~545k recursive calls/frame vs 72–135k on other maps, and a single player-FOV
computation costs 47 ms. The FOV is recomputed from scratch every draw
(`game/mod.rs:601`) with no memoization and no recursion depth cap or
visited-portal set; the racetrack's 19-face L portal and 3-wide two-way corners
give it many branching crossings. Secondary costs are the angle-interval/trig
math feeding the recursion, angled-block glyph mapping over point tuples, and
general `WorldSquare` SipHash.

Wrote `docs/PERFORMANCE.md` with the numbers and method, and linked it from the
ARCHITECTURE profiling section. Root cause overlaps roadmap W.C ("no depth
cap"); this quantifies its frame-time impact.

---

## 2026-09 — Document which profilers actually run in the sandbox

### docs: note available profiling tooling in ARCHITECTURE
The `flake.nix` dev shell lists `cargo-flamegraph`, `cargo-profiler` and
`uftrace`, but in this sandbox only some resolve. Replaced the stale
`flamegraph.svg` mention in "Testing & Tooling" with a table of what actually
runs: `uftrace` (verified via dynamic tracing of `map_diagram racetrack`),
`gprof` (sampling from `gmon.out`), and `cargo-nextest` work; `cargo-flamegraph`
and `cargo-profiler` do not, because `perf` and `valgrind` are absent (as are
gdb/lldb/rr). Noted that `.cargo/config.toml` already builds with
`-Zinstrument-mcount`, and that the TTY game loop must be profiled through a
headless harness (`map_diagram`, `draw_headless_now`, or a frame-loop test).

---

## 2026-09 — Build wide racetrack portal bands from one transform

### game: derive every wide-band strip from one portal transform
Replaced the map's hand-laid multi-square portals with a single helper,
`place_wide_portal_from_transform(entrance, exit, width)`: caller gives the
center strip and the helper derives every other strip's exit from that
portal's rigid transform. The corners (width 3), both shuttle flips (width
3) and the 19-wide L portal now all use it, so a band is one coherent window
by construction — including turns (180° flips, 90° corners) that reverse the
wall's lateral direction. This supersedes the manual `-d` mirror and the
manually reversed L exit ordering.

Removed `test_every_map_has_coherent_portal_bands` and the L-specific
coherence test: asserting coherent bands as an invariant is too strong, since
unorthodox portal arrangements may be intentional. Coherence now comes from
the construction helper; behavior stays covered by the lap, shuttle and
L-render tests.

`place_wide_corner_portal` is gone (folded into the new helper).

---

## 2026-09 — Racetrack shuttle band sheared; audit all map portal bands

### game: mirror the racetrack shuttle's 3-wide exit band
The same shear as the L portal, one square over: a 180° flip reverses a
wall's lateral direction, so a 3-wide entrance band's exit band must be its
mirror. The shuttle offset both ends of each flip by `+d`, so the three
strips each got a different transform and the window twisted. Exit offsets
now use `-d` for both shuttle walls.

### game: assert every map's portal bands share one transform
Added `test_every_map_has_coherent_portal_bands`, which walks demo,
racetrack, hallways and test_map and asserts that entrance strips laid side
by side (adjacent along the axis perpendicular to their shared entrance
direction) are `is_coherent_with` each other. It fails on the shuttle before
the mirror fix (`racetrack: portal band shears: (19,10) -> (19,14) vs
(20,10) -> (20,14)`) and passes after. Adjacency along the travel axis is
deliberately excluded — those are separate windows, not one band. Demo,
hallways and test_map were already coherent.

---

## 2026-09 — Racetrack L portal sheared across its strips

### game: make the racetrack's big L portal one coherent window
The "L" exhibit places a 19-face vertical entrance wall (x = base.x+22,
facing right) turning 90° onto a 19-face horizontal exit wall at
y = base.y+7 (facing up). The turn is a +90° (anticlockwise) rotation, so
the entrance wall's "up" tangent maps to "left": the exit wall must run
right-to-left as the entrance runs bottom-to-top. The code walked the exit
wall left-to-right (`base.x + 2 + i`), so each of the 19 strips got its own
rigid transform (rotation centres stepping along a diagonal) instead of one
shared transform. The wall was therefore a glide-reflection, not a portal —
it rendered with the "weird turns and breaks" of a sheared surface, and the
per-strip exits were mirrored. Fixed to `base.x + 20 - i` (exit x = 44 down
to 26 as the entrance climbs y = 4..22).

New regression test `test_racetrack_l_portal_strips_are_one_coherent_portal`
asserts every strip `is_coherent_with` the first (it failed before the fix).
Updated `test_racetrack_stationary_cubes_render_at_l_portal_wall`: the
bottom entrance's sliver now emerges at the right end of the exit wall
`(44,20)` instead of the mirrored `(26,20)`.

---

## 2026-09 — Racetrack bottom-left corner portal placement

### game: fix bottom-left racetrack corner exit off-by-one
The racetrack's four 3-wide turning portals must each exit two squares past
their corner square: at one square the 3-wide exit band overlaps the 3-wide
entrance band (sharing a corner square), and the resulting lap is asymmetric.
The bottom-left corner alone used `bottom_row_y + 1`, so its exit row (y=12)
met its entrance column (x=28) at `(28,12)`, and the left straightaway ran a
square longer than the right. Corrected to `+ 2`, giving a symmetric 9x5 path
with the exit bands clear of the entrance bands.

The unintuitive placement was masked by tests that had been written to the
asymmetric lap: it also made the lap length 29 rather than the true 28. Updated
`test_racetrack_map_cubes_loop_back_after_one_lap` (7.25s -> 7.0s),
`test_racetrack_map_cubes_survive_frame_rate_ticks` (345 -> 333 frames), the
`test_portal_aware_move_racetrack_lap_closes` geometry (bottom-left exit
`(10,6)` -> `(10,7)`, move 29 -> 28), and the map's doc comment (lap 29 -> 28,
plus the lap direction: up-right-down-left is clockwise, not counter-).

---

## 2026-09 — Black diagonal portal-seam artifact

### fov: merge adjacent portal-slice view cones as a set (fixes black diagonal seam)
Root cause of `issues/black-diagonal-portal-seam/`: standing on one of a
stacked pair of east-facing portals, a square on the 45-degree seam between
their openings had only one partial visibility and rendered as an
`OUT_OF_SIGHT` black diagonal. The item-12 merge (`combined_with_unioning_arcs`)
carried a single `view_arc`; when two same-root slices did not touch it kept one
arc but the other's squares, so a later touching slice unioned only with the
retained arc and the dropped slice's squares were recomputed under an arc that
no longer covered them (order-dependent, since `combined_sub_fovs` reduces a
`HashMap` group). `FieldOfViewResult` now carries `view_arcs:
Vec<AngleInterval>`; `combined_main_view_only` merges touching/overlapping
fragments (`merge_contiguous_arc_intervals`) and recomputes squares under the
merged set (`visibility_of_square_under_arc_intervals`), while
blocker-separated arcs stay distinct. All seam cells `rel(k,-k)` are now fully
visible. Regression tests `test_stacked_portal_slices_union_to_full_visibility`
and `stacked_portal_seam_has_no_out_of_sight_partial`; full suite green
(536 passed / 9 skipped).

### game: issue capture + minimized repro for the black diagonal seam
New `issues/black-diagonal-portal-seam/` with the snapshot, a write-up
(symptom, observed cells, minimal repro, mechanism), and `debug/` outputs from
`snapshot_tool` (render/diff/cells/fov-trace/explain/invariants). The minimizer
reduced the capture to **player + 2 portals**. The repro was first written with
`--no-screen-crop` because the crop candidate crashed on an odd-width bug (see
next entry); that workaround and its false diagnosis ("halls of mirrors") were
later corrected.

### game: fix odd-width screen crop + clip off-screen draws
The virtual-screen crop panicked (`Tried to draw character off screen`) because
`screen_crop_candidate` returned `2 * (dx + margin) + 1` — always an odd width.
One world square is two char columns addressed by even left columns, so an odd
width's last column is a half-visible square whose right half is off-screen;
`Screen::all_screen_squares` yielded it and `draw_glyphs_straight_to_screen_square`
panicked. Large crops only appeared to work because their last column fell
outside the sight radius. Fix: `screen_crop_candidate` now returns an even
width, and off-screen character columns are clipped instead of panicking
(`Screen::draw_glyph_straight_to_screen_buffer`, `draw_string_to_screen`;
roadmap item 5). The minimized state crops to 22x13. Tests:
`drawing_an_odd_width_edge_square_clips_instead_of_panicking` and the
even-width assertion in `screen_crop_candidate_bounds_artifact_with_margin`.
Superseded/reverted the earlier false fix (portal-square bounding,
`catch_unwind`, late crop).

---

## 2026-09 — Debug tooling wishlist (W.A–W.G)

Implemented the debug-tooling wishlist from the front of
[ROADMAP.md](ROADMAP.md), motivated by the portal-depth partial-visibility
artifact in `issues/black-block-deep-in-portal/`. All workspace tests green.

### fov: union portal-slice view cones before merging (fixes black-block artifact)
Root cause: sub-FOVs reaching the same transformed root through adjacent
portal-face slices were merged by combining their per-square half-planes
(`combined_increasing_visibility`), which cannot represent the union of two
non-complementary partials; the uncovered part rendered as an `OUT_OF_SIGHT`
(black) partial. Carry the view cone on `FieldOfViewResult::view_arc` and, when
`combined_sub_fovs` merges same-root results, union the arcs and recompute
affected squares under the unioned cone (`combined_with_unioning_arcs`); the
top-level octant fold and blocker splits keep their arcs. The artifact cell
`(49,37)` now renders as a fully-visible tinted floor. Added regression test
`test_portal_slice_arcs_union_to_full_visibility`; full workspace suite green
(238 game lib). Regenerated the issue debug outputs (analysis post-fix;
minimizer outputs moved to `debug/pre-fix/`).

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
