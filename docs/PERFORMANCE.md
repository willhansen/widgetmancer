# Performance

## Racetrack frame-time hotspots

The racetrack map renders far over budget. Reproduce with the headless
harness `crates/game/examples/profile_racetrack.rs`, which replays the real
loop (`tick_realtime_effects` + a full headless draw per frame):

```
cargo run --release -p game --example profile_racetrack -- racetrack 200   # ms/frame
cargo run --release -p game --example profile_racetrack -- fov racetrack 50 # ms per player-FOV
```

Prototype toggles (harness defaults off; see "Fix approaches"):
`--cache` (A) and `--budget=<squares>` (B).

The same toggles are available on the real game binary, where they now
**default on**. Opt out with `--no-fov-cache` / `--no-fov-budget`, or override
the budget:

```
./play-game --map racetrack                       # cache + budget 16 (default)
./play-game --map racetrack --no-fov-cache        # recompute the FOV every draw
./play-game --map racetrack --no-fov-budget       # legacy per-hop sight
./play-game --map racetrack --fov-budget 8        # tighter budget
./maps/racetrack.sh --no-fov-budget
```

For function attribution, build and run the harness through
`scripts/profile.sh` (mcount instrumentation is opt-in; see
[ARCHITECTURE.md](ARCHITECTURE.md#profiling-what-actually-runs-in-this-sandbox)),
from a scratch directory so it drops a `gmon.out` on exit, then feed that to
`gprof`:

```
scripts/profile.sh cargo build --release -p game --example profile_racetrack
cd /tmp && /root/project/target/release/examples/profile_racetrack racetrack 200
gprof /root/project/target/release/examples/profile_racetrack gmon.out
```

Measured in release on the sandbox host (i5-8400). The loop sleeps 21 ms per
iteration, so ~21 ms/frame is the budget (~48 fps).

| map      | ms/frame | ms / player-FOV |
|----------|---------:|----------------:|
| demo     |     29.2 |            22.2 |
| hallways |     32.9 |            25.4 |
| racetrack|     71.2 |            47.3 |

Racetrack is ~3.4x the frame budget; a *single* player-FOV computation is
47 ms of the 71 ms frame.

## Ranking (racetrack)

Per-frame time is entirely in `Game::draw` →
`populate_draw_buffer` + `update_screen_from_draw_buffer`. Hotspots, by
gprof call count over a 60-frame run (call counts are exact; gprof self-times
are noisy under PIE/ICF):

1. **Portal-recursive FOV.** `Game::player_field_of_view` →
   `portal_aware_field_of_view_from_point_traced` →
   `field_of_view_within_arc_in_single_octant` (`fov_stuff.rs:1064`).
   ~545k recursive calls/frame on racetrack, vs ~135k on demo and ~72k on
   hallways. The whole FOV is recomputed from scratch on every draw
   (`game/mod.rs:601`), with **no memoization** and **no recursion depth cap or
   visited-portal set**; recursion terminates only when the view arc narrows
   below `NARROWEST_VIEW_CONE_ALLOWED_IN_DEGREES`. The racetrack's 19-face L
   portal plus four 3-wide two-way corners give the recursion many branching
   portal crossings, which is why it dominates this map specifically.
2. **Angle-interval + trig math** feeding that recursion:
   `AngleInterval::from_square_and_center_offset` (~9–28M calls/60 frames),
   `contains_or_touches_angle`, `overlaps_other_by_at_least_this_much`,
   `QuarterTurnsAnticlockwise::from_start_and_end_directions` (14.6M),
   `int_cos`/`int_sin`, `fmodf`.
3. **Angled-block glyph mapping** —
   `terminal_rendering::angled_blocks::points_to_angled_block_mapping` /
   `half_plane_to_angled_block_character`, SipHash over point tuples. Scales
   with the 15 death cubes that straddle portal faces.
4. **General `HashMap<WorldSquare, _>` SipHash** (`RandomState::hash_one`),
   ~7M/60 frames, mostly from the FOV path.

## Root cause

Roadmap item W.C already records cumulative arc shrink (no depth cap) as the
FOV correctness concern. The profile shows the same unbounded recursion is the
*frame-time* driver on portal-dense maps.

The existing `radius` bound is **per portal hop**, not cumulative:
`field_of_view_within_arc_in_single_octant` measures `relative_square` from the
current sub-view's transformed center (`fov_stuff.rs:1088-1092`), and every
portal crossing passes a fresh `transformed_center` (`:1207-1219`). A sight line
can therefore travel `radius` squares, cross a portal, travel `radius` again,
and so on. Recursion terminates only when the view arc shrinks below
`NARROWEST_VIEW_CONE_ALLOWED_IN_DEGREES = 0.001°`, which is what explodes on the
racetrack. A cumulative budget is *not* implemented.

Also relevant to every fix below: `player_field_of_view` (`game/mod.rs:1410`) is
render-only, called once per draw (`:601`), and depends only on `player_square`
+ `blocks.blocks` + `portal_geometry`. Blocks are only inserted during map setup
(`place_block`; no runtime removal) and portals are fixed, and the player's
facing does not affect the full 360° view. So the FOV is a pure function of the
player's square.

## Fix approaches

### A. Cache the player FOV per player square
Store `Option<(WorldSquare, FieldOfViewResult)>` on `Game` and recompute only
when `player_square()` (or a map/`blocks` version stamp) changes; reset on
snapshot load. Expected: stationary frames drop 71 ms → ~24 ms (FOV is 47 of
71 ms). Moving still pays ~47 ms/step, so pair with B or C. Low risk; the
result is `Clone`, and `FieldOfViewResult` is pure. Verify with a
recompute-count test plus the existing golden render tests.

> **Map mutation:** the cache key is only the player square, valid while the
> map is static between draws. `place_block` and the portal-placement methods
> now clear the cache, so mid-game map mutation cannot serve a stale view. A
> `map_version: u64` keyed cache would allow partial invalidation instead of a
> full clear, but the maps are small enough that a clear is cheap.

### B. Cumulative *relative-radius* cap
Thread a `remaining_radius: f32` through the recursion, initialized to the
budget (top-level extent clamped to `radius`), and at each portal crossing
subtract how far sight travelled **in the current frame** to reach the portal
(the portal square's Chebyshev offset), not the portal's absolute jump. The
current frame's extent is `min(radius, remaining_radius)`. This bounds total
relative sight travel (and thus recursion) without charging a portal its
unwrapped jump distance — so an adjacent portal that leads far away stays
see-through. It is a radius cap, not a depth cap. Medium risk: it changes
visible results (it shrinks peripheral views as the budget is spent), so pick a
budget that preserves intended views. Verify against the FOV invariant oracle,
`test_portal_slice_arcs_union_to_full_visibility`, the racetrack L-wall render
test, and snapshot goldens.

> Earlier cuts were wrong in two ways: (1) `remaining` was initialized to
> `radius`, silently ignoring the budget's magnitude (every budget ≥ radius
> behaved identically); (2) the spend was the portal's absolute
> entrance↔exit separation, which hides adjacent portals that lead far (see the
> issue snapshot below). Both are fixed; the numbers below reflect the
> relative-radius metric.

### C. Dominance pruning (no cap at all)
Track, per transformed root (or `(root, octant)`), the union of arcs already
fully explored. If an incoming arc is covered, skip; if partially covered,
process only the remainder. This kills the racetrack lap exactly — after one lap
the next incoming arc is narrower and already covered — with no depth or
distance cap. Highest complexity; see the float-keying caveat below. The
coverage set reuses `FieldOfViewResult::view_arcs` /
`combined_with_unioning_arcs`.

### D. Constant-factor cleanups
Faster hasher for the `WorldSquare` / point-tuple keys on the FOV and
angled-block paths; drop the `portal_view_arcs.clone()` (`:1162`) and repeated
`portals_entering_from_square` allocations; hoist
`from_square_and_center_offset` per octant pass. ~1.5–2.5x, no behavior change.

### Caveat: why sub-FOV memoization is not the primary fix (float keys)
Memoizing sub-FOVs keyed on the incoming `view_arc`/`center_offset` is fragile.
Those are `f32` values produced by different chains of geometry ops
(`intersection`, `transform_arc`, `from_square_and_center_offset`), so two
mathematically identical arcs from different portal paths differ in the low
bits. Exact-equality keys almost never hit (dead cache). Bucketed/approximate
keys either miss (no speedup) or produce false hits, where a sub-view is
computed from a different arc and per-square partial-visibility classifications
flip — reintroducing the sub-degree arc artifacts that roadmap item 12 fixed.
Dominance pruning (C) avoids this by testing arc *containment* (⊆) with a
conservative epsilon instead of equality, so the worst case is redundant work,
never wrong output. (An exact representation of angles as integer half-planes
would make memoization sound, but that is a large refactor of `AngleInterval`
and the FOV pipeline.)

## Prototype results (2026-09-27)

Both prototypes are on by default on `Game` (the cache at construction, the
budget initialized to `PLAYER_SIGHT_RADIUS`), and are also reachable from the
game binary as opt-outs `--no-fov-cache` / `--no-fov-budget` (with
`--fov-budget <n>` to override the value). Placing a block or portal
invalidates the cache, since its key is only the player square. The full suite
is green. Tests cover: cache-matches-fresh, cache-invalidation-on-move,
cache-invalidation-on-map-change, defaults-on, budget-as-a-fidelity-dial,
deterministic portal iteration, and byte-identical frames.

Racetrack, release, 120 frames:

| configuration        | ms/frame | ms / player-FOV |
|----------------------|---------:|----------------:|
| baseline             |     67.5 |            44.2 |
| A: cache             |     23.7 |             —   |
| B: budget=16         |     40.1 |            24.0 |
| A + B: budget=16     |     17.0 |             —   |

Budget 4/16/1000 → 2.2/24.0/43.9 ms/FOV. A coarse visibility probe over an 11x7
grid around the player (`full`/`partial`/`invisible`):

| budget | full | partial | invisible |
|--------|-----:|--------:|----------:|
| none   |   42 |      21 |        14 |
| 4      |    7 |       2 |        68 |
| 8      |   15 |       6 |        56 |
| 16     |   36 |      15 |        26 |
| 1000   |   42 |      21 |        14 |

So the relative-radius cap is a real dial: budget 1000 is indistinguishable
from baseline, budget 16 roughly halves the FOV cost while shrinking peripheral
views, budget 4 is a ~20x speedup with a very small view.

Findings:
- **A** alone removes the per-frame recomputation but the (large) FOV tree is
  still traversed every frame for compositing: 67.5 → 23.7 ms for a stationary
  player. It does nothing for the per-step hitch while moving.
- **A + B(16)** compounds to 17.0 ms/frame.

### The portal-window bug is fixed by the relative metric
The issue snapshot `issues/fov-budget-optimization-cant-see-through-portal/`
has the player at (53,27), adjacent to the L portal's exit at (54,27). The old
*absolute-distance* cap charged that crossing the L pair's ~17-square
entrance↔exit separation, so everything seen through the adjacent portal was
hidden. The relative-radius metric charges only the ~1 square travelled in the
current frame to reach it, so the window is preserved (verified: the previously
hidden squares are visible again at budget 16).

### Byte-identical frames + deterministic portal ordering

The FOV's sub-view order varied between identical computations, because visible
portals were collected into a `HashMap` and iterated to build
`transformed_sub_fovs`; `FieldOfViewResult::sorted_by_draw_order` then broke
ties only by portal depth (a stable sort), so equal-depth visibilities fell back
to that HashMap order and the renderer picked different glyphs frame-to-frame —
the "ambiguity (and thus flashing)" its old TODO warned about. Two fixes:

1. `sorted_by_draw_order` is now a **total** order — `(portal_depth,
   absolute_square.x, absolute_square.y, rotation)` — so equal-depth tie-breaks
   no longer fall back to hash order.
2. Portal iteration itself is deterministic: `portals_entering_from_square` and
   `iter_portals` sort by `SquareWithOrthogonalDir::sort_key()` (square, then
   step), and `combined_sub_fovs` groups by root through a `BTreeMap`. Sorting
   is the cheap fix here because at most two portals are visible per square
   (the recursion asserts ≤ 2), so the lists sorted are tiny; a `BTreeMap`
   throughout would mean deriving `Ord` across `Portal`/`SquareWithOrthogonalDir`
   and still wouldn't reach the `into_group_map_by` grouping.

Frames are byte-identical across ticks and maps (regression
`test_headless_frames_are_byte_identical`); `test_portal_iteration_is_deterministic`
guards the portal sort. The per-square visibility map is still a `HashMap`, so
`FieldOfViewResult`'s `Debug` output order can vary; that does not affect
frames.

## Staging

1. **A** — quick, safe symptom relief; re-profile.
2. **B** — the cumulative distance cap; bounds the recursion generically.
3. **D** — if still over budget.
4. **C** — only if B+D leave the moving-player hitch too high, or if a
   cap-free fix is wanted.
