# Performance

## Racetrack frame-time hotspots

The racetrack map renders far over budget. Reproduce with the headless
harness `crates/game/examples/profile_racetrack.rs`, which replays the real
loop (`tick_realtime_effects` + a full headless draw per frame):

```
cargo run --release -p game --example profile_racetrack -- racetrack 200   # ms/frame
cargo run --release -p game --example profile_racetrack -- fov racetrack 50 # ms per player-FOV
```

Harness toggles: `--cache` (A) and `--radius=<squares>` (override the sight
radius, the single FOV dial).

The cache also defaults **on** on the real game binary. There is no separate
budget: the sight radius is the one knob (see "The cumulative budget is
removed" below). `--fov-radius <n>` shrinks the radius for profiling; opt out of
the cache with `--no-fov-cache`:

```
./play-game --map racetrack                       # cache + full 16-square sight (default)
./play-game --map racetrack --no-fov-cache        # recompute the FOV every draw
./play-game --map racetrack --fov-radius 8        # smaller sight radius
./maps/racetrack.sh --fov-radius 8
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
   `field_of_view_within_arc_in_single_octant` (`fov_stuff.rs:1076`).
   ~545k recursive calls/frame on racetrack, vs ~135k on demo and ~72k on
   hallways. The whole FOV is recomputed from scratch on every draw
   (`game/mod.rs:603`), with **no memoization** and **no visited-`(root, arc)`
   set**. Depth is *not* unbounded (see [Depth
   bound](#depth-bound-2026-09-28)); recursion stops when a frame's extent
   `max_extent` is exceeded or the view arc narrows below
   `NARROWEST_VIEW_CONE_ALLOWED_IN_DEGREES`. The cost is exponential in the
   **branching**: each crossed square can spawn up to two portal sub-views, and
   racetrack's 19-face L portal plus four 3-wide two-way corners give it many
   branching crossings, which is why it dominates this map specifically.
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

The `radius` bound is **per portal hop**, not cumulative:
`field_of_view_within_arc_in_single_octant` measures `relative_square` from the
current sub-view's transformed center (`fov_stuff.rs:1109`), and every
portal crossing passes a fresh `transformed_center` (`:1224`). A path can
therefore cross many portals, each time re-applying the full `radius`. A
cumulative budget is *not* implemented at the point of this profile.

That does **not** make the recursion unbounded (corrected 2026-09-28, see [Depth
bound](#depth-bound-2026-09-28)). Along any path the apparent image strictly
advances — a deeper frame inherits the parent's `starting_step_in_fov_sequence`
and starts past the near field — and each frame's scan stops at
`max_extent = radius`, so **depth ≤ radius + 1**. The frame-time driver is the
**number of paths**: each crossed square can spawn up to two portal sub-views
(plus blocker-split sub-arcs at the same center), so the tree grows roughly
`branching^radius`. Cost is exponential in the sight radius, not in the depth.

Also relevant to every fix below: `player_field_of_view` (`game/mod.rs:1428`) is
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
71 ms). Moving still pays ~47 ms/step, so pair with C. Low risk; the
result is `Clone`, and `FieldOfViewResult` is pure. Verify with a
recompute-count test plus the existing golden render tests.

> **Map mutation:** the cache key is only the player square, valid while the
> map is static between draws. `place_block` and the portal-placement methods
> now clear the cache, so mid-game map mutation cannot serve a stale view. A
> `map_version: u64` keyed cache would allow partial invalidation instead of a
> full clear, but the maps are small enough that a clear is cheap.

### B. Cumulative *relative-radius* budget — **removed (2026-09-28)**
An experiment that threaded a `remaining_radius` accumulator through the
recursion and shrank each sub-view's extent to `min(radius, budget)`, gating
crossings once spent. It was tried, debugged several times (see the corrections
below), and then **deleted**: the budget was only ever "the radius" (its default
was `PLAYER_SIGHT_RADIUS`, and its remaining effect was to shrink the view), and
the extra crossing gate produced artifact-prone pruning. The sight radius is now
the single FOV dial (`Game::set_player_sight_radius`, `--fov-radius`). Details of
what was learned are kept below for the record.

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
angled-block paths; avoid repeated
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

> Historical. The budget rows below used the over-shrinking metric corrected on
> 2026-09-28, and the budget feature itself was removed later that day; see the
> corrections, the depth bound, and "The cumulative budget is removed".

At the time, both prototypes were on by default on `Game` (the cache at
construction, the budget initialized to `PLAYER_SIGHT_RADIUS`), reachable from
the game binary as `--no-fov-cache` / `--no-fov-budget` / `--fov-budget <n>`.
Placing a block or portal invalidates the cache, since its key is only the
player square. Tests covered cache-matches-fresh, cache-invalidation-on-move,
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

### Correction history (2026-09-28): budget artifacts (feature later removed)

> The budget was removed after these corrections; kept for the record. See "The
> cumulative budget is removed" below.

The original metric subtracted the portal's `spent` radius from the child
sub-view's *extent*. That is geometrically wrong: a sub-view is centered on the
viewer's virtual image (the transformed player), so the portal exit sits at
child-relative radius `spent`, and child-relative distance already equals
cumulative apparent distance. Subtracting `spent` again clipped the window to
the portal plane. For a portal more than half the sight radius away (`spent >
budget/2`) nothing was visible through it — the residual reported by
the original portal-window report after the earlier
"relative metric" fix (snapshot_2, portal 8–9 squares north).

`field_of_view_within_arc_in_single_octant_impl` now gives every level the same
apparent extent, `min(radius, budget)`. The `remaining_radius` accumulator is a
per-hop *path* cost, not apparent distance: through the parallel portals of a
hall of mirrors it grows roughly twice as fast as the image, so using it to gate
at `budget == radius` blacked out the far half of the view (the live `snapshot/`
infinite portal hallway: rel 7..16 black, rel 4..6 visible). The gate is now
active only when the budget is *tighter* than the sight radius
(`budget < radius`); at or above the radius the view is legacy, because
`max_extent` already caps apparent reach. Re-measured racetrack FOV:

| budget | ms / FOV |
|-------:|---------:|
| none   |     8.3  |
| 4      |     0.42 |
| 8      |     1.8  |
| 16     |     8.2  |
| 1000   |     8.3  |

The old 24 ms figure for budget 16 was an artifact of the over-shrinking. The
corrected metric makes budget ≥ radius a no-op (the default 16 included);
tighter budgets still buy large speedups by shrinking the per-level extent and
pruning crossings.

### Depth bound (2026-09-28)

The per-hop `radius` does not bound recursion depth *directly*, but the
recursion is not unbounded. Along a path the apparent image strictly advances:
a deeper frame inherits the parent's `starting_step_in_fov_sequence`, so it
starts past the near field, and each frame scans only `radius` squares
(`max_extent`). A portal on the player's own square adds at most one extra
"zeroth" hop at apparent distance 0. So **depth ≤ radius + 1 in every
configuration tested** (not proven in general). Measured at radius 5
(`test_portal_recursion_depth_stays_near_the_sight_radius`, plus a
randomized check over 100 portal layouts at radius 2..6).

| setup (radius 5) | max depth |
|---|---:|
| 3 portals immediately north → one common exit | 1 |
| single portal north, exit on its own square | 5 |
| single portal on the player's own square | 6 |

No tested configuration reached deeper, and no zero-advance loop formed: a child
frame skips its near field, so it cannot re-cross a portal at the same apparent
distance.

The **frame-time consequence** is that cost is exponential in the *radius*, not
the depth: each crossed square can spawn up to two sub-views, so the path count
grows like `branching^radius`. Measured racetrack FOV with `--radius`
(`0.60 ms` at R=4 → `2.3 ms` at R=8 → `8.6 ms` at R=16) scales ~`3.7x` per `+4`
radius, i.e. an effective branching factor ~1.4. Raising `PLAYER_SIGHT_RADIUS` is
therefore super-linearly expensive; lowering it is super-linearly cheap.

Because depth is already bounded, a **depth cap is not a useful lever**. The
lever is the number of paths/nodes: that is what a sub-radius budget trimmed and
what [dominance pruning (C)](#c-dominance-pruning-no-cap-at-all) would collapse
without a fidelity cost.

### The cumulative budget is removed (2026-09-28)

With depth bounded and the budget's default (`PLAYER_SIGHT_RADIUS`) a no-op, the
budget was redundant: its only remaining effect was to shrink the effective
sight radius *and* run a per-hop crossing gate that produced artifacts. It was
deleted. `FovOptions` / `cumulative_radius_budget` / `FovOptions` threading and
the `--fov-budget` / `--no-fov-budget` flags are gone; the recursion's extent is
now always `max_extent = radius`. The radius is the single FOV dial, exposed as
`Game::set_player_sight_radius` (`--fov-radius <n>` on the binary; `--radius=<n>`
on the harness). Default behavior is unchanged (the old default was already the
no-op case).

### The portal-window bug and the relative metric
The original portal-window report had the player at (53,27), adjacent to the
L portal's exit at (54,27). The old
*absolute-distance* cap charged that crossing the L pair's ~17-square
entrance↔exit separation, so everything seen through the adjacent portal was
hidden. The relative-radius metric charges only the ~1 square travelled in the
current frame to reach it, so an adjacent portal window is preserved; this
correction extends that to distant portals as well.

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

1. **A** — cache, done, default on.
2. **B** — cumulative relative-radius budget: tried, debugged, **removed** (it
   was only ever the radius plus an artifact-prone crossing gate).
3. **D** — constant-factor cleanups; safe, no behavior change.
4. **C** — dominance pruning. The remaining *cap-free* fix: it attacks the
   exponential path count directly (the real cost, since depth is already
   bounded) with no fidelity loss.
