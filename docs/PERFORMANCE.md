# Performance

## Racetrack frame-time hotspots

The racetrack map renders far over budget. Reproduce with the headless
harness `crates/game/examples/profile_racetrack.rs`, which replays the real
loop (`tick_realtime_effects` + a full headless draw per frame):

```
cargo run --release -p game --example profile_racetrack -- racetrack 200   # ms/frame
cargo run --release -p game --example profile_racetrack -- fov racetrack 50 # ms per player-FOV
```

Prototype toggles (default off; see "Fix approaches"):
`--cache` (A) and `--budget=<squares>` (B).

The same toggles are available on the real game binary, so a map can be played
with them enabled:

```
./play-game --map racetrack --fov-cache
./play-game --map racetrack --fov-budget 16
./maps/racetrack.sh --fov-cache --fov-budget 16
```

For function attribution, run the harness from a scratch directory (it drops a
`gmon.out` on exit) and feed it to `gprof`:

```
gprof target/release/examples/profile_racetrack gmon.out
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

### B. Make the distance cap cumulative
Thread a `remaining: f32` budget through the recursion, initialized to the
budget (not `radius`), and subtract the hop distance
(`|transformed_center.square() − center_square|`) at each portal crossing
(`:1207`); stop recursing at `remaining <= 0`. The per-hop `out_of_range` bound
stays `radius`, so this only gates *whether* to cross a portal — it is a
distance cap, not a depth cap, and it never shrinks a hop's own view. Racetrack
corners are ~2-3 squares per hop, so total portal-aware sight distance is
bounded while nearby portals can still be crossed several times. Medium risk:
it changes visible results, so pick a budget that preserves intended views (the
19-wide L wall, snapshots). Verify against the FOV invariant oracle,
`test_portal_slice_arcs_union_to_full_visibility`, the racetrack L-wall render
test, and snapshot goldens.

> A first cut initialized `remaining` to `radius`, which silently ignored the
> budget's magnitude (every budget ≥ radius behaved identically). The prototype
> now initializes from the budget; the numbers below reflect the fix.

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

Both prototypes are implemented behind default-off runtime toggles on `Game`
(`set_fov_cache_enabled`, `set_fov_cumulative_distance_budget`), reachable from
the game binary as `--fov-cache` / `--fov-budget <n>`. Defaults are unchanged
and the full suite (539 tests) is green. Three tests exercise the toggles:
cache-matches-fresh, cache-invalidation-on-move, and budget-as-a-fidelity-dial.

Racetrack, release, 150–200 frames:

| configuration      | ms/frame | ms / player-FOV |
|--------------------|---------:|----------------:|
| baseline           |     70.1 |            46.3 |
| A: cache           |     23.7 |             —   |
| B: budget=8        |     23.8 |            16.1 |
| A + B: budget=8    |      7.7 |             —   |

B is now a real speed/fidelity dial (budget 4/8/16/32/1000 → 13.7/16.1/32.1/
43.0/46.6 ms per FOV). A coarse visibility probe over an 11x7 grid around the
player (`full`/`partial`/`invisible`):

| budget | full | partial | invisible |
|--------|-----:|--------:|----------:|
| none   |   42 |      21 |        14 |
| 4      |   29 |      13 |        35 |
| 8      |   33 |      13 |        31 |
| 16     |   38 |      18 |        21 |
| 32     |   42 |      20 |        15 |
| 1000   |   42 |      21 |        14 |

So budget 32 is nearly indistinguishable from baseline (1 partial lost) but
buys little; budget 8 loses 9 of 63 visible squares for ~3x. A budget that
preserves intended views (and golden validation) still needs choosing.

Findings:
- **A** alone removes the per-frame recomputation but the (large) FOV tree is
  still traversed every frame for compositing: 70 → 23.7 ms for a stationary
  player. It does nothing for the per-step hitch while moving.
- **A + B** compounds to 7.7 ms/frame — comfortably under the 21 ms tick.

### Side finding: FOV ordering is non-deterministic

The FOV's sub-view order varies between identical computations, because visible
portals are collected into a `HashMap` and iterated to build
`transformed_sub_fovs` (`fov_stuff.rs`, `significantly_visible_portals_in_sight`).
The *set* of visibilities is stable (the cache-equivalence test uses an
order-insensitive signature for this reason), but the renderer picks glyphs
first-match, so frames are not byte-identical — this defeats roadmap W.A's
"byte-identical frame at any index" promise and is unrelated to the cache.
Worth its own fix (iterate in a deterministic order).

## Staging

1. **A** — quick, safe symptom relief; re-profile.
2. **B** — the cumulative distance cap; bounds the recursion generically.
3. **D** — if still over budget.
4. **C** — only if B+D leave the moving-player hitch too high, or if a
   cap-free fix is wanted.
