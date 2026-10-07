# Black diagonal partial-square seam through adjacent portals

Status: **fixed** (2026-09). Captured from `snapshot/` (map `hallways`), player
at world (37,23) facing east. Same bug class as the
[`black-block-deep-in-portal`](../black-block-deep-in-portal/PORTAL_FOV_DEPTH_ARTIFACT.md)
artifact; the item-12 arc-union fix did not cover this geometry.

## Symptom

Standing in a vertical wall of east-facing portals, a diagonal line of partial
black squares renders down-and-right from the player. Each cell is an
`OUT_OF_SIGHT` (BLACK) partial with a red tint — `bg(base) = (26,0,0)` for
portal depth 1 — i.e. a `PartialVisibilityDrawable` shadow background with no
shallower view behind it.

In the original capture the run is five cells (relative `(1,-1)` .. `(5,-5)`).

## Observed cells (from `snapshot/screen.txt`, parsed)

Terminal 126x44; world 63x44; screen center/origin (37,23)/(6,44);
player buffer square (31,21) (left character column 62).

| buffer square | row | world | rel | char | fg | bg | meaning |
|---|---|---|---|---|---|---|---|
| (32,22) | 22 | (38,22) | (1,-1) | 🭥🭓 | (255,0,105) | (26,0,0) | **artifact**, depth 1 |
| (33,23) | 23 | (39,21) | (2,-2) | 🭥🭓 | (197,172,172) | (26,0,0) | **artifact**, depth 1 |
| (34,24) | 24 | (40,20) | (3,-3) | 🭥🭓 | (140,114,114) | (26,0,0) | **artifact**, depth 1 |
| (35,25) | 25 | (41,19) | (4,-4) | 🭥🭓 | (197,172,172) | (26,0,0) | **artifact**, depth 1 |
| (36,26) | 26 | (42,18) | (5,-5) | 🭥🭓 | (197,172,172) | (26,0,0) | **artifact**, depth 1 |

Decoding:
- bg `(26,0,0)` = `OUT_OF_SIGHT_COLOR` (BLACK) tinted RED at strength 0.1
  (`0.1 * portal_depth`, `fov_stuff.rs:823`), depth 1.
- fg = floor/wall GREY tints of the underlying drawable of the *absolute*
  square reached through the portal view.
- Each cell has exactly **one** FOV visibility (the depth-1 portal slice,
  `abs_vis` a partial half-plane). The complementary near-side visibility that
  would fill the black half is missing.

`snapshot_tool explain issues/black-diagonal-portal-seam 32 22`:

```
cell (square 32, row 22) left_col 64 world(38,22) rel(1,-1) fov_center(37,23):
  final: '🭥' fg(255,0,105) bg(26,0,0)
  fov visibilities: 1
    depth 1 abs(33,22) rot 0 abs_vis "🭥🭓" rel_vis "🭥🭓"
      draw_buffer: OffsetSquare(...)
```

Contrast `rel(1,-5)` (buffer 32,26), which is **correct**: it has two
complementary visibilities (`depth 0 abs(38,18) "█ "` and
`depth 1 abs(33,18) " █"`) that composite to a full square.

## Minimal reproduction

`minimized/` holds the minimizer output (`--no-screen-crop`):
**player + 2 portals**, no other entities/blocks:

- `(37,22) dir[1,0] -> (33,22)`
- `(37,23) dir[1,0] -> (33,23)`

With only these two portals the diagonal is longer (the portal chain is no
longer broken by neighboring portals): relative `(k,-k)` for `k = 1..15`,
absolute `(32+k, 23-k)`, each a single depth-1 partial on `bg(26,0,0)`.

```
snapshot_tool render issues/black-diagonal-portal-seam/minimized
snapshot_tool explain issues/black-diagonal-portal-seam/minimized 32 22
```

## Where to look

- `crates/game/src/fov_stuff.rs`
  - `field_of_view_within_arc_in_single_octant` (:1050) — portal handling at
    :1114-:1233; when a portal is found at the center square it redirects the
    east arc and `break`s at :1257 after recursing the remaining sub-arcs.
  - `combined_main_view_only` (:426) / `combined_increasing_visibility` (:105)
    — top-level main-view merging of arc slices still combines per-square
    half-planes, which cannot represent the union of non-complementary
    partials.
  - `combined_with_unioning_arcs` (:576) — item-12 fix; unions arcs only for
    same-root *sub-FOV*s (`combined_sub_fovs`, :527), not for the top-level
    main-view split.
  - `visibility_of_offset_square` (:1403),
    `square_visibility_from_one_view_arc_with_center_offset` (:1428).
  - `OctantFOVSquareSequenceIter` (:842) — the diagonal square `(k,-k)` is the
    `(out=1, across=1)` straddler of both octant 6 and octant 7, and is reached
    by the remaining-arc recursion only after the octant's arc has already been
    clipped by a neighboring portal.
- `crates/utility/src/angle_interval.rs` — `subtract` (:219), `union` (:146),
  `intersection` (:128), `at_least_fully_overlaps` (:167).
- `crates/game/src/game/snapshot.rs` — `explain_cell` (:876),
  `fov_invariants_report` (:890), `derive_artifact_anchor` / minimizer (:936+).

## Hypothesis

The player stands *on* an east-facing portal, so the whole `[-45°,45°]` view
arc is redirected through it. The square `rel(1,-1)` lies exactly on the 45°
seam between two stacked portals: the east face of the north portal `(37,22)`
clips the octant-6 slice at `[-71.57°,-45°]`, and the player's own portal
`(37,23)` covers the rest. The only surviving contribution is the depth-1
portal partial of `abs(33,22)`; the near-side coverage (the direct floor of
`(38,22)`) is dropped, so the uncovered half of the square renders opaque
`OUT_OF_SIGHT` black. Because the seam is at exactly 45°, the affected squares
form a perfect diagonal.

`snapshot_tool invariants` reports the exact same absolute squares as
"fully visible via one portal depth/relative square but partially visible via
another" (`abs(33,22)`, `abs(34,21)`, `abs(35,20)`, `abs(36,19)`,
`abs(37,18)`), i.e. the spurious shadow signature from item 12. The count is
257 for the full snapshot (99 for the minimized one), but many are the known
false positives (complementary partials that render correctly, e.g.
`rel(1,-4) abs(33,19)`).

## Tooling notes / gaps

- `snapshot_tool minimize` **panicked** during the virtual-screen crop on this
  snapshot: `Tried to draw character off screen: (x: 21, y: 0)`
  (`crates/terminal_rendering/src/screen.rs:412`). The minimized repro was
  initially produced with `--no-screen-crop`.
  The real cause is an **edge/parity bug**, not the view radius:
  `screen_crop_candidate` returned `2 * (dx + margin_chars) + 1`, i.e. always an
  **odd** width. One world square is two character columns and the renderer
  addresses glyphs by even left columns, so an odd width's last column is a
  left glyph whose right half is off-screen; `Screen::all_screen_squares`
  yields it and `draw_glyphs_straight_to_screen_square` panics. Large crops
  (e.g. 65x33) appeared to work only because their last column falls outside
  the sight radius, so nothing is drawn there — masking the bug.
- Fixed: `screen_crop_candidate` now returns an even width, and off-screen
  character columns are **clipped** instead of panicking
  (`Screen::draw_glyph_straight_to_screen_buffer`, `draw_string_to_screen`;
  roadmap item 5). With these, the same minimized state renders at 13 rows and
  drops from 126x44 to a 22x13 frame.
- `invariants` still has many false positives (roadmap W.D "reduce false
  positives" remains open). After the fix, the artifact relative squares are
  no longer flagged, but `abs(35,20)`, `abs(36,19)`, `abs(37,18)` still appear
  via other relative squares whose partials are complementary partners that
  render correctly.

## Repro commands

```sh
I=issues/black-diagonal-portal-seam
B=./target/debug/snapshot_tool   # cargo build -p game --features debug-tools --bin snapshot_tool
$B render   "$I"           > "$I/debug/render.txt"
$B diff     "$I"           > "$I/debug/diff.txt"      # OK: screen.txt matches
$B cells    "$I"           > "$I/debug/cells.txt"
$B fov-trace      "$I"     > "$I/debug/fov-trace.txt"
$B fov-trace-json "$I"     > "$I/debug/fov-trace.json"
$B explain  "$I" 32 22     > "$I/debug/explain-32-22.txt"
$B invariants "$I"         > "$I/debug/invariants.txt"
$B minimize "$I" 32 22 "$I/minimized.json" --no-screen-crop
```

## Plan (general fix)

1. Add a snapshot regression test asserting the artifact cells carry no
   `OUT_OF_SIGHT`-background partial (and/or that the listed absolute squares
   are not flagged with the full-vs-partial invariant).
2. Extend the item-12 arc handling to the top-level main-view split: when a
   `view_arc` is divided by a portal/blocker, visit squares straddling the
   split under the **union** of the resulting sub-arcs (do not union genuinely
   distinct octants/blockers).
3. Optionally: never let a deeper `OUT_OF_SIGHT` partial paint over a shallower
   visibility of the same relative square.
4. Verify `./run-tests`, `snapshot_tool diff`/`invariants`/`render`; update
   `docs/ROADMAP.md` and `docs/CHANGELOG.md`.

## Resolution (2026-09)

Diagnosed with `explain`/`fov-trace`/`invariants`: each artifact cell had exactly
one visibility — the depth-1 portal slice — and no complementary partner. The
player stands on an east-facing portal whose opening covers the whole east
quadrant, so the top-level main view records nothing there. The square on the
45-degree seam is covered by two stacked portals' openings (the north portal's
east face covers `[-71.57 deg, -45 deg]`, the player's own portal `[-45 deg,
45 deg]`), but only one slice ended up in the merged sub-FOV.

Root cause was in the item-12 merge. `combined_with_unioning_arcs` carried a
**single** `view_arc`; when two same-root portal slices did not touch, it fell
back to keeping one operand's arc while retaining the other's squares. A third,
touching slice then unioned only with the retained arc, so the dropped slice's
squares were recomputed under an arc that no longer covered them — a lone
partial rendered as opaque `OUT_OF_SIGHT` black. `combined_sub_fovs` reduces a
`HashMap` group, so the loss was order-dependent.

**Fix:** carry the view cones as a set, `FieldOfViewResult::view_arcs`, and
merge touching/overlapping fragments before recomputing (`view_arcs` in
`combined_main_view_only`, `merge_contiguous_arc_intervals`,
`visibility_of_square_under_arc_intervals` in `fov_stuff.rs`). Genuinely
separated arcs (either side of a sight blocker) stay distinct, preserving the
blocker semantics the item-12 comment called out. After the fix every seam cell
`rel(k,-k)` is fully visible; the black diagonal is gone.

Tests: `test_stacked_portal_slices_union_to_full_visibility` (FOV) and
`stacked_portal_seam_has_no_out_of_sight_partial` (end-to-end render); full
suite green (536 passed / 9 skipped; 275 with `debug-tools`).

### Minimizer crop fix (separate, follow-up)

The `--no-screen-crop` workaround was itself a bug: `screen_crop_candidate`
returned an always-odd width, and an odd width's last left-glyph column has its
right half off-screen, which `draw_glyphs_straight_to_screen_square` panicked
on. Fixed by returning an even width and by clipping off-screen character
columns instead of panicking (`Screen::draw_glyph_straight_to_screen_buffer`,
`draw_string_to_screen`). The minimized state now crops to 22x13. Tests:
`drawing_an_odd_width_edge_square_clips_instead_of_panicking`,
`screen_crop_candidate_bounds_artifact_with_margin` (even-width assertion).

### Methodology post-mortem

The first "minimizer hardening" (bounding portal squares, `catch_unwind`,
late crop) was a **false fix** built on a wrong diagnosis ("the portals form a
hall of mirrors whose images reach the sight radius"). It was wrong because:

- I treated the `x: / y:` in the panic message as noise instead of checking it
  against the terminal width. `x == width` is the smoking gun for the edge bug.
- I compared two **odd** sizes (33/35 panicked, 65/67 rendered) and inferred
  "size vs content extent" without noticing all were odd. I never tested an
  even small size, which would have worked and falsified the radius theory.
- I reasoned about where content *could* be drawn (portal reflections) instead
  of reading `Screen::all_screen_squares` / the glyph-drawing path, where the
  parity assumption is explicit (`left char = x * 2`).
- The proposed fix (bound portals) did not reproduce the failure and did not
  fix it; it only made the window bigger, which is the same accidental
  workaround the old 73x11 crop relied on.

Lesson: when a renderer panics with a coordinate, check the coordinate against
the dimension it allegedly exceeds; and prefer a discriminating experiment
(small even width) over a plausible mechanism.

