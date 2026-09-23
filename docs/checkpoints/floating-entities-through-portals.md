# Floating entities travel through portals — 2026-09-23 (ROADMAP item 11)

Feature: death cubes and hunter drones cross portal entrances with
position and velocity transformed (like rays already do), and a square
straddling a portal face is rendered on both sides of it. Both
`#[ignore = "TODO"]` tests (`test_hunter_drone_moves_through_portal`,
`test_hunter_drone_visually_pokes_through_a_portal_a_little_bit`) are
un-ignored and passing. Suite: 509 passed / 9 skipped.

## Model side: `PortalGeometry::portal_aware_move`

`portal_aware_move(start, movement) -> (WorldPoint, QuarterTurnsAnticlockwise, Vec<WorldLine>)`
(portal_geometry.rs) is the mover-side sibling of
`ray_to_naive_line_segments`: loop over
`first_portal_entrance_hit_by_ray` (movement → angle + range), cut the
path at the intersection, continue through
`RigidTransform::transform_ray`, accumulate the rotation, collect each
straight sub-path. Key differences from the ray path:

- **No draw-back epsilon.** Ray rendering steps 0.001 back/forward around
  a crossing so line glyphs don't straddle square borders; entity
  positions persist between ticks, so crossings must land exactly on the
  transformed intersection point.
- **Guard cap** (`MAX_PORTAL_CROSSINGS_PER_MOVE = 16`) against degenerate
  chains (an exit face lying exactly on another entrance face can produce
  zero-length crossings forever).
- **Zero movement → empty path** (`WorldLine::new` forbids degenerate
  segments). `tick_death_cubes` maps an empty path to the old naive
  behavior (kill the cube's own square).

Callers:
- `slide_floating_entity_with_portal_awareness` (realtime.rs) now returns
  `(T, Vec<WorldLine>)`, sets the end position, and rotates the mover's
  velocity by the accumulated quarter-turns. All movement funnels route
  through it: hunter drone ticks, conveyor pushes, floor-arrow pushes.
- `tick_death_cubes` routes through the slide and kills along each
  sub-path segment (Bresenham per segment), so a cube that portaled
  mid-tick kills along the bent path, not the naive straight line.
  Without portals the path is one segment with the same endpoints as the
  old code — byte-identical behavior.
- Drone `sight_direction` is deliberately NOT rotated at crossings: it
  sweeps continuously at 90°/s anyway, and its sight ray is already
  portal-aware.

## Render side: straddle remap (entrance-only rule)

Geometry that makes the rule simple and exact:

- Portal faces lie on half-integer planes = **cell borders**, and a face's
  lateral extent equals **exactly one cell** — so the part of a floating
  square beyond a portal face is always the content of the single cell
  `entrance.stepped()`.
- The rigid portal transform maps cell centers to cell centers, so the
  remap is: move that cell's drawable to
  `transform_pose(entrance.stepped()).square()` with the offset rotated
  by the portal's rotation.

`remap_floating_square_drawables_through_portals` (graphics/drawable.rs)
runs in `Graphics::draw_floating_square` after the biased 9-cell map is
built: for every registered entrance whose plane the entity straddles
(`|along| < 0.5 && lateral < 1.0`), move that one cell through the
portal. Why entrance-only is sufficient (no inverse transforms, no
per-entity transit tracking):

- **One-way portal, pre-crossing:** the beyond-face cell lands at the
  exit — the "pokes through a little bit" test.
- **One-way portal, post-crossing:** the tail stays absolute near the
  exit square. This is correct, not a shortcut: the space behind a
  one-way exit face is not a visual window (FOV and rays treat it the
  same way), so a tail sticking back over the exit square is exactly
  what observers see. It also means unrelated entities hovering at the
  exit plane can never be ghosted to the entrance.
- **Two-way / double-sided:** the reverse/back faces are themselves
  registered entrances, so tails and back-side straddles are covered by
  the same forward rule.
- Snap-family hysteresis is preserved for straight (0-rotation) portals
  so the seam between the two halves can't tear; rotated portals drop
  the forced family (same policy as `OffsetSquareDrawable::rotated`).

## Tests

- portal_geometry.rs: `portal_aware_move` crosses a straight portal
  (end position, 180° rotation, 2 segments); naive without portals;
  zero movement stays put.
- game/tests.rs: cube travels through a portal and kills along the bent
  path (rook beyond the exit captured, rook on the naive path spared,
  velocity rotated 180°); drone velocity rotates through a turning
  portal (emerges moving in the exit direction); floor arrow pushes a
  drone through a portal (push-funnel call site); straddling square
  remapped in the draw buffer (content at exit square, gone from the
  beyond-face cell); the two previously-ignored tests.

## Accepted edge cases

- An entity spawned mid-portal at a corner where two entrance faces
  meet: the first portal in iteration order wins the shared cell
  (the discrete step code has elaborate corner-coherence logic; the
  continuous path doesn't — spawn-mid-portal is already an invalid-ish
  state).
- Degenerate portal chains (exit face exactly on another entrance face):
  16-crossing cap; leftover movement in that tick is discarded.
- Float underflow (|movement| so small that `pos + movement == pos`)
  would panic in `WorldLine::new`; unreachable with in-game velocities
  (O(0.5–20)).