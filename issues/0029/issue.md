# Issue 0029

Map: cubes
Captured: 2026-10-20 (consolidates 0020, 0022, 0024)

## Description

Direct cube columns are shadowed by portals: where a portal's depth-1 sub-view
resolves a cell, the directly-visible cube (top or side) is lost, or the side
the reporter expects never lands where they read the cube's edge.

This is the umbrella for the three earlier captures that all describe the same
"the cube is right there but I can't see it" complaint:

| issue | capture | flagged cells | class |
|---|---|---|---|
| 0020 | `issues/0020` | screen `(51,34)`, `(51,35)` | **A** compositor |
| 0022 | `issues/0022` | screen `(46..48, 46..49)` | **B** wall dropped |
| 0024 | `issues/0024` | screen `(47..50, 40..44)` | **B** wall dropped |

Those directories stay in `issues/` as evidence. They are *not* moved to
`solved/`: `verify-issues` requires a matching `screen.pre-fix.txt` and a
blessed `screen.txt`, which would freeze their still-buggy renders as "fixed"
fixtures. Fix 0029, then re-bless or archive each capture on its own merits.

Minimized repro: `issues/0029/snapshot` (from 0020 screen `(51,35)`, via
`snapshot_tool minimize`; 50 renders, frame cropped to 17x12). The artifact is
at screen `(4,12)`; the capture renders byte-identically to the current tree.

## The three classes (measured on the current tree)

The FOV model is not at fault: `snapshot_tool visibility-pairs` shows the direct
and portal half-planes are **exact complements** on every capture in the cluster
(0020 115/116, 0022 121/121, 0024 112/112, 0025 112/112, 0026 172/172), so the
half-planes partition each cell correctly and `combined_*` is never applied
across depths.

### A — the compositor keeps one glyph (0020)

```
screen(4,12) in 0029, world(4,8) rel(-4,1)
  d0 abs(4,8)  rel_vis="🬭🬭" db=solid   (direct cube top, partially visible)
  d1 abs(19,20) rel_vis="🬎🬎" db=solid  (portal sub-view, exact complement)
  final: '█' fg=(88,68,83)              (portal glyph on top)
```

`drawable_at_relative_square` composites the stack with
`PartialVisibilityDrawable::drawn_over`: the deeper (portal) view owns the
glyph and the shallower (direct) half is reduced to a flat colour
(`color_if_backgroundified`). A terminal cell carries one glyph, so the direct
cube's shape/edge cannot appear next to the portal content. ROADMAP item 15.

Flipping the draw order (shallower-on-top) was tried and breaks all 18 portal
fixtures, so the fix has to be sub-cell compositing, not ordering.

### B — the direct column's side is dropped (0022, 0024)

Every flagged cell in both captures resolves to depth-0 **void**
(`d0 abs_vis="  " db=<none>`) and the cube side is drawn one screen square away
by the raised-terrain forward pass, or not at all. The pass gates each column's
z-shifted wall by the *view frame of the destination cell*, so a cell whose
resolved view is not the wall's frame gets no side; relaxing the gate re-adds
walls that the `solved/0008`/`0017` fixes classified as spurious: in the
2026-10-12 experiment that relaxed the gate, fixture 0007 grew by 37 wall
cells, 0008 by 60, 0012 by 49, 0017 by 78 and 0009 by 19. So it needs a real
occlusion model rather than a looser gate.

Note (0024, reporter 2026-10-17): "the highlighted region doesn't look quite
right. Possible the highlighted region itself has rendering issues." Treat the
flagged box as approximate; the minimized 0029 repro is the ground truth for
class A, and 0022/0024 remain the ground truth for class B.

### C — a side is painted over void (tracked separately: 0027)

The mirror image of class B shows up in `issues/0027` (`(29,30)`, `(30,30)`):
both views resolve the cell to void, yet a red-tinted `▒` side from the
neighbouring depth-1 column is painted there. It *looks* different (extra
geometry rather than missing geometry) and is filed as its own issue; the two
share ROADMAP item 17 because a correct occlusion model must satisfy both
polarities at once.

## What is *not* the cause

- The portal aperture arc / "entrance face on the wrong side" lead from 0020:
  refuted, the arc is the aperture the entrance direction describes.
- The FOV merge (`combined_with` / unioning arcs): refuted, the halves are
  complementary.
- Portal tint on the shadow half: fixed (0025); no remaining cell in the cluster
  has a tinted `OUT_OF_SIGHT` background.

## Next steps

1. **Class A** — sub-cell compositing (ROADMAP 15): represent the complementary
   pair with both contents clipped to their half-planes, commit to per-character
   compositing if one glyph cannot carry both. Verify against the 0029 minimized
   repro first, then re-bless the `cubes` fixtures.
2. **Class B** — occlusion model for the forward pass (ROADMAP 17): decide each
   wall voxel's screen cell from the wall's own geometry (is any part of it
   visible along that ray?), not from the destination cell's resolved frame.
   Must not lose the legit `solved/0009` cliff sides.
3. Re-run `./snapshot-tool verify-issues`, `visibility-pairs`, and
   `./issue-review 0020 0022 0024` after each step; archive each capture once its
   own report reads fixed.
