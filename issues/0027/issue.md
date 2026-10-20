# Issue 0027

Map: cubes
Captured: 2026-10-13

## Description

Something has clearly gone very wrong here.

## Flagged regions

1. world(-4, 7)..(-3, 7) — screen(29, 30)..(30, 30)

## Investigation (2026-10-13) — wall drawn where the FOV sees void

The flagged cells `(29,30)`,`(30,30)` show a portal-tinted `▒` wall
(`fg=bg=(69,47,58)`) even though the FOV resolves both visibilities to **void**:

```
29 30 world(-4,7) rel(-18,-1) '▒' vis=2
   d0 abs(-4,7)  rel_vis="🬭🬭" db=<none>
   d1 abs(20,12) rel_vis="🬎🬎" db=<none>
30 30 world(-3,7) rel(-17,-1) '▒' vis=2
   d0 abs(-3,7)  rel_vis="🭆🬹" db=<none>
   d1 abs(20,13) rel_vis="🬎🭜" db=<none>
```

Instrumentation: the wall comes from a depth-1 portal column `abs(19,12)` /
`abs(19,13)` (frame center `[19,30]`); its z-shifted side lands on the cell whose
portal view is the **on-board gap** `abs(20,12)`/`abs(20,13)`. So a cube side is
painted where the FOV says void.

Pre-existing: the same cells render identically at `HEAD` (before the recent
0026 fixes), so this is not a regression.

Same class as the cluster's "wall projection vs height occlusion" limitation
(ROADMAP item 15 / the notes on 0020/0022/0024/0025): the forward pass gates a
z-shifted wall by the view *frame* of its own cell, but two different absolute
squares in one frame can share a screen ray. Here the column and the resolved
void square are adjacent, so the wall spills onto the void cell. Blanket-
suppressing walls that land on void cells is not viable — it removes legitimate
cliff sides (e.g. solved/0009 drops 646 cells). Needs an occlusion model, not a
frame heuristic.

## Appended note (2026-10-17)

Remains valid

## Related (2026-10-20)

The inverse of the missing-side reports 0022/0024 (there: an expected cube side
is not drawn; here: a cube side is drawn where the FOV sees void). Both come
from the forward pass gating a wall by the resolved view of the *destination*
cell instead of the wall's own occlusion, so they share ROADMAP item 17
("Portal wall projection vs height occlusion"). Filed separately because the
picture is visibly different; fixing one is not evidence for the other.
