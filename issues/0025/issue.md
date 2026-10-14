# Issue 0025

Map: cubes
Captured: 2026-10-12

## Description

At these locations, the portal edge goes through these squares,
but the portal tint appears to be applied outside the portal.

## Root cause (2026-10-12)

Confirmed to the line. The red is the portal-depth tint applied to the
`OUT_OF_SIGHT` **shadow** half of a partially-visible portal cell.

`screen-explain issues/0025/snapshot --region 44 39 44 39`:

```
44 39 world(40,23) rel(8,-8) char='🭞' fg=(88,68,83) bg=(26,0,0) vis=2
  d0 abs(40,23) rot=0 abs_vis="🭥🭓" rel_vis="🭥🭓" db=<none>
  d1 abs(21,11) rot=3 abs_vis="🭞🭚" rel_vis="🭍🬿" db=solid
```

`bg(26,0,0)` is `OUT_OF_SIGHT_COLOR` (black) tinted 0.1 toward RED. The chain:

- `fov_stuff.rs:900-906` wraps the depth-1 drawable in
  `PartialVisibilityDrawable` and calls `.tinted(RED, 0.1 * depth)`; the tint is
  keyed on `portal_depth()` only, never on how much of the cell the portal
  covers.
- `PartialVisibilityDrawable::tinted` (`graphics/drawable.rs:232-239`) tints
  **both** `fg_color` and `bg_color`, and `bg_color` is the
  `OUT_OF_SIGHT_COLOR` set in `from_partially_visible_drawable`
  (`drawable.rs:180`). So the shadow (non-visible) half is painted dark red,
  exactly "outside the portal".

`snapshot_tool invariants` reports 46 violations here.

**Fix (a) applied (2026-10-12):** in
`PartialVisibilityDrawable::tinted`, `bg_color` (the shadow background) is left
un-tinted and only `fg_color` is tinted, so the portal red stays inside the
visible portion instead of washing the shadow half. Regression test
`tinting_a_partial_leaves_its_shadow_background_untinted` (shown to fail
pre-fix); the affected portal fixtures were re-blessed. The flagged cell above
now renders `bg=(0,0,0)`.

The remaining cluster defect (the portal sub-view owning cells at its aperture
edge) is tracked as the **leading hypothesis for fix (b)** in 0022/0024/0026.

## Flagged regions

1. world(40, 23)..(40, 23) — screen(44, 39)..(44, 39)
2. world(41, 22)..(41, 22) — screen(43, 40)..(43, 40)
3. world(42, 21)..(42, 21) — screen(42, 41)..(42, 41)
4. world(43, 20)..(43, 20) — screen(41, 42)..(41, 42)
5. world(44, 19)..(44, 19) — screen(40, 43)..(40, 43)

## Update (2026-10-12) — leading hypothesis refuted; over-painting bug fixed

The **coverage-aware FOV merge / nearest-view ownership** hypothesis above is
**refuted**. `snapshot_tool visibility-pairs` (new: `Game`'s
`fov_visibility_pairs_report`, wired into `bin/snapshot_tool.rs`) checks every
relative cell with more than one visibility and finds the direct and portal
half-planes are **exact complements**:

| capture | multi-visibility cells | exact complements |
|---|---|---|
| 0020 | 116 | 115 |
| 0022 | 121 | 121 |
| 0024 | 112 | 112 |
| 0025 | 112 | 112 |
| 0026 | 172 | 172 |

So the FOV already partitions each cell correctly; `combined_*` is never involved
across depths, and one `SquareVisibility` cannot carry two contents anyway.

The confirmed defect is the terrain forward pass's wall-frame gate
(`graphics.rs::load_screen_buffer_from_terrain`): it compared the **deepest**
visibility's frame, but the flat composite paints the topmost visibility *that
has a drawable*. When the deepest view is a portal view of void (no drawable), a
portal column could paint its wall over a directly-drawn cube. Fixed by also
dropping a wall whose own cell resolves to a void view while a different frame
paints it (suppression-only); regression test
`portal_void_wall_does_not_overpaint_a_direct_cube` (shown to fail pre-fix).
Re-blesses portal fixtures 0007/0008/0010-0014/0016/0017.

Fix (a) already stopped tinting the `OUT_OF_SIGHT` shadow half. The remaining
flag is the portal sub-view's *geometry* (`d1` solid) reaching cells the reporter
reads as outside the portal; the over-painting fix touches two nearby cells only.
Issue stays open.

## Investigation (2026-10-12) — the remainder is a model-level limitation

`SNAP_DEBUG_SKIP` instrumentation of the forward pass, plus
`visibility-pairs`, show the remaining symptom is not a wrong frame-gate
decision but one of two model limitations:

1. **Complementary-pair compositing.** Where a direct view and a portal view
   split a cell into complementary half-planes, `drawable_at_relative_square`
   draws the deeper (portal) drawable's glyph and reduces the shallower (direct)
   half to a flat colour. One terminal cell carries one glyph, so the direct
   cube's shape/edge cannot appear beside the portal content. (ROADMAP item 15.)
2. **Wall projection vs height occlusion.** The forward pass gates each
   column's z-shifted wall by the view frame of its own cell. Relaxing it so a
   portal column may draw its wall over a void cell re-adds walls that the
   0008/0017 fixes deemed spurious across seven fixtures (0007 +37, 0008 +60,
   0012 +49, 0017 +78, 0009 +19 …), so it is not safe without a real occlusion
   model.

Applied here: the depth-1 portal sub-view's *solid* content reaches the flagged
cells and the direct/portal halves are exact complements, so the model is
correct; the perceived "tint outside the portal" is limitation 1 / aperture
appearance.
