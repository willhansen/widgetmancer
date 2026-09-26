# Portal-depth partial-visibility artifact

Status: open — blocked on missing headless FOV-inspection tooling.
Captured from: `snapshot/` (map `hallways`), player at world (37,44) facing east.

## Symptom

Looking east from the player, four player glyphs render (portal recursion depths
0–3) at relative squares (0,0),(5,0),(10,0),(15,0). Directly above the
**rightmost (depth-3) player**, at relative square (15,1), a black partial block
renders that should not be there.

## Observed cells (from `snapshot/screen.txt`, parsed)

Terminal 141x78; screen-buffer center (34,38); player buffer square (34,38).

| buffer square | relative | char | fg | bg | meaning |
|---|---|---|---|---|---|
| (34,37) | (0,1)  | ' '   | (255,255,255) | (127,127,127) | floor GREY, depth 0 |
| (39,37) | (5,1)  | ' '   | (255,255,255) | (140,114,114) | floor GREY tinted 0.1 |
| (44,37) | (10,1) | ' '   | (255,255,255) | (153,102,102) | floor GREY tinted 0.2 |
| (49,37) | (15,1) | 🬭🬭 | (165,89,89)   | (77,0,0)      | **artifact**, depth 3 |

Decoding:
- bg `(77,0,0)` = `OUT_OF_SIGHT_COLOR` (BLACK) tinted RED at strength 0.3
  (`0.1 * portal_depth`, fov_stuff.rs:760). So this is a
  `PartialVisibilityDrawable` (drawable.rs:87) shadow background.
- fg `(165,89,89)` = floor GREY (127,127,127) tinted 0.3 → the underlying
  drawable is the plain floor of absolute square (37,45).
- Depths 0–2 render absolute (37,45) as full floor; only depth 3 marks it
  partially visible.

Absolute square (37,45) is the portal exit square directly above the player
(portals at y=44,45,46 between x=37 and x=41). Relative (15,1) at depth 3 maps
to it under the accumulated portal transform.

## Where to look

- `crates/game/src/fov_stuff.rs`
  - `field_of_view_within_arc_in_single_octant` (~:831, portal recursion at
    :950) — no explicit portal-depth cap.
  - `PositionedSquareVisibilityInFov::one_portal_deeper` (:236).
  - `visibilities_of_relative_square_in_one_sub_view` (:620),
    `visibilities_of_relative_square` (:648).
  - `drawable_at_relative_square` (:696), partial wrap (:745), tint (:760).
  - `visibility_of_offset_square` / `square_visibility_from_one_view_arc_with_center_offset`
    (:1100–:1173) — where the half-plane shadow is produced.
- `crates/game/src/graphics.rs` — `load_screen_buffer_from_fov` (:253),
  `draw_player` (:306), `tint_portals`/`render_portals_with_line_of_sight` (:77).
- `crates/game/src/game/mod.rs` — `player_field_of_view` (:1371),
  `update_screen_from_draw_buffer` (:580).
- `crates/game/src/graphics/drawable.rs` — `PartialVisibilityDrawable` (:87).
- `crates/terminal_rendering/src/glyph_constants.rs` — `OUT_OF_SIGHT_COLOR`
  (:31), `BOARD_BLACK = GREY` (:28), `RED` (:8).

## Hypothesis

The per-crossing portal recursion re-derives the view arc / virtual center each
level (`transform.transform_arc(view_arc.intersection(portal_view_arc))` and
`transformed_center`), and there is no depth cap. The last reachable depth
(3 here) appears to accumulate a narrower/incorrect shadow half-plane for a
square that depths 0–2 treat as fully visible, yielding a spurious
`OUT_OF_SIGHT`-colored partial block. Needs confirmation (see tooling gap).

## Missing tooling (blocker)

No supported way to dump FOV internals for a loaded snapshot. Needed:

1. A headless entry point to load `snapshot/` and obtain `player_field_of_view()`
   (`load_snapshot_game` is `pub(crate)`, snapshot.rs:547; the FOV method is
   private, game/mod.rs:1371; snapshot.rs tests are in-crate so this is
   reachable from a test module).
2. A dump of, for a given relative square, every `PositionedSquareVisibilityInFov`:
   absolute square, `portal_depth`, rotation, and
   `square_visibility_in_absolute_frame()` / `_relative_frame()` `as_string()`.
3. Ideally the per-level `view_arc`, `transformed_center`, and portal transform
   so the arc accumulation can be traced across depths.

## Resume plan (once tooling exists)

1. Reproduce: load `snapshot/`, render headless, confirm (49,37) artifact.
2. Dump FOV visibilities for relative (5,1), (10,1), (15,1) and compare.
3. Identify whether depth 3 differs due to arc accumulation, frame/rotation
   handling in `square_visibility_in_relative_frame`, or depth aliasing.
4. Fix at the source (candidate: explicit portal-depth cap and/or preserving the
   containing arc on intersection instead of accumulating shrink).
5. Regression test on the snapshot asserting no `OUT_OF_SIGHT`-background partial
   appears at (15,1); run `cargo nextest run`.
