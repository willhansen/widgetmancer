# Debug outputs for `black-block-deep-in-portal`

Generated from `../snapshot/` with the `debug-tools`-gated `snapshot_tool`.

**Status: fixed.** The portal-depth partial-visibility artifact no longer
renders. The artifact cell (buffer square `(49,37)`, player-relative `(15,1)`)
is now a fully-visible tinted floor (`bg(165,89,89)`) instead of an
`OUT_OF_SIGHT` black partial (`bg(77,0,0)`), and the minimizer can no longer
reproduce it (the predicate fails, by design). See
`pre-fix/` for the outputs captured while the bug was live.

Root cause: sub-FOVs reaching the same transformed root through adjacent
portal-face slices were merged by combining their per-square half-planes
(`combined_increasing_visibility`), which cannot represent the union of two
non-complementary partials; the uncovered part rendered as black. Fix: merge
those sub-FOVs at the **arc** level (`FieldOfViewResult::view_arc`,
`combined_with_unioning_arcs`) and recompute affected squares under the unioned
cone.

Regenerate the analysis outputs (from the repo root):

```sh
cargo build -p game --features debug-tools --bin snapshot_tool
D=issues/black-block-deep-in-portal/snapshot
O=issues/black-block-deep-in-portal/debug
B=./target/debug/snapshot_tool
$B render "$D"           > "$O/render.txt"
$B diff   "$D"           > "$O/diff.txt"
$B cells  "$D"           > "$O/cells.txt"
$B fov-trace "$D"        > "$O/fov-trace.txt"
$B fov-trace-json "$D"   > "$O/fov-trace.json"
$B explain "$D" 49 37    > "$O/explain-49-37.txt"
$B invariants "$D"       > "$O/invariants.txt"
```

## Files (current, post-fix)

| File | Command | What it is |
|---|---|---|
| `render.txt` | `render` | ANSI render at the captured time (artifact gone) |
| `diff.txt` | `diff` | per-cell diff vs the checked-in `screen.txt` (the capture predates the fix, so the artifact cells and the death-cube wall-clock skew differ) |
| `cells.txt` | `cells` | the reference grid as plain characters |
| `fov-trace.txt` / `fov-trace.json` | `fov-trace[-json]` | portal-recursion trace |
| `explain-49-37.txt` | `explain 49 37` | provenance of the formerly-artifact cell: now `abs_vis "  "` (fully visible) |
| `invariants.txt` | `invariants` | FOV visibility-consistency violations (heuristic; no longer includes `abs(37,45)`) |
| `minimize-summary.txt` | `minimize` | now errors: `no partially-visible FOV square at buffer (49,37)` — the artifact is fixed |

## `pre-fix/` (captured while the bug was live)

The minimizer outputs that reduced the live artifact to a minimal repro:

| File | What it is |
|---|---|
| `pre-fix/minimized.json` | minimal repro: player + 2 portals, 73×11 |
| `pre-fix/minimized-keep-portals.json` | same without portal minimization (21 portals) |
| `pre-fix/minimize-review*.txt` | screen-by-screen reduction transcripts |
| `pre-fix/minimized/`, `pre-fix/minimized-nocrop/` | materialized minimized snapshot dirs |
| `pre-fix/render-minimized*.txt`, `cells-minimized.txt`, `explain-minimized.txt`, `diff-minimized.txt` | renders/provenance of the pre-fix minimized state |
