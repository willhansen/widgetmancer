# Checkpoint — Roadmap 2: Remove globally suppressed warnings (IN PROGRESS)

Working state for [ROADMAP.md](../ROADMAP.md) item 2, in case of
interruption. **Not yet committed to ROADMAP.md** — update it when the item
completes.

## Approved decisions (from the user)

1. **Deprecated-API strategy — case by case:** migrate callers to the
   replacement API where a drop-in replacement exists; **un-deprecate with a
   TODO** where the API is still load-bearing and no adopted replacement
   exists (much of the "screen rotation" migration was started but never
   finished).
2. **Ordering:** the `game` crate's import cleanup is folded into roadmap
   item 4 (glob imports) rather than fixing imports twice. Item 2's `game`
   step is "items 2+4 together for the `game` crate".

## Baseline measurement (allows temporarily stripped, then restored)

| crate | warnings | notes |
|---|---|---|
| `utility` | 23 | 20× self-deprecated `AngleIntervalSet`, 3 misc |
| `terminal_rendering` | 133 | ~90× self-deprecated screen-rotation-era API; **no crate-root allows** (warnings were never suppressed, but count toward "warning-free workspace") |
| `game` (lib) | 241 | ~110× deprecated `terminal_rendering` API, 37× unused glob imports (item 4 overlap), unused muts/vars, must_use, snake_case |
| `game` (portal_playground bin) | 31 | same flavor |

## Progress

### Step 1: `utility` — DONE
`cargo build --tests -p utility` is warning-free; both crate-root allows
(`#![allow(dead_code)]`, `#![allow(deprecated)]`) removed from
`crates/utility/src/lib.rs`. Full suite still 470 passed / 11 skipped.

Changes made:
- `angle_interval.rs`: removed bare `#[deprecated]` from `AngleIntervalSet`
  (used only within its own file, no replacement exists).
- `angle_interval.rs`: `is_valid()` is test-only → marked `#[cfg(test)]`.
- `angle_interval.rs` tests: underscore-prefixed unused vars (`_new_arcs`,
  `_arc_extend_cw`, etc. — **careful**: some same-named vars in other tests
  ARE used; only the warned lines were renamed); removed unused imports via
  `cargo fix --tests -p utility --allow-no-vcs`.
- `lib.rs`: renamed `int_to_T` → `int_to_t` (2 internal callers, no external
  users); underscore-prefixed `_speed_at_start` with a TODO (leftover
  approach-speed derivation, delete-or-use).
- Root `Cargo.toml`: removed invalid `workspace.package.name` key.

### Step 2: `terminal_rendering` — IN PROGRESS
No crate-root allows to remove; the work is warning cleanup per decision 1.

Done so far:
- `glyph.rs`: migrated the two internal `has_no_fg()` callers to
  `!self.has_fg()` and **deleted** the deprecated `has_no_fg` method
  (drop-in replacement existed; no external callers — verified by grep).

Planned next (survey already done):
- **Un-deprecate with TODO** (load-bearing, no adopted replacement):
  - `screen.rs` lines ~446–575: `WorldCharacterSquare/Point/Step/Move`,
    `WorldSquareGlyphMap`, `WorldCharacterSquareGlyphMap`,
    `WorldCharacterSquareToCharMap`, `CharacterGridInWorldFrame`, and the
    `world_*character*` conversion fns ("Invalidated by screen rotation") —
    still used across 5 files in this crate and pervasively in `game`.
    Replace `#[deprecated(note = ...)]` with a `// TODO(screen-rotation):
    slated for replacement; migration unfinished` comment.
  - `glyph.rs:690` `pair_up_character_square_map` — load-bearing in `game`
    graphics + `braille.rs`.
  - `glyph.rs:252` `get_glyphs_for_player` ("Use ArrowDrawable instead") —
    only remaining caller is `game/tests.rs:1705`, which asserts on exact
    glyph strings; migration not drop-in → un-deprecate with TODO.
  - `floating_square.rs:239` `character_map_for_full_square_at_point` —
    only callers are its own tests (lines ~571, ~579); replacement returns
    drawables, not a char map → un-deprecate with TODO.
- **Mechanical remainder** (after deprecations): `cargo fix --lib
  -p terminal_rendering --allow-no-vcs` for unused imports/muts; then
  hand-fix: ambiguous glob re-exports (5), private-shadows-glob (3),
  snake_case consts (`fg_set`, `fg_reset`, `bg_set`, `bg_reset`,
  `default_fg_color`, `default_bg_color` — **pub API, check `game` callers
  before renaming**), unused vars, 1 must_use `Result`, never-used methods
  (`set_screen_origin_by_world_square`, `screen_max_as_world_square`,
  `set_buffered_glyph`, `regular_display_string_to_raw_display_string`,
  `combine_characters` — delete or `#[cfg(test)]`, check test usage first),
  2× function-pointer comparison, useless comparisons, escaped-newline.
- This un-deprecation pass will also eliminate ~110 of `game`'s 241
  warnings for free.

### Step 3: `game` — NOT STARTED (do jointly with item 4)
Sequence: replace glob imports with explicit ones (item 4) → `cargo fix`
for mechanical bulk → hand-fix unused vars / must_use / snake_case consts
(`CUBE_SPEED`, `CUBES_PER_SECOND`, `STEP_BACK_DISTANCE`, `pointA`, `pointB`)
→ remove `#![allow(warnings)]` AND the redundant `#![allow(non_snake_case)]`
(and the commented-out one) from `crates/game/src/lib.rs`. Don't forget
`portal_playground.rs` bin (31 warnings) and `game/tests.rs`.

### Step 4: lock-in — NOT STARTED
Add `cargo clippy` to the workflow (bacon.toml already exists); consider
`RUSTFLAGS="-D warnings"`.

## Verify after every step

```
cargo build --workspace --tests   # zero warnings outside remaining allows
cargo nextest run                 # must stay 470 passed / 11 skipped
```

Note: no git repo in /root/project — `cargo fix` needs `--allow-no-vcs`,
and there is no VCS safety net; the phase-1 measurement backups were in
/tmp (already restored).
