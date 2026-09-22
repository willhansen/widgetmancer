# Floating square debug tool moved to its own top-level directory

**Status: COMPLETE.** Workspace checks pass; terminal_rendering tests pass.

## User request

Move the floating square debug tool into a new top-level directory
containing the run script, a readme, and the tool's code.

## What was built

- New workspace member `floating-square-debug/` (package
  `floating_square_debug`, bin name unchanged):
  - `src/main.rs` — moved verbatim from
    `crates/terminal_rendering/src/bin/floating_square_debug.rs`; only
    the run-instructions header changed.
  - `Cargo.toml` — terminal_rendering (workspace), rgb, euclid, termion.
    euclid had to be declared explicitly: in-package bins share their
    package's `[dependencies]`, external crates do not. (Applies to any
    future bin moved out of a package.)
  - `README.md` — what the tool is, run modes, layout, doc pointers.
  - `debug-floating-squares` — moved from repo root; now runs
    `cargo run -p floating_square_debug`. Usage-comment paths updated.
- Root `Cargo.toml`: members now `["crates/*", "floating-square-debug"]`.
- The shared oracle (`coverage.rs`, `family_map.rs`, `family_map_table.rs`)
  stays in `terminal_rendering`: the coherence tests there use it, and
  moving it would create a circular dependency. No visibility changes
  were needed — the bin only ever saw the lib's public API
  (`#[doc(hidden)] pub mod coverage` etc.), which an external crate sees
  identically. lib.rs's comment updated ("bin" → "tool").
- Living docs updated: `docs/FLOATING_BLOCKS.md` wrapper + command.
  Historical checkpoint docs and proposals/patches intentionally keep
  the old paths (they record past states).

## Verification

- `cargo check -p floating_square_debug` — clean (one pre-existing
  `unused variable: lr` warning, unchanged from the old bin).
- `cargo check --workspace --all-targets` — passes.
- `cargo nextest run -p terminal_rendering` — 150 passed, 1 skipped;
  nothing referenced the bin.
- End-to-end through the moved script: `pos 1.3 0.7`, `sweep`, `glyphs`
  all exit 0 with expected output (diagnostics, boundary map, glyph
  table).
