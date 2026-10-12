# Issue 0021

Type: feature request (no snapshot)
Captured: 2026-10-11

## Description

Expand the in-game UI for *creating new issues*: when a bug is noticed, the
player should be able to mark the relevant square(s) on screen and have that
identification captured with the snapshot, so an issue description can name the
exact location(s) instead of "downward one left of the player".

The original motivation (from issue 0020) was a debug indicator for a single
world square / screen cell. This issue broadens that to an issue-reporting
workflow.

## Requirements (settled 2026-10-11)

- **Report overlay**: a toggled in-game mode that freezes the game, lets the
  player flag squares, and files the issue.
- **Embedded external editor**: the user's `$VISUAL`/`$EDITOR` runs in its own
  pseudo-terminal, in a fixed region of the screen, so the overlay stays
  visible. The editor edits a plain empty file; the issue template (header,
  flagged-square section) is wrapped around the text *after* entry, for ease of
  initial typing.
- **Focus swap**: swap between marking the game screen and typing in the editor.
- **Square selection**: both mouse (click / drag a box) and keyboard (cursor +
  flag key).
- **Per-square capture**: world and screen coordinates of each flagged region.
  (Revised 2026-10-11: the auto `explain`-style summary — glyph, FOV
  visibilities, portal depth — was dropped; it is cheap to re-derive while
  investigating, so baking it into every filed issue was redundant.)
- **Un-altered screen saved**: `snapshot/screen.txt` stays the pure game render
  (never baked with the overlay/markers), so `verify-issues` is unaffected.
- **Gating**: behind `debug-tools`, enabled by default.
- **Key**: the same key opens and cancels the overlay.
- **Layout**: fixed, derived from the terminal aspect ratio.
- **Deps**: `portable-pty` + `vt100` are acceptable.

## Plan

1. Overlay + mouse/keyboard flagging + summary + template wrapping + `flags.json`
   and un-altered `screen.txt`, still using the full-screen external editor.
2. Embedded editor pane (`report` feature, PTY + `vt100`, ratio layout, focus
   swap, input routing).
3. Quick in-game line editor, keybinding polish, docs.

## Status

- Phase 1 done (2026-10-11): overlay, flagging, summary, filing.
- Phase 2 was the embedded PTY editor pane; it was **removed** (2026-10-11) for
  severe lag and flicker (full-pane blit every frame).
- Phase 3 (2026-10-11): the note is now typed in a small built-in text editor
  in the overlay (`Tab` swaps focus), and the overlay repaints only when its
  state changes. Docs updated.
- Phase 4 (2026-10-11): reworked around the reporter's static-frame model. On
  `Ctrl+P` the rendered frame is captured and repositioned into the game region
  (so the player is centered there) and mouse input maps back through the
  recorded offset. Selection is now one numbered region per click/drag (a click
  is a 1x1 region), tinted magenta so the underlying glyph shows through, with
  numbered labels. Drawing is cell-diffed against the previous overlay and mouse
  motion is coalesced, so typing and dragging no longer flicker or lag.
- Phase 5 (2026-10-11): follow-up polish from reporter feedback. `Esc` no longer
  cancels the issue (it leaves the editor to marking mode, vim-style; `Ctrl+P`
  remains the cancel). Filed issues now record only each region's world/screen
  bounds — the per-square `explain` summaries were dropped as redundant.
