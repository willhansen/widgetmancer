# `issue_review` — plan

A standalone, terminal-only tool for reviewing a filed issue capture beside the
current render. It is deliberately decoupled from the game loop and from the
in-game report overlay.

## Purpose

Show the reviewer the three pictures that matter and let them leave a free-form
note. It does **not** decide, move, or archive anything: later processing reads
the note and acts.

## CLI

```
issue_review [<NNNN|path>] [--all] [--issues-dir DIR]
```

- No target (or `--all`) — sweep every open issue (`issues/*` with a
  `snapshot/`, skipping `solved/`), numerically sorted. **This is the default.**
- A number or path — review just that issue.
- `--issues-dir DIR` — base for discovery and numeric resolution. Defaults to
  `snapshot::issues_dir()`. It also lets the tool review a checkout elsewhere,
  and lets the integration test run against a temporary copy instead of the
  real issue files.

The repo-root `./issue-review` wrapper runs the bin through cargo (so the
binary matches the sources), forwarding all arguments.

## Display

Three panes sharing **one** crop. The crop is **dynamic**: it shows as much of
the frame as the pane fits, windowed around the flagged regions (or the frame
center when there are none) and clamped to the frame. The layout — side-by-side
or stacked — is whichever shows more of the frame. There is no hotkey to change
either.

| pane | content |
|------|---------|
| `before` | the saved capture (`screen.txt`), clean |
| `before-marked` | the capture with flagged regions tinted magenta |
| `after` | a fresh render of the **current working tree**, clean |

`before` and `after` are deliberately free of overlays so they can be compared
directly; only `before-marked` carries the region tint.

The note editor is a sub-pane at the bottom, and — on a wide terminal — the
issue's `issue.md` is shown, word-wrapped to the pane width, in a scrollable
column on the right (`PageUp`/`PageDown`).

## Control scheme

The note editor is the only interactive pane; there is no marking mode and no
pane-cycling key. The editing keys are the standard line-editor set — printable
characters, `Enter` (newline), `Backspace`, `Delete`, arrows, `Home`, `End` —
and they live in one place (`review_ui::edit_buffer`), shared with the in-game
report overlay's editor so the two cannot drift.

Meta keys:

- `Ctrl+S` — save the note and advance to the next issue.
- `Ctrl+P` — cancel/quit (the same cancel key the report overlay uses).
- `PageUp`/`PageDown` — scroll the issue document.

## Saving a note

On `Ctrl+S`, if the note buffer is empty (or the issue's `issue.md` is absent),
nothing is written — the file is left untouched. Otherwise the note is appended
to `issue.md`:

```
## Appended note (YYYY-MM-DD)

<note text>
```

A blank line precedes the header, the existing content is preserved, and the
date comes from `snapshot::today_utc()`.

## Verification

A hand-rolled pty harness (via `libc`) drives the built binary in a virtual
terminal, passing input through the pty slave (the child's stdin):

- the harness has its own tests (stdin round-trip, exit status, window size,
  environment clearing, reader EOF);
- a sweep test runs the default (no-target) mode against a temporary
  `--issues-dir` with two copied fixtures, typing a note for one and saving an
  empty buffer for the other, then asserts the first `issue.md` gained the
  appended note and the second is byte-identical;
- the alternate-screen enter/leave sequences are asserted.

## History

This supersedes an earlier interactive approve/reject design that performed the
`git mv` to `issues/solved/` itself. That direction was dropped: movement is
left to later processing, and the tool only records a note.

## Future

- **Animation.** A static `after` cannot show a motion bug (issue 0023). Play
  back a few seconds by stepping the capture's logical time (or
  `simulate_keys`) and rendering N frames. Recorded in `docs/vision/ideas.md`.
