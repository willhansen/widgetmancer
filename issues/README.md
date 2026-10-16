Each issue is a directory or a single markdown file. If extra files need to be added to an issue that is only a file, move it to a directory sharing the issue name, and rename the issue's main markdown file to 'issue.md'.

This directory only represents existing and up to date issues. If an issue is no longer relevant, it is to be updated to relevancy or moved to the `solved/` subdirectory. Solved issues are never deleted and their numbers are never reused, so `solved/` is part of the record: new issues are numbered one past the highest number in `issues/` and `issues/solved/` combined. Their history also remains in git.

## Reviewing an issue: see the pictures, leave a note

Rendering bugs are hard to judge from a diff. The standalone `issue_review`
tool shows the three pictures that matter beside a note editor; it does not
decide or move anything, it just records a free-form note for later processing.

```
./issue-review --all
```

With no issue (or `--all`) it sweeps every open issue in turn; a number or path
reviews just that one:

```
./issue-review 0027
```

It renders the **current working tree** as the "after" picture, reads the
capture's `screen.txt` as "before", and outlines the flagged regions from
`flags.json` in magenta with the player in cyan. All three panes share one
crop: the union of the flagged regions and the player, plus a margin. On a wide
terminal the issue's `issue.md` is shown word-wrapped in a scrollable column on
the right (`PageUp`/`PageDown`).

Your note is the only input; the editing keys are the standard line-editor ones
(characters, `Enter`, `Backspace`/`Delete`, arrows, `Home`/`End`), shared with
the in-game report overlay. Two meta keys:

- `Ctrl+S` — append the note to `issue.md` and go to the next issue. An empty
  note is skipped (the file is left untouched).
- `Ctrl+P` — cancel.

A saved note is appended as:

```
## Appended note (YYYY-MM-DD)

<your text>
```

Options: `--issues-dir DIR` (base for discovery; default `issues/`),
`--margin N` (crop padding in squares, default 4), `--no-crop` (whole capture).
`scripts/issue_before_after.py` remains for offline PNG/HTML review of open and
solved captures.

