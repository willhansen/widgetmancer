Each issue is a directory or a single markdown file. If extra files need to be added to an issue that is only a file, move it to a directory sharing the issue name, and rename the issue's main markdown file to 'issue.md'.

This directory only represents existing and up to date issues. If an issue is no longer relevant, it is to be updated to relevancy or moved to the `solved/` subdirectory. Solved issues are never deleted and their numbers are never reused, so `solved/` is part of the record: new issues are numbered one past the highest number in `issues/` and `issues/solved/` combined. Their history also remains in git.

## Resolving an issue: show before/after, get approval

Rendering bugs are hard to judge from a diff, and a fix is only real when the
reporter agrees the *picture* is right. The standalone `issue_review` tool
presents a candidate fix as a before / before-marked / after triple and asks
whether to move the issue to `issues/solved/`:

```
cargo run -p game --features debug-tools --bin issue_review -- 0027
```

It renders the **current working tree** as the "after" picture (so the fix must
be in the tree), reads the capture's `screen.txt` as "before", and outlines the
flagged regions from `flags.json` in magenta with the player in cyan. All three
panes share one crop: the union of the flagged regions and the player, plus a
margin. Press:

- `y` to approve — the original capture is preserved as `screen.pre-fix.txt`,
  the current render is blessed into `screen.txt`, and the issue is `git mv`d to
  `issues/solved/`. A render identical to the pre-fix capture still moves, but
  warns that the fixture is vacuous (no picture change).
- `n` to reject — a built-in editor opens; the typed text is appended as a
  `## Clarification` section of `issue.md`, and the issue stays open.
- `q` / `Esc` to cancel.

Options: `--flags FILE` (default `<issue>/flags.json`; a missing file means no
flagged regions), `--margin N` (crop padding in squares, default 4), and
`--no-crop` (show the whole capture). `scripts/issue_before_after.py` remains
for offline PNG/HTML review of open and solved captures.

