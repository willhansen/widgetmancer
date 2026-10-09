# AGENTS.md

## Commit messages are ephemeral — record them in `docs/CHANGELOG.md`

Work in this sandbox is committed to a scratch git repo
(`GIT_DIR=/sandbox-git`) that dies with the container and has no remotes.
Commit messages therefore never reach the real repository.

To preserve the narrative, **append an entry to [`docs/CHANGELOG.md`](docs/CHANGELOG.md)
for every commit you make**, using the same subject and body you would put in
the commit. Newest first, under a dated heading. Do this in the same commit as
the change (or immediately after), so the changelog and the work stay in sync.

## Debugging discipline

Snapshots and `snapshot_tool` exist to answer "why did this happen?". Work in
this order, and don't skip to reading gameplay code:

1. **Data before code.** Start from the failure's own data and check each
   value against its bound. For an `issues/` capture, parse `game_state.json`
   (board size, player position, `floor_cells`, terrain, nearby entities) and
   inspect it with `snapshot_tool` (`cells`, `heights`, `explain X Y`). Never
   `Read`/`cat` `screen.txt` — it is dense ANSI.
2. **Reconnoiter.** `git diff` and `docs/CHANGELOG.md` show what changed
   recently (the tree is often mid-refactor); find which blessed fixtures a
   fix will touch (`snapshot/`, via `snapshot_tool diff snapshot/`) before
   proposing the fix.
3. **One falsifiable invariant.** Most bugs are a broken promise between
   model and render (e.g. the terrain slab matching the board size). State
   the invariant and the smallest case that violates it; a fix must
   reproduce the failure and make it disappear, or it is a workaround.
4. **Group the reports.** Numbered captures sharing a structure usually share
   one root cause; "can't do X, unclear why" often means model and render
   disagree.

## Tooling: always run `snapshot_tool` through cargo

Run it via the repo-root `./snapshot-tool ...` wrapper (which does
`cargo run -p game --features debug-tools --bin snapshot_tool --`). **Never
invoke `target/**/snapshot_tool` directly** and never trust one whose mtime
predates a source edit: a stale binary silently reports wrong diffs (this has
happened). If you must call the binary, first assert it is newer than
`crates/game/src` (`ls -la --time-style=full-iso`).

## Regression tests must fail on the pre-fix revision

A test that passes before the fix proves nothing. For every regression test,
verify it **fails against the parent revision** — e.g.
`git stash push -- <fix files>` (keeping the test staged/unstaged), run it, see
it fail, then `git stash pop`. Say in the commit body that the test was shown to
fail pre-fix. For a bug captured under `issues/`, prefer the two-sided fixture
check (`./snapshot-tool verify-issues`): the render must match the blessed
`screen.txt` and differ from `screen.pre-fix.txt`.

