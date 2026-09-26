# AGENTS.md

## Commit messages are ephemeral — record them in `docs/CHANGELOG.md`

Work in this sandbox is committed to a scratch git repo
(`GIT_DIR=/sandbox-git`) that dies with the container and has no remotes.
Commit messages therefore never reach the real repository.

To preserve the narrative, **append an entry to [`docs/CHANGELOG.md`](docs/CHANGELOG.md)
for every commit you make**, using the same subject and body you would put in
the commit. Newest first, under a dated heading. Do this in the same commit as
the change (or immediately after), so the changelog and the work stay in sync.
