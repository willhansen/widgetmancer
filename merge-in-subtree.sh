#!/usr/bin/env bash
set -euo pipefail
cd "$(git rev-parse --show-toplevel)"

SUBPATH="crates/terminal_rendering"
REMOTE_NAME="terminal_rendering"

# Sanity checks
git diff --quiet && git diff --cached --quiet || { echo "Commit or stash your changes first."; exit 1; }
[ -f .gitmodules ] || { echo ".gitmodules not found — wrong repo?"; exit 1; }

# 1. Make sure the submodule is checked out
git submodule update --init "$SUBPATH"

# 2. Record the submodule's currently checked-out branch and commit
BRANCH=$(git -C "$SUBPATH" symbolic-ref --short HEAD 2>/dev/null || true)
COMMIT=$(git -C "$SUBPATH" rev-parse HEAD)
[ -n "$BRANCH" ] && echo "Using submodule branch: $BRANCH" || echo "Submodule is on detached HEAD: $COMMIT"

# 3. Fetch the submodule's history from the local checkout (includes unpushed commits)
git remote add "$REMOTE_NAME" "./$SUBPATH" 2>/dev/null || git remote set-url "$REMOTE_NAME" "./$SUBPATH"
git fetch "$REMOTE_NAME"

# 4. Remove the submodule registration (keeps history fetch intact)
git submodule deinit -f "$SUBPATH"
git rm -f "$SUBPATH"
rm -rf ".git/modules/$SUBPATH"
git commit -m "Remove $REMOTE_NAME submodule"

# 5. Merge the submodule's history into the same path
git merge -s ours --no-commit --allow-unrelated-histories "$COMMIT"
git read-tree --prefix="$SUBPATH/" -u "$COMMIT"
git commit -m "Merge $REMOTE_NAME history into $SUBPATH"

# 6. Clean up
git remote remove "$REMOTE_NAME"

echo "Done. History preserved under: git log --follow $SUBPATH/"
