# Solved issues

Resolved issues are moved here instead of being deleted. Keeping them preserves
the record that their numbers were assigned, so `create_new_issue` (which scans
both this directory and its parent) never hands out a number twice.

Move a solved issue directory here with `git mv issues/NNNN issues/solved/NNNN`
(or `mv` for a descriptive issue). Do not delete from here; if an entry must go,
it is still recoverable from git history, but its number stays reserved.