#!/usr/bin/env bash
# Launch the game on the space-cubes map (the recreated cube_iso demo scene).
# Maps are selected via --map; see crates/game/src/main.rs.
set -euo pipefail
exec "$(dirname "${BASH_SOURCE[0]}")/../play-game" --map space-cubes "$@"
