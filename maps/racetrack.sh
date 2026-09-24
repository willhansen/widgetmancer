#!/usr/bin/env bash
# Launch the game on the portal cube racetrack map (maps are selected via
# --map; see crates/game/src/main.rs).
set -euo pipefail
exec "$(dirname "${BASH_SOURCE[0]}")/../play-game" --map racetrack