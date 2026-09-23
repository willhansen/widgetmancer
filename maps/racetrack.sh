#!/usr/bin/env bash
# Launch the game on the portal cube racetrack map (maps are selected via
# --map; see crates/game/src/main.rs).
set -euo pipefail
cd "$(dirname "${BASH_SOURCE[0]}")"
exec ./play-game --map racetrack