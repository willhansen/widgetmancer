#!/usr/bin/env bash
# Visually debug floating square rendering (sub-square character snapping).
# Renders the same glyph picks the game uses for OffsetSquareDrawable.
# Assumes cargo is on PATH (i.e. you're already in the nix dev shell).
#   ./scripts/debug-floating-squares.sh            single square at default pos
#   ./scripts/debug-floating-squares.sh pos 1.3 0.7
#   ./scripts/debug-floating-squares.sh sweep      table of x/y offsets
#   ./scripts/debug-floating-squares.sh animate    orbiting square (q quits)
set -euo pipefail

cd "$(dirname "${BASH_SOURCE[0]}")/.."
exec cargo run --quiet -p terminal_rendering --bin floating_square_debug -- "$@"
