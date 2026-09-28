#!/usr/bin/env bash
# Build and run a command with gprof call-count instrumentation (opt-in).
#
# The default build deliberately omits `-Zinstrument-mcount`: putting it in
# .cargo/config.toml instruments every target, test binaries included, and
# roughly quadruples the test suite. Profiling is the only thing that needs
# it, so it lives here. Usage:
#
#   scripts/profile.sh cargo run --release -p game --example profile_racetrack -- racetrack 200
#   scripts/profile.sh cargo test -p terminal_rendering --no-run
#
# RUSTFLAGS *replaces* .cargo/config.toml's rustflags rather than appending,
# so the frame-pointer flag is repeated here.
set -euo pipefail
export RUSTFLAGS="${RUSTFLAGS:-} -Cforce-frame-pointers=yes -Zinstrument-mcount -Cpasses=ee-instrument<post-inline>"
exec "$@"
