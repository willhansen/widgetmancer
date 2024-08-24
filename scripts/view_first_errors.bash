#! /usr/bin/env bash

set -o pipefail -o errexit -o nounset -o xtrace

# RUSTFLAGS=-Awarnings cargo check --color always 2>&1 | head -n"$(tput lines)"
RUSTFLAGS="-Zmacro-backtrace" cargo +nightly lcheck
