#! /usr/bin/env bash

# CMD="nvim src/utility/geometry/coordinates.rs"

systemd-run --user --scope -p MemoryHigh=8G -p MemoryMax=10G $@
