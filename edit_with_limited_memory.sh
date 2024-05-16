#! /usr/bin/env sh

CMD="nvim src/utility/coordinates.rs"

systemd-run --scope -p MemoryHigh=8G -p MemoryMax=10G --user ${CMD}
