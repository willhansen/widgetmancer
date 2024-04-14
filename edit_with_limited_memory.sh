#! /usr/bin/env sh

systemd-run --scope -p MemoryHigh=8G -p MemoryMax=10G --user hx src/utility/coordinates.rs
