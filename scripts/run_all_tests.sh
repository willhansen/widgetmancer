#! /usr/bin/env sh

cargo nextest run --no-fail-fast |& tee test_output.log
