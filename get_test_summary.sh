#! /usr/bin/env sh

./run_all_tests.sh > /dev/null
tac test_output.log | rg -m 1 "Summary" | sed -e 's/^[ \t]*//'
