#! /usr/bin/env bash

# TODO: make this graph actually useful

set -o errexit pipefail

dotfile="/tmp/reqgraph.gv"

echo "digraph {" > $dotfile

cargo check \
  |& rg 'required for.*to implement' \
  | sd '.*`(.*)`.*`(.*)`.*' '"$1" -> "$2"' \
  | sd '::' '::\n\t' \
  >> $dotfile 

echo "}" >> $dotfile

nop $dotfile > "/tmp/pretty_dotfile.gv"

output="/tmp/reqs.png"

dot -T png -K dot $dotfile -o $output

xdg-open $output
