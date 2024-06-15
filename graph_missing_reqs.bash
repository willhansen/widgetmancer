#! /usr/bin/env bash

setopt -e

dotfile="/tmp/dotfile.dot"

echo "digraph {" > $dotfile

cargo check |& rg 'required for.*to implement' | sd -p '.*`(.*)`.*`(.*)`.*' '$1 -> $2' >> $dotfile 

echo "}" >> $dotfile
