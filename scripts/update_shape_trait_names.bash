#! /usr/bin/env bash

set -o pipefail -o errexit -o nounset -o xtrace

cd src/utility/geometry

rg "\b\w+Ops" -o --no-filename | uniq | tee /tmp/original_names.txt |sed -E "s/([A-Z][a-z]*)/\l\1_/g"|sed -E "s/_ops_$/::Operations/" | tee /tmp/new_names.txt > /dev/null

paste -d" " /tmp/original_names.txt /tmp/new_names.txt | xargs -n2 bash -c 'sed -Ei "s/$0/$1/g" $(find . -type f -name "*")' 
