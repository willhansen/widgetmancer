cargo test --no-run 2>&1 | rg Executable | sed -E 's/.*\((.*)\)$/\1/'
