# Widgetmancer

A roguelike in rust featuring portals.

Run with `cargo run --release`

run tests with `cargo nextest run`

## Gameplay

https://github.com/willhansen/rust_roguelike/assets/2918280/4b05359b-7560-4e56-97ae-ad4b993ee7ce

https://github.com/willhansen/rust_roguelike/assets/2918280/8e103e14-6331-4321-ba14-a2c3e64b4405

## Debug snapshots

Press `Ctrl-P` while playing to file a bug from a live session. The game pauses,
writes the current game state, rendered screen, and input history into a new
sequentially numbered issue directory (`issues/0001/snapshot/`, `0002/`, ...),
opens `$VISUAL`/`$EDITOR` (falling back to `vi`) on that issue's `issue.md` for
a description, and resumes when the editor exits. This is meant for capturing
transient rendering bugs from a live session.

Load a snapshot back and keep playing with
`cargo run --release -- --load issues/0001/snapshot` (or `--load <dir>`). The
persistent world state (player, pieces, blocks, portals, floating entities,
turn/world clock) is restored, so the session continues where it left off.
Transient visuals that are not serialized (in-flight animations, selectors)
reset. Snapshots can be combined with `--map` only when not loading.

The repo-root `snapshot/` directory is still used by `snapshot_tool` and manual
dumps; `Ctrl-P` files captures under `issues/` instead.

### `snapshot-tool` (headless debug)

Always run it through the repo-root wrapper, which rebuilds from source so the
binary can't be stale:

    ./snapshot-tool <render|diff|bless|cells|heights|fov-trace|fov-trace-json|explain|explain-diff|invariants|render-at|simulate|verify-issues|minimize> <dir> [args]

Notable modes: `diff` reports a char/fg/bg category split (fg/bg-only is usually
time-dependent starfield); `explain X Y [--char-col]` says how a cell was
resolved; `explain-diff <dir> <ref>` explains each changed cell in the current
render; `render-at <dir> [--player X Y] [--rotate N]` renders a capture with the
player moved (e.g. one step from where it was filed); `simulate <dir> --keys
<chars> [--render-each]` steps a capture forward; and `verify-issues` checks that
every `issues/solved/*` render matches its blessed `screen.txt` and still
differs from `screen.pre-fix.txt` (a fixture that matches both proves nothing).

## Font debugging

Many glyphs the game draws are not in the configured terminal font, so the
terminal renders them from a fallback. `scripts/glyph-fonts.sh` reports which
font the local stack actually picks for each glyph the game can draw. On Linux
it queries fontconfig *with the configured family*
(`fc-match '<family>:charset=<U+XXXX>'`), so results that differ from that
family are the true fallbacks:

    scripts/glyph-fonts.sh          # all glyphs, grouped by rendering font
    scripts/glyph-fonts.sh ❶ ◾     # just these
    scripts/glyph-fonts.sh --files  # add the font file path
    scripts/glyph-fonts.sh --all U+1F880   # ranked candidates per glyph

The family defaults to `CaskaydiaMono Nerd Font` (the author's terminal);
override with `--family NAME`. Without `fc-match` it falls back to listing a
font that contains each glyph.

See also the `floating_square_debug` tool's `pixels` (true glyph pixels from a
font file) and `which-font` (fonts containing a glyph) modes.

`scripts/collect-fonts.sh` bundles the fonts the game actually needs into the
gitignored `local-fonts/`: it resolves the terminal's ordered fallback chain,
truncates it at the last font that renders a glyph in the vocabulary, copies
those files (order-preserving `NNN-` names, `MANIFEST.tsv`), then runs the
`cover` mode over the copy to flag any glyph with no renderer:

    scripts/collect-fonts.sh                 # game vocabulary -> local-fonts/
    scripts/collect-fonts.sh ► ▲ U+1F880    # also check symbols you may add
    scripts/collect-fonts.sh --chain=full    # whole fc-match -s chain
    scripts/collect-fonts.sh --dry-run

