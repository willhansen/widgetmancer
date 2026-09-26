# Widgetmancer

A roguelike in rust featuring portals.

Run with `cargo run --release`

run tests with `cargo nextest run`

## Gameplay

https://github.com/willhansen/rust_roguelike/assets/2918280/4b05359b-7560-4e56-97ae-ad4b993ee7ce

https://github.com/willhansen/rust_roguelike/assets/2918280/8e103e14-6331-4321-ba14-a2c3e64b4405

## Debug snapshots

Press `p` while playing to dump the current game state, rendered screen, and
input history into the (gitignored) `snapshot/` directory at the repo root.
This is meant for capturing transient rendering bugs from a live session.

Load a snapshot back and keep playing with `cargo run --release -- --load snapshot`
(or `--load <dir>`). The persistent world state (player, pieces, blocks,
portals, floating entities, turn/world clock) is restored, so the session
continues where it left off. Transient visuals that are not serialized
(in-flight animations, selectors) reset. Snapshots can be combined with
`--map` only when not loading.

