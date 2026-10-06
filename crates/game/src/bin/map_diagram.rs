//! Print a colorless ASCII diagram of a map's placement. See
//! `game::game::map_diagram`.
//!
//! Usage: cargo run -p game --bin map_diagram -- [demo|racetrack|hallways|numbered-boxes|space-cubes]

use std::env;
use game::LogicalTime;

use game::game::Game;
use game::set_up_map_by_name;

fn main() {
    let map_name = env::args().nth(1).unwrap_or_else(|| "demo".to_string());

    // The map sets its own board size; this Game size is only the render
    // viewport, so it is independent of what the map defines.
    let mut game = Game::new(96, 26, LogicalTime::ZERO);
    set_up_map_by_name(&mut game, Some(&map_name));

    print!("{}", game.ascii_diagram());
}
