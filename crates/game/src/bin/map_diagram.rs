//! Print a colorless ASCII diagram of a map's placement. See
//! `game::game::map_diagram`.
//!
//! Usage: cargo run -p game --bin map_diagram -- [demo|racetrack|hallways]

use std::env;
use std::time::Instant;

use euclid::point2;

use game::game::Game;
use game::set_up_map_by_name;

fn main() {
    let map_name = env::args().nth(1).unwrap_or_else(|| "demo".to_string());

    // The racetrack map spans far enough right that a 96x26-character
    // terminal (48x26 squares, player at 24,13) is the smallest that holds
    // it; this is the same size `do_everything` clamps to.
    let mut game = Game::new(96, 26, Instant::now());
    game.place_player(point2(24, 13));
    set_up_map_by_name(&mut game, Some(&map_name));

    print!("{}", game.ascii_diagram());
}
