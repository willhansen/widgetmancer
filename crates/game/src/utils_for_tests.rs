use std::time::Instant;

use euclid::point2;

use crate::game::Game;
use crate::piece::Piece;
use utility::coordinate_frame_conversions::*;
use utility::{LEFT_I, RIGHT_I, STEP_LEFT, STEP_UP, UP_I};

pub fn set_up_nxm_game(rows: u32, cols: u32) -> Game {
    Game::new(cols as u16 * 2, rows as u16, Instant::now())
}

pub fn set_up_nxn_game(board_size: u32) -> Game {
    set_up_nxm_game(board_size, board_size)
}

pub fn set_up_10x10_game() -> Game {
    set_up_nxn_game(10)
}

pub fn set_up_game_with_player() -> Game {
    let mut game = set_up_10x10_game();
    game.place_player(game.mid_square());
    game
}

pub fn set_up_game_at_time() -> (Game, Instant) {
    let start_time = Instant::now();
    (Game::new(20, 10, start_time), start_time)
}

pub fn set_up_pawn_threatening_player() -> Game {
    let mut game = set_up_game_with_player();
    let pawn_pos = game.player_square() + WorldStep::new(1, 1);
    game.place_piece(Piece::pawn(), pawn_pos);
    game
}

pub fn set_up_player_facing_pawn_on_left() -> Game {
    let mut game = set_up_game_with_player();
    let one_left = game.player_square() + LEFT_I.cast_unit();
    game.place_piece(Piece::pawn(), one_left);

    game.raw_set_player_faced_direction(STEP_LEFT.into());
    game
}

pub fn set_up_player_facing_n_pawns_m_blocks_up(num_pawns: i32, blocks_up: i32) -> Game {
    let mut game = Game::new(20, 10 + blocks_up as u16, Instant::now());
    game.place_player(point2(5, 5));
    game.raw_set_player_faced_direction(STEP_UP.into());
    let line_start: WorldSquare =
        game.player_square() + UP_I.cast_unit() * blocks_up + LEFT_I.cast_unit() * num_pawns / 2;
    for i in 0..num_pawns {
        game.place_piece(Piece::pawn(), line_start + RIGHT_I.cast_unit() * i);
    }
    game
}
