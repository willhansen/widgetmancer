use euclid::{point2, vec2};
use rust_roguelike::game::Game;
use rust_roguelike::piece::Piece;
use rust_roguelike::utility::{WorldSquare, WorldStep, LEFT_I, RIGHT_I, UP_I};

pub fn set_up_game() -> Game {
    Game::new(20, 10)
}
pub fn set_up_pawn_threatening_player() -> Game {
    let mut game = set_up_game();
    let pawn_pos = game.player_position() + WorldStep::new(1, 1);
    game.place_piece(Piece::pawn(), pawn_pos)
        .expect("place pawn");
    game
}

pub fn set_up_game_with_player_in_corner() -> Game {
    let mut game = set_up_game();
    game.set_player_position(point2(0, 0))
        .expect("place player");
    game
}

pub fn set_up_player_facing_pawn_on_left() -> Game {
    let mut game = set_up_game();
    let one_left = game.player_position() + LEFT_I.cast_unit();
    game.place_piece(Piece::pawn(), one_left)
        .expect("Failed to place pawn");

    game.set_player_faced_direction(LEFT_I.cast_unit());
    game
}

pub fn set_up_player_facing_n_pawns_m_blocks_up(num_pawns: i32, blocks_up: i32) -> Game {
    let mut game = Game::new(20, 10 + blocks_up as u16);
    game.set_player_position(point2(5, 5))
        .expect("set player position");
    game.set_player_faced_direction(UP_I.cast_unit());
    let line_start: WorldSquare =
        game.player_position() + UP_I.cast_unit() * blocks_up + LEFT_I.cast_unit() * num_pawns / 2;
    for i in 0..num_pawns {
        game.place_piece(Piece::pawn(), line_start + RIGHT_I.cast_unit() * i)
            .expect("place pawn");
    }
    game
}
