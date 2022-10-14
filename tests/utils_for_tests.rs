use rust_roguelike::game::Game;
use rust_roguelike::piece::Piece;
use rust_roguelike::utility::LEFT_I;

pub fn make_game() -> Game {
    Game::new(20, 10)
}

pub fn set_up_player_facing_pawn_on_left() -> Game {
    let mut game = make_game();
    let one_left = game.player_position() + LEFT_I.cast_unit();
    game.place_piece(Piece::pawn(), one_left)
        .expect("Failed to place pawn");

    game.set_player_faced_direction(LEFT_I.cast_unit());
    game
}
