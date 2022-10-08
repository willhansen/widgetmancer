use euclid::*;

use rust_roguelike::game::Game;

#[test]
fn test_walk_in_circle() {
    let mut game = Game::new(10, 10);
    let start_pos = game.get_player_position();
    game.move_player(vec2(1, 0));
    game.move_player(vec2(0, 1));
    game.move_player(vec2(-1, 0));
    game.move_player(vec2(0, -1));
    assert_eq!(game.get_player_position(), start_pos)
}