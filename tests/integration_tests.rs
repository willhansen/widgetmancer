use game::Game;
use euclid::*;


#[test]
fn test_walk_in_circle() {
    let game = Game::new(10,10);
    let start_pos = game.get_player_position();
    game.move_player(point2(1,0));
    game.move_player(point2(0,1));
    game.move_player(point2(-1,0));
    game.move_player(point2(0,-1));
    assert!(game.get_player_position == start_pos)
}