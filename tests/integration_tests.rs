use euclid::*;

use rust_roguelike::game::Game;
use rust_roguelike::utility::{IPoint, IVector};

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

#[test]
fn test_player_drawn_to_screen() {
    let mut game = Game::new(10, 10);
    let start_pos = game.get_player_position();
    let graphics = game.borrow_graphics_mut();
    graphics.display(&mut None);
    let screen_player_pos: IPoint = graphics.world_to_screen(start_pos);
    let drawn_player_string: String = graphics.get_char_at_screen_pos(screen_player_pos).to_string();
    assert_eq!("@", drawn_player_string)
}
