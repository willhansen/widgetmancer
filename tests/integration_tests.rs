mod integration_utils;

use euclid::*;

use rust_roguelike::utility::{DOWN_I, RIGHT_I};
use crate::integration_utils::make_game;

#[test]
fn test_walk_in_circle() {
    let mut game = make_game();
    let start_pos = game.get_player_position();
    game.move_player(vec2(1, 0)).expect("");
    game.move_player(vec2(0, 1)).expect("");
    game.move_player(vec2(-1, 0)).expect("");
    game.move_player(vec2(0, -1)).expect("");
    assert_eq!(game.get_player_position(), start_pos)
}

#[test]
fn test_player_drawn_to_screen() {
    let mut game = make_game();
    let start_pos = game.get_player_position();
    game.draw_headless();
    let graphics = game.borrow_graphics_mut();
    let screen_player_pos = graphics.world_to_screen(start_pos);
    let drawn_player_string: String = graphics.get_char_at_screen_pos(screen_player_pos).to_string();
    dbg!(start_pos, screen_player_pos);
    assert_eq!("@", drawn_player_string)
}

#[test]
fn test_player_can_not_move_off_low_edge() {
    let mut game = make_game();
    let start_pos = point2(0, 0);
    game.set_player_position(&start_pos).expect("Failed to set player pos");

    let result = game.move_player(DOWN_I.cast_unit());
    assert!(result.is_err());
}

#[test]
fn test_player_can_not_move_off_high_edge() {
    let mut game = make_game();

    game.draw_headless();

    let bottom_right = point2((game.board_width()-1) as i32, 0);

    game.set_player_position(&bottom_right).expect("Failed to set player pos");

    let result = game.move_player(RIGHT_I.cast_unit());
    assert!(result.is_err());

    let result = game.move_player(DOWN_I.cast_unit());
    assert!(result.is_err());

    game.draw_headless();
}

