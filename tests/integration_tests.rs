use euclid::*;
use pretty_assertions::{assert_eq, assert_ne};

use rust_roguelike::piece::{Piece, PieceType};
use rust_roguelike::utility::{DOWN_I, LEFT_I, RIGHT_I, UP_I};

use crate::utils_for_tests::{make_game, set_up_player_facing_pawn_on_left};

mod utils_for_tests;

#[test]
fn test_walk_in_circle() {
    let mut game = make_game();
    let start_pos = game.player_position();
    game.move_player(&vec2(1, 0)).expect("");
    game.move_player(&vec2(0, 1)).expect("");
    game.move_player(&vec2(-1, 0)).expect("");
    game.move_player(&vec2(0, -1)).expect("");
    assert_eq!(game.player_position(), start_pos)
}

#[test]
fn test_player_drawn_to_screen() {
    let mut game = make_game();
    let start_pos = game.player_position();
    game.draw_headless();
    let graphics = game.borrow_graphics_mut();
    let screen_player_pos = graphics.world_pos_to_screen_pos(&start_pos);
    let drawn_player_string: String = graphics
        .get_char_at_screen_pos(screen_player_pos)
        .to_string();
    assert_eq!("@", drawn_player_string)
}

#[test]
fn test_player_can_not_move_off_low_edge() {
    let mut game = make_game();
    let start_pos = point2(0, 0);
    game.set_player_position(&start_pos)
        .expect("Failed to set player pos");

    let result = game.move_player(&DOWN_I.cast_unit());
    assert!(result.is_err());
}

#[test]
fn test_player_can_not_move_off_high_edge() {
    let mut game = make_game();

    game.draw_headless();

    let bottom_right = point2((game.board_width() - 1) as i32, 0);

    game.set_player_position(&bottom_right)
        .expect("Failed to set player pos");

    let result = game.move_player(&RIGHT_I.cast_unit());
    assert!(result.is_err());

    let result = game.move_player(&DOWN_I.cast_unit());
    assert!(result.is_err());

    game.draw_headless();
}

#[test]
fn test_checkerboard_background() {
    let mut game = make_game();

    game.draw_headless();

    let graphics = game.borrow_graphics_mut();

    let base_point = point2(0, 0);

    let steps = vec![RIGHT_I, UP_I];

    for step in steps {
        assert_eq!(
            graphics.get_buffered_glyphs_for_square(base_point),
            graphics.get_buffered_glyphs_for_square(base_point + step.cast_unit() * 2)
        );
        assert_ne!(
            graphics.get_buffered_glyphs_for_square(base_point),
            graphics.get_buffered_glyphs_for_square(base_point + step.cast_unit() * 1)
        );
        assert_eq!(
            graphics.get_buffered_glyphs_for_square(base_point + step.cast_unit() * 1),
            graphics.get_buffered_glyphs_for_square(base_point + step.cast_unit() * 3)
        );
    }
}

#[test]
fn test_draw_placed_pawn() {
    let mut game = make_game();
    let one_left = game.player_position() + LEFT_I.cast_unit();
    game.place_piece(Piece::pawn(), &one_left)
        .expect("Failed to place pawn");
    game.draw_headless();
    let pawn_glyphs = game
        .borrow_graphics_mut()
        .get_buffered_glyphs_for_square(one_left);
    assert_ne!(pawn_glyphs.0.character, ' ', "There should be a ");
}

#[test]
fn test_capture_pawn() {
    let mut game = make_game();
    let one_left = game.player_position() + LEFT_I.cast_unit();
    game.place_piece(Piece::pawn(), &one_left)
        .expect("Failed to place pawn");

    assert_eq!(
        1,
        game.piece_type_count(PieceType::Pawn),
        "Should be one pawn"
    );

    game.move_player(&LEFT_I.cast_unit())
        .expect("Failed to move player");

    assert_eq!(
        0,
        game.piece_type_count(PieceType::Pawn),
        "Should have captured pawn"
    );
}

#[test]
fn test_pawn_capture_player() {
    let mut game = make_game();
    let one_upleft = game.player_position() + (UP_I + LEFT_I).cast_unit();
    game.place_piece(Piece::pawn(), &one_upleft)
        .expect("Failed to place pawn");
    game.move_piece_at(&one_upleft);
    assert!(game.player_is_dead());
    assert_eq!(
        *game.get_piece_at(&game.player_position()).unwrap(),
        Piece::pawn()
    );
}

#[test]
fn test_pawn_move_towards_player() {
    let mut game = make_game();
    let two_left = game.player_position() + (LEFT_I * 2).cast_unit();
    let one_left = game.player_position() + (LEFT_I).cast_unit();
    game.place_piece(Piece::pawn(), &two_left)
        .expect("Failed to place pawn");
    game.move_all_pieces();
    assert_eq!(*game.get_piece_at(&one_left).unwrap(), Piece::pawn());
}

#[test]
fn test_shoot_pawn() {
    let mut game = set_up_player_facing_pawn_on_left();

    assert_eq!(1, game.piece_type_count(PieceType::Pawn));
    game.player_shoot();
    assert_eq!(0, game.piece_type_count(PieceType::Pawn));
}
#[test]
fn test_move_to_turn() {
    let mut game = make_game();
    game.move_player(&UP_I.cast_unit()).expect("step");
    assert_eq!(
        game.player_faced_direction(),
        UP_I.cast_unit(),
        "turn with step"
    );

    game.move_player(&((DOWN_I + RIGHT_I) * 3).cast_unit())
        .expect("step");
    assert_eq!(
        game.player_faced_direction(),
        (DOWN_I + RIGHT_I).cast_unit(),
        "only face directions with length one"
    );

    game.move_player(&(UP_I + LEFT_I * 4).cast_unit())
        .expect("step");
    assert_eq!(
        game.player_faced_direction(),
        LEFT_I.cast_unit(),
        "round to the standard 8 directions"
    );
}
