use euclid::*;
use pretty_assertions::{assert_eq, assert_ne};
use rust_roguelike::animations::DOTS_IN_SELECTOR;
use rust_roguelike::glyph::{Glyph, RED};

use rust_roguelike::piece::{Piece, PieceType};
use rust_roguelike::utility::{
    SquareGridInWorldFrame, WorldPoint, WorldSquare, WorldSquareRect, WorldStep, DOWN_I, LEFT_I,
    RIGHT_I, UP_I,
};

use crate::utils_for_tests::{
    set_up_game, set_up_game_with_player_in_corner, set_up_pawn_threatening_player,
    set_up_player_facing_n_pawns_m_blocks_up, set_up_player_facing_pawn_on_left,
};

mod utils_for_tests;

#[test]
fn test_walk_in_circle() {
    let mut game = set_up_game();
    let start_pos = game.player_position();
    game.move_player(vec2(1, 0)).expect("");
    game.move_player(vec2(0, 1)).expect("");
    game.move_player(vec2(-1, 0)).expect("");
    game.move_player(vec2(0, -1)).expect("");
    assert_eq!(game.player_position(), start_pos)
}

#[test]
fn test_player_drawn_to_screen() {
    let mut game = set_up_game();
    let start_pos = game.player_position();
    game.set_player_faced_direction(RIGHT_I.cast_unit());
    game.draw_headless_now();
    let graphics = game.borrow_graphics_mut();
    let drawn_glyphs = graphics.get_buffered_glyphs_for_square(start_pos);
    assert_ne!(drawn_glyphs[0].character, ' ');
}

#[test]
fn test_player_can_not_move_off_low_edge() {
    let mut game = set_up_game();
    let start_pos = point2(0, 0);
    game.set_player_position(start_pos)
        .expect("Failed to set player pos");

    let result = game.move_player(DOWN_I.cast_unit());
    assert!(result.is_err());
}

#[test]
fn test_player_can_not_move_off_high_edge() {
    let mut game = set_up_game();

    game.draw_headless_now();

    let bottom_right = point2((game.board_size().width - 1) as i32, 0);

    game.set_player_position(bottom_right)
        .expect("Failed to set player pos");

    let result = game.move_player(RIGHT_I.cast_unit());
    assert!(result.is_err());

    let result = game.move_player(DOWN_I.cast_unit());
    assert!(result.is_err());

    game.draw_headless_now();
}

#[test]
fn test_checkerboard_background() {
    let mut game = set_up_game();
    game.set_player_position(point2(0, 0)).expect("move player"); // out of the way

    game.draw_headless_now();

    let graphics = game.borrow_graphics_mut();

    let start_square = WorldSquare::new(5, 5);
    let left_square = start_square + LEFT_I.cast_unit();
    let up_square = start_square + UP_I.cast_unit();

    let start_square_glyphs = graphics.get_buffered_glyphs_for_square(start_square);
    let left_square_glyphs = graphics.get_buffered_glyphs_for_square(left_square);
    let up_square_glyphs = graphics.get_buffered_glyphs_for_square(up_square);

    // same color within square
    assert_eq!(start_square_glyphs[0], start_square_glyphs[1]);
    assert_eq!(left_square_glyphs[0], left_square_glyphs[1]);
    assert_eq!(up_square_glyphs[0], up_square_glyphs[1]);

    assert_eq!(left_square_glyphs, up_square_glyphs);

    assert_ne!(
        start_square_glyphs[0].bg_color,
        left_square_glyphs[0].bg_color
    );
}

#[test]
fn test_draw_placed_pawn() {
    let mut game = set_up_game();
    let one_left = game.player_position() + LEFT_I.cast_unit();
    game.place_piece(Piece::pawn(), one_left)
        .expect("Failed to place pawn");
    game.draw_headless_now();
    let pawn_glyphs = game
        .borrow_graphics_mut()
        .get_buffered_glyphs_for_square(one_left);
    assert_ne!(pawn_glyphs[0].character, ' ', "There should be a ");
}

#[test]
fn test_capture_pawn() {
    let mut game = set_up_game();
    let one_left = game.player_position() + LEFT_I.cast_unit();
    game.place_piece(Piece::pawn(), one_left)
        .expect("Failed to place pawn");

    assert_eq!(
        1,
        game.piece_type_count(PieceType::Pawn),
        "Should be one pawn"
    );

    game.move_player(LEFT_I.cast_unit())
        .expect("Failed to move player");

    assert_eq!(
        0,
        game.piece_type_count(PieceType::Pawn),
        "Should have captured pawn"
    );
}

#[test]
fn test_pawn_capture_player() {
    let mut game = set_up_game();
    let one_upleft = game.player_position() + (UP_I + LEFT_I).cast_unit();
    game.place_piece(Piece::pawn(), one_upleft)
        .expect("Failed to place pawn");
    game.move_piece_at(one_upleft);
    assert!(game.player_is_dead());
    assert_eq!(
        *game.get_piece_at(game.player_position()).unwrap(),
        Piece::pawn()
    );
}

#[test]
fn test_pawn_move_towards_player() {
    let mut game = set_up_game();
    let two_left = game.player_position() + (LEFT_I * 2).cast_unit();
    let one_left = game.player_position() + (LEFT_I).cast_unit();
    game.place_piece(Piece::pawn(), two_left)
        .expect("Failed to place pawn");
    game.move_all_pieces();
    assert_eq!(*game.get_piece_at(one_left).unwrap(), Piece::pawn());
}

#[test]
fn test_shoot_pawn() {
    let mut game = set_up_player_facing_pawn_on_left();

    assert_eq!(1, game.piece_type_count(PieceType::Pawn));
    game.player_shoot_shotgun();
    assert_eq!(0, game.piece_type_count(PieceType::Pawn));
}

#[test]
fn test_move_to_turn() {
    let mut game = set_up_game();
    game.move_player(UP_I.cast_unit()).expect("step");
    assert_eq!(
        game.player_faced_direction(),
        UP_I.cast_unit(),
        "turn with step"
    );

    game.move_player(((DOWN_I + RIGHT_I) * 3).cast_unit())
        .expect("step");
    assert_eq!(
        game.player_faced_direction(),
        (DOWN_I + RIGHT_I).cast_unit(),
        "only face directions with length one"
    );

    game.move_player((UP_I + LEFT_I * 4).cast_unit())
        .expect("step");
    assert_eq!(
        game.player_faced_direction(),
        LEFT_I.cast_unit(),
        "round to the standard 8 directions"
    );
}

#[test]
fn test_visible_laser() {
    let mut game = set_up_game();
    let inspection_square: WorldSquare = game.player_position() + game.player_faced_direction();
    game.player_shoot_shotgun();
    game.draw_headless_now();

    let drawn_glyphs = game
        .borrow_graphics_mut()
        .get_buffered_glyphs_for_square(inspection_square);

    assert_eq!(drawn_glyphs[0].fg_color, RED);
}

#[test]
fn test_player_background_is_transparent() {
    let mut game = set_up_game();
    let inspection_square: WorldSquare = game.player_position();

    game.draw_headless_now();

    let drawn_glyphs_at_pos_1 = game
        .borrow_graphics_mut()
        .get_buffered_glyphs_for_square(inspection_square);

    game.move_player(RIGHT_I.cast_unit()).expect("move player");
    game.draw_headless_now();

    let inspection_square: WorldSquare = game.player_position();
    let drawn_glyphs_at_pos_2 = game
        .borrow_graphics_mut()
        .get_buffered_glyphs_for_square(inspection_square);

    // one horizontal step -> different checker color
    assert_ne!(
        drawn_glyphs_at_pos_1[0].bg_color,
        drawn_glyphs_at_pos_2[0].bg_color
    );
}

#[test]
fn test_laser_background_is_transparent() {
    let mut game = set_up_game_with_player_in_corner();
    let left_point: WorldPoint = point2(2.0, 2.0);
    // Two lasers, because it can make a difference
    for _ in 0..2 {
        game.borrow_graphics_mut()
            .add_simple_laser(left_point, left_point + RIGHT_I.cast_unit().to_f32() * 4.0);
    }

    game.draw_headless_now();

    let test_point_a = left_point.round().to_i32() + RIGHT_I.cast_unit();
    let test_point_b = test_point_a + RIGHT_I.cast_unit();

    let glyphs_a = game
        .borrow_graphics_mut()
        .get_buffered_glyphs_for_square(test_point_a);
    let glyphs_b = game
        .borrow_graphics_mut()
        .get_buffered_glyphs_for_square(test_point_b);

    assert_ne!(glyphs_a[0].bg_color, glyphs_b[0].bg_color);
}

#[test]
fn test_pawn_background_is_transparent() {
    let mut game = set_up_game_with_player_in_corner();
    let square1 = point2(2, 3);
    let square2 = point2(2, 4);
    game.place_piece(Piece::pawn(), square1).expect("pawn1");
    game.place_piece(Piece::pawn(), square2).expect("pawn2");

    game.draw_headless_now();

    let gr = game.borrow_graphics_mut();

    let pawn1_glyphs = gr.get_buffered_glyphs_for_square(square1);
    let pawn2_glyphs = gr.get_buffered_glyphs_for_square(square2);
    assert_ne!(pawn1_glyphs[0].bg_color, pawn2_glyphs[0].bg_color,);
}

#[test]
fn test_shotgun_spread() {
    let start_pawns = 5;
    let mut game = set_up_player_facing_n_pawns_m_blocks_up(start_pawns, 5);
    game.player_shoot_shotgun();
    let end_pawns = game.piece_type_count(PieceType::Pawn);

    assert!(end_pawns < start_pawns - 1);
}

#[test]
fn test_particles_on_piece_death() {
    let mut game = set_up_game_with_player_in_corner();
    let pawn_square = point2(5, 5);
    game.place_piece(Piece::pawn(), pawn_square)
        .expect("place_pawn");
    game.capture_piece_at(pawn_square).expect("capture pawn");
    game.draw_headless_now();

    let graphics = game.borrow_graphics_mut();

    let glyphs = graphics.get_buffered_glyphs_for_square(pawn_square);
    assert!(Glyph::is_braille(glyphs[0].character) || Glyph::is_braille(glyphs[1].character))
}

#[test]
fn test_sniper_one_shot_one_kill() {
    let mut game = set_up_player_facing_n_pawns_m_blocks_up(3, 20);
    game.select_closest_piece();
    game.player_shoot_sniper();
    game.select_closest_piece();
    game.player_shoot_sniper();
    game.select_closest_piece();
    game.player_shoot_sniper();
    assert_eq!(game.piece_type_count(PieceType::Pawn), 0);
}

#[test]
fn test_selector() {
    let mut game = set_up_game_with_player_in_corner();
    let test_square = point2(5, 5);
    game.place_piece(Piece::pawn(), test_square)
        .expect("place piece");
    game.select_all_pieces();
    game.draw_headless_now();

    let diagonal = WorldStep::new(2, 2);
    let test_area: Box2D<i32, SquareGridInWorldFrame> = Box2D {
        min: test_square - diagonal,
        max: test_square + diagonal,
    };

    assert!(
        game.borrow_graphics_mut()
            .count_buffered_braille_dots_in_rect(test_area)
            > 0
    );
}

#[test]
fn test_game_over_on_capture_player() {
    let mut game = set_up_pawn_threatening_player();
    assert!(game.running());
    game.move_all_pieces();
    assert!(!game.running());
}
#[test]
fn test_rook_move() {
    let mut game = set_up_game();
    game.place_piece(
        Piece::rook(),
        // three right, one up
        game.player_position() + RIGHT_I.cast_unit() * 3 + UP_I.cast_unit() * 1,
    )
    .expect("place rook");

    let one_up = game.player_position() + UP_I.cast_unit();
    assert_eq!(game.get_piece_at(one_up), None);
    game.move_all_pieces();
    assert_eq!(game.get_piece_at(one_up), Some(&Piece::rook()));
}
#[test]
fn test_rook_capture() {
    let mut game = set_up_game();
    game.place_piece(
        Piece::rook(),
        game.player_position() + RIGHT_I.cast_unit() * 3,
    )
    .expect("place rook");

    assert!(game.running());
    game.move_all_pieces();
    assert!(!game.running());
}

#[test]
fn test_correct_amount_of_braille_in_selector() {
    let mut game = set_up_game_with_player_in_corner();
    let test_square = point2(5, 5);
    let diag = vec2(1, 1);
    let test_rectangle = WorldSquareRect::new(test_square - diag, test_square + diag);
    game.select_square(test_square);
    game.draw_headless_now();
    assert_eq!(
        game.borrow_graphics_mut()
            .count_buffered_braille_dots_in_rect(test_rectangle),
        DOTS_IN_SELECTOR
    );
    game.clear_selectors();
    game.draw_headless_now();
    assert_eq!(
        game.borrow_graphics_mut()
            .count_buffered_braille_dots_in_rect(test_rectangle),
        0
    );
}
