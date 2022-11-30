use std::collections::HashSet;

use euclid::*;
use ntest::assert_false;
use pretty_assertions::{assert_eq, assert_ne};

use rust_roguelike::animations::DOTS_IN_SELECTOR;
use rust_roguelike::glyph::glyph_constants::*;
use rust_roguelike::glyph::{DoubleGlyphFunctions, Glyph};
use rust_roguelike::piece::{Faction, Piece, PieceType};
use rust_roguelike::utility::{
    SquareGridInWorldFrame, WorldPoint, WorldSquare, WorldSquareRect, WorldStep, DOWN_I, LEFT_I,
    RIGHT_I, STEP_DOWN, STEP_RIGHT, STEP_UP, STEP_UP_RIGHT, UP_I,
};

use rust_roguelike::utils_for_tests::*;

#[test]
fn test_walk_in_circle() {
    let mut game = set_up_game_with_player();
    let start_pos = game.try_get_player_square();
    game.move_player(vec2(1, 0)).expect("");
    game.move_player(vec2(0, 1)).expect("");
    game.move_player(vec2(-1, 0)).expect("");
    game.move_player(vec2(0, -1)).expect("");
    assert_eq!(game.try_get_player_square(), start_pos)
}

#[test]
fn test_player_drawn_to_screen() {
    let mut game = set_up_game_with_player();
    let start_pos = game.player_square();
    game.raw_set_player_faced_direction(RIGHT_I.cast_unit());
    game.draw_headless_now();
    let graphics = game.borrow_graphics_mut();
    let drawn_glyphs = graphics.get_buffered_glyphs_for_square(start_pos);
    assert_ne!(drawn_glyphs[0].character, ' ');
}

#[test]
fn test_player_can_not_move_off_low_edge() {
    let mut game = set_up_game_with_player();
    let start_pos = point2(0, 0);
    game.try_set_player_position(start_pos)
        .expect("Failed to set player pos");

    let result = game.move_player(DOWN_I.cast_unit());
    assert!(result.is_err());
}

#[test]
fn test_player_can_not_move_off_high_edge() {
    let mut game = set_up_game_with_player();

    game.draw_headless_now();

    let bottom_right = point2((game.board_size().width - 1) as i32, 0);

    game.try_set_player_position(bottom_right)
        .expect("Failed to set player pos");

    let result = game.move_player(RIGHT_I.cast_unit());
    assert!(result.is_err());

    let result = game.move_player(DOWN_I.cast_unit());
    assert!(result.is_err());

    game.draw_headless_now();
}

#[test]
fn test_checkerboard_background() {
    let mut game = set_up_game_with_player();
    game.try_set_player_position(point2(0, 0))
        .expect("move player"); // out of the way

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
    let mut game = set_up_game_with_player();
    let one_left = game.player_square() + LEFT_I.cast_unit();
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
    let mut game = set_up_game_with_player();
    let one_left = game.player_square() + LEFT_I.cast_unit();
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
    let mut game = set_up_game_with_player();
    let player_start_square = game.player_square();
    let one_upleft = game.player_square() + (UP_I + LEFT_I).cast_unit();
    game.place_piece(Piece::pawn(), one_upleft)
        .expect("Failed to place pawn");
    game.move_piece_at(one_upleft);
    assert!(game.player_is_dead());
    assert_eq!(
        *game.get_piece_at(player_start_square).unwrap(),
        Piece::pawn()
    );
}

#[test]
fn test_pawn_move_towards_player() {
    let mut game = set_up_game_with_player();
    let two_left = game.player_square() + (LEFT_I * 2).cast_unit();
    let one_left = game.player_square() + (LEFT_I).cast_unit();
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
    let mut game = set_up_game_with_player();
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
    let mut game = set_up_game_with_player();
    let inspection_square: WorldSquare = game.player_square() + game.player_faced_direction();
    game.player_shoot_sniper();
    game.draw_headless_now();

    let drawn_glyphs = game
        .borrow_graphics_mut()
        .get_buffered_glyphs_for_square(inspection_square);

    assert_eq!(drawn_glyphs[0].fg_color, RED);
}

#[test]
fn test_player_background_is_transparent() {
    let mut game = set_up_game_with_player();
    let inspection_square: WorldSquare = game.player_square();

    game.draw_headless_now();

    let drawn_glyphs_at_pos_1 = game
        .borrow_graphics_mut()
        .get_buffered_glyphs_for_square(inspection_square);

    game.move_player(RIGHT_I.cast_unit()).expect("move player");
    game.draw_headless_now();

    let inspection_square: WorldSquare = game.player_square();
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
    let mut game = set_up_game();
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
    let mut game = set_up_game();
    let square1 = point2(2, 3);
    let square2 = square1 + UP_I.cast_unit() * 3;
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
    let mut game = set_up_game();
    let pawn_square = point2(5, 5);
    game.place_piece(Piece::pawn(), pawn_square)
        .expect("place_pawn");
    game.capture_piece_at(pawn_square).expect("capture pawn");
    game.draw_headless_now();

    let graphics = game.borrow_graphics_mut();

    let glyphs = graphics.get_buffered_glyphs_for_square(pawn_square);
    assert!(glyphs[0].is_braille() || (glyphs[1].is_braille()))
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
    let mut game = set_up_game();
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
    let mut game = set_up_game_with_player();
    game.place_piece(
        Piece::rook(),
        // three right, one up
        game.player_square() + RIGHT_I.cast_unit() * 3 + UP_I.cast_unit() * 1,
    )
    .expect("place rook");

    let one_up = game.player_square() + UP_I.cast_unit();
    assert_eq!(game.get_piece_at(one_up), None);
    game.move_all_pieces();
    assert_eq!(game.get_piece_at(one_up), Some(&Piece::rook()));
}

#[test]
fn test_rook_capture() {
    let mut game = set_up_game_with_player();
    game.place_piece(
        Piece::rook(),
        game.player_square() + RIGHT_I.cast_unit() * 3,
    )
    .expect("place rook");

    assert!(game.running());
    game.move_all_pieces();
    assert!(!game.running());
}

#[test]
fn test_king_move() {
    let mut game = set_up_game_with_player();
    let diag_up_right: WorldStep = RIGHT_I.cast_unit() + UP_I.cast_unit();
    game.place_piece(
        Piece::king(),
        // three right, one up
        game.player_square() + diag_up_right * 2,
    )
    .expect("place king");

    let king_end_square = game.player_square() + diag_up_right;
    assert_eq!(game.get_piece_at(king_end_square), None);
    game.move_all_pieces();
    assert_eq!(game.get_piece_at(king_end_square), Some(&Piece::king()));
}

#[test]
fn test_knight_move() {
    let mut game = set_up_game_with_player();
    game.place_piece(
        Piece::knight(),
        // three right, one up
        game.player_square() + RIGHT_I.cast_unit() * 3 + UP_I.cast_unit(),
    )
    .expect("place knight");

    let end_square = game.player_square() + RIGHT_I.cast_unit();
    assert_eq!(game.get_piece_at(end_square), None);
    game.move_all_pieces();
    assert_eq!(game.get_piece_at(end_square), Some(&Piece::knight()));
}

#[test]
fn test_correct_amount_of_braille_in_selector() {
    let mut game = set_up_game();
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

#[test]
fn test_no_move_into_check() {
    let mut game = set_up_game_with_player();
    let rook_square = game.player_square() + LEFT_I.cast_unit() + UP_I.cast_unit() * 3;
    game.place_piece(Piece::rook(), rook_square)
        .expect("place rook");
    game.move_player(LEFT_I.cast_unit())
        .expect_err("no move into check");
}

#[test]
fn test_draw_danger_squares() {
    let mut game = set_up_game_with_player();
    let rook_square = game.player_square() + LEFT_I.cast_unit() + UP_I.cast_unit() * 3;
    game.place_piece(Piece::rook(), rook_square)
        .expect("place rook");
    let danger_square = game.player_square() + LEFT_I.cast_unit();
    game.draw_headless_now();
    let actual_glyphs = game
        .borrow_graphics_mut()
        .get_buffered_glyphs_for_square(danger_square);

    assert_eq!(actual_glyphs[0].character, MOVE_AND_CAPTURE_SQUARE_CHARS[0]);
    assert_eq!(actual_glyphs[1].character, MOVE_AND_CAPTURE_SQUARE_CHARS[1]);
}

#[test]
fn test_no_step_on_block() {
    let mut game = set_up_game_with_player();
    let block_square = game.player_square() + RIGHT_I.cast_unit();
    game.place_block(block_square);
    assert!(game.is_block_at(block_square));
    game.move_player(RIGHT_I.cast_unit())
        .expect_err("no step on blok");
}

#[test]
fn test_rook_stopped_by_block() {
    let mut game = set_up_game_with_player();
    let rook_start_square = game.player_square() + RIGHT_I.cast_unit() * 4;
    let rook_end_square = game.player_square() + RIGHT_I.cast_unit() * 2;
    let block_square = game.player_square() + RIGHT_I.cast_unit();
    game.place_block(block_square);
    game.place_piece(Piece::rook(), rook_start_square)
        .expect("place rook");

    assert_eq!(game.get_piece_at(rook_end_square), None);
    game.move_all_pieces();
    assert_eq!(game.get_piece_at(rook_end_square), Some(&Piece::rook()));
}

#[test]
fn test_some_indicator_that_a_pawn_might_step_out_of_the_path_of_a_rook_immediately_before_that_rook_moves(
) {
    let mut game = set_up_game_with_player();
    let pawn_square = game.player_square() + RIGHT_I.cast_unit() * 3;
    let rook_square = game.player_square() + RIGHT_I.cast_unit() * 4;
    let square_to_check = game.player_square() + RIGHT_I.cast_unit() * 1;
    game.place_piece(Piece::pawn(), pawn_square).ok();
    game.place_piece(Piece::rook(), rook_square).ok();

    game.draw_headless_now();

    let graphics = game.borrow_graphics_mut();
    let test_square_glyphs = graphics.get_buffered_glyphs_for_square(square_to_check);
    let pawn_square_glyphs = graphics.get_buffered_glyphs_for_square(pawn_square);
    assert_false!(test_square_glyphs.looks_solid());
    assert_eq!(pawn_square_glyphs[0].bg_color, DANGER_SQUARE_COLOR);
    assert_eq!(pawn_square_glyphs[1].bg_color, DANGER_SQUARE_COLOR);
}

#[test]
fn test_pawn_move_and_capture_squares_both_visible_and_look_different() {
    let mut game = set_up_game();
    let pawn_square = game.mid_square();
    game.place_piece(Piece::pawn(), pawn_square).ok();
    let move_square = pawn_square + RIGHT_I.cast_unit();
    let capture_square = pawn_square + RIGHT_I.cast_unit() + UP_I.cast_unit();

    game.draw_headless_now();

    let move_glyphs = game.graphics().get_buffered_glyphs_for_square(move_square);
    let capture_glyphs = game
        .graphics()
        .get_buffered_glyphs_for_square(capture_square);

    assert_false!(move_glyphs.looks_solid());
    assert_false!(capture_glyphs.looks_solid());
    assert_ne!(move_glyphs, capture_glyphs);
}

#[test]
fn test_king_pathfind() {
    let mut game = set_up_nxn_game(20);
    let player_square = WorldSquare::new(3, 10);
    game.place_player(player_square);
    let u_center = WorldSquare::new(6, 10);
    let king_square = u_center + vec2(1, 0);
    game.place_piece(Piece::king(), king_square).ok();
    game.place_block(u_center + vec2(-1, -1));
    game.place_block(u_center + vec2(-1, 0));
    game.place_block(u_center + vec2(-1, 1));
    game.place_block(u_center + vec2(0, -1));
    game.place_block(u_center + vec2(0, 1));
    game.place_block(u_center + vec2(1, -1));
    game.place_block(u_center + vec2(1, 1));

    game.move_all_pieces();
    let new_king_square = *game.pieces().keys().next().unwrap();
    assert_ne!(new_king_square, u_center);
}

#[test]
#[ignore] // for now
fn test_draw_pathfind_paths() {
    let mut game = set_up_nxn_game(20);
    let player_square = point2(5, 5);
    let king_square = player_square + STEP_UP_RIGHT * 5;
    let test_square = player_square + STEP_UP_RIGHT * 2;
    game.place_player(player_square);
    game.place_piece(Piece::king(), king_square).ok();
    game.draw_headless_now();
    let path_glyphs = game.graphics().get_buffered_glyphs_for_square(test_square);

    assert_eq!(path_glyphs[0].character, KING_PATH_GLYPHS[0]);
    assert_eq!(path_glyphs[1].character, KING_PATH_GLYPHS[1]);
}

#[test]
fn test_turn_if_move_into_wall() {
    let mut game = set_up_game_with_player();
    game.raw_set_player_faced_direction(STEP_UP);
    game.place_block(game.player_square() + STEP_RIGHT);

    let start_square = game.player_square();
    game.move_player(STEP_RIGHT).ok();

    assert_eq!(game.player_faced_direction(), STEP_RIGHT);
    assert_eq!(game.player_square(), start_square);
}

#[test]
fn test_one_move_per_faction_per_turn() {
    let mut game = set_up_game();
    game.place_player(point2(0, 0));
    let pawn1 = Piece {
        piece_type: PieceType::Pawn,
        faction: Faction::from_id(0),
    };
    let pawn2 = Piece {
        piece_type: PieceType::Pawn,
        faction: Faction::from_id(1),
    };
    game.place_piece(pawn1, point2(2, 2)).ok();
    game.place_piece(pawn1, point2(4, 2)).ok();
    game.place_piece(pawn2, point2(2, 5)).ok();
    game.place_piece(pawn2, point2(4, 5)).ok();
    let positions_before: HashSet<WorldSquare> = HashSet::from_iter(game.pieces().keys().cloned());
    game.move_all_factions();
    let positions_after: HashSet<WorldSquare> = HashSet::from_iter(game.pieces().keys().cloned());

    assert_eq!(positions_before.intersection(&positions_after).count(), 2);
}

#[test]
fn test_blocks_block_view_visibly() {
    let mut game = set_up_game();
    game.place_player(point2(5, 5));
    game.place_block(point2(5, 4));
    let test_square = point2(5, 3);
    game.draw_headless_now();
    for dy in 0..3 {
        assert!(game
            .graphics()
            .get_buffered_glyphs_for_square(test_square + STEP_DOWN * dy)
            .iter()
            .all(|g| g.looks_solid_color(OUT_OF_SIGHT_COLOR)));
    }
}
