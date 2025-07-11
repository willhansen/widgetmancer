use std::collections::HashSet;
use std::time::{Duration, Instant};

use euclid::*;
use ntest::assert_false;
use pretty_assertions::{assert_eq, assert_ne};
use rand::SeedableRng;

use rust_roguelike::animations::DOTS_IN_SELECTOR;
use rust_roguelike::game::Game;
use rust_roguelike::glyph::glyph_constants::*;
use rust_roguelike::glyph::{DoubleGlyph, DoubleGlyphFunctions};
use rust_roguelike::piece::{Piece, PieceType};
use rust_roguelike::utility::coordinate_frame_conversions::*;
use rust_roguelike::utility::{
    DOWN_I, LEFT_I, RIGHT_I, STEP_DOWN, STEP_DOWN_LEFT, STEP_DOWN_RIGHT, STEP_RIGHT, STEP_UP,
    STEP_UP_RIGHT, UP_I,
};
use rust_roguelike::utils_for_tests::*;

#[test]
fn test_walk_in_circle() {
    let mut game = set_up_game_with_player();
    let start_pos = game.try_get_player_square();
    game.try_slide_player(vec2(1, 0)).expect("");
    game.try_slide_player(vec2(0, 1)).expect("");
    game.try_slide_player(vec2(-1, 0)).expect("");
    game.try_slide_player(vec2(0, -1)).expect("");
    assert_eq!(game.try_get_player_square(), start_pos)
}

#[test]
fn test_player_drawn_to_screen() {
    let mut game = set_up_game_with_player();
    let start_pos = game.player_square();
    game.raw_set_player_faced_direction(STEP_RIGHT.into());
    game.draw_headless_now();
    let graphics = game.borrow_graphics_mut();
    let drawn_glyphs = graphics.screen.get_screen_glyphs_at_world_square(start_pos);
    assert_ne!(drawn_glyphs[0].character, ' ');
}

#[test]
fn test_player_can_not_move_off_low_edge() {
    let mut game = set_up_game_with_player();
    let start_pos = point2(0, 0);
    game.try_set_player_position(start_pos)
        .expect("Failed to set player pos");

    let result = game.try_slide_player(DOWN_I.cast_unit());
    assert!(result.is_err());
}

#[test]
fn test_player_can_not_move_off_high_edge() {
    let mut game = set_up_game_with_player();

    game.draw_headless_now();

    let bottom_right = point2((game.board_size().width - 1) as i32, 0);

    game.try_set_player_position(bottom_right)
        .expect("Failed to set player pos");

    let result = game.try_slide_player(RIGHT_I.cast_unit());
    assert!(result.is_err());

    let result = game.try_slide_player(DOWN_I.cast_unit());
    assert!(result.is_err());

    game.draw_headless_now();
}

#[ignore]
#[test]
fn test_checkerboard_background() {
    let mut game = set_up_game_with_player();
    game.try_set_player_position(point2(1, 2))
        .expect("move player"); // out of the way

    game.draw_headless_now();

    let graphics = game.borrow_graphics_mut();

    let start_square = WorldSquare::new(5, 5);
    let left_square = start_square + LEFT_I.cast_unit();
    let up_square = start_square + UP_I.cast_unit();

    let start_square_glyphs = graphics
        .screen
        .get_screen_glyphs_at_world_square(start_square);
    let left_square_glyphs = graphics
        .screen
        .get_screen_glyphs_at_world_square(left_square);
    let up_square_glyphs = graphics.screen.get_screen_glyphs_at_world_square(up_square);

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
    game.place_piece(Piece::pawn(), one_left);
    game.draw_headless_now();
    let pawn_glyphs = game
        .borrow_graphics_mut()
        .screen
        .get_screen_glyphs_at_world_square(one_left);
    assert_ne!(pawn_glyphs[0].character, ' ', "There should be a ");
}

#[test]
fn test_capture_pawn() {
    let mut game = set_up_game_with_player();
    let one_left = game.player_square() + LEFT_I.cast_unit();
    game.place_piece(Piece::pawn(), one_left);

    assert_eq!(
        1,
        game.piece_type_count(PieceType::OmniDirectionalPawn),
        "Should be one pawn"
    );

    game.try_slide_player(LEFT_I.cast_unit())
        .expect("Failed to move player");

    assert_eq!(
        0,
        game.piece_type_count(PieceType::OmniDirectionalPawn),
        "Should have captured pawn"
    );
}

#[test]
fn test_pawn_capture_player() {
    let mut game = set_up_game_with_player();
    let player_start_square = game.player_square();
    let one_up_left = game.player_square() + (UP_I + LEFT_I).cast_unit();
    game.place_piece(Piece::pawn(), one_up_left);
    game.move_piece_at_square_and_return_end_position_if_moved(one_up_left);
    assert_false!(game.player_is_alive());
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
    game.place_piece(Piece::pawn(), two_left);
    game.move_all_pieces();
    assert_eq!(*game.get_piece_at(one_left).unwrap(), Piece::pawn());
}

#[test]
fn test_shoot_pawn() {
    let mut game = set_up_player_facing_pawn_on_left();

    assert_eq!(1, game.piece_type_count(PieceType::OmniDirectionalPawn));
    game.do_player_shoot_shotgun();
    assert_eq!(0, game.piece_type_count(PieceType::OmniDirectionalPawn));
}

#[test]
fn test_move_to_turn() {
    let mut game = set_up_game_with_player();
    game.try_slide_player(UP_I.cast_unit()).expect("step");
    assert_eq!(
        game.player_faced_direction(),
        STEP_UP.into(),
        "turn with step"
    );

    game.try_slide_player(((DOWN_I + RIGHT_I) * 3).cast_unit())
        .expect("step");
    assert_eq!(
        game.player_faced_direction(),
        STEP_DOWN_RIGHT.into(),
        "only face directions with length one"
    );

    //game.try_slide_player((UP_I + LEFT_I * 4).cast_unit()) .expect("step");
    //assert_eq!( game.player_faced_direction(), LEFT_I.cast_unit(), "round to the standard 8 directions" );
}

#[test]
fn test_visible_laser() {
    let mut game = set_up_game_with_player();
    let inspection_square: WorldSquare =
        game.player_square() + game.player_faced_direction().step();
    game.do_player_shoot_sniper();
    game.draw_headless_now();

    let drawn_glyphs = game
        .borrow_graphics_mut()
        .screen
        .get_screen_glyphs_at_world_square(inspection_square);

    assert_eq!(drawn_glyphs[0].fg_color, RED);
}

#[test]
fn test_player_background_is_transparent() {
    let mut game = set_up_game_with_player();

    let player_glyph = |game: &mut Game| -> DoubleGlyph {
        let inspection_square: WorldSquare = game.player_square();

        game.draw_headless_now();

        game.borrow_graphics_mut()
            .screen
            .get_screen_glyphs_at_world_square(inspection_square)
    };

    let drawn_glyphs_at_pos_1 = player_glyph(&mut game);

    game.try_slide_player(RIGHT_I.cast_unit())
        .expect("move player");

    let drawn_glyphs_at_pos_2 = player_glyph(&mut game);

    // one horizontal step -> different checker color
    assert_ne!(
        drawn_glyphs_at_pos_1[0].bg_color,
        drawn_glyphs_at_pos_2[0].bg_color
    );
}

#[test]
fn test_laser_background_is_transparent() {
    let mut game = set_up_10x10_game();
    let left_point: WorldPoint = point2(2.0, 2.0);
    let test_point_a = left_point.round().to_i32() + RIGHT_I.cast_unit();
    game.draw_headless_now();
    let glyphs_before = game
        .borrow_graphics_mut()
        .screen
        .get_screen_glyphs_at_world_square(test_point_a);
    // Two lasers, because it can make a difference
    for _ in 0..2 {
        game.borrow_graphics_mut()
            .add_simple_laser(left_point, left_point + RIGHT_I.cast_unit().to_f32() * 4.0);
    }

    game.draw_headless_now();

    let glyphs_a = game
        .borrow_graphics_mut()
        .screen
        .get_screen_glyphs_at_world_square(test_point_a);

    assert_eq!(glyphs_a[0].bg_color, glyphs_before[0].bg_color);
}

#[test]
fn test_pawn_background_is_transparent() {
    let mut game = set_up_10x10_game();
    let square1 = point2(2, 3);
    let square2 = square1 + UP_I.cast_unit() * 3;
    game.place_piece(Piece::pawn(), square1);
    game.place_piece(Piece::pawn(), square2);

    game.draw_headless_now();

    let gr = game.borrow_graphics_mut();

    let pawn1_glyphs = gr.screen.get_screen_glyphs_at_world_square(square1);
    let pawn2_glyphs = gr.screen.get_screen_glyphs_at_world_square(square2);

    assert_ne!(pawn1_glyphs[0].bg_color, pawn2_glyphs[0].bg_color,);
}

#[test]
fn test_shotgun_spread() {
    let start_pawns = 5;
    let mut game = set_up_player_facing_n_pawns_m_blocks_up(start_pawns, 5);
    game.do_player_shoot_shotgun();
    let end_pawns = game.piece_type_count(PieceType::OmniDirectionalPawn);

    assert!(end_pawns < start_pawns - 1);
}

#[test]
fn test_particles_on_piece_death() {
    let mut game = set_up_10x10_game();
    let pawn_square = point2(5, 5);
    game.place_piece(Piece::pawn(), pawn_square);
    game.capture_piece_at(pawn_square);
    game.draw_headless_now();

    let graphics = game.borrow_graphics_mut();

    let glyphs = graphics
        .screen
        .get_screen_glyphs_at_world_square(pawn_square);
    assert!(glyphs[0].is_braille() || (glyphs[1].is_braille()))
}

#[ignore = "TODO"]
#[test]
fn test_piece_death_animation_finishes() {
    let mut game = set_up_10x10_game();
    let pawn_square = point2(5, 5);
    game.place_piece(Piece::pawn(), pawn_square);
    game.capture_piece_at(pawn_square);
    game.draw_headless_at_duration_from_start(Duration::from_secs_f32(10.5));
    game.draw_headless_now();

    let graphics = game.borrow_graphics_mut();

    let glyphs = graphics
        .screen
        .get_screen_glyphs_at_world_square(pawn_square);
    assert!(!glyphs[0].is_braille() || (!glyphs[1].is_braille()));
    assert!(glyphs.looks_solid());
}

#[test]
fn test_sniper_one_shot_one_kill() {
    let mut game = set_up_player_facing_n_pawns_m_blocks_up(3, 20);
    game.select_closest_piece();
    game.do_player_shoot_sniper();
    game.select_closest_piece();
    game.do_player_shoot_sniper();
    game.select_closest_piece();
    game.do_player_shoot_sniper();
    assert_eq!(game.piece_type_count(PieceType::OmniDirectionalPawn), 0);
}

#[test]
fn test_selector() {
    let mut game = set_up_10x10_game();
    let test_square = point2(5, 5);
    game.place_piece(Piece::pawn(), test_square);
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
    );

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
    );

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
    );

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
    );

    let end_square = game.player_square() + RIGHT_I.cast_unit();
    assert_eq!(game.get_piece_at(end_square), None);
    game.move_all_pieces();
    assert_eq!(game.get_piece_at(end_square), Some(&Piece::knight()));
}

#[test]
fn test_correct_amount_of_braille_in_selector() {
    let mut game = set_up_10x10_game();
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
    game.place_piece(Piece::rook(), rook_square);
    game.try_slide_player(LEFT_I.cast_unit())
        .expect_err("no move into check");
}

#[test]
fn test_draw_danger_squares() {
    let mut game = set_up_game_with_player();
    let rook_square = game.player_square() + LEFT_I.cast_unit() + UP_I.cast_unit() * 3;
    game.place_piece(Piece::rook(), rook_square);
    let danger_square = game.player_square() + LEFT_I.cast_unit();
    game.draw_headless_now();
    let actual_glyphs = game
        .borrow_graphics_mut()
        .screen
        .get_screen_glyphs_at_world_square(danger_square);

    assert_eq!(actual_glyphs[0].character, MOVE_AND_CAPTURE_SQUARE_CHARS[0]);
    assert_eq!(actual_glyphs[1].character, MOVE_AND_CAPTURE_SQUARE_CHARS[1]);
}

#[test]
fn test_no_step_on_block() {
    let mut game = set_up_game_with_player();
    let block_square = game.player_square() + RIGHT_I.cast_unit();
    game.place_block(block_square);
    assert!(game.is_block_at(block_square));
    game.try_slide_player(RIGHT_I.cast_unit())
        .expect_err("no step on blok");
}

#[test]
fn test_rook_stopped_by_block() {
    let mut game = set_up_game_with_player();
    let rook_start_square = game.player_square() + RIGHT_I.cast_unit() * 4;
    let rook_end_square = game.player_square() + RIGHT_I.cast_unit() * 2;
    let block_square = game.player_square() + RIGHT_I.cast_unit();
    game.place_block(block_square);
    game.place_piece(Piece::rook(), rook_start_square);

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
    game.place_piece(Piece::pawn(), pawn_square);
    game.place_piece(Piece::rook(), rook_square);

    game.draw_headless_now();

    let graphics = game.borrow_graphics_mut();
    let test_square_glyphs = graphics
        .screen
        .get_screen_glyphs_at_world_square(square_to_check);
    let pawn_square_glyphs = graphics
        .screen
        .get_screen_glyphs_at_world_square(pawn_square);
    assert_false!(test_square_glyphs.looks_solid());
    assert_eq!(pawn_square_glyphs[0].bg_color, DANGER_SQUARE_COLOR);
    assert_eq!(pawn_square_glyphs[1].bg_color, DANGER_SQUARE_COLOR);
}

#[test]
fn test_pawn_move_and_capture_squares_both_visible_and_look_different() {
    let mut game = set_up_10x10_game();
    let pawn_square = game.mid_square();
    game.place_piece(Piece::pawn(), pawn_square);
    let move_square = pawn_square + RIGHT_I.cast_unit();
    let capture_square = pawn_square + RIGHT_I.cast_unit() + UP_I.cast_unit();

    game.draw_headless_now();

    let move_glyphs = game
        .graphics()
        .screen
        .get_screen_glyphs_at_world_square(move_square);
    let capture_glyphs = game
        .graphics()
        .screen
        .get_screen_glyphs_at_world_square(capture_square);

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
    game.place_piece(Piece::king(), king_square);
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
    game.place_piece(Piece::king(), king_square);
    game.draw_headless_now();
    let path_glyphs = game
        .graphics()
        .screen
        .get_screen_glyphs_at_world_square(test_square);

    assert_eq!(path_glyphs[0].character, KING_PATH_GLYPHS[0]);
    assert_eq!(path_glyphs[1].character, KING_PATH_GLYPHS[1]);
}

#[test]
fn test_turn_if_move_into_wall() {
    let mut game = set_up_game_with_player();
    game.raw_set_player_faced_direction(STEP_UP.into());
    game.place_block(game.player_square() + STEP_RIGHT);

    let start_square = game.player_square();
    game.try_slide_player(STEP_RIGHT).ok();

    assert_eq!(game.player_faced_direction(), STEP_RIGHT.into());
    assert_eq!(game.player_square(), start_square);
}

#[test]
fn test_one_move_per_faction_per_turn() {
    let mut game = set_up_10x10_game();
    game.place_player(point2(0, 0));
    let pawn1 = Piece::new(PieceType::OmniDirectionalPawn, game.get_new_faction());
    let pawn2 = Piece::new(PieceType::OmniDirectionalPawn, game.get_new_faction());
    game.place_piece(pawn1, point2(2, 2));
    game.place_piece(pawn1, point2(4, 2));
    game.place_piece(pawn2, point2(2, 5));
    game.place_piece(pawn2, point2(4, 5));
    let positions_before: HashSet<WorldSquare> = HashSet::from_iter(game.pieces().keys().cloned());
    game.move_non_arrow_factions();
    let positions_after: HashSet<WorldSquare> = HashSet::from_iter(game.pieces().keys().cloned());

    assert_eq!(positions_before.intersection(&positions_after).count(), 2);
}

#[test]
fn test_blocks_visibly_block_view() {
    let mut game = set_up_10x10_game();
    let player_pos = point2(5, 5);
    game.place_player(player_pos);
    game.place_block(player_pos + STEP_DOWN);
    let test_square = player_pos + STEP_DOWN * 2;
    game.draw_headless_now();
    for dy in 0..3 {
        assert!(game
            .graphics()
            .screen
            .get_screen_glyphs_at_world_square(test_square + STEP_DOWN * dy)
            .iter()
            .all(|g| g.looks_solid_color(OUT_OF_SIGHT_COLOR)));
    }
}

#[test]
fn test_mystery_labyrinth_death() {
    let (width, height) = (50, 50);
    let mut game = Game::new(width, height, Instant::now());
    game.place_player(point2(width as i32 / 4, height as i32 / 2));
    let mut rng = rand::rngs::StdRng::seed_from_u64(5);
    game.set_up_labyrinth(&mut rng);
    game.try_slide_player(STEP_DOWN_LEFT).ok();
    game.draw_headless_now();
}

#[test]
fn test_factions_attack_each_other() {
    let mut game = set_up_nxn_game(10);
    let square = point2(3, 3);

    game.place_new_king_pawn_faction(square);
    game.place_new_king_pawn_faction(square + STEP_RIGHT * 3);
    let num_pieces = game.pieces().len();
    assert_eq!(num_pieces, 18);
    game.move_non_arrow_factions();
    assert!(game.pieces().len() < num_pieces);
}
