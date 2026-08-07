//! Inline tests for the `game` module, extracted from the `game` god module
//! (ROADMAP.md item 1, step 6). Kept as a child module (`#[cfg(test)] mod
//! tests;` in `mod.rs`) rather than integration tests so the tests retain
//! access to `Game` internals (private fields and methods) via `use super::*`.

    use crate::fov_stuff::{
        print_fov_as_absolute, print_fov_as_relative, PositionedSquareVisibilityInFov,
    };
    use ::num::integer::Roots;
    use ntest::{assert_about_eq, assert_false};
    use pretty_assertions::{assert_eq, assert_ne};
    use terminal_rendering::glyph_constants::named_chars;

    use crate::game;
    use crate::graphics::drawable::{Drawable, DrawableEnum};
    use crate::piece::PieceType::Rook;
    use crate::piece::Upgrade;
    use crate::utils_for_tests::*;
    use terminal_rendering::glyph::glyph_constants::{
        BLACK, BLOCK_FG, BLUE, FULL_BLOCK, GREY, LEFT_HALF_BLOCK, OUT_OF_SIGHT_COLOR, RED,
        RIGHT_HALF_BLOCK,
    };

    use super::*;

    #[test]
    fn test_try_set_player_on_block_is_fail() {
        let mut game = Game::new(20, 10, Instant::now());
        game.place_player(point2(5, 5));
        game.place_block(point2(3, 3));
        assert!(game.try_set_player_position(point2(3, 3)).is_err());
    }

    #[test]
    fn test_blocks_block_view() {
        let mut game = set_up_10x10_game();
        game.place_player(point2(5, 5));
        game.place_block(point2(5, 4));
        let test_square = point2(5, 3);
        assert_false!(game.square_is_fully_visible_to_player(test_square));
    }

    #[test]
    fn test_fov_mask_non_partials() {
        let mut game = set_up_10x10_game();
        game.place_player(point2(5, 5));
        for i in 0..4 {
            game.place_block(game.player_square() + STEP_DOWN + STEP_RIGHT * i);
        }
        let relative_squares_that_should_be_fully_visible = vec![
            STEP_RIGHT,
            STEP_UP_RIGHT,
            STEP_UP,
            STEP_RIGHT * 2,
            STEP_UP_LEFT,
            STEP_LEFT,
        ];
        let relative_squares_that_should_be_fully_blocked = vec![
            STEP_DOWN * 2,
            STEP_DOWN * 2 + STEP_RIGHT,
            STEP_DOWN * 2 + STEP_RIGHT * 2,
            STEP_DOWN * 2 + STEP_RIGHT * 3,
        ];
        for step in relative_squares_that_should_be_fully_visible {
            let square = game.player_square() + step;
            assert!(
                game.square_is_fully_visible_to_player(square),
                "should be fully visible.  square: {}",
                point_to_string(square)
            );
        }
        for step in relative_squares_that_should_be_fully_blocked {
            let square = game.player_square() + step;
            assert!(
                game.square_is_not_visible_to_player(square),
                "should be fully blocked.  square: {}",
                point_to_string(square)
            );
        }
    }

    #[test]
    fn test_faction_moves_closest_piece_to_player() {
        let mut game = set_up_game_with_player();
        let king_square = game.player_square() + STEP_UP_RIGHT * 3;
        game.place_new_king_pawn_faction(king_square);
        let test_square = king_square + STEP_DOWN_LEFT;
        assert_false!(game.square_is_empty(test_square));
        game.move_non_arrow_factions();
        assert!(game.square_is_empty(test_square));
    }

    #[test]
    fn test_pawn_reproduction_in_surrounded_squares() {
        let mut game = set_up_10x10_game();
        let test_square = point2(5, 5);
        let faction = game.get_new_faction();
        for step in ORTHOGONAL_STEPS {
            game.place_piece(Piece::new(OmniDirectionalPawn, faction), test_square + step);
        }
        assert_eq!(game.pieces.len(), 4);
        for _ in 0..=TURNS_TO_SPAWN_PAWN {
            game.tick_pawn_incubation();
        }
        assert!(game.pieces.len() > 4);
    }

    #[test]
    fn test_pawn_reproduction_does_not_apply_to_filled_squares() {
        let mut game = set_up_10x10_game();

        let test_square = point2(5, 5);
        let faction = game.get_new_faction();

        for step in ORTHOGONAL_STEPS {
            game.place_piece(Piece::new(OmniDirectionalPawn, faction), test_square + step);
        }
        game.place_piece(Piece::new(OmniDirectionalPawn, faction), test_square);

        assert_eq!(game.pieces.len(), 5);
        for _ in 0..=TURNS_TO_SPAWN_PAWN {
            game.tick_pawn_incubation();
        }
        assert_eq!(game.pieces.len(), 5);
    }

    #[test]
    fn test_faction_with_only_pawns_becomes_red_pawns() {
        let mut game = set_up_10x10_game();

        let king_square = point2(5, 5);
        let test_square = king_square + STEP_UP_RIGHT;
        game.place_new_king_pawn_faction(king_square);
        let placed_faction = game.get_piece_at(king_square).unwrap().faction;

        assert_eq!(
            game.get_piece_at(test_square).unwrap().faction,
            placed_faction
        );
        assert_ne!(
            game.get_piece_at(test_square).unwrap().faction,
            game.red_pawn_faction
        );

        game.capture_piece_at(king_square);
        game.on_turn_end();

        let the_piece = game.get_piece_at(test_square).unwrap();
        assert_ne!(the_piece.faction, placed_faction);
        assert_eq!(the_piece.faction, game.red_pawn_faction);
    }

    #[test]
    fn test_red_pawn_looks_red() {
        let mut game = set_up_10x10_game();
        let square = point2(5, 5);
        game.place_red_pawn(square);
        game.draw_headless_now();
        let glyphs = game
            .graphics
            .screen
            .get_screen_glyphs_at_world_square(square);
        assert_eq!(glyphs.get(0).unwrap().fg_color, RED_PAWN_COLOR);
    }

    #[test]
    fn test_red_pawns_dont_move_if_stable() {
        let mut game = set_up_10x10_game();
        game.place_player(point2(0, 0));
        let center_square = point2(5, 5);
        let pawn_squares: SquareSet = ORTHOGONAL_STEPS
            .iter()
            .map(|&step| center_square + step)
            .collect();
        for &pawn_square in &pawn_squares {
            game.place_red_pawn(pawn_square);
        }
        game.move_non_arrow_factions();
        let found_pawn_squares: SquareSet = game.pieces.keys().cloned().collect();
        assert_eq!(pawn_squares, found_pawn_squares);
    }

    #[test]
    fn test_red_pawn_will_move_into_protection() {
        let mut game = set_up_10x10_game();
        let moving_pawn_square = point2(5, 5);
        let correct_end_square = moving_pawn_square + STEP_LEFT;
        game.place_red_pawn(correct_end_square + STEP_DOWN_LEFT);
        game.place_red_pawn(moving_pawn_square);
        game.move_piece_at_square_and_return_end_position_if_moved(moving_pawn_square);
        assert!(game.pieces.contains_key(&correct_end_square));
    }

    #[test]
    fn test_red_pawns_dont_try_to_capture_each_other() {
        let mut game = set_up_10x10_game();
        let start_square = point2(5, 5);

        for dx in 0..3 {
            for dy in 0..3 {
                let vec = vec2(dx, dy);
                game.place_red_pawn(start_square + vec);
            }
        }
        let num_pieces_at_start = game.pieces.len();
        game.move_piece_at_square_and_return_end_position_if_moved(start_square);
        assert_eq!(num_pieces_at_start, game.pieces.len());
    }

    #[test]
    fn test_red_pawns_try_to_not_pack_tightly() {
        let mut game = set_up_10x10_game();
        let pawn_squares = (4..=6).flat_map(|x| (4..=5).map(move |y| point2(x, y)));
        for square in pawn_squares {
            game.place_red_pawn(square);
        }
        assert_eq!(game.piece_type_count(OmniDirectionalPawn), 6);
        let test_square = point2(5, 5);
        assert_false!(game.square_is_empty(test_square));
        game.move_piece_at_square_and_return_end_position_if_moved(test_square);
        assert!(game.square_is_empty(test_square));
    }

    #[test]
    fn test_red_pawns_slightly_prefer_movement_over_non_movement() {
        let mut game = set_up_10x10_game();
        let pawn_square = point2(5, 5);
        game.place_red_pawn(pawn_square);
        assert_false!(game.square_is_empty(pawn_square));
        game.move_piece_at_square_and_return_end_position_if_moved(pawn_square);
        assert!(game.square_is_empty(pawn_square));
    }

    #[test]
    fn test_death_cube_kills_player() {
        let mut game = set_up_game_with_player();
        let death_cube_start_pos = (game.player_square() + STEP_LEFT).to_f32();
        let death_cube_start_vel = STEP_RIGHT.to_f32() * 20.0;
        game.place_linear_death_cube(death_cube_start_pos, death_cube_start_vel);
        assert!(game.player_is_alive());
        game.tick_death_cubes(Duration::from_secs_f32(1.0));
        assert_false!(game.player_is_alive());
    }

    #[test]
    fn test_death_cube_kills_rook() {
        let mut game = set_up_10x10_game();
        let rook_square = point2(5, 5);
        game.place_piece(Piece::new(Rook, game.default_enemy_faction), rook_square);
        let death_cube_start_pos = (rook_square + STEP_LEFT).to_f32();
        let death_cube_start_vel = STEP_RIGHT.to_f32() * 20.0;
        game.place_linear_death_cube(death_cube_start_pos, death_cube_start_vel);
        assert!(!game.pieces.is_empty());
        game.tick_death_cubes(Duration::from_secs_f32(1.0));
        assert!(game.pieces.is_empty());
    }

    #[test]
    fn test_death_cube_can_be_seen() {
        let mut game = set_up_10x10_game();
        let test_square = point2(5, 5);

        game.draw_headless_now();
        assert!(game
            .graphics
            .screen
            .get_screen_glyphs_at_world_square(test_square)
            .looks_solid());

        let death_cube_start_pos = test_square.to_f32() + vec2(0.3, 0.0);
        game.place_linear_death_cube(death_cube_start_pos, vec2(0.0, 0.0));

        game.draw_headless_now();
        assert!(!game
            .graphics
            .screen
            .get_screen_glyphs_at_world_square(test_square)
            .looks_solid());
    }

    #[test]
    fn test_death_cube_moves() {
        let mut game = set_up_10x10_game();
        game.place_linear_death_cube(point2(3.0, 4.5), vec2(1.0, 0.0));
        game.tick_death_cubes(Duration::from_secs_f32(1.0));
        assert_about_eq!(game.death_cubes[0].position.x, 4.0);
    }

    #[test]
    fn test_death_cube_visually_moves() {
        let mut game = set_up_10x10_game();
        let test_square = point2(5, 5);

        game.draw_headless_now();
        let get_solidness = |game: &Game| -> Vec<bool> {
            (0..4)
                .map(|dx| {
                    game.graphics
                        .screen
                        .get_screen_glyphs_at_world_square(test_square + STEP_RIGHT * dx)
                        .looks_solid()
                })
                .collect()
        };

        let squares_that_look_solid = get_solidness(&game);
        assert_eq!(squares_that_look_solid, vec![true, true, true, true]);

        let death_cube_start_pos = test_square.to_f32() + vec2(0.3, 0.0);
        game.place_linear_death_cube(death_cube_start_pos, vec2(1.0, 0.0));

        game.draw_headless_now();
        let squares_that_look_solid = get_solidness(&game);
        assert_eq!(squares_that_look_solid, vec![false, false, true, true]);

        game.tick_death_cubes(Duration::from_secs_f32(1.0));

        game.draw_headless_now();
        let squares_that_look_solid = get_solidness(&game);
        assert_eq!(squares_that_look_solid, vec![true, false, false, true]);
    }

    #[test]
    fn test_death_cubes_change_color_over_time() {
        let mut game = set_up_10x10_game();
        let test_square = point2(3, 3);
        game.place_linear_death_cube(test_square.to_f32(), vec2(0.0, 0.0));
        game.draw_headless_at_duration_from_start(Duration::from_secs_f32(1.0));
        let cube_color_1 = game
            .graphics
            .screen
            .get_screen_glyphs_at_world_square(test_square)
            .get_solid_color()
            .unwrap();
        game.draw_headless_at_duration_from_start(Duration::from_secs_f32(1.23432));
        let cube_color_2 = game
            .graphics
            .screen
            .get_screen_glyphs_at_world_square(test_square)
            .get_solid_color()
            .unwrap();
        assert_ne!(cube_color_1, cube_color_2);
    }

    #[test]
    fn test_death_cube_turret_shoots_death_cubes() {
        let mut game = set_up_10x10_game();
        let turret_square = point2(5, 5);
        game.place_death_turret(turret_square);
        assert!(game.death_cubes.is_empty());
        game.tick_realtime_effects(Duration::from_secs_f32(5.0));
        assert!(!game.death_cubes.is_empty());

        game.tick_death_cubes(Duration::from_secs_f32(1.0));
        assert!(!game.pieces.is_empty());
    }

    #[test]
    fn test_death_cubes_vanish_when_off_board() {
        let mut game = set_up_nxn_game(5);
        game.place_linear_death_cube(point2(4.9, 4.9), vec2(20.0, 0.0));
        assert!(!game.death_cubes.is_empty());
        game.tick_death_cubes(Duration::from_secs_f32(5.0));
        assert!(game.death_cubes.is_empty());
    }

    #[test]
    fn test_player_blink() {
        let mut game = set_up_game_with_player();
        let start_pos = game.player_square();
        game.player_blink(STEP_RIGHT);
        let square_blink_dist = (game.player_square() - start_pos).square_length();
        assert!(square_blink_dist > 1);
    }

    #[test]
    fn test_blink_is_also_strafe() {
        let mut game = set_up_game_with_player();
        let start_pos = game.player_square();
        game.raw_set_player_faced_direction(STEP_UP.into());
        game.player_blink(STEP_RIGHT);
        assert_eq!(game.player_faced_direction(), STEP_UP.into());
    }

    #[test]
    fn test_player_no_blink_through_block() {
        let mut game = set_up_game_with_player();
        let start_pos = game.player_square();
        let block_pos = game.player_square() + STEP_RIGHT * 3;
        game.place_block(block_pos);
        game.player_blink(STEP_RIGHT);
        assert_eq!(game.player_square(), block_pos + STEP_LEFT);
    }

    #[test]
    fn test_blink_leaves_blue_trail() {
        let mut game = set_up_game_with_player();
        let start_pos = game.player_square();
        game.player_blink(STEP_RIGHT);
        let end_pos = game.player_square();

        // TODO: why is the duration necessary? (might be just a random empty block)
        game.draw_headless_at_duration_from_start(Duration::from_secs_f32(0.1));

        // check all the intermediate squares, but only require at least one of the two characters in each square has a particle
        (start_pos.x + 1..end_pos.x).for_each(|x| {
            let square = point2(x, start_pos.y);
            let glyphs = game
                .graphics
                .screen
                .get_screen_glyphs_at_world_square(square);
            //assert!(!glyphs.looks_solid());
            assert!(
                glyphs[0].fg_color == BLINK_EFFECT_COLOR
                    || glyphs[1].fg_color == BLINK_EFFECT_COLOR
            );
        })
    }

    #[test]
    fn test_overlapping_blink_trails_have_uniform_color() {
        let mut game = set_up_game_with_player();
        let start_pos = game.player_square();
        game.player_blink(STEP_RIGHT);
        let end_pos = game.player_square();
        game.player_blink(STEP_LEFT);

        let blink_step = end_pos - start_pos;

        for i in 0..20 {
            // TODO: why is the duration necessary? (might be just randomness)
            let delta = Duration::from_secs_f32(i as f32 * 0.1);
            game.draw_headless_at_duration_from_start(delta);
            //game.draw_headless_now();
            for dx in 1..blink_step.x {
                let square = start_pos + vec2(dx, 0);
                let glyphs = game
                    .graphics
                    .screen
                    .get_screen_glyphs_at_world_square(square);
                // There might not be particles in every character square.  Don't test the empty ones
                if !glyphs[0].looks_solid() {
                    assert_eq!(glyphs[0].fg_color, BLINK_EFFECT_COLOR);
                }
                if !glyphs[1].looks_solid() {
                    assert_eq!(glyphs[1].fg_color, BLINK_EFFECT_COLOR);
                }
            }
        }
    }

    #[test]
    fn test_try_to_blink_but_blocked() {
        let mut game = set_up_10x10_game();
        game.place_player(point2(0, 0));
        game.player_blink(STEP_LEFT);
    }

    #[test]
    fn test_protected_piece_has_fully_colored_background() {
        let mut game = set_up_10x10_game();
        let square1 = point2(5, 5);
        game.place_red_pawn(square1);
        game.place_red_pawn(square1 + STEP_UP_RIGHT);
        game.draw_headless_now();
        let pawn_glyphs = game
            .graphics
            .screen
            .get_screen_glyphs_at_world_square(square1);

        assert_eq!(pawn_glyphs[0].bg_color, DANGER_SQUARE_COLOR);
        assert_eq!(pawn_glyphs[1].bg_color, DANGER_SQUARE_COLOR);
    }

    #[test]
    fn test_blink_range_upgrade() {
        let mut game = set_up_nxn_game(20);
        let start = point2(5, 5);
        game.place_player(start);
        let start_blink_range = 3;
        game.player().blink_range = start_blink_range;
        game.player_blink(STEP_RIGHT);
        assert_eq!(
            (start - game.player_square()).square_length().sqrt(),
            start_blink_range as i32
        );

        let upgrade_square = point2(5, 6);
        game.place_upgrade(BlinkRange, upgrade_square);

        game.move_player_to(upgrade_square);

        game.player_blink(STEP_RIGHT);

        assert_eq!(
            (upgrade_square - game.player_square())
                .square_length()
                .sqrt(),
            start_blink_range as i32 + 1
        );
    }

    #[test]
    fn test_kings_drop_upgrades() {
        let mut game = set_up_10x10_game();
        let square = point2(5, 5);
        game.place_piece(Piece::new(King, game.default_enemy_faction), square);
        assert!(game.blocks.upgrades.is_empty());
        game.capture_piece_at(square);
        assert_eq!(game.blocks.upgrades.get(&square).unwrap(), &BlinkRange);
    }

    #[test]
    fn test_soldier() {
        let mut game = set_up_10x10_game();
        let square = point2(5, 5);
        let piece = Piece::from_type(OmniDirectionalSoldier);
        game.place_piece(piece, square);
        assert_false!(piece.can_turn());
        assert!(game
            .on_board_move_squares_for_piece_at(square, false)
            .contains(&(square + STEP_RIGHT)));
        assert!(game
            .guarded_squares_for_piece_at(square)
            .contains(&(square + STEP_RIGHT)))
    }

    #[test]
    fn test_turning_soldier_turns_toward_player() {
        let mut game = set_up_10x10_game();
        let player_square = point2(5, 5);
        game.place_player(player_square);

        let soldier_square = player_square + STEP_LEFT * 3;

        game.place_piece(Piece::from_type(TurningSoldier), soldier_square);
        game.get_mut_piece_at(soldier_square)
            .unwrap()
            .set_faced_direction(STEP_UP.into());

        assert_eq!(
            game.move_options_for_piece_at(soldier_square),
            vec![soldier_square + STEP_UP]
        );
        game.move_all_pieces();
        assert_eq!(
            game.move_options_for_piece_at(soldier_square),
            vec![soldier_square + STEP_RIGHT]
        );
    }

    #[test]
    fn test_turning_pawn_turns_toward_player() {
        let mut game = set_up_10x10_game();
        let player_square = point2(5, 5);
        game.place_player(player_square);

        let square = player_square + STEP_LEFT * 3;
        game.place_piece(Piece::from_type(TurningPawn), square);

        game.get_mut_piece_at(square)
            .unwrap()
            .set_faced_direction(STEP_UP.into());

        assert_eq!(
            game.move_options_for_piece_at(square),
            vec![square + STEP_UP]
        );
        game.move_all_pieces();
        assert_eq!(
            game.move_options_for_piece_at(square),
            vec![square + STEP_RIGHT]
        );
    }

    #[test]
    fn test_player_radial_attack() {
        let mut game = set_up_10x10_game();
        let square = point2(5, 5);
        game.place_player(square);
        KING_STEPS
            .iter()
            .for_each(|&step: &WorldStep| game.place_piece(Piece::pawn(), square + step));
        game.place_piece(Piece::pawn(), square + STEP_RIGHT * 2);
        assert_eq!(game.pieces.len(), 9);
        game.do_player_radial_attack();
        assert_eq!(game.pieces.len(), 1);
    }

    #[test]
    fn test_player_spear_attack() {
        let mut game = set_up_10x10_game();
        let square = point2(5, 5);
        game.place_player(square);
        game.player().faced_direction = STEP_RIGHT.into();

        game.place_piece(Piece::pawn(), square + STEP_RIGHT * 2);
        assert_eq!(game.pieces.len(), 1);
        game.do_player_spear_attack();
        assert_eq!(game.pieces.len(), 0);
    }

    #[test]
    fn test_arrow_travels() {
        let mut game = set_up_10x10_game();
        let square = point2(5, 5);
        game.place_arrow(square, STEP_RIGHT.into());
        assert!(game.is_arrow_at(square));
        assert_eq!(game.arrows().len(), 1);
        game.tick_projectile_arrows();
        assert!(game.is_arrow_at(square + STEP_RIGHT));
        assert_eq!(game.arrows().len(), 1);
    }

    #[test]
    fn test_draw_arrows() {
        let mut game = set_up_10x10_game();
        let square = point2(5, 5);
        game.place_arrow(square, STEP_RIGHT.into());
        game.draw_headless_now();
        let glyphs = game
            .graphics
            .screen
            .get_screen_glyphs_at_world_square(square);
        assert_false!(glyphs.looks_solid());
    }

    #[test]
    fn test_player_shoot_arrow() {
        let mut game = set_up_10x10_game();
        let square = point2(5, 5);
        game.place_player(square);
        game.player().faced_direction = STEP_RIGHT.into();
        assert!(game.arrows().is_empty());
        game.do_player_shoot_arrow();
        assert_false!(game.arrows().is_empty());
    }

    #[test]
    fn test_player_shoot_arrow_diagonal() {
        let mut game = set_up_10x10_game();
        let square = point2(5, 5);
        game.place_player(square);
        game.player().faced_direction = STEP_UP_RIGHT.into();
        assert!(game.arrows().is_empty());
        game.do_player_shoot_arrow();
        assert_false!(game.arrows().is_empty());
    }

    #[test]
    fn test_player_can_capture_arrow() {
        let mut game = set_up_10x10_game();
        let square = point2(5, 5);
        game.place_player(square + STEP_UP);
        game.place_arrow(square, STEP_RIGHT.into());
        assert_false!(game.arrows().is_empty());
        game.move_player_to(square);
        assert!(game.arrows().is_empty());
    }

    #[test]
    fn test_player_can_step_through_portal() {
        let mut game = set_up_10x10_game();
        game.place_player(point2(5, 5));
        game.place_single_sided_one_way_portal(
            SquareWithOrthogonalDir::from_square_and_worldstep(point2(5, 5), STEP_RIGHT.into()),
            SquareWithOrthogonalDir::from_square_and_worldstep(point2(5, 7), STEP_LEFT.into()),
        );
        game.try_slide_player(STEP_RIGHT).expect("move player");

        assert_eq!(game.player_square(), point2(5, 7));
        assert_eq!(game.player_faced_direction(), STEP_LEFT.into());
    }

    #[test]
    fn test_portal_steps() {
        let mut game = set_up_10x10_game();
        let entrance_step =
            SquareWithOrthogonalDir::from_square_and_worldstep(point2(2, 6), STEP_UP.into());
        let exit_step =
            SquareWithOrthogonalDir::from_square_and_worldstep(point2(5, 2), STEP_RIGHT.into());
        game.place_single_sided_one_way_portal(entrance_step, exit_step);
        assert_eq!(
            game.portal_aware_single_step(entrance_step.into()).unwrap(),
            exit_step.into()
        );
    }

    #[test]
    fn test_move_through_multiple_portals() {
        let mut game = set_up_10x10_game();
        let start =
            SquareWithOrthogonalDir::from_square_and_worldstep(point2(2, 6), STEP_RIGHT.into());
        let mid =
            SquareWithOrthogonalDir::from_square_and_worldstep(point2(5, 5), STEP_DOWN.into());
        let end =
            SquareWithOrthogonalDir::from_square_and_worldstep(point2(5, 2), STEP_LEFT.into());
        game.place_single_sided_one_way_portal(start, mid);
        game.place_single_sided_one_way_portal(mid, end);
        assert_eq!(
            game.multiple_portal_aware_steps(start.into(), 2).unwrap(),
            end.into()
        );
    }

    #[test]
    fn test_arrow_through_portal() {
        let mut game = set_up_10x10_game();
        let start =
            SquareWithOrthogonalDir::from_square_and_worldstep(point2(2, 6), STEP_RIGHT.into());
        let end =
            SquareWithOrthogonalDir::from_square_and_worldstep(point2(5, 2), STEP_DOWN.into());
        game.place_single_sided_one_way_portal(start, end);
        game.place_arrow(start.square(), start.direction().into());
        game.tick_projectile_arrows();
        assert_eq!(game.arrows().get(&end.square()), Some(&STEP_DOWN.into()));
    }

    #[test]
    fn test_piece_capture_through_portal() {
        let mut game = set_up_10x10_game();
        let enemy_square = point2(5, 5);
        let player_square = point2(2, 2);
        let entrance =
            SquareWithOrthogonalDir::from_square_and_worldstep(enemy_square, STEP_RIGHT.into());
        let exit =
            SquareWithOrthogonalDir::from_square_and_worldstep(player_square, STEP_DOWN.into());
        game.place_single_sided_one_way_portal(entrance, exit);
        game.place_piece(Piece::from_type(OmniDirectionalSoldier), enemy_square);
        game.place_player(player_square);
        assert!(game.player_is_alive());
        game.move_all_pieces();
        assert_false!(game.player_is_alive());
        assert!(game.is_non_player_piece_at(player_square));
    }

    #[test]
    fn test_spear_stab_through_portal() {
        let mut game = set_up_10x10_game();
        let enemy_square = point2(5, 5);
        let player_square = point2(2, 2);
        let entrance =
            SquareWithOrthogonalDir::from_square_and_worldstep(player_square, STEP_RIGHT.into());
        let exit =
            SquareWithOrthogonalDir::from_square_and_worldstep(enemy_square, STEP_DOWN.into());

        game.place_single_sided_one_way_portal(entrance, exit);

        game.place_piece(Piece::from_type(OmniDirectionalSoldier), enemy_square);

        game.place_player(player_square);
        game.player().faced_direction = entrance.direction().into();

        assert_false!(game.pieces.is_empty());
        game.do_player_spear_attack();
        assert!(game.pieces.is_empty());
    }

    #[test]
    fn test_arrow_does_not_turn_toward_player() {
        let mut game = set_up_10x10_game();
        game.place_player(point2(5, 5));
        let arrow_square = point2(3, 5);
        game.place_arrow(arrow_square, STEP_LEFT.into());
        game.move_all_pieces();
        assert!(game.is_arrow_at(arrow_square + STEP_LEFT));
        assert_eq!(
            game.arrows().get(&(arrow_square + STEP_LEFT)),
            Some(&STEP_LEFT.into())
        );
    }

    #[test]
    fn test_see_through_portal() {
        let mut game = set_up_10x10_game();

        let player_square = point2(2, 2);
        game.place_player(player_square);

        let enemy_square = player_square + STEP_UP * 2;
        game.place_piece(Piece::from_type(OmniDirectionalSoldier), enemy_square);

        let entrance = SquareWithOrthogonalDir::from_square_and_worldstep(
            player_square + STEP_RIGHT,
            STEP_RIGHT.into(),
        );
        let exit = SquareWithOrthogonalDir::from_square_and_worldstep(
            enemy_square + STEP_DOWN,
            STEP_UP.into(),
        );
        game.place_single_sided_one_way_portal(entrance, exit);

        game.draw_headless_now();
        let visible_enemy_square = player_square + STEP_RIGHT * 3;


        // game.graphics.print_screen_buffer();

        let fov = game.player_field_of_view();

        assert_eq!(fov.sub_fovs().len(), 1);
        assert_eq!(fov.visibilities_of_absolute_square(enemy_square).len(), 2);
        assert_eq!(
            game.graphics
                .screen
                .get_screen_glyphs_at_world_square(visible_enemy_square)
                .to_clean_string(),
            game.graphics
                .get_drawable_for_square_from_draw_buffer(enemy_square)
                .unwrap()
                .to_glyphs()
                .to_clean_string()
        );
    }

    #[test]
    fn test_observed_crash_from_one_pillar_shadow() {
        let mut game = set_up_nxn_game(20);
        let player_square = point2(0, 0);
        let block_offset = vec2(14, -5);
        game.place_player(player_square);
        game.place_block(player_square + block_offset);
        game.draw_headless_now();
        // shouldn't crash
    }

    #[test]
    fn test_observed_crash_from_seeing_back_of_portal() {
        let mut game = set_up_10x10_game();
        game.place_player(point2(5, 5));
        let block_square = game.player_square() + STEP_RIGHT * 4;
        let entrance_square = block_square + STEP_UP_RIGHT;
        let exit_square = block_square + STEP_DOWN_RIGHT * 4;
        game.place_single_sided_one_way_portal(
            SquareWithOrthogonalDir::from_square_and_step(entrance_square, STEP_DOWN.into()),
            SquareWithOrthogonalDir::from_square_and_step(exit_square, STEP_LEFT.into()),
        );
        game.draw_headless_now();
    }

    #[test]
    fn test_observed_crash_from_being_near_portal() {
        let mut game = set_up_10x10_game();
        game.place_player(point2(5, 5));
        game.place_single_sided_one_way_portal(
            SquareWithOrthogonalDir::from_square_and_step(
                game.player_square() + STEP_DOWN_RIGHT,
                STEP_DOWN.into(),
            ),
            SquareWithOrthogonalDir::from_square_and_step(
                game.player_square() + STEP_DOWN_LEFT * 3,
                STEP_RIGHT.into(),
            ),
        );
        game.draw_headless_now();
    }

    #[test]
    fn test_observed_crash_from_being_near_portal_2() {
        let mut game = set_up_nxn_game(20);
        game.place_player(point2(5, 5));
        game.place_single_sided_one_way_portal(
            SquareWithOrthogonalDir::from_square_and_step(
                game.player_square() + STEP_DOWN_LEFT,
                STEP_DOWN.into(),
            ),
            SquareWithOrthogonalDir::from_square_and_step(
                game.player_square() + STEP_DOWN_LEFT * 3,
                STEP_RIGHT.into(),
            ),
        );
        game.draw_headless_now();
    }

    #[test]
    fn test_see_through_portal_while_next_to_it() {
        let mut game = set_up_10x10_game();

        let player_square = point2(5, 5);
        game.place_player(player_square);

        let enemy_square = player_square + STEP_RIGHT * 2;
        game.place_piece(Piece::from_type(OmniDirectionalSoldier), enemy_square);

        let entrance =
            SquareWithOrthogonalDir::from_square_and_worldstep(player_square, STEP_RIGHT.into());
        let exit =
            SquareWithOrthogonalDir::from_square_and_worldstep(enemy_square, STEP_RIGHT.into());
        game.place_single_sided_one_way_portal(entrance, exit);

        game.draw_headless_now();
        let correct_apparent_enemy_square = player_square + STEP_RIGHT;


        // game.graphics.print_screen_buffer();

        let fov = game.player_field_of_view();

        assert_eq!(fov.sub_fovs().len(), 1);
        assert_eq!(fov.visibilities_of_absolute_square(enemy_square).len(), 1);
        assert_eq!(
            game.graphics
                .screen
                .get_screen_glyphs_at_world_square(correct_apparent_enemy_square)
                .to_clean_string(),
            game.graphics
                .get_drawable_for_square_from_draw_buffer(enemy_square)
                .unwrap()
                .to_glyphs()
                .to_clean_string()
        );
    }

    #[test]
    fn test_portals_causing_shadows_at_certain_angles() {
        let mut game = set_up_nxn_game(20);

        let player_square = point2(10, 10);
        game.place_player(player_square);

        let entrance = SquareWithOrthogonalDir::from_square_and_worldstep(
            player_square + STEP_DOWN_RIGHT * 3,
            STEP_RIGHT.into(),
        );
        let exit = entrance.with_offset(STEP_RIGHT * 2);
        game.place_single_sided_one_way_portal(entrance, exit);

        game.draw_headless_now();
        let square_that_should_be_visible = entrance.square() + STEP_DOWN_LEFT;


        // game.graphics.screen.print_screen_buffer();

        let fov = game.player_field_of_view();

        assert_eq!(
            fov.visibilities_of_absolute_square(square_that_should_be_visible)
                .len(),
            1
        );
        assert_ne!(
            game.graphics
                .get_drawable_for_square_from_draw_buffer(square_that_should_be_visible)
                .unwrap()
                .to_glyphs()
                .get_solid_color()
                .unwrap(),
            OUT_OF_SIGHT_COLOR
        );
    }

    #[test]
    fn test_rotate_screen() {
        let mut game = set_up_nxn_game(20);

        let player_square = point2(10, 10);
        game.place_player(player_square);
        game.raw_set_player_faced_direction(STEP_RIGHT.into());

        let step_to_enemy = STEP_RIGHT * 2;

        let enemy_square = player_square + step_to_enemy;
        game.place_piece(Piece::from_type(OmniDirectionalSoldier), enemy_square);

        game.draw_headless_now();
        let enemy_chars = game
            .graphics
            .get_drawable_for_square_from_draw_buffer(enemy_square)
            .unwrap()
            .to_glyphs()
            .to_clean_string();

        let player_square = game.player_square();

        assert_eq!(
            game.graphics
                .screen
                .get_screen_glyphs_at_world_square(enemy_square)
                .to_clean_string(),
            enemy_chars
        );

        let player_screen_char_square = game
            .graphics
            .screen
            .world_square_to_left_screen_buffer_character_square(player_square);

        assert_eq!(
            game.graphics
                .screen
                .get_screen_buffered_glyph(player_screen_char_square + STEP_RIGHT.cast_unit() * 4)
                .character,
            enemy_chars.chars().collect_vec()[0]
        );

        game.graphics
            .screen
            .rotate(QuarterTurnsAnticlockwise::new(3));

        game.draw_headless_now();

        assert_eq!(
            game.graphics
                .screen
                .get_screen_glyphs_at_world_square(enemy_square)
                .to_clean_string(),
            enemy_chars
        );
        let player_screen_char_square = game
            .graphics
            .screen
            .world_square_to_left_screen_buffer_character_square(player_square);

        assert_eq!(
            game.graphics
                .screen
                .get_screen_buffered_glyph(
                    player_screen_char_square + SCREEN_STEP_UP.cast_unit() * 2
                )
                .character,
            enemy_chars.chars().collect_vec()[0]
        );
    }

    #[test]
    fn test_screen_rotates_when_stepping_through_portal() {
        let mut game = set_up_10x10_game();
        let player_square = point2(5, 5);
        game.place_player(player_square);
        let entrance_step =
            SquareWithOrthogonalDir::from_square_and_worldstep(player_square, STEP_RIGHT.into());
        let exit_step = SquareWithOrthogonalDir::from_square_and_worldstep(
            player_square + STEP_DOWN_RIGHT * 3,
            STEP_UP.into(),
        );
        game.place_single_sided_two_way_portal(entrance_step, exit_step);

        game.draw_headless_now();
        assert_eq!(
            game.graphics.screen.rotation(),
            QuarterTurnsAnticlockwise::new(0)
        );

        game.try_slide_player_by_direction(STEP_RIGHT.into(), 1)
            .ok();
        assert_eq!(
            game.portal_aware_single_step(entrance_step.into()).unwrap(),
            exit_step.into()
        );

        game.draw_headless_now();
        assert_eq!(
            game.graphics.screen.rotation(),
            QuarterTurnsAnticlockwise::new(1)
        );
    }

    #[test]
    fn test_move_relative_to_screen() {
        let mut game = set_up_10x10_game();
        let player_square = point2(5, 5);
        game.place_player(player_square);
        game.graphics
            .screen
            .rotate(QuarterTurnsAnticlockwise::new(1));
        game.try_slide_player_relative_to_screen(SCREEN_STEP_UP)
            .expect("slide");
        assert_eq!(game.player_square(), player_square + STEP_LEFT);
    }

    #[test]
    fn test_blink_relative_to_screen() {
        let mut game = set_up_10x10_game();
        let player_square = point2(5, 5);
        game.place_player(player_square);
        game.graphics
            .screen
            .rotate(QuarterTurnsAnticlockwise::new(1));
        game.player_blink_relative_to_screen(SCREEN_STEP_UP);
        assert_eq!(game.player_square().y, player_square.y);
        assert!(game.player_square().x < player_square.x);
    }

    #[test]
    fn test_rotated_shadows() {
        let player_square = point2(5, 5);

        let mut unrotated_game = set_up_10x10_game();
        let mut rotated_game = set_up_10x10_game();
        rotated_game
            .graphics
            .screen
            .rotate(QuarterTurnsAnticlockwise::new(-1));

        let mut games = [unrotated_game, rotated_game];
        let shadow_glyphs: Vec<DoubleGlyph> = games
            .iter_mut()
            .map(|game: &mut Game| {
                game.place_player(player_square);
                let right_of_player = player_square
                    + game
                        .graphics
                        .screen
                        .screen_step_to_world_step(SCREEN_STEP_RIGHT);

                game.place_block(right_of_player);
                game.draw_headless_now();

                let shadow_screen_square = game
                    .graphics
                    .screen
                    .world_square_to_screen_buffer_square(player_square)
                    + SCREEN_STEP_UP_RIGHT;
                game.graphics
                    .screen
                    .get_glyphs_at_screen_square(shadow_screen_square)
            })
            .collect_vec();
        assert_eq!(
            shadow_glyphs[0].to_clean_string(),
            shadow_glyphs[1].to_clean_string()
        );
    }

    #[test]
    fn test_can_fail_to_walk_into_block_without_crashing() {
        let mut game = set_up_10x10_game();
        game.place_player(point2(5, 5));
        game.place_block(game.player_square() + STEP_RIGHT);
        game.try_slide_player_relative_to_screen(SCREEN_STEP_RIGHT)
            .ok();
    }

    #[test]
    fn test_partial_shadows_are_drawn() {
        let player_square = point2(5, 5);
        let mut game = set_up_10x10_game();
        game.place_player(player_square);
        let right_of_player = player_square
            + game
                .graphics
                .screen
                .screen_step_to_world_step(SCREEN_STEP_RIGHT);

        game.place_block(right_of_player);
        game.draw_headless_now();

        let shadow_screen_square = game
            .graphics
            .screen
            .world_square_to_screen_buffer_square(player_square)
            + SCREEN_STEP_UP_RIGHT;
        let glyphs = game
            .graphics
            .screen
            .get_glyphs_at_screen_square(shadow_screen_square);
        assert_eq!("🭞🭚", glyphs.to_clean_string());
        assert_ne!(glyphs[0].fg_color, glyphs[1].bg_color);
    }

    fn set_up_player_just_left_of_portal_through_wall() -> Game {
        let player_square = point2(5, 5);
        let mut game = set_up_10x10_game();
        game.place_player(player_square);

        let entrance_square = player_square;
        let exit_square = entrance_square + STEP_RIGHT * 2;
        let entrance =
            SquareWithOrthogonalDir::from_square_and_step(entrance_square, STEP_RIGHT.into());
        let exit = SquareWithOrthogonalDir::from_square_and_step(exit_square, STEP_RIGHT.into());
        game.place_double_sided_two_way_portal(entrance, exit);

        let r = 2;
        let n = 2 * r + 1;
        (0..n).for_each(|i| {
            game.place_block(entrance_square + STEP_RIGHT + STEP_UP * r + STEP_DOWN * i)
        });
        game
    }

    #[test]
    fn test_do_not_see_block_through_portal() {
        let mut game = set_up_player_just_left_of_portal_through_wall();

        game.draw_headless_now();

        game.graphics.screen.print_screen_buffer();

        let up_right_glyphs = game
            .graphics
            .screen
            .get_screen_glyphs_at_visual_offset_from_center(SCREEN_STEP_UP_RIGHT);
        assert_false!(up_right_glyphs.looks_solid());
    }

    #[test]
    fn test_freestanding_portals_can_be_seamless() {
        let player_square = point2(5, 5);
        let mut game = set_up_10x10_game();
        game.place_player(player_square);
        game.graphics.tint_portals = false;

        let entrance_square = game.player_square();
        let exit_square = entrance_square + STEP_RIGHT * 3;
        let entrance =
            SquareWithOrthogonalDir::from_square_and_step(entrance_square, STEP_RIGHT.into());
        let exit = SquareWithOrthogonalDir::from_square_and_step(exit_square, STEP_RIGHT.into());
        game.place_double_sided_two_way_portal(entrance, exit);

        game.draw_headless_now();

        game.graphics.screen.print_screen_buffer();

        let glyphs = game
            .graphics
            .screen
            .get_screen_glyphs_at_visual_offset_from_center(SCREEN_STEP_UP_RIGHT);

        assert!(glyphs.looks_solid());
    }

    fn assert_screen_is_stable(mut game: &mut Game, n: usize) {
        let consecutive_frames = (0..n)
            .map(|_i| {
                game.draw_headless_now();
                game.graphics.screen.current_screen_state.clone()
            })
            .collect_vec();

        assert_eq!(consecutive_frames.len(), n);

        let mut the_iter = consecutive_frames.iter();
        let first_frame = the_iter.next().unwrap();
        the_iter.for_each(|f| {
            if f != first_frame {
                Screen::print_buffer_of_glyphs(first_frame);
                Screen::print_buffer_of_glyphs(f);
            }
            assert_eq!(f, first_frame)
        });

        assert!(consecutive_frames.iter().all_equal());
    }

    #[test]
    fn test_portal_edges_are_stable_dense_horizontal() {
        let player_square = point2(0, 5);
        let mut game = set_up_nxm_game(10, 30);
        game.place_player(player_square);

        game.place_dense_horizontal_portals(player_square, 3, 6);
        assert_screen_is_stable(&mut game, 5);
    }

    #[test]
    fn test_portal_edges_are_stable_simple_case() {
        let player_square = point2(0, 2);
        let mut game = set_up_nxm_game(5, 5);
        game.place_player(player_square);

        game.place_offset_rightward_double_sided_two_way_portal(player_square, STEP_RIGHT);

        assert_screen_is_stable(&mut game, 5);
    }

    #[test]
    fn test_portal_edges_are_stable_two_deep() {
        let player_square = point2(0, 2);
        let mut game = set_up_nxm_game(5, 10);
        game.place_player(player_square);

        game.place_dense_horizontal_portals(player_square, 1, 2);

        assert_screen_is_stable(&mut game, 10);
    }

    fn place_portal_capping_block_bar(game: &mut Game, square: WorldSquare, length: u32) {
        game.place_offset_rightward_double_sided_two_way_portal(
            square + STEP_LEFT,
            STEP_RIGHT * length as i32,
        );
        (0..length)
            .map(|i| square + STEP_RIGHT * i as i32)
            .for_each(|abs_square| game.place_block(abs_square));
    }

    #[test]
    fn test_portal_drawn_in_correct_order_over_partially_visible_block() {
        let player_square = point2(5, 5);
        let test_square = SCREEN_STEP_DOWN_RIGHT + SCREEN_STEP_RIGHT * 2;

        let mut game = set_up_nxm_game(10, 20);
        game.place_player(player_square);
        place_portal_capping_block_bar(&mut game, player_square + STEP_DOWN_RIGHT + STEP_RIGHT, 5);

        game.draw_headless_now();

        game.graphics.screen.print_screen_buffer();
        let test_glyphs = game
            .graphics
            .screen
            .get_screen_glyphs_at_visual_offset_from_center(test_square);

        assert_eq!(test_glyphs.to_clean_string(), "🬹🭑");
        assert_eq!(test_glyphs.map(|g| g.bg_color), [BLOCK_FG; 2]);
    }

    #[ignore = "May or may not want this"]
    #[test]
    fn test_shadow_arc_squashed_by_portal_still_looks_connected() {
        let player_square = point2(5, 5);
        let test_square = SCREEN_STEP_DOWN_RIGHT + SCREEN_STEP_RIGHT * 3;

        let mut game = set_up_nxm_game(10, 10);
        game.place_player(player_square);

        let bar_left_end = player_square + STEP_DOWN_RIGHT + STEP_RIGHT;
        game.place_offset_rightward_double_sided_two_way_portal(
            bar_left_end + STEP_LEFT,
            STEP_RIGHT * 20,
        );
        (0..2)
            .map(|i| bar_left_end + STEP_RIGHT * i)
            .for_each(|abs_square| game.place_block(abs_square));

        game.draw_headless_now();

        game.graphics.screen.print_screen_buffer();
        let test_glyphs = game
            .graphics
            .screen
            .get_screen_glyphs_at_visual_offset_from_center(test_square);

        todo!();
        assert_eq!(test_glyphs.to_clean_string(), "🬹🭑");
    }

    #[test]
    fn test_set_floor_color() {
        let mut game = set_up_nxm_game(10, 10);
        game.draw_headless_now();
        let default_floor_color: RGB8 = game
            .graphics
            .screen
            .get_screen_glyphs_at_visual_offset_from_center(SCREEN_STEP_ZERO)
            .get_solid_color()
            .unwrap();
        assert_ne!(default_floor_color, RED);

        game.graphics.set_solid_floor_color(RED);
        game.draw_headless_now();
        let new_floor_color: RGB8 = game
            .graphics
            .screen
            .get_screen_glyphs_at_visual_offset_from_center(SCREEN_STEP_ZERO)
            .get_solid_color()
            .unwrap();
        assert_eq!(new_floor_color, RED);
    }

    #[test]
    fn test_portals_tint_views() {
        let test_square: ScreenBufferStep = SCREEN_STEP_RIGHT * 3;
        let player_square: WorldSquare = point2(3, 3);
        let mut game = set_up_10x10_game();
        game.place_player(player_square);
        game.graphics.set_solid_floor_color(GREY);
        game.draw_headless_now();
        game.graphics.screen.print_screen_buffer();
        let get_color = |game: &Game| -> RGB8 {
            game.graphics
                .screen
                .get_screen_glyphs_at_visual_offset_from_center(test_square)
                .get_solid_color()
                .unwrap()
        };
        let start_color: RGB8 = get_color(&game);

        assert_eq!(start_color.b, start_color.r);

        game.place_offset_rightward_double_sided_two_way_portal(player_square, STEP_UP * 5);
        game.draw_headless_now();
        game.graphics.screen.print_screen_buffer();

        let end_color: RGB8 = get_color(&game);

        assert!(end_color.b < end_color.r, "Should tint red");
    }

    #[test]
    fn test_horizontal_wide_portal_has_no_internal_defects() {
        let mut game = set_up_nxm_game(30, 30);
        let player_square: WorldSquare = point2(3, 15);
        game.place_player(player_square);

        let entrance = SquareWithOrthogonalDir::from_square_and_worldstep(
            player_square + STEP_RIGHT * 2 + STEP_UP * 2,
            STEP_RIGHT.into(),
        );
        let exit = entrance.stepped_n(5);
        game.place_wide_portal(entrance, exit, 5);

        game.draw_headless_now();
        game.graphics.screen.print_screen_buffer();

        let top_left_test_square = SCREEN_STEP_RIGHT * 3;

        (0..4).for_each(|dx| {
            (0..3).for_each(|dy_down| {
                let test_square =
                    top_left_test_square + SCREEN_STEP_RIGHT * dx + SCREEN_STEP_DOWN * dy_down;
                let glyphs = game
                    .graphics
                    .screen
                    .get_screen_glyphs_at_visual_offset_from_center(test_square);
                assert!(glyphs.looks_solid(), "offset: ({},{})", dx, dy_down);
            })
        });
    }

    #[ignore = "TODO"]
    #[test]
    fn test_horizontal_wide_portal_has_smooth_edge() {
        let mut game = set_up_nxm_game(16, 10);
        let player_square: WorldSquare = point2(3, game.board_size.height as i32 / 2);
        game.place_player(player_square);

        let entrance = SquareWithOrthogonalDir::from_square_and_worldstep(
            player_square + STEP_RIGHT * 0 + STEP_UP * 2,
            STEP_RIGHT.into(),
        );
        let exit = entrance.stepped_n(2);
        game.place_wide_portal(entrance, exit, 5);

        game.draw_headless_now();
        game.graphics.screen.print_screen_buffer();
        let test_relative_squares_on_screen_top_to_bottom = vec![
            ScreenBufferStep::new(1, -4),
            ScreenBufferStep::new(1, -3),
            ScreenBufferStep::new(1, 3),
            ScreenBufferStep::new(1, 4),
        ];
        let test_steps_in_world_top_to_bottom = test_relative_squares_on_screen_top_to_bottom
            .iter()
            .map(|&screen_step| game.graphics.screen.screen_step_to_world_step(screen_step))
            .collect_vec();

        let fov = game.player_field_of_view();

        print_fov_as_absolute(&fov, 5);
        dbg!(
            "asdf",
            test_steps_in_world_top_to_bottom
                .iter()
                .map(|&step| fov.visibilities_of_relative_square(step))
                .collect_vec(),
        );

        test_steps_in_world_top_to_bottom
            .iter()
            .map(|&world_step| fov.visibilities_of_relative_square(world_step))
            .for_each(
                |visibilities_of_rel_square: Vec<PositionedSquareVisibilityInFov>| {
                    let vis1 = visibilities_of_rel_square[0].square_visibility_in_absolute_frame();
                    let vis2 = visibilities_of_rel_square[1].square_visibility_in_absolute_frame();
                    assert!(vis1.is_about_complementary_to(vis2));
                    assert!(vis1.is_visually_complementary_to(vis2));
                },
            );

        let correct_strings_top_to_bottom = vec!["🭋█", "🭅█", "🭖█", "🭦█"];

        (0..test_relative_squares_on_screen_top_to_bottom.len()).for_each(|i| {
            assert_eq!(
                game.graphics
                    .screen
                    .get_screen_glyphs_at_visual_offset_from_center(
                        test_relative_squares_on_screen_top_to_bottom[i]
                    )
                    .to_clean_string(),
                correct_strings_top_to_bottom[i]
            )
        });
    }

    #[test]
    fn test_vertical_wide_portals_have_no_internal_defects() {
        let mut game = set_up_nxm_game(30, 30);
        let player_square: WorldSquare = point2(15, 5);
        game.place_player(player_square);

        let entrance = SquareWithOrthogonalDir::from_square_and_worldstep(
            player_square + STEP_UP * 2 + STEP_LEFT * 2,
            STEP_UP.into(),
        );
        let exit = entrance.stepped_n(3);
        game.place_wide_portal(entrance, exit, 5);
        game.draw_headless_now();
        game.graphics.screen.print_screen_buffer();

        let bottom_left_test_square = SCREEN_STEP_UP * 3;

        (0..3).for_each(|dx| {
            (0..4).for_each(|dy| {
                let test_square =
                    bottom_left_test_square + SCREEN_STEP_RIGHT * dx + SCREEN_STEP_UP * dy;
                let glyphs = game
                    .graphics
                    .screen
                    .get_screen_glyphs_at_visual_offset_from_center(test_square);
                assert!(glyphs.looks_solid(), "offset: ({},{})", dx, dy);
            })
        });
    }

    #[ignore = "TODO"]
    #[test]
    fn test_vertical_wide_portals_have_smooth_edges() {
        let mut game = set_up_nxm_game(30, 30);
        let player_square: WorldSquare = point2(15, 5);
        game.place_player(player_square);

        let entrance = SquareWithOrthogonalDir::from_square_and_worldstep(
            player_square + STEP_UP * 0 + STEP_LEFT * 2,
            STEP_UP.into(),
        );
        let exit = entrance.stepped_n(3);
        game.place_wide_portal(entrance, exit, 5);
        game.draw_headless_now();
        game.graphics.screen.print_screen_buffer();

        let test_squares_left_to_right = vec![
            ScreenBufferStep::new(-4, -1),
            ScreenBufferStep::new(-3, -1),
            ScreenBufferStep::new(3, -1),
            ScreenBufferStep::new(4, -1),
        ];
        let correct_strings_left_to_right = vec!["🬎🬎", "🭓█", "█🭞", "🬎🬎"];

        let bottom_left_test_square = SCREEN_STEP_UP * 3;

        (0..test_squares_left_to_right.len()).for_each(|i| {
            assert_eq!(
                game.graphics
                    .screen
                    .get_screen_glyphs_at_visual_offset_from_center(test_squares_left_to_right[i])
                    .to_clean_string(),
                correct_strings_left_to_right[i]
            )
        });
    }

    #[test]
    fn test_rotated_wide_portals_have_no_internal_defects() {
        let mut game = set_up_nxm_game(30, 30);
        let player_square: WorldSquare = point2(5, 5);
        game.place_player(player_square);

        let width = 5;
        let entrance = SquareWithOrthogonalDir::from_square_and_worldstep(
            player_square + STEP_RIGHT * 2 + STEP_UP * 2,
            STEP_RIGHT.into(),
        );
        let exit = SquareWithOrthogonalDir::from_square_and_worldstep(
            entrance.square() + STEP_UP + STEP_RIGHT * 3,
            STEP_UP.into(),
        );
        (0..width).for_each(|i| {
            game.place_double_sided_two_way_portal(
                entrance.strafed_right_n(i),
                exit.strafed_right_n(i),
            );
        });
        game.draw_headless_now();
        game.graphics.screen.print_screen_buffer();

        let top_left_test_square = SCREEN_STEP_RIGHT * 3;

        (0..4).for_each(|dx| {
            (-2..3).for_each(|dy| {
                let test_square =
                    top_left_test_square + SCREEN_STEP_RIGHT * dx + SCREEN_STEP_UP * dy;
                let glyphs = game
                    .graphics
                    .screen
                    .get_screen_glyphs_at_visual_offset_from_center(test_square);
                assert!(glyphs.looks_solid(), "offset: ({},{})", dx, dy);
            })
        })
    }

    #[test]
    fn test_turning_wide_portal_specific_defect() {
        let mut game = set_up_nxm_game(30, 30);
        let player_square: WorldSquare = point2(5, 15);
        game.place_player(player_square);

        let width = 3;
        (0..width).for_each(|i| {
            let entrance = SquareWithOrthogonalDir::from_square_and_worldstep(
                player_square + STEP_RIGHT * 2 + STEP_UP,
                STEP_RIGHT.into(),
            )
            .strafed_right_n(i);
            let exit = SquareWithOrthogonalDir::from_square_and_worldstep(
                game.player_square() + STEP_UP * 8 + STEP_RIGHT,
                STEP_UP.into(),
            )
            .strafed_right_n(i);
            game.place_double_sided_two_way_portal(entrance, exit);
        });

        game.populate_draw_buffer(Instant::now());

        let rel_square = STEP_RIGHT * 3 + STEP_UP;
        let player_fov = game.player_field_of_view();
        let visibilities_at_rel_square = player_fov.visibilities_of_relative_square(rel_square);
        assert_eq!(visibilities_at_rel_square.len(), 1);
        let maybe_drawable: Option<DrawableEnum> = game
            .graphics
            .maybe_drawable_for_rel_square_of_fov(&player_fov, rel_square);
        let glyphs = maybe_drawable.unwrap().to_glyphs();
        assert!(
            glyphs.looks_solid(),
            "The glyphs: {}, the chars: {}",
            glyphs.to_string(),
            glyphs.to_clean_string()
        );
    }

    #[test]
    fn test_player_rotates_with_portal() {
        let mut game = set_up_10x10_game();
        let player_square = point2(5, 5);
        game.place_player_with_direction(player_square, STEP_RIGHT);
        game.place_single_sided_one_way_portal(
            SquareWithOrthogonalDir::from_square_and_step(player_square, STEP_RIGHT),
            SquareWithOrthogonalDir::from_square_and_step(player_square + STEP_UP_RIGHT, STEP_UP),
        );
        game.draw_headless_now();
        //game.graphics.screen.print_screen_buffer();
        let before_glyphs = game
            .graphics
            .screen
            .get_screen_glyphs_at_visual_offset_from_center(SCREEN_STEP_ZERO);
        game.try_slide_player(STEP_RIGHT).expect("");
        game.draw_headless_now();
        //game.graphics.screen.print_screen_buffer();
        let after_glyphs = game
            .graphics
            .screen
            .get_screen_glyphs_at_visual_offset_from_center(SCREEN_STEP_ZERO);

        assert_eq!(
            before_glyphs.to_clean_string(),
            after_glyphs.to_clean_string()
        );
    }

    #[test]
    fn test_move_vertical_looks_correct() {
        let mut game = set_up_10x10_game();
        game.place_player_with_direction(point2(5, 5), STEP_UP);
        game.draw_headless_now();
        let before_glyphs = game
            .graphics
            .screen
            .get_screen_glyphs_at_visual_offset_from_center(SCREEN_STEP_ZERO);
        game.try_slide_player_relative_to_screen(SCREEN_STEP_UP)
            .expect("");
        let after_glyphs = game
            .graphics
            .screen
            .get_screen_glyphs_at_visual_offset_from_center(SCREEN_STEP_ZERO);
        assert_eq!(
            before_glyphs.to_clean_string(),
            after_glyphs.to_clean_string()
        );
        assert_eq!(
            before_glyphs.to_clean_string(),
            Glyph::get_glyphs_for_player(STEP_UP.into()).to_clean_string()
        );
    }

    #[test]
    fn test_player_seen_through_portal_is_rotated_correctly() {
        let mut game = set_up_10x10_game();
        let player_square = point2(5, 5);
        game.place_player_with_direction(player_square, STEP_RIGHT);
        game.place_single_sided_one_way_portal(
            SquareWithOrthogonalDir::from_square_and_step(player_square, STEP_RIGHT),
            SquareWithOrthogonalDir::from_square_and_step(player_square, STEP_DOWN),
        );
        game.draw_headless_now();
        game.graphics.screen.print_screen_buffer();
        let player_glyphs = game
            .graphics
            .screen
            .get_screen_glyphs_at_visual_offset_from_center(SCREEN_STEP_ZERO);
        let seen_glyphs = game
            .graphics
            .screen
            .get_screen_glyphs_at_visual_offset_from_center(SCREEN_STEP_RIGHT);

        assert_ne!(
            player_glyphs.to_clean_string(),
            seen_glyphs.to_clean_string()
        );
        assert_eq!(seen_glyphs[0].character, '🢁');
    }

    #[test]
    fn test_player_diagonal_step_around_adjacent_portal() {
        for orthodir in ORTHOGONAL_STEPS {
            [-1, 1].into_iter().for_each(|i| {
                let otherdir = rotated_n_quarter_turns_counter_clockwise(orthodir, i);
                let diagdir = orthodir + otherdir;

                let mut game = set_up_nxm_game(10, 25);
                let start_square = point2(5, 5);
                game.place_player(start_square);
                game.place_single_sided_one_way_portal(
                    SquareWithOrthogonalDir::from_square_and_step(game.player_square(), orthodir),
                    SquareWithOrthogonalDir::from_square_and_step(
                        game.player_square() + STEP_RIGHT * 10,
                        STEP_UP,
                    ),
                );
                game.try_slide_player(diagdir).expect("slide");
                assert_eq!(game.player_square(), start_square + diagdir);
            });
        }
    }

    #[test]
    fn test_player_diagonal_step_into_off_adjacent_portal() {
        for orthodir in ORTHOGONAL_STEPS {
            for i in [-1, 1] {
                let strafedir = rotated_n_quarter_turns_counter_clockwise(orthodir, i);
                let diagdir = orthodir + strafedir;

                let mut game = set_up_nxm_game(10, 25);
                let start_square = point2(5, 5);
                game.place_player(start_square);
                let far_square = game.player_square() + STEP_RIGHT * 10;
                game.place_single_sided_one_way_portal(
                    SquareWithOrthogonalDir::from_square_and_step(
                        game.player_square() + strafedir,
                        orthodir,
                    ),
                    SquareWithOrthogonalDir::from_square_and_step(far_square, STEP_UP),
                );
                game.try_slide_player(diagdir).expect("slide");
                assert_eq!(game.player_square(), far_square);
            }
        }
    }

    #[test]
    fn test_player_diagonal_step_into_matching_convex_corner_portal() {
        for left_orthodir in ORTHOGONAL_STEPS {
            let right_orthodir = rotated_n_quarter_turns_counter_clockwise(left_orthodir, -1);
            let diagonaldir = left_orthodir + right_orthodir;
            let mut game = set_up_10x10_game();
            let start_square = point2(5, 5);
            game.place_player(start_square);

            let left_entrance = SquareWithOrthogonalDir::from_square_and_step(
                start_square + left_orthodir,
                right_orthodir,
            );
            let exit_offset = 3;
            let left_exit = left_entrance.stepped().strafed_left_n(exit_offset);
            let right_entrance = SquareWithOrthogonalDir::from_square_and_step(
                start_square + right_orthodir,
                left_orthodir,
            );
            let right_exit = right_entrance.stepped().stepped_n(exit_offset);

            game.place_single_sided_one_way_portal(left_entrance, left_exit);
            game.place_single_sided_one_way_portal(right_entrance, right_exit);

            game.try_slide_player(diagonaldir).expect("should slide");
            assert_eq!(game.player_square(), left_exit.square());
        }
    }

    #[test]
    fn test_player_diagonal_step_into_mismatched_convex_corner_portal() {
        for left_orthodir in ORTHOGONAL_STEPS {
            let right_orthodir = rotated_n_quarter_turns_counter_clockwise(left_orthodir, -1);
            let diagonaldir = left_orthodir + right_orthodir;
            let mut game = set_up_10x10_game();
            let start_square = point2(5, 5);
            game.place_player(start_square);

            let left_entrance = SquareWithOrthogonalDir::from_square_and_step(
                start_square + left_orthodir,
                right_orthodir,
            );
            let exit_offset = 3;
            let left_exit = left_entrance.stepped().strafed_left_n(exit_offset);
            let right_entrance = SquareWithOrthogonalDir::from_square_and_step(
                start_square + right_orthodir,
                left_orthodir,
            );
            let right_exit = right_entrance.stepped().strafed_right_n(exit_offset);

            game.place_single_sided_one_way_portal(left_entrance, left_exit);
            game.place_single_sided_one_way_portal(right_entrance, right_exit);

            game.try_slide_player(diagonaldir)
                .expect_err("should not slide");
            assert_eq!(game.player_square(), start_square);
        }
    }

    #[test]
    fn test_player_diagonal_step_into_matching_concave_corner_portal() {
        for left_orthodir in ORTHOGONAL_STEPS {
            let right_orthodir = rotated_n_quarter_turns_counter_clockwise(left_orthodir, -1);
            let diagonaldir = left_orthodir + right_orthodir;
            let mut game = set_up_10x10_game();
            let start_square = point2(5, 5);
            game.place_player(start_square);

            let left_entrance =
                SquareWithOrthogonalDir::from_square_and_step(start_square, left_orthodir);
            let exit_offset = 3;
            let left_exit = left_entrance.stepped().strafed_right_n(exit_offset);
            let right_entrance =
                SquareWithOrthogonalDir::from_square_and_step(start_square, right_orthodir);
            let right_exit = right_entrance.stepped().stepped_n(exit_offset);

            game.place_single_sided_one_way_portal(left_entrance, left_exit);
            game.place_single_sided_one_way_portal(right_entrance, right_exit);

            game.try_slide_player(diagonaldir).expect("should slide");
            assert_eq!(game.player_square(), left_exit.strafed_right().square());
        }
    }

    #[test]
    fn test_player_diagonal_step_into_mismatched_concave_corner_portal() {
        for left_orthodir in ORTHOGONAL_STEPS {
            let right_orthodir = rotated_n_quarter_turns_counter_clockwise(left_orthodir, -1);
            let diagonaldir = left_orthodir + right_orthodir;
            let mut game = set_up_10x10_game();
            let start_square = point2(5, 5);
            game.place_player(start_square);

            let left_entrance =
                SquareWithOrthogonalDir::from_square_and_step(start_square, left_orthodir);
            let exit_offset = 3;
            let left_exit = left_entrance.stepped().stepped_n(exit_offset);
            let right_entrance =
                SquareWithOrthogonalDir::from_square_and_step(start_square, right_orthodir);
            let right_exit = right_entrance.stepped().stepped_n(exit_offset);

            game.place_single_sided_one_way_portal(left_entrance, left_exit);
            game.place_single_sided_one_way_portal(right_entrance, right_exit);

            game.try_slide_player(diagonaldir)
                .expect_err("should not slide");
            assert_eq!(game.player_square(), start_square);
        }
    }

    #[test]
    fn test_push_widget() {
        let mut game = set_up_10x10_game();
        let start_square = point2(5, 5);
        game.place_player(start_square);
        let widget_val = 4;
        game.place_widget(Widget::new(widget_val), start_square + STEP_RIGHT);
        game.try_slide_player(STEP_RIGHT).expect("should slide");
        assert_eq!(game.player_square(), start_square + STEP_RIGHT);
        let correct_widget_end_square = start_square + STEP_RIGHT * 2;
        assert!(game.widgets.contains_key(&correct_widget_end_square));
        assert_eq!(
            game.widgets.get(&correct_widget_end_square).unwrap().val(),
            widget_val
        );
    }

    #[test]
    fn test_draw_widget() {
        let mut game = set_up_10x10_game();
        let start_square = point2(5, 5);
        game.place_player(start_square);
        let widget_val = 4;
        game.place_widget(Widget::new(widget_val), start_square + STEP_RIGHT);
        game.draw_headless_now();
        let widget_glyphs = game
            .graphics
            .screen
            .get_screen_glyphs_at_visual_offset_from_center(SCREEN_STEP_RIGHT);
        assert_eq!(widget_glyphs.to_clean_string(), "④ ")
    }

    #[test]
    fn test_draw_floor_arrows() {
        let mut game = set_up_10x10_game();
        let start_square = point2(5, 5);
        game.place_player(start_square);
        let pushable_square = start_square + STEP_RIGHT;
        game.place_floor_push_arrow(pushable_square, STEP_UP);
        game.draw_headless_now();
        let glyphs = game
            .graphics
            .screen
            .get_screen_glyphs_at_visual_offset_from_center(SCREEN_STEP_RIGHT);
        //⯬ ⯭ ⯮ ⯯
        assert_eq!(glyphs.to_clean_string(), "⯭ ")
    }

    #[test]
    fn test_floor_arrows_push_widget() {
        let mut game = set_up_10x10_game();
        let start_square = point2(5, 5);
        //game.place_player(start_square);
        let widget_square = start_square + STEP_RIGHT;
        game.place_widget(Widget::new(4), widget_square);
        game.place_floor_push_arrow(widget_square, STEP_UP);

        assert!(game.widgets.contains_key(&widget_square));
        game.tick_game_logic();
        assert!(game.widgets.contains_key(&(widget_square + STEP_UP)));
    }

    #[test]
    fn test_widget_visible_next_to_turning_portal() {
        let mut game = set_up_10x10_game();
        let start_square = point2(2, 2);
        game.place_player(start_square);
        let widget_square = start_square + STEP_RIGHT * 2 + STEP_UP;
        game.place_widget(Widget::new(4), widget_square);
        for i in 0..2 {
            game.place_single_sided_one_way_portal(
                SquareWithOrthogonalDir::from_square_and_worldstep(
                    widget_square + STEP_DOWN * i,
                    STEP_RIGHT,
                ),
                SquareWithOrthogonalDir::from_square_and_worldstep(
                    widget_square + STEP_RIGHT * (3 + i) + STEP_UP * 3,
                    STEP_UP,
                ),
            );
        }
        game.draw_headless_now();
        game.graphics.screen.print_screen_buffer();
        let fov = game.player_field_of_view();

        let glyphs = game
            .graphics
            .screen
            .get_screen_glyphs_at_visual_offset_from_center(SCREEN_STEP_UP + SCREEN_STEP_RIGHT * 2);
        assert_false!(glyphs.looks_solid());
    }

    #[test]
    fn test_floating_hunter_drone_place_and_draw() {
        let mut game = set_up_10x10_game();
        game.place_player(point2(4, 5));
        game.place_floating_hunter_drone(
            WorldPoint::new(5.0, 5.0),
            STEP_RIGHT.to_f32(),
            Angle::degrees(0.0),
        );
        game.draw_headless_now();
        game.graphics.screen.print_screen_buffer();

        let drone_glyphs = game
            .graphics
            .screen
            .get_screen_glyphs_at_visual_offset_from_center(SCREEN_STEP_RIGHT);
        let sight_line_glyphs = game
            .graphics
            .screen
            .get_screen_glyphs_at_visual_offset_from_center(SCREEN_STEP_RIGHT * 3);

        assert_eq!(drone_glyphs.get_solid_color().unwrap(), HUNTER_DRONE_COLOR);
        assert!(char_is_braille(sight_line_glyphs[0].character));
        assert!(char_is_braille(sight_line_glyphs[1].character));
        assert_eq!(sight_line_glyphs[0].fg_color, SIGHT_LINE_SEEKING_COLOR);
    }

    #[test]
    fn test_floating_hunter_drone_rotate_over_time() {
        let mut game = set_up_10x10_game();
        game.place_floating_hunter_drone(
            WorldPoint::new(5.0, 5.0),
            STEP_RIGHT.to_f32(),
            Angle::degrees(0.0),
        );

        let start_angle = game.floating_hunter_drones[0].sight_direction;

        game.tick_realtime_effects(Duration::from_secs_f32(0.5));

        let end_angle = game.floating_hunter_drones[0].sight_direction;

        assert_ne!(start_angle, end_angle);
    }

    #[test]
    fn test_floating_hunter_drone_move_over_time() {
        let mut game = set_up_10x10_game();
        game.place_floating_hunter_drone(
            WorldPoint::new(5.0, 5.0),
            STEP_RIGHT.to_f32(),
            Angle::degrees(0.0),
        );

        let start_pos = game.floating_hunter_drones[0].position;

        game.tick_realtime_effects(Duration::from_secs_f32(0.5));

        let end_pos = game.floating_hunter_drones[0].position;

        assert_ne!(start_pos, end_pos);
    }

    #[test]
    fn test_floating_hunter_drone_bounce_off_board_edge() {
        let mut game = set_up_10x10_game();
        game.place_floating_hunter_drone(
            WorldPoint::new(9.0, 5.0),
            STEP_RIGHT.to_f32(),
            Angle::degrees(0.0),
        );

        let start_vel = game.floating_hunter_drones[0].velocity;

        assert!(start_vel.x > 0.0);
        game.tick_realtime_effects(Duration::from_secs_f32(5.0));

        let end_vel = game.floating_hunter_drones[0].velocity;

        assert!(end_vel.x < 0.0);
    }

    #[test]
    fn test_reflect_off_board_edges() {
        let game = set_up_nxm_game(10, 20);

        assert_eq!(
            game.reflect_off_board_edges(point2(0.0, 0.0), STEP_RIGHT.to_f32()),
            STEP_RIGHT.to_f32(),
            "at origin"
        );
        assert_eq!(
            game.reflect_off_board_edges(point2(-0.6, 0.0), STEP_LEFT.to_f32()),
            STEP_RIGHT.to_f32(),
            "left edge"
        );
        assert_eq!(
            game.reflect_off_board_edges(point2(5.0, 9.9), STEP_UP_LEFT.to_f32()),
            STEP_DOWN_LEFT.to_f32(),
            "top edge"
        );
        assert_eq!(
            game.reflect_off_board_edges(point2(5.0, -9.9), STEP_DOWN_LEFT.to_f32()),
            STEP_UP_LEFT.to_f32(),
            "bottom edge"
        );
        assert_eq!(
            game.reflect_off_board_edges(point2(5.0, -9.9), STEP_UP_LEFT.to_f32()),
            STEP_UP_LEFT.to_f32(),
            "bottom edge, after reflection"
        );
        assert_eq!(
            game.reflect_off_board_edges(point2(20.0, 5.0), STEP_RIGHT.to_f32()),
            STEP_LEFT.to_f32(),
            "right edge"
        );
        assert_eq!(
            game.reflect_off_board_edges(point2(20.0, 5.0), STEP_RIGHT.to_f32() * 5.0),
            STEP_LEFT.to_f32() * 5.0,
            "right edge, but faster"
        );
        assert_eq!(
            game.reflect_off_board_edges(point2(19.6, 9.7), STEP_UP_RIGHT.to_f32()),
            STEP_DOWN_LEFT.to_f32(),
            "two edges at once"
        );
    }

    #[test]
    fn test_hunter_drone_rotates_with_screen_rotation() {
        let mut game = set_up_10x10_game();
        game.place_player(point2(5, 5));
        game.place_floating_hunter_drone(
            WorldPoint::new(6.5, 5.0),
            STEP_RIGHT.to_f32(),
            Angle::degrees(0.0),
        );
        game.graphics
            .screen
            .set_rotation(QuarterTurnsAnticlockwise::new(1));
        game.draw_headless_now();
        game.graphics.screen.print_screen_buffer();
        let upper_glyphs = game
            .graphics
            .screen
            .get_screen_glyphs_at_visual_offset_from_center(SCREEN_STEP_DOWN);
        let middle_glyphs = game
            .graphics
            .screen
            .get_screen_glyphs_at_visual_offset_from_center(SCREEN_STEP_DOWN * 2);
        let lower_glyphs = game
            .graphics
            .screen
            .get_screen_glyphs_at_visual_offset_from_center(SCREEN_STEP_DOWN * 3);
        assert_eq!(upper_glyphs.to_clean_string(), "▄▄");
        assert_eq!(middle_glyphs.to_clean_string(), "▀▀");
        assert_eq!(lower_glyphs.chars(), [named_chars::SPACE, '⡇']); // Might need to flip horizontally at some point
    }
    #[test]
    fn test_conveyor_belt_place_and_draw() {
        let mut game = set_up_10x10_game();
        let player_square = point2(4, 5);
        game.place_player(player_square);
        let belt_square = player_square + STEP_DOWN;
        assert!(square_is_even(belt_square));
        game.place_conveyor_belt(belt_square, STEP_RIGHT);
        let advance_and_get_glyphs = |game: &mut Game, duration: Duration| {
            game.tick_realtime_effects(duration);
            game.draw_headless_now();

            game.graphics
                .screen
                .get_screen_glyphs_at_visual_offset_from_center(SCREEN_STEP_DOWN)
        };

        let glyphs1 = advance_and_get_glyphs(&mut game, Duration::from_secs_f32(0.0));

        assert!(glyphs1.get_solid_color().is_some());

        let glyphs2 = advance_and_get_glyphs(&mut game, CONVEYOR_BELT_VISUAL_PERIOD.div_f32(8.0));

        assert_ne!(glyphs1, glyphs2);
        assert_eq!(glyphs2.chars(), [RIGHT_HALF_BLOCK, FULL_BLOCK]);

        let glyphs2_5 = advance_and_get_glyphs(&mut game, CONVEYOR_BELT_VISUAL_PERIOD.div_f32(2.0));

        assert_eq!(glyphs2_5.chars(), [LEFT_HALF_BLOCK, named_chars::SPACE]);

        let glyphs3 = advance_and_get_glyphs(&mut game, CONVEYOR_BELT_VISUAL_PERIOD.div_f32(2.0));

        assert_eq!(glyphs2, glyphs3);
    }

    #[test]
    fn test_conveyor_belt_push_player() {
        let mut game = set_up_10x10_game();
        let square = point2(5, 5);
        game.place_player(square);
        let dir = STEP_RIGHT;
        game.place_conveyor_belt(square, dir.into());

        assert_eq!(game.player_square(), square);
        game.tick_realtime_effects(CONVEYOR_BELT_MOVEMENT_PERIOD.mul_f32(1.1));
        assert_eq!(game.player_square(), square + dir);
    }
    #[test]
    fn test_conveyor_belt_push_widget() {
        let mut game = set_up_10x10_game();
        let square = point2(5, 5);
        game.place_widget(Widget::new(5), square);
        let dir = STEP_RIGHT;
        game.place_conveyor_belt(square, dir.into());

        assert!(game.widgets.contains_key(&square));
        game.tick_realtime_effects(CONVEYOR_BELT_MOVEMENT_PERIOD.mul_f32(1.1));
        assert!(game.widgets.contains_key(&(square + dir)));
    }
    #[test]
    fn test_conveyor_belt_push_hunter_drone() {
        let mut game = set_up_10x10_game();
        let square = point2(5, 5);
        game.place_floating_hunter_drone(square.to_f32(), STEP_ZERO.to_f32(), Angle::degrees(0.0));

        let dir = STEP_RIGHT;
        game.place_conveyor_belt(square, dir.into());

        let pos = game.floating_hunter_drones.iter().next().unwrap().position;
        assert!((pos - square.to_f32()).length() < 0.001);

        let dt = CONVEYOR_BELT_MOVEMENT_PERIOD.mul_f32(0.4635);
        game.tick_realtime_effects(dt);

        let new_pos = game.floating_hunter_drones.iter().next().unwrap().position;
        let new_correct_pos =
            square.to_f32() + STEP_RIGHT.to_f32() * dt.as_secs_f32() * Game::conveyor_belt_speed();

        assert!((new_pos - new_correct_pos).length() < 0.001);
    }
    #[test]
    fn test_conveyor_belt_pushes_death_cube() {
        let mut game = set_up_10x10_game();
        let square = point2(5, 5);
        game.place_linear_death_cube(square.to_f32(), vec2(0.0, 0.0));
        let dir = STEP_RIGHT;
        game.place_conveyor_belt(square, dir.into());

        let pos = game.death_cubes.get(0).unwrap().position;
        assert!((pos - square.to_f32()).length() < 0.001);

        let dt = CONVEYOR_BELT_MOVEMENT_PERIOD.mul_f32(0.4635);
        game.tick_realtime_effects(dt);

        let new_pos = game.death_cubes.get(0).unwrap().position;
        let new_correct_pos =
            square.to_f32() + STEP_RIGHT.to_f32() * dt.as_secs_f32() * Game::conveyor_belt_speed();

        assert!((new_pos - new_correct_pos).length() < 0.001);
    }
    #[test]
    fn test_floor_arrows_push_hunter_drone() {
        let mut game = set_up_10x10_game();
        let square = point2(5, 5);
        let start_pos = square.to_f32() + vec2(0.123, 0.342);
        game.place_floating_hunter_drone(
            start_pos.to_f32(),
            STEP_ZERO.to_f32(),
            Angle::degrees(0.0),
        );
        let dir = STEP_RIGHT;
        game.place_floor_push_arrow(square, dir.into());

        let pos = game.floating_hunter_drones.iter().next().unwrap().position;
        assert!((pos - start_pos).length() < 0.001);

        game.tick_game_logic();

        let new_pos = game.floating_hunter_drones.iter().next().unwrap().position;
        let new_correct_pos = start_pos + dir.to_f32();

        assert!((new_pos - new_correct_pos).length() < 0.001);
    }
    #[test]
    fn test_floor_arrows_push_player() {
        let mut game = set_up_10x10_game();
        let square = point2(5, 5);
        game.place_player(square);
        game.place_floor_push_arrow(square, STEP_RIGHT.into());

        assert_eq!(game.player_square(), square);
        game.tick_game_logic();
        assert_eq!(game.player_square(), square + STEP_RIGHT);
    }
    #[test]
    fn test_floor_arrow_does_not_push_too_far() {
        let mut game = set_up_10x10_game();
        let square = point2(5, 5);
        game.place_widget(Widget::new(5), square);
        game.place_floor_push_arrow(square, STEP_RIGHT.into());
        game.tick_game_logic();
        assert_eq!(*game.widgets.keys().next().unwrap(), square + STEP_RIGHT);
        game.tick_game_logic();
        assert_eq!(*game.widgets.keys().next().unwrap(), square + STEP_RIGHT);
    }
    #[ignore = "TODO"]
    #[test]
    fn test_player_tries_to_walk_against_push_arrows() {
        let mut game = set_up_10x10_game();
        let square = point2(8, 5);
        game.place_player(square);
        for i in 0..5 {
            game.place_floor_push_arrow(square + STEP_LEFT * (i + 1), STEP_RIGHT.into());
        }
        game.draw_headless_now();
        game.graphics.screen.print_screen_buffer();
        for i in 0..30 {
            game.try_slide_player(STEP_LEFT).ok();
            game.tick_game_logic();
        }
        game.draw_headless_now();
        game.graphics.screen.print_screen_buffer();
        assert_eq!(game.player_square(), square);
    }
    #[test]
    fn test_hunter_drone_turns_towards_player_upon_detection() {
        let mut game = set_up_10x10_game();
        let square = point2(5, 5);
        game.place_player(square);

        let start_vel = STEP_RIGHT.to_f32();
        game.place_floating_hunter_drone(
            (square + STEP_DOWN * 3).to_f32(),
            start_vel,
            Angle::degrees(90.0),
        );

        game.tick_realtime_effects(Duration::from_secs_f32(0.001));

        let drone: &mut FloatingHunterDrone = game.floating_hunter_drones.get_mut(0).unwrap();

        let end_vel = drone.velocity;

        assert_about_eq!(start_vel.x, end_vel.y);
        assert_about_eq!(start_vel.length(), end_vel.length());
    }
    #[test]
    fn test_raycast_hit_nothing() {
        let game = set_up_10x10_game();
        let result = game.raycast(point2(5.0, 5.0), Angle::degrees(0.0), 3.0);
        assert!(result.grid_entities.is_empty());
    }
    #[test]
    fn test_raycast_hit_block() {
        let mut game = set_up_10x10_game();
        let block_square = point2(9, 5);
        game.place_block(block_square);
        let result = game.raycast(
            (block_square + STEP_LEFT * 4).to_f32(),
            Angle::degrees(0.0),
            5.0,
        );
        assert_eq!(result.grid_entities[0], (STEP_RIGHT * 4, GridEntity::Block));
    }
    #[test]
    fn test_raycast_not_enough_range_to_hit_block() {
        let mut game = set_up_10x10_game();
        let block_square = point2(9, 5);
        game.place_block(block_square);
        let result = game.raycast(
            (block_square + STEP_LEFT * 4).to_f32(),
            Angle::degrees(0.0),
            3.0,
        );
        assert!(result.grid_entities.is_empty());
    }
    #[test]
    fn test_raycast_barely_hit_block() {
        let mut game = set_up_10x10_game();
        let block_square = point2(9, 5);
        game.place_block(block_square);
        let result = game.raycast(
            block_square.to_f32() - vec2(3.51, 0.0),
            Angle::degrees(0.0),
            3.02,
        );
        assert_eq!(result.grid_entities[0], (STEP_RIGHT * 4, GridEntity::Block));
    }
    #[test]
    fn test_raycast_barely_out_of_range() {
        let mut game = set_up_10x10_game();
        let block_square = point2(9, 5);
        game.place_block(block_square);
        let result = game.raycast(
            block_square.to_f32() - vec2(3.51, 0.0),
            Angle::degrees(0.0),
            3.0,
        );
        assert!(result.grid_entities.is_empty());
    }
    #[test]
    fn test_raycast_goes_through_portal() {
        let mut game = set_up_10x10_game();
        let block_square = point2(9, 5);
        game.place_block(block_square);

        let dist_from_exit = 1;
        let portal_entrance_square = block_square + STEP_UP_LEFT * 2;
        let portal_exit_square = block_square + STEP_LEFT * dist_from_exit;

        game.place_single_sided_one_way_portal(
            SquareWithOrthogonalDir::from_square_and_step(portal_entrance_square, STEP_RIGHT),
            SquareWithOrthogonalDir::from_square_and_step(portal_exit_square, STEP_RIGHT),
        );

        let dist_from_entrance = 3;
        let result = game.raycast(
            (portal_entrance_square + STEP_LEFT * dist_from_entrance).to_f32(),
            Angle::degrees(0.0),
            5.0,
        );
        let dist_from_portal_step = 1;
        assert_eq!(
            result.grid_entities[0],
            (
                STEP_RIGHT * (dist_from_exit + dist_from_entrance + dist_from_portal_step),
                GridEntity::Block
            )
        );
    }
    #[test]
    fn test_raycast_goes_through_turned_portal() {
        let mut game = set_up_10x10_game();
        let block_square = point2(5, 5);
        game.place_block(block_square);

        let portal_entrance_square = block_square + STEP_LEFT * 4;
        let portal_exit_square = block_square;

        game.place_single_sided_one_way_portal(
            SquareWithOrthogonalDir::from_square_and_step(portal_entrance_square, STEP_UP),
            SquareWithOrthogonalDir::from_square_and_step(portal_exit_square, STEP_RIGHT),
        );

        let result = game.raycast(portal_entrance_square.to_f32(), Angle::degrees(90.0), 5.0);
        assert_eq!(result.grid_entities[0], (STEP_UP, GridEntity::Block));
    }
    #[test]
    fn test_hunter_drone_raycasts_visually_go_through_portal() {
        let mut game = set_up_10x10_game();
        let drone_square = point2(2, 2);
        game.place_floating_hunter_drone(
            drone_square.to_f32(),
            STEP_ZERO.to_f32(),
            Angle::degrees(90.0),
        );
        let test_square = drone_square + STEP_RIGHT * 5;
        game.place_single_sided_one_way_portal(
            (drone_square, STEP_UP).into(),
            (test_square, STEP_DOWN).into(),
        );
        game.place_player(drone_square + STEP_DOWN);
        game.draw_headless_now();
        game.graphics.screen.print_screen_buffer();
        let drawable = game
            .graphics
            .get_drawable_for_square_from_draw_buffer(test_square);

        assert!(matches!(drawable, Some(DrawableEnum::Braille(_))));

        let chars = drawable.unwrap().to_glyphs().chars();
        assert!(char_is_braille(chars[0]) || char_is_braille(chars[1]));
    }
    #[ignore = "TODO"]
    #[test]
    fn test_hunter_drone_moves_through_portal() {
        let mut game = set_up_10x10_game();
        let drone_square = point2(2, 2);
        game.place_floating_hunter_drone(
            drone_square.to_f32() + vec2(0.0, 0.49),
            STEP_UP.to_f32(),
            Angle::degrees(90.0),
        );
        let test_square = drone_square + STEP_RIGHT * 5;
        game.place_single_sided_one_way_portal(
            (drone_square, STEP_UP).into(),
            (test_square, STEP_DOWN).into(),
        );
        game.tick_realtime_effects(Duration::from_secs_f32(0.5));
        let drone = game.floating_hunter_drones[0];
        assert!(drone.position.x > drone_square.x as f32 + 2.0);
    }
    #[ignore = "TODO"]
    #[test]
    fn test_hunter_drone_visually_pokes_through_a_portal_a_little_bit() {
        let mut game = set_up_10x10_game();
        let drone_square = point2(2, 2);
        game.place_floating_hunter_drone(
            drone_square.to_f32() + vec2(0.0, 0.49),
            STEP_ZERO.to_f32(),
            Angle::degrees(180.0),
        );
        let test_square = drone_square + STEP_RIGHT * 5;
        game.place_single_sided_one_way_portal(
            (drone_square, STEP_UP).into(),
            (test_square, STEP_DOWN).into(),
        );
        game.place_player(drone_square + STEP_DOWN);
        game.draw_headless_now();
        game.graphics.screen.print_screen_buffer();
        let glyphs = game
            .graphics
            .screen
            .get_screen_glyphs_at_visual_offset_from_center(SCREEN_STEP_RIGHT * 5 + SCREEN_STEP_UP);

        assert_false!(glyphs.looks_solid());
    }
