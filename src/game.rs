use std::cmp::{max, min};
use std::collections::{HashMap, HashSet};
use std::io::Write;
use std::time::{Duration, Instant};

use euclid::*;
use line_drawing::Point;
use rand::{thread_rng, Rng};
use strum::IntoEnumIterator;
use strum_macros::EnumIter;

use crate::animations::Selector;
use crate::graphics::Graphics;
use crate::piece::{Piece, PieceType};
use crate::utility::SquareSet;
use crate::{
    lerp, point_to_string, rand_radial_offset, rotate_vect, round_to_king_step, BoardSize, Glyph,
    IPoint, IVector, SquareGridInWorldFrame, SquareList, WorldMove, WorldPoint, WorldSquare,
    WorldStep, LEFT_I,
};

pub struct Player {
    pub position: WorldSquare,
    pub faced_direction: WorldStep,
}

pub struct Game {
    board_size: BoardSize,
    // (x,y), left to right, top to bottom
    //step_foes: Vec<StepFoe>,
    running: bool,
    // set false to quit
    player_optional: Option<Player>,
    graphics: Graphics,
    pieces: HashMap<WorldSquare, Piece>,
    blocks: HashSet<WorldSquare>,
    turn_count: u32,
    selectors: Vec<Selector>,
    selected_square: Option<WorldSquare>,
}

impl Game {
    pub fn new(terminal_width: u16, terminal_height: u16, start_time: Instant) -> Game {
        let board_size = BoardSize::new(terminal_width as u32 / 2, terminal_height as u32);
        let mut game = Game {
            board_size,
            running: true,
            player_optional: None,
            graphics: Graphics::new(terminal_width, terminal_height, start_time),
            pieces: HashMap::new(),
            blocks: HashSet::new(),
            turn_count: 0,
            selectors: vec![],
            selected_square: None,
        };
        game.graphics.set_empty_board_animation(board_size);
        game
    }
    pub fn board_size(&self) -> BoardSize {
        self.board_size
    }

    pub fn player_is_dead(&self) -> bool {
        self.player_optional.is_none()
    }
    pub fn turn_count(&self) -> u32 {
        self.turn_count
    }

    pub fn place_player(&mut self, square: WorldSquare) {
        self.player_optional = Some(Player {
            position: square,
            faced_direction: LEFT_I.cast_unit(),
        });
    }

    fn mid_square(&self) -> IPoint {
        point2(
            self.board_size().width as i32 / 2,
            self.board_size().height as i32 / 2,
        )
    }

    fn square_is_on_board(&self, pos: WorldSquare) -> bool {
        pos.x >= 0
            && pos.x < self.board_size().width as i32
            && pos.y >= 0
            && pos.y < self.board_size().height as i32
    }

    pub fn quit(&mut self) {
        self.running = false;
    }
    pub fn running(&self) -> bool {
        self.running
    }

    pub fn move_player(&mut self, movement: WorldStep) -> Result<(), ()> {
        let new_pos = self.player_square() + movement;
        if self.danger_squares().contains(&new_pos) || self.is_block_at(new_pos) {
            return Err(());
        }

        self.raw_set_player_faced_direction(round_to_king_step(movement));
        self.try_set_player_position(new_pos)
    }

    pub fn player_square(&self) -> WorldSquare {
        if let Some(player) = &self.player_optional {
            player.position
        } else {
            panic!("Can't get square; player is dead")
        }
    }

    pub fn try_set_player_position(&mut self, pos: WorldSquare) -> Result<(), ()> {
        if self.is_piece_at(pos) {
            self.capture_piece_at(pos).expect("capture failed");
        }

        if !self.square_is_on_board(pos) {
            return Err(());
        }

        self.raw_set_player_position(pos);

        return Ok(());
    }

    fn raw_set_player_position(&mut self, square: WorldSquare) {
        if let Some(player) = &mut self.player_optional {
            player.position = square
        } else {
            panic!("Player is too dead to move")
        }
    }

    fn danger_squares(&self) -> SquareSet {
        self.pieces
            .keys()
            .map(|&square| self.guarded_squares_for_piece_at(square))
            .flatten()
            .collect()
    }

    fn tricky_danger_squares(&self) -> SquareSet {
        // squares protected by all enemy pieces, ignoring being blocked by pieces
        let all_danger_squares_including_tricky_ones: SquareSet = self
            .pieces
            .keys()
            .map(|&square| self.guarded_squares_passing_through_pieces_for_piece_at(square))
            .flatten()
            .collect();
        all_danger_squares_including_tricky_ones
            .difference(&self.danger_squares())
            .copied()
            .collect()
    }

    pub fn player_faced_direction(&self) -> WorldStep {
        if let Some(player) = &self.player_optional {
            player.faced_direction
        } else {
            panic!("player is dead")
        }
    }

    pub fn raw_set_player_faced_direction(&mut self, new_dir: WorldStep) {
        assert_eq!(
            max(new_dir.x.abs(), new_dir.y.abs()),
            1,
            "bad input vector{:?}",
            new_dir
        );
        if let Some(player) = &mut self.player_optional {
            player.faced_direction = new_dir
        } else {
            panic!("Player is too dead to rotate")
        }
    }

    pub fn borrow_graphics_mut(&mut self) -> &mut Graphics {
        return &mut self.graphics;
    }

    pub fn draw_headless_at_duration_from_start(&mut self, delta: Duration) {
        let draw_time = self.graphics.start_time() + delta;
        self.draw(&mut None, draw_time);
    }
    pub fn draw_headless_now(&mut self) {
        self.draw(&mut None, Instant::now());
    }

    pub fn draw(&mut self, mut writer: &mut Option<Box<dyn Write>>, time: Instant) {
        self.graphics.fill_output_buffer_with_black();
        self.graphics.draw_board_animation(time);
        self.graphics.draw_danger_squares(self.danger_squares());
        self.graphics
            .draw_tricky_danger_squares(self.tricky_danger_squares());
        for &square in &self.blocks {
            self.graphics.draw_block(square);
        }
        for (&square, &piece) in &self.pieces {
            self.graphics.draw_piece(piece, square);
        }
        self.graphics.draw_non_board_animations(time);
        self.graphics
            .draw_player(self.player_square(), self.player_faced_direction());
        self.graphics.display(&mut writer);
        self.graphics.remove_finished_animations(time);
    }
    fn is_player_at(&self, square: WorldSquare) -> bool {
        !self.player_is_dead() && self.player_square() == square
    }

    fn square_is_empty(&self, pos: WorldSquare) -> bool {
        !self.is_player_at(pos) && !self.is_piece_at(pos) && !self.is_block_at(pos)
    }

    pub fn place_piece(&mut self, piece: Piece, square: WorldSquare) -> Result<(), ()> {
        if !self.square_is_empty(square) || !self.square_is_on_board(square) {
            return Err(());
        }
        self.pieces.insert(square, piece);
        Ok(())
    }

    pub fn random_empty_square(&self) -> Result<WorldSquare, ()> {
        let num_attempts = 40;
        for _ in 0..num_attempts {
            let rand_pos = WorldSquare::new(
                thread_rng().gen_range(0..self.board_size().width as i32),
                thread_rng().gen_range(0..self.board_size().height as i32),
            );
            if self.square_is_empty(rand_pos) {
                return Ok(rand_pos);
            }
        }
        Err(())
    }

    pub fn place_piece_randomly(&mut self, piece: Piece) -> Result<(), ()> {
        let rand_pos = self
            .random_empty_square()
            .expect("failed to get random square");
        let place_result = self.place_piece(piece, rand_pos);
        if place_result.is_ok() {
            return place_result;
        }
        return Err(());
    }
    pub fn place_block_randomly(&mut self) {
        let rand_pos = self
            .random_empty_square()
            .expect("failed to get random square");
        self.place_block(rand_pos);
    }

    pub fn get_piece_at(&self, square: WorldSquare) -> Option<&Piece> {
        self.pieces.get(&square)
    }

    pub fn is_piece_at(&self, square: WorldSquare) -> bool {
        self.get_piece_at(square).is_some()
    }

    pub fn piece_type_count(&self, piece_type: PieceType) -> i32 {
        self.pieces
            .values()
            .filter(|&&piece| piece.piece_type == piece_type)
            .count() as i32
    }

    pub fn select_all_pieces(&mut self) {
        self.graphics
            .select_squares(self.pieces.keys().cloned().collect());
    }
    pub fn select_closest_piece(&mut self) {
        let closest_piece_square: Option<WorldSquare> = self.square_of_closest_piece_to_player();

        self.selected_square = closest_piece_square;
        if let Some(square) = closest_piece_square {
            self.select_square(square);
        } else {
            self.clear_selectors();
        }
    }

    pub fn select_square(&mut self, square: WorldSquare) {
        self.graphics.select_squares(vec![square]);
    }
    pub fn clear_selectors(&mut self) {
        self.graphics.select_squares(vec![]);
    }

    fn square_of_closest_piece_to_player(&self) -> Option<WorldSquare> {
        let slightly_right_of_player_position: WorldPoint =
            self.player_square().to_f32() + WorldMove::new(0.01, 0.0);

        self.pieces
            .keys()
            .min_by_key(|square| {
                ordered_float::OrderedFloat(
                    (square.to_f32() - slightly_right_of_player_position).length(),
                )
            })
            .cloned()
    }

    pub fn move_all_pieces(&mut self) {
        let mut moved_piece_locations = HashSet::<WorldSquare>::new();
        let piece_start_locations: SquareList = self.pieces.keys().cloned().collect();
        for piece_square in piece_start_locations {
            // already moved this one
            if moved_piece_locations.contains(&piece_square) {
                continue;
            }
            if let Some(end_pos) = self.move_piece_at(piece_square) {
                moved_piece_locations.insert(end_pos);
            }
        }
        self.turn_count += 1;
    }

    fn move_piece(&mut self, start: WorldSquare, end: WorldSquare) {
        if !self.is_piece_at(start) {
            panic!("No piece at {}", point_to_string(start));
        }
        if !self.square_is_empty(end) {
            panic!("Square is occupied: {}", point_to_string(end));
        }

        let piece = self.pieces.remove(&start).unwrap();
        self.pieces.insert(end, piece);
    }
    fn range_cast(
        &self,
        start_square: WorldSquare,
        repeating_step: WorldStep,
        pass_through_pieces: bool,
    ) -> SquareList {
        let mut valid_squares: SquareList = vec![];
        let mut i = 1;
        loop {
            let square = start_square + repeating_step * i;
            if !self.square_is_on_board(square) {
                break;
            }
            valid_squares.push(square);
            if pass_through_pieces && self.is_piece_at(square) {
                // keep going
            } else {
                if !self.square_is_empty(square) {
                    break;
                }
            }
            i += 1;
        }
        valid_squares
    }

    pub fn steps_in_direction_including_hit(
        &self,
        piece_square: WorldSquare,
        repeating_step: WorldStep,
    ) -> SquareList {
        self.range_cast(piece_square, repeating_step, false)
    }
    pub fn steps_in_direction_passing_through_pieces_including_hit(
        &self,
        piece_square: WorldSquare,
        repeating_step: WorldStep,
    ) -> SquareList {
        self.range_cast(piece_square, repeating_step, true)
    }

    // returns where the piece moves to, if applicable
    pub fn move_piece_at(&mut self, pos: WorldSquare) -> Option<WorldSquare> {
        if !self.is_piece_at(pos) {
            return None;
        }

        let piece = self.get_piece_at(pos).unwrap().clone();

        // want to capture the player
        let capture_option: Option<WorldSquare> = self
            .capture_options_for_piece_at(pos)
            .into_iter()
            .find(|&square| square == self.player_square());

        if let Some(square) = capture_option {
            self.player_optional = None;
            self.quit();
            self.move_piece(pos, square);
            return Some(square);
        }

        // want to move closer to player
        let move_option: Option<WorldSquare> = self
            .move_options_for_piece_at(pos)
            .into_iter()
            .filter(|&square| self.square_is_empty(square) && self.square_is_on_board(square))
            .min_by_key(|&square| (square - self.player_square()).square_length());

        if let Some(square) = move_option {
            self.move_piece(pos, square);
            return Some(square);
        }

        None
    }

    fn move_options_for_piece_at(&self, piece_square: WorldSquare) -> SquareList {
        if !self.is_piece_at(piece_square) {
            return vec![]; // should be error instead?
        }

        let mut move_squares: SquareList = vec![];

        if let Some(piece) = self.get_piece_at(piece_square) {
            for move_step in piece.relative_move_steps() {
                let target_square = piece_square + move_step;
                if self.square_is_on_board(target_square) && self.square_is_empty(target_square) {
                    move_squares.push(target_square);
                }
            }

            for move_direction in piece.move_directions() {
                let mut squares_to_collision =
                    self.steps_in_direction_including_hit(piece_square, move_direction);
                if let Some(last_square) = squares_to_collision.last() {
                    if !self.square_is_empty(*last_square) {
                        squares_to_collision.pop();
                    }
                }
                move_squares.append(&mut squares_to_collision);
            }
        }
        move_squares
    }

    fn guarded_squares_for_piece_at(&self, piece_square: WorldSquare) -> SquareList {
        assert!(self.is_piece_at(piece_square));

        let mut guarded_squares: SquareList = vec![];
        let piece = self.get_piece_at(piece_square).unwrap();

        for move_step in piece.relative_capture_steps() {
            let target_square = piece_square + move_step;
            if self.square_is_on_board(target_square) {
                guarded_squares.push(target_square);
            }
        }

        for move_direction in piece.capture_directions() {
            let mut squares_to_collision =
                self.steps_in_direction_including_hit(piece_square, move_direction);
            guarded_squares.append(&mut squares_to_collision);
        }
        guarded_squares
    }
    fn guarded_squares_passing_through_pieces_for_piece_at(
        &self,
        piece_square: WorldSquare,
    ) -> SquareList {
        assert!(self.is_piece_at(piece_square));

        let mut guarded_squares: SquareList = vec![];
        let piece = self.get_piece_at(piece_square).unwrap();

        for move_step in piece.relative_capture_steps() {
            let target_square = piece_square + move_step;
            if self.square_is_on_board(target_square) {
                guarded_squares.push(target_square);
            }
        }

        for move_direction in piece.capture_directions() {
            let mut squares_to_collision = self
                .steps_in_direction_passing_through_pieces_including_hit(
                    piece_square,
                    move_direction,
                );
            guarded_squares.append(&mut squares_to_collision);
        }
        guarded_squares
    }

    fn capture_options_for_piece_at(&self, piece_square: WorldSquare) -> SquareList {
        assert!(self.is_piece_at(piece_square));

        let mut capture_squares: SquareList = vec![];

        for square in self.guarded_squares_for_piece_at(piece_square) {
            if !self.square_is_empty(square) {
                capture_squares.push(square);
            }
        }
        capture_squares
    }

    pub fn player_shoot_shotgun(&mut self) {
        let num_lasers = 10;
        let range = 5.0;
        let spread_radians = 1.0;
        let random_spread_radius = 1.0;
        for i in 0..num_lasers {
            let line_start: WorldSquare = self.player_square();
            let rotation_if_uniform = lerp(
                -spread_radians / 2.0,
                spread_radians / 2.0,
                i as f32 / num_lasers as f32,
            );
            let line_end: WorldPoint = line_start.to_f32()
                + rotate_vect(
                    self.player_faced_direction().to_f32() * range,
                    rotation_if_uniform,
                )
                .cast_unit()
                + rand_radial_offset(random_spread_radius).cast_unit();

            // Orthogonal steps only
            for (x, y) in line_drawing::WalkGrid::new(
                line_start.to_tuple(),
                line_end.round().to_i32().to_tuple(),
            ) {
                let square = WorldSquare::new(x, y);
                self.capture_piece_at(square).ok();
            }

            self.graphics
                .add_simple_laser(line_start.to_f32(), line_end);
        }
        self.graphics
            .start_recoil_animation(self.board_size, self.player_faced_direction());
    }

    pub fn player_shoot_sniper(&mut self) {
        let mut graphical_laser_end: WorldSquare;
        if let Some(square) = self.selected_square {
            if self.pieces.contains_key(&square) {
                self.capture_piece_at(square).expect("capture with sniper");
            }
            graphical_laser_end = square;
        } else {
            graphical_laser_end = self.player_square() + self.player_faced_direction() * 300;
        }
        // laser should start at edge of player square, where player is facing
        let graphical_laser_start =
            self.player_square().to_f32() + self.player_faced_direction().to_f32() * 0.5;
        self.graphics
            .add_floaty_laser(graphical_laser_start, graphical_laser_end.to_f32());
    }

    pub fn capture_piece_at(&mut self, square: WorldSquare) -> Result<(), ()> {
        if !self.square_is_on_board(square) || !self.is_piece_at(square) {
            return Err(());
        }
        self.pieces.remove(&square);
        self.graphics.add_explosion(square.to_f32());
        Ok(())
    }

    pub fn place_block(&mut self, square: WorldSquare) {
        self.blocks.insert(square);
    }
    pub fn is_block_at(&self, square: WorldSquare) -> bool {
        self.blocks.contains(&square)
    }

    pub fn set_up_labyrinth_hunt(&mut self) {
        let board_squares_total = self.board_size().width * self.board_size().height;
        let num_blocks = board_squares_total / 3;
        for _ in 0..num_blocks {
            self.place_block_randomly();
        }

        for piece_type in PieceType::iter() {
            for _ in 0..2 {
                self.place_piece_randomly(Piece::from_type(piece_type))
                    .expect("random placement");
            }
        }
    }
}
