use std::cmp::{max, min, Ordering};
use std::collections::{BinaryHeap, HashMap, HashSet};
use std::io::Write;
use std::time::{Duration, Instant};

use euclid::*;
use line_drawing::Point;
use priority_queue::DoublePriorityQueue;
use rand::{thread_rng, Rng};
use strum::IntoEnumIterator;
use strum_macros::EnumIter;

use crate::animations::Selector;
use crate::graphics::Graphics;
use crate::piece::{Piece, PieceType};
use crate::utility::{king_distance, reversed, SquareSet};
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

    pub fn mid_square(&self) -> WorldSquare {
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
        self.raw_set_player_faced_direction(round_to_king_step(movement));
        let new_pos = self.player_square() + movement;
        if self
            .capture_squares_for_all_pieces(false)
            .contains(&new_pos)
        {
            return Err(());
        }

        self.try_set_player_position(new_pos)
    }

    pub fn try_get_player_square(&self) -> Option<WorldSquare> {
        if let Some(player) = &self.player_optional {
            Some(player.position)
        } else {
            None
        }
    }
    pub fn player_square(&self) -> WorldSquare {
        if let Some(square) = self.try_get_player_square() {
            square
        } else {
            panic!("player is dead")
        }
    }

    pub fn try_set_player_position(&mut self, square: WorldSquare) -> Result<(), ()> {
        if self.is_piece_at(square) {
            self.capture_piece_at(square).expect("capture failed");
        }

        if !self.square_is_on_board(square) || self.is_block_at(square) {
            return Err(());
        }

        self.raw_set_player_position(square);

        return Ok(());
    }

    fn raw_set_player_position(&mut self, square: WorldSquare) {
        if let Some(player) = &mut self.player_optional {
            player.position = square
        } else {
            panic!("Player is too dead to move")
        }
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
    pub fn graphics(&self) -> &Graphics {
        return &self.graphics;
    }
    pub fn pieces(&mut self) -> &mut HashMap<WorldSquare, Piece> {
        return &mut self.pieces;
    }

    fn find_pieces(&self, target_piece: Piece) -> SquareSet {
        self.pieces
            .iter()
            .filter(|(&square, &piece)| piece == target_piece)
            .map(|(&square, &piece)| square)
            .collect()
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

        // TODO: fix redundant calculation
        // TODO: make redundant calculation actually produce the same path every time
        if let Some(player_square) = self.try_get_player_square() {
            let king_squares = self.find_pieces(Piece::king());
            let king_paths = king_squares
                .iter()
                .filter_map(|&king_square| self.find_king_path(king_square, player_square))
                .collect();
            self.graphics.draw_paths(king_paths);
        }

        self.graphics.draw_move_marker_squares(
            self.move_squares_for_all_pieces(false),
            self.capture_squares_for_all_pieces(false),
            self.move_squares_for_all_pieces(true),
            self.capture_squares_for_all_pieces(true),
        );

        self.graphics.draw_blocks(&self.blocks);
        for (&square, &piece) in &self.pieces {
            self.graphics.draw_piece(piece, square);
        }
        self.graphics.draw_non_board_animations(time);
        if !self.player_is_dead() {
            self.graphics
                .draw_player(self.player_square(), self.player_faced_direction());
        }
        self.graphics.display(&mut writer);
        self.graphics.remove_finished_animations(time);
    }
    fn is_player_at(&self, square: WorldSquare) -> bool {
        !self.player_is_dead() && self.try_get_player_square() == Some(square)
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
    pub fn on_turn_start(&mut self) {}
    pub fn on_turn_end(&mut self) {
        self.select_closest_piece();
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
    pub fn move_piece_at(&mut self, piece_square: WorldSquare) -> Option<WorldSquare> {
        if !self.is_piece_at(piece_square) {
            return None;
        }

        let piece = self.get_piece_at(piece_square).unwrap().clone();
        let mut end_square;

        if piece.piece_type == PieceType::King {
            if let Some(path_to_player) = self.find_king_path(piece_square, self.player_square()) {
                let first_step_square = *path_to_player.get(1).unwrap();
                end_square = first_step_square;
            } else {
                return None;
            }
        } else if let Some(capture_square) = self
            .capture_options_for_piece_at(piece_square)
            .into_iter()
            .find(|&square| square == self.player_square())
        {
            end_square = capture_square;
        } else if let Some(square) = self
            .move_options_for_piece_at(piece_square)
            .into_iter()
            .filter(|&square| self.square_is_empty(square) && self.square_is_on_board(square))
            .min_by_key(|&square| (square - self.player_square()).square_length())
        {
            end_square = square;
        } else {
            return None;
        }

        // capture player
        if self.is_player_at(end_square) {
            self.player_optional = None;
            self.quit();
        }
        self.move_piece(piece_square, end_square);
        Some(end_square)
    }

    fn move_options_for_piece_at(&self, piece_square: WorldSquare) -> SquareList {
        self.move_squares_for_piece_at(piece_square, false)
            .into_iter()
            .filter(|&square| self.square_is_empty(square))
            .collect()
    }

    fn move_or_capture_squares_for_piece_at(
        &self,
        piece_square: WorldSquare,
        capture_instead_of_move: bool,
        pass_through_pieces: bool,
    ) -> SquareSet {
        assert!(self.is_piece_at(piece_square));
        let mut squares = SquareSet::new();
        let piece = self.get_piece_at(piece_square).unwrap();

        let function_for_steps = if capture_instead_of_move {
            Piece::relative_capture_steps
        } else {
            Piece::relative_move_steps
        };
        let function_for_directions = if capture_instead_of_move {
            Piece::capture_directions
        } else {
            Piece::move_directions
        };

        for single_step in function_for_steps(piece) {
            let target_square = piece_square + single_step;
            if self.square_is_on_board(target_square) {
                squares.insert(target_square);
            }
        }

        for move_direction in function_for_directions(piece) {
            let mut squares_to_collision =
                self.range_cast(piece_square, move_direction, pass_through_pieces);
            squares.extend(squares_to_collision);
        }
        squares
    }

    fn capture_squares_for_piece_at(
        &self,
        piece_square: WorldSquare,
        pass_through_pieces: bool,
    ) -> SquareSet {
        self.move_or_capture_squares_for_piece_at(piece_square, true, pass_through_pieces)
    }
    fn move_squares_for_piece_at(
        &self,
        piece_square: WorldSquare,
        pass_through_pieces: bool,
    ) -> SquareSet {
        self.move_or_capture_squares_for_piece_at(piece_square, false, pass_through_pieces)
    }

    fn move_squares_for_all_pieces(&self, pass_through_pieces: bool) -> SquareSet {
        self.pieces
            .keys()
            .map(|&square| self.move_squares_for_piece_at(square, pass_through_pieces))
            .flatten()
            .collect()
    }
    fn capture_squares_for_all_pieces(&self, pass_through_pieces: bool) -> SquareSet {
        self.pieces
            .keys()
            .map(|&square| self.capture_squares_for_piece_at(square, pass_through_pieces))
            .flatten()
            .collect()
    }

    fn guarded_squares_for_piece_at(&self, piece_square: WorldSquare) -> SquareSet {
        self.capture_squares_for_piece_at(piece_square, false)
    }

    fn find_king_path(
        &self,
        start_square: WorldSquare,
        target_square: WorldSquare,
    ) -> Option<Vec<WorldSquare>> {
        fn cost_heuristic(a: WorldSquare, b: WorldSquare) -> u32 {
            king_distance(a, b)
        }
        let relative_steps = Piece::relative_move_steps_for_type(PieceType::King);
        let mut recorded_step_start_squares_by_step_end_squares =
            HashMap::<WorldSquare, WorldSquare>::new();
        let mut squares_to_check = DoublePriorityQueue::<WorldSquare, u32>::new();
        squares_to_check.push(start_square, cost_heuristic(start_square, target_square));
        while let Some((square_to_check, cost)) = squares_to_check.pop_min() {
            let next_squares: SquareList = relative_steps
                .clone()
                .into_iter()
                .map(|step_to_next_square| square_to_check + step_to_next_square)
                .filter(|&next_square| {
                    !recorded_step_start_squares_by_step_end_squares.contains_key(&next_square)
                        && (self.square_is_empty(next_square) || self.is_player_at(next_square))
                })
                .collect();
            next_squares.clone().into_iter().for_each(|next_square| {
                let new_cost = cost + cost_heuristic(next_square, target_square);
                squares_to_check.push(next_square, new_cost);
                recorded_step_start_squares_by_step_end_squares
                    .insert(next_square, square_to_check);
            });
            if next_squares.contains(&target_square) {
                break;
            }
        }
        if !recorded_step_start_squares_by_step_end_squares.contains_key(&target_square) {
            return None;
        }
        let mut reverse_full_path = vec![target_square];
        while *reverse_full_path.last().unwrap() != start_square {
            reverse_full_path.push(
                *recorded_step_start_squares_by_step_end_squares
                    .get(reverse_full_path.last().unwrap())
                    .unwrap(),
            );
        }
        Some(reversed(reverse_full_path))
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

    fn set_up_labyrinth(&mut self) {
        let board_squares_total = self.board_size().width * self.board_size().height;
        let num_blocks = board_squares_total / 3;
        for _ in 0..num_blocks {
            self.place_block_randomly();
        }
    }

    pub fn set_up_labyrinth_hunt(&mut self) {
        self.set_up_labyrinth();
        for piece_type in PieceType::iter() {
            for _ in 0..2 {
                self.place_piece_randomly(Piece::from_type(piece_type))
                    .expect("random placement");
            }
        }
    }
    pub fn set_up_labyrinth_kings(&mut self) {
        self.set_up_labyrinth();
        for _ in 0..8 {
            self.place_piece_randomly(Piece::king())
                .expect("random king placement");
        }
    }
}
#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::{assert_eq, assert_ne};

    #[test]
    fn test_try_set_player_on_block_is_fail() {
        let mut game = Game::new(20, 10, Instant::now());
        game.place_player(point2(5, 5));
        game.place_block(point2(3, 3));
        assert!(game.try_set_player_position(point2(3, 3)).is_err());
    }
}
