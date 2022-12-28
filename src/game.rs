use std::cmp::{max, min, Ordering};
use std::collections::{BinaryHeap, HashMap, HashSet};
use std::io::Write;
use std::time::{Duration, Instant};

use euclid::*;
use itertools::Itertools;
use line_drawing::Point;
use ordered_float::OrderedFloat;
use priority_queue::DoublePriorityQueue;
use rand::rngs::StdRng;
use rand::seq::{IteratorRandom, SliceRandom};
use rand::{thread_rng, Rng, SeedableRng};
use strum::IntoEnumIterator;
use strum_macros::EnumIter;

use crate::animations::Selector;
use crate::fov_stuff::{field_of_view_from_square, FovResult};
use crate::glyph::glyph_constants::SPACE;
use crate::graphics::Graphics;
use crate::piece::PieceType::Pawn;
use crate::piece::{Faction, Piece, PieceType};
use crate::utility::coordinate_frame_conversions::*;
use crate::utility::*;
use crate::{
    lerp, point_to_string, rand_radial_offset, rotate_vect, round_to_king_step, Glyph, IPoint,
    IVector, LEFT_I,
};

const TURNS_TO_SPAWN_PAWN: u32 = 10;

pub struct Player {
    pub position: WorldSquare,
    pub faced_direction: WorldStep,
}

#[derive(Clone, Eq, PartialEq, Debug, Copy)]
pub struct IncubatingPawn {
    pub age_in_turns: u32,
    pub faction: Faction,
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
    incubating_pawns: HashMap<WorldSquare, IncubatingPawn>,
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
            incubating_pawns: Default::default(),
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
            //let king_paths = king_squares .iter() .filter_map(|&king_square| self.find_king_path(king_square, player_square)) .collect();
            //self.graphics.draw_paths(king_paths);
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
            self.graphics
                .draw_field_of_view_mask(self.fov_mask_for_player());
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

    pub fn place_king_pawn_group(
        &mut self,
        king_square: WorldSquare,
        faction: Faction,
    ) -> Result<(), ()> {
        self.place_piece(Piece::new(PieceType::King, faction), king_square)?;
        for x in -1..=1 {
            for y in -1..=1 {
                let pawn_square = king_square + vec2(x, y);
                if pawn_square == king_square {
                    continue;
                }
                self.place_piece(Piece::new(PieceType::Pawn, faction), pawn_square)
                    .ok();
            }
        }
        Ok(())
    }

    pub fn place_piece(&mut self, piece: Piece, square: WorldSquare) -> Result<(), ()> {
        if !self.square_is_empty(square) || !self.square_is_on_board(square) {
            return Err(());
        }
        self.pieces.insert(square, piece);
        Ok(())
    }

    pub fn tick_pawn_incubation(&mut self) {
        let found_incubation_squares: SquareSet = self.squares_surrounded_by_pawns_of_one_faction();

        self.incubating_pawns
            .retain(|old_square, _| found_incubation_squares.contains(old_square));

        for square in found_incubation_squares {
            let faction = self.get_piece_at(square + STEP_UP).unwrap().faction;
            if let Some(existing_incubation) = self.incubating_pawns.get_mut(&square) && existing_incubation.faction == faction {
                existing_incubation.age_in_turns += 1;
                if existing_incubation.age_in_turns >= TURNS_TO_SPAWN_PAWN {
                    self.place_piece(Piece::new(PieceType::Pawn, faction), square).expect(&*("Spawn pawn at ".to_owned() + &point_to_string(square)));
                }
            } else {
                let new_incubation = IncubatingPawn {
                    age_in_turns: 0,
                    faction,
                };
                self.incubating_pawns.insert(square, new_incubation);
            }
        }
    }

    pub fn squares_surrounded_by_pawns_of_one_faction(&self) -> SquareSet {
        let mut pawn_adjacency_counter = HashMap::<(WorldSquare, Faction), u32>::new();
        self.pieces
            .iter()
            .cartesian_product(ORTHOGONAL_STEPS)
            .for_each(|((pawn_square, piece), orthogonal_step)| {
                *pawn_adjacency_counter
                    .entry(((*pawn_square + orthogonal_step), piece.faction))
                    .or_insert(0) += 1;
            });
        pawn_adjacency_counter
            .into_iter()
            .filter(|(_, count)| *count == 4)
            .map(|((square, _), _)| square)
            .collect()
    }

    pub fn random_empty_square(&self, rng: &mut StdRng) -> Result<WorldSquare, ()> {
        let num_attempts = 40;
        for _ in 0..num_attempts {
            let rand_pos = WorldSquare::new(
                rng.gen_range(0..self.board_size().width as i32),
                rng.gen_range(0..self.board_size().height as i32),
            );
            if self.square_is_empty(rand_pos) {
                return Ok(rand_pos);
            }
        }
        Err(())
    }

    pub fn place_piece_randomly(&mut self, piece: Piece, rng: &mut StdRng) -> Result<(), ()> {
        let rand_pos = self
            .random_empty_square(rng)
            .expect("failed to get random square");
        let place_result = self.place_piece(piece, rand_pos);
        if place_result.is_ok() {
            return place_result;
        }
        return Err(());
    }
    pub fn place_block_randomly(&mut self, rng: &mut StdRng) {
        let rand_pos = self
            .random_empty_square(rng)
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

    pub fn move_one_piece_per_faction(&mut self) {
        for faction in self.get_all_living_factions() {
            self.move_one_piece_of_faction(faction);
        }
    }

    fn get_all_living_factions(&self) -> HashSet<Faction> {
        self.pieces
            .values()
            .map(|piece| piece.faction)
            .unique()
            .collect()
    }

    fn squares_of_pieces_in_faction(&self, faction: Faction) -> Vec<WorldSquare> {
        self.pieces
            .iter()
            .filter(|(square, piece)| piece.faction == faction)
            .map(|(&square, piece)| square)
            .collect()
    }

    fn move_one_piece_of_faction(&mut self, faction: Faction) {
        let faction_squares = self.squares_of_pieces_in_faction(faction);

        if let Some(selected_piece) = if !self.player_is_dead() {
            faction_squares
                .into_iter()
                .min_by_key(|&square| (square - self.player_square()).square_length())
        } else {
            faction_squares
                .into_iter()
                .min_by_key(|&square| OrderedFloat(square.x as f32 + 0.1 + square.y as f32))
        } {
            self.move_piece_at(selected_piece);
        } else {
            panic!("No pieces in faction to move");
        }
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

    pub fn square_to_move_toward_player_for_piece_at(
        &self,
        piece_square: WorldSquare,
    ) -> Option<WorldSquare> {
        if self.player_is_dead() {
            return None;
        }
        self.move_options_for_piece_at(piece_square)
            .into_iter()
            .filter(|&square| self.square_is_empty(square) && self.square_is_on_board(square))
            .min_by_key(|&square| (square - self.player_square()).square_length())
    }

    pub fn piece_can_capture_player(&self, piece_square: WorldSquare) -> bool {
        !self.player_is_dead()
            && self
                .capture_options_for_piece_at(piece_square)
                .into_iter()
                .contains(&self.player_square())
    }
    pub fn square_to_capture_for_piece_at(&self, piece_square: WorldSquare) -> Option<WorldSquare> {
        let friendly_faction = self.get_piece_at(piece_square).unwrap().faction;
        // TODO: choose randomly rather than first
        self.capture_options_for_piece_at(piece_square)
            .into_iter()
            .filter(|&world_square| {
                self.get_piece_at(world_square)
                    .is_some_and(|piece| piece.faction != friendly_faction)
            })
            .next()
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
        } else if self.piece_can_capture_player(piece_square) {
            end_square = self.player_square();
        } else if let Some(square) = self.square_to_capture_for_piece_at(piece_square) {
            end_square = square;
        } else if let Some(square) = self.square_to_move_toward_player_for_piece_at(piece_square) {
            end_square = square;
        } else {
            return None;
        }

        // capture player
        if self.is_player_at(end_square) {
            self.player_optional = None;
            self.quit();
        }
        if self.is_piece_at(end_square) {
            self.capture_piece_at(end_square).expect("capture failed");
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

    pub fn set_up_vs_mini_factions(&mut self) {
        let distance = 5;
        self.place_king_pawn_group(
            self.player_square() + STEP_UP_LEFT * distance,
            Faction::from_id(0),
        )
        .ok();
        self.place_king_pawn_group(
            self.player_square() + STEP_UP * distance,
            Faction::from_id(1),
        )
        .ok();
        self.place_king_pawn_group(
            self.player_square() + STEP_UP_RIGHT * distance,
            Faction::from_id(2),
        )
        .ok();
    }

    pub fn set_up_columns(&mut self) {
        self.place_block(self.player_square() + STEP_RIGHT * 4);
        self.place_block(self.player_square() + STEP_RIGHT * 7);
        self.place_block(self.player_square() + STEP_RIGHT * 4 + STEP_UP * 3);
        self.place_block(self.player_square() + STEP_RIGHT * 7 + STEP_UP * 3);
    }

    pub fn set_up_labyrinth(&mut self, rng: &mut StdRng) {
        let board_squares_total = self.board_size().width * self.board_size().height;
        let num_blocks = board_squares_total / 3;
        for _ in 0..num_blocks {
            self.place_block_randomly(rng);
        }
    }

    pub fn set_up_labyrinth_hunt(&mut self, rng: &mut StdRng) {
        self.set_up_labyrinth(rng);
        for piece_type in PieceType::iter() {
            for _ in 0..2 {
                self.place_piece_randomly(Piece::from_type(piece_type), rng)
                    .expect("random placement");
            }
        }
    }
    pub fn set_up_labyrinth_kings(&mut self, rng: &mut StdRng) {
        self.set_up_labyrinth(rng);
        for _ in 0..8 {
            self.place_piece_randomly(Piece::king(), rng)
                .expect("random king placement");
        }
    }
    pub fn square_is_fully_visible_to_player(&self, square: WorldSquare) -> bool {
        self.fov_mask_for_player()
            .fully_visible_squares
            .contains(&square)
    }
    pub fn square_is_not_visible_to_player(&self, square: WorldSquare) -> bool {
        let fov_mask = self.fov_mask_for_player();
        !fov_mask.fully_visible_squares.contains(&square)
            && !fov_mask.partially_visible_squares.contains_key(&square)
    }
    fn fov_mask_for_player(&self) -> FovResult {
        let start_square = self.player_square();
        field_of_view_from_square(start_square, &self.blocks)
    }
}

#[cfg(test)]
mod tests {
    use ntest::assert_false;
    use pretty_assertions::{assert_eq, assert_ne};

    use crate::utility::{
        STEP_DOWN, STEP_DOWN_RIGHT, STEP_LEFT, STEP_RIGHT, STEP_UP, STEP_UP_LEFT, STEP_UP_RIGHT,
    };
    use crate::utils_for_tests::*;

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
        let mut game = set_up_game();
        game.place_player(point2(5, 5));
        game.place_block(point2(5, 4));
        let test_square = point2(5, 3);
        assert_false!(game.square_is_fully_visible_to_player(test_square));
    }

    #[test]
    fn test_fov_mask_non_partials() {
        let mut game = set_up_game();
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
        game.place_king_pawn_group(king_square, Faction::from_id(0))
            .ok();
        let test_square = king_square + STEP_DOWN_LEFT;
        assert_false!(game.square_is_empty(test_square));
        game.move_one_piece_per_faction();
        assert!(game.square_is_empty(test_square));
    }

    #[test]
    fn test_pawn_reproduction_in_surrounded_squares() {
        let mut game = set_up_game();
        let test_square = point2(5, 5);
        let faction = Faction::from_id(0);
        let rel_positions = vec![STEP_UP, STEP_RIGHT, STEP_LEFT, STEP_DOWN];
        for rel_pos in rel_positions {
            game.place_piece(Piece::new(PieceType::Pawn, faction), test_square + rel_pos)
                .expect("place pawn");
        }
        assert_eq!(game.pieces.len(), 4);
        for _ in 0..=TURNS_TO_SPAWN_PAWN {
            game.tick_pawn_incubation();
        }
        assert!(game.pieces.len() > 4);
    }
}
