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
use rgb::RGB8;
use strum::IntoEnumIterator;
use strum_macros::EnumIter;

use crate::animations::Selector;
use crate::fov_stuff::{field_of_view_from_square, FovResult};
use crate::glyph::glyph_constants::{ENEMY_PIECE_COLOR, RED_PAWN_COLOR, SPACE, WHITE};
use crate::graphics::Graphics;
use crate::piece::PieceType::Pawn;
use crate::piece::{Faction, FactionFactory, FactionInfo, Piece, PieceType};
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
    faction_factory: FactionFactory,
    // faction_info: HashMap<Faction, FactionInfo>, //TODO: LATER MAYBE
    red_pawn_faction: Faction,
    default_enemy_faction: Faction,
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
            faction_factory: FactionFactory::new(),
            //faction_info: Default::default(),
            red_pawn_faction: Faction::default(),
            default_enemy_faction: Faction::default(),
        };
        game.default_enemy_faction = game.get_new_faction();
        assert_eq!(game.default_enemy_faction, Faction::default());

        game.red_pawn_faction = game.get_new_faction();

        game.graphics.set_empty_board_animation(board_size);
        game
    }
    pub fn board_size(&self) -> BoardSize {
        self.board_size
    }

    #[deprecated(note = "'Dead' is a negative, use `player_is_alive` instead")]
    pub fn player_is_dead(&self) -> bool {
        self.player_optional.is_none()
    }

    pub fn player_is_alive(&self) -> bool {
        self.player_optional.is_some()
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
        if self.is_non_player_piece_at(square) {
            self.capture_piece_at(square);
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
        //if let Some(player_square) = self.try_get_player_square() {
        //let king_squares = self.find_pieces(Piece::king());
        //let king_paths = king_squares .iter() .filter_map(|&king_square| self.find_king_path(king_square, player_square)) .collect();
        //self.graphics.draw_paths(king_paths);
        //}

        self.graphics.draw_move_marker_squares(
            self.move_squares_for_all_pieces(false),
            self.capture_squares_for_all_pieces(false),
            self.move_squares_for_all_pieces(true),
            self.capture_squares_for_all_pieces(true),
        );

        self.graphics.draw_blocks(&self.blocks);
        for (&square, &piece) in &self.pieces {
            let color = if piece.faction == self.red_pawn_faction {
                RED_PAWN_COLOR
            } else {
                ENEMY_PIECE_COLOR
            };
            self.graphics
                .draw_piece_with_color(square, piece.piece_type, color)
        }
        self.graphics.remove_finished_animations(time);
        self.graphics.draw_non_board_animations(time);
        if self.player_is_alive() {
            self.graphics
                .draw_player(self.player_square(), self.player_faced_direction());
            self.graphics
                .draw_field_of_view_mask(self.fov_mask_for_player());
        }
        self.graphics.display(&mut writer);
    }
    fn is_player_at(&self, square: WorldSquare) -> bool {
        self.player_is_alive() && self.try_get_player_square() == Some(square)
    }

    fn square_is_empty(&self, pos: WorldSquare) -> bool {
        !self.is_player_at(pos) && !self.is_non_player_piece_at(pos) && !self.is_block_at(pos)
    }

    pub fn place_new_king_pawn_faction(&mut self, king_square: WorldSquare) {
        let faction = self.get_new_faction();
        self.place_piece(Piece::new(PieceType::King, faction), king_square);
        for x in -1..=1 {
            for y in -1..=1 {
                let pawn_square = king_square + vec2(x, y);
                if pawn_square == king_square {
                    continue;
                }
                self.place_piece(Piece::new(Pawn, faction), pawn_square);
            }
        }
    }

    pub fn place_piece(&mut self, piece: Piece, square: WorldSquare) {
        if !self.square_is_on_board(square) {
            panic!(
                "Tried to place piece off board at {}",
                point_to_string(square)
            );
        }
        if !self.square_is_empty(square) {
            panic!("Tried to overwrite piece at {}", point_to_string(square));
        }
        self.pieces.insert(square, piece);
    }

    pub fn place_red_pawn(&mut self, square: WorldSquare) {
        self.place_piece(Piece::new(Pawn, self.red_pawn_faction), square)
    }

    pub fn tick_pawn_incubation(&mut self) {
        let found_incubation_squares: SquareSet =
            self.empty_squares_surrounded_by_pawns_of_one_faction();

        self.incubating_pawns
            .retain(|old_square, _| found_incubation_squares.contains(old_square));

        for square in found_incubation_squares {
            let faction = self.get_piece_at(square + STEP_UP).unwrap().faction;
            if let Some(existing_incubation) = self.incubating_pawns.get_mut(&square) && existing_incubation.faction == faction {
                existing_incubation.age_in_turns += 1;
                if existing_incubation.age_in_turns >= TURNS_TO_SPAWN_PAWN {
                    self.place_piece(Piece::new(Pawn, faction), square);
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

    pub fn empty_squares_surrounded_by_pawns_of_one_faction(&self) -> SquareSet {
        let mut pawn_adjacency_counter = HashMap::<(WorldSquare, Faction), u32>::new();
        self.pieces
            .iter()
            .cartesian_product(ORTHOGONAL_STEPS)
            .map(|((&pawn_square, piece), orthogonal_step)| (pawn_square + orthogonal_step, piece))
            .filter(|(adjacent_square, _)| self.square_is_empty(*adjacent_square))
            .for_each(|(adjacent_square, piece)| {
                *pawn_adjacency_counter
                    .entry((adjacent_square, piece.faction))
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

    pub fn place_piece_randomly(&mut self, piece: Piece, rng: &mut StdRng) -> WorldSquare {
        let rand_pos = self
            .random_empty_square(rng)
            .expect("failed to get random square");
        self.place_piece(piece, rand_pos);
        return rand_pos;
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

    pub fn is_non_player_piece_at(&self, square: WorldSquare) -> bool {
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
                OrderedFloat((square.to_f32() - slightly_right_of_player_position).length())
            })
            .cloned()
    }
    pub fn on_turn_start(&mut self) {}

    pub fn on_turn_end(&mut self) {
        self.tick_pawn_incubation();
        self.convert_orphaned_pieces();
        if self.player_is_alive() {
            self.select_closest_piece();
        }
    }

    pub fn convert_orphaned_pieces(&mut self) {
        for faction in self.get_all_living_factions() {
            let mut pieces_in_faction: Vec<&mut Piece> = self
                .pieces
                .iter_mut()
                .map(|(_, piece)| piece)
                .filter(|piece| piece.faction == faction)
                .collect();
            let all_same_piece_type: bool = pieces_in_faction.iter().all_equal();
            let faction_has_a_pawn = pieces_in_faction.iter().next().unwrap().piece_type == Pawn;
            let faction_has_only_pawns = all_same_piece_type && faction_has_a_pawn;
            if faction_has_only_pawns {
                pieces_in_faction
                    .iter_mut()
                    .for_each(|piece| piece.faction = self.red_pawn_faction);
            }
        }
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

    pub fn move_all_factions(&mut self) {
        for faction in self.get_all_living_factions() {
            self.move_faction(faction);
        }
    }

    fn get_all_living_factions(&self) -> HashSet<Faction> {
        self.pieces
            .values()
            .map(|piece| piece.faction)
            .unique()
            .collect()
    }

    pub fn get_new_faction(&mut self) -> Faction {
        let new_faction = self.faction_factory.get_new_faction();
        //self.faction_info.insert(new_faction, Default::default());
        new_faction
    }

    fn squares_of_pieces_in_faction(&self, faction: Faction) -> Vec<WorldSquare> {
        self.pieces
            .iter()
            .filter(|(square, piece)| piece.faction == faction)
            .map(|(&square, piece)| square)
            .collect()
    }

    fn move_faction(&mut self, faction: Faction) {
        let faction_squares = self.squares_of_pieces_in_faction(faction);

        if faction == self.red_pawn_faction {
            // all pieces move
            faction_squares.iter().for_each(|&square| {
                self.move_piece_at(square);
            });
        } else if self.player_is_alive() {
            self.move_piece_at(self.square_of_closest_piece_to_player_in_faction(faction));
        } else {
            // select one non-randomly
            let square_of_piece_to_move = faction_squares
                .into_iter()
                .min_by_key(|&square| OrderedFloat(square.x as f32 + 0.1 + square.y as f32))
                .unwrap();
            self.move_piece_at(square_of_piece_to_move);
        }
    }

    fn square_of_closest_piece_to_player_in_faction(&self, faction: Faction) -> WorldSquare {
        self.squares_of_pieces_in_faction(faction)
            .into_iter()
            .min_by_key(|&square| (square - self.player_square()).square_length())
            .unwrap()
    }

    fn move_piece(&mut self, start: WorldSquare, end: WorldSquare) {
        // capture player
        if !self.is_non_player_piece_at(start) {
            panic!("No piece to move at {}", point_to_string(start));
        }
        if self.is_player_at(end) {
            // TODO: less abrupt game-over
            self.player_optional = None;
            self.quit();
        }
        if self.is_non_player_piece_at(end) {
            let target_piece = self.pieces.get(&end).unwrap();
            let this_piece = self.pieces.get(&start).unwrap();
            if this_piece.faction == target_piece.faction {
                panic!("Tried to capture allied piece at {}", point_to_string(end));
            }
            self.capture_piece_at(end);
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
            if pass_through_pieces && self.is_non_player_piece_at(square) {
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
        if !self.player_is_alive() {
            return None;
        }
        self.move_options_for_piece_at(piece_square)
            .into_iter()
            .filter(|&square| self.square_is_empty(square) && self.square_is_on_board(square))
            .min_by_key(|&square| (square - self.player_square()).square_length())
    }

    pub fn piece_can_capture_player(&self, piece_square: WorldSquare) -> bool {
        self.player_is_alive()
            && self
                .capture_options_for_piece_at(piece_square)
                .into_iter()
                .contains(&self.player_square())
    }
    pub fn capture_square_for_piece_at(&self, piece_square: WorldSquare) -> Option<WorldSquare> {
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

    pub fn allies_within_radius_excluding_center(
        &self,
        center_square: WorldSquare,
        radius: u32,
        faction: Faction,
    ) -> SquareSet {
        let mut nearby_ally_squares = SquareSet::new();
        // intentional shadow
        let radius = radius as i32;
        (-radius..=radius).for_each(|y_offset| {
            (-radius..=radius).for_each(|x_offset| {
                let square = center_square + STEP_UP * y_offset + STEP_RIGHT * x_offset;
                if square != center_square
                    && self
                        .pieces
                        .get(&square)
                        .is_some_and(|other_piece| other_piece.faction == faction)
                {
                    nearby_ally_squares.insert(square);
                }
            });
        });
        nearby_ally_squares
    }

    pub fn protection_strengths_from_given_pawns(
        &self,
        pawn_squares: SquareSet,
    ) -> HashMap<WorldSquare, u32> {
        let steps = HashSet::from_iter(Piece::relative_capture_steps_for_type(Pawn));
        cross_correlate_squares_with_steps(pawn_squares, steps)
    }

    pub fn orthogonal_adjacency_from_given_squares(
        &self,
        squares: SquareSet,
    ) -> HashMap<WorldSquare, u32> {
        let steps = HashSet::from(ORTHOGONAL_STEPS);
        cross_correlate_squares_with_steps(squares, steps)
    }

    // returns where the piece moves to, if applicable
    pub fn move_piece_at(&mut self, piece_square: WorldSquare) -> Option<WorldSquare> {
        if !self.is_non_player_piece_at(piece_square) {
            return None;
        }

        let piece = self.get_piece_at(piece_square).unwrap().clone();

        let mut end_square: Option<WorldSquare>;

        if piece.faction == self.red_pawn_faction {
            // Look at surrounding 5x5 square
            let mut nearby_ally_squares =
                self.allies_within_radius_excluding_center(piece_square, 2, piece.faction);
            let nearby_protection_strengths =
                self.protection_strengths_from_given_pawns(nearby_ally_squares.clone());
            let nearby_ally_crowdedness =
                self.orthogonal_adjacency_from_given_squares(nearby_ally_squares.clone());
            let mut best_case_move_steps = Vec::from(ORTHOGONAL_STEPS);
            best_case_move_steps.push(vec2(0, 0));

            let viable_move_squares: HashSet<WorldSquare> = best_case_move_steps
                .iter()
                .map(|&step| piece_square + step)
                .filter(|&square| square == piece_square || self.square_is_empty(square))
                .collect();

            let neutral_goodness_at_viable_move_squares = viable_move_squares
                .iter()
                .map(|&square| (square, 0.0))
                .collect();

            let protection_at_movable_squares: HashMap<WorldSquare, u32> =
                nearby_protection_strengths
                    .into_iter()
                    .filter(|(protected_square, strength)| {
                        viable_move_squares.contains(protected_square)
                    })
                    .collect();

            let ally_crowdedness_at_movable_squares: HashMap<WorldSquare, u32> =
                nearby_ally_crowdedness
                    .into_iter()
                    .filter(|(protected_square, strength)| {
                        viable_move_squares.contains(protected_square)
                    })
                    .collect();

            let mut goodness_metric_at_move_options = map_sum(
                neutral_goodness_at_viable_move_squares,
                map_to_float(map_sum(
                    map_to_signed(protection_at_movable_squares),
                    map_neg(map_to_signed(ally_crowdedness_at_movable_squares)),
                )),
            );

            // slight preference for motion
            *goodness_metric_at_move_options
                .entry(piece_square)
                .or_default() -= 1.5;

            let current_goodness: f32 = goodness_metric_at_move_options
                .get(&piece_square)
                .cloned()
                .unwrap_or_default();
            let most_goodness_available: f32 = goodness_metric_at_move_options
                .values()
                .max_by_key(|&&x| OrderedFloat(x))
                .cloned()
                .unwrap_or_default();
            if most_goodness_available > current_goodness {
                end_square = Some(
                    goodness_metric_at_move_options
                        .iter()
                        .max_by_key(|(&square, &goodness)| OrderedFloat(goodness))
                        .unwrap()
                        .0
                        .clone(),
                );
            } else {
                end_square = None
            }
        } else if self.player_is_alive() {
            if piece.piece_type == PieceType::King {
                if let Some(path_to_player) =
                    self.find_king_path(piece_square, self.player_square())
                {
                    let first_step_square = *path_to_player.get(1).unwrap();
                    end_square = Some(first_step_square);
                } else {
                    end_square = None;
                }
            } else if self.piece_can_capture_player(piece_square) {
                end_square = Some(self.player_square());
            } else if let Some(square) =
                self.square_to_move_toward_player_for_piece_at(piece_square)
            {
                end_square = Some(square);
            } else {
                end_square = None;
            }
        } else if let Some(square) = self.capture_square_for_piece_at(piece_square) {
            end_square = Some(square);
        } else {
            end_square = None;
        }

        if let Some(move_square) = end_square {
            self.move_piece(piece_square, move_square);
        }
        end_square
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
        assert!(self.is_non_player_piece_at(piece_square));
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
        assert!(self.is_non_player_piece_at(piece_square));

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
                if self.is_non_player_piece_at(square) {
                    self.capture_piece_at(square);
                }
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
                self.capture_piece_at(square);
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

    pub fn capture_piece_at(&mut self, square: WorldSquare) {
        if !self.square_is_on_board(square) {
            panic!(
                "Tried to capture piece off board at {}",
                point_to_string(square)
            );
        }
        if !self.is_non_player_piece_at(square) {
            panic!(
                "Tried to capture an empty square at {}",
                point_to_string(square)
            );
        }
        self.pieces.remove(&square);
        self.graphics.start_piece_death_animation_at(square);
    }

    pub fn place_block(&mut self, square: WorldSquare) {
        self.blocks.insert(square);
    }
    pub fn is_block_at(&self, square: WorldSquare) -> bool {
        self.blocks.contains(&square)
    }

    pub fn set_up_vs_red_pawns(&mut self) {
        let distance = 4;
        let width = 9;
        let depth = 5;

        let start_square = self.player_square() + STEP_UP * distance + STEP_LEFT * width / 2;
        for dx in 0..width {
            for dy in 0..depth {
                let vec = vec2(dx, dy);
                self.place_red_pawn(start_square + vec);
            }
        }
    }

    pub fn set_up_vs_mini_factions(&mut self) {
        let distance = 5;
        self.place_new_king_pawn_faction(self.player_square() + STEP_UP_LEFT * distance);
        self.place_new_king_pawn_faction(self.player_square() + STEP_UP * distance);
        self.place_new_king_pawn_faction(self.player_square() + STEP_UP_RIGHT * distance);
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
                self.place_piece_randomly(Piece::from_type(piece_type), rng);
            }
        }
    }
    pub fn set_up_labyrinth_kings(&mut self, rng: &mut StdRng) {
        self.set_up_labyrinth(rng);
        for _ in 0..8 {
            self.place_piece_randomly(Piece::king(), rng);
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

    pub fn get_color_for_faction(&self, faction: Faction) -> RGB8 {
        if faction == self.red_pawn_faction {
            RED_PAWN_COLOR
        } else {
            WHITE
        }
    }
}

#[cfg(test)]
mod tests {
    use ntest::assert_false;
    use pretty_assertions::{assert_eq, assert_ne};

    use crate::glyph::glyph_constants::RED_PAWN_COLOR;
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
        game.place_new_king_pawn_faction(king_square);
        let test_square = king_square + STEP_DOWN_LEFT;
        assert_false!(game.square_is_empty(test_square));
        game.move_all_factions();
        assert!(game.square_is_empty(test_square));
    }

    #[test]
    fn test_pawn_reproduction_in_surrounded_squares() {
        let mut game = set_up_game();
        let test_square = point2(5, 5);
        let faction = game.get_new_faction();
        for step in ORTHOGONAL_STEPS {
            game.place_piece(Piece::new(Pawn, faction), test_square + step);
        }
        assert_eq!(game.pieces.len(), 4);
        for _ in 0..=TURNS_TO_SPAWN_PAWN {
            game.tick_pawn_incubation();
        }
        assert!(game.pieces.len() > 4);
    }

    #[test]
    fn test_pawn_reproduction_does_not_apply_to_filled_squares() {
        let mut game = set_up_game();

        let test_square = point2(5, 5);
        let faction = game.get_new_faction();

        for step in ORTHOGONAL_STEPS {
            game.place_piece(Piece::new(Pawn, faction), test_square + step);
        }
        game.place_piece(Piece::new(Pawn, faction), test_square);

        assert_eq!(game.pieces.len(), 5);
        for _ in 0..=TURNS_TO_SPAWN_PAWN {
            game.tick_pawn_incubation();
        }
        assert_eq!(game.pieces.len(), 5);
    }

    #[test]
    fn test_faction_with_only_pawns_becomes_red_pawns() {
        let mut game = set_up_game();

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
        let mut game = set_up_game();
        let square = point2(5, 5);
        game.place_red_pawn(square);
        game.draw_headless_now();
        let glyphs = game.graphics.get_buffered_glyphs_for_square(square);
        assert_eq!(glyphs.get(0).unwrap().fg_color, RED_PAWN_COLOR);
    }

    #[test]
    fn test_red_pawns_dont_move_if_stable() {
        let mut game = set_up_game();
        game.place_player(point2(0, 0));
        let center_square = point2(5, 5);
        let pawn_squares: SquareSet = ORTHOGONAL_STEPS
            .iter()
            .map(|&step| center_square + step)
            .collect();
        for &pawn_square in &pawn_squares {
            game.place_red_pawn(pawn_square);
        }
        game.move_all_factions();
        let found_pawn_squares: SquareSet = game.pieces.keys().cloned().collect();
        assert_eq!(pawn_squares, found_pawn_squares);
    }

    #[test]
    fn test_red_pawn_will_move_into_protection() {
        let mut game = set_up_game();
        let moving_pawn_square = point2(5, 5);
        let correct_end_square = moving_pawn_square + STEP_LEFT;
        game.place_red_pawn(correct_end_square + STEP_DOWN_LEFT);
        game.place_red_pawn(moving_pawn_square);
        game.move_piece_at(moving_pawn_square);
        assert!(game.pieces.contains_key(&correct_end_square));
    }

    #[test]
    fn test_red_pawns_dont_try_to_capture_each_other() {
        let mut game = set_up_game();
        let start_square = point2(5, 5);

        for dx in 0..3 {
            for dy in 0..3 {
                let vec = vec2(dx, dy);
                game.place_red_pawn(start_square + vec);
            }
        }
        let num_pieces_at_start = game.pieces.len();
        game.move_piece_at(start_square);
        assert_eq!(num_pieces_at_start, game.pieces.len());
    }

    #[test]
    fn test_red_pawns_try_to_not_pack_tightly() {
        let mut game = set_up_game();
        let pawn_squares = (4..=6).flat_map(|x| (4..=5).map(move |y| point2(x, y)));
        for square in pawn_squares {
            game.place_red_pawn(square);
        }
        assert_eq!(game.piece_type_count(Pawn), 6);
        let test_square = point2(5, 5);
        assert_false!(game.square_is_empty(test_square));
        game.move_piece_at(test_square);
        assert!(game.square_is_empty(test_square));
    }

    #[test]
    fn test_red_pawns_slightly_prefer_movement_over_non_movement() {
        let mut game = set_up_game();
        let pawn_square = point2(5, 5);
        game.place_red_pawn(pawn_square);
        assert_false!(game.square_is_empty(pawn_square));
        game.move_piece_at(pawn_square);
        assert!(game.square_is_empty(pawn_square));
    }
}
