//! Enemy pathfinding and decision logic, extracted from the `game` god module
//! (ROADMAP.md item 1, step 3). Implemented as an `impl Game` block in a
//! submodule; private methods are `pub(crate)` because `Game`'s orchestration
//! methods and inline tests still live in `mod.rs`.

use std::collections::{HashMap, HashSet};

use euclid::{vec2, Angle};

use itertools::Itertools;
use ordered_float::OrderedFloat;
use priority_queue::DoublePriorityQueue;

use crate::piece::PieceType::*;
use crate::piece::*;
use crate::*;
use terminal_rendering::*;
use utility::*;

use super::Game;

impl Game {
    pub(crate) fn non_arrow_piece_squares(&self) -> SquareSet {
        self.pieces
            .iter()
            .filter(|(&_square, &piece)| piece.piece_type != Arrow)
            .map(|(&square, &_piece)| square)
            .collect()
    }

    pub fn move_non_arrow_factions(&mut self) {
        for faction in self.get_enemy_factions() {
            self.move_faction(faction);
        }
    }

    pub(crate) fn get_enemy_factions(&self) -> HashSet<Faction> {
        self.pieces
            .values()
            .map(|piece| piece.faction)
            .unique()
            .filter(|&faction| matches!(faction, Faction::Enemy(_) | Faction::RedPawn))
            .collect()
    }

    pub(crate) fn squares_of_pieces_in_faction(&self, faction: Faction) -> Vec<WorldSquare> {
        self.pieces
            .iter()
            .filter(|(square, piece)| piece.faction == faction)
            .map(|(&square, piece)| square)
            .collect()
    }

    pub(crate) fn move_faction(&mut self, faction: Faction) {
        let faction_squares = self.squares_of_pieces_in_faction(faction);

        if faction == self.red_pawn_faction {
            // all pieces move
            faction_squares.iter().for_each(|&square| {
                self.move_piece_at_square_and_return_end_position_if_moved(square);
            });
        } else if self.player_is_alive() {
            self.move_piece_at_square_and_return_end_position_if_moved(
                self.square_of_closest_piece_to_player_in_faction(faction),
            );
        } else {
            // select one non-randomly
            let square_of_piece_to_move = faction_squares
                .into_iter()
                .min_by_key(|&square| OrderedFloat(square.x as f32 + 0.1 + square.y as f32))
                .unwrap();
            self.move_piece_at_square_and_return_end_position_if_moved(square_of_piece_to_move);
        }
    }

    pub(crate) fn square_of_closest_piece_to_player_in_faction(&self, faction: Faction) -> WorldSquare {
        self.squares_of_pieces_in_faction(faction)
            .into_iter()
            .min_by_key(|&square| (square - self.player_square()).square_length())
            .unwrap()
    }
    pub(crate) fn move_piece(&mut self, start: WorldSquare, end: WorldSquare) {
        // capture player
        if !self.is_non_player_piece_at(start) {
            panic!("No piece to move at {}", point_to_string(start));
        }
        if self.is_player_at(end) {
            self.kill_player();
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

    pub(crate) fn slide_cast(
        &self,
        start_square: WorldSquare,
        repeating_step: NStep,
        pass_through_pieces: bool,
    ) -> SquareList {
        let mut valid_squares: SquareList = vec![];
        let range_cap: u32 = repeating_step.n().unwrap_or(MAX_PIECE_RANGE);
        for i in 0..range_cap {
            let distance = i + 1;
            // TODO: Allow knights to step through portals (probably by line-of-sight between start and end squares)
            let square = if is_king_step(repeating_step.stepp()) {
                if let Ok(end_pose) = self.multiple_portal_aware_steps(
                    SquareWithKingDir::from_square_and_step(
                        start_square,
                        repeating_step.stepp().into(),
                    ),
                    distance,
                ) {
                    end_pose.square()
                } else {
                    break;
                }
            } else {
                start_square + repeating_step.stepp() * distance as i32
            };
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
        }
        valid_squares
    }

    pub fn square_to_move_toward_player_for_piece_at(
        &self,
        piece_square: WorldSquare,
    ) -> Option<WorldSquare> {
        if !self.player_is_alive() {
            return None;
        }
        let current_square_distance_to_player =
            (self.player_square() - piece_square).square_length();
        let closest_move_option_to_player = self
            .move_options_for_piece_at(piece_square)
            .into_iter()
            .filter(|&square| self.square_is_empty(square) && self.square_is_on_board(square))
            .min_by_key(|&square| (square - self.player_square()).square_length());
        if let Some(end_square) = closest_move_option_to_player {
            let possible_square_distance = (end_square - self.player_square()).square_length();
            if possible_square_distance < current_square_distance_to_player {
                return closest_move_option_to_player;
            }
        }
        None
    }

    pub fn piece_can_capture_player(&self, piece_square: WorldSquare) -> bool {
        self.player_is_alive()
            && self
                .capture_options_for_piece_at(piece_square)
                .into_iter()
                .contains(&self.player_square())
    }
    pub fn highest_priority_capture_square_for_piece_at(
        &self,
        piece_square: WorldSquare,
    ) -> Option<WorldSquare> {
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
        let steps = HashSet::from_iter(DIAGONAL_STEPS);
        cross_correlate_squares_with_steps(pawn_squares, steps)
    }

    pub fn orthogonal_adjacency_from_given_squares(
        &self,
        squares: SquareSet,
    ) -> HashMap<WorldSquare, u32> {
        let steps = HashSet::from(ORTHOGONAL_STEPS);
        cross_correlate_squares_with_steps(squares, steps)
    }

    pub(crate) fn move_red_pawn_at(&mut self, piece_square: WorldSquare) -> Option<WorldSquare> {
        let mut end_square: Option<WorldSquare>;
        let piece = self.get_piece_at(piece_square).unwrap().clone();
        assert_eq!(piece.faction, self.red_pawn_faction);
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

        let protection_at_movable_squares: HashMap<WorldSquare, u32> = nearby_protection_strengths
            .into_iter()
            .filter(|(protected_square, strength)| viable_move_squares.contains(protected_square))
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

        if let Some(move_square) = end_square {
            self.move_piece(piece_square, move_square);
        }
        end_square
    }

    // returns where the piece moves to, if applicable
    pub fn move_piece_at_square_and_return_end_position_if_moved(
        &mut self,
        piece_square: WorldSquare,
    ) -> Option<WorldSquare> {
        if !self.is_non_player_piece_at(piece_square) {
            return None;
        }

        let piece = self.get_piece_at(piece_square).unwrap().clone();

        let mut end_square: Option<WorldSquare>;

        if piece.faction == self.red_pawn_faction {
            return self.move_red_pawn_at(piece_square);
        } else if self.player_is_alive() {
            if piece.piece_type == King {
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
            } else if piece.can_turn() {
                self.turn_piece_toward_player(piece_square);
                return Some(piece_square);
            } else {
                end_square = None;
            }
        } else if let optional_square =
            self.highest_priority_capture_square_for_piece_at(piece_square)
        {
            end_square = optional_square;
        } else {
            end_square = None;
        }

        if let Some(move_square) = end_square {
            self.move_piece(piece_square, move_square);
        }
        end_square
    }

    pub(crate) fn turn_piece_toward_player(&mut self, piece_square: WorldSquare) {
        assert!(self.is_non_player_piece_at(piece_square));
        let piece = self.get_piece_at(piece_square).unwrap();
        assert!(piece.can_turn());
        assert!(self.player_is_alive());

        let vector_to_player = self.player_square() - piece_square;
        let angle_to_player = |p: &Piece| -> Angle<f32> {
            p.faced_direction()
                .step()
                .to_f32()
                .angle_to(vector_to_player.to_f32())
        };
        let mut best_angle_to_player_yet = angle_to_player(piece);
        let mut best_rotation_yet = piece.clone();
        for turned_piece in piece.turned_versions() {
            let possible_angle_to_player = angle_to_player(&turned_piece);
            if possible_angle_to_player.radians.abs() < best_angle_to_player_yet.radians.abs() {
                best_rotation_yet = turned_piece;
                best_angle_to_player_yet = possible_angle_to_player;
            }
        }

        if best_rotation_yet != *piece {
            self.pieces.remove(&piece_square);
            self.pieces.insert(piece_square, best_rotation_yet);
        }
    }

    pub(crate) fn move_options_for_piece_at(&self, piece_square: WorldSquare) -> SquareList {
        self.on_board_move_squares_for_piece_at(piece_square, false)
            .into_iter()
            .filter(|&square| self.square_is_empty(square))
            .collect()
    }

    pub(crate) fn on_board_move_or_capture_squares_for_piece_at(
        &self,
        piece_square: WorldSquare,
        capture_instead_of_move: bool,
        pass_through_pieces: bool,
    ) -> SquareSet {
        assert!(self.is_non_player_piece_at(piece_square));
        let mut squares = SquareSet::new();
        let piece = self.get_piece_at(piece_square).unwrap();

        let move_function = if capture_instead_of_move {
            Piece::relative_captures
        } else {
            Piece::relative_moves
        };

        for move_direction in move_function(piece) {
            let mut squares_to_collision =
                self.slide_cast(piece_square, move_direction, pass_through_pieces);
            squares.extend(squares_to_collision);
        }
        squares
    }

    pub(crate) fn on_board_capture_squares_for_piece_at(
        &self,
        piece_square: WorldSquare,
        pass_through_pieces: bool,
    ) -> SquareSet {
        self.on_board_move_or_capture_squares_for_piece_at(piece_square, true, pass_through_pieces)
    }
    pub(crate) fn on_board_move_squares_for_piece_at(
        &self,
        piece_square: WorldSquare,
        pass_through_pieces: bool,
    ) -> SquareSet {
        self.on_board_move_or_capture_squares_for_piece_at(piece_square, false, pass_through_pieces)
    }

    pub(crate) fn move_squares_for_all_pieces(&self, pass_through_pieces: bool) -> SquareSet {
        self.pieces
            .keys()
            .map(|&square| self.on_board_move_squares_for_piece_at(square, pass_through_pieces))
            .flatten()
            .collect()
    }
    pub(crate) fn squares_threatened_by_any_piece(&self, pass_through_pieces: bool) -> SquareSet {
        self.pieces
            .keys()
            .map(|&square| self.on_board_capture_squares_for_piece_at(square, pass_through_pieces))
            .flatten()
            .collect()
    }

    pub(crate) fn guarded_squares_for_piece_at(&self, piece_square: WorldSquare) -> SquareSet {
        self.on_board_capture_squares_for_piece_at(piece_square, false)
    }

    pub(crate) fn find_king_path(
        &self,
        start_square: WorldSquare,
        target_square: WorldSquare,
    ) -> Option<Vec<WorldSquare>> {
        fn cost_heuristic(a: WorldSquare, b: WorldSquare) -> u32 {
            king_distance(a - b)
        }
        let relative_steps = KING_STEPS;
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

    pub(crate) fn capture_options_for_piece_at(&self, piece_square: WorldSquare) -> SquareList {
        assert!(self.is_non_player_piece_at(piece_square));

        let mut capture_squares: SquareList = vec![];

        for square in self.guarded_squares_for_piece_at(piece_square) {
            if !self.square_is_empty(square) {
                capture_squares.push(square);
            }
        }
        capture_squares
    }
}
