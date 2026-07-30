//! Turn advancement and game-over handling, extracted from the `game` god
//! module (ROADMAP.md item 1, step 5). Implemented as an `impl Game` block in
//! a submodule; private methods are `pub(crate)` because `Game`'s
//! orchestration methods and inline tests still live in `mod.rs`.

use std::collections::{HashMap, HashSet};

use itertools::Itertools;

use crate::piece::PieceType::*;
use crate::piece::*;
use crate::*;
use terminal_rendering::*;
use utility::*;

use super::Game;

impl Game {
    pub fn quit(&mut self) {
        self.running = false;
    }
    pub fn running(&self) -> bool {
        self.running
    }

    pub(crate) fn kill_player(&mut self) {
        // TODO: less abrupt game-over
        self.player_optional = None;
        self.quit();
    }

    pub fn tick_game_logic(&mut self) {
        self.move_non_arrow_factions();
        self.tick_projectile_arrows();
        self.tick_floor_push_arrows();

        self.on_turn_end();
    }

    fn tick_floor_push_arrows(&mut self) {
        let push_directions: HashMap<WorldSquare, KingWorldStep> = self
            .floor_push_arrows
            .iter()
            .map(|(&start_square, &push_direction)| (start_square, push_direction.into()))
            .collect();
        self.simultaneously_push_several_grid_entities(&push_directions);
        self.simultaneously_push_floating_entities_at_several_squares(&push_directions, 1.0);
    }

    pub(crate) fn simultaneously_push_several_grid_entities(
        &mut self,
        push_directions: &HashMap<WorldSquare, KingWorldStep>,
    ) {
        let mut push_end_squares = HashSet::new();
        push_directions
            .iter()
            .for_each(|(&start_square, &push_direction)| {
                let already_pushed_something_to_here = push_end_squares.contains(&start_square);
                if !already_pushed_something_to_here {
                    let push_end_pose = self.portal_aware_single_step(SquareWithKingDir::new(
                        start_square,
                        push_direction.into(),
                    ));
                    if let Ok((end_square, end_dir)) = push_end_pose.map(|x| x.tuple()) {
                        self.try_push_grid_entity(start_square, push_direction.into())
                            .ok();
                        push_end_squares.insert(end_square);
                    }
                }
            });
    }
    pub(crate) fn simultaneously_push_floating_entities_at_several_squares(
        &mut self,
        push_directions: &HashMap<WorldSquare, KingWorldStep>,
        push_distance: f32,
    ) {
        let mut push_end_squares = HashSet::new();
        push_directions
            .clone()
            .iter()
            .for_each(|(&start_square, &push_direction)| {
                let already_pushed_something_to_here = push_end_squares.contains(&start_square);
                if !already_pushed_something_to_here {
                    let push_end_pose = self.portal_aware_single_step(SquareWithKingDir::new(
                        start_square,
                        push_direction.into(),
                    ));
                    if let Ok((end_square, end_dir)) = push_end_pose.map(|x| x.tuple()) {
                        self.push_floating_entities_that_are_in_square_in_king_direction(
                            start_square,
                            push_direction.into(),
                            push_distance,
                        );
                        push_end_squares.insert(end_square);
                    }
                }
            });
    }

    fn drain_arrows(&mut self) -> HashMap<WorldSquare, KingWorldStep> {
        let old_arrows = self.arrows();
        old_arrows.iter().for_each(|(square, _)| {
            self.pieces.remove(&square);
        });
        old_arrows
    }

    fn set_arrows(&mut self, new_arrows: HashMap<WorldSquare, KingWorldStep>) {
        new_arrows.into_iter().for_each(|(square, dir)| {
            self.pieces.insert(square, Piece::arrow(dir));
        });
    }

    pub fn tick_projectile_arrows(&mut self) {
        let old_arrows = self.drain_arrows();

        // arrows that hit arrows, blocks, or board edges disappear
        let mut next_arrows = HashMap::<WorldSquare, KingWorldStep>::new();
        let mut arrow_midair_collisions = SquareSet::new();
        let mut capture_squares = SquareSet::new();
        old_arrows
            .iter()
            .for_each(|(&square, &dir): (&WorldSquare, &KingWorldStep)| {
                if let Ok(next_pose) =
                    self.portal_aware_single_step(SquareWithKingDir::new(square, dir))
                {
                    let (next_square, next_dir) = next_pose.tuple();
                    if self.is_piece_at(next_square) {
                        capture_squares.insert(next_square);
                    }
                    if !self.is_block_at(next_square)
                        && self.square_is_on_board(next_square)
                        && !arrow_midair_collisions.contains(&next_square)
                    {
                        let is_new_midair_collision = next_arrows.contains_key(&next_square);
                        if is_new_midair_collision {
                            next_arrows.remove(&next_square);
                            arrow_midair_collisions.insert(next_square);
                        } else {
                            next_arrows.insert(next_square, next_dir);
                        }
                    }
                }
            });
        // apply captures
        capture_squares.into_iter().for_each(|square| {
            self.try_capture_piece_at(square).ok();
        });

        self.set_arrows(next_arrows);
    }

    pub fn on_turn_end(&mut self) {
        self.tick_pawn_incubation();
        self.convert_orphaned_pieces();
        if self.player_is_alive() {
            self.select_closest_piece();
        }
    }

    pub fn convert_orphaned_pieces(&mut self) {
        for faction in self.get_enemy_factions() {
            let mut pieces_in_faction: Vec<&mut Piece> = self
                .pieces
                .iter_mut()
                .map(|(_, piece)| piece)
                .filter(|piece| piece.faction == faction)
                .collect();
            let all_same_piece_type: bool = pieces_in_faction.iter().all_equal();
            let faction_has_a_pawn =
                pieces_in_faction.iter().next().unwrap().piece_type == OmniDirectionalPawn;
            let faction_has_only_pawns = all_same_piece_type && faction_has_a_pawn;
            if faction_has_only_pawns {
                pieces_in_faction
                    .iter_mut()
                    .for_each(|piece| piece.faction = self.red_pawn_faction);
            }
        }
    }

    pub fn move_all_pieces(&mut self) {
        self.move_non_arrow_factions();
        self.tick_projectile_arrows();
        self.turn_count += 1;
    }
}
