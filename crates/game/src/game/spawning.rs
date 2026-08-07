//! Pawn/drone/piece spawning logic, extracted from the `game` god module
//! (ROADMAP.md item 1, step 4). Implemented as an `impl Game` block in a
//! submodule, plus the `IncubatingPawn` state type. Generic map geometry
//! placement (`place_block`, portals, conveyor belts, widgets) and the
//! `set_up_*` test/demo map builders stay in `mod.rs`.

use std::collections::HashMap;

use euclid::{vec2, Angle};
use itertools::Itertools;
use rand::rngs::StdRng;
use rand::Rng;

use super::{DeathCube, FloatingHunterDrone};

use crate::piece::PieceType::*;
use crate::piece::*;
use crate::*;

use super::Game;

pub(crate) const TURNS_TO_SPAWN_PAWN: u32 = 10;

#[derive(Clone, Eq, PartialEq, Debug, Copy)]
pub struct IncubatingPawn {
    pub age_in_turns: u32,
    pub faction: Faction,
}


impl Game {
    pub fn place_new_king_pawn_faction(&mut self, king_square: WorldSquare) {
        let faction = self.get_new_faction();
        self.place_piece(Piece::new(King, faction), king_square);
        for x in -1..=1 {
            for y in -1..=1 {
                let pawn_square = king_square + vec2(x, y);
                if pawn_square == king_square {
                    continue;
                }
                self.place_piece(Piece::new(OmniDirectionalPawn, faction), pawn_square);
            }
        }
    }

    pub fn place_random_3x3_faction(&mut self, king_square: WorldSquare) {
        let faction = self.get_new_faction();
        self.place_piece(Piece::new(King, faction), king_square);
        for x in -1..=1 {
            for y in -1..=1 {
                let square = king_square + vec2(x, y);
                if square == king_square {
                    continue;
                }
                self.place_piece(
                    Piece::new(Piece::random_subordinate_type(), faction),
                    square,
                );
            }
        }
    }

    pub fn place_linear_death_cube(&mut self, position: WorldPoint, velocity: WorldMove) {
        self.death_cubes.push(DeathCube::new(position, velocity));
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
        self.place_piece(
            Piece::new(OmniDirectionalPawn, self.red_pawn_faction),
            square,
        )
    }

    pub fn place_death_turret(&mut self, square: WorldSquare) {
        self.place_piece(Piece::new(DeathCubeTurret, self.death_cube_faction), square);
    }

    pub fn place_floating_hunter_drone(
        &mut self,
        point: WorldPoint,
        velocity: WorldMove,
        sight_angle: Angle<f32>,
    ) {
        self.floating_hunter_drones
            .push(FloatingHunterDrone::new(point, velocity, sight_angle));
    }

    pub fn place_upgrade(&mut self, upgrade_type: Upgrade, square: WorldSquare) {
        assert!(self.square_is_empty(square));
        self.blocks.place_upgrade(upgrade_type, square);
    }

    pub fn tick_pawn_incubation(&mut self) {
        let found_incubation_squares: SquareSet =
            self.empty_squares_surrounded_by_pawns_of_one_faction();

        self.incubating_pawns
            .retain(|old_square, _| found_incubation_squares.contains(old_square));

        for square in found_incubation_squares {
            let faction = self.get_piece_at(square + STEP_UP).unwrap().faction;
            let maybe_existing_incubation = self.incubating_pawns.get_mut(&square);
            if maybe_existing_incubation
                .as_ref()
                .is_some_and(|incubation| incubation.faction == faction)
            {
                let existing_incubation = maybe_existing_incubation.unwrap();
                existing_incubation.age_in_turns += 1;
                if existing_incubation.age_in_turns >= TURNS_TO_SPAWN_PAWN {
                    self.place_piece(Piece::new(OmniDirectionalPawn, faction), square);
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

    pub fn get_new_faction(&mut self) -> Faction {
        let new_faction = self.faction_factory.get_new_faction();
        //self.faction_info.insert(new_faction, Default::default());
        new_faction
    }

}
