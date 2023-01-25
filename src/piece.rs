use std::f32::consts::PI;

use euclid::*;
use rgb::RGB8;
use simple_piece::SimplePiece;
use strum::IntoEnumIterator;
use strum_macros::Display;
use strum_macros::EnumIter;

use crate::glyph::DoubleGlyph;
use crate::glyph_constants::*;
use crate::piece::PieceType::*;
use crate::utility::coordinate_frame_conversions::*;
use crate::utility::{get_new_rng, random_choice, DIAGONAL_STEPS, KING_STEPS, ORTHOGONAL_STEPS};
use crate::{get_4_rotations_of, get_8_quadrants_of, quarter_turns_counter_clockwise, Glyph};

pub(crate) mod simple_piece;
mod turning_piece;

pub const MAX_PIECE_RANGE: u32 = 5;

#[derive(Debug, Display, Copy, Clone, Eq, PartialEq, EnumIter)]
pub enum Upgrade {
    BlinkRange,
}

#[derive(Debug, Display, Copy, Clone, Eq, PartialEq, EnumIter)]
pub enum PieceType {
    Pawn,
    Soldier,
    TurningSoldier,
    Knight,
    Bishop,
    Rook,
    Queen,
    King,
    DeathCubeTurret,
}

#[derive(Debug, Copy, Clone, Default, Eq, PartialEq, Hash)]
pub struct Faction {
    pub(crate) id: u32,
}

impl Faction {
    pub fn from_id(id: u32) -> Faction {
        Faction { id }
    }
}

pub struct FactionFactory {
    id_of_next_faction: u32,
}

impl FactionFactory {
    pub fn new() -> FactionFactory {
        FactionFactory {
            // Default faction is 0.  This is also the first generated.
            id_of_next_faction: 0,
        }
    }
    pub fn get_new_faction(&mut self) -> Faction {
        let faction = Faction::from_id(self.id_of_next_faction);
        self.id_of_next_faction += 1;
        faction
    }
}

pub trait Turnable {
    fn faced_direction(&self) -> WorldStep;
    fn turn_to_face_direction(&mut self, new_dir: WorldStep);
}

pub trait Movable {
    fn relative_move_steps(&self) -> StepList;
    fn relative_capture_steps(&self) -> StepList;
    fn move_directions(&self) -> StepList;
    fn capture_directions(&self) -> StepList;
}

pub trait Piece {
    fn piece_type(&self) -> PieceType;
    fn faction(&self) -> Faction;
    fn new(piece_type: PieceType, faction: Faction) -> Self;
    fn from_type(piece_type: PieceType) -> Self;
}

#[cfg(test)]
mod tests {
    use std::collections::HashSet;

    use pretty_assertions::{assert_eq, assert_ne};

    use super::*;

    #[test]
    fn test_pawn_moveset() {
        let pawn_moveset = HashSet::from_iter(SimplePiece::pawn().relative_move_steps());
        let correct_moveset = HashSet::from([vec2(1, 0), vec2(-1, 0), vec2(0, 1), vec2(0, -1)]);
        assert_eq!(correct_moveset, pawn_moveset);
    }

    #[test]
    fn test_pawn_captureset() {
        let pawn_captureset = HashSet::from_iter(SimplePiece::pawn().relative_capture_steps());
        let correct_captureset =
            HashSet::from([vec2(1, 1), vec2(-1, 1), vec2(1, -1), vec2(-1, -1)]);
        assert_eq!(correct_captureset, pawn_captureset);
    }
}
