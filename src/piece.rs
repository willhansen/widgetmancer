use std::f32::consts::PI;

use euclid::*;
use rgb::RGB8;
use strum::IntoEnumIterator;
use strum_macros::Display;
use strum_macros::EnumIter;

use crate::glyph_constants::*;

use crate::glyph::DoubleGlyph;
use crate::piece::PieceType::*;
use crate::utility::coordinate_frame_conversions::*;
use crate::utility::{get_new_rng, random_choice, DIAGONAL_STEPS, KING_STEPS, ORTHOGONAL_STEPS};
use crate::{get_4_rotations_of, get_8_quadrants_of, quarter_turns_counter_clockwise, Glyph};

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

#[derive(PartialEq, Debug, Copy, Clone)]
pub struct Piece {
    pub piece_type: PieceType,
    pub faction: Faction,
}

impl Piece {
    pub fn new(piece_type: PieceType, faction: Faction) -> Piece {
        Piece {
            piece_type,
            faction,
        }
    }

    pub fn from_type(piece_type: PieceType) -> Piece {
        Piece::new(piece_type, Faction::default())
    }

    pub fn pawn() -> Piece {
        Piece::from_type(Pawn)
    }
    pub fn knight() -> Piece {
        Piece::from_type(Knight)
    }
    pub fn bishop() -> Piece {
        Piece::from_type(Bishop)
    }
    pub fn rook() -> Piece {
        Piece::from_type(Rook)
    }
    pub fn queen() -> Piece {
        Piece::from_type(Queen)
    }
    pub fn king() -> Piece {
        Piece::from_type(King)
    }

    pub fn random_subordinate_type() -> PieceType {
        let mut rng = get_new_rng();
        let options = vec![Pawn, Knight, Bishop, Rook, Queen];
        *random_choice(&mut rng, &options)
    }

    pub fn glyphs(&self) -> DoubleGlyph {
        Piece::glyphs_for_type(self.piece_type)
    }

    pub fn glyphs_for_type(piece_type: PieceType) -> DoubleGlyph {
        Piece::chars_for_type(piece_type)
            .map(|character| Glyph::fg_only(character, ENEMY_PIECE_COLOR))
    }

    pub fn chars_for_type(piece_type: PieceType) -> [char; 2] {
        match piece_type {
            Pawn => ['♟', ' '],
            Soldier => ['S', 'o'],
            Knight => ['♞', ' '],
            Bishop => ['♝', ' '],
            Rook => ['♜', ' '],
            Queen => ['♛', ' '],
            King => ['♚', ' '],
            DeathCubeTurret => ['𐄳', ' '],
            _ => panic!("Tried to draw unknown piece type: {:?}", piece_type),
        }
    }

    pub fn relative_move_steps_for_type(piece_type: PieceType) -> StepList {
        match piece_type {
            Pawn | Soldier => ORTHOGONAL_STEPS.into(),
            King => KING_STEPS.into(),
            Knight => get_8_quadrants_of(WorldStep::new(1, 2)),
            _ => vec![],
        }
    }
    pub fn relative_move_steps(&self) -> StepList {
        Self::relative_move_steps_for_type(self.piece_type)
    }

    pub fn relative_capture_steps_for_type(piece_type: PieceType) -> StepList {
        match piece_type {
            Pawn => DIAGONAL_STEPS.into(),
            _ => Self::relative_move_steps_for_type(piece_type),
        }
    }
    pub fn relative_capture_steps(&self) -> StepList {
        Self::relative_capture_steps_for_type(self.piece_type)
    }

    pub fn move_directions_for_type(piece_type: PieceType) -> StepList {
        match piece_type {
            Bishop => get_4_rotations_of(WorldStep::new(1, 1)),
            Rook => get_4_rotations_of(WorldStep::new(1, 0)),
            Queen => (0..=1)
                .map(|y| WorldStep::new(1, y))
                .map(get_4_rotations_of)
                .flatten()
                .collect(),
            _ => vec![],
        }
    }
    pub fn move_directions(&self) -> StepList {
        Self::move_directions_for_type(self.piece_type)
    }

    pub fn capture_directions_for_type(piece_type: PieceType) -> StepList {
        match piece_type {
            _ => Self::move_directions_for_type(piece_type),
        }
    }

    pub fn capture_directions(&self) -> StepList {
        Self::capture_directions_for_type(self.piece_type)
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashSet;

    use pretty_assertions::{assert_eq, assert_ne};

    use super::*;

    #[test]
    fn test_pawn_moveset() {
        let pawn_moveset = HashSet::from_iter(Piece::pawn().relative_move_steps());
        let correct_moveset = HashSet::from([vec2(1, 0), vec2(-1, 0), vec2(0, 1), vec2(0, -1)]);
        assert_eq!(correct_moveset, pawn_moveset);
    }

    #[test]
    fn test_pawn_captureset() {
        let pawn_captureset = HashSet::from_iter(Piece::pawn().relative_capture_steps());
        let correct_captureset =
            HashSet::from([vec2(1, 1), vec2(-1, 1), vec2(1, -1), vec2(-1, -1)]);
        assert_eq!(correct_captureset, pawn_captureset);
    }
}
