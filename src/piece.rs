use std::f32::consts::PI;

use euclid::*;
use strum::IntoEnumIterator;
use strum_macros::Display;
use strum_macros::EnumIter;

use crate::glyph_constants::*;

use crate::utility::coordinate_frame_conversions::*;
use crate::{get_4_rotations_of, get_8_quadrants_of, quarter_turns_counter_clockwise, Glyph};

#[derive(Debug, Display, Copy, Clone, Eq, PartialEq, EnumIter)]
pub enum PieceType {
    Pawn,
    Knight,
    Bishop,
    Rook,
    Queen,
    King,
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
            // Default faction is 0.  Don't want to overlap with that
            id_of_next_faction: 1,
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
        Piece::from_type(PieceType::Pawn)
    }
    pub fn knight() -> Piece {
        Piece::from_type(PieceType::Knight)
    }
    pub fn bishop() -> Piece {
        Piece::from_type(PieceType::Bishop)
    }
    pub fn rook() -> Piece {
        Piece::from_type(PieceType::Rook)
    }
    pub fn queen() -> Piece {
        Piece::from_type(PieceType::Queen)
    }
    pub fn king() -> Piece {
        Piece::from_type(PieceType::King)
    }

    pub fn glyphs(&self) -> [Glyph; 2] {
        Piece::chars_for_type(self.piece_type)
            .map(|character| Glyph::fg_only(character, ENEMY_PIECE_COLOR))
    }

    pub fn chars_for_type(piece_type: PieceType) -> [char; 2] {
        match piece_type {
            PieceType::Pawn => ['♟', ' '],
            PieceType::Knight => ['♞', ' '],
            PieceType::Bishop => ['♝', ' '],
            PieceType::Rook => ['♜', ' '],
            PieceType::Queen => ['♛', ' '],
            PieceType::King => ['♚', ' '],
        }
    }

    pub fn relative_move_steps_for_type(piece_type: PieceType) -> StepList {
        match piece_type {
            PieceType::Pawn => get_4_rotations_of(WorldStep::new(1, 0)),
            PieceType::King => (0..=1)
                .map(|y| WorldStep::new(1, y))
                .map(get_4_rotations_of)
                .flatten()
                .collect(),
            PieceType::Knight => get_8_quadrants_of(WorldStep::new(1, 2)),
            _ => vec![],
        }
    }
    pub fn relative_move_steps(&self) -> StepList {
        Self::relative_move_steps_for_type(self.piece_type)
    }

    pub fn relative_capture_steps_for_type(piece_type: PieceType) -> StepList {
        match piece_type {
            PieceType::Pawn => get_4_rotations_of(WorldStep::new(1, 1)),
            _ => Self::relative_move_steps_for_type(piece_type),
        }
    }
    pub fn relative_capture_steps(&self) -> StepList {
        Self::relative_capture_steps_for_type(self.piece_type)
    }

    pub fn move_directions_for_type(piece_type: PieceType) -> StepList {
        match piece_type {
            PieceType::Bishop => get_4_rotations_of(WorldStep::new(1, 1)),
            PieceType::Rook => get_4_rotations_of(WorldStep::new(1, 0)),
            PieceType::Queen => (0..=1)
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
