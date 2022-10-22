use std::f32::consts::PI;

use euclid::*;
use strum_macros::Display;

use crate::{
    get_4_rotations, quarter_turns_counter_clockwise, Glyph, SquareGridInWorldFrame, StepList,
    WorldStep,
};

#[derive(Debug, Display, Copy, Clone, Eq, PartialEq)]
pub enum PieceType {
    Pawn,
    Knight,
    Bishop,
    Rook,
    Queen,
    King,
}

#[derive(Debug, Display, Copy, Clone, Eq, PartialEq)]
pub enum AiType {
    TowardsPlayer,
    AwayFromPlayer,
    Random,
}

#[derive(PartialEq, Debug, Copy, Clone)]
pub struct Piece {
    pub piece_type: PieceType,
    pub ai_type: AiType,
}

impl Piece {
    pub fn new(piece_type: PieceType, ai_type: AiType) -> Piece {
        Piece {
            piece_type,
            ai_type,
        }
    }

    pub fn pawn() -> Piece {
        Piece {
            piece_type: PieceType::Pawn,
            ai_type: AiType::TowardsPlayer,
        }
    }

    pub fn glyphs(&self) -> [Glyph; 2] {
        Piece::chars_for_type(self.piece_type).map(Glyph::from_char)
    }

    pub fn chars_for_type(piece_type: PieceType) -> [char; 2] {
        match piece_type {
            PieceType::Pawn => ['P', 'a'],
            _ => panic!("invalid type"),
        }
    }

    pub fn relative_move_steps_for_type(piece_type: PieceType) -> StepList {
        match piece_type {
            PieceType::Pawn => get_4_rotations(WorldStep::new(1, 0)),
            _ => vec![],
        }
    }
    pub fn relative_move_steps(&self) -> StepList {
        Self::relative_move_steps_for_type(self.piece_type)
    }

    pub fn relative_capture_steps_for_type(piece_type: PieceType) -> StepList {
        match piece_type {
            PieceType::Pawn => get_4_rotations(Vector2D::<i32, SquareGridInWorldFrame>::new(1, 1)),
            _ => Self::relative_move_steps_for_type(piece_type),
        }
    }
    pub fn get_relative_capture_steps(&self) -> StepList {
        Self::relative_capture_steps_for_type(self.piece_type)
    }

    pub fn move_directions_for_type(piece_type: PieceType) -> StepList {
        match piece_type {
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
        let pawn_captureset = HashSet::from_iter(Piece::pawn().get_relative_capture_steps());
        let correct_captureset =
            HashSet::from([vec2(1, 1), vec2(-1, 1), vec2(1, -1), vec2(-1, -1)]);
        assert_eq!(correct_captureset, pawn_captureset);
    }
}
