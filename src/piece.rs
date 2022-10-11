use std::f32::consts::PI;
use strum_macros::Display;

use euclid::*;

use crate::{get_4_rotations, quarter_turns_counter_clockwise, WorldSpace};

#[derive(Debug, Display, Copy, Clone, Eq, PartialEq)]
pub enum PieceType {
    Pawn,
    Knight,
    Bishop,
    Rook,
    Queen,
    King,
}

#[derive(PartialEq, Debug, Copy, Clone)]
pub struct Piece {
    piece_type: PieceType,
    position: Point2D<i32, WorldSpace>,
}

impl Piece {
    pub fn new(piece_type: PieceType, position: Point2D<i32, WorldSpace>) -> Piece {
        Piece {
            piece_type,
            position,
        }
    }

    fn get_position(&self) -> Point2D<i32, WorldSpace> {
        self.position.clone()
    }

    fn get_piece_type(&self) -> PieceType {
        self.piece_type
    }

    fn get_relative_move_steps(piece_type: PieceType) -> Vec<Vector2D<i32, WorldSpace>> {
        match piece_type {
            PieceType::Pawn => get_4_rotations(Vector2D::<i32, WorldSpace>::new(1, 0)),
            _ => vec![],
        }
    }

    fn get_relative_capture_steps(piece_type: PieceType) -> Vec<Vector2D<i32, WorldSpace>> {
        match piece_type {
            PieceType::Pawn => get_4_rotations(Vector2D::<i32, WorldSpace>::new(1, 1)),
            _ => Self::get_relative_move_steps(piece_type),
        }
    }
    fn get_move_directions(piece_type: PieceType) -> Vec<Vector2D<i32, WorldSpace>> {
        match piece_type {
            _ => vec![],
        }
    }
    fn get_capture_directions(piece_type: PieceType) -> Vec<Vector2D<i32, WorldSpace>> {
        match piece_type {
            _ => Self::get_move_directions(piece_type),
        }
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashSet;
    use pretty_assertions::{assert_eq, assert_ne};

    use super::*;

    #[test]
    fn test_pawn_moveset() {
        let pawn_moveset = HashSet::from_iter(Piece::get_relative_move_steps(PieceType::Pawn));
        let correct_moveset = HashSet::from([
            vec2(1, 0),
            vec2(-1, 0),
            vec2(0, 1),
            vec2(0, -1),
        ]);
        assert_eq!(correct_moveset, pawn_moveset);
    }

    #[test]
    fn test_pawn_captureset() {
        let pawn_captureset = HashSet::from_iter(Piece::get_relative_capture_steps(PieceType::Pawn));
        let correct_captureset = HashSet::from([
            vec2(1, 1),
            vec2(-1, 1),
            vec2(1, -1),
            vec2(-1, -1),
        ]);
        assert_eq!(correct_captureset, pawn_captureset);
    }
}
