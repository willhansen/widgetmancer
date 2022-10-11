use std::f32::consts::PI;
use strum_macros::Display;

use euclid::*;

use crate::{get_4_rotations, quarter_turns_counter_clockwise, WorldSpace};

#[derive(Debug, Display)]
pub enum PieceType {
    Pawn,
    Knight,
    Bishop,
    Rook,
    Queen,
    King,
}

pub fn make_piece(piece_type: PieceType, position: Point2D<i32, WorldSpace>) -> Box<dyn Piece> {
    Box::new(
        match piece_type {
            PieceType::Pawn => Pawn { position },
            _ => panic!("tried to make bad piece type {}", piece_type.to_string())
        })
}

pub trait Piece {
    fn get_position(&self) -> Point2D<i32, WorldSpace>;
    fn get_piece_type(&self) -> PieceType;
    fn get_move_squares(&self) -> Vec<Vector2D<i32, WorldSpace>> {
        vec![]
    }
    fn get_capture_squares(&self) -> Vec<Vector2D<i32, WorldSpace>> {
        self.get_move_squares()
    }
    fn get_move_directions(&self) -> Vec<Vector2D<i32, WorldSpace>> {
        vec![]
    }
    fn get_capture_directions(&self) -> Vec<Vector2D<i32, WorldSpace>> {
        self.get_move_directions()
    }
}

#[derive(PartialEq, Debug, Copy, Clone, Default)]
pub struct Pawn {
    position: Point2D<i32, WorldSpace>,
}

impl Piece for Pawn {
    fn get_position(&self) -> Point2D<i32, WorldSpace> {
        self.position.clone()
    }

    fn get_piece_type(&self) -> PieceType {
        PieceType::Pawn
    }

    fn get_move_squares(&self) -> Vec<Vector2D<i32, WorldSpace>> {
        get_4_rotations(Vector2D::<i32, WorldSpace>::new(1, 0))
    }

    fn get_capture_squares(&self) -> Vec<Vector2D<i32, WorldSpace>> {
        get_4_rotations(Vector2D::<i32, WorldSpace>::new(1, 1))
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashSet;
    use pretty_assertions::{assert_eq, assert_ne};

    use super::*;

    #[test]
    fn test_pawn_moveset() {
        let pawn_moveset = HashSet::from_iter(Pawn::default().get_move_squares());
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
        let pawn_captureset = HashSet::from_iter(Pawn::get_capture_squares());
        let correct_captureset = HashSet::from([
            vec2(1, 1),
            vec2(-1, 1),
            vec2(1, -1),
            vec2(-1, -1),
        ]);
        assert_eq!(correct_captureset, pawn_captureset);
    }
}
