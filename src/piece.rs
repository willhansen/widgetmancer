use std::f32::consts::PI;

use euclid::*;

use crate::{get_4_rotations, quarter_turns_counter_clockwise, WorldSpace};

enum Pieces {
    Player,
    Pawn,
}

trait Piece {
    fn get_position(&self) -> Point2D<i32, WorldSpace>;
    fn get_move_squares() -> Vec<Vector2D<i32, WorldSpace>>;
    fn get_capture_squares() -> Vec<Vector2D<i32, WorldSpace>>;
    fn get_move_directions() -> Vec<Vector2D<i32, WorldSpace>>;
    fn get_capture_directions() -> Vec<Vector2D<i32, WorldSpace>>;
}

#[derive(PartialEq, Debug, Copy, Clone)]
struct Pawn {
    position: Point2D<i32, WorldSpace>,
}

impl Piece for Pawn {
    fn get_position(&self) -> Point2D<i32, WorldSpace> {
        self.position
    }

    fn get_move_squares() -> Vec<Vector2D<i32, WorldSpace>> {
        get_4_rotations(Vector2D::<i32, WorldSpace>::new(1, 0))
    }

    fn get_capture_squares() -> Vec<Vector2D<i32, WorldSpace>> {
        get_4_rotations(Vector2D::<i32, WorldSpace>::new(1, 1))
    }

    fn get_move_directions() -> Vec<Vector2D<i32, WorldSpace>> {
        vec![]
    }

    fn get_capture_directions() -> Vec<Vector2D<i32, WorldSpace>> {
        vec![]
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashSet;
    use pretty_assertions::{assert_eq, assert_ne};

    use super::*;

    #[test]
    fn test_pawn_moveset() {
        let pawn_moveset = HashSet::from_iter(Pawn::get_move_squares());
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
