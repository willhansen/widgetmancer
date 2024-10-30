// use crate::utility::*;
use crate::int_coordinate::IntCoordinate;
use crate::orthogonal_direction::OrthogonalDirection;
use angles::*;

#[derive(Clone, Hash, Eq, PartialEq, Debug, Copy)]
pub struct KingDirection {
    step: IntCoordinate,
}

impl KingDirection {
    pub fn new(dir: IntCoordinate) -> Self {
        assert!(dir.is_king_step());
        KingDirection { step: dir }
    }
    pub fn step(&self) -> IntCoordinate {
        self.step
    }
}

impl From<OrthogonalDirection> for KingDirection {
    fn from(value: OrthogonalDirection) -> Self {
        KingDirection::new(value.to_step())
    }
}

// TODO: generate with macro
impl QuarterTurnRotatable for KingDirection {
    fn quarter_rotated_ccw(&self, quarter_turns_ccw: impl Into<NormalizedOrthoAngle>) -> Self {
        self.step().quarter_rotated_ccw(quarter_turns_ccw).into()
    }
}

impl From<IntCoordinate> for KingDirection {
    fn from(value: IntCoordinate) -> Self {
        KingDirection::new(value)
    }
}

