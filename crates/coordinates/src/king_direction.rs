// use crate::utility::*;
use crate::IntCoord;
use crate::orthogonal_direction::OrthogonalDirection;
use angles::*;

#[derive(Clone, Hash, Eq, PartialEq, Debug, Copy)]
pub struct KingDirection {
    step: IntCoord,
}

impl KingDirection {
    pub fn new(dir: IntCoord) -> Self {
        assert!(dir.is_king_step());
        KingDirection { step: dir }
    }
    pub fn step(&self) -> IntCoord {
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

impl From<IntCoord> for KingDirection {
    fn from(value: IntCoord) -> Self {
        KingDirection::new(value)
    }
}

impl From<KingDirection> for IntCoord {
    fn from(value: KingDirection) -> Self {
        value.step
    }
}
