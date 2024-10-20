// use crate::utility::*;
use crate::quarter_turn_rotatable::QuarterTurnRotatable;
use crate::ortho_angle::{OrthoAngle, OrthoAngleOperations};

// AKA NormalizedQuarterTurnsCcw
#[derive(Hash, Default, Debug, Copy, Clone, Eq, PartialEq, derive_more::AddAssign)]
pub struct NormalizedOrthoAngle(i32);
impl OrthoAngleOperations for NormalizedOrthoAngle
where
    Self: Sized,
{
    fn from_quarter_turns_ccw(quarter_turns_ccw: i32) -> Self {
        NormalizedOrthoAngle(quarter_turns_ccw.rem_euclid(4))
    }
    fn quarter_turns_ccw(&self) -> i32 {
        self.0
    }
}

impl std::ops::Add for NormalizedOrthoAngle {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        Self::from_quarter_turns_ccw(self.quarter_turns_ccw().add(rhs.quarter_turns_ccw()))
    }
}
impl std::ops::Sub for NormalizedOrthoAngle {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        Self::from_quarter_turns_ccw(self.quarter_turns_ccw().sub(rhs.quarter_turns_ccw()))
    }
}

impl QuarterTurnRotatable for NormalizedOrthoAngle {
    fn quarter_rotated_ccw(&self, quarter_turns_ccw: OrthoAngle) -> Self {
        (quarter_turns_ccw + OrthoAngle::from(*self)).normalized()
    }
}

// impl From<ortho_angle::OrthoAngle> for NormalizedOrthoAngle {
//     fn from(value: ortho_angle::OrthoAngle) -> Self {
//         value.normalized()
//     }
// }
