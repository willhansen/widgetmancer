// use crate::utility::*;
use crate::ortho_angle;

// AKA NormalizedQuarterTurnsCcw
#[derive(Hash, Default, Debug, Copy, Clone, Eq, PartialEq, derive_more::AddAssign)]
pub struct NormalizedOrthoAngle(i32);
impl ortho_angle::Operations for NormalizedOrthoAngle
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

impl From<ortho_angle::OrthoAngle> for NormalizedOrthoAngle {
    fn from(value: ortho_angle::OrthoAngle) -> Self {
        value.normalized()
    }
}
