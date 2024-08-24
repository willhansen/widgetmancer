use crate::utility::*;

#[derive(Hash, Default, Debug, Copy, Clone, Eq, PartialEq, derive_more::AddAssign)]
pub struct NormalizedOrthoAngle(i32);
impl ortho_angle::Operations for NormalizedOrthoAngle
where
    Self: Sized,
{
    fn new_from_quarter_turns(quarter_turns_ccw: i32) -> Self {
        NormalizedOrthoAngle(quarter_turns_ccw.rem_euclid(4))
    }
    fn quarter_turns(&self) -> i32 {
        self.0
    }
}

impl From<OrthoAngle> for NormalizedOrthoAngle {
    fn from(value: OrthoAngle) -> Self {
        value.normalized()
    }
}
