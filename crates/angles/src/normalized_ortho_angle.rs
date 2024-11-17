// use crate::utility::*;
use crate::OrthoAngle;
use crate::OrthoAngleOperations;
use static_assertions;

// AKA NormalizedQuarterTurnsCcw
#[derive(Hash, Default, Debug, Copy, Clone, Eq, PartialEq, derive_more::AddAssign)]
pub struct NormalizedOrthoAngle(i32);

impl NormalizedOrthoAngle {
    pub const fn from_quarter_turns_ccw(quarter_turns_ccw: i32) -> Self {
        NormalizedOrthoAngle(quarter_turns_ccw.rem_euclid(4))
    }
    pub fn cos<T: num::Signed>(&self) -> T {
        match self.0 {
            0 => T::one(),
            1 | 3 => T::zero(),
            2 => -T::one(),
            x => panic!("Invalid angle: {}", x),
        }
    }
    pub fn sin<T: num::Signed>(&self) -> T {
        match self.0 {
            0 | 2 => T::zero(),
            1 => T::one(),
            3 => -T::one(),
            x => panic!("Invalid angle: {}", x),
        }
    }
}

static_assertions::assert_impl_all!(NormalizedOrthoAngle: OrthoAngleOperations);

impl OrthoAngleOperations for NormalizedOrthoAngle
where
    Self: Sized,
{
    fn quarter_turns_ccw(&self) -> i32 {
        self.0
    }
    fn normalized(&self) -> NormalizedOrthoAngle {
        *self
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


// impl From<ortho_angle::OrthoAngle> for NormalizedOrthoAngle {
//     fn from(value: ortho_angle::OrthoAngle) -> Self {
//         value.normalized()
//     }
// }
