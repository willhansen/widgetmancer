// use crate::utility::*;

use crate::coordinate;
use angles::NormalizedOrthoAngle;
use angles::{OrthoAngle, OrthoAngleOperations};
use angles::FAngle;
use angles::QuarterTurnRotatable;



#[derive(Hash, Default, Debug, Copy, Clone, Eq, PartialEq)]
pub struct OrthogonalDirection(NormalizedOrthoAngle);

impl OrthogonalDirection {
    pub fn from_angle_hint(hint: FAngle) -> Self {
        let hint = hint.standardized_starting_at_zero();
        let quarter_turns_ccw = ((hint.radians + std::f32::consts::FRAC_PI_4) / std::f32::consts::FRAC_PI_2) as i32;
        Self(NormalizedOrthoAngle::from_quarter_turns(
            quarter_turns_ccw,
        ))
    }
    pub fn from_degrees_hint(deg: f32) -> Self {
        Self::from_angle_hint(FAngle::from_degrees(deg))
    }
}

pub const RIGHT: OrthogonalDirection = OrthogonalDirection(NormalizedOrthoAngle::from_quarter_turns_ccw(0));
pub const UP: OrthogonalDirection = OrthogonalDirection(NormalizedOrthoAngle::from_quarter_turns_ccw(1));
pub const LEFT: OrthogonalDirection = OrthogonalDirection(NormalizedOrthoAngle::from_quarter_turns_ccw(2));
pub const DOWN: OrthogonalDirection = OrthogonalDirection(NormalizedOrthoAngle::from_quarter_turns_ccw(3));

// Behavior of negative is the main difference between orthogonaldirection and normalizedorthoangle
impl std::ops::Neg for OrthogonalDirection {
    type Output = Self;

    fn neg(self) -> Self::Output {
        self.reversed()
    }
}

impl std::fmt::Display for OrthogonalDirection {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.dir_name())
    }
}

// TODO: need this?
// impl<T: Operations> From<T> for OrthogonalDirection {
//     fn from(value: T) -> Self {
//         value.dir()
//     }
// }

impl From<OrthogonalDirection> for NormalizedOrthoAngle {
    fn from(value: OrthogonalDirection) -> Self {
        value.angle()
    }
}
impl QuarterTurnRotatable for OrthogonalDirection {
    fn quarter_rotated_ccw(&self, quarter_turns_ccw: OrthoAngle) -> Self {
        self.0.quarter_rotated_ccw(quarter_turns_ccw).into()
    }
}
pub fn sorted_left_to_right(faces: [OrthogonalDirection; 2]) -> [OrthogonalDirection; 2] {
    assert_ne!(faces[0], faces[1]);
    assert_ne!(faces[0], -faces[1]);
    if faces[0] == faces[1].quarter_rotated_ccw(NormalizedOrthoAngle::new_from_quarter_turns(1)) {
        faces
    } else {
        [faces[1], faces[0]]
    }
}
