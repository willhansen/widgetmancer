// use crate::utility::*;

use angles::normalized_ortho_angle::NormalizedOrthoAngle;
use angles::ortho_angle::{OrthoAngle, OrthoAngleOperations};
use angles::float_angle::FAngle;
use angles::quarter_turn_rotatable::QuarterTurnRotatable;



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

impl<T: OrthoAngleOperations> From<T> for OrthogonalDirection {
    fn from(value: T) -> Self {
        value.dir()
    }
}
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
