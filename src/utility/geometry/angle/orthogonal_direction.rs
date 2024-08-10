use crate::utility::*;

#[derive(Hash, Default, Debug, Copy, Clone, Eq, PartialEq)]
pub struct OrthogonalDirection(NormalizedOrthoAngle);

impl OrthogonalDirection {
    pub fn from_angle_hint(hint: FAngle) -> Self {
        let hint = standardize_angle_with_zero_min(hint);
        let quarter_turns_ccw = ((hint.radians + std::f32::consts::FRAC_PI_4) / std::f32::consts::FRAC_PI_2) as i32;
        Self(NormalizedOrthoAngle::new_from_quarter_turns(
            quarter_turns_ccw,
        ))
    }
    pub fn from_degrees_hint(deg: f32) -> Self {
        Self::from_angle_hint(Angle::degrees(deg))
    }
}

pub const RIGHT: OrthogonalDirection = OrthogonalDirection(NormalizedOrthoAngle::new_from_quarter_turns(0));
pub const UP: OrthogonalDirection = OrthogonalDirection(NormalizedOrthoAngle::new_from_quarter_turns(1));
pub const LEFT: OrthogonalDirection = OrthogonalDirection(NormalizedOrthoAngle::new_from_quarter_turns(2));
pub const DOWN: OrthogonalDirection = OrthogonalDirection(NormalizedOrthoAngle::new_from_quarter_turns(3));

// Behavior of negative is the main difference between orthogonaldirection and normalizedorthoangle
impl Neg for OrthogonalDirection {
    type Output = Self;

    fn neg(self) -> Self::Output {
        self.reversed()
    }
}

impl Display for OrthogonalDirection {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.dir_name())
    }
}

impl<T: ortho_angle::Operations> From<T> for OrthogonalDirection {
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
    fn quarter_rotated_ccw(&self, quarter_turns_ccw: impl Into<NormalizedOrthoAngle>) -> Self {
        self.0.quarter_rotated_ccw(quarter_turns_ccw).into()
    }
}
