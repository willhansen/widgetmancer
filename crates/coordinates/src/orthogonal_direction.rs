use angles::*;
use crate::*;



#[derive(Hash, Default, Debug, Copy, Clone, Eq, PartialEq)]
pub struct OrthogonalDirection(NormalizedOrthoAngle);

impl OrthogonalDirection {
    pub fn snap_from_angle(hint: FAngle) -> Self {
        let hint = hint.standardized_starting_at_zero();
        // TODO: use an angle method for this
        let quarter_turns_ccw = ((hint.radians() + std::f32::consts::FRAC_PI_4) / std::f32::consts::FRAC_PI_2) as i32;
        Self(NormalizedOrthoAngle::from_quarter_turns_ccw(
            quarter_turns_ccw,
        ))
    }
    pub fn snap_from_degrees(deg: f32) -> Self {
        Self::snap_from_angle(FAngle::from_degrees(deg))
    }
    pub fn to_step(&self) -> ICoord {
        self.xy_array().into()
    }
}

impl DirectionOperations for OrthogonalDirection {
    fn x<T: num::Signed>(&self) -> T {
        self.0.x()
    }

    fn y<T: num::Signed>(&self) -> T {
        self.0.y()
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

// impl std::fmt::Display for OrthogonalDirection {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         write!(f, "{}", self.dir_name())
//     }
// }

// TODO: need this?
// impl<T: Operations> From<T> for OrthogonalDirection {
//     fn from(value: T) -> Self {
//         value.dir()
//     }
// }

impl Into<NormalizedOrthoAngle> for OrthogonalDirection{
    fn into(self) -> NormalizedOrthoAngle {
        self.0
    }
}
impl Into<ICoord> for OrthogonalDirection{
    fn into(self) -> ICoord {
        ICoord::new(self.x(), self.y())
    }
}
impl QuarterTurnRotatable for OrthogonalDirection {
    fn quarter_rotated_ccw(&self, quarter_turns_ccw: OrthoAngle) -> Self {
        Self(self.0.quarter_rotated_ccw(quarter_turns_ccw))
    }
}
pub fn sorted_left_to_right(faces: [OrthogonalDirection; 2]) -> [OrthogonalDirection; 2] {
    assert_ne!(faces[0], faces[1]);
    assert_ne!(faces[0], -faces[1]);
    if faces[0] == faces[1].ez_quarter_rotated_ccw(1) {
        faces
    } else {
        [faces[1], faces[0]]
    }
}
