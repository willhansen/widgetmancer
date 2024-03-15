use std::ops::{Add, Div, Mul, Neg, Sub};

use euclid::Angle;

use crate::utility::*;

#[derive(
    Hash, Default, Debug, Copy, Clone, Eq, PartialEq, getset::CopyGetters, derive_more::AddAssign,
)]
#[get_copy = "pub"]
pub struct OrthoAngle {
    pub(crate) quarter_turns: i32,
}

impl OrthoAngle {
    pub fn new(quarter_turns: i32) -> Self {
        OrthoAngle {
            quarter_turns: quarter_turns.rem_euclid(4),
        }
    }
    pub fn to_orthogonal_direction(&self) -> WorldStep {
        self.step()
    }
    pub fn step<T: SignedCoordinate>(&self) -> T {
        T::right().quarter_rotated_ccw(*self)
    }
    pub fn all_4() -> impl Iterator<Item = Self> + Clone {
        (0..4).map(|x| x.into())
    }
    pub fn from_orthogonal_vector<T: Coordinate>(dir: T) -> Self {
        assert!(dir.is_orthogonal());
        OrthoAngle::new(if dir.x() == T::DataType::zero() {
            if dir.y() > T::DataType::zero() {
                1
            } else {
                3
            }
        } else {
            if dir.x() > T::DataType::zero() {
                0
            } else {
                2
            }
        })
    }
    pub fn quarter_turns_from_x_axis<P: IntCoordinate>(end: P) -> Self {
        Self::from_start_and_end_directions(P::right(), end)
    }

    pub fn from_start_and_end_directions<P: IntCoordinate>(start: P, end: P) -> Self {
        assert!(start.is_king_step());
        assert!(end.is_king_step());
        // needs to be quarter turn, no eighths
        assert_eq!(start.is_diagonal(), end.is_diagonal());

        let d_angle = start.to_f32().angle_to(end.to_f32());
        let quarter_turns = (d_angle.to_degrees() / 90.0).round() as i32;
        Self::new(quarter_turns)
    }

    pub fn rotate_angle(&self, angle: Angle<f32>) -> Angle<f32> {
        standardize_angle(Angle::<f32>::degrees(
            angle.to_degrees() + 90.0 * (self.quarter_turns() as f32),
        ))
    }
    pub fn rotate_vector<PointType: SignedCoordinate>(&self, v: PointType) -> PointType {
        v.quarter_rotated_ccw(self.quarter_turns)
    }
}

impl Neg for OrthoAngle {
    type Output = Self;

    fn neg(self) -> Self::Output {
        OrthoAngle::new(-self.quarter_turns)
    }
}

impl Add for OrthoAngle {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        Self::new(self.quarter_turns() + rhs.quarter_turns())
    }
}

impl Sub for OrthoAngle {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        Self::new(self.quarter_turns() - rhs.quarter_turns())
    }
}

impl From<i32> for OrthoAngle {
    fn from(value: i32) -> Self {
        Self::new(value)
    }
}

impl QuarterTurnRotatable for OrthoAngle {
    fn quarter_rotated_ccw(&self, quarter_turns_ccw: impl Into<OrthoAngle> + Copy) -> Self {
        *self + quarter_turns_ccw.into()
    }
}
