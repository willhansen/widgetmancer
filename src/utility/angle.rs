use std::ops::{Add, Div, Mul, Neg, Sub};

use crate::utility::*;

#[derive(Hash, Default, Debug, Copy, Clone, Eq, PartialEq, derive_more::AddAssign)]
pub struct OrthoAngle {
    quarter_turns: i32,
}

impl OrthoAngle {
    const X_AXIS: Self = Self::new(0);
    pub fn new(quarter_turns: i32) -> Self {
        OrthoAngle {
            quarter_turns: quarter_turns.rem_euclid(4),
        }
    }
    pub fn right() -> Self {
        Self::X_AXIS
    }
    pub fn quarter_turns(&self) -> i32 {
        self.quarter_turns
    }
    pub fn xy<T: num::Signed>(&self) -> (T, T) {
        match self.quarter_turns().rem_euclid(4) {
            0 => (T::one(), T::zero()),
            1 => (T::zero(), T::one()),
            2 => (-T::one(), T::zero()),
            3 => (T::zero(), -T::one()),
        }
    }
    pub fn cos<T: num::Signed>(&self) -> T {
        match self.quarter_turns.rem_euclid(4) {
            0 => T::one(),
            1 | 3 => T::zero(),
            2 => -T::one(),
        }
    }
    pub fn sin<T: num::Signed>(&self) -> T {
        match self.quarter_turns.rem_euclid(4) {
            0 | 2 => T::zero(),
            1 => T::one(),
            3 => -T::one(),
        }
    }
    pub fn dot<T: num::Signed>(&self, other: impl Into<Self>) -> T {
        (*self - other.into()).cos()
    }
    pub fn is_parallel(&self, other: impl Into<Self>) -> bool {
        self.dot::<i32>(other) != 0
    }
    pub fn is_horizontal(&self) -> bool {
        self.cos::<i32>() != 0
    }
    pub fn is_vertical(&self) -> bool {
        self.sin::<i32>() != 0
    }

    #[deprecated(note = "use step instead")]
    pub fn to_orthogonal_direction(&self) -> WorldStep {
        self.step()
    }
    pub fn step<T: SignedCoordinate>(&self) -> T {
        self.xy().into()
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

    pub fn rotate_angle(&self, angle: FAngle) -> FAngle {
        standardize_angle(euclid::Angle::<f32>::degrees(
            angle.to_degrees() + 90.0 * (self.quarter_turns() as f32),
        ))
    }
    pub fn to_float_angle(&self) -> FAngle {
        self.rotate_angle(FAngle::degrees(0.0))
    }
    pub fn rotate_vector<PointType: SignedCoordinate>(&self, v: PointType) -> PointType {
        v.quarter_rotated_ccw(self.quarter_turns)
    }
    pub fn turned_left(&self) -> Self {
        self.quarter_rotated_ccw(1)
    }
    pub fn turned_right(&self) -> Self {
        self.quarter_rotated_ccw(3)
    }
    pub fn turned_back(&self) -> Self {
        self.quarter_rotated_ccw(2)
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
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_dot() {
        [
            (0, 0, 1),
            (0, 1, 0),
            (0, 2, -1),
            (0, 3, 0),
            (1, 0, 0),
            (2, 1, 0),
            (3, 3, 1),
            (4, 2, -1),
        ]
        .into_iter()
        .for_each(|(a, b, c)| {
            assert_eq!(OrthoAngle::new(a).dot::<i32>(OrthoAngle::new(b)), c);
        });
    }
}
