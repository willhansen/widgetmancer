use std::ops::{Add, Div, Mul, Neg, Sub};

use crate::utility::*;

#[derive(Hash, Default, Debug, Copy, Clone, Eq, PartialEq, derive_more::AddAssign)]
pub struct NormalizedOrthoAngle(i32);
impl OrthoAngle for NormalizedOrthoAngle
where
    Self: Sized,
{
    fn new(quarter_turns: i32) -> Self {
        NormalizedOrthoAngle(quarter_turns.rem_euclid(4))
    }
    fn quarter_turns(&self) -> i32 {
        self.0
    }
}
pub type OrthogonalDirection = NormalizedOrthoAngle;

pub const UP: NormalizedOrthoAngle = NormalizedOrthoAngle(0);
pub const DOWN: NormalizedOrthoAngle = NormalizedOrthoAngle(1);
pub const RIGHT: NormalizedOrthoAngle = NormalizedOrthoAngle(2);
pub const LEFT: NormalizedOrthoAngle = NormalizedOrthoAngle(3);

#[derive(Hash, Default, Debug, Copy, Clone, Eq, PartialEq, derive_more::AddAssign)]
pub struct UnNormalizedOrthoAngle(i32);
impl OrthoAngle for UnNormalizedOrthoAngle
where
    Self: Sized,
{
    fn new(quarter_turns: i32) -> Self {
        UnNormalizedOrthoAngle(quarter_turns)
    }
    fn quarter_turns(&self) -> i32 {
        self.0
    }
}

pub trait OrthoAngle:
    Sized
    + Sub<NormalizedOrthoAngle, Output = Self>
    + Add<NormalizedOrthoAngle, Output = Self>
    + Sub
    + Add
    + QuarterTurnRotatable
    + Copy
{
    fn new(quarter_turns: i32) -> Self;
    fn quarter_turns(&self) -> i32;
    fn normalized(&self) -> NormalizedOrthoAngle {
        NormalizedOrthoAngle::new(self.quarter_turns())
    }
    fn x_axis() -> Self {
        Self::new(0)
    }
    fn right() -> Self {
        Self::x_axis()
    }
    fn up() -> Self {
        Self::x_axis().quarter_rotated_ccw(1)
    }
    fn xy<T: num::Signed>(&self) -> (T, T) {
        // TODO: use enum rather than matching an i32?
        match self.normalized().quarter_turns() {
            0 => (T::one(), T::zero()),
            1 => (T::zero(), T::one()),
            2 => (-T::one(), T::zero()),
            3 => (T::zero(), -T::one()),
            x => panic!("Invalid OrthogonalDirection: {}", x)
        }
    }
    fn dir_name(&self) -> &'static str {
        match self.normalized().quarter_turns() {
            0 => "Right",
            1 => "Up",
            2 => "Left",
            3 => "Down",
            x => panic!("Invalid OrthogonalDirection: {}", x)
        }
    }
    fn cos<T: num::Signed>(&self) -> T {
        match self.normalized().0 {
            0 => T::one(),
            1 | 3 => T::zero(),
            2 => -T::one(),
            x => panic!("Invalid OrthogonalDirection: {}", x)
        }
    }
    fn sin<T: num::Signed>(&self) -> T {
        match self.normalized().0 {
            0 | 2 => T::zero(),
            1 => T::one(),
            3 => -T::one(),
            x => panic!("Invalid OrthogonalDirection: {}", x)
        }
    }
    fn dot<T: num::Signed>(&self, other: impl OrthoAngle) -> T {
        (*self - other.normalized()).cos()
    }
    fn is_parallel(&self, other: impl OrthoAngle) -> bool {
        self.dot::<i32>(other) != 0
    }
    fn is_horizontal(&self) -> bool {
        self.cos::<i32>() != 0
    }
    fn is_vertical(&self) -> bool {
        self.sin::<i32>() != 0
    }
    fn is_positive(&self) -> bool {
        match self.normalized().0 {
            0 | 1 => true,
            2 | 3 => false,
            x => panic!("Invalid OrthogonalDirection: {}", x)
        }
    }

    #[deprecated(note = "use to_step instead")]
    fn to_orthogonal_direction(&self) -> WorldStep {
        self.to_step()
    }
    // TODO: find a way to automatically just take whatever type is convenient
    fn to_step<T: SignedCoordinate>(&self) -> T {
        self.xy().into()
    }
    fn from_orthogonal_vector<T: Coordinate>(dir: T) -> Self {
        assert!(dir.is_orthogonal());
        Self::new(if dir.x() == T::DataType::zero() {
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
    fn quarter_turns_from_x_axis<P: IntCoordinate>(end: P) -> Self {
        Self::from_start_and_end_directions(P::right(), end)
    }

    fn from_start_and_end_directions<P: IntCoordinate>(start: P, end: P) -> Self {
        assert!(start.is_king_step());
        assert!(end.is_king_step());
        // needs to be quarter turn, no eighths
        assert_eq!(start.is_diagonal(), end.is_diagonal());

        let d_angle = start.to_f32().angle_to(end.to_f32());
        let quarter_turns = (d_angle.to_degrees() / 90.0).round() as i32;
        Self::new(quarter_turns)
    }

    fn rotate_angle(&self, angle: FAngle) -> FAngle {
        standardize_angle(euclid::Angle::<f32>::degrees(
            angle.to_degrees() + 90.0 * (self.quarter_turns() as f32),
        ))
    }
    fn to_float_angle(&self) -> FAngle {
        self.rotate_angle(FAngle::degrees(0.0))
    }
    fn rotate_vector<PointType: SignedCoordinate>(&self, v: PointType) -> PointType {
        v.quarter_rotated_ccw(self.quarter_turns())
    }
    fn turned_left(&self) -> Self {
        self.quarter_rotated_ccw(1)
    }
    fn turned_right(&self) -> Self {
        self.quarter_rotated_ccw(3)
    }
    fn turned_back(&self) -> Self {
        self.quarter_rotated_ccw(2)
    }
}

macro_rules! impl_ops_for_OrthoAngles {
    ($Type:ty) => {
        impl Neg for $Type {
            type Output = Self;

            fn neg(self) -> Self::Output {
                Self::new(-self.quarter_turns())
            }
        }

        impl<T: OrthoAngle> Add<T> for $Type {
            type Output = Self;

            fn add(self, rhs: T) -> Self::Output {
                Self::new(self.quarter_turns() + rhs.quarter_turns())
            }
        }

        impl<T: OrthoAngle> Sub<T> for $Type {
            type Output = Self;

            fn sub(self, rhs: T) -> Self::Output {
                Self::new(self.quarter_turns() - rhs.quarter_turns())
            }
        }

        impl From<i32> for $Type {
            fn from(value: i32) -> Self {
                Self::new(value)
            }
        }
        impl QuarterTurnRotatable for $Type {
            fn quarter_rotated_ccw(
                &self,
                quarter_turns_ccw: impl Into<NormalizedOrthoAngle>,
            ) -> Self {
                *self + quarter_turns_ccw.into()
            }
        }

        impl Display for $Type {
            fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
                // TODO: tidy
                write!(
                    f,
                    "(x:{}, y:{}) {} {} ",
                    self.xy::<i32>().0,
                    self.xy::<i32>().1,
                    self.dir_name(),
                    Glyph::extract_arrow_from_arrow_string(self.to_step(), FACE_ARROWS)
                )
            }
        }
        impl Into<FAngle> for $Type {
            fn into(self) -> FAngle {
                self.to_float_angle()
            }
        }
    };
}

impl_ops_for_OrthoAngles!(NormalizedOrthoAngle);
impl_ops_for_OrthoAngles!(UnNormalizedOrthoAngle);

// impl<T: Coordinate> TryFrom<T> for OrthoAngle {
//     type Error = ();
//     fn try_from(value: T) -> Result<T, Self::Error> {
//         Ok(Self::from_orthogonal_vector(value))
//     }
// }

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
            assert_eq!(
                NormalizedOrthoAngle::new(a).dot::<i32>(NormalizedOrthoAngle::new(b)),
                c
            );
        });
    }
}
