automod::dir!(pub "src/utility/geometry/angle");

use std::{
    f32::consts::{FRAC_PI_2, FRAC_PI_4},
    ops::{Add, Div, Mul, Neg, Sub},
};

use crate::utility::*;

pub use ortho_angle::OrthoAngle;
pub use normalized_ortho_angle::NormalizedOrthoAngle;

pub enum AngleVariety {
    Orthogonal,
    Diagonal,
    OrthoDiagonal,
    FloatingPoint,
}
// TODO: normalized angle enum


// TODO: Add a level of relativity here to differentiate it from an absolute position.
pub type QuarterTurns = OrthoAngle;

#[derive(Hash, Default, Debug, Copy, Clone, Eq, PartialEq)]
pub struct OrthogonalDirection(NormalizedOrthoAngle);

impl OrthogonalDirection {
    pub fn from_angle_hint(hint: FAngle) -> Self {
        let hint = standardize_angle_with_zero_min(hint);
        let quarter_turns_ccw = ((hint.radians + FRAC_PI_4) / FRAC_PI_2) as i32;
        Self(NormalizedOrthoAngle::new_from_quarter_turns(
            quarter_turns_ccw,
        ))
    }
    pub fn from_degrees_hint(deg: f32) -> Self {
        Self::from_angle_hint(Angle::degrees(deg))
    }
}

pub const RIGHT: OrthogonalDirection = OrthogonalDirection(NormalizedOrthoAngle(0));
pub const UP: OrthogonalDirection = OrthogonalDirection(NormalizedOrthoAngle(1));
pub const LEFT: OrthogonalDirection = OrthogonalDirection(NormalizedOrthoAngle(2));
pub const DOWN: OrthogonalDirection = OrthogonalDirection(NormalizedOrthoAngle(3));

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
pub trait Direction: QuarterTurnRotatable + Copy + Sized {
    // TODO: diagonals or float angles too?  Template on an `AngleType` enum?
    fn angle(&self) -> NormalizedOrthoAngle;
    fn from_angle(angle: impl ortho_angle::Operations) -> Self;
    fn from_coordinate<T: coordinate::Operations>(dir: T) -> Self {
        Self::try_from_coordinate(dir).unwrap()
    }
    fn try_from_coordinate<T: coordinate::Operations>(coord: T) -> Result<Self, String> {
        Ok(Self::from_angle(
            <NormalizedOrthoAngle as ortho_angle::Operations>::try_from_coordinate(coord)?,
        ))
    }
    fn left(&self) -> Self {
        self.turned_left()
    }
    fn right(&self) -> Self {
        self.turned_right()
    }
    fn reversed(&self) -> Self {
        self.turned_back()
    }
    fn is_parallel(&self, other: impl Direction) -> bool {
        self.dot::<i32>(other) != 0
    }
    fn is_horizontal(&self) -> bool {
        self.angle().cos::<i32>() != 0
    }
    fn is_vertical(&self) -> bool {
        self.angle().sin::<i32>() != 0
    }
    fn is_positive(&self) -> bool {
        let a = self.angle();
        a.cos::<i32>() > 0 || a.sin::<i32>() > 0
    }
    fn dot<T: num::Signed>(&self, other: impl Direction) -> T {
        (self.angle() - other.angle()).cos()
    }
    fn xy<T: num::Signed>(&self) -> (T, T) {
        // TODO: use enum rather than matching an i32?
        match self.angle().quarter_turns() {
            0 => (T::one(), T::zero()),
            1 => (T::zero(), T::one()),
            2 => (-T::one(), T::zero()),
            3 => (T::zero(), -T::one()),
            x => panic!("Invalid OrthogonalDirection: {}", x),
        }
    }
    fn dir_name(&self) -> &'static str {
        match self.angle().quarter_turns() {
            0 => "Right",
            1 => "Up",
            2 => "Left",
            3 => "Down",
            x => panic!("Invalid OrthogonalDirection: {}", x),
        }
    }
    // TODO: find a way to automatically just take whatever type is convenient
    fn to_step<T: Signedcoordinate::Operations>(&self) -> T {
        // let (x, y) = self.xy();
        // T::new(x, y)
        self.xy().into()
    }
}

impl<T> Direction for T
where
    T: ortho_angle::Operations,
{
    fn angle(&self) -> NormalizedOrthoAngle {
        self.normalized()
    }

    // TODO: use enum that covers OrthoAngle and Angle<f32>
    fn from_angle(angle: impl ortho_angle::Operations) -> Self {
        todo!()
    }

    fn try_from_coordinate<P: coordinate::Operations>(coord: P) -> Result<Self, String> {
        if coord.is_orthogonal() {}
        todo!()
    }
}

impl Direction for OrthogonalDirection {
    fn angle(&self) -> NormalizedOrthoAngle {
        self.0
    }

    fn from_angle(angle: impl ortho_angle::Operations) -> Self {
        Self(angle.normalized())
    }
}

// TODO: make member of more general angle trait
pub fn fangle_dot(a: FAngle, b: FAngle) -> f32 {
    WorldPoint::unit_vector_from_angle(a).dot(WorldPoint::unit_vector_from_angle(b))
}

pub fn standardize_angle_with_zero_mid(angle: Angle<f32>) -> Angle<f32> {
    let mut radians = angle.radians;
    if radians > -PI && radians <= PI {
        angle
    } else {
        radians = radians.rem_euclid(TAU);
        if radians > PI {
            radians -= TAU;
        }
        Angle::radians(radians)
    }
}
// TODO: make difference in names clearer
pub fn standardize_angle_with_zero_min(angle: Angle<f32>) -> Angle<f32> {
    let with_zero_mid = standardize_angle_with_zero_mid(angle);
    if with_zero_mid.radians < 0.0 {
        angle + Angle::two_pi()
    } else {
        angle
    }
}

pub fn abs_angle_distance(a: Angle<f32>, b: Angle<f32>) -> Angle<f32> {
    Angle::radians(
        standardize_angle_with_zero_mid(a)
            .angle_to(standardize_angle_with_zero_mid(b))
            .radians
            .abs(),
    )
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_init() {
        [
            (-3, UP),
            (-2, LEFT),
            (-1, DOWN),
            (0, RIGHT),
            (1, UP),
            (2, LEFT),
            (3, DOWN),
            (4, RIGHT),
            (5, UP),
            (6, LEFT),
        ]
        .into_iter()
        .for_each(|(quarter_turns, dir)| {
            assert_eq!(
                NormalizedOrthoAngle::new_from_quarter_turns(quarter_turns).dir(),
                dir
            );
        });
    }
    #[test]
    fn test_negative() {
        assert_eq!(-LEFT, RIGHT);
        assert_eq!(-RIGHT, LEFT);
        assert_eq!(-UP, DOWN);
        assert_eq!(-DOWN, UP);
    }

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
                NormalizedOrthoAngle::new_from_quarter_turns(a)
                    .dot::<i32>(NormalizedOrthoAngle::new_from_quarter_turns(b)),
                c
            );
        });
    }
    #[test]
    fn test_orthogonal_direction_from_angle_hint() {
        let f = OrthogonalDirection::from_degrees_hint;
        assert_eq!(f(0.0), RIGHT, "zero case");
        assert_eq!(f(90.0), UP, "simple case");
        assert_eq!(f(180.0), LEFT, "simple case");
        assert_eq!(f(-90.0), DOWN, "simple case");
        assert_eq!(f(46.0), UP, "rounding");
        assert_eq!(f(44.0), RIGHT, "rounding");
        assert_eq!(f(-43.0), RIGHT, "rounding");
        assert_eq!(f(-43.0 + 360.0), RIGHT, "rounding");
    }
}
