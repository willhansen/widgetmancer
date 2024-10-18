use std::{
    f32::consts::{FRAC_PI_2, FRAC_PI_4, PI, TAU},
};
use derive_more;
use misc_utilities;
use ordered_float::OrderedFloat;

pub const FULL_TURN: FAngle = FAngle(TAU);
pub const HALF_TURN: FAngle = FAngle(PI);
pub const QUARTER_TURN: FAngle = FAngle(FRAC_PI_2);

#[derive(Default, Debug, Copy, Clone, PartialEq, PartialOrd, derive_more::Add, derive_more::Sub)]
pub struct FAngle(f32);

impl FAngle {
    pub fn one_turn() -> Self {
        Self::from_turns(1.0)
    }
    pub fn rad(&self) -> f32 {
        self.0
    }
    pub fn radians(&self) -> f32 {
        self.rad()
    }
    pub fn deg(&self) -> f32 {
        self.0.to_degrees()
    }
    pub fn degrees(&self) -> f32 {
        self.deg()
    }
    pub fn turns(&self) -> f32 {
        self.rad()/TAU
    }
    pub fn from_deg(x: f32) -> Self {
        Self(x.to_radians())
    }
    pub fn from_rad(x: f32) -> Self {
        Self(x)
    }
    pub fn from_turns(x: f32) -> Self {
        Self::from_rad(x * TAU)
    }
    pub fn from_quarter_turns(x: f32) -> Self {
        Self::from_turns(x / 4.0)
    }
    pub fn sin(&self) -> f32 {
        self.rad().sin()
    }
    pub fn cos(&self) -> f32 {
        self.rad().cos()
    }
    // TODO: find a neater way to alias functions
    pub fn x(&self) -> f32 {
        self.cos()
    }
    // TODO: find a neater way to alias functions
    pub fn y(&self) -> f32 {
        self.sin()
    }
    pub fn xy(&self) -> [f32;2] {
        [self.x(), self.y()]
    }
    pub fn tan(&self) -> f32 {
        self.rad().tan()
    }
    pub fn standardized_centered_at_zero(&self) -> Self {
        (*self + HALF_TURN).standardized_starting_at_zero() - HALF_TURN
    }
    pub fn standardized_starting_at_zero(&self) -> Self {
        Self::from_rad(self.rad().rem_euclid(TAU))
    }
    pub fn diagonals() -> [Self;4] {
        misc_utilities::general_utility::range_array::<4>().map(|i| 1.0/8.0 + 1.0/4.0 * i as f32).map(|turns| FAngle::from_turns(turns))
    }
    pub fn orthogonals() -> [Self;4] {
        core::array::from_fn(|i| deg(90.0 * i as f32))
    }
    pub fn snap_to_diagonal(&self) -> Self {
            Self::diagonals().into_iter()
            .min_by_key(|&snap_angle| OrderedFloat(self.abs_smallest_angle_to(snap_angle).rad()))
            .unwrap()
    }
    pub fn dot(&self, other: Self) -> f32 {
        self.x() * other.x() + self.y() * other.y()
    }
    pub fn smallest_angle_to(&self, other: Self) -> Self {
        let a = self.standardized_starting_at_zero();
        let b = other.standardized_starting_at_zero();
        let a_to_b = b-a;
        let a_to_b2 = (b + Self::one_turn()) - a;
        if a_to_b.abs() < a_to_b2.abs() {
            a_to_b
        } else {
            a_to_b2
        }
    }
    // Includes 0
    pub fn positive_angle_to(&self, other: Self) -> Self {
        let a = self.standardized_starting_at_zero();
        let b = other.standardized_starting_at_zero();
        if a < b {
            b - a
        } else {
            a - b
        }
    }
    pub fn abs(&self) -> Self {
        Self::from_rad(self.rad().abs())
    }
    pub fn abs_smallest_angle_to(&self, other: Self) -> Self {
        self.smallest_angle_to(other).abs()
    }
}
pub fn deg(x: f32) -> FAngle {
    FAngle::from_deg(x)
}


#[allow(non_snake_case)]
#[cfg(test)]
mod tests {
    use super::*;
    use ntest::assert_about_eq;
    use more_asserts as ma;
    #[test]
    fn test_constructors() {
        assert_about_eq!(1.0, FAngle::from_deg(90.0).y());
        assert_about_eq!(1.0, FAngle::from_rad(FRAC_PI_2).y());
        assert_about_eq!(1.0, FAngle::from_turns(0.25).y());
        assert_about_eq!(1.0, FAngle::from_quarter_turns(1.0).y());

        assert_about_eq!(-1.0, FAngle::from_deg(180.0).x());
        assert_about_eq!(-1.0, FAngle::from_rad(PI).x());
        assert_about_eq!(-1.0, FAngle::from_turns(0.5).x());
        assert_about_eq!(-1.0, FAngle::from_quarter_turns(2.0).x());
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
            assert_about_eq!(
                FAngle::from_quarter_turns(a as f32)
                    .dot(FAngle::from_quarter_turns(b as f32)),
                c as f32
            );
        });
    }
    #[test]
    fn test_diagonal() {
        assert_about_eq!(FAngle::diagonals()[0].standardized_starting_at_zero().deg(), 45.0);
        assert_about_eq!(FAngle::diagonals()[1].standardized_starting_at_zero().deg(), (90.0 + 180.0) / 2.0);
        assert_about_eq!(FAngle::diagonals()[2].standardized_starting_at_zero().deg(), (180.0 + 270.0) / 2.0);
        assert_about_eq!(FAngle::diagonals()[3].standardized_starting_at_zero().deg(), (270.0 + 360.0) / 2.0);
    }
    mod angle_to {
        use super::*;
        macro_rules! tests_for_angle_to {
            ($($name:ident: $value:expr,)*) => {
                $(
                    #[test]
                    fn $name() {
                        let (start, end, small_dist, pos_dist) = $value;
                        ma::assert_lt!((small_dist - deg(start).smallest_angle_to(deg(end)).deg()).abs(), 1e-6, "smallest_angle");
                        ma::assert_lt!((pos_dist - deg(start).positive_angle_to(deg(end)).deg()).abs(), 1e-6, "positive_angle");
                    }
                )*
            }
        }
        tests_for_angle_to! {
            // start, end, small_dist, pos_dist
            zero_at_zero: (0.0, 0.0, 0.0, 0.0),
            zero_at_not_zero: (20.0, 20.0, 0.0, 0.0),
            zero_to_quadrant_1: (0.0, 20.0, 20.0, 20.0),
            quadrant_1_to_zero: (20.0, 0.0, -20.0, 340.0),
            zero_to_half: (0.0, 180.0, 180.0, 180.0),
            wraparound: (359.0, 1.0, 2.0, 2.0),
            negative_wraparound: (1.0, 359.0, -2.0, 358.0),
        }
    }
    mod conversions {
        use super::*;
        macro_rules! test_for_degrees_to_turns {
            ($($name:ident: $value:expr,)*) => {
                $(
                    #[test]
                    fn $name() {
                        let (deg, turns) = $value;
                        assert_about_eq!(turns, FAngle::from_deg(deg).turns());
                    }
                )*
            }
        }
        test_for_degrees_to_turns! {
            zero_to_zero: (0.0, 0.0),
            up_to_up: (90.0, 0.25),
            left_to_left: (180.0, 0.5),
        }
    }
}
