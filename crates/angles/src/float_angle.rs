use crate::*;
use derive_more;
use misc_utilities;
use ordered_float::OrderedFloat;
use rand::{rngs::StdRng, Rng, SeedableRng};
use std::f32::consts::{FRAC_PI_2, PI, TAU};

#[derive(
    Default,
    Copy,
    Clone,
    PartialEq,
    PartialOrd,
    derive_more::Add,
    derive_more::Sub,
    derive_more::Neg,
)]
pub struct FAngle(f32);

impl FAngle {
    pub const FULL_TURN: FAngle = FAngle(TAU);
    pub const HALF_TURN: FAngle = FAngle(PI);
    pub const QUARTER_TURN: FAngle = FAngle(FRAC_PI_2);
    pub const NO_TURN: FAngle = FAngle(0.0);

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
        self.rad() / TAU
    }
    pub fn quarter_turns(&self) -> f32 {
        self.turns() * 4.0
    }
    pub fn from_deg(x: f32) -> Self {
        Self(x.to_radians())
    }
    pub fn from_degrees(x: f32) -> Self {
        Self::from_deg(x)
    }

    pub fn from_rad(x: f32) -> Self {
        Self(x)
    }
    pub fn from_radians(x: f32) -> Self {
        Self::from_rad(x)
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
    pub fn xy(&self) -> [f32; 2] {
        [self.x(), self.y()]
    }
    pub fn tan(&self) -> f32 {
        self.rad().tan()
    }
    pub fn standardized_centered_at_zero(&self) -> Self {
        (*self + FAngle::HALF_TURN).standardized_starting_at_zero() - FAngle::HALF_TURN
    }
    pub fn standardized_starting_at_zero(&self) -> Self {
        Self::from_rad(self.rad().rem_euclid(TAU))
    }
    pub fn diagonals() -> [Self; 4] {
        misc_utilities::range_array::<4>()
            .map(|i| 1.0 / 8.0 + 1.0 / 4.0 * i as f32)
            .map(FAngle::from_turns)
    }
    pub fn orthogonals() -> [Self; 4] {
        core::array::from_fn(|i| deg(90.0 * i as f32))
    }
    pub fn snap_to_diagonal(&self) -> Self {
        Self::diagonals()
            .into_iter()
            .min_by_key(|&snap_angle| OrderedFloat(self.abs_smallest_angle_to(snap_angle).rad()))
            .unwrap()
    }
    pub fn dot(&self, other: Self) -> f32 {
        self.x() * other.x() + self.y() * other.y()
    }
    pub fn smallest_angle_to(&self, other: Self) -> Self {
        let a = self.standardized_starting_at_zero();
        let b = other.standardized_starting_at_zero();

        let diff = b - a;
        if diff.abs() < FAngle::HALF_TURN || diff == FAngle::HALF_TURN {
            diff
        } else if diff > FAngle::HALF_TURN {
            diff - FAngle::FULL_TURN
        } else {
            diff + FAngle::FULL_TURN
        }

        //         let bs = [b-FAngle::FULL_TURN, b, b+FAngle::FULL_TURN];
        //         let diffs: [FAngle; 3] = bs.map(|bi| bi - a);
        //         let the_min = diffs.into_iter().min_by_key(|di| OrderedFloat(di.abs().deg())).unwrap();
        //         // dbg!(a,b,bs, diffs, the_min);
        //         if the_min + FAngle::FULL_TURN  {
        //             the_min + FAngle::FULL_TURN
        //         } else {
        //             the_min
        //         }

        // bs.into_iter().min_by(|&b1, &b2 | (b1-a).abs().partial_cmp(&((b2-a).abs())).unwrap()).unwrap() - a
    }
    // Includes 0
    pub fn positive_angle_to(&self, other: Self) -> Self {
        let a = self.standardized_starting_at_zero();
        let b = other.standardized_starting_at_zero();
        if a <= b {
            b - a
        } else {
            b - a + FAngle::one_turn()
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

impl std::ops::Mul<f32> for FAngle {
    type Output = Self;
    fn mul(self, rhs: f32) -> Self::Output {
        Self::from_rad(self.rad().mul(rhs))
    }
}
impl std::ops::Div<f32> for FAngle {
    type Output = Self;
    fn div(self, rhs: f32) -> Self::Output {
        Self::from_rad(self.rad().div(rhs))
    }
}

impl std::fmt::Debug for FAngle {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("FAngle")
            .field("Radians", &self.rad())
            .field("Degrees", &self.deg())
            .field("Turns", &self.turns())
            .field("Quarter Turns", &self.quarter_turns())
            .finish()
    }
}


pub fn opposite_angle(a: FAngle) -> FAngle {
    a + FAngle::HALF_TURN
}

impl From<OrthoAngle> for FAngle {
    fn from(value: OrthoAngle) -> Self {
        FAngle::from_quarter_turns(value.quarter_turns_ccw() as f32)
    }
}

pub fn random_angle() -> FAngle {
    FAngle::from_degrees(rand::thread_rng().gen_range(0.0..360.0))
}

#[allow(non_snake_case)]
#[cfg(test)]
mod tests {
    use super::*;
    use more_asserts as ma;
    use ntest::assert_about_eq;
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
    fn test_standardized() {
        assert_about_eq!(
            0.1,
            FAngle::from_turns(2.1)
                .standardized_starting_at_zero()
                .turns()
        );
        assert_about_eq!(
            -0.1,
            FAngle::from_turns(2.9)
                .standardized_centered_at_zero()
                .turns()
        );
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
                FAngle::from_quarter_turns(a as f32).dot(FAngle::from_quarter_turns(b as f32)),
                c as f32
            );
        });
    }
    #[test]
    fn test_diagonal() {
        assert_about_eq!(
            FAngle::diagonals()[0].standardized_starting_at_zero().deg(),
            45.0,
            1e-4
        );
        assert_about_eq!(
            FAngle::diagonals()[1].standardized_starting_at_zero().deg(),
            (90.0 + 180.0) / 2.0,
            1e-4
        );
        assert_about_eq!(
            FAngle::diagonals()[2].standardized_starting_at_zero().deg(),
            (180.0 + 270.0) / 2.0,
            1e-4
        );
        assert_about_eq!(
            FAngle::diagonals()[3].standardized_starting_at_zero().deg(),
            (270.0 + 360.0) / 2.0,
            1e-4
        );
    }
    mod angle_to {
        use super::*;
        macro_rules! tests_for_angle_to {
            ($($name:ident: $value:expr,)*) => {
                $(
                    #[test]
                    fn $name() {
                        let (start, end, correct_small_deg, correct_pos_deg) = $value;
                        (0..365).for_each(|i| {
                            let start_plus = start + i as f32;
                            let end_plus = end + i as f32;
                            let small_deg = deg(start_plus).smallest_angle_to(deg(end_plus)).deg();
                            let abs_small_err =(correct_small_deg - small_deg).abs();
                            ma::assert_lt!(abs_small_err, 1e-4, "smallest_angle from {start_plus}° to {end_plus}° is {small_deg}°, should_have_been {correct_small_deg}°");

                            let pos_deg = deg(start_plus).positive_angle_to(deg(end_plus)).deg();
                            let abs_pos_err =(correct_pos_deg - pos_deg).abs();
                            ma::assert_lt!(abs_pos_err, 1e-4, "positive_angle from {start_plus}° to {end_plus}° is {pos_deg}°, should_have_been {correct_pos_deg}°");
                        });
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
            zero_to_nearly_half: (0.0, 179.0, 179.0, 179.0),
            nearly_half_to_zero: (179.0, 0.0, -179.0, 181.0),
            wraparound: (359.0, 1.0, 2.0, 2.0),
            negative_wraparound: (1.0, 359.0, -2.0, 358.0),
        }
    }
    #[test]
    fn test_positive_half_turn_for_smallest_angle_to() {
        assert_eq!(
            FAngle::HALF_TURN,
            FAngle::NO_TURN.smallest_angle_to(FAngle::HALF_TURN)
        );
        ma::assert_lt!(
            0.0,
            FAngle::QUARTER_TURN
                .smallest_angle_to(FAngle::QUARTER_TURN * 3.0)
                .rad()
        );
        let n = 10;
        for i in 0..n {
            let start = FAngle::FULL_TURN / n as f32 * i as f32;
            let end = start + FAngle::HALF_TURN;
            if end.rad() - start.rad() - PI == 0.0 {
                ma::assert_lt!(
                    0.0,
                    start.smallest_angle_to(end).rad(),
                    "start: {}, end: {}, end-start-PI: {}",
                    start.rad(),
                    end.rad(),
                    end.rad() - start.rad() - PI
                );
            }
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
