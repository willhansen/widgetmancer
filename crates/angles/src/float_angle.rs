use std::{
    f32::consts::{FRAC_PI_2, FRAC_PI_4, PI, TAU},
};
use derive_more;

pub const HALF_TURN: FAngle = FAngle(PI);
pub const QUARTER_TURN: FAngle = FAngle(FRAC_PI_2);

#[derive(Default, Debug, Copy, Clone, PartialEq, derive_more::Add, derive_more::Sub)]
pub struct FAngle(f32);

impl FAngle {
    pub fn rad(&self) -> f32 {
        self.0
    }
    pub fn deg(&self) -> f32 {
        self.0.to_degrees()
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
    pub fn sin(&self) -> f32 {
        self.rad().sin()
    }
    pub fn cos(&self) -> f32 {
        self.rad().cos()
    }
    pub fn tan(&self) -> f32 {
        self.rad().tan()
    }
    pub fn standardized_with_zero_mid(&self) -> Self {
        (self + HALF_TURN).standardized_with_positive() - HALF_TURN
    }
    pub fn standardized_positive(&self) -> Self {
        Self::from_rad(self.rad().rem_euclid(TAU))
    }
    pub fn diagonals() -> [Self;4] {

    }
    pub fn orthogonals() -> [Self;4] {
        core::array::from_fn(|i| deg(90.0 * i as i32))

    }
    pub fn snap_to_diagonal(&self) -> Self {
        (0..4)
            .map(|i| standardize_angle_with_zero_mid(deg(45.0 + 90.0 * i as f32)))
            .min_by_key(|&snap_angle| OrderedFloat(abs_angle_distance(snap_angle, angle).radians))
            .unwrap()
    }
    pub fn dot(&self, other: &Self) -> f32 {
        WorldPoint::unit_vector_from_angle(a).dot(WorldPoint::unit_vector_from_angle(b))
    }
    pub fn angle_to(&self, other: &Self) -> Self {
        todo!()
    }
}
pub fn deg(x: f32) -> FAngle {
    FAngle::from_deg(x)
}

// TODO: make difference in names clearer
pub fn standardize_angle_with_zero_min(angle: FAngle) -> FAngle {}

pub fn abs_angle_distance(a: FAngle, b: FAngle) -> FAngle {
    Angle::radians(
        standardize_angle_with_zero_mid(a)
            .angle_to(standardize_angle_with_zero_mid(b))
            .radians
            .abs(),
    )
}

#[allow(non_snake_case)]
#[cfg(test)]
mod tests {
    use super::*;
    use ntest::assert_about_eq;

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
    mod angle_to {
        use super::*;
        macro_rules! tests_for_angle_to {
            ($($name:ident: $value:expr,)*) => {
                $(
                    #[test]
                    fn $name() {
                        let (start, end, correct_dist) = $value;
                        assert_about_eq!(expected, deg(start).angle_to(deg(end)).deg());
                    }
                )*
            }
        }
        tests_for_angle_to! {
            zero_at_zero: (0.0, 0.0, 0.0),
            zero_at_not_zero: (20.0, 20.0, 0.0),
            zero_to_quadrant_1: (0.0, 20.0, 20.0),
            zero_to_half: (0.0, 180.0, 180.0),
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
