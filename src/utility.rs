extern crate num;

use euclid::*;
use num::traits::Signed;
use std::f32::consts::TAU;
use std::fmt::Display;
use std::ops::Neg;

// empty enums for euclid typing
pub enum WorldSpace {}

pub enum BrailleWorldSpace {}

pub enum CharacterWorldSpace {}

pub enum ScreenBufferCharacterSpace {}

pub enum ScreenCharacterSpace {}

pub type IPoint = default::Point2D<i32>;
pub type FPoint = default::Point2D<f32>;
pub type IVector = default::Vector2D<i32>;
pub type FVector = default::Vector2D<f32>;

pub type Square = Point2D<i32, WorldSpace>;
pub type Step = Vector2D<i32, WorldSpace>;
pub type SquareList = Vec<Point2D<i32, WorldSpace>>;
pub type StepList = Vec<Vector2D<i32, WorldSpace>>;

pub const DOWN_I: IVector = vec2(0, -1);
pub const UP_I: IVector = vec2(0, 1);
pub const LEFT_I: IVector = vec2(-1, 0);
pub const RIGHT_I: IVector = vec2(1, 0);

pub fn sign(x: f32) -> f32 {
    if x < 0.0 {
        -1.0
    } else if x > 0.0 {
        1.0
    } else {
        0.0
    }
}

pub fn sign2d<U>(point: Point2D<f32, U>) -> Point2D<f32, U> {
    point2(sign(point.x), sign(point.y))
}

pub fn fraction_part<U>(point: Point2D<f32, U>) -> Point2D<f32, U> {
    (point - point.round()).to_point()
}

pub fn get_by_point<T, U>(grid: &Vec<Vec<T>>, p: Point2D<i32, U>) -> &T {
    &grid[p.x as usize][p.y as usize]
}

pub fn int_to_T<T: Signed>(x: i32) -> T {
    match x {
        1 => T::one(),
        0 => T::zero(),
        -1 => -T::one(),
        _ => panic!(),
    }
}
pub fn quarter_turns_counter_clockwise<T: Signed + Copy, U>(
    v: &Vector2D<T, U>,
    quarter_periods: i32,
) -> Vector2D<T, U> {
    vec2(
        v.x * int_to_T(int_cos(quarter_periods)) - v.y * int_to_T(int_sin(quarter_periods)),
        v.x * int_to_T(int_sin(quarter_periods)) + v.y * int_to_T(int_cos(quarter_periods)),
    )
}
pub fn int_cos(quarter_periods: i32) -> i32 {
    match quarter_periods.rem_euclid(4) {
        0 => 1,
        1 | 3 => 0,
        2 => -1,
        _ => panic!(),
    }
}
pub fn int_sin(quarter_periods: i32) -> i32 {
    match quarter_periods.rem_euclid(4) {
        0 | 2 => 0,
        1 => 1,
        3 => -1,
        _ => panic!(),
    }
}

pub fn get_4_rotations<T: Signed + Copy, U>(v: Vector2D<T, U>) -> Vec<Vector2D<T, U>> {
    let mut all_4 = vec![];
    for i in 0..4 {
        all_4.push(quarter_turns_counter_clockwise(&v, i))
    }
    all_4
}

pub fn point_to_string<T: Display, U>(point: Point2D<T, U>) -> String {
    format!("(x: {}, y: {})", point.x, point.y)
}

pub fn round_to_king_step(step: Step) -> Step {
    if step == Step::new(0, 0) {
        return Step::new(0, 0);
    }
    let radians_from_plus_x = step.to_f32().angle_from_x_axis();
    let eighth_steps_from_plus_x = (radians_from_plus_x.radians * 8.0 / TAU).round();
    let rounded_radians_from_plus_x = Angle::radians(eighth_steps_from_plus_x * TAU / 8.0);

    let float_step =
        Vector2D::<f32, WorldSpace>::from_angle_and_length(rounded_radians_from_plus_x, 1.5);
    // 1.5 length to allow truncating down to 1 i32 in the diagonal case
    // because 1.5/sqrt(2) > 1.0

    // truncate towards zero intentionally
    float_step.to_i32()
}

#[cfg(test)]
mod tests {
    use pretty_assertions::{assert_eq, assert_ne};

    use super::*;

    #[test]
    fn test_round_to_kingstep() {
        assert_eq!(
            Step::new(0, 0),
            round_to_king_step(Step::new(0, 0)),
            "zero to zero"
        );
        assert_eq!(
            Step::new(1, 0),
            round_to_king_step(Step::new(5, 0)),
            "reduce length"
        );
        assert_eq!(
            Step::new(0, -1),
            round_to_king_step(Step::new(5, -300)),
            "snap to orthogonal"
        );
        assert_eq!(
            Step::new(-1, 1),
            round_to_king_step(Step::new(-30, 25)),
            "snap to diagonal"
        );
    }
}
