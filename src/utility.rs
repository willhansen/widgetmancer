extern crate num;

use std::collections::HashMap;
use std::f32::consts::TAU;
use std::fmt::Display;
use std::ops::Neg;

use euclid::*;
use num::traits::Signed;
use rand::Rng;

use crate::{Glyph, TwoGlyphs};

// empty enums for euclid typing
pub enum SquareGridInWorldFrame {}

pub enum BrailleGridInWorldFrame {}

pub enum CharacterGridInWorldFrame {}

pub enum CharacterGridInBufferFrame {}

pub enum CharacterGridInScreenFrame {}

pub type IPoint = default::Point2D<i32>;
pub type FPoint = default::Point2D<f32>;
pub type IVector = default::Vector2D<i32>;
pub type FVector = default::Vector2D<f32>;

pub type WorldSquare = Point2D<i32, SquareGridInWorldFrame>;
pub type WorldPoint = Point2D<f32, SquareGridInWorldFrame>;
pub type WorldSquareRect = Box2D<i32, SquareGridInWorldFrame>;
pub type BoardSize = Size2D<u32, SquareGridInWorldFrame>;

pub type WorldStep = Vector2D<i32, SquareGridInWorldFrame>;
pub type WorldMove = Vector2D<f32, SquareGridInWorldFrame>;

pub type SquareList = Vec<Point2D<i32, SquareGridInWorldFrame>>;
pub type StepList = Vec<Vector2D<i32, SquareGridInWorldFrame>>;

pub type WorldCharacterSquare = Point2D<i32, CharacterGridInWorldFrame>;
pub type WorldCharacterPoint = Point2D<f32, CharacterGridInWorldFrame>;

pub type WorldBrailleSquare = Point2D<i32, BrailleGridInWorldFrame>;
pub type WorldBraillePoint = Point2D<f32, BrailleGridInWorldFrame>;

pub type BufferCharacterSquare = Point2D<i32, CharacterGridInBufferFrame>;
pub type BufferCharacterPoint = Point2D<f32, CharacterGridInBufferFrame>;

pub type ScreenCharacterSquare = Point2D<i32, CharacterGridInScreenFrame>;
pub type ScreenCharacterPoint = Point2D<f32, CharacterGridInScreenFrame>;

pub type WorldSquareGlyphMap = HashMap<WorldSquare, TwoGlyphs>;
pub type WorldCharacterGlyphMap = HashMap<WorldCharacterSquare, Glyph>;
pub type BufferGlyphMap = HashMap<BufferCharacterSquare, Glyph>;

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

pub fn get_4_rotations_of<T: Signed + Copy, U>(v: Vector2D<T, U>) -> Vec<Vector2D<T, U>> {
    (0..4)
        .map(|i| quarter_turns_counter_clockwise(&v, i))
        .collect()
}
pub fn get_8_quadrants_of<T: Signed + Copy, U>(v: Vector2D<T, U>) -> Vec<Vector2D<T, U>> {
    let transpose = Vector2D::<T, U>::new(v.y, v.x);
    vec![v, transpose]
        .into_iter()
        .map(get_4_rotations_of)
        .flatten()
        .collect()
}

pub fn point_to_string<T: Display, U>(point: Point2D<T, U>) -> String {
    format!("(x: {}, y: {})", point.x, point.y)
}

pub fn round_to_king_step(step: WorldStep) -> WorldStep {
    if step.square_length() == 0 {
        return step;
    }
    let radians_from_plus_x = step.to_f32().angle_from_x_axis();
    let eighth_steps_from_plus_x = (radians_from_plus_x.radians * 8.0 / TAU).round();
    let rounded_radians_from_plus_x = Angle::radians(eighth_steps_from_plus_x * TAU / 8.0);

    let float_step = Vector2D::<f32, SquareGridInWorldFrame>::from_angle_and_length(
        rounded_radians_from_plus_x,
        1.5,
    );
    // 1.5 length to allow truncating down to 1 i32 in the diagonal case
    // because 1.5/sqrt(2) > 1.0

    // truncate towards zero intentionally
    float_step.to_i32()
}

pub fn is_king_step(step: WorldStep) -> bool {
    is_orthogonal_king_step(step) || is_diagonal_king_step(step)
}

pub fn is_orthogonal_king_step(step: WorldStep) -> bool {
    step.square_length() == 1
}

pub fn is_diagonal_king_step(step: WorldStep) -> bool {
    step.square_length() == 2
}

pub fn is_orthogonal(v: WorldMove) -> bool {
    v.x == 0.0 || v.y == 0.0
}

pub fn rand_radial_offset(radius: f32) -> default::Vector2D<f32> {
    let mut v = vec2(10.0, 10.0);
    while v.square_length() > 1.0 {
        v.x = rand::thread_rng().gen_range(-1.0..=1.0);
        v.y = rand::thread_rng().gen_range(-1.0..=1.0);
    }
    v * radius
}

pub fn rotate_vect<U>(vector: Vector2D<f32, U>, radians: f32) -> Vector2D<f32, U> {
    let angle = vector.angle_from_x_axis();
    let new_angle = angle + Angle::radians(radians);
    Vector2D::<f32, U>::from_angle_and_length(new_angle, vector.length())
}

pub fn world_square_glyph_map_to_world_character_glyph_map(
    world_square_glyph_map: WorldSquareGlyphMap,
) -> WorldCharacterGlyphMap {
    let mut world_character_glyph_map = WorldCharacterGlyphMap::new();
    world_square_glyph_map
        .into_iter()
        .for_each(|(world_square, two_glyphs)| {
            let left_char_square = Glyph::world_square_to_left_world_character_square(world_square);
            let right_char_square = left_char_square + RIGHT_I.cast_unit();
            world_character_glyph_map.insert(left_char_square, two_glyphs[0]);
            world_character_glyph_map.insert(right_char_square, two_glyphs[1]);
        });
    world_character_glyph_map
}

pub fn lerp(a: f32, b: f32, t: f32) -> f32 {
    a * (1.0 - t) + b * t
}

#[cfg(test)]
mod tests {
    use pretty_assertions::{assert_eq, assert_ne};

    use super::*;

    #[test]
    fn test_round_to_kingstep() {
        assert_eq!(
            WorldStep::new(0, 0),
            round_to_king_step(WorldStep::new(0, 0)),
            "zero to zero"
        );
        assert_eq!(
            WorldStep::new(1, 0),
            round_to_king_step(WorldStep::new(5, 0)),
            "reduce length"
        );
        assert_eq!(
            WorldStep::new(0, -1),
            round_to_king_step(WorldStep::new(5, -300)),
            "snap to orthogonal"
        );
        assert_eq!(
            WorldStep::new(-1, 1),
            round_to_king_step(WorldStep::new(-30, 25)),
            "snap to diagonal"
        );
    }
}
