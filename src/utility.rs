extern crate num;

use std::collections::{HashMap, HashSet};
use std::f32::consts::{PI, TAU};
use std::fmt::Display;
use std::ops::{Add, Neg};

use euclid::*;
use itertools::Itertools;
use num::traits::real::Real;
use num::traits::Signed;
use rand::Rng;

use crate::utility::coordinate_frame_conversions::*;
use crate::{DoubleGlyph, Glyph};

pub mod angle_interval;
pub mod coordinate_frame_conversions;

pub type IPoint = default::Point2D<i32>;
pub type FPoint = default::Point2D<f32>;
pub type IVector = default::Vector2D<i32>;
pub type FVector = default::Vector2D<f32>;

pub const DOWN_I: IVector = vec2(0, -1);
pub const UP_I: IVector = vec2(0, 1);
pub const LEFT_I: IVector = vec2(-1, 0);
pub const RIGHT_I: IVector = vec2(1, 0);

pub const STEP_UP: WorldStep = vec2(0, 1);
pub const STEP_DOWN: WorldStep = vec2(0, -1);
pub const STEP_RIGHT: WorldStep = vec2(1, 0);
pub const STEP_LEFT: WorldStep = vec2(-1, 0);

pub const STEP_UP_RIGHT: WorldStep = vec2(1, 1);
pub const STEP_UP_LEFT: WorldStep = vec2(-1, 1);
pub const STEP_DOWN_LEFT: WorldStep = vec2(-1, -1);
pub const STEP_DOWN_RIGHT: WorldStep = vec2(1, -1);

#[derive(Clone, PartialEq, Debug, Copy)]
pub struct Line<T, U> {
    pub p1: Point2D<T, U>,
    pub p2: Point2D<T, U>,
}

impl<T, U> Line<T, U> {
    pub fn new(p1: Point2D<T, U>, p2: Point2D<T, U>) -> Line<T, U> {
        Line { p1, p2 }
    }
}
impl<U> Line<f32, U> {
    pub fn point_is_on_line(&self, point: Point2D<f32, U>) -> bool {
        in_line(self.p1, self.p2, point)
    }
    pub fn point_clockwise_of_line(&self) -> Point2D<f32, U> {
        rotate_point_around_point(self.p1, self.p2, Angle::radians(-PI / 2.0))
    }
}

impl<U> Add<Vector2D<f32, U>> for Line<f32, U> {
    type Output = Line<f32, U>;

    fn add(self, rhs: Vector2D<f32, U>) -> Self::Output {
        Line {
            p1: self.p1 + rhs,
            p2: self.p2 + rhs,
        }
    }
}

pub struct Ray<U> {
    pub point: Point2D<f32, U>,
    pub angle: Angle<f32>,
}

#[derive(Clone, PartialEq, Debug, Copy)]
pub struct HalfPlane<T, U> {
    pub dividing_line: Line<T, U>,
    pub point_on_half_plane: Point2D<T, U>,
}

impl<U: Copy> HalfPlane<f32, U> {
    pub fn new(line: Line<f32, U>, point: Point2D<f32, U>) -> Self {
        assert!(!line.point_is_on_line(point));
        HalfPlane {
            dividing_line: line,
            point_on_half_plane: point,
        }
    }
}

pub type WorldLine = Line<f32, SquareGridInWorldFrame>;
pub type LocalCharacterLine = Line<f32, CharacterGridInLocalCharacterFrame>;

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

pub fn king_distance(a: WorldSquare, b: WorldSquare) -> u32 {
    let x_dist = a.x.abs_diff(b.x);
    let y_dist = a.y.abs_diff(b.y);
    x_dist.max(y_dist)
}

pub fn reversed<T: Copy>(v: Vec<T>) -> Vec<T> {
    let mut new_v = v.clone();
    new_v.reverse();
    new_v
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

pub fn lerp(a: f32, b: f32, t: f32) -> f32 {
    a * (1.0 - t) + b * t
}

pub fn derivative(f: fn(f32) -> f32, x: f32, dx: f32) -> f32 {
    if dx == 0.0 {
        panic!("approximate derivatives only!");
    }
    (f(x + dx / 2.0) - f(x - dx / 2.0)) / dx
}

pub fn pair_up_glyph_map(
    character_glyph_map: WorldCharacterSquareToGlyphMap,
) -> WorldSquareGlyphMap {
    let mut output_map = WorldSquareGlyphMap::new();
    character_glyph_map
        .into_iter()
        .for_each(|(character_square, glyph)| {
            let world_square = world_character_square_to_world_square(character_square);
            let is_left_glyph =
                is_world_character_square_left_square_of_world_square(character_square);
            let position_index = if is_left_glyph { 0 } else { 1 };

            if output_map.contains_key(&world_square) {
                let mut existing_glyph = output_map.get_mut(&world_square).unwrap();
                existing_glyph[position_index] = glyph;
            } else {
                let mut new_double_glyph = [Glyph::default_transparent(); 2];
                new_double_glyph[position_index] = glyph;
                output_map.insert(world_square, new_double_glyph);
            }
        });

    output_map
}

pub fn glyph_map_to_string(glyph_map: &WorldCharacterSquareToGlyphMap) -> String {
    let top_row = glyph_map.keys().map(|square| square.y).max().unwrap();
    let bottom_row = glyph_map.keys().map(|square| square.y).min().unwrap();
    let left_column = glyph_map.keys().map(|square| square.x).min().unwrap();
    let right_column = glyph_map.keys().map(|square| square.x).max().unwrap();
    let mut string = String::new();
    for bottom_to_top_y in bottom_row..=top_row {
        let y = top_row + bottom_row - bottom_to_top_y;
        for x in left_column..=right_column {
            let square = WorldCharacterSquare::new(x, y);
            let new_part = if let Some(glyph) = glyph_map.get(&square) {
                glyph.to_string()
            } else {
                " ".to_string()
            };

            string += &new_part;
        }
        string += "\n";
    }
    string
}

pub fn print_glyph_map(glyph_map: &WorldCharacterSquareToGlyphMap) {
    print!("{}", glyph_map_to_string(glyph_map));
}

pub fn line_intersections_with_centered_unit_square<U>(line: Line<f32, U>) -> Vec<Point2D<f32, U>> {
    let line_point_a = line.p1;
    let line_point_b = line.p2;
    let is_same_point = line_point_a == line_point_b;
    let is_vertical_line = line_point_a.x == line_point_b.x;
    let is_horizontal_line = line_point_a.y == line_point_b.y;
    if is_same_point {
        panic!("gave same point {}", point_to_string(line_point_a));
    } else if is_vertical_line {
        let x = line_point_a.x;
        if x.abs() <= 0.5 {
            vec![point2(x, 0.5), point2(x, -0.5)]
        } else {
            vec![]
        }
    } else if is_horizontal_line {
        let y = line_point_a.y;
        if y.abs() <= 0.5 {
            vec![point2(y, 0.5), point2(y, -0.5)]
        } else {
            vec![]
        }
    } else {
        // y = mx + b
        let dy = line_point_b.y - line_point_a.y;
        let dx = line_point_b.x - line_point_a.x;
        let m = dy / dx;
        // b = y - m*x
        let b = line_point_a.y - m * line_point_a.x;

        let side_positions = vec![0.5, -0.5];

        let mut candidate_intersections: Vec<Point2D<f32, U>> = vec![];
        for &x in &side_positions {
            let y = m * x + b;
            if y.abs() <= 0.5 {
                candidate_intersections.push(point2(x, y));
            }
        }
        for y in side_positions {
            let x = (y - b) / m;
            // top and bottom don't catch corners, sides do
            if x.abs() < 0.5 {
                candidate_intersections.push(point2(x, y));
            }
        }
        // this captures the edge case of corners
        // remove duplicates
        match candidate_intersections.len() {
            2 => {
                if candidate_intersections[0] == candidate_intersections[1] {
                    vec![candidate_intersections[0]]
                } else {
                    candidate_intersections
                }
            }
            1 => candidate_intersections,
            0 => vec![],
            _ => {
                panic!(
                    "Too many intersections {}",
                    candidate_intersections
                        .iter()
                        .map(|&point| point_to_string(point))
                        .join(", ")
                );
            }
        }
    }
}

pub fn same_side_of_line<U>(
    line: Line<f32, U>,
    point_c: Point2D<f32, U>,
    point_d: Point2D<f32, U>,
) -> bool {
    let point_a = line.p1;
    let point_b = line.p2;
    is_clockwise(point_a, point_b, point_c) == is_clockwise(point_a, point_b, point_d)
}

pub fn is_clockwise<U>(a: Point2D<f32, U>, b: Point2D<f32, U>, c: Point2D<f32, U>) -> bool {
    let ab = b - a;
    let ac = c - a;
    ab.cross(ac) < 0.0
}

pub fn in_line<U>(a: Point2D<f32, U>, b: Point2D<f32, U>, c: Point2D<f32, U>) -> bool {
    let ab = b - a;
    let ac = c - a;
    ab.cross(ac) == 0.0
}

pub fn is_world_character_square_left_square_of_world_square(
    character_square: WorldCharacterSquare,
) -> bool {
    world_square_to_left_world_character_square(world_character_square_to_world_square(
        character_square,
    )) == character_square
}

pub fn octant_to_outward_and_across_directions(octant_number: i32) -> (WorldStep, WorldStep) {
    let reduced_octant = octant_number.rem_euclid(8);
    // TODO: probably make this an actual equation
    match reduced_octant {
        0 => (STEP_RIGHT, STEP_UP),
        1 => (STEP_UP, STEP_RIGHT),
        2 => (STEP_UP, STEP_LEFT),
        3 => (STEP_LEFT, STEP_UP),
        4 => (STEP_LEFT, STEP_DOWN),
        5 => (STEP_DOWN, STEP_LEFT),
        6 => (STEP_DOWN, STEP_RIGHT),
        7 => (STEP_RIGHT, STEP_DOWN),
        _ => panic!("bad octant: {}", reduced_octant),
    }
}

pub fn rotate_point_around_point<U>(
    axis_point: Point2D<f32, U>,
    moving_point: Point2D<f32, U>,
    angle: Angle<f32>,
) -> Point2D<f32, U> {
    axis_point + rotate_vect((moving_point - axis_point), angle.radians)
}

#[cfg(test)]
mod tests {
    use ntest::assert_false;
    use pretty_assertions::{assert_eq, assert_ne};
    use rgb::RGB8;

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

    #[test]
    fn test_pair_up_glyph_map__positions() {
        let character_squares: Vec<WorldCharacterSquare> = vec![
            point2(0, 0),
            point2(1, 0),
            point2(1, 1),
            point2(2, 0),
            point2(2, 1),
        ];

        let mut character_glyph_map = WorldCharacterSquareToGlyphMap::new();
        for square in character_squares {
            character_glyph_map.insert(square, Glyph::default_transparent());
        }
        let square_glyph_map = pair_up_glyph_map(character_glyph_map);
        let correct_squares = vec![point2(0, 0), point2(1, 0), point2(0, 1), point2(1, 1)];
        assert_eq!(square_glyph_map.len(), correct_squares.len());
        for square in correct_squares {
            assert!(square_glyph_map.contains_key(&square));
        }
    }

    #[test]
    fn test_pair_up_glyph_map__glyphs() {
        let mut character_glyph_map = WorldCharacterSquareToGlyphMap::new();
        let test_glyph = Glyph {
            character: ' ',
            fg_color: RGB8::new(0, 0, 0),
            bg_color: RGB8::new(100, 100, 150),
            bg_transparent: false,
        };
        character_glyph_map.insert(point2(0, 0), test_glyph);
        character_glyph_map.insert(point2(1, 0), test_glyph);
        let square_glyph_map = pair_up_glyph_map(character_glyph_map);
        assert_eq!(square_glyph_map.len(), 1);
        assert_eq!(
            *square_glyph_map.get(&point2(0, 0)).unwrap(),
            [test_glyph; 2]
        );
    }

    #[test]
    fn test_clockwise() {
        assert!(is_clockwise::<WorldPoint>(
            point2(0.0, 0.0),
            point2(0.0, 1.0),
            point2(1.0, 0.0),
        ));
        assert_false!(is_clockwise::<WorldPoint>(
            point2(0.0, 0.0),
            point2(0.0, 1.0),
            point2(-0.1, -10.0)
        ));
    }

    #[test]
    fn test_world_pos_to_character_world_pos() {
        assert_eq!(
            Point2D::<f32, CharacterGridInWorldFrame>::new(0.5, 0.0),
            world_point_to_world_character_point(Point2D::<f32, SquareGridInWorldFrame>::new(
                0.0, 0.0,
            )),
            "zero is actually between two characters"
        );
        assert_eq!(
            Point2D::<f32, CharacterGridInWorldFrame>::new(2.5, 1.0),
            world_point_to_world_character_point(Point2D::<f32, SquareGridInWorldFrame>::new(
                1.0, 1.0,
            )),
            "diagonal a bit"
        );
    }
    #[test]
    fn test_local_square_point_to_local_character_point() {
        assert_eq!(
            local_square_point_to_local_character_point(point2(0.0, 0.0), 0),
            point2(0.5, 0.0)
        );
        assert_eq!(
            local_square_point_to_local_character_point(point2(0.0, 0.0), 1),
            point2(-0.5, 0.0)
        );
    }
    #[test]
    fn test_line_intersections_with_square_are_in_same_order_as_input_line() {
        let input_line: Line<f32, SquareGridInWorldFrame> =
            Line::new(point2(-1.5, -1.0), point2(0.0, 0.0));
        let output_points = line_intersections_with_centered_unit_square(input_line);
        let output_line = Line::new(output_points[0], output_points[1]);
        let in_vec = input_line.p2 - input_line.p1;
        let out_vec = output_line.p2 - output_line.p1;

        let same_direction = in_vec.dot(out_vec) > 0.0;
        assert!(same_direction);
    }
}
