#![allow(dead_code)]
#![allow(deprecated)]

extern crate num;

use std::collections::{HashMap, HashSet};
use std::f32::consts::{LN_2, PI, TAU};
use std::fmt::{Debug, Display, Formatter};
use std::hash::Hash;
use std::mem;
use std::ops::{Add, Neg, Sub};

use derive_more::{AddAssign, Constructor, Neg};
use euclid::approxeq::ApproxEq;
use euclid::*;
use getset::CopyGetters;
use itertools::Itertools;
use line_drawing::Supercover;
use num::traits::Signed;
use ordered_float::OrderedFloat;
use rand::rngs::StdRng;
use rand::{Rng, SeedableRng};
use rgb::RGB8;

pub mod angle_interval;
pub use angle_interval::*;

pub mod coordinate_frame_conversions;
pub use coordinate_frame_conversions::*;

use crate::geometry2::{FPointExt, IPointExt};

pub mod geometry2;

pub type IPoint = default::Point2D<i32>;
pub type FPoint = default::Point2D<f32>;
pub type IVector = default::Vector2D<i32>;
pub type FVector = default::Vector2D<f32>;

pub const DOWN_I: IVector = vec2(0, -1);
pub const UP_I: IVector = vec2(0, 1);
pub const LEFT_I: IVector = vec2(-1, 0);
pub const RIGHT_I: IVector = vec2(1, 0);

pub const STEP_ZERO: WorldStep = vec2(0, 0);
pub const STEP_UP: WorldStep = vec2(0, 1);
pub const STEP_DOWN: WorldStep = vec2(0, -1);
pub const STEP_RIGHT: WorldStep = vec2(1, 0);
pub const STEP_LEFT: WorldStep = vec2(-1, 0);

pub const STEP_UP_RIGHT: WorldStep = vec2(1, 1);
pub const STEP_UP_LEFT: WorldStep = vec2(-1, 1);
pub const STEP_DOWN_LEFT: WorldStep = vec2(-1, -1);
pub const STEP_DOWN_RIGHT: WorldStep = vec2(1, -1);

pub const ORTHOGONAL_STEPS: [WorldStep; 4] = [STEP_UP, STEP_DOWN, STEP_RIGHT, STEP_LEFT];
pub const DIAGONAL_STEPS: [WorldStep; 4] =
    [STEP_UP_RIGHT, STEP_UP_LEFT, STEP_DOWN_RIGHT, STEP_DOWN_LEFT];
pub const KING_STEPS: [WorldStep; 8] = [
    STEP_UP,
    STEP_DOWN,
    STEP_RIGHT,
    STEP_LEFT,
    STEP_UP_RIGHT,
    STEP_UP_LEFT,
    STEP_DOWN_RIGHT,
    STEP_DOWN_LEFT,
];

#[macro_export]
macro_rules! pub_mod_and_use {
    ($($module:ident), +) => {
        $(
            pub mod $module;
            pub use self::$module::*;
        )+
    };
}

#[macro_export]
macro_rules! pub_use {
    ($($module:ident,)+) => {
        $(
            pub use self::$module::*;
        )+
    };
}

#[macro_export]
macro_rules! boxx {
    ($x:expr) => {
        Box::new($x)
    };
}

#[derive(Hash, Default, Debug, Copy, Clone, Eq, PartialEq, CopyGetters, AddAssign)]
#[get_copy = "pub"]
pub struct QuarterTurnsAnticlockwise {
    quarter_turns: i32,
}

impl QuarterTurnsAnticlockwise {
    pub fn new(quarter_turns: i32) -> Self {
        QuarterTurnsAnticlockwise {
            quarter_turns: quarter_turns.rem_euclid(4),
        }
    }
    pub fn to_vector(&self) -> WorldStep {
        rotated_n_quarter_turns_counter_clockwise(STEP_RIGHT, self.quarter_turns)
    }
    pub fn from_vector(dir: WorldStep) -> Self {
        assert!(is_orthogonal(dir));
        QuarterTurnsAnticlockwise::new(if dir.x == 0 {
            if dir.y > 0 {
                1
            } else {
                3
            }
        } else if dir.x > 0 {
            0
        } else {
            2
        })
    }

    pub fn from_start_and_end_directions(start: WorldStep, end: WorldStep) -> Self {
        assert!(is_king_step(start));
        assert!(is_king_step(end));
        // needs to be quarter turn, no eighths
        assert_eq!(is_diagonal(start), is_diagonal(end));

        let d_angle = start.to_f32().angle_to(end.to_f32());
        let quarter_turns = (d_angle.to_degrees() / 90.0).round() as i32;
        Self::new(quarter_turns)
    }

    pub fn rotate_angle(&self, angle: Angle<f32>) -> Angle<f32> {
        standardize_angle(Angle::<f32>::degrees(
            angle.to_degrees() + 90.0 * (self.quarter_turns() as f32),
        ))
    }
    pub fn rotate_vector<T, U>(&self, v: Vector2D<T, U>) -> Vector2D<T, U>
    where
        T: Signed + Copy,
    {
        rotated_n_quarter_turns_counter_clockwise(v, self.quarter_turns)
    }
}

impl Neg for QuarterTurnsAnticlockwise {
    type Output = Self;

    fn neg(self) -> Self::Output {
        QuarterTurnsAnticlockwise::new(-self.quarter_turns)
    }
}

impl Add for QuarterTurnsAnticlockwise {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        Self::new(self.quarter_turns() + rhs.quarter_turns())
    }
}

impl Sub for QuarterTurnsAnticlockwise {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        Self::new(self.quarter_turns() - rhs.quarter_turns())
    }
}

impl From<QuarterTurnsAnticlockwise> for i32 {
    fn from(val: QuarterTurnsAnticlockwise) -> Self {
        val.quarter_turns()
    }
}
impl From<i32> for QuarterTurnsAnticlockwise {
    fn from(value: i32) -> Self {
        Self::new(value)
    }
}

#[derive(Clone, PartialEq, Debug, Copy)]
pub struct Octant(i32);

impl Octant {
    pub fn new(octant: i32) -> Self {
        Octant(octant.rem_euclid(8))
    }
    pub fn with_n_quarter_turns_anticlockwise(
        &self,
        quarter_turns: QuarterTurnsAnticlockwise,
    ) -> Self {
        Self::new(self.0 + quarter_turns.quarter_turns() * 2)
    }
    pub fn outward_and_across_directions(&self) -> (WorldStep, WorldStep) {
        // TODO: probably make this an actual equation
        match self.0 {
            0 => (STEP_RIGHT, STEP_UP),
            1 => (STEP_UP, STEP_RIGHT),
            2 => (STEP_UP, STEP_LEFT),
            3 => (STEP_LEFT, STEP_UP),
            4 => (STEP_LEFT, STEP_DOWN),
            5 => (STEP_DOWN, STEP_LEFT),
            6 => (STEP_DOWN, STEP_RIGHT),
            7 => (STEP_RIGHT, STEP_DOWN),
            _ => panic!("bad octant: {}", self.0),
        }
    }
    pub fn number(&self) -> i32 {
        self.0
    }
}

#[derive(Clone, PartialEq, Copy)]
pub struct Line<T, U> {
    pub p1: Point2D<T, U>,
    pub p2: Point2D<T, U>,
}

impl<T, U> Line<T, U>
where
    T: Clone + Debug + PartialEq + Signed + Copy,
{
    pub fn new(p1: Point2D<T, U>, p2: Point2D<T, U>) -> Line<T, U> {
        assert_ne!(p1, p2);
        Line { p1, p2 }
    }
    pub fn reverse(&mut self) {
        mem::swap(&mut self.p2, &mut self.p1);
    }
    pub fn reversed(&self) -> Self {
        Self::new(self.p2, self.p1)
    }
    pub fn get(&self, index: u32) -> Point2D<T, U> {
        match index {
            0 => self.p1,
            1 => self.p2,
            _ => panic!("only two points defining the line"),
        }
    }
    pub fn rotated(&self, quarter_rotations_anticlockwise: i32) -> Self {
        let new_points = [0, 1].map(|i| {
            point_rotated_n_quarter_turns_counter_clockwise(
                self.get(i),
                quarter_rotations_anticlockwise,
            )
        });
        Self::new(new_points[0], new_points[1])
    }
    pub fn square_length(&self) -> T {
        (self.p1 - self.p2).square_length()
    }
}

impl<U: Copy> Line<f32, U> {
    pub fn length(&self) -> f32 {
        (self.p1 - self.p2).length()
    }
    pub fn point_is_on_line(&self, point: Point2D<f32, U>) -> bool {
        on_line(self.p1, self.p2, point)
    }
    pub fn point_is_approx_on_line(&self, point: Point2D<f32, U>, tolerance: f32) -> bool {
        self.normal_distance_to_point(point) < tolerance
    }
    pub fn normal_distance_to_point(&self, point: Point2D<f32, U>) -> f32 {
        let p1_to_point = point - self.p1;
        let p1_to_p2 = self.p2 - self.p1;
        let parallel_part_of_p1_to_point = p1_to_point.project_onto_vector(p1_to_p2);
        let perpendicular_part_of_p1_to_point = p1_to_point - parallel_part_of_p1_to_point;
        perpendicular_part_of_p1_to_point.length()
    }
    pub fn point_clockwise_of_line(&self) -> Point2D<f32, U> {
        rotate_point_around_point(self.p1, self.p2, Angle::radians(-PI / 2.0))
    }
    pub fn point_anticlockwise_of_line(&self) -> Point2D<f32, U> {
        rotate_point_around_point(self.p1, self.p2, Angle::radians(PI / 2.0))
    }
    pub fn point_right_of_line(&self) -> Point2D<f32, U> {
        self.point_clockwise_of_line()
    }
    pub fn point_left_of_line(&self) -> Point2D<f32, U> {
        self.point_anticlockwise_of_line()
    }
    pub fn lerp(&self, t: f32) -> Point2D<f32, U> {
        lerp2d(self.p1, self.p2, t)
    }
    pub fn point_is_on_or_normal_to_line_segment(&self, point: Point2D<f32, U>) -> bool {
        let start_point = self.p1;
        let end_point = self.p2;

        let point_relative_to_start_point = point - start_point;
        let end_point_relative_to_start_point = end_point - start_point;
        let point_is_on_end_side_of_start_point =
            point_relative_to_start_point.dot(end_point_relative_to_start_point) > 0.0;

        let point_relative_to_end_point = point - end_point;
        let point_is_on_start_side_of_end_point =
            point_relative_to_end_point.dot(-end_point_relative_to_start_point) > 0.0;

        point_is_on_end_side_of_start_point && point_is_on_start_side_of_end_point
    }

    pub fn approx_eq_eps(&self, other: Self, tolerance: f32) -> bool {
        let p11 = self
            .p1
            .approx_eq_eps(&other.p1, &point2(tolerance, tolerance));
        let p22 = self
            .p2
            .approx_eq_eps(&other.p2, &point2(tolerance, tolerance));
        let p12 = self
            .p1
            .approx_eq_eps(&other.p2, &point2(tolerance, tolerance));
        let p21 = self
            .p2
            .approx_eq_eps(&other.p1, &point2(tolerance, tolerance));

        // don't care about point order
        (p11 && p22) || (p12 && p21)
    }

    pub fn approx_on_same_line(&self, other: Self, tolerance: f32) -> bool {
        self.point_is_approx_on_line(other.p1, tolerance)
            && self.point_is_approx_on_line(other.p2, tolerance)
    }

    pub fn angle_with_positive_x_axis(&self) -> Angle<f32> {
        let angle_a = better_angle_from_x_axis(self.p1 - self.p2);
        let angle_b = better_angle_from_x_axis(self.p2 - self.p1);
        if angle_a.radians.cos() < 0.0 {
            angle_b
        } else {
            angle_a
        }
    }

    pub fn reflect_point_over_line(&self, point: Point2D<f32, U>) -> Point2D<f32, U> {
        let p1_to_p = point - self.p1;
        let p1_to_p2 = self.p2 - self.p1;
        let parallel_part = p1_to_p.project_onto_vector(p1_to_p2);
        let perpendicular_part = p1_to_p - parallel_part;
        let p1_to_reflected_p = parallel_part - perpendicular_part;
        self.p1 + p1_to_reflected_p
    }

    pub fn direction(&self) -> Angle<f32> {
        better_angle_from_x_axis(self.p2 - self.p1)
    }
    pub fn from_ray(start: Point2D<f32, U>, angle: Angle<f32>, length: f32) -> Self {
        assert!(length > 0.0);
        Self::new(start, naive_ray_endpoint(start, angle, length))
    }
    pub fn same_side_of_line(&self, point_c: Point2D<f32, U>, point_d: Point2D<f32, U>) -> bool {
        let point_a = self.p1;
        let point_b = self.p2;
        let c_on_line = self.point_is_on_line(point_c);
        let d_on_line = self.point_is_on_line(point_d);

        if c_on_line {
            return if d_on_line { true } else { false };
        } else if d_on_line {
            return false;
        }

        is_clockwise(point_a, point_b, point_c) == is_clockwise(point_a, point_b, point_d)
    }
    pub fn line_intersections_with_centered_unit_square(&self) -> Vec<Point2D<f32, U>> {
        let line_point_a = self.p1;
        let line_point_b = self.p2;
        let is_same_point = line_point_a == line_point_b;
        if is_same_point {
            panic!("gave same point {}", point_to_string(line_point_a));
        }

        let is_vertical_line = line_point_a.x == line_point_b.x;
        let is_horizontal_line = line_point_a.y == line_point_b.y;

        if is_vertical_line {
            let x = line_point_a.x;
            if x.abs() <= 0.5 {
                self.points_in_line_order(vec![point2(x, 0.5), point2(x, -0.5)])
            } else {
                vec![]
            }
        } else if is_horizontal_line {
            let y = line_point_a.y;
            if y.abs() <= 0.5 {
                self.points_in_line_order(vec![point2(0.5, y), point2(-0.5, y)])
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
                        self.points_in_line_order(candidate_intersections)
                    }
                }
                1 => candidate_intersections,
                0 => vec![],
                _ => furthest_apart_points(candidate_intersections).into(),
            }
        }
    }
    pub fn line_intersects_with_centered_unit_square(&self) -> bool {
        !self
            .line_intersections_with_centered_unit_square()
            .is_empty()
    }
    fn points_in_line_order(&self, mut points: Vec<Point2D<f32, U>>) -> Vec<Point2D<f32, U>> {
        let normalized_line_direction = (self.p2 - self.p1).normalize();
        points.sort_by_key(|&point| OrderedFloat(normalized_line_direction.dot(point.to_vector())));
        points
    }

    pub fn seeded_random_point_on_line(&self, rng: &mut StdRng) -> Point2D<f32, U> {
        let t = rng.gen_range(0.0..=1.0);
        self.lerp(t)
    }

    pub fn seeded_random_point_near_line(&self, rng: &mut StdRng, radius: f32) -> Point2D<f32, U> {
        // TODO: make more uniform
        self.seeded_random_point_on_line(rng) + seeded_rand_radial_offset(rng, radius).cast_unit()
    }

    pub fn random_point_near_line(&self, radius: f32) -> Point2D<f32, U> {
        self.seeded_random_point_near_line(&mut get_new_rng(), radius)
    }
    pub fn intersection_point_with_other_extended_line(
        &self,
        other: &Self,
    ) -> Option<Point2D<f32, U>> {
        // Equation from https://en.wikipedia.org/wiki/Line%E2%80%93line_intersection
        let (x1, y1) = self.p1.to_tuple();
        let (x2, y2) = self.p2.to_tuple();
        let (x3, y3) = other.p1.to_tuple();
        let (x4, y4) = other.p2.to_tuple();

        let a = x1 * y2 - y1 * x2;
        let b = x3 * y4 - y3 * x4;
        let denominator = (x1 - x2) * (y3 - y4) - (y1 - y2) * (x3 - x4);
        if denominator == 0.0 {
            return None;
        }
        let final_x = (a * (x3 - x4) - (x1 - x2) * b) / denominator;
        let final_y = (a * (y3 - y4) - (y1 - y2) * b) / denominator;
        return Some(point2(final_x, final_y));
    }

    pub fn intersection_point_with_other_line(&self, other: &Self) -> Option<Point2D<f32, U>> {
        if self.same_side_of_line(other.p1, other.p2) || other.same_side_of_line(self.p1, self.p2) {
            let on_same_line = self.point_is_on_line(other.p1);
            if !on_same_line {
                return None;
            }
            return if self.p2 == other.p1 && on_line_in_this_order(self.p1, self.p2, other.p2) {
                Some(self.p2)
            } else if self.p2 == other.p2 && on_line_in_this_order(self.p1, self.p2, other.p1) {
                Some(self.p2)
            } else if self.p1 == other.p1 && on_line_in_this_order(self.p2, self.p1, other.p2) {
                Some(self.p1)
            } else if self.p1 == other.p2 && on_line_in_this_order(self.p2, self.p1, other.p1) {
                Some(self.p1)
            } else {
                None
            };
        }
        // from here, we know the line segments are overlapping, including the case of exactly touching
        // A simple line intersection check is all that's left

        self.intersection_point_with_other_extended_line(&other)
    }
}

impl<T, U> From<[[T; 2]; 2]> for Line<T, U>
where
    T: Clone + Debug + PartialEq + Signed + Copy,
{
    fn from(value: [[T; 2]; 2]) -> Self {
        Self {
            p1: value[0].into(),
            p2: value[1].into(),
        }
    }
}

impl WorldLine {
    pub fn touched_squares(&self) -> Vec<WorldSquare> {
        let start_square = world_point_to_world_square(self.p1);
        let end_square = world_point_to_world_square(self.p2);
        // TODO: use better line algorithm.  Account for floating point start and ends
        line_drawing::WalkGrid::new(start_square.to_tuple(), end_square.to_tuple())
            .map(|(x, y)| point2(x, y))
            .collect_vec()
    }
}

impl<T, U> Debug for Line<T, U>
where
    T: Display + Copy,
{
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "p1: {}, p2: {}",
            point_to_string(self.p1),
            point_to_string(self.p2),
        )
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

#[derive(Clone, Debug, Copy)]
pub struct HalfPlane<T, U>
where
    T: Display + Copy,
{
    // Internal convention is that the half plane is clockwise of the vector from p1 to p2 of the dividing line
    dividing_line: Line<T, U>,
}

impl<U: Copy + Debug> HalfPlane<f32, U> {
    pub fn from_line_and_point_on_half_plane(
        dividing_line: Line<f32, U>,
        point_on_half_plane: Point2D<f32, U>,
    ) -> Self {
        HalfPlane {
            dividing_line: if is_clockwise(dividing_line.p1, dividing_line.p2, point_on_half_plane)
            {
                dividing_line
            } else {
                dividing_line.reversed()
            },
        }
    }

    pub fn complement(&self) -> Self {
        HalfPlane::from_line_and_point_on_half_plane(
            self.dividing_line,
            self.point_off_half_plane(),
        )
    }
    pub fn dividing_line(&self) -> Line<f32, U> {
        self.dividing_line
    }

    pub fn point_on_half_plane(&self) -> Point2D<f32, U> {
        self.dividing_line.point_right_of_line()
    }

    pub fn point_off_half_plane(&self) -> Point2D<f32, U> {
        self.dividing_line
            .reflect_point_over_line(self.point_on_half_plane())
    }

    pub fn is_about_complementary_to(&self, other: Self, tolerance: f32) -> bool {
        self.dividing_line
            .approx_on_same_line(other.dividing_line, tolerance)
            && !self
                .dividing_line
                .same_side_of_line(self.point_on_half_plane(), other.point_on_half_plane())
    }

    pub fn point_is_on_half_plane(&self, point: Point2D<f32, U>) -> bool {
        self.dividing_line
            .same_side_of_line(self.point_on_half_plane(), point)
    }
    pub fn overlapping_or_touching_point(&self, point: Point2D<f32, U>) -> bool {
        !self
            .dividing_line
            .same_side_of_line(self.point_off_half_plane(), point)
    }
    pub fn covers_origin(&self) -> bool {
        self.point_is_on_half_plane(point2(0.0, 0.0))
    }
    pub fn fully_covers_unit_square(&self) -> bool {
        self.fully_covers_expanded_unit_square(0.0)
    }
    pub fn fully_covers_expanded_unit_square(&self, per_face_extension: f32) -> bool {
        DIAGONAL_STEPS
            .map(Vector2D::to_f32)
            .map(|x| x * (0.5 + per_face_extension))
            .map(Vector2D::to_point)
            .map(Point2D::cast_unit)
            .iter()
            .all(|&p| self.overlapping_or_touching_point(p))
    }
    pub fn extended(&self, extended_distance: f32) -> Self {
        let direction = self.direction_away_from_plane();
        let move_vector = Vector2D::from_angle_and_length(direction, extended_distance);

        let line = self.dividing_line();
        let point = self.point_on_half_plane();

        let shifted_point = point + move_vector;
        let shifted_line = Line::new(line.p1 + move_vector, line.p2 + move_vector);

        Self::from_line_and_point_on_half_plane(shifted_line, shifted_point)
    }
    pub fn direction_away_from_plane(&self) -> Angle<f32> {
        standardize_angle(self.dividing_line.direction() + Angle::degrees(90.0))
    }
    pub fn direction_toward_plane(&self) -> Angle<f32> {
        standardize_angle(-self.direction_away_from_plane())
    }

    pub fn at_least_partially_covers_unit_square(&self) -> bool {
        !self.complement().fully_covers_unit_square()
    }

    //Fn(Point2D<f32, U>) -> Point2D<f32, V>,
    //fun: Box<dyn Fn<Point2D<f32, U>, Output = Point2D<f32, V>>>,
    pub fn with_transformed_points<F, V>(&self, point_transform_function: F) -> HalfPlane<f32, V>
    where
        V: Copy + Debug,
        F: Fn(Point2D<f32, U>) -> Point2D<f32, V>,
    {
        HalfPlane::from_line_and_point_on_half_plane(
            Line {
                p1: point_transform_function(self.dividing_line.p1),
                p2: point_transform_function(self.dividing_line.p2),
            },
            point_transform_function(self.point_on_half_plane()),
        )
    }
    pub fn rotated(&self, quarter_rotations_anticlockwise: i32) -> Self {
        let line = self.dividing_line();
        let point = self.point_on_half_plane();
        let new_point =
            point_rotated_n_quarter_turns_counter_clockwise(point, quarter_rotations_anticlockwise);
        let new_line = line.rotated(quarter_rotations_anticlockwise);
        Self::from_line_and_point_on_half_plane(new_line, new_point)
    }
    pub fn top_half_plane() -> Self {
        Self::from_line_and_point_on_half_plane(
            Line::<f32, U> {
                p1: Point2D::new(1.0, 0.0),
                p2: Point2D::new(-1.0, 0.0),
            },
            Point2D::<f32, U>::new(0.0, 1.0),
        )
    }
    pub fn depth_of_point_in_half_plane(&self, point: Point2D<f32, U>) -> f32 {
        let dist = self.dividing_line().normal_distance_to_point(point);
        if self.point_is_on_half_plane(point) {
            dist
        } else {
            -dist
        }
    }
    pub fn from_clockwise_sweeping_line(line: [[f32; 2]; 2]) -> Self {
        Self {
            dividing_line: line.into(),
        }
    }
}

pub type WorldLine = Line<f32, SquareGridInWorldFrame>;

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

pub fn rotated_n_quarter_turns_counter_clockwise<T: Signed + Copy, U>(
    v: Vector2D<T, U>,
    quarter_turns: i32,
) -> Vector2D<T, U> {
    vec2(
        v.x * int_to_T(int_cos(quarter_turns)) - v.y * int_to_T(int_sin(quarter_turns)),
        v.x * int_to_T(int_sin(quarter_turns)) + v.y * int_to_T(int_cos(quarter_turns)),
    )
}

pub fn point_rotated_n_quarter_turns_counter_clockwise<T: Signed + Copy, U>(
    p: Point2D<T, U>,
    quarter_turns: i32,
) -> Point2D<T, U> {
    rotated_n_quarter_turns_counter_clockwise(p.to_vector(), quarter_turns).to_point()
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
        .map(|i| rotated_n_quarter_turns_counter_clockwise(v, i))
        .collect()
}

pub fn get_8_octants_of<T: Signed + Copy, U>(v: Vector2D<T, U>) -> Vec<Vector2D<T, U>> {
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

pub fn vector2_to_string<T: Display, U>(vec: Vector2D<T, U>) -> String {
    format!("(dx: {}, dy: {})", vec.x, vec.y)
}

pub fn king_distance(step: WorldStep) -> u32 {
    step.x.abs().max(step.y.abs()) as u32
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
    let radians_from_plus_x = better_angle_from_x_axis(step.to_f32());
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

pub fn is_orthogonal<T: Signed, U>(v: Vector2D<T, U>) -> bool {
    v.x == T::zero() || v.y == T::zero()
}

pub fn is_diagonal<T: Signed, U>(v: Vector2D<T, U>) -> bool {
    v.x == v.y || v.x == v.y.neg()
}

pub fn is_orthodiagonal<T: Signed + Copy, U>(v: Vector2D<T, U>) -> bool {
    is_orthogonal(v) || is_diagonal(v)
}

pub fn seeded_rand_radial_offset(rng: &mut StdRng, radius: f32) -> default::Vector2D<f32> {
    let mut v = vec2(10.0, 10.0);
    while v.square_length() > 1.0 {
        v.x = rng.gen_range(-1.0..=1.0);
        v.y = rng.gen_range(-1.0..=1.0);
    }
    v * radius
}

pub fn get_new_rng() -> StdRng {
    StdRng::from_rng(rand::thread_rng()).unwrap()
}

pub fn rand_radial_offset(radius: f32) -> default::Vector2D<f32> {
    seeded_rand_radial_offset(&mut get_new_rng(), radius)
}

pub fn random_event(p: f32) -> bool {
    assert!(p >= 0.0 && p <= 1.0);
    rand::thread_rng().gen_range(0.0..=1.0) < p
}

pub fn random_angle() -> Angle<f32> {
    Angle::degrees(rand::thread_rng().gen_range(0.0..360.0))
}

pub fn random_unit_vector() -> FVector {
    let angle = random_angle();
    unit_vector_from_angle(angle)
}

pub fn unit_vector_from_angle(angle: Angle<f32>) -> FVector {
    vec2(angle.radians.cos(), angle.radians.sin())
}

pub fn snap_angle_to_diagonal(angle: Angle<f32>) -> Angle<f32> {
    (0..4)
        .map(|i| standardize_angle(Angle::degrees(45.0 + 90.0 * i as f32)))
        .min_by_key(|&snap_angle| OrderedFloat(abs_angle_distance(snap_angle, angle).radians))
        .unwrap()
}

pub fn random_choice<'a, T>(rng: &'a mut StdRng, v: &'a Vec<T>) -> &'a T {
    v.get(rng.gen_range(0..v.len())).unwrap()
}

pub fn rotate_vect<U>(vector: Vector2D<f32, U>, delta_angle: Angle<f32>) -> Vector2D<f32, U> {
    if vector.length() == 0.0 {
        return vector;
    }
    let start_angle = better_angle_from_x_axis(vector);
    let new_angle = start_angle + delta_angle;
    Vector2D::<f32, U>::from_angle_and_length(new_angle, vector.length())
}

pub fn lerp(a: f32, b: f32, t: f32) -> f32 {
    a * (1.0 - t) + b * t
}

pub fn lerp2d<U>(a: Point2D<f32, U>, b: Point2D<f32, U>, t: f32) -> Point2D<f32, U> {
    point2(lerp(a.x, b.x, t), lerp(a.y, b.y, t))
}

pub fn derivative(f: fn(f32) -> f32, x: f32, dx: f32) -> f32 {
    if dx == 0.0 {
        panic!("approximate derivatives only!");
    }
    (f(x + dx / 2.0) - f(x - dx / 2.0)) / dx
}

fn furthest_apart_points<U>(points: Vec<Point2D<f32, U>>) -> [Point2D<f32, U>; 2] {
    assert!(points.len() >= 2);
    let furthest = points
        .iter()
        .combinations(2)
        .max_by_key(|two_points: &Vec<&Point2D<f32, U>>| {
            OrderedFloat((*two_points[0] - *two_points[1]).length())
        })
        .unwrap();
    let furthest_values: Vec<Point2D<f32, U>> = furthest.into_iter().copied().collect();
    furthest_values.try_into().unwrap()
}

pub fn is_clockwise<U>(a: Point2D<f32, U>, b: Point2D<f32, U>, c: Point2D<f32, U>) -> bool {
    let ab = b - a;
    let ac = c - a;
    ab.cross(ac) < 0.0
}

pub fn on_line<U>(a: Point2D<f32, U>, b: Point2D<f32, U>, c: Point2D<f32, U>) -> bool {
    let ab = b - a;
    let ac = c - a;
    ab.cross(ac) == 0.0
}
pub fn on_line_in_this_order<U>(
    a: Point2D<f32, U>,
    b: Point2D<f32, U>,
    c: Point2D<f32, U>,
) -> bool {
    on_line(a, b, c) && (a - b).length() < (a - c).length()
}

pub fn rotate_point_around_point<U>(
    axis_point: Point2D<f32, U>,
    moving_point: Point2D<f32, U>,
    angle: Angle<f32>,
) -> Point2D<f32, U> {
    axis_point + rotate_vect(moving_point - axis_point, angle)
}

pub fn cross_correlate_squares_with_steps(
    squares: SquareSet,
    steps: HashSet<WorldStep>,
) -> HashMap<WorldSquare, u32> {
    let mut step_count_map = HashMap::<WorldSquare, u32>::new();
    squares.iter().for_each(|&square| {
        steps
            .iter()
            .map(|&diagonal_step| square + diagonal_step)
            .for_each(|step_square| *step_count_map.entry(step_square).or_default() += 1)
    });
    step_count_map
}

pub fn map_sum<K, V>(a: HashMap<K, V>, b: HashMap<K, V>) -> HashMap<K, V>
where
    K: Eq + Hash,
    V: Default + std::ops::AddAssign,
{
    let mut sum = HashMap::new();
    for m in [a, b] {
        m.into_iter().for_each(|(k, v): (K, V)| {
            *sum.entry(k).or_default() += v;
        });
    }
    sum
}

pub fn map_neg<K, V>(m: HashMap<K, V>) -> HashMap<K, V>
where
    K: Eq + Hash,
    V: Neg<Output = V>,
{
    m.into_iter().map(|(k, v): (K, V)| (k, -v)).collect()
}

pub fn map_to_signed<K>(m: HashMap<K, u32>) -> HashMap<K, i32>
where
    K: Eq + Hash,
{
    m.into_iter()
        .map(|(k, v): (K, u32)| (k, v as i32))
        .collect()
}

pub fn map_to_float<K>(m: HashMap<K, i32>) -> HashMap<K, f32>
where
    K: Eq + Hash,
{
    m.into_iter()
        .map(|(k, v): (K, i32)| (k, v as f32))
        .collect()
}

pub fn hue_to_rgb(hue_360: f32) -> RGB8 {
    let hue = hue_360.rem_euclid(360.0);
    let x = 1.0 - ((hue / 60.0) % 2.0 - 1.0).abs();
    let float_rgb: (f32, f32, f32) = if hue < 60.0 {
        (1.0, x, 0.0)
    } else if hue < 120.0 {
        (x, 1.0, 0.0)
    } else if hue < 180.0 {
        (0.0, 1.0, x)
    } else if hue < 240.0 {
        (0.0, x, 1.0)
    } else if hue < 300.0 {
        (x, 0.0, 1.0)
    } else {
        (1.0, 0.0, x)
    };
    RGB8::new(
        (float_rgb.0 * 255.0) as u8,
        (float_rgb.1 * 255.0) as u8,
        (float_rgb.2 * 255.0) as u8,
    )
}

pub fn adjacent_king_steps(dir: WorldStep) -> StepSet {
    assert!(is_king_step(dir));
    if ORTHOGONAL_STEPS.contains(&dir) {
        if dir.x != 0 {
            HashSet::from([dir + STEP_UP, dir + STEP_DOWN])
        } else {
            HashSet::from([dir + STEP_LEFT, dir + STEP_RIGHT])
        }
    } else {
        let no_x = vec2(0, dir.y);
        let no_y = vec2(dir.x, 0);
        HashSet::from([no_x, no_y])
    }
}

#[derive(Clone, Hash, Eq, PartialEq, Neg, Debug, Copy, CopyGetters, Constructor)]
#[get_copy = "pub"]
pub struct StepWithQuarterRotations {
    stepp: WorldStep,
    rotation: QuarterTurnsAnticlockwise,
}

impl StepWithQuarterRotations {
    pub fn from_direction_squares(
        start: SquareWithOrthogonalDir,
        end: SquareWithOrthogonalDir,
    ) -> Self {
        let translation = end.square() - start.square();
        let rotation = end.direction_in_quarter_turns() - start.direction_in_quarter_turns();
        Self::new(translation, rotation)
    }
}

impl Default for StepWithQuarterRotations {
    fn default() -> Self {
        StepWithQuarterRotations::new(STEP_ZERO, QuarterTurnsAnticlockwise::new(0))
    }
}

impl Add for StepWithQuarterRotations {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        StepWithQuarterRotations::new(self.stepp + rhs.stepp, self.rotation + rhs.rotation)
    }
}

#[derive(Clone, Hash, Eq, PartialEq, Copy, CopyGetters)]
#[get_copy = "pub"]
pub struct SquareWithOrthogonalDir {
    square: WorldSquare,
    step: OrthogonalWorldStep,
}

impl SquareWithOrthogonalDir {
    pub fn direction_in_quarter_turns(&self) -> QuarterTurnsAnticlockwise {
        QuarterTurnsAnticlockwise::from_start_and_end_directions(STEP_RIGHT, self.step.into())
    }
    pub fn from_square_and_step(square: WorldSquare, direction: WorldStep) -> Self {
        SquareWithOrthogonalDir {
            square,
            step: direction.into(),
        }
    }
    pub fn from_square_and_worldstep(square: WorldSquare, direction: WorldStep) -> Self {
        Self::from_square_and_step(square, direction.into())
    }
    pub fn from_square_and_turns(
        square: WorldSquare,
        quarter_turns: QuarterTurnsAnticlockwise,
    ) -> Self {
        SquareWithOrthogonalDir::from_square_and_worldstep(square, quarter_turns.to_vector().into())
    }
    pub fn direction(&self) -> OrthogonalWorldStep {
        self.step()
    }
    pub fn stepped(&self) -> Self {
        SquareWithOrthogonalDir::from_square_and_worldstep(
            self.square + self.direction().step(),
            self.direction().into(),
        )
    }
    pub fn stepped_n(&self, n: i32) -> Self {
        SquareWithOrthogonalDir::from_square_and_worldstep(
            self.square + self.direction().step() * n,
            self.direction().into(),
        )
    }
    pub fn stepped_back(&self) -> Self {
        SquareWithOrthogonalDir::from_square_and_worldstep(
            self.square - self.direction().step(),
            self.direction().into(),
        )
    }
    pub fn strafed_left(&self) -> Self {
        self.strafed_right_n(-1)
    }
    pub fn strafed_right(&self) -> Self {
        self.strafed_right_n(1)
    }
    pub fn strafed_right_n(&self, n: i32) -> Self {
        SquareWithOrthogonalDir::from_square_and_worldstep(
            self.square + rotated_n_quarter_turns_counter_clockwise(self.direction().into(), 3) * n,
            self.direction().into(),
        )
    }
    pub fn strafed_left_n(&self, n: i32) -> Self {
        self.strafed_right_n(-n)
    }

    pub fn turned_left(&self) -> Self {
        SquareWithOrthogonalDir::from_square_and_worldstep(
            self.square,
            rotated_n_quarter_turns_counter_clockwise(self.direction().into(), 1).into(),
        )
    }
    pub fn turned_right(&self) -> Self {
        SquareWithOrthogonalDir::from_square_and_worldstep(
            self.square,
            rotated_n_quarter_turns_counter_clockwise(self.direction().into(), 3).into(),
        )
    }
    pub fn turned_back(&self) -> Self {
        SquareWithOrthogonalDir::from_square_and_worldstep(self.square, -self.direction().step())
    }
    pub fn with_offset(&self, offset: WorldStep) -> Self {
        Self::from_square_and_step(self.square + offset, self.step().into())
    }
    pub fn with_direction(&self, dir: WorldStep) -> Self {
        Self::from_square_and_worldstep(self.square, dir)
    }
    pub fn reversed(&self) -> Self {
        self.with_direction(-self.direction().step())
    }
}

impl Debug for SquareWithOrthogonalDir {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Pos: {}, Dir: {}",
            point_to_string(self.square),
            vector2_to_string(self.direction().step())
        )
    }
}

impl TryFrom<SquareWithKingDir> for SquareWithOrthogonalDir {
    type Error = ();

    fn try_from(value: SquareWithKingDir) -> Result<Self, Self::Error> {
        if is_orthogonal(value.direction().into()) {
            Ok(SquareWithOrthogonalDir::from_square_and_worldstep(
                value.square(),
                value.direction().into(),
            ))
        } else {
            Err(())
        }
    }
}

impl Add<StepWithQuarterRotations> for SquareWithOrthogonalDir {
    type Output = Self;

    fn add(self, rhs: StepWithQuarterRotations) -> Self::Output {
        SquareWithOrthogonalDir::from_square_and_turns(
            self.square + rhs.stepp,
            self.direction_in_quarter_turns() + rhs.rotation,
        )
    }
}

impl Sub<SquareWithOrthogonalDir> for SquareWithOrthogonalDir {
    type Output = StepWithQuarterRotations;

    fn sub(self, rhs: SquareWithOrthogonalDir) -> Self::Output {
        StepWithQuarterRotations::new(
            self.square - rhs.square,
            self.direction_in_quarter_turns() - rhs.direction_in_quarter_turns(),
        )
    }
}

#[derive(Clone, Hash, Eq, PartialEq, Debug, Copy)]
pub struct KingWorldStep {
    step: WorldStep,
}

impl KingWorldStep {
    pub fn new(dir: WorldStep) -> Self {
        assert!(is_king_step(dir));
        KingWorldStep { step: dir }
    }
    pub fn step(&self) -> WorldStep {
        self.step
    }
}

impl From<WorldStep> for KingWorldStep {
    fn from(value: WorldStep) -> Self {
        KingWorldStep::new(value)
    }
}

impl From<OrthogonalWorldStep> for KingWorldStep {
    fn from(value: OrthogonalWorldStep) -> Self {
        KingWorldStep::new(value.step)
    }
}

impl From<KingWorldStep> for WorldStep {
    fn from(value: KingWorldStep) -> Self {
        value.step
    }
}

#[derive(Clone, Hash, Eq, PartialEq, Debug, Copy)]
pub struct OrthogonalWorldStep {
    step: WorldStep,
}

impl OrthogonalWorldStep {
    pub fn new(dir: WorldStep) -> Self {
        assert!(is_orthogonal_king_step(dir));
        OrthogonalWorldStep { step: dir }
    }
    pub fn step(&self) -> WorldStep {
        self.step
    }
}

impl From<WorldStep> for OrthogonalWorldStep {
    fn from(value: WorldStep) -> Self {
        OrthogonalWorldStep::new(value)
    }
}

impl From<OrthogonalWorldStep> for WorldStep {
    fn from(value: OrthogonalWorldStep) -> Self {
        value.step
    }
}

impl From<KingWorldStep> for OrthogonalWorldStep {
    fn from(value: KingWorldStep) -> Self {
        OrthogonalWorldStep::new(value.step)
    }
}
impl From<(WorldSquare, OrthogonalWorldStep)> for SquareWithOrthogonalDir {
    fn from(value: (WorldSquare, OrthogonalWorldStep)) -> Self {
        Self::from_square_and_step(value.0, value.1.into())
    }
}
impl From<(WorldSquare, WorldStep)> for SquareWithOrthogonalDir {
    fn from(value: (WorldSquare, WorldStep)) -> Self {
        Self::from_square_and_step(value.0, value.1)
    }
}
impl From<SquareWithOrthogonalDir> for (WorldSquare, OrthogonalWorldStep) {
    fn from(value: SquareWithOrthogonalDir) -> (WorldSquare, OrthogonalWorldStep) {
        (value.square, value.direction())
    }
}

#[derive(Clone, Hash, Eq, PartialEq, Debug, Copy, CopyGetters)]
#[get_copy = "pub"]
pub struct SquareWithKingDir {
    square: WorldSquare,
    direction: KingWorldStep,
}

impl SquareWithKingDir {
    pub fn new(square: WorldSquare, direction: KingWorldStep) -> Self {
        SquareWithKingDir { square, direction }
    }
    pub fn from_square_and_step(square: WorldSquare, direction: WorldStep) -> SquareWithKingDir {
        Self::new(square, direction.into())
    }
    pub fn tuple(&self) -> (WorldSquare, KingWorldStep) {
        (self.square, self.direction)
    }
    pub fn stepped(&self) -> SquareWithKingDir {
        SquareWithKingDir::from_square_and_step(
            self.square + self.direction.step,
            self.direction.into(),
        )
    }
}

impl From<SquareWithOrthogonalDir> for SquareWithKingDir {
    fn from(value: SquareWithOrthogonalDir) -> Self {
        SquareWithKingDir {
            square: value.square(),
            direction: value.direction().into(),
        }
    }
}

impl From<(WorldSquare, KingWorldStep)> for SquareWithKingDir {
    fn from(value: (WorldSquare, KingWorldStep)) -> Self {
        Self::new(value.0, value.1)
    }
}
impl From<SquareWithKingDir> for (WorldSquare, KingWorldStep) {
    fn from(value: SquareWithKingDir) -> (WorldSquare, KingWorldStep) {
        (value.square, value.direction)
    }
}

#[derive(Clone, Hash, Eq, PartialEq, Debug, Copy, CopyGetters)]
#[get_copy = "pub"]
pub struct TranslationAndRotationTransform {
    translation: WorldStep,
    quarter_rotations_counterclockwise: u32,
}

pub fn set_of_keys<K, V>(hashmap: &HashMap<K, V>) -> HashSet<K>
where
    K: Clone + Hash + Eq,
{
    hashmap.keys().cloned().collect::<HashSet<K>>()
}

pub fn union<T: Clone + Hash + Eq>(a: &HashSet<T>, b: &HashSet<T>) -> HashSet<T> {
    a.union(b).cloned().collect()
}

pub fn intersection<T: Clone + Hash + Eq>(a: &HashSet<T>, b: &HashSet<T>) -> HashSet<T> {
    a.intersection(b).cloned().collect()
}

// TODO: remember the reason for this existing (there IS a good reason)
// related to `test_built_in_angle_from_x_axis_can_not_be_trusted`
pub fn better_angle_from_x_axis<U>(v: Vector2D<f32, U>) -> Angle<f32> {
    Angle::radians(v.y.atan2(v.x))
}

pub fn standardize_angle(angle: Angle<f32>) -> Angle<f32> {
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

pub fn abs_angle_distance(a: Angle<f32>, b: Angle<f32>) -> Angle<f32> {
    Angle::radians(
        standardize_angle(a)
            .angle_to(standardize_angle(b))
            .radians
            .abs(),
    )
}

pub fn revolve_square(
    moving_square: WorldSquare,
    pivot_square: WorldSquare,
    rotation: QuarterTurnsAnticlockwise,
) -> WorldSquare {
    let rel_square = moving_square - pivot_square;
    pivot_square + rotation.rotate_vector(rel_square)
}

pub fn rgb_to_string(rgb: RGB8) -> String {
    format!("( {:>3}, {:>3}, {:>3} )", rgb.r, rgb.g, rgb.b)
}

pub fn flip_y<T, U>(v: Vector2D<T, U>) -> Vector2D<T, U>
where
    T: Signed,
{
    vec2(v.x, -v.y)
}

pub fn flip_x<T, U>(v: Vector2D<T, U>) -> Vector2D<T, U>
where
    T: Signed,
{
    vec2(-v.x, v.y)
}

// TODO: turn into iter
pub fn squares_on_board(size: BoardSize) -> SquareSet {
    (0..size.width)
        .cartesian_product((0..size.height).into_iter())
        .map(|(x, y)| WorldSquare::new(x as i32, y as i32))
        .collect()
}

pub fn tint_color(original_color: RGB8, tint_color: RGB8, mut tint_strength: f32) -> RGB8 {
    // basically just lerp for now, I guess
    tint_strength = tint_strength.clamp(0.0, 1.0);
    let c1: [u8; 3] = original_color.into();
    let c2: [u8; 3] = tint_color.into();
    (0..3)
        .map(|i| lerp(c1[i] as f32, c2[i] as f32, tint_strength).round() as u8)
        .collect_tuple::<(u8, u8, u8)>()
        .unwrap()
        .into()
}

pub fn number_to_hue_rotation(x: f32, period: f32) -> RGB8 {
    hue_to_rgb(x * 360.0 / period)
}

pub fn ith_projection_of_step(step: WorldStep, i: u32) -> WorldStep {
    match i {
        0 => WorldStep::new(step.x, 0),
        1 => WorldStep::new(0, step.y),
        _ => panic!("Too many dimensions: {}", i),
    }
}

pub fn snap_to_nths(x: f32, denominator: u32) -> f32 {
    (x * denominator as f32).round() / denominator as f32
}
pub fn looping_clamp(a: f32, b: f32, x: f32) -> f32 {
    assert!(a < b);
    ((x - a).rem_euclid(b - a)) + a
}

pub fn square_is_odd(square: WorldSquare) -> bool {
    (square.x + square.y) % 2 == 1
}
pub fn square_is_even(square: WorldSquare) -> bool {
    !square_is_odd(square)
}

#[derive(Debug, Clone)]
pub struct BoolArray2D<const WIDTH: usize, const HEIGHT: usize> {
    array: [[bool; WIDTH]; HEIGHT],
}

impl<const WIDTH: usize, const HEIGHT: usize> BoolArray2D<WIDTH, HEIGHT> {
    pub fn from_array(array: [[bool; WIDTH]; HEIGHT]) -> Self {
        Self { array }
    }
    pub fn empty() -> Self {
        Self::from_array([[false; WIDTH]; HEIGHT])
    }
    pub fn get_xy(&self, x: usize, y: usize) -> bool {
        self.get_row_col(Self::y_to_row(y), x)
    }
    pub fn set_xy(&mut self, x: usize, y: usize, val: bool) {
        self.set_row_col(Self::y_to_row(y), x, val);
    }
    fn y_to_row(y: usize) -> usize {
        HEIGHT - 1 - y
    }
    pub fn get_row_col(&self, row: usize, col: usize) -> bool {
        self.array[row][col]
    }
    pub fn set_row_col(&mut self, row: usize, col: usize, val: bool) {
        self.array[row][col] = val;
    }
    pub fn print(&self) {
        for row in 0..HEIGHT {
            let mut line = "".to_string();
            for col in 0..WIDTH {
                let val = self.get_row_col(row, col);
                line += if val { "o" } else { "." };
            }
            println!("{}", line);
        }
    }
    pub fn height() -> usize {
        HEIGHT
    }
    pub fn width() -> usize {
        WIDTH
    }
}
pub type SquareBoolArray2D<const SIZE: usize> = BoolArray2D<SIZE, SIZE>;

impl<const SIZE: usize> SquareBoolArray2D<SIZE> {
    pub fn rotated(&self, quarter_turns: QuarterTurnsAnticlockwise) -> Self {
        let rotation_function = match quarter_turns.quarter_turns() {
            0 => |x, y| (x, y),
            1 => |x, y| (SIZE - 1 - y, x),
            2 => |x, y| (SIZE - 1 - x, SIZE - 1 - y),
            3 => |x, y| (y, SIZE - 1 - x),
            _ => panic!("Malformed parameter"),
        };
        let mut the_clone = self.clone();
        for x in 0..SIZE {
            for y in 0..SIZE {
                let (new_x, new_y) = rotation_function(x, y);
                the_clone.set_xy(new_x, new_y, self.get_xy(x, y));
            }
        }
        the_clone
    }
}

pub fn first_inside_square_face_hit_by_ray(
    start: WorldPoint,
    angle: Angle<f32>,
    range: f32,
    inside_faces: &HashSet<SquareWithOrthogonalDir>,
) -> Option<(SquareWithOrthogonalDir, WorldPoint)> {
    let ray_direction: WorldMove = unit_vector_from_angle(angle).cast_unit();

    let inside_faces_facing_ray: HashSet<SquareWithOrthogonalDir> = inside_faces
        .iter()
        .filter(|&&face| {
            let vector_into_face = face.direction();
            ray_direction.dot(vector_into_face.step().to_f32()) >= 0.0
        })
        .cloned()
        .collect();

    let naive_end_point: WorldPoint = start + unit_vector_from_angle(angle).cast_unit() * range;

    let squares_on_naive_line: HashSet<WorldSquare> = Supercover::new(
        start.to_i32().to_tuple(),
        naive_end_point.to_i32().to_tuple(),
    )
    .map(|(x, y)| WorldSquare::new(x, y))
    .collect();

    let inside_faces_of_squares_touching_line: HashSet<SquareWithOrthogonalDir> =
        inside_faces_facing_ray
            .iter()
            .filter(|face| squares_on_naive_line.contains(&face.square()))
            .cloned()
            .collect();

    inside_faces_of_squares_touching_line
        .iter()
        .map(|&face| {
            (
                face,
                ray_intersection_point_with_oriented_square_face(start, angle, range, face),
            )
        })
        .filter(|(_face, point)| point.is_some())
        .map(|(face, point)| (face, point.unwrap()))
        .min_by_key(|(_face, point)| OrderedFloat((start - *point).length()))
}
pub fn square_face_as_line(square: WorldSquare, face_direction: OrthogonalWorldStep) -> WorldLine {
    let square_center = square.to_f32();
    let face_center = square_center + face_direction.step().to_f32() * 0.5;
    let p1_direction = rotated_n_quarter_turns_counter_clockwise(face_direction.step(), 1);
    let p2_direction = -p1_direction;
    WorldLine::new(
        face_center + p1_direction.to_f32() * 0.5,
        face_center + p2_direction.to_f32() * 0.5,
    )
}
pub fn ray_intersection_point_with_oriented_square_face(
    start: WorldPoint,
    angle: Angle<f32>,
    range: f32,
    face: SquareWithOrthogonalDir,
) -> Option<WorldPoint> {
    let ray_direction: WorldMove = unit_vector_from_angle(angle).cast_unit();
    let face_is_facing_ray = ray_direction.dot(face.step().step().to_f32()) > 0.0;
    if !face_is_facing_ray {
        return None;
    }
    let face_line_segment = square_face_as_line(face.square, face.step);
    let ray_line_segment = WorldLine::from_ray(start, angle, range);
    ray_line_segment.intersection_point_with_other_line(&face_line_segment)
}
pub fn does_ray_hit_oriented_square_face(
    start: WorldPoint,
    angle: Angle<f32>,
    range: f32,
    face: SquareWithOrthogonalDir,
) -> bool {
    ray_intersection_point_with_oriented_square_face(start, angle, range, face).is_some()
}

pub fn assert_about_eq_2d(p1: impl Into<WorldPoint>, p2: impl Into<WorldPoint>) {
    let p1 = p1.into();
    let p2 = p2.into();
    let tolerance = 0.001;
    assert!(
        (p1 - p2).length().abs() < tolerance,
        "Points too far apart: p1: {:?}, p2: {:?}",
        p1,
        p2
    );
}

pub fn naive_ray_endpoint<U>(
    start: Point2D<f32, U>,
    angle: Angle<f32>,
    length: f32,
) -> Point2D<f32, U> {
    start + unit_vector_from_angle(angle).cast_unit() * length
}

#[derive(Eq, PartialEq, Debug, Copy, Clone, Hash, Constructor, CopyGetters)]
#[get_copy = "pub"]
pub struct NStep {
    stepp: WorldStep,
    n: Option<u32>,
}

impl NStep {
    pub fn one(stepp: WorldStep) -> Self {
        NStep { stepp, n: Some(1) }
    }
    pub fn dir(step: WorldStep) -> Self {
        NStep {
            stepp: step,
            n: None,
        }
    }

    pub fn quadrant_symmetries(&self) -> Vec<Self> {
        get_4_rotations_of(self.stepp)
            .into_iter()
            .map(|step| NStep::new(step, self.n))
            .collect()
    }
    pub fn octant_symmetries(&self) -> Vec<Self> {
        get_8_octants_of(self.stepp)
            .into_iter()
            .map(|step| NStep::new(step, self.n))
            .collect()
    }
}

pub trait ToDebug {
    fn to_debug(&self) -> String;
}

impl<T> ToDebug for T
where
    T: Debug,
{
    fn to_debug(&self) -> String {
        format!("{:?}", self)
    }
}

pub fn array_zip<T1, T2, const N: usize>(a: [T1; N], b: [T2; N]) -> [(T1, T2); N]
where
    T1: Debug,
    T2: Debug,
{
    a.into_iter()
        .zip(b.into_iter())
        .collect_vec()
        .try_into()
        .unwrap()
}

// trunc, but away from zero instead of towards zero
pub fn trunceil(x: f32) -> f32 {
    if x == x.trunc() {
        x
    } else {
        x.trunc() + x.signum()
    }
}

pub fn transpose_grid<T: Copy>(grid: &Vec<Vec<T>>) -> Vec<Vec<T>> {
    let height = grid.len();
    let width = grid[0].len();

    (0..width)
        .map(|col| (0..height).map(|row| grid[row][col]).collect_vec())
        .collect_vec()
}
pub fn exponential_approach(
    start_pos: geometry2::FPoint,
    target_pos: geometry2::FPoint,
    dt: f32,
    dist_halflife: f32,
) -> geometry2::FPoint {
    let time_constant = dist_halflife / LN_2;
    let dp = target_pos.sub(start_pos);
    let dist = dp.length();
    let end_dist = dist * (-dt / time_constant).exp();
    target_pos.sub(dp.normalized().mul(end_dist))
}
pub fn linear_approach(
    start_pos: geometry2::FPoint,
    target_pos: geometry2::FPoint,
    dt: f32,
    speed: f32,
) -> geometry2::FPoint {
    let dp = target_pos.sub(start_pos);
    let dist = dp.length();
    let arrival_dt = dist / speed;
    if arrival_dt < dt {
        return target_pos;
    }

    start_pos.lerp(target_pos, dt / arrival_dt)
}

pub fn exponential_approach_with_min_speed(
    start_pos: geometry2::FPoint,
    target_pos: geometry2::FPoint,
    dt: f32,
    dist_halflife: f32,
    min_speed: f32,
) -> geometry2::FPoint {
    let delta_pos = target_pos.sub(start_pos);
    let start_dist = delta_pos.length();

    let time_constant = dist_halflife / LN_2;

    let speed_at_start = start_dist / time_constant * (-dt / time_constant).exp();

    let time_until_transition = time_constant * (start_dist / (min_speed * time_constant)).ln();

    if time_until_transition > dt {
        return exponential_approach(start_pos, target_pos, dt, dist_halflife);
    }

    let linear_start_pos;
    let linear_dt;
    if time_until_transition < 0.0 {
        linear_start_pos = start_pos;
        linear_dt = dt;
    } else {
        linear_dt = dt - time_until_transition;
        let movement_direction = delta_pos.normalized();
        let dist_that_decay_reaches_min_speed = min_speed * time_constant;
        linear_start_pos =
            target_pos.sub(movement_direction.mul(dist_that_decay_reaches_min_speed));
    }

    linear_approach(linear_start_pos, target_pos, linear_dt, min_speed)
}

pub fn rect_corner_by_quadrant<T: num::Zero + Copy>(
    max_corner: [T; 2],
    nth_quadrant: i32,
) -> [T; 2] {
    let [w, h] = max_corner;
    match nth_quadrant.rem_euclid(4) {
        0 => [w, h],
        1 => [T::zero(), h],
        2 => [T::zero(), T::zero()],
        3 => [w, T::zero()],
        _ => unreachable!("rem_euclid fail"),
    }
}
pub fn rect_border(
    bottom_left: geometry2::IPoint,
    width_height: geometry2::IPoint,
) -> impl Iterator<Item = geometry2::IPoint> {
    let [w, h] = width_height;
    let [x1,y1] = width_height.sub([1,1]);
    (0..x1)
        .map(move |dx| [dx, 0])
        .chain((0..y1).map(move |dy| [x1, dy]))
        .chain((0..x1).map(move |dx| [x1 - dx, y1]))
        .chain((0..y1).map(move |dy| [0, y1 - dy]))
        .map(move |x| bottom_left.add(x))
}
pub fn rect_squares(
    bottom_left: geometry2::IPoint,
    width_height: geometry2::IPoint,
) -> impl Iterator<Item = geometry2::IPoint> {
    let [w, h] = width_height;
    let [x0, y0] = bottom_left;
    (0..w).map(move |x| (0..h).map(move |y| [x0 + x, y0 + y])).flatten()
}

#[cfg(test)]
mod tests {
    use std::f32;

    use ntest::{assert_about_eq, assert_false};
    use num::traits::real::Real;
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
    fn test_line_intersections_with_square_are_in_same_order_as_input_line() {
        let input_line: Line<f32, SquareGridInWorldFrame> =
            Line::new(point2(-1.5, -1.0), point2(0.0, 0.0));
        let output_points = input_line.line_intersections_with_centered_unit_square();
        let output_line = Line::new(output_points[0], output_points[1]);
        let in_vec = input_line.p2 - input_line.p1;
        let out_vec = output_line.p2 - output_line.p1;

        let same_direction = in_vec.dot(out_vec) > 0.0;
        assert!(same_direction);
    }

    #[test]
    fn test_line_intersections_with_square_are_in_same_order_as_input_line_vertical_line_on_left_edge(
    ) {
        let input_line: Line<f32, SquareGridInWorldFrame> =
            Line::new(point2(-0.5, -0.5), point2(-0.5, 0.5));
        let output_points = input_line.line_intersections_with_centered_unit_square();
        assert_eq!(input_line.p1, output_points[0]);
        assert_eq!(input_line.p2, output_points[1]);
    }

    #[test]
    fn test_same_side_of_line_vertical_line() {
        let line = Line::new(WorldPoint::new(-0.5, -0.5), point2(-0.5, 0.5));
        let origin = point2(0.0, 0.0);
        let neg_point = point2(-20.0, 0.0);
        assert_false!(line.same_side_of_line(neg_point, origin))
    }

    #[test]
    fn test_adjacent_king_steps() {
        assert_eq!(
            adjacent_king_steps(STEP_UP),
            vec![STEP_UP_RIGHT, STEP_UP_LEFT].into_iter().collect()
        );
        assert_eq!(
            adjacent_king_steps(STEP_RIGHT),
            vec![STEP_UP_RIGHT, STEP_DOWN_RIGHT].into_iter().collect()
        );
        assert_eq!(
            adjacent_king_steps(STEP_DOWN_LEFT),
            vec![STEP_DOWN, STEP_LEFT].into_iter().collect()
        );
    }

    #[test]
    fn test_rotate_zero_vector() {
        assert_eq!(
            rotate_vect(WorldMove::new(0.0, 0.0), Angle::radians(PI)),
            vec2(0.0, 0.0)
        );
    }

    #[test]
    fn test_half_plane_complementary_check_different_lines() {
        let line: Line<f32, SquareGridInWorldFrame> = Line::new(point2(0.0, 0.0), point2(1.0, 1.0));
        let line2: Line<f32, SquareGridInWorldFrame> =
            Line::new(point2(0.1, 0.0), point2(1.0, 1.0));
        let p1 = point2(0.0, 1.0);
        let p2 = point2(1.0, 0.0);

        let half_plane_1 = HalfPlane::from_line_and_point_on_half_plane(line, p1);
        let half_plane_2 = HalfPlane::from_line_and_point_on_half_plane(line, p2);
        let half_plane_3 = HalfPlane::from_line_and_point_on_half_plane(line2, p2);

        assert!(half_plane_1.is_about_complementary_to(half_plane_2, 1e-6));
        assert!(half_plane_2.is_about_complementary_to(half_plane_1, 1e-6));
        assert_false!(half_plane_1.is_about_complementary_to(half_plane_1, 1e-6));
        assert_false!(half_plane_1.is_about_complementary_to(half_plane_3, 1e-6));
        assert_false!(half_plane_2.is_about_complementary_to(half_plane_3, 1e-6));
    }

    #[test]
    fn test_half_plane_complementary_check_equivalent_lines() {
        let line: Line<f32, SquareGridInWorldFrame> = Line::new(point2(0.0, 0.0), point2(1.0, 1.0));
        let line2: Line<f32, SquareGridInWorldFrame> =
            Line::new(point2(2.0, 2.0), point2(5.0, 5.0));
        let p1 = point2(0.0, 1.0);
        let p2 = point2(1.0, 0.0);

        let half_plane_1 = HalfPlane::from_line_and_point_on_half_plane(line, p1);
        let half_plane_2 = HalfPlane::from_line_and_point_on_half_plane(line2, p2);

        assert!(half_plane_1.is_about_complementary_to(half_plane_2, 1e-6));
    }

    #[test]
    fn test_check_line_intersection_with_standard_square() {
        let line: WorldLine = Line::new(point2(5.0, 5.0), point2(4.0, 5.0));
        assert_false!(line.line_intersects_with_centered_unit_square());
    }

    #[test]
    fn test_angle_from_x_axis() {
        assert_about_eq!(
            better_angle_from_x_axis(default::Vector2D::new(0.5, 0.5)).to_degrees(),
            45.0
        );
        assert_about_eq!(
            better_angle_from_x_axis(default::Vector2D::new(0.0, 0.5)).to_degrees(),
            90.0
        );
        assert_about_eq!(
            better_angle_from_x_axis(default::Vector2D::new(0.0, -0.5)).to_degrees(),
            -90.0
        );
        assert_about_eq!(
            better_angle_from_x_axis(default::Vector2D::new(1.0, 0.0)).to_degrees(),
            0.0
        );
        assert_about_eq!(
            better_angle_from_x_axis(default::Vector2D::new(-1.0, 0.0)).to_degrees(),
            180.0
        );
    }

    #[test]
    fn test_built_in_angle_from_x_axis_can_not_be_trusted() {
        assert!(
            (default::Vector2D::new(0.5, 0.5)
                .angle_from_x_axis()
                .to_degrees()
                - 45.0)
                .abs()
                > 0.01
        );
    }

    #[test]
    fn test_standardize_angle() {
        assert_about_eq!(
            standardize_angle(Angle::<f32>::degrees(75.0)).radians,
            standardize_angle(Angle::<f32>::degrees(75.0 - 360.0)).radians
        );
    }

    #[test]
    fn test_line_intersections_observed_3_intersections() {
        Line::new(
            WorldPoint::new(-29.5, 5.0),
            WorldPoint::new(-27.589872, 4.703601),
        )
        .line_intersections_with_centered_unit_square();
    }

    #[test]
    fn test_revolve_square() {
        assert_eq!(
            revolve_square(
                point2(3, 4),
                point2(5, 5),
                QuarterTurnsAnticlockwise::new(3),
            ),
            point2(4, 7)
        );
    }

    #[test]
    fn test_quarter_turns_from_vectors() {
        assert_eq!(
            QuarterTurnsAnticlockwise::from_start_and_end_directions(STEP_UP, STEP_UP),
            QuarterTurnsAnticlockwise::new(0)
        );
        assert_eq!(
            QuarterTurnsAnticlockwise::from_start_and_end_directions(STEP_UP, STEP_RIGHT),
            QuarterTurnsAnticlockwise::new(3)
        );
        assert_eq!(
            QuarterTurnsAnticlockwise::from_start_and_end_directions(STEP_LEFT, STEP_RIGHT),
            QuarterTurnsAnticlockwise::new(2)
        );
        assert_eq!(
            QuarterTurnsAnticlockwise::from_start_and_end_directions(
                STEP_DOWN_LEFT,
                STEP_DOWN_RIGHT,
            ),
            QuarterTurnsAnticlockwise::new(1)
        );
    }

    #[test]
    fn test_step_back_pose() {
        let pose = SquareWithOrthogonalDir::from_square_and_step(point2(4, 6), STEP_RIGHT.into());
        let back = SquareWithOrthogonalDir::from_square_and_step(point2(3, 6), STEP_RIGHT.into());
        assert_eq!(pose.stepped_back(), back);
    }

    #[test]
    fn test_step_or_turn_pose() {
        let p = SquareWithOrthogonalDir::from_square_and_step;
        let s = point2(5, 5);
        assert_eq!(
            p(s, STEP_RIGHT.into()).stepped(),
            p(s + STEP_RIGHT, STEP_RIGHT.into())
        );
        assert_eq!(
            p(s, STEP_UP.into()).stepped(),
            p(s + STEP_UP, STEP_UP.into())
        );
        assert_eq!(
            p(s, STEP_DOWN.into()).strafed_left(),
            p(s + STEP_RIGHT, STEP_DOWN.into())
        );
        assert_eq!(
            p(s, STEP_LEFT.into()).strafed_right(),
            p(s + STEP_UP, STEP_LEFT.into())
        );
        assert_eq!(p(s, STEP_LEFT.into()).turned_left(), p(s, STEP_DOWN.into()));
        assert_eq!(p(s, STEP_LEFT.into()).turned_right(), p(s, STEP_UP.into()));
        assert_eq!(
            p(s, STEP_LEFT.into()).turned_back(),
            p(s, STEP_RIGHT.into())
        );
    }

    #[test]
    fn test_line_point_reflection() {
        let line = Line::new(WorldPoint::new(1.0, 5.0), WorldPoint::new(2.4, 5.0));

        assert_about_eq!(
            line.reflect_point_over_line(point2(0.0, 3.0)).to_array(),
            WorldPoint::new(0.0, 7.0).to_array()
        );
        assert_ne!(
            line.reflect_point_over_line(point2(0.0, 3.0)).to_array(),
            WorldPoint::new(0.0, 8.0).to_array()
        );
    }

    #[test]
    fn test_half_plane_cover_unit_square() {
        let [exactly_cover, less_than_cover, more_than_cover]: [HalfPlane<_, _>; 3] =
            [0.0, 0.01, -0.01].map(|dx| {
                HalfPlane::from_line_and_point_on_half_plane(
                    Line::new(
                        WorldPoint::new(-0.5 + dx, 0.0),
                        point2(-0.5 + 2.0 * dx, 1.0),
                    ),
                    point2(1.5, 0.0),
                )
            });

        assert!(more_than_cover.fully_covers_unit_square());
        assert_false!(less_than_cover.fully_covers_unit_square());
        assert!(exactly_cover.fully_covers_unit_square());
    }

    #[test]
    fn test_same_side_of_line() {
        let line = Line::<_, WorldPoint>::new(point2(1.0, 1.0), point2(2.0, 1.0));
        let low = point2(0.0, 0.0);
        let low2 = point2(9.0, 0.3);
        let high = point2(0.0, 10.0);
        let high2 = point2(5.0, 10.0);
        let on = point2(0.0, 1.0);
        let on2 = point2(5.0, 1.0);

        assert!(line.same_side_of_line(low, low2));

        assert!(line.same_side_of_line(high, high2));
        assert!(line.same_side_of_line(high2, high));

        assert!(line.same_side_of_line(on, on2));
        assert!(line.same_side_of_line(on2, on));

        assert_false!(line.same_side_of_line(low, on2));
        assert_false!(line.same_side_of_line(high, on));
        assert_false!(line.same_side_of_line(low, high2));
    }

    #[test]
    fn test_halfplane_covers_expanded_unit_square() {
        let the_plane = HalfPlane::from_line_and_point_on_half_plane(
            Line::new(WorldPoint::new(1.0, 5.0), point2(1.0, 6.0)),
            point2(-5.0, 0.0),
        );
        assert!(the_plane.fully_covers_expanded_unit_square(0.0));
        assert!(the_plane.fully_covers_expanded_unit_square(0.49));
        assert_false!(the_plane.fully_covers_expanded_unit_square(0.51));
        assert_false!(the_plane.fully_covers_expanded_unit_square(100.0));
    }

    #[test]
    fn test_squares_on_board() {
        let size = BoardSize::new(5, 40);
        let squares = squares_on_board(size);
        assert!(squares.contains(&point2(4, 20)));
        assert_false!(squares.contains(&point2(14, 2)));
    }

    #[test]
    fn test_horizontal_line_intersection_with_square() {
        let input_line: Line<f32, SquareGridInWorldFrame> =
            Line::new(point2(0.5, 0.0), point2(-1.5, 0.0));
        let output_points = input_line.line_intersections_with_centered_unit_square();
        assert_eq!(output_points, vec![point2(0.5, 0.0), point2(-0.5, 0.0)]);
    }

    #[test]
    fn test_vertical_line_intersection_with_square() {
        let input_line: Line<f32, SquareGridInWorldFrame> =
            Line::new(point2(0.0, 0.5), point2(0.0, -1.5));
        let output_points = input_line.line_intersections_with_centered_unit_square();
        assert_eq!(output_points, vec![point2(0.0, 0.5), point2(0.0, -0.5)]);
    }

    #[test]
    fn test_depth_of_point_in_half_plane() {
        let horizontal = HalfPlane::from_line_and_point_on_half_plane(
            Line::new(WorldPoint::new(0.0, 0.0), point2(1.0, 0.0)),
            point2(0.0, 5.0),
        );

        assert_about_eq!(
            horizontal.depth_of_point_in_half_plane(point2(0.0, 12.0)),
            12.0
        );
        assert_about_eq!(
            horizontal
                .extended(3.0)
                .depth_of_point_in_half_plane(point2(2000.0, 10.0)),
            13.0
        );
        assert_about_eq!(
            horizontal.depth_of_point_in_half_plane(point2(0.0, -1.0)),
            -1.0
        );

        let diag = HalfPlane::from_line_and_point_on_half_plane(
            Line::new(WorldPoint::new(0.0, 0.0), point2(-1.0, 1.0)),
            point2(0.0, 5.0),
        );
        assert_about_eq!(
            diag.depth_of_point_in_half_plane(point2(1.0, 0.0)),
            1.0 / 2.0.sqrt()
        );
    }
    #[test]
    fn test_looping_clamp() {
        assert_about_eq!(looping_clamp(0.0, 5.0, 3.0), 3.0); // in range
        assert_about_eq!(looping_clamp(0.0, 5.0, 5.1), 0.1); // above
        assert_about_eq!(looping_clamp(0.0, 5.0, -0.1), 4.9); // below
        assert_about_eq!(looping_clamp(-1.0, 5.0, 11.1), -0.9); // multiple periods above
        assert_about_eq!(looping_clamp(-1.5, 1.5, 2.0), -1.0); // fraction above
    }

    #[test]
    fn test_ray_hit_face_simple() {
        let start_point = point2(5.0, 5.0);
        let degrees = 90;
        let range = 5.0;
        let face = (point2(5, 6), STEP_UP).into();

        let result = does_ray_hit_oriented_square_face(
            start_point,
            Angle::degrees(degrees as f32),
            range,
            face,
        );

        assert!(result);
    }

    #[test]
    fn test_ray_hit_face_face_must_face_ray() {
        let start_point = point2(5.0, 5.0);
        let degrees = 90;
        let range = 5.0;
        let face = (point2(5, 6), STEP_DOWN).into();

        let result = does_ray_hit_oriented_square_face(
            start_point,
            Angle::degrees(degrees as f32),
            range,
            face,
        );

        assert_false!(result);
    }
    #[test]
    fn test_ray_hit_face_miss() {
        let start_point = point2(5.0, 5.0);
        let degrees = 90;
        let range = 5.0;
        let face = (point2(6, 6), STEP_UP).into();

        let result = does_ray_hit_oriented_square_face(
            start_point,
            Angle::degrees(degrees as f32),
            range,
            face,
        );

        assert_false!(result);
    }
    #[test]
    fn test_ray_hit_face_under_ranged() {
        let start_point = point2(5.0, 5.0);
        let degrees = 90;
        let range = 1.49;
        let face = (point2(5, 6), STEP_UP).into();

        let result = does_ray_hit_oriented_square_face(
            start_point,
            Angle::degrees(degrees as f32),
            range,
            face,
        );

        assert_false!(result);
    }
    #[test]
    fn test_ray_hit_face_just_within_range() {
        let start_point = point2(5.0, 5.0);
        let degrees = 90;
        let range = 01.501;
        let face = (point2(5, 6), STEP_UP).into();

        let result = does_ray_hit_oriented_square_face(
            start_point,
            Angle::degrees(degrees as f32),
            range,
            face,
        );

        assert!(result);
    }
    #[test]
    fn test_ray_hit_face_just_out_of_closer_range() {
        let start_point = point2(5.0, 5.49);
        let degrees = 90;
        let range = 1.0;
        let face = (point2(5, 6), STEP_UP).into();

        let result = does_ray_hit_oriented_square_face(
            start_point,
            Angle::degrees(degrees as f32),
            range,
            face,
        );

        assert_false!(result);
    }
    #[test]
    fn test_ray_hit_face_just_within_closer_range() {
        let start_point = point2(5.0, 5.49);
        let degrees = 90;
        let range = 1.02;
        let face = (point2(5, 6), STEP_UP).into();

        let result = does_ray_hit_oriented_square_face(
            start_point,
            Angle::degrees(degrees as f32),
            range,
            face,
        );

        assert!(result);
    }
    #[test]
    fn test_ray_hit_face_just_out_of_really_close_range() {
        let start_point = point2(5.0, 6.49);
        let degrees = 90;
        let range = 0.001;
        let face = (point2(5, 6), STEP_UP).into();

        let result = does_ray_hit_oriented_square_face(
            start_point,
            Angle::degrees(degrees as f32),
            range,
            face,
        );

        assert_false!(result);
    }
    #[test]
    fn test_ray_hit_face_just_within_really_close_range() {
        assert!(does_ray_hit_oriented_square_face(
            point2(5.0, 6.49),
            Angle::degrees(90.0),
            0.02,
            (point2(5, 6), STEP_UP).into(),
        ));
    }
    #[test]
    fn test_ray_hit_face_angled_miss() {
        assert_false!(does_ray_hit_oriented_square_face(
            point2(5.0, 5.49),
            Angle::degrees(45.0),
            5.0,
            (point2(5, 6), STEP_UP).into(),
        ));
    }
    #[test]
    fn test_ray_hit_face_angled_hit() {
        assert!(does_ray_hit_oriented_square_face(
            point2(5.0, 5.49),
            Angle::degrees(45.0),
            5.0,
            (point2(5, 6), STEP_RIGHT).into(),
        ));
    }
    #[test]
    fn test_ray_hit_face_just_barely_touching_still_counts() {
        assert!(does_ray_hit_oriented_square_face(
            point2(5.5, 5.0),
            Angle::degrees(90.0),
            5.5,
            (point2(5, 10), STEP_UP).into(),
        ));
    }
    #[test]
    fn test_ray_hit_face_parallel_hit_does_not_count() {
        assert_false!(does_ray_hit_oriented_square_face(
            point2(5.0, 5.5),
            Angle::degrees(0.0),
            5.0,
            (point2(7, 5), STEP_UP).into(),
        ));
    }
    #[test]
    fn test_line_line_intersection_easy_orthogonal_hit() {
        assert_about_eq_2d(
            WorldLine::new(point2(0.0, 0.0), point2(0.0, 4.0))
                .intersection_point_with_other_line(&WorldLine::new(
                    point2(-1.0, 1.0),
                    point2(1.0, 1.0),
                ))
                .unwrap(),
            point2(0.0, 1.0),
        )
    }
    #[test]
    fn test_line_line_intersection_diagonal_intersection() {
        assert_about_eq_2d(
            WorldLine::new(point2(0.0, 0.0), point2(1.0, 1.0))
                .intersection_point_with_other_line(&WorldLine::new(
                    point2(1.0, 0.0),
                    point2(0.0, 1.0),
                ))
                .unwrap(),
            point2(0.5, 0.5),
        )
    }
    #[test]
    fn test_line_line_intersection_miss() {
        assert!(WorldLine::new(point2(0.0, 0.0), point2(1.0, 1.0))
            .intersection_point_with_other_line(&WorldLine::new(
                point2(100.0, 1000.0),
                point2(10.0, 10.0),
            ))
            .is_none())
    }
    #[test]
    fn test_line_line_intersection_endpoint_touch_mid_counts() {
        assert_about_eq_2d(
            WorldLine::new(point2(5.0, 5.0), point2(7.0, 5.0))
                .intersection_point_with_other_line(&WorldLine::new(
                    point2(5.5, 5.0),
                    point2(10.0, 10.0),
                ))
                .unwrap(),
            point2(5.5, 5.0),
        )
    }
    #[test]
    fn test_line_line_intersection_perpendicular_endpoints_touch() {
        assert_about_eq_2d(
            WorldLine::new(point2(5.0, 5.0), point2(10.0, 5.0))
                .intersection_point_with_other_line(&WorldLine::new(
                    point2(10.0, 5.0),
                    point2(10.0, 10.0),
                ))
                .unwrap(),
            point2(10.0, 5.0),
        )
    }
    #[test]
    fn test_line_line_intersection_parallel_endpoints_touch() {
        let line1 = WorldLine::new(point2(5.0, 5.0), point2(10.0, 5.0));
        let line2 = WorldLine::new(point2(10.0, 5.0), point2(20.0, 5.0));
        assert_about_eq_2d(
            line1.intersection_point_with_other_line(&line2).unwrap(),
            point2(10.0, 5.0),
        );
        assert_about_eq_2d(
            line1
                .reversed()
                .intersection_point_with_other_line(&line2)
                .unwrap(),
            point2(10.0, 5.0),
        );
        assert_about_eq_2d(
            line1
                .intersection_point_with_other_line(&line2.reversed())
                .unwrap(),
            point2(10.0, 5.0),
        );
        assert_about_eq_2d(
            line1
                .reversed()
                .intersection_point_with_other_line(&line2.reversed())
                .unwrap(),
            point2(10.0, 5.0),
        );
    }
    #[test]
    fn test_line_line_intersection_parallel_miss() {
        assert!(WorldLine::new(point2(5.0, 5.0), point2(10.0, 5.0))
            .intersection_point_with_other_line(&WorldLine::new(
                point2(11.0, 5.0),
                point2(20.0, 5.0),
            ))
            .is_none(),)
    }
    #[test]
    fn test_line_line_intersection_parallel_overlap_does_not_count() {
        assert!(WorldLine::new(point2(5.0, 5.0), point2(10.0, 5.0))
            .intersection_point_with_other_line(&WorldLine::new(
                point2(9.0, 5.0),
                point2(20.0, 5.0),
            ))
            .is_none(),)
    }
    #[test]
    fn test_line_line_intersection_parallel_full_overlap_does_not_count() {
        assert!(WorldLine::new(point2(5.0, 5.0), point2(10.0, 5.0))
            .intersection_point_with_other_line(&WorldLine::new(
                point2(0.0, 5.0),
                point2(20.0, 5.0),
            ))
            .is_none(),)
    }
    #[test]
    fn test_line_line_intersection_parallel_exact_overlap_does_not_count() {
        assert!(WorldLine::new(point2(5.0, 5.0), point2(10.0, 5.0))
            .intersection_point_with_other_line(&WorldLine::new(
                point2(5.0, 5.0),
                point2(10.0, 5.0),
            ))
            .is_none(),)
    }
    #[test]
    fn test_first_inside_square_face_hit_by_ray_simple_case() {
        let inside_faces = HashSet::from([
            (point2(5, 6), STEP_UP).into(),
            (point2(5, 7), STEP_DOWN).into(),
            (point2(5, 7), STEP_UP).into(),
        ]);
        let result = first_inside_square_face_hit_by_ray(
            point2(5.0, 5.0),
            Angle::degrees(90.0),
            20.0,
            &inside_faces,
        );
        assert_eq!(result.unwrap().0, (point2(5, 6), STEP_UP).into());
        assert_about_eq_2d(result.unwrap().1, point2(5.0, 6.5));
    }
    #[test]
    fn test_exponential_approach_with_min_speed() {
        assert_about_eq!(
            exponential_approach_with_min_speed([0.0, 0.0], [10.0, 0.0], 1.0, 1.0, 0.0),
            [5.0, 0.0]
        );
        assert_about_eq!(
            exponential_approach_with_min_speed([0.0, 0.0], [10.0, 0.0], 1.0, f32::INFINITY, 5.0),
            [5.0, 0.0]
        );

        assert_about_eq!(
            exponential_approach_with_min_speed([0.0, 0.0], [0.0, 100.0], 1.0, 0.01, 5.0),
            [0.0, 100.0]
        );
        assert_about_eq!(
            exponential_approach_with_min_speed([0.0, 0.0], [0.0, 100.0], 1.0, 5.0, 999.0),
            [0.0, 100.0]
        );
    }
    #[test]
    fn test_rect_border() {
        assert_eq!(
            rect_border([4, 5], [3, 2]).collect_vec(),
            vec![[4, 5], [5, 5], [6, 5], [6, 6], [5, 6], [4, 6],]
        );
    }
    #[test]
    fn test_rect_fill() {
        let got = HashSet::from_iter(rect_squares([4, 5], [3, 3]));
        let correct = HashSet::from( [
            [4, 5], [5, 5], [6, 5],
            [4, 6], [5, 6], [6, 6],
            [4, 7], [5, 7], [6, 7],
        ]);
        assert_eq!(
            got, correct
        );
    }
}
