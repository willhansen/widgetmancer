use std::ops::Add;

use euclid::approxeq::ApproxEq;
use line_drawing::Supercover;
use num::{traits::float::FloatCore, NumCast, One, Signed, Zero};
use rand::{rngs::StdRng, Rng};

use crate::utility::*;

pub type TwoDifferentWorldPoints = TwoDifferentPoints<WorldPoint>;
pub type TwoDifferentWorldSquares = TwoDifferentPoints<WorldSquare>;
pub type TwoDifferentLocalSquarePoints =
    TwoDifferentPoints<Point2D<f32, SquareGridInLocalSquareFrame>>;
pub type TwoDifferentLocalCharacterPoints =
    TwoDifferentPoints<Point2D<f32, CharacterGridInLocalCharacterFrame>>;
// TODO: put in some kind of default module like euclid does?
pub type TwoDifferentFloatPoints = TwoDifferentPoints<Point2D<f32, euclid::UnknownUnit>>;

pub trait LineTrait: Sized + Copy + QuarterTurnRotatable {
    type PointType: SignedCoordinate;
    // type DataType = <Self::PointType as Coordinate>::DataType;
    fn new_from_two_points(p1: impl Into<Self::PointType>, p2: impl Into<Self::PointType>) -> Self;

    fn two_different_arbitrary_points_on_line(&self) -> [Self::PointType; 2];

    fn arbitrary_point_on_line(&self) -> Self::PointType {
        self.two_different_arbitrary_points_on_line()[0]
    }

    fn from_other_line<OtherLine>(other_line: OtherLine) -> Self
    where
        OtherLine: LineTrait<PointType = Self::PointType>,
    {
        let [p1, p2] = other_line.two_different_arbitrary_points_on_line();
        Self::new_from_two_points(p1, p2)
    }

    fn new_horizontal(y: <Self::PointType as Coordinate>::DataType) -> Self {
        Self::new_from_two_points(
            Self::PointType::new(<Self::PointType as Coordinate>::DataType::zero(), y),
            Self::PointType::new(<Self::PointType as Coordinate>::DataType::one(), y),
        )
    }
    fn new_vertical(x: <Self::PointType as Coordinate>::DataType) -> Self {
        Self::new_from_two_points(
            Self::PointType::new(x, <Self::PointType as Coordinate>::DataType::zero()),
            Self::PointType::new(x, <Self::PointType as Coordinate>::DataType::one()),
        )
    }
    fn new_through_origin(second_point: impl Into<Self::PointType>) -> Self {
        Self::new_from_two_points(
            Self::PointType::new(
                <Self::PointType as Coordinate>::DataType::zero(),
                <Self::PointType as Coordinate>::DataType::zero(),
            ),
            second_point,
        )
    }
    fn from_point_and_direction(
        point: impl Into<Self::PointType>,
        direction: impl Into<Self::PointType>,
    ) -> Self {
        let p1 = point.into();
        let v = direction.into();
        let p2 = p1 + v;
        Self::new_from_two_points(p1, p2)
    }
    fn is_orthogonal(&self) -> bool {
        let [p1, p2] = self.two_different_arbitrary_points_on_line();
        p1.x() == p2.x() || p1.y() == p2.y()
    }
    fn from_array(a: [Self::PointType; 2]) -> Self {
        Self::new_from_two_points(a[0], a[1])
    }
    fn x_intercept(&self) -> Option<f32> {
        if self.is_vertical() {
            let p = self.arbitrary_point_on_line();
            return Some(p.to_f32().x());
        }
        if self.is_horizontal() {
            return None;
        }
        Some(-self.y_intercept().unwrap() / self.slope().unwrap())
    }
    fn y_intercept(&self) -> Option<f32> {
        if self.is_vertical() {
            return None;
        }
        let p = self.arbitrary_point_on_line().to_f32();
        Some(p.y() - self.slope().unwrap() * p.x())
    }
    fn is_vertical(&self) -> bool {
        let [p1, p2] = self.two_different_arbitrary_points_on_line();
        p1.x() == p2.x()
    }
    fn is_horizontal(&self) -> bool {
        let [p1, p2] = self.two_different_arbitrary_points_on_line();
        p1.y() == p2.y()
    }
    fn slope(&self) -> Option<f32> {
        if self.is_vertical() {
            return None;
        }
        let [p1, p2] = self
            .two_different_arbitrary_points_on_line()
            .map(|a| a.to_f32());
        let (l, r) = if p1.x() < p2.x() { (p1, p2) } else { (p2, p1) };
        Some((r.y() - l.y()) / (r.x() - l.x()))
    }
}

pub trait FloatLineTrait: LineTrait<PointType = Self::_PointType> {
    type _PointType: FloatCoordinate; // Dummy type to allow for trait bound propagation

    fn point_is_on_line(&self, point: impl Into<Self::PointType>) -> bool {
        let [p1, p2] = self.two_different_arbitrary_points_on_line();
        on_line(p1, p2, point.into())
    }
    fn point_is_approx_on_line(&self, point: Self::PointType, tolerance: f32) -> bool {
        self.normal_distance_to_point(point) < tolerance
    }
    fn closest_point_on_extended_line_to_point(
        &self,
        point: impl Into<Self::PointType>,
    ) -> Self::PointType {
        let point = point.into();
        let [p1, p2] = self.two_different_arbitrary_points_on_line();
        let p1_to_point = point - p1;
        let p1_to_p2 = p2 - p1;
        let parallel_part_of_p1_to_point = p1_to_point.projected_onto(p1_to_p2);
        p1 + parallel_part_of_p1_to_point
    }
    fn normal_vector_to_point(&self, point: impl Into<Self::PointType>) -> Self::PointType {
        let point = point.into();
        point - self.closest_point_on_extended_line_to_point(point)
    }
    fn normal_vector_from_origin(&self) -> Self::PointType {
        -self.normal_vector_to_point((0.0, 0.0))
    }
    fn normal_distance_to_point(&self, point: impl Into<Self::PointType>) -> f32 {
        self.normal_vector_to_point(point).length()
    }
    fn point_is_on_or_normal_to_line_segment(&self, point: Self::PointType) -> bool {
        let [start_point, end_point] = self.two_different_arbitrary_points_on_line();

        let point_relative_to_start_point = point - start_point;
        let end_point_relative_to_start_point = end_point - start_point;
        let point_is_on_end_side_of_start_point =
            point_relative_to_start_point.dot(end_point_relative_to_start_point) > 0.0;

        let point_relative_to_end_point = point - end_point;
        let point_is_on_start_side_of_end_point =
            point_relative_to_end_point.dot(-end_point_relative_to_start_point) > 0.0;

        point_is_on_end_side_of_start_point && point_is_on_start_side_of_end_point
    }

    fn approx_on_same_line(&self, other: Self, tolerance: f32) -> bool {
        let [p1, p2] = self.two_different_arbitrary_points_on_line();
        self.point_is_approx_on_line(p1, tolerance) && self.point_is_approx_on_line(p2, tolerance)
    }

    fn angle_with_positive_x_axis(&self) -> Angle<f32> {
        let [p1, p2] = self.two_different_arbitrary_points_on_line();
        let angle_a = (p1 - p2).better_angle_from_x_axis();
        let angle_b = (p2 - p1).better_angle_from_x_axis();
        if angle_a.radians.cos() < 0.0 {
            angle_b
        } else {
            angle_a
        }
    }

    fn reflect_point_over_line(&self, point: impl Into<Self::PointType>) -> Self::PointType {
        let [p1, p2] = self.two_different_arbitrary_points_on_line();
        let p1_to_p = point.into() - p1;
        let p1_to_p2 = p2 - p1;
        let parallel_part = p1_to_p.projected_onto(p1_to_p2);
        let perpendicular_part = p1_to_p - parallel_part;
        let p1_to_reflected_p = parallel_part - perpendicular_part;
        p1 + p1_to_reflected_p
    }

    fn parallel_directions(&self) -> [Angle<f32>; 2] {
        let [p1, p2] = self.two_different_arbitrary_points_on_line();
        [
            (p2 - p1).better_angle_from_x_axis(),
            (p1 - p2).better_angle_from_x_axis(),
        ]
    }
    fn from_ray(start: Self::PointType, angle: Angle<f32>, length: f32) -> Self {
        assert!(length > 0.0);
        Self::new_from_two_points(start, naive_ray_endpoint(start, angle, length))
    }
    fn same_side_of_line(
        &self,
        point_c: impl Into<Self::PointType> + Copy,
        point_d: impl Into<Self::PointType> + Copy,
    ) -> bool {
        let [p1, p2] = self.two_different_arbitrary_points_on_line();
        let point_a = p1;
        let point_b = p2;
        let c_on_line = self.point_is_on_line(point_c);
        let d_on_line = self.point_is_on_line(point_d);

        if c_on_line {
            return if d_on_line { true } else { false };
        }
        if d_on_line {
            return false;
        }

        three_points_are_clockwise(point_a, point_b, point_c.into())
            == three_points_are_clockwise(point_a, point_b, point_d.into())
    }
    fn unordered_line_intersections_with_centered_unit_square_with_tolerance(
        &self,
        tolerance: f32,
    ) -> Vec<Self::PointType> {
        let regular_intersections = self.unordered_line_intersections_with_centered_unit_square();
        if !regular_intersections.is_empty() {
            return regular_intersections;
        }

        let square_corners_and_distances = corner_points_of_centered_unit_square()
            .iter()
            .map(|&p| (p, self.normal_distance_to_point(p)))
            .collect_vec();
        let filtered_by_distance = square_corners_and_distances
            .iter()
            .filter(|(point, dist)| *dist <= tolerance)
            .collect_vec();

        if filtered_by_distance.is_empty() {
            return vec![];
        }

        let grouped_by_distance = filtered_by_distance
            .iter()
            .sorted_by_key(|(point, distance)| OrderedFloat(*distance))
            .group_by(|(point, distance)| distance);
        let closest_points = grouped_by_distance
            .into_iter()
            .next()
            .unwrap()
            .1
            .map(|(point, distance)| *point)
            .collect_vec();
        closest_points
    }
    fn unordered_line_intersections_with_expanded_centered_unit_square(
        &self,
        expansion_length: f32,
    ) -> Vec<Self::PointType> {
        let [line_point_a, line_point_b] = self.two_different_arbitrary_points_on_line();
        let half_side_length = 0.5 + expansion_length;

        let is_vertical_line = line_point_a.x() == line_point_b.x();
        let is_horizontal_line = line_point_a.y() == line_point_b.y();

        if is_vertical_line {
            let x = line_point_a.x();
            if x.abs() <= half_side_length {
                vec![(x, half_side_length).into(), (x, -half_side_length).into()]
            } else {
                vec![]
            }
        } else if is_horizontal_line {
            let y = line_point_a.y();
            if y.abs() <= half_side_length {
                vec![(half_side_length, y).into(), (-half_side_length, y).into()]
            } else {
                vec![]
            }
        } else {
            // y = mx + b
            let dy = line_point_b.y() - line_point_a.y();
            let dx = line_point_b.x() - line_point_a.x();
            let m = dy / dx;
            // b = y - m*x
            let b = line_point_a.y() - m * line_point_a.x();

            let side_positions = vec![half_side_length, -half_side_length];

            let mut candidate_intersections: Vec<Self::PointType> = vec![];
            for &x in &side_positions {
                let y = m * x + b;
                if y.abs() <= half_side_length {
                    candidate_intersections.push((x, y).into());
                }
            }
            for y in side_positions {
                let x = (y - b) / m;
                // top and bottom don't catch corners, sides do
                if x.abs() < half_side_length {
                    candidate_intersections.push((x, y).into());
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
                _ => furthest_apart_points(candidate_intersections).into(),
            }
        }
    }
    fn unordered_line_intersections_with_centered_unit_square(&self) -> Vec<Self::PointType> {
        self.unordered_line_intersections_with_expanded_centered_unit_square(0.0)
    }
    fn line_intersects_with_centered_unit_square(&self) -> bool {
        self.intersects_with_expanded_centered_unit_square(0.0)
    }
    fn intersects_with_expanded_centered_unit_square(&self, per_face_extension: f32) -> bool {
        !self
            .unordered_line_intersections_with_expanded_centered_unit_square(per_face_extension)
            .is_empty()
    }

    fn intersection_point_with_other_extended_line(&self, other: &Self) -> Option<Self::PointType> {
        let [self_p1, self_p2] = self.two_different_arbitrary_points_on_line();
        let [other_p1, other_p2] = other.two_different_arbitrary_points_on_line();

        // Equation from https://en.wikipedia.org/wiki/Line%E2%80%93line_intersection
        let (x1, y1) = self_p1.tuple();
        let (x2, y2) = self_p2.tuple();
        let (x3, y3) = other_p1.tuple();
        let (x4, y4) = other_p2.tuple();

        let a = x1 * y2 - y1 * x2;
        let b = x3 * y4 - y3 * x4;
        let denominator = (x1 - x2) * (y3 - y4) - (y1 - y2) * (x3 - x4);
        if denominator == 0.0 {
            return None;
        }
        let final_x = (a * (x3 - x4) - (x1 - x2) * b) / denominator;
        let final_y = (a * (y3 - y4) - (y1 - y2) * b) / denominator;
        Some((final_x, final_y).into())
    }

    // TODO: make parameter impl LineSegment
    fn intersection_point_with_other_line_segment(&self, other: &Self) -> Option<Self::PointType> {
        let [self_p1, self_p2] = self.two_different_arbitrary_points_on_line();
        let [other_p1, other_p2] = other.two_different_arbitrary_points_on_line();

        if self.same_side_of_line(other_p1, other_p2) || other.same_side_of_line(self_p1, self_p2) {
            let on_same_line = self.point_is_on_line(other_p1);
            if !on_same_line {
                return None;
            }
            return if self_p2 == other_p1 && on_line_in_this_order(self_p1, self_p2, other_p2) {
                Some(self_p2)
            } else if self_p2 == other_p2 && on_line_in_this_order(self_p1, self_p2, other_p1) {
                Some(self_p2)
            } else if self_p1 == other_p1 && on_line_in_this_order(self_p2, self_p1, other_p2) {
                Some(self_p1)
            } else if self_p1 == other_p2 && on_line_in_this_order(self_p2, self_p1, other_p1) {
                Some(self_p1)
            } else {
                None
            };
        }
        // from here, we know the line segments are overlapping, including the case of exactly touching
        // A simple line intersection check is all that's left

        self.intersection_point_with_other_extended_line(&other)
    }
}
impl<L> FloatLineTrait for L
where
    L: LineTrait,
    L::PointType: FloatCoordinate,
{
    type _PointType = L::PointType; // Dummy type to allow for trait bound propagation
}

pub trait DirectedLineTrait: LineTrait {
    fn p1(&self) -> Self::PointType;
    fn p2(&self) -> Self::PointType;
    fn arbitrary_vector_along_line(&self) -> Self::PointType {
        self.p2() - self.p1()
    }
    fn get_point_by_index(&self, index: u32) -> Self::PointType {
        match index {
            0 => self.p1(),
            1 => self.p2(),
            _ => panic!("only two points defining the line"),
        }
    }
    fn from_other_directed_line<OtherLine>(other: OtherLine) -> Self
    where
        OtherLine: DirectedLineTrait<PointType = Self::PointType>,
    {
        Self::new_from_two_points(other.p1(), other.p2())
    }
    fn reversed(&self) -> Self {
        Self::new_from_two_points(self.p2(), self.p1())
    }
    fn to_array(&self) -> [Self::PointType; 2] {
        [self.p1(), self.p2()]
    }
    fn arbitrary_point_clockwise_of_line(&self) -> Self::PointType {
        self.arbitrary_point_on_line() + self.arbitrary_vector_along_line().quarter_rotated_ccw(-1)
    }
    fn arbitrary_point_anticlockwise_of_line(&self) -> Self::PointType {
        self.arbitrary_point_on_line() + self.arbitrary_vector_along_line().quarter_rotated_ccw(1)
    }
    fn arbitrary_point_right_of_line(&self) -> Self::PointType {
        self.arbitrary_point_clockwise_of_line()
    }
    fn arbitrary_point_left_of_line(&self) -> Self::PointType {
        self.arbitrary_point_anticlockwise_of_line()
    }
}

pub trait DirectedFloatLineTrait: FloatLineTrait + DirectedLineTrait {
    fn points_in_line_order(&self, mut points: Vec<Self::PointType>) -> Vec<Self::PointType> {
        let normalized_line_direction = (self.p2() - self.p1()).normalize();
        points.sort_by_key(|&point| OrderedFloat(normalized_line_direction.dot(point)));
        points
    }
    fn ordered_line_intersections_with_centered_unit_square_with_tolerance(
        &self,
        tolerance: f32,
    ) -> Vec<Self::PointType> {
        self.points_in_line_order(
            self.unordered_line_intersections_with_centered_unit_square_with_tolerance(tolerance),
        )
    }
    fn ordered_line_intersections_with_expanded_centered_unit_square(
        &self,
        expansion_length: f32,
    ) -> Vec<Self::PointType> {
        self.points_in_line_order(
            self.unordered_line_intersections_with_expanded_centered_unit_square(expansion_length),
        )
    }
    fn ordered_line_intersections_with_centered_unit_square(&self) -> Vec<Self::PointType> {
        self.ordered_line_intersections_with_expanded_centered_unit_square(0.0)
    }
    fn direction(&self) -> Angle<f32> {
        (self.p2() - self.p1()).better_angle_from_x_axis()
    }
}
impl<L> DirectedFloatLineTrait for L where L: DirectedLineTrait + FloatLineTrait {}

#[derive(Clone, Copy, PartialEq)]
pub struct TwoDifferentPoints<PointType: Coordinate> {
    p1: PointType,
    p2: PointType,
}

impl<P: Coordinate> TwoDifferentPoints<P> {
    pub fn new(p1: impl Into<P>, p2: impl Into<P>) -> Self {
        let p1 = p1.into();
        let p2 = p2.into();
        assert_ne!(p1, p2);
        TwoDifferentPoints { p1, p2 }
    }
    pub fn p1(&self) -> P {
        self.p1
    }
    pub fn p2(&self) -> P {
        self.p2
    }
    pub fn x_min(&self) -> P::DataType {
        min_for_partial_ord(self.p1.x(), self.p2.x())
    }
    pub fn x_max(&self) -> P::DataType {
        max_for_partial_ord(self.p1.x(), self.p2.x())
    }
    pub fn y_min(&self) -> P::DataType {
        min_for_partial_ord(self.p1.y(), self.p2.y())
    }
    pub fn y_max(&self) -> P::DataType {
        max_for_partial_ord(self.p1.y(), self.p2.y())
    }
    pub fn width(&self) -> P::DataType {
        self.x_max() - self.x_min()
    }
    pub fn height(&self) -> P::DataType {
        self.y_max() - self.y_min()
    }
}
#[derive(Clone, PartialEq, Debug)]
pub struct TwoDifferentPointsOnCenteredUnitSquare<P: FloatCoordinate>(TwoDifferentPoints<P>);
impl<P: FloatCoordinate> Copy for TwoDifferentPointsOnCenteredUnitSquare<P> {}
impl<P: FloatCoordinate> TwoDifferentPointsOnCenteredUnitSquare<P> {
    fn try_new(p1: impl Into<P>, p2: impl Into<P>) -> Option<Self> {
        let p1 = p1.into();
        let p2 = p2.into();
        let points_are_valid = p1.on_centered_unit_square() && p2.on_centered_unit_square();
        if points_are_valid {
            Some(Self::new(p1, p2))
        } else {
            None
        }
    }
    fn new(p1: impl Into<P>, p2: impl Into<P>) -> Self {
        let p1 = p1.into();
        let p2 = p2.into();
        assert!(p1.on_centered_unit_square());
        assert!(p2.on_centered_unit_square());
        Self(TwoDifferentPoints::new(p1, p2))
    }
}

impl<PointType: SignedCoordinate> LineTrait for TwoDifferentPoints<PointType> {
    type PointType = PointType;
    fn new_from_two_points(p1: impl Into<PointType>, p2: impl Into<PointType>) -> Self {
        TwoDifferentPoints::new(p1, p2)
    }
    fn two_different_arbitrary_points_on_line(&self) -> [PointType; 2] {
        [self.p2, self.p1] // order chosen by coin flip
    }
}
impl<PointType: SignedCoordinate> DirectedLineTrait for TwoDifferentPoints<PointType> {
    fn p1(&self) -> PointType {
        self.p1
    }
    fn p2(&self) -> PointType {
        self.p2
    }
}

impl<PointType: FloatCoordinate> LineTrait for TwoDifferentPointsOnCenteredUnitSquare<PointType> {
    type PointType = PointType;
    fn new_from_two_points(p1: impl Into<PointType>, p2: impl Into<PointType>) -> Self {
        Self::new(p1, p2)
    }

    fn two_different_arbitrary_points_on_line(&self) -> [PointType; 2] {
        self.0.two_different_arbitrary_points_on_line()
    }
}
impl<PointType: FloatCoordinate> DirectedLineTrait
    for TwoDifferentPointsOnCenteredUnitSquare<PointType>
{
    fn p1(&self) -> Self::PointType {
        self.0.p1()
    }

    fn p2(&self) -> Self::PointType {
        self.0.p2()
    }
}

macro_rules! make_point_grouping_rotatable {
    ($grouping_type:ident, $point_trait:ident) => {
        impl<PointType: $point_trait> QuarterTurnRotatable for $grouping_type<PointType> {
            fn quarter_rotated_ccw(
                &self,
                quarter_turns_ccw: impl Into<OrthogonalDirection>,
            ) -> Self {
                let quarter_turns_ccw = quarter_turns_ccw.into();
                let new_points = self
                    .to_array()
                    .map(|p| p.quarter_rotated_ccw(quarter_turns_ccw));
                Self::from_array(new_points)
            }
        }
    };
}

make_point_grouping_rotatable!(TwoDifferentPoints, SignedCoordinate);
make_point_grouping_rotatable!(TwoDifferentPointsOnCenteredUnitSquare, FloatCoordinate);

impl<PointType: SignedCoordinate, CanBePointType> From<(CanBePointType, CanBePointType)>
    for TwoDifferentPoints<PointType>
where
    CanBePointType: Into<PointType>,
{
    fn from(value: (CanBePointType, CanBePointType)) -> Self {
        Self::new_from_two_points(value.0, value.1)
    }
}

// TODO: Can generalize to any line from any line?
impl<P: FloatCoordinate> From<TwoDifferentPointsOnCenteredUnitSquare<P>> for TwoDifferentPoints<P> {
    fn from(value: TwoDifferentPointsOnCenteredUnitSquare<P>) -> Self {
        value.0
    }
}

impl<P: FloatCoordinate> TryFrom<TwoDifferentPoints<P>>
    for TwoDifferentPointsOnCenteredUnitSquare<P>
{
    type Error = ();

    fn try_from(value: TwoDifferentPoints<P>) -> Result<Self, Self::Error> {
        Self::try_new(value.p1, value.p2).ok_or(())
    }
}

// TODO: move to the FloatLineTrait trait
impl<PointType: FloatCoordinate> TwoDifferentPoints<PointType> {}
impl TwoDifferentWorldPoints {
    pub fn touched_squares(&self) -> Vec<WorldSquare> {
        let start_square = world_point_to_world_square(self.p1);
        let end_square = world_point_to_world_square(self.p2);
        // TODO: use better line algorithm.  Account for floating point start and ends
        line_drawing::WalkGrid::new(start_square.to_tuple(), end_square.to_tuple())
            .map(|(x, y)| point2(x, y))
            .collect_vec()
    }
}

impl<PointType: SignedCoordinate> Debug for TwoDifferentPoints<PointType> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "p1: {}, p2: {}\n\tx-intercept: {}\n\ty-intercept: {}\n\tslope: {}",
            self.p1.to_string(),
            self.p2.to_string(),
            self.x_intercept()
                .map_or("N/A".to_owned(), |v| v.to_string()),
            self.y_intercept()
                .map_or("N/A".to_owned(), |v| v.to_string()),
            self.slope().map_or("inf".to_owned(), |v| v.to_string()),
        )
    }
}
impl<PointType: SignedCoordinate> Display for TwoDifferentPoints<PointType> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        std::fmt::Debug::fmt(&self, f)
    }
}

impl<PointType> Add<PointType> for TwoDifferentPoints<PointType>
where
    PointType: Coordinate,
{
    type Output = TwoDifferentPoints<PointType>;

    fn add(self, rhs: PointType) -> Self::Output {
        TwoDifferentPoints {
            p1: self.p1 + rhs,
            p2: self.p2 + rhs,
        }
    }
}

pub struct Ray<U> {
    pub point: Point2D<f32, U>,
    pub angle: Angle<f32>,
}

pub fn first_inside_square_face_hit_by_ray(
    start: WorldPoint,
    angle: Angle<f32>,
    range: f32,
    inside_faces: &HashSet<WorldSquareWithOrthogonalDir>,
) -> Option<(WorldSquareWithOrthogonalDir, WorldPoint)> {
    let ray_direction: WorldMove = WorldMove::unit_vector_from_angle(angle);

    let inside_faces_facing_ray: HashSet<WorldSquareWithOrthogonalDir> = inside_faces
        .iter()
        .filter(|&&face| {
            let vector_into_face = face.direction();
            ray_direction.dot(vector_into_face.to_step()) >= 0.0
        })
        .cloned()
        .collect();

    let naive_end_point: WorldPoint =
        start + WorldMove::unit_vector_from_angle(angle).cast_unit() * range;

    let squares_on_naive_line: HashSet<WorldSquare> = Supercover::new(
        start.to_i32().to_tuple(),
        naive_end_point.to_i32().to_tuple(),
    )
    .map(|(x, y)| WorldSquare::new(x, y))
    .collect();

    let inside_faces_of_squares_touching_line: HashSet<WorldSquareWithOrthogonalDir> =
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
        .filter(|(face, point)| point.is_some())
        .map(|(face, point)| (face, point.unwrap()))
        .min_by_key(|(face, point)| OrderedFloat((start - *point).length()))
}
pub fn square_face_as_line<P: IntCoordinate>(
    square: P,
    face_direction: NormalizedOrthoAngle,
) -> TwoDifferentPoints<P::Floating> {
    let square_center = square.to_f32();
    // TODO: avoid the type notation on `step` somehow
    let face_center = square_center + face_direction.to_step::<P::Floating>() * 0.5;
    let p1_direction = face_direction.quarter_rotated_ccw(1);
    let p2_direction = -p1_direction;
    TwoDifferentPoints::new_from_two_points(
        face_center + p1_direction.to_step::<P::Floating>() * 0.5,
        face_center + p2_direction.to_step::<P::Floating>() * 0.5,
    )
}
pub fn ray_intersection_point_with_oriented_square_face(
    start: WorldPoint,
    angle: Angle<f32>,
    range: f32,
    face: WorldSquareWithOrthogonalDir,
) -> Option<WorldPoint> {
    let ray_direction = WorldMove::unit_vector_from_angle(angle);
    let face_is_facing_ray = ray_direction.dot(face.dir().to_step()) > 0.0;
    if !face_is_facing_ray {
        return None;
    }
    let face_line_segment = square_face_as_line(face.square(), face.dir());
    let ray_line_segment = TwoDifferentWorldPoints::from_ray(start, angle, range);
    ray_line_segment.intersection_point_with_other_line_segment(&face_line_segment)
}
pub fn does_ray_hit_oriented_square_face(
    start: WorldPoint,
    angle: Angle<f32>,
    range: f32,
    face: WorldSquareWithOrthogonalDir,
) -> bool {
    ray_intersection_point_with_oriented_square_face(start, angle, range, face).is_some()
}
pub fn naive_ray_endpoint<P: FloatCoordinate>(start: P, angle: Angle<f32>, length: f32) -> P {
    start + P::unit_vector_from_angle(angle) * length
}

#[cfg(test)]
mod tests {

    use ntest::{assert_about_eq, assert_false, assert_true, timeout};
    use pretty_assertions::{assert_eq, assert_ne};

    use super::*;
    #[test]
    fn test_line_intersections_with_square_are_in_same_order_as_input_line() {
        let input_line = TwoDifferentWorldPoints::new_from_two_points((-1.5, -1.0), (0.0, 0.0));
        let output_points = input_line.ordered_line_intersections_with_centered_unit_square();
        let output_line =
            TwoDifferentWorldPoints::new_from_two_points(output_points[0], output_points[1]);
        let in_vec = input_line.p2 - input_line.p1;
        let out_vec = output_line.p2 - output_line.p1;

        let same_direction = in_vec.dot(out_vec) > 0.0;
        assert!(same_direction);
    }

    #[test]
    fn test_line_intersections_with_square_are_in_same_order_as_input_line__vertical_line_on_left_edge(
    ) {
        let input_line: TwoDifferentPoints<WorldPoint> =
            TwoDifferentPoints::new_from_two_points(point2(-0.5, -0.5), point2(-0.5, 0.5));
        let output_points = input_line.ordered_line_intersections_with_centered_unit_square();
        assert_eq!(input_line.p1, output_points[0]);
        assert_eq!(input_line.p2, output_points[1]);
    }

    #[test]
    fn test_same_side_of_line__vertical_line() {
        let line = TwoDifferentPoints::<WorldPoint>::new_from_two_points((-0.5, -0.5), (-0.5, 0.5));
        let origin = point2(0.0, 0.0);
        let neg_point = point2(-20.0, 0.0);
        assert_false!(line.same_side_of_line(neg_point, origin))
    }
    #[test]
    fn test_check_line_intersection_with_standard_square() {
        let line: TwoDifferentWorldPoints = TwoDifferentPoints::new_horizontal(5.0);
        assert_false!(line.line_intersects_with_centered_unit_square());
    }
    #[test]
    fn test_line_intersections__observed_3_intersections() {
        TwoDifferentPoints::<WorldPoint>::new_from_two_points((-29.5, 5.0), (-27.589872, 4.703601))
            .ordered_line_intersections_with_centered_unit_square();
    }
    #[test]
    fn test_line_point_reflection() {
        let line: TwoDifferentWorldPoints =
            TwoDifferentPoints::new_from_two_points((1.0, 5.0), (2.4, 5.0));

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
    fn test_same_side_of_line() {
        let line = TwoDifferentPoints::<WorldPoint>::new_from_two_points(
            point2(1.0, 1.0),
            point2(2.0, 1.0),
        );
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
    fn test_horizontal_line_intersection_with_square() {
        let input_line: TwoDifferentPoints<WorldPoint> =
            TwoDifferentPoints::new_from_two_points(point2(0.5, 0.0), point2(-1.5, 0.0));
        let output_points = input_line.ordered_line_intersections_with_centered_unit_square();
        assert_eq!(output_points, vec![point2(0.5, 0.0), point2(-0.5, 0.0)]);
    }
    #[test]
    fn test_orthogonal_line_intersects_with_expanded_square() {
        let per_side_extension = 0.01;
        let fs: [fn(f32) -> TwoDifferentPoints<WorldPoint>; 2] = [
            TwoDifferentPoints::new_horizontal,
            TwoDifferentPoints::new_vertical,
        ];
        fs.into_iter().for_each(|f| {
            let exact_line = f(0.5);
            let closer_line = f(0.5 - per_side_extension / 2.0);
            let further_line = f(0.5 + per_side_extension / 2.0);
            let lines = [exact_line, closer_line, further_line];
            lines.into_iter().for_each(|line| {
                let string = format!("line: {}", line);
                assert!(
                    line.intersects_with_expanded_centered_unit_square(per_side_extension),
                    "{}, tolerance: {}",
                    string,
                    per_side_extension
                );
                assert!(
                    !line.intersects_with_expanded_centered_unit_square(-per_side_extension),
                    "{}, tolerance: {}",
                    string,
                    -per_side_extension
                );
            })
        });
    }

    #[test]
    fn test_vertical_line_intersection_with_square() {
        let input_line: TwoDifferentPoints<WorldPoint> =
            TwoDifferentPoints::new_from_two_points(point2(0.0, 0.5), point2(0.0, -1.5));
        let output_points = input_line.ordered_line_intersections_with_centered_unit_square();
        assert_eq!(output_points, vec![point2(0.0, 0.5), point2(0.0, -0.5)]);
    }
    #[test]
    fn test_ray_hit_face__simple() {
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
    fn test_ray_hit_face__face_must_face_ray() {
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
    fn test_ray_hit_face__miss() {
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
    fn test_ray_hit_face__under_ranged() {
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
    fn test_ray_hit_face__just_within_range() {
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
    fn test_ray_hit_face__just_out_of_closer_range() {
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
    fn test_ray_hit_face__just_within_closer_range() {
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
    fn test_ray_hit_face__just_out_of_really_close_range() {
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
    fn test_ray_hit_face__just_within_really_close_range() {
        assert!(does_ray_hit_oriented_square_face(
            point2(5.0, 6.49),
            Angle::degrees(90.0),
            0.02,
            (point2(5, 6), STEP_UP).into(),
        ));
    }
    #[test]
    fn test_ray_hit_face__angled_miss() {
        assert_false!(does_ray_hit_oriented_square_face(
            point2(5.0, 5.49),
            Angle::degrees(45.0),
            5.0,
            (point2(5, 6), STEP_UP).into(),
        ));
    }
    #[test]
    fn test_ray_hit_face__angled_hit() {
        assert!(does_ray_hit_oriented_square_face(
            point2(5.0, 5.49),
            Angle::degrees(45.0),
            5.0,
            (point2(5, 6), STEP_RIGHT).into(),
        ));
    }
    #[test]
    fn test_ray_hit_face__just_barely_touching_still_counts() {
        assert!(does_ray_hit_oriented_square_face(
            point2(5.5, 5.0),
            Angle::degrees(90.0),
            5.5,
            (point2(5, 10), STEP_UP).into(),
        ));
    }
    #[test]
    fn test_ray_hit_face__parallel_hit_does_not_count() {
        assert_false!(does_ray_hit_oriented_square_face(
            point2(5.0, 5.5),
            Angle::degrees(0.0),
            5.0,
            (point2(7, 5), STEP_UP).into(),
        ));
    }
    #[test]
    fn test_line_line_intersection__easy_orthogonal_hit() {
        assert_about_eq_2d(
            TwoDifferentWorldPoints::new_from_two_points(point2(0.0, 0.0), point2(0.0, 4.0))
                .intersection_point_with_other_line_segment(
                    &TwoDifferentWorldPoints::new_from_two_points(
                        point2(-1.0, 1.0),
                        point2(1.0, 1.0),
                    ),
                )
                .unwrap(),
            point2(0.0, 1.0),
        )
    }
    #[test]
    fn test_line_line_intersection__diagonal_intersection() {
        assert_about_eq_2d(
            TwoDifferentWorldPoints::new_from_two_points(point2(0.0, 0.0), point2(1.0, 1.0))
                .intersection_point_with_other_line_segment(
                    &TwoDifferentWorldPoints::new_from_two_points(
                        point2(1.0, 0.0),
                        point2(0.0, 1.0),
                    ),
                )
                .unwrap(),
            point2(0.5, 0.5),
        )
    }
    #[test]
    fn test_line_line_intersection__miss() {
        assert!(
            TwoDifferentWorldPoints::new_from_two_points(point2(0.0, 0.0), point2(1.0, 1.0))
                .intersection_point_with_other_line_segment(
                    &TwoDifferentWorldPoints::new_from_two_points(
                        point2(100.0, 1000.0),
                        point2(10.0, 10.0),
                    )
                )
                .is_none()
        )
    }
    #[test]
    fn test_line_line_intersection__endpoint_touch_mid_counts() {
        assert_about_eq_2d(
            TwoDifferentWorldPoints::new_from_two_points(point2(5.0, 5.0), point2(7.0, 5.0))
                .intersection_point_with_other_line_segment(
                    &TwoDifferentWorldPoints::new_from_two_points(
                        point2(5.5, 5.0),
                        point2(10.0, 10.0),
                    ),
                )
                .unwrap(),
            point2(5.5, 5.0),
        )
    }
    #[test]
    fn test_line_line_intersection__perpendicular_endpoints_touch() {
        assert_about_eq_2d(
            TwoDifferentWorldPoints::new_from_two_points(point2(5.0, 5.0), point2(10.0, 5.0))
                .intersection_point_with_other_line_segment(
                    &TwoDifferentWorldPoints::new_from_two_points(
                        point2(10.0, 5.0),
                        point2(10.0, 10.0),
                    ),
                )
                .unwrap(),
            point2(10.0, 5.0),
        )
    }
    #[test]
    fn test_line_line_intersection__parallel_endpoints_touch() {
        let line1 =
            TwoDifferentWorldPoints::new_from_two_points(point2(5.0, 5.0), point2(10.0, 5.0));
        let line2 =
            TwoDifferentWorldPoints::new_from_two_points(point2(10.0, 5.0), point2(20.0, 5.0));
        assert_about_eq_2d(
            line1
                .intersection_point_with_other_line_segment(&line2)
                .unwrap(),
            point2(10.0, 5.0),
        );
        assert_about_eq_2d(
            line1
                .reversed()
                .intersection_point_with_other_line_segment(&line2)
                .unwrap(),
            point2(10.0, 5.0),
        );
        assert_about_eq_2d(
            line1
                .intersection_point_with_other_line_segment(&line2.reversed())
                .unwrap(),
            point2(10.0, 5.0),
        );
        assert_about_eq_2d(
            line1
                .reversed()
                .intersection_point_with_other_line_segment(&line2.reversed())
                .unwrap(),
            point2(10.0, 5.0),
        );
    }
    #[test]
    fn test_line_line_intersection__parallel_miss() {
        assert!(
            TwoDifferentWorldPoints::new_from_two_points(point2(5.0, 5.0), point2(10.0, 5.0))
                .intersection_point_with_other_line_segment(
                    &TwoDifferentWorldPoints::new_from_two_points(
                        point2(11.0, 5.0),
                        point2(20.0, 5.0),
                    )
                )
                .is_none(),
        )
    }
    #[test]
    fn test_line_line_intersection__parallel_overlap_does_not_count() {
        assert!(
            TwoDifferentWorldPoints::new_from_two_points(point2(5.0, 5.0), point2(10.0, 5.0))
                .intersection_point_with_other_line_segment(
                    &TwoDifferentWorldPoints::new_from_two_points(
                        point2(9.0, 5.0),
                        point2(20.0, 5.0),
                    )
                )
                .is_none(),
        )
    }
    #[test]
    fn test_line_line_intersection__parallel_full_overlap_does_not_count() {
        assert!(
            TwoDifferentWorldPoints::new_from_two_points(point2(5.0, 5.0), point2(10.0, 5.0))
                .intersection_point_with_other_line_segment(
                    &TwoDifferentWorldPoints::new_from_two_points(
                        point2(0.0, 5.0),
                        point2(20.0, 5.0),
                    )
                )
                .is_none(),
        )
    }
    #[test]
    fn test_line_line_intersection__parallel_exact_overlap_does_not_count() {
        assert!(
            TwoDifferentWorldPoints::new_from_two_points(point2(5.0, 5.0), point2(10.0, 5.0))
                .intersection_point_with_other_line_segment(
                    &TwoDifferentWorldPoints::new_from_two_points(
                        point2(5.0, 5.0),
                        point2(10.0, 5.0),
                    )
                )
                .is_none(),
        )
    }
    #[test]
    fn test_first_inside_square_face_hit_by_ray__simple_case() {
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
    fn test_line_intersections_with_centered_unit_square_with_tolerance__regular_intersections() {
        let y = 0.2;

        let expected_points = [(0.5, y), (-0.5, y)];
        let intersections = TwoDifferentPoints::<WorldPoint>::new_horizontal(y)
            .ordered_line_intersections_with_centered_unit_square_with_tolerance(0.1);
        assert_eq!(intersections.len(), expected_points.len());
        expected_points
            .iter()
            .for_each(|&p| assert!(intersections.contains(&p.into())));
    }
    #[test]
    fn test_line_intersections_with_centered_unit_square_with_tolerance__is_intersecting__within_tolerance(
    ) {
        let y = 0.49;
        let tolerance = 0.1;

        let expected_points = [(0.5, y), (-0.5, y)];
        let intersections = TwoDifferentWorldPoints::new_horizontal(y)
            .unordered_line_intersections_with_centered_unit_square_with_tolerance(tolerance);
        assert_eq!(intersections.len(), expected_points.len());
        expected_points
            .iter()
            .for_each(|&p| assert!(intersections.contains(&p.into())));
    }
    #[test]
    fn test_line_intersections_with_centered_unit_square_with_tolerance__exactly_on_edge() {
        let y = 0.5;
        let tolerance = 0.0;

        let expected_points = [(0.5, y), (-0.5, y)];
        let intersections = TwoDifferentFloatPoints::new_horizontal(y)
            .unordered_line_intersections_with_centered_unit_square_with_tolerance(tolerance);
        assert_eq!(intersections.len(), expected_points.len());
        expected_points
            .iter()
            .for_each(|&p| assert!(intersections.contains(&p.into())));
    }
    #[test]
    fn test_line_intersections_with_centered_unit_square_with_tolerance__not_intersecting__parallel__within_tolerance(
    ) {
        let y = 0.51;
        let tolerance = 0.1;

        let expected_points = [(0.5, 0.5), (-0.5, 0.5)];
        let intersections = TwoDifferentFloatPoints::new_horizontal(y)
            .unordered_line_intersections_with_centered_unit_square_with_tolerance(tolerance);
        assert_eq!(intersections.len(), expected_points.len());
        expected_points
            .iter()
            .for_each(|&p| assert!(intersections.contains(&p.into())));
    }
    #[test]
    fn test_line_intersections_with_centered_unit_square_with_tolerance__not_intersecting__parallel__outside_tolerance(
    ) {
        let y = 0.7;
        let tolerance = 0.1;

        let intersections = TwoDifferentFloatPoints::new_horizontal(y)
            .unordered_line_intersections_with_centered_unit_square_with_tolerance(tolerance);
        assert!(intersections.is_empty());
    }
    #[test]
    fn test_line_intersections_with_centered_unit_square_with_tolerance__cut_corner__within_tolerance(
    ) {
        let line: TwoDifferentFloatPoints =
            TwoDifferentPoints::new_from_two_points((0.49, 0.5), (0.5, 0.49));
        let tolerance = 0.1;

        let expected_points = [(0.49, 0.5), (0.5, 0.49)];
        let intersections =
            line.unordered_line_intersections_with_centered_unit_square_with_tolerance(tolerance);
        assert_eq!(intersections.len(), expected_points.len());
        expected_points
            .iter()
            .for_each(|&p| assert!(intersections.contains(&p.into())));
    }
    #[test]
    fn test_line_intersections_with_centered_unit_square_with_tolerance__cut_corner_exact() {
        let line: TwoDifferentFloatPoints =
            TwoDifferentPoints::from_point_and_direction((0.5, 0.5), (1.0, -1.0));
        let tolerance = 0.1;

        let expected_points = [(0.5, 0.5)];
        let intersections =
            line.unordered_line_intersections_with_centered_unit_square_with_tolerance(tolerance);
        assert_eq!(intersections.len(), expected_points.len());
        expected_points
            .iter()
            .for_each(|&p| assert!(intersections.contains(&p.into())));
    }
    #[test]
    fn test_line_intersections_with_centered_unit_square_with_tolerance__miss_corner__within_tolerance(
    ) {
        let line: TwoDifferentFloatPoints =
            TwoDifferentPoints::from_point_and_direction((0.5, 0.52), (1.0, -1.0));
        let tolerance = 0.1;
        let expected_points = [(0.5, 0.5)];

        let intersections =
            line.unordered_line_intersections_with_centered_unit_square_with_tolerance(tolerance);
        assert_eq!(intersections.len(), expected_points.len());
        expected_points
            .iter()
            .for_each(|&p| assert!(intersections.contains(&p.into())));
    }
    #[test]
    fn test_line_intersections_with_centered_unit_square_with_tolerance__miss_corner__outside_tolerance(
    ) {
        let line: TwoDifferentFloatPoints =
            TwoDifferentPoints::from_point_and_direction((0.5, 0.52), (1.0, -1.0));
        let tolerance = 0.0001;

        let intersections =
            line.unordered_line_intersections_with_centered_unit_square_with_tolerance(tolerance);
        assert!(intersections.is_empty());
    }
    #[test]
    fn test_line_intersections_with_centered_unit_square_with_tolerance__miss_corner__all_corners_within_tolerance(
    ) {
        let line: TwoDifferentFloatPoints =
            TwoDifferentPoints::from_point_and_direction((0.5, 0.52), (1.0, -1.0));
        let tolerance = 100.0;
        let expected_points = [(0.5, 0.5)];

        let intersections =
            line.unordered_line_intersections_with_centered_unit_square_with_tolerance(tolerance);
        assert_eq!(intersections.len(), expected_points.len());
        expected_points
            .iter()
            .for_each(|&p| assert!(intersections.contains(&p.into())));
    }
    #[test]
    fn test_line_intersections_with_centered_unit_square_with_tolerance__miss_parallel__all_corners_within_tolerance(
    ) {
        let line: TwoDifferentFloatPoints = TwoDifferentPoints::new_horizontal(0.7);
        let tolerance = 100.0;
        let expected_points = [(-0.5, 0.5), (0.5, 0.5)];

        let intersections =
            line.unordered_line_intersections_with_centered_unit_square_with_tolerance(tolerance);
        assert_eq!(intersections.len(), expected_points.len());
        expected_points
            .iter()
            .for_each(|&p| assert!(intersections.contains(&p.into())));
    }
    #[test]
    fn test_line_intersections_with_centered_unit_square_with_tolerance__miss_almost_parallel__all_corners_within_tolerance(
    ) {
        let line: TwoDifferentFloatPoints =
            TwoDifferentPoints::from_point_and_direction((0.0, 0.52), (1.0, 0.0001));
        let tolerance = 100.0;
        let expected_points = [(-0.5, 0.5)];

        let intersections =
            line.unordered_line_intersections_with_centered_unit_square_with_tolerance(tolerance);
        assert_eq!(intersections.len(), expected_points.len());
        expected_points
            .iter()
            .for_each(|&p| assert!(intersections.contains(&p.into())));
    }
    #[test]
    fn test_x_intercept__vertical_zero() {
        let line = TwoDifferentWorldPoints::new_from_two_points((0.0, 0.0), (0.0, 1.0));
        assert_about_eq!(line.x_intercept().unwrap(), 0.0);
    }
    #[test]
    fn test_x_intercept__vertical_non_zero() {
        let x = 5.5;
        let line = TwoDifferentWorldPoints::new_from_two_points((x, 0.0), (x, 1.0));
        assert_about_eq!(line.x_intercept().unwrap(), x);
    }
    #[test]
    fn test_x_intercept__horizontal_zero() {
        let line = TwoDifferentWorldPoints::new_from_two_points((0.0, 0.0), (1000.0, 0.0));
        assert!(line.x_intercept().is_none());
    }
    #[test]
    fn test_x_intercept__horizontal_non_zero() {
        let line = TwoDifferentWorldPoints::new_from_two_points((7.0, 1.0), (0.0, 1.0));
        assert!(line.x_intercept().is_none());
    }
    #[test]
    fn test_x_intercept__diagonal() {
        let line = TwoDifferentWorldPoints::new_from_two_points((5.0, 1.0), (6.0, 2.0));
        assert_about_eq!(line.x_intercept().unwrap(), 4.0);
    }
    #[test]
    fn test_y_intercept_vertical_zero() {
        let line = TwoDifferentWorldPoints::new_from_two_points((0.0, 1.0), (0.0, 0.0));
        assert!(line.y_intercept().is_none());
    }
    #[test]
    fn test_y_intercept_vertical_non_zero() {
        let line = TwoDifferentWorldPoints::new_from_two_points((7.0, 1.0), (7.0, 7.0));
        assert!(line.y_intercept().is_none());
    }
    #[test]
    fn test_y_intercept_positive_slope() {
        let line = TwoDifferentWorldPoints::new_from_two_points((2.0, 1.0), (3.0, 2.0));
        assert_about_eq!(line.y_intercept().unwrap(), -1.0);
    }
    #[test]
    fn test_y_intercept_horizontal() {
        let y = 2.0;
        let line = TwoDifferentWorldPoints::new_from_two_points((-2.0, y), (3.0, y));
        assert_about_eq!(line.y_intercept().unwrap(), y);
    }
    #[test]
    fn test_y_intercept_negative_slope() {
        let line = TwoDifferentWorldPoints::new_from_two_points((-2.0, 1.0), (-3.0, 2.0));
        assert_about_eq!(line.y_intercept().unwrap(), -1.0);
    }
    #[test]
    fn test_slope__positive() {
        let line = TwoDifferentWorldPoints::new_from_two_points((5.5, 1.0), (6.0, 2.0));
        assert_about_eq!(line.slope().unwrap(), 2.0);
    }
    #[test]
    fn test_slope__vertical() {
        let line = TwoDifferentWorldPoints::new_from_two_points((6.0, 1.0), (6.0, 2.0));
        assert!(line.slope().is_none());
    }
    #[test]
    fn test_slope__horizontal() {
        let line = TwoDifferentWorldPoints::new_from_two_points((5.5, 2.0), (60.0, 2.0));
        assert_about_eq!(line.slope().unwrap(), 0.0);
    }
    #[test]
    fn test_slope__negative() {
        let line = TwoDifferentWorldPoints::new_from_two_points((4.0, 0.0), (0.0, 80.0));
        assert_about_eq!(line.slope().unwrap(), -20.0);
    }
}
