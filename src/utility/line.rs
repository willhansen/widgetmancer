use std::ops::Add;

use euclid::approxeq::ApproxEq;
use line_drawing::Supercover;
use rand::{rngs::StdRng, Rng};

use super::{
    coordinate_frame_conversions::*, coordinates::*, general_utility::*, get_new_rng, poses::*,
};

pub type WorldLine = Line<f32, SquareGridInWorldFrame>;
pub type WorldSquareLine = Line<i32, SquareGridInWorldFrame>;
pub type LocalCharacterLine = Line<f32, CharacterGridInLocalCharacterFrame>;

#[derive(Clone, PartialEq, Copy)]
pub struct Line<T, U = euclid::UnknownUnit> {
    pub p1: Point2D<T, U>,
    pub p2: Point2D<T, U>,
}

impl<T, U> Line<T, U>
where
    T: Clone + Debug + PartialEq + Signed + Copy,
{
    pub fn new(
        can_be_p1: impl Into<Point2D<T, U>>,
        can_be_p2: impl Into<Point2D<T, U>>,
    ) -> Line<T, U> {
        let p1 = can_be_p1.into();
        let p2 = can_be_p2.into();
        assert_ne!(p1, p2);
        Line { p1, p2 }
    }
    pub fn new_horizontal(y: T) -> Self {
        Line::new((T::zero(), y), (T::one(), y))
    }
    pub fn new_vertical(x: T) -> Self {
        Line::new((x, T::zero()), (x, T::one()))
    }
    pub fn new_through_origin(second_point: impl Into<Point2D<T, U>>) -> Self {
        Self::new((T::zero(), T::zero()), second_point)
    }
    pub fn from_point_and_direction(
        point: impl Into<Point2D<T, U>>,
        direction: impl Into<Vector2D<T, U>>,
    ) -> Self {
        let p = point.into();
        Self::new(p, p + direction.into())
    }
    pub fn reverse(&mut self) {
        mem::swap(&mut self.p2, &mut self.p1);
    }
    pub fn reversed(&self) -> Self {
        Self::new(self.p2.clone(), self.p1.clone())
    }
    pub fn get(&self, index: u32) -> Point2D<T, U> {
        match index {
            0 => self.p1.clone(),
            1 => self.p2.clone(),
            _ => panic!("only two points defining the line"),
        }
    }
    pub fn square_length(&self) -> T {
        (self.p1 - self.p2).square_length()
    }
    pub fn is_orthogonal(&self) -> bool {
        self.p1.x == self.p2.x || self.p1.y == self.p2.y
    }
    pub fn as_array(&self) -> [Point2D<T, U>; 2] {
        [self.p1, self.p2]
    }
}
impl<T, U> QuarterTurnRotatable for Line<T, U>
where
    T: Clone + Debug + PartialEq + Signed + Copy,
{
    fn rotated(&self, quarter_turns_anticlockwise: QuarterTurnsAnticlockwise) -> Self {
        let new_points = [0, 1].map(|i| self.get(i).rotated(quarter_turns_anticlockwise));
        Self::new(new_points[0].clone(), new_points[1].clone())
    }
}

impl<NumberType, UnitType, CanBePointType> From<(CanBePointType, CanBePointType)>
    for Line<NumberType, UnitType>
where
    CanBePointType: Into<Point2D<NumberType, UnitType>>,
    NumberType: Clone + Debug + PartialEq + Signed + Copy,
{
    fn from(value: (CanBePointType, CanBePointType)) -> Self {
        Self::new(value.0, value.1)
    }
}

impl<U: Copy> Line<f32, U> {
    pub fn length(&self) -> f32 {
        (self.p1 - self.p2).length()
    }
    pub fn point_is_on_line(&self, point: impl Into<Point2D<f32, U>>) -> bool {
        on_line(self.p1, self.p2, point.into())
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
    pub fn a_point_clockwise_of_line(&self) -> Point2D<f32, U> {
        rotate_point_around_point(self.p1, self.p2, Angle::radians(-PI / 2.0))
    }
    pub fn a_point_anticlockwise_of_line(&self) -> Point2D<f32, U> {
        rotate_point_around_point(self.p1, self.p2, Angle::radians(PI / 2.0))
    }
    pub fn a_point_right_of_line(&self) -> Point2D<f32, U> {
        self.a_point_clockwise_of_line()
    }
    pub fn a_point_left_of_line(&self) -> Point2D<f32, U> {
        self.a_point_anticlockwise_of_line()
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

    pub fn reflect_point_over_line(&self, point: impl Into<Point2D<f32, U>>) -> Point2D<f32, U> {
        let p1_to_p = point.into() - self.p1;
        let p1_to_p2 = self.p2 - self.p1;
        let parallel_part = p1_to_p.project_onto_vector(p1_to_p2);
        let perpendicular_part = p1_to_p - parallel_part;
        let p1_to_reflected_p = parallel_part - perpendicular_part;
        self.p1 + p1_to_reflected_p
    }

    pub fn direction(&self) -> Angle<f32> {
        better_angle_from_x_axis(self.p2 - self.p1)
    }
    pub fn parallel_directions(&self) -> [Angle<f32>; 2] {
        [
            better_angle_from_x_axis(self.p2 - self.p1),
            better_angle_from_x_axis(self.p1 - self.p2),
        ]
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

        three_points_are_clockwise(point_a, point_b, point_c)
            == three_points_are_clockwise(point_a, point_b, point_d)
    }
    pub fn line_intersections_with_centered_unit_square_with_tolerance(
        &self,
        tolerance: f32,
    ) -> Vec<Point2D<f32, U>> {
        let regular_intersections = self.line_intersections_with_centered_unit_square();
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
    pub fn line_intersections_with_expanded_centered_unit_square(
        &self,
        expansion_length: f32,
    ) -> Vec<Point2D<f32, U>> {
        let line_point_a = self.p1;
        let line_point_b = self.p2;
        let half_side_length = 0.5 + expansion_length;

        let is_vertical_line = line_point_a.x == line_point_b.x;
        let is_horizontal_line = line_point_a.y == line_point_b.y;

        if is_vertical_line {
            let x = line_point_a.x;
            if x.abs() <= half_side_length {
                self.points_in_line_order(vec![
                    point2(x, half_side_length),
                    point2(x, -half_side_length),
                ])
            } else {
                vec![]
            }
        } else if is_horizontal_line {
            let y = line_point_a.y;
            if y.abs() <= half_side_length {
                self.points_in_line_order(vec![
                    point2(half_side_length, y),
                    point2(-half_side_length, y),
                ])
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

            let side_positions = vec![half_side_length, -half_side_length];

            let mut candidate_intersections: Vec<Point2D<f32, U>> = vec![];
            for &x in &side_positions {
                let y = m * x + b;
                if y.abs() <= half_side_length {
                    candidate_intersections.push(point2(x, y));
                }
            }
            for y in side_positions {
                let x = (y - b) / m;
                // top and bottom don't catch corners, sides do
                if x.abs() < half_side_length {
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
    pub fn line_intersections_with_centered_unit_square(&self) -> Vec<Point2D<f32, U>> {
        self.line_intersections_with_expanded_centered_unit_square(0.0)
    }
    pub fn line_intersects_with_centered_unit_square(&self) -> bool {
        self.intersects_with_expanded_centered_unit_square(0.0)
    }
    pub fn intersects_with_expanded_centered_unit_square(&self, per_face_extension: f32) -> bool {
        !self
            .line_intersections_with_expanded_centered_unit_square(per_face_extension)
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

    pub fn intersection_point_with_other_line_segment(
        &self,
        other: &Self,
    ) -> Option<Point2D<f32, U>> {
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
            self.p1.to_string(),
            self.p2.to_string(),
        )
    }
}
impl<T, U> Display for Line<T, U>
where
    T: Display + Copy,
{
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        std::fmt::Debug::fmt(&self, f)
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
        .filter(|(face, point)| point.is_some())
        .map(|(face, point)| (face, point.unwrap()))
        .min_by_key(|(face, point)| OrderedFloat((start - *point).length()))
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
    let face_is_facing_ray = ray_direction.dot(face.dir().step().to_f32()) > 0.0;
    if !face_is_facing_ray {
        return None;
    }
    let face_line_segment = square_face_as_line(face.square(), face.dir());
    let ray_line_segment = WorldLine::from_ray(start, angle, range);
    ray_line_segment.intersection_point_with_other_line_segment(&face_line_segment)
}
pub fn does_ray_hit_oriented_square_face(
    start: WorldPoint,
    angle: Angle<f32>,
    range: f32,
    face: SquareWithOrthogonalDir,
) -> bool {
    ray_intersection_point_with_oriented_square_face(start, angle, range, face).is_some()
}
pub fn naive_ray_endpoint<U>(
    start: Point2D<f32, U>,
    angle: Angle<f32>,
    length: f32,
) -> Point2D<f32, U> {
    start + unit_vector_from_angle(angle).cast_unit() * length
}

#[cfg(test)]
mod tests {

    use ntest::{assert_about_eq, assert_false, assert_true, timeout};
    use pretty_assertions::{assert_eq, assert_ne};

    use super::*;
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
    fn test_line_intersections_with_square_are_in_same_order_as_input_line__vertical_line_on_left_edge(
    ) {
        let input_line: Line<f32, SquareGridInWorldFrame> =
            Line::new(point2(-0.5, -0.5), point2(-0.5, 0.5));
        let output_points = input_line.line_intersections_with_centered_unit_square();
        assert_eq!(input_line.p1, output_points[0]);
        assert_eq!(input_line.p2, output_points[1]);
    }

    #[test]
    fn test_same_side_of_line__vertical_line() {
        let line = Line::new(WorldPoint::new(-0.5, -0.5), point2(-0.5, 0.5));
        let origin = point2(0.0, 0.0);
        let neg_point = point2(-20.0, 0.0);
        assert_false!(line.same_side_of_line(neg_point, origin))
    }
    #[test]
    fn test_check_line_intersection_with_standard_square() {
        let line: WorldLine = Line::new_horizontal(5.0);
        assert_false!(line.line_intersects_with_centered_unit_square());
    }
    #[test]
    fn test_line_intersections__observed_3_intersections() {
        Line::new(
            WorldPoint::new(-29.5, 5.0),
            WorldPoint::new(-27.589872, 4.703601),
        )
        .line_intersections_with_centered_unit_square();
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
    fn test_horizontal_line_intersection_with_square() {
        let input_line: Line<f32, SquareGridInWorldFrame> =
            Line::new(point2(0.5, 0.0), point2(-1.5, 0.0));
        let output_points = input_line.line_intersections_with_centered_unit_square();
        assert_eq!(output_points, vec![point2(0.5, 0.0), point2(-0.5, 0.0)]);
    }
    #[test]
    fn test_orthogonal_line_intersects_with_expanded_square() {
        let per_side_extension = 0.01;
        let fs: [fn(f32) -> Line<f32>; 2] = [Line::new_horizontal, Line::new_vertical];
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
        let input_line: Line<f32, SquareGridInWorldFrame> =
            Line::new(point2(0.0, 0.5), point2(0.0, -1.5));
        let output_points = input_line.line_intersections_with_centered_unit_square();
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
            WorldLine::new(point2(0.0, 0.0), point2(0.0, 4.0))
                .intersection_point_with_other_line_segment(&WorldLine::new(
                    point2(-1.0, 1.0),
                    point2(1.0, 1.0),
                ))
                .unwrap(),
            point2(0.0, 1.0),
        )
    }
    #[test]
    fn test_line_line_intersection__diagonal_intersection() {
        assert_about_eq_2d(
            WorldLine::new(point2(0.0, 0.0), point2(1.0, 1.0))
                .intersection_point_with_other_line_segment(&WorldLine::new(
                    point2(1.0, 0.0),
                    point2(0.0, 1.0),
                ))
                .unwrap(),
            point2(0.5, 0.5),
        )
    }
    #[test]
    fn test_line_line_intersection__miss() {
        assert!(WorldLine::new(point2(0.0, 0.0), point2(1.0, 1.0))
            .intersection_point_with_other_line_segment(&WorldLine::new(
                point2(100.0, 1000.0),
                point2(10.0, 10.0),
            ))
            .is_none())
    }
    #[test]
    fn test_line_line_intersection__endpoint_touch_mid_counts() {
        assert_about_eq_2d(
            WorldLine::new(point2(5.0, 5.0), point2(7.0, 5.0))
                .intersection_point_with_other_line_segment(&WorldLine::new(
                    point2(5.5, 5.0),
                    point2(10.0, 10.0),
                ))
                .unwrap(),
            point2(5.5, 5.0),
        )
    }
    #[test]
    fn test_line_line_intersection__perpendicular_endpoints_touch() {
        assert_about_eq_2d(
            WorldLine::new(point2(5.0, 5.0), point2(10.0, 5.0))
                .intersection_point_with_other_line_segment(&WorldLine::new(
                    point2(10.0, 5.0),
                    point2(10.0, 10.0),
                ))
                .unwrap(),
            point2(10.0, 5.0),
        )
    }
    #[test]
    fn test_line_line_intersection__parallel_endpoints_touch() {
        let line1 = WorldLine::new(point2(5.0, 5.0), point2(10.0, 5.0));
        let line2 = WorldLine::new(point2(10.0, 5.0), point2(20.0, 5.0));
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
        assert!(WorldLine::new(point2(5.0, 5.0), point2(10.0, 5.0))
            .intersection_point_with_other_line_segment(&WorldLine::new(
                point2(11.0, 5.0),
                point2(20.0, 5.0),
            ))
            .is_none(),)
    }
    #[test]
    fn test_line_line_intersection__parallel_overlap_does_not_count() {
        assert!(WorldLine::new(point2(5.0, 5.0), point2(10.0, 5.0))
            .intersection_point_with_other_line_segment(&WorldLine::new(
                point2(9.0, 5.0),
                point2(20.0, 5.0),
            ))
            .is_none(),)
    }
    #[test]
    fn test_line_line_intersection__parallel_full_overlap_does_not_count() {
        assert!(WorldLine::new(point2(5.0, 5.0), point2(10.0, 5.0))
            .intersection_point_with_other_line_segment(&WorldLine::new(
                point2(0.0, 5.0),
                point2(20.0, 5.0),
            ))
            .is_none(),)
    }
    #[test]
    fn test_line_line_intersection__parallel_exact_overlap_does_not_count() {
        assert!(WorldLine::new(point2(5.0, 5.0), point2(10.0, 5.0))
            .intersection_point_with_other_line_segment(&WorldLine::new(
                point2(5.0, 5.0),
                point2(10.0, 5.0),
            ))
            .is_none(),)
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
        let intersections = Line::<f32>::new_horizontal(y)
            .line_intersections_with_centered_unit_square_with_tolerance(0.1);
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
        let intersections = Line::<f32>::new_horizontal(y)
            .line_intersections_with_centered_unit_square_with_tolerance(tolerance);
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
        let intersections = Line::<f32>::new_horizontal(y)
            .line_intersections_with_centered_unit_square_with_tolerance(tolerance);
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
        let intersections = Line::<f32>::new_horizontal(y)
            .line_intersections_with_centered_unit_square_with_tolerance(tolerance);
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

        let intersections = Line::<f32>::new_horizontal(y)
            .line_intersections_with_centered_unit_square_with_tolerance(tolerance);
        assert!(intersections.is_empty());
    }
    #[test]
    fn test_line_intersections_with_centered_unit_square_with_tolerance__cut_corner__within_tolerance(
    ) {
        let line: Line<f32> = Line::new((0.49, 0.5), (0.5, 0.49));
        let tolerance = 0.1;

        let expected_points = [(0.49, 0.5), (0.5, 0.49)];
        let intersections =
            line.line_intersections_with_centered_unit_square_with_tolerance(tolerance);
        assert_eq!(intersections.len(), expected_points.len());
        expected_points
            .iter()
            .for_each(|&p| assert!(intersections.contains(&p.into())));
    }
    #[test]
    fn test_line_intersections_with_centered_unit_square_with_tolerance__cut_corner_exact() {
        let line: Line<f32> = Line::from_point_and_direction((0.5, 0.5), (1.0, -1.0));
        let tolerance = 0.1;

        let expected_points = [(0.5, 0.5)];
        let intersections =
            line.line_intersections_with_centered_unit_square_with_tolerance(tolerance);
        assert_eq!(intersections.len(), expected_points.len());
        expected_points
            .iter()
            .for_each(|&p| assert!(intersections.contains(&p.into())));
    }
    #[test]
    fn test_line_intersections_with_centered_unit_square_with_tolerance__miss_corner__within_tolerance(
    ) {
        let line: Line<f32> = Line::from_point_and_direction((0.5, 0.52), (1.0, -1.0));
        let tolerance = 0.1;
        let expected_points = [(0.5, 0.5)];

        let intersections =
            line.line_intersections_with_centered_unit_square_with_tolerance(tolerance);
        assert_eq!(intersections.len(), expected_points.len());
        expected_points
            .iter()
            .for_each(|&p| assert!(intersections.contains(&p.into())));
    }
    #[test]
    fn test_line_intersections_with_centered_unit_square_with_tolerance__miss_corner__outside_tolerance(
    ) {
        let line: Line<f32> = Line::from_point_and_direction((0.5, 0.52), (1.0, -1.0));
        let tolerance = 0.0001;

        let intersections =
            line.line_intersections_with_centered_unit_square_with_tolerance(tolerance);
        assert!(intersections.is_empty());
    }
    #[test]
    fn test_line_intersections_with_centered_unit_square_with_tolerance__miss_corner__all_corners_within_tolerance(
    ) {
        let line: Line<f32> = Line::from_point_and_direction((0.5, 0.52), (1.0, -1.0));
        let tolerance = 100.0;
        let expected_points = [(0.5, 0.5)];

        let intersections =
            line.line_intersections_with_centered_unit_square_with_tolerance(tolerance);
        assert_eq!(intersections.len(), expected_points.len());
        expected_points
            .iter()
            .for_each(|&p| assert!(intersections.contains(&p.into())));
    }
    #[test]
    fn test_line_intersections_with_centered_unit_square_with_tolerance__miss_parallel__all_corners_within_tolerance(
    ) {
        let line: Line<f32> = Line::new_horizontal(0.7);
        let tolerance = 100.0;
        let expected_points = [(-0.5, 0.5), (0.5, 0.5)];

        let intersections =
            line.line_intersections_with_centered_unit_square_with_tolerance(tolerance);
        assert_eq!(intersections.len(), expected_points.len());
        expected_points
            .iter()
            .for_each(|&p| assert!(intersections.contains(&p.into())));
    }
    #[test]
    fn test_line_intersections_with_centered_unit_square_with_tolerance__miss_almost_parallel__all_corners_within_tolerance(
    ) {
        let line: Line<f32> = Line::from_point_and_direction((0.0, 0.52), (1.0, 0.0001));
        let tolerance = 100.0;
        let expected_points = [(-0.5, 0.5)];

        let intersections =
            line.line_intersections_with_centered_unit_square_with_tolerance(tolerance);
        assert_eq!(intersections.len(), expected_points.len());
        expected_points
            .iter()
            .for_each(|&p| assert!(intersections.contains(&p.into())));
    }
}
