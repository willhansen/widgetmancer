#[derive(Clone, PartialEq, Copy)]
pub struct Line<T, U> {
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
    pub fn new_through_origin(second_point: impl Into<Point2D<T, U>>) -> Self {
        Self::new((T::zero(), T::zero()), second_point)
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
    pub fn line_intersections_with_centered_unit_square(&self) -> Vec<Point2D<f32, U>> {
        let line_point_a = self.p1;
        let line_point_b = self.p2;
        let is_same_point = line_point_a == line_point_b;
        if is_same_point {
            panic!("gave same point {}", line_point_a.to_string());
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
#[cfg(test)]
mod tests {

    use ntest::{assert_about_eq, assert_false, assert_true, timeout};
    use pretty_assertions::{assert_eq, assert_ne};

    use super::*;
}
