#[derive(PartialEq, Clone, Debug, Copy)]
pub struct HalfPlane<T, U>
where
    T: Display + Copy,
{
    // Internal convention is that the half plane is clockwise of the vector from p1 to p2 of the dividing line
    dividing_line: Line<T, U>,
}

impl<U: Copy + Debug> HalfPlane<f32, U> {
    pub fn from_line_and_point_on_half_plane(
        can_be_dividing_line: impl Into<Line<f32, U>>,
        point_on_half_plane: impl Into<Point2D<f32, U>>,
    ) -> Self {
        let dividing_line = can_be_dividing_line.into();
        HalfPlane {
            dividing_line: if three_points_are_clockwise(
                dividing_line.p1,
                dividing_line.p2,
                point_on_half_plane.into(),
            ) {
                dividing_line
            } else {
                dividing_line.reversed()
            },
        }
    }
    pub fn from_line_away_from_origin(can_be_dividing_line: impl Into<Line<f32, U>>) -> Self {
        let line: Line<f32, U> = can_be_dividing_line.into();
        assert_false!(line.point_is_on_line((0.0, 0.0)));
        todo!();
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
        self.dividing_line.a_point_right_of_line()
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
    pub fn overlaps_within_unit_square(&self, other: &Self, tolerance: f32) -> bool {
        todo!()
    }
}

impl<U: Copy + Debug> QuarterTurnRotatable for HalfPlane<f32, U> {
    fn rotated(&self, quarter_turns_anticlockwise: QuarterTurnsAnticlockwise) -> Self {
        let line = self.dividing_line();
        let point = self.point_on_half_plane();
        let new_point = point.rotated(quarter_turns_anticlockwise);
        let new_line = line.rotated(quarter_turns_anticlockwise);
        Self::from_line_and_point_on_half_plane(new_line, new_point)
    }
}
#[cfg(test)]
mod tests {

    use ntest::{assert_about_eq, assert_false, assert_true, timeout};
    use pretty_assertions::{assert_eq, assert_ne};

    use super::*;
    #[test]
    fn test_half_plane_complementary_check__different_lines() {
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
    fn test_half_plane_complementary_check__equivalent_lines() {
        let line: Line<f32, SquareGridInWorldFrame> = Line::new(point2(0.0, 0.0), point2(1.0, 1.0));
        let line2: Line<f32, SquareGridInWorldFrame> =
            Line::new(point2(2.0, 2.0), point2(5.0, 5.0));
        let p1 = point2(0.0, 1.0);
        let p2 = point2(1.0, 0.0);

        let half_plane_1 = HalfPlane::from_line_and_point_on_half_plane(line, p1);
        let half_plane_2 = HalfPlane::from_line_and_point_on_half_plane(line2, p2);

        assert!(half_plane_1.is_about_complementary_to(half_plane_2, 1e-6));
    }
}
