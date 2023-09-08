use super::{coordinate_frame_conversions::*, coordinates::*, general_utility::*, line::*};

// TODO: make this a newtype with restricted points
pub type LocalSquareHalfPlane = HalfPlane<f32, SquareGridInLocalSquareFrame>;

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
    pub fn new_away_from_origin_from_border_line(
        can_be_dividing_line: impl Into<Line<f32, U>>,
    ) -> Self {
        let line: Line<f32, U> = can_be_dividing_line.into();
        assert_false!(line.point_is_on_line((0.0, 0.0)));
        Self::from_line_and_point_on_half_plane(line, line.reflect_point_over_line((0.0, 0.0)))
    }
    pub fn new_toward_origin_from_border_line(
        can_be_dividing_line: impl Into<Line<f32, U>>,
    ) -> Self {
        let line: Line<f32, U> = can_be_dividing_line.into();
        assert_false!(line.point_is_on_line((0.0, 0.0)));
        Self::from_line_and_point_on_half_plane(line, (0.0, 0.0))
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
    pub fn retracted(&self, retracted_distance: f32) -> Self {
        self.extended(-retracted_distance)
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
        let border_intersection_is_inside_square = if let Some(intersection_point) = self
            .dividing_line
            .intersection_point_with_other_extended_line(&other.dividing_line)
        {
            point_is_in_centered_unit_square(intersection_point, tolerance)
        } else {
            false
        };
        if border_intersection_is_inside_square {
            return true;
        }

        // from here, each plane is either is fully overlapping the square, not overlapping, or partially overlapping.

        let full_cover = [self, other].map(|hp| hp.fully_covers_expanded_unit_square(tolerance));
        let partial_cover = [self, other].map(|hp| {
            hp.dividing_line
                .line_intersects_with_expanded_centered_unit_square(tolerance)
        });
        // Just in case
        (0..2).for_each(|i| {
            assert_false!(full_cover[i] && partial_cover[i]);
        });

        let no_cover = [0, 1].map(|i| !full_cover[i] && !partial_cover[i]);

        let both_full_cover = all_true(&full_cover);
        let any_full_cover = any_true(&full_cover);
        let any_partial_cover = any_true(&partial_cover);
        let both_partial_cover = all_true(&partial_cover);
        let any_no_cover = any_true(&no_cover);

        let one_full_cover_and_one_partial_cover = any_full_cover && any_partial_cover;

        if both_full_cover || one_full_cover_and_one_partial_cover {
            return true;
        } else if any_no_cover {
            return false;
        }
        // both partial cover case
        assert!(both_partial_cover);
        // we've already ruled out an intersection in the square

        let same_direction = unit_vector_from_angle(self.direction_toward_plane())
            .dot(unit_vector_from_angle(other.direction_toward_plane()))
            > 0.0;
        if same_direction {
            return true;
        }

        // Now need to know if the two lines are on each others half planes
        return self.point_is_on_half_plane(other.dividing_line.p1);
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

    use std::array::from_fn;

    use ntest::{assert_about_eq, assert_false, assert_true, timeout};
    use pretty_assertions::{assert_eq, assert_ne};

    use crate::utility::coordinate_frame_conversions::SquareGridInWorldFrame;

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
            1.0 / 2.0_f32.sqrt()
        );
    }
    // TODO: This test isn't readable.
    #[test]
    fn test_halfplane_overlap_within_unit_square() {
        //
        //      +----------------+
        //   o  |  o         o   |
        //   G  |  A         B   |
        //      |                |
        //      |       +        |   o
        //      |       O        |   C
        //      |                |
        //   F  |  E         D   |
        //   o  |  o         o   |
        //      +----------------+
        //
        let a = (-0.4, 0.4);
        let b = (0.4, 0.4);
        let c = (0.6, 0.0);
        let d = (0.4, -0.4);
        let e = (-0.4, -0.4);
        let f = (-0.6, -0.4);
        let g = (-0.6, 0.4);

        let up: LocalSquareHalfPlane =
            HalfPlane::from_line_and_point_on_half_plane((a, b), (0.0, 5.0));
        let up_right = HalfPlane::from_line_and_point_on_half_plane((b, c), (1.0, 1.0));
        let down_right = HalfPlane::from_line_and_point_on_half_plane((c, d), (1.0, -1.0));
        let down = HalfPlane::from_line_and_point_on_half_plane((d, e), (0.0, -5.0));
        let left = HalfPlane::from_line_and_point_on_half_plane((f, g), (-5.0, 0.0));

        let tolerance = 1e-5;

        let f = HalfPlane::overlaps_within_unit_square;
        let vars = vec![up, up_right, down_right, down, left];
        // in format of f(vars[row], vars[col]).  Should be symmetric anyway
        let correct_boolean_matrix = [
            [1, 1, 0, 0, 0],
            [1, 1, 0, 0, 0],
            [0, 0, 1, 1, 0],
            [0, 0, 1, 1, 0],
            [0, 0, 0, 0, 0],
        ];
        let actual_boolean_matrix: [[i32; 5]; 5] = from_fn(|row| {
            from_fn(|col| {
                if f(&vars[row], &vars[col], tolerance) {
                    1
                } else {
                    0
                }
            })
        });

        assert_eq!(actual_boolean_matrix, correct_boolean_matrix);
    }
    #[test]
    fn test_halfplane_overlap_within_unit_square__true__horizontal_up() {
        let a: LocalSquareHalfPlane =
            HalfPlane::new_away_from_origin_from_border_line(Line::new_horizontal(0.3));
        let b: LocalSquareHalfPlane =
            HalfPlane::new_away_from_origin_from_border_line(Line::new_horizontal(0.4));
        assert!(a.overlaps_within_unit_square(&b, 1e-5))
    }
    #[test]
    fn test_halfplane_overlap_within_unit_square__true__vertical_left() {
        let a: LocalSquareHalfPlane =
            HalfPlane::new_away_from_origin_from_border_line(Line::new_vertical(-0.3));
        let b: LocalSquareHalfPlane =
            HalfPlane::new_away_from_origin_from_border_line(Line::new_vertical(-0.4));
        assert!(a.overlaps_within_unit_square(&b, 1e-5))
    }
    #[test]
    fn test_halfplane_overlap_within_unit_square__false__horizontal__both_partially_covering_square(
    ) {
        let a: LocalSquareHalfPlane =
            HalfPlane::new_away_from_origin_from_border_line(Line::new_horizontal(0.3));
        let b: LocalSquareHalfPlane =
            HalfPlane::new_away_from_origin_from_border_line(Line::new_horizontal(-0.4));
        assert_false!(a.overlaps_within_unit_square(&b, 1e-5))
    }
    #[test]
    fn test_halfplane_overlap_within_unit_square__true__both_fully_cover() {
        let a: LocalSquareHalfPlane =
            HalfPlane::new_toward_origin_from_border_line(Line::new_horizontal(5.0));
        let b: LocalSquareHalfPlane =
            HalfPlane::new_away_from_origin_from_border_line(Line::new_horizontal(5.0));
        assert!(a.overlaps_within_unit_square(&b, 1e-5))
    }
    #[test]
    fn test_halfplane_overlap_within_unit_square__true__one_fully_covers() {
        let a: LocalSquareHalfPlane =
            HalfPlane::new_toward_origin_from_border_line(Line::new_horizontal(0.3));
        let b: LocalSquareHalfPlane =
            HalfPlane::new_away_from_origin_from_border_line(Line::new_horizontal(5.0));
        assert!(a.overlaps_within_unit_square(&b, 1e-5))
    }
    #[test]
    fn test_halfplane_overlap_within_unit_square__true__orthogonal() {
        let a: LocalSquareHalfPlane =
            HalfPlane::new_away_from_origin_from_border_line(Line::new_vertical(0.3));
        let b: LocalSquareHalfPlane =
            HalfPlane::new_away_from_origin_from_border_line(Line::new_horizontal(0.1));
        assert!(a.overlaps_within_unit_square(&b, 1e-5))
    }
    #[test]
    fn test_halfplane_overlap_within_unit_square__false__orthogonal() {
        let a: LocalSquareHalfPlane =
            HalfPlane::new_away_from_origin_from_border_line(Line::new_vertical(0.6));
        let b: LocalSquareHalfPlane =
            HalfPlane::new_away_from_origin_from_border_line(Line::new_horizontal(0.1));
        assert_false!(a.overlaps_within_unit_square(&b, 1e-5))
    }
    #[test]
    fn test_halfplane_overlap_within_unit_square__false__orthogonal__one_fully_covers() {
        let a: LocalSquareHalfPlane =
            HalfPlane::new_away_from_origin_from_border_line(Line::new_vertical(0.6));
        let b: LocalSquareHalfPlane =
            HalfPlane::new_toward_origin_from_border_line(Line::new_horizontal(5.0));
        assert_false!(a.overlaps_within_unit_square(&b, 1e-5))
    }
    #[test]
    fn test_halfplane_overlap_within_unit_square__false__orthogonal__neither_fully_covers() {
        let a: LocalSquareHalfPlane =
            HalfPlane::new_away_from_origin_from_border_line(Line::new_vertical(0.6));
        let b: LocalSquareHalfPlane =
            HalfPlane::new_away_from_origin_from_border_line(Line::new_horizontal(5.0));
        assert_false!(a.overlaps_within_unit_square(&b, 1e-5))
    }
    #[test]
    fn test_halfplane_overlap_within_unit_square__true__orthogonal_within_tolerance_diagonal() {
        let tolerance = 0.01;
        let a: LocalSquareHalfPlane = HalfPlane::new_away_from_origin_from_border_line(
            Line::new_vertical(0.5 + tolerance / 2.0),
        );
        let b: LocalSquareHalfPlane = HalfPlane::new_away_from_origin_from_border_line(
            Line::new_horizontal(0.5 + tolerance / 2.0),
        );
        assert!(a.overlaps_within_unit_square(&b, tolerance))
    }
    #[test]
    fn test_halfplane_overlap_within_unit_square__false__orthogonal_within_tolerance_diagonal() {
        let tolerance = -0.01;
        let a: LocalSquareHalfPlane = HalfPlane::new_away_from_origin_from_border_line(
            Line::new_vertical(0.5 + tolerance / 2.0),
        );
        let b: LocalSquareHalfPlane = HalfPlane::new_away_from_origin_from_border_line(
            Line::new_horizontal(0.5 + tolerance / 2.0),
        );
        assert!(a.overlaps_within_unit_square(&b, tolerance))
    }
    #[test]
    fn test_halfplane_overlap_within_unit_square__true__angled() {
        let a: LocalSquareHalfPlane =
            HalfPlane::new_away_from_origin_from_border_line(Line::new_horizontal(-0.2));
        let b: LocalSquareHalfPlane =
            HalfPlane::new_toward_origin_from_border_line(Line::new((0.0, 0.5), (0.5, -0.3)));
        assert!(a.overlaps_within_unit_square(&b, 1e-5))
    }
    #[test]
    fn test_halfplane_overlap_within_unit_square__false__angled() {
        let a: LocalSquareHalfPlane =
            HalfPlane::new_away_from_origin_from_border_line(Line::new_horizontal(-0.2));
        let b: LocalSquareHalfPlane =
            HalfPlane::new_toward_origin_from_border_line(Line::new((0.0, 0.5), (0.5, -0.1)));
        assert_false!(a.overlaps_within_unit_square(&b, 1e-5))
    }
    #[test]
    fn test_halfplane_overlap_within_unit_square__false__identical_on_edge() {
        let a: LocalSquareHalfPlane =
            HalfPlane::new_away_from_origin_from_border_line(Line::new_horizontal(0.5));
        let b: LocalSquareHalfPlane = a.clone();
        assert_false!(a.overlaps_within_unit_square(&b, 0.0))
    }
    #[test]
    fn test_halfplane_overlap_within_unit_square__false__opposing_parallel_half_planes_exact_touch()
    {
        let a: LocalSquareHalfPlane =
            HalfPlane::new_away_from_origin_from_border_line(Line::new_horizontal(0.5));
        let b: LocalSquareHalfPlane = a.complement();
        assert_false!(a.overlaps_within_unit_square(&b, 0.0))
    }
    #[test]
    fn test_halfplane_overlap_within_unit_square__false__opposing_parallel_half_planes_extended_but_within_tolerance(
    ) {
        let tolerance = 0.01;
        let a: LocalSquareHalfPlane =
            HalfPlane::new_away_from_origin_from_border_line(Line::new_horizontal(0.5));
        let b: LocalSquareHalfPlane = a.complement().extended(tolerance / 2.0);
        assert_false!(a.overlaps_within_unit_square(&b, -tolerance))
    }
    #[test]
    fn test_halfplane_overlap_within_unit_square__true__opposing_parallel_half_planes_retracted_but_within_tolerance(
    ) {
        let tolerance = 0.01;
        let a: LocalSquareHalfPlane =
            HalfPlane::new_away_from_origin_from_border_line(Line::new_horizontal(0.5));
        let b: LocalSquareHalfPlane = a.complement().retracted(tolerance / 2.0);
        assert!(a.overlaps_within_unit_square(&b, tolerance))
    }
}
