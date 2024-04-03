use std::ops::Add;

use euclid::approxeq::ApproxEq;
use line_drawing::Supercover;
use num::{traits::float::FloatCore, NumCast, One, Signed, Zero};
use rand::{rngs::StdRng, Rng};

use crate::utility::*;

/// A traditional line that extends infinitely in both directions
pub trait Line: LineLike + TryFromTwoPoints<Self::PointType> {
    fn from_point_array(points: [Self::PointType; 2]) -> Self {
        Self::from_array_of_two_exact_points(points)
    }
    fn from_line_like(line_like: impl LineLike<PointType = Self::PointType>) -> Self {
        let p = line_like.two_different_arbitrary_points_on_line();
        Self::from_two_points_allowing_snap_along_line(p[0], p[1])
    }
    fn point_is_on_line(&self, point: impl Into<Self::PointType>) -> bool {
        let [p1, p2] = self.two_different_arbitrary_points_on_line();
        on_line(p1, p2, point.into())
    }
    // // fn try_new_from_line(line: impl Line<PointType = Self::PointType>) -> Result<Self, String>;
    fn new_horizontal(y: <Self::PointType as Coordinate>::DataType) -> Self {
        Self::from_two_points_allowing_snap_along_line(
            Self::PointType::new(<Self::PointType as Coordinate>::DataType::zero(), y),
            Self::PointType::new(<Self::PointType as Coordinate>::DataType::one(), y),
        )
    }
    fn new_vertical(x: <Self::PointType as Coordinate>::DataType) -> Self {
        Self::from_two_points_allowing_snap_along_line(
            Self::PointType::new(x, <Self::PointType as Coordinate>::DataType::zero()),
            Self::PointType::new(x, <Self::PointType as Coordinate>::DataType::one()),
        )
    }
    fn new_through_origin(second_point: impl Into<Self::PointType>) -> Self {
        Self::from_two_points_allowing_snap_along_line(
            <Self::PointType as euclid::num::Zero>::zero(),
            second_point.into(),
        )
    }
    fn from_point_and_vector(
        point: impl Into<Self::PointType>,
        direction: impl Into<Self::PointType>,
    ) -> Self {
        let p1 = point.into();
        let v = direction.into();
        let p2 = p1 + v;
        Self::from_two_points_allowing_snap_along_line(p1, p2)
    }
    fn with_direction(
        &self,
        direction_hint: FAngle,
    ) -> impl DirectedLine<PointType = Self::PointType> {
        let p = self.arbitrary_point_on_line();
        let dirs = self.parallel_directions_as_vectors();

        let Some(good_dir) = dirs
            .iter()
            .find(|dir| dir.position_on_axis(direction_hint) > 0.0)
        else {
            panic!(
                "direction_hint ({:?}) not in either direction ({:?})",
                direction_hint, dirs
            );
        };
        let p2 = p + *good_dir;
        TwoDifferentPoints::new(p, p2)
    }
    fn with_arbitrary_direction(&self) -> impl DirectedLine<PointType = Self::PointType> {
        self.with_direction(self.parallel_directions()[1])
    }
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
pub fn square_face_as_line<P: SignedIntCoordinate>(
    square: P,
    face_direction: OrthogonalDirection,
) -> TwoDifferentPoints<P::Floating> {
    let square_center = square.to_f32();
    // TODO: avoid the type notation on `step` somehow
    let face_center = square_center.moved(face_direction, 0.5);
    TwoDifferentPoints::new_from_two_unordered_points_on_line(
        face_center.moved(face_direction.left(), 0.5),
        face_center.moved(face_direction.right(), 0.5),
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
    let ray_line_segment = TwoDifferentWorldPoints::new_from_point_and_radial(start, angle, range);
    ray_line_segment.intersection_point_with_other_line_segment(face_line_segment)
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
        let input_line =
            TwoDifferentWorldPoints::easy_new_from_two_points_on_line((-1.5, -1.0), (0.0, 0.0));
        let output_points = input_line.ordered_line_intersections_with_centered_unit_square();
        let output_line = TwoDifferentWorldPoints::new_from_two_unordered_points_on_line(
            output_points[0],
            output_points[1],
        );
        let in_vec = input_line.p2() - input_line.p1();
        let out_vec = output_line.p2() - output_line.p1();

        let same_direction = in_vec.dot(out_vec) > 0.0;
        assert!(same_direction);
    }

    #[test]
    fn test_line_intersections_with_square_are_in_same_order_as_input_line__vertical_line_on_left_edge(
    ) {
        let input_line: TwoDifferentPoints<WorldPoint> =
            TwoDifferentPoints::new_from_two_ordered_points_on_line(
                point2(-0.5, -0.5),
                point2(-0.5, 0.5),
            );
        let output_points = input_line.ordered_line_intersections_with_centered_unit_square();
        assert_eq!(input_line.p1(), output_points[0]);
        assert_eq!(input_line.p2(), output_points[1]);
    }

    #[test]
    fn test_same_side_of_line__vertical_line() {
        let line =
            TwoDifferentWorldPoints::easy_new_from_two_points_on_line((-0.5, -0.5), (-0.5, 0.5));
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
        TwoDifferentWorldPoints::easy_new_from_two_points_on_line(
            (-29.5, 5.0),
            (-27.589872, 4.703601),
        )
        .ordered_line_intersections_with_centered_unit_square();
    }
    #[test]
    fn test_line_point_reflection() {
        let line: TwoDifferentWorldPoints =
            TwoDifferentPoints::easy_new_from_two_points_on_line((1.0, 5.0), (2.4, 5.0));

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
        let line = TwoDifferentPoints::<WorldPoint>::new_from_two_unordered_points_on_line(
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
            TwoDifferentPoints::new_from_two_unordered_points_on_line(
                point2(0.5, 0.0),
                point2(-1.5, 0.0),
            );
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
            TwoDifferentPoints::new_from_two_unordered_points_on_line(
                point2(0.0, 0.5),
                point2(0.0, -1.5),
            );
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
            TwoDifferentWorldPoints::new_from_two_ordered_points_on_line(
                point2(0.0, 0.0),
                point2(0.0, 4.0),
            )
            .intersection_point_with_other_line_segment(
                TwoDifferentWorldPoints::new_from_two_ordered_points_on_line(
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
            TwoDifferentWorldPoints::new_from_two_ordered_points_on_line(
                point2(0.0, 0.0),
                point2(1.0, 1.0),
            )
            .intersection_point_with_other_line_segment(
                TwoDifferentWorldPoints::new_from_two_ordered_points_on_line(
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
            TwoDifferentWorldPoints::new_from_two_unordered_points_on_line(
                point2(0.0, 0.0),
                point2(1.0, 1.0)
            )
            .intersection_point_with_other_line_segment(
                TwoDifferentWorldPoints::new_from_two_unordered_points_on_line(
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
            TwoDifferentWorldPoints::new_from_two_ordered_points_on_line(
                point2(5.0, 5.0),
                point2(7.0, 5.0),
            )
            .intersection_point_with_other_line_segment(
                TwoDifferentWorldPoints::new_from_two_ordered_points_on_line(
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
        let a = TwoDifferentWorldPoints::new_from_two_unordered_points_on_line(
            point2(5.0, 5.0),
            point2(10.0, 5.0),
        );
        let b = TwoDifferentWorldPoints::new_from_two_unordered_points_on_line(
            point2(10.0, 5.0),
            point2(10.0, 10.0),
        );
        assert_about_eq_2d(
            a.intersection_point_with_other_line_segment(b).unwrap(),
            point2(10.0, 5.0),
        )
    }
    #[test]
    fn test_line_line_intersection__parallel_endpoints_touch() {
        let line1 = TwoDifferentWorldPoints::new_from_two_unordered_points_on_line(
            point2(5.0, 5.0),
            point2(10.0, 5.0),
        );
        let line2 = TwoDifferentWorldPoints::new_from_two_unordered_points_on_line(
            point2(10.0, 5.0),
            point2(20.0, 5.0),
        );
        assert_about_eq_2d(
            line1
                .intersection_point_with_other_line_segment(line2)
                .unwrap(),
            point2(10.0, 5.0),
        );
        assert_about_eq_2d(
            line1
                .reversed()
                .intersection_point_with_other_line_segment(line2)
                .unwrap(),
            point2(10.0, 5.0),
        );
        assert_about_eq_2d(
            line1
                .intersection_point_with_other_line_segment(line2.reversed())
                .unwrap(),
            point2(10.0, 5.0),
        );
        assert_about_eq_2d(
            line1
                .reversed()
                .intersection_point_with_other_line_segment(line2.reversed())
                .unwrap(),
            point2(10.0, 5.0),
        );
    }
    #[test]
    fn test_line_line_intersection__parallel_miss() {
        assert!(
            TwoDifferentWorldPoints::new_from_two_unordered_points_on_line(
                point2(5.0, 5.0),
                point2(10.0, 5.0)
            )
            .intersection_point_with_other_line_segment(
                TwoDifferentWorldPoints::new_from_two_unordered_points_on_line(
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
            TwoDifferentWorldPoints::new_from_two_unordered_points_on_line(
                point2(5.0, 5.0),
                point2(10.0, 5.0)
            )
            .intersection_point_with_other_line_segment(
                TwoDifferentWorldPoints::new_from_two_unordered_points_on_line(
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
            TwoDifferentWorldPoints::new_from_two_unordered_points_on_line(
                point2(5.0, 5.0),
                point2(10.0, 5.0)
            )
            .intersection_point_with_other_line_segment(
                TwoDifferentWorldPoints::new_from_two_unordered_points_on_line(
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
            TwoDifferentWorldPoints::new_from_two_unordered_points_on_line(
                point2(5.0, 5.0),
                point2(10.0, 5.0)
            )
            .intersection_point_with_other_line_segment(
                TwoDifferentWorldPoints::new_from_two_unordered_points_on_line(
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
        let intersections = TwoDifferentWorldPoints::new_horizontal(y)
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
        let intersections = default::TwoDifferentFloatPoints::new_horizontal(y)
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

        let intersections = default::TwoDifferentFloatPoints::new_horizontal(y)
            .unordered_line_intersections_with_centered_unit_square_with_tolerance(tolerance);
        assert!(intersections.is_empty());
    }
    #[test]
    fn test_line_intersections_with_centered_unit_square_with_tolerance__cut_corner__within_tolerance(
    ) {
        let line: default::TwoDifferentFloatPoints =
            TwoDifferentPoints::easy_new_from_two_points_on_line((0.49, 0.5), (0.5, 0.49));
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
        let line: default::TwoDifferentFloatPoints =
            TwoDifferentPoints::from_point_and_vector((0.5, 0.5), (1.0, -1.0));
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
        let line: default::TwoDifferentFloatPoints =
            TwoDifferentPoints::from_point_and_vector((0.5, 0.52), (1.0, -1.0));
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
        let line: default::TwoDifferentFloatPoints =
            TwoDifferentPoints::from_point_and_vector((0.5, 0.52), (1.0, -1.0));
        let tolerance = 0.0001;

        let intersections =
            line.unordered_line_intersections_with_centered_unit_square_with_tolerance(tolerance);
        assert!(intersections.is_empty());
    }
    #[test]
    fn test_line_intersections_with_centered_unit_square_with_tolerance__miss_corner__all_corners_within_tolerance(
    ) {
        let line: default::TwoDifferentFloatPoints =
            TwoDifferentPoints::from_point_and_vector((0.5, 0.52), (1.0, -1.0));
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
        let line: default::TwoDifferentFloatPoints = TwoDifferentPoints::new_horizontal(0.7);
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
        let line: default::TwoDifferentFloatPoints =
            TwoDifferentPoints::from_point_and_vector((0.0, 0.52), (1.0, 0.0001));
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
        let line =
            TwoDifferentWorldPoints::easy_new_from_two_points_on_line((0.0, 0.0), (0.0, 1.0));
        assert_about_eq!(line.x_intercept().unwrap(), 0.0);
    }
    #[test]
    fn test_x_intercept__vertical_non_zero() {
        let x = 5.5;
        let line = TwoDifferentWorldPoints::easy_new_from_two_points_on_line((x, 0.0), (x, 1.0));
        assert_about_eq!(line.x_intercept().unwrap(), x);
    }
    #[test]
    fn test_x_intercept__horizontal_zero() {
        let line =
            TwoDifferentWorldPoints::easy_new_from_two_points_on_line((0.0, 0.0), (1000.0, 0.0));
        assert!(line.x_intercept().is_none());
    }
    #[test]
    fn test_x_intercept__horizontal_non_zero() {
        let line =
            TwoDifferentWorldPoints::easy_new_from_two_points_on_line((7.0, 1.0), (0.0, 1.0));
        assert!(line.x_intercept().is_none());
    }
    #[test]
    fn test_x_intercept__diagonal() {
        let line =
            TwoDifferentWorldPoints::easy_new_from_two_points_on_line((5.0, 1.0), (6.0, 2.0));
        assert_about_eq!(line.x_intercept().unwrap(), 4.0);
    }
    #[test]
    fn test_y_intercept_vertical_zero() {
        let line =
            TwoDifferentWorldPoints::easy_new_from_two_points_on_line((0.0, 1.0), (0.0, 0.0));
        assert!(line.y_intercept().is_none());
    }
    #[test]
    fn test_y_intercept_vertical_non_zero() {
        let line =
            TwoDifferentWorldPoints::easy_new_from_two_points_on_line((7.0, 1.0), (7.0, 7.0));
        assert!(line.y_intercept().is_none());
    }
    #[test]
    fn test_y_intercept_positive_slope() {
        let line =
            TwoDifferentWorldPoints::easy_new_from_two_points_on_line((2.0, 1.0), (3.0, 2.0));
        assert_about_eq!(line.y_intercept().unwrap(), -1.0);
    }
    #[test]
    fn test_y_intercept_horizontal() {
        let y = 2.0;
        let line = TwoDifferentWorldPoints::easy_new_from_two_points_on_line((-2.0, y), (3.0, y));
        assert_about_eq!(line.y_intercept().unwrap(), y);
    }
    #[test]
    fn test_y_intercept_negative_slope() {
        let line =
            TwoDifferentWorldPoints::easy_new_from_two_points_on_line((-2.0, 1.0), (-3.0, 2.0));
        assert_about_eq!(line.y_intercept().unwrap(), -1.0);
    }
    #[test]
    fn test_slope__positive() {
        let line =
            TwoDifferentWorldPoints::easy_new_from_two_points_on_line((5.5, 1.0), (6.0, 2.0));
        assert_about_eq!(line.slope().unwrap(), 2.0);
    }
    #[test]
    fn test_slope__vertical() {
        let line =
            TwoDifferentWorldPoints::easy_new_from_two_points_on_line((6.0, 1.0), (6.0, 2.0));
        assert!(line.slope().is_none());
    }
    #[test]
    fn test_slope__horizontal() {
        let line =
            TwoDifferentWorldPoints::easy_new_from_two_points_on_line((5.5, 2.0), (60.0, 2.0));
        assert_about_eq!(line.slope().unwrap(), 0.0);
    }
    #[test]
    fn test_slope__negative() {
        let line =
            TwoDifferentWorldPoints::easy_new_from_two_points_on_line((4.0, 0.0), (0.0, 80.0));
        assert_about_eq!(line.slope().unwrap(), -20.0);
    }
}
