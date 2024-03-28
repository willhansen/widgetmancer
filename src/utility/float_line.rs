use crate::utility::*;

/// A traditional line that extends infinitely in both directions, now with floating point coordinates
pub trait FloatLine: Line + FloatLineLike {
    fn from_point_angle_and_distance(
        start: Self::PointType,
        angle: Angle<f32>,
        length: f32,
    ) -> Self {
        assert!(length > 0.0);
        Self::new_from_two_points_on_line(start, naive_ray_endpoint(start, angle, length))
    }
    fn point_is_on_line(&self, point: impl Into<Self::PointType>) -> bool {
        let [p1, p2] = self.two_different_arbitrary_points_on_line();
        on_line(p1, p2, point.into())
    }
    fn point_is_approx_on_line(&self, point: Self::PointType, tolerance: f32) -> bool {
        self.normal_distance_to_point(point) < tolerance
    }
    fn closest_point_to_point(&self, point: impl Into<Self::PointType>) -> Self::PointType {
        let point = point.into();
        let [p1, p2] = self.two_different_arbitrary_points_on_line();
        let p1_to_point = point - p1;
        let p1_to_p2 = p2 - p1;
        let parallel_part_of_p1_to_point = p1_to_point.projected_onto(p1_to_p2);
        p1 + parallel_part_of_p1_to_point
    }
    fn depth_in_square(&self, square: <Self::PointType as Coordinate>::OnGrid) -> f32 {
        let square_center = square.to_f32();
        let closest_point_on_line = self.closest_point_to_point(square_center);
        let vector_from_square_center_to_point = closest_point_on_line - square_center;
        // let proj_of_square_on_axis = self.perpendicular_directions()[0]
        // let dir_to_point =
        // let_corner_positions_on_axis = square.square_corners().map(Coordinate::position_on_axis())
        todo!()
    }
}
impl<L> FloatLine for L
where
    L: Line,
    L::PointType: FloatCoordinate,
{
}
