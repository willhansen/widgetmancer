use crate::utility::*;

/// A traditional line that extends infinitely in both directions, now with floating point coordinates
pub trait FloatLine: Line + FloatLineLike {
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
        let normal_to_line: FAngle = self.perpendicular_directions()[0];

        let line_position_on_axis = self
            .arbitrary_point_on_line()
            .position_on_axis(normal_to_line);

        let square_projected_onto_axis: ClosedInterval<f32> =
            square.projected_onto_axis(normal_to_line);

        square_projected_onto_axis.depth_of(line_position_on_axis)
    }
}
impl<L> FloatLine for L
where
    L: Line,
    L::PointType: FloatCoordinate,
{
}
