use crate::utility::*;

pub trait DirectedFloatLineLike: FloatLineLike + DirectedLineLike {
    fn points_sorted_by_line_direction(
        &self,
        mut points: Vec<Self::PointType>,
    ) -> Vec<Self::PointType> {
        let normalized_line_direction = Self::PointType::unit_vector_from_angle(self.direction());
        points.sort_by_key(|&point| OrderedFloat(normalized_line_direction.dot(point)));
        points
    }
}
impl<L> DirectedFloatLineLike for L where L: DirectedLineLike + FloatLineLike {}
