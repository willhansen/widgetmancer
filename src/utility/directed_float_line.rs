use crate::utility::*;

pub trait DirectedFloatLine: DirectedLine + FloatLine {
    fn ordered_line_intersections_with_centered_unit_square_with_tolerance(
        &self,
        tolerance: f32,
    ) -> Vec<Self::PointType> {
        self.points_sorted_by_line_direction(
            self.unordered_line_intersections_with_centered_unit_square_with_tolerance(tolerance),
        )
    }
    fn ordered_line_intersections_with_expanded_centered_unit_square(
        &self,
        expansion_length: f32,
    ) -> Vec<Self::PointType> {
        self.points_sorted_by_line_direction(
            self.unordered_line_intersections_with_expanded_centered_unit_square(expansion_length),
        )
    }
    fn ordered_line_intersections_with_centered_unit_square(&self) -> Vec<Self::PointType> {
        self.ordered_line_intersections_with_expanded_centered_unit_square(0.0)
    }
    fn ordered_line_intersections_with_square(
        &self,
        square: <Self::PointType as Coordinate>::OnGrid,
    ) -> Vec<Self::PointType> {
        let offset = square.to_f32();
        let relative_self = *self - offset;
        let relative_intersections =
            relative_self.ordered_line_intersections_with_centered_unit_square();
        relative_intersections
            .iter()
            .map(|&p| p + offset)
            .collect_vec()
    }
}

impl<L> DirectedFloatLine for L where L: FloatLine + DirectedLine {}
