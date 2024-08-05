use crate::utility::*;

trait_alias!(pub trait PointReqs = directed_line::PointReqs + float_line::PointReqs);

pub type Shape<UnitType> = DirectedLine<Point2D<f32, UnitType>>;

pub trait Operations<P: PointReqs>:
    directed_line::Operations<P> + float_line::Operations<P>
{
    fn points_sorted_by_line_direction(&self, mut points: Vec<P>) -> Vec<P> {
        let normalized_line_direction = P::unit_vector_from_angle(self.direction());
        points.sort_by_key(|&point| OrderedFloat(normalized_line_direction.dot(point)));
        points
    }
    fn ordered_line_intersections_with_centered_unit_square_with_tolerance(
        &self,
        tolerance: f32,
    ) -> Vec<P> {
        self.points_sorted_by_line_direction(
            self.unordered_line_intersections_with_centered_unit_square_with_tolerance(tolerance),
        )
    }
    fn ordered_line_intersections_with_expanded_centered_unit_square(
        &self,
        expansion_length: f32,
    ) -> Vec<P> {
        self.points_sorted_by_line_direction(
            self.unordered_line_intersections_with_expanded_centered_unit_square(expansion_length),
        )
    }
    fn ordered_line_intersections_with_centered_unit_square(&self) -> Vec<P> {
        self.ordered_line_intersections_with_expanded_centered_unit_square(0.0)
    }
    fn ordered_line_intersections_with_square(
        &self,
        square: <P as coordinate::Operations>::OnGrid,
    ) -> Vec<P> {
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

impl<L, P: PointReqs> Operations<P> for L where
    L: float_line::Operations<P> + directed_line::Operations<P>
{
}
