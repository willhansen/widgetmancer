use crate::utility::*;

trait_alias_macro!(pub trait PointReqsForDirectedFloatLine = PointReqsForDirectedLine + PointReqsForFloatLine);
trait_alias_macro!(trait PointReqs =PointReqsForDirectedFloatLine );

pub type DirectedFloatLine<UnitType> = DirectedLine<Point2D<f32, UnitType>>;

pub trait DirectedFloatLineOps<P: PointReqs>: OperationsForDirectedLine<P> + FloatLineOps<P> {
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
        square: <P as CoordinateOps>::OnGrid,
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

impl<L, P: PointReqs> DirectedFloatLineOps<P> for L where L: FloatLineOps<P> + OperationsForDirectedLine<P> {}
