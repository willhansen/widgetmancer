use crate::utility::*;

pub trait FromDirectedLine<P: Coordinate> {
    fn new_from_two_ordered_points_on_line(p1: P, p2: P) -> Self
    where
        Self: Sized,
    {
        Self::try_new_from_two_ordered_points_on_line(p1, p2).unwrap()
    }
    fn try_new_from_two_ordered_points_on_line(p1: P, p2: P) -> Result<Self, String>
    where
        Self: Sized,
    {
        let line = TwoDifferentPoints::<P>::new(p1, p2);
        todo!();
        // Self::try_new_from_directed_line(line)
    }
    fn new_from_directed_line(line: impl DirectedLine<PointType = P>) -> Self
    where
        Self: Sized,
    {
        Self::try_new_from_directed_line(line).unwrap()
    }
    fn try_new_from_directed_line(line: impl DirectedLine<PointType = P>) -> Result<Self, String>
    where
        Self: Sized;
    fn from_point_and_angle(point: impl Into<P>, direction: impl Into<FAngle>) -> Self
    where
        Self: TwoPointsWithRestriction<P>,
        P: FloatCoordinate,
    {
        let p1 = point.into();
        let v = P::unit_vector_from_angle(direction.into());
        let p2 = p1 + v;
        Self::new_from_two_ordered_points_on_line(p1, p2)
    }
}
