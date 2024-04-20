use crate::utility::*;

pub trait DirectedLineConstructors {
    type _PointType: SignedCoordinateOps;

    fn new_from_two_ordered_points_on_line(p1: Self::_PointType, p2: Self::_PointType) -> Self
    where
        Self: Sized,
    {
        Self::try_new_from_two_ordered_points_on_line(p1, p2).unwrap()
    }
    fn try_new_from_two_ordered_points_on_line(
        p1: Self::_PointType,
        p2: Self::_PointType,
    ) -> Result<Self, String>
    where
        Self: Sized,
    {
        let line = TwoDifferentPoints::<Self::_PointType>::new(p1, p2);
        Self::try_new_from_directed_line(line)
    }
    fn new_from_directed_line(line: impl DirectedLineOps<PointType = Self::_PointType>) -> Self
    where
        Self: Sized,
    {
        Self::try_new_from_directed_line(line).unwrap()
    }
    fn try_new_from_directed_line(
        line: impl DirectedLineOps<PointType = Self::_PointType>,
    ) -> Result<Self, String>
    where
        Self: Sized;
    fn from_point_and_angle(
        point: impl Into<Self::_PointType>,
        direction: impl Into<FAngle>,
    ) -> Self
    where
        Self: TwoPointsWithRestriction<Self::_PointType>,
        Self::_PointType: FloatCoordinateOps,
    {
        let p1 = point.into();
        let v = Self::_PointType::unit_vector_from_angle(direction.into());
        let p2 = p1 + v;
        Self::new_from_two_ordered_points_on_line(p1, p2)
    }
}
