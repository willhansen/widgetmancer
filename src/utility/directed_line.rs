use crate::utility::*;

trait_alias_macro!(pub trait DirectedLinePointReqs = CoordinateOps);

#[derive(Clone, PartialEq, Debug, Copy, Hash, Eq)]
pub struct DirectedLine<PointType: DirectedLinePointReqs>(TwoDifferentPoints<PointType>);

impl<P: DirectedLinePointReqs> From<TwoDifferentPoints<P>> for DirectedLine<P> {
    fn from(value: TwoDifferentPoints<P>) -> Self {
        Self(value)
    }
}

impl<P: DirectedLinePointReqs> Add<P> for DirectedLine<P> {
    type Output = Self;

    fn add(self, rhs: P) -> Self::Output {
        Self::new(self.0.add(rhs))
    }
}
impl<P: DirectedLinePointReqs> Sub<P> for DirectedLine<P> {
    type Output = Self;

    fn sub(self, rhs: P) -> Self::Output {
        Self::new(self.0.sub(rhs))
    }
}

pub trait DirectedLineOps: LineOps + Reversible {
    fn two_points_on_line_in_order(&self) -> [Self::PointType; 2];
    fn arbitrary_vector_along_line(&self) -> Self::PointType {
        let [p1, p2] = self.two_points_on_line_in_order();
        p2 - p1
    }
    fn direction(&self) -> FAngle {
        self.arbitrary_vector_along_line()
            .better_angle_from_x_axis()
    }
    fn arbitrary_point_clockwise_of_line(&self) -> Self::PointType {
        self.arbitrary_point_on_shape() + self.arbitrary_vector_along_line().quarter_rotated_ccw(-1)
    }
    fn arbitrary_point_anticlockwise_of_line(&self) -> Self::PointType {
        self.arbitrary_point_on_shape() + self.arbitrary_vector_along_line().quarter_rotated_ccw(1)
    }
    fn arbitrary_point_right_of_line(&self) -> Self::PointType {
        self.arbitrary_point_clockwise_of_line()
    }
    fn arbitrary_point_left_of_line(&self) -> Self::PointType {
        self.arbitrary_point_anticlockwise_of_line()
    }
}

impl<P> LineOps for DirectedLine<P>
where
    P: DirectedLinePointReqs,
{
    type PointType = P;

    fn two_different_arbitrary_points_on_line(&self) -> [Self::PointType; 2] {
        todo!()
    }
}

impl<P> DirectedLineOps for DirectedLine<P>
where
    P: DirectedLinePointReqs,
    DirectedLine<P>: LineOps<PointType = P>,
{
    fn two_points_on_line_in_order(&self) -> [Self::PointType; 2] {
        todo!()
    }
}

// impl<L> Reversible for L
// where
//     L: DirectedLineOps + DirectedLineConstructors,
impl<P: DirectedLinePointReqs> Reversible for DirectedLine<P> {
    fn reversed(&self) -> Self {
        todo!()
    }
}

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
