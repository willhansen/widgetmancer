use crate::utility::*;

// TODO: macro or something to not have to rewrite this trait everywhere
trait_alias_macro!(pub trait PointReqsForDirectedLine = PointReqsForTwoDifferentPoints);

#[derive(Clone, PartialEq, Debug, Copy, Hash, Eq)]
pub struct DirectedLine<PointType: PointReqsForDirectedLine>(TwoDifferentPoints<PointType>);

impl<P: PointReqsForDirectedLine> DirectedLine<P> {
    fn new(value: TwoDifferentPoints<P>) -> Self {
        Self(value)
    }
}

impl<P: PointReqsForDirectedLine> From<TwoDifferentPoints<P>> for DirectedLine<P> {
    fn from(value: TwoDifferentPoints<P>) -> Self {
        Self::new(value)
    }
}

impl<P: PointReqsForDirectedLine> Add<P> for DirectedLine<P> {
    type Output = Self;

    fn add(self, rhs: P) -> Self::Output {
        self.0.add(rhs).into()
    }
}
impl<P: PointReqsForDirectedLine> Sub<P> for DirectedLine<P> {
    type Output = Self;

    fn sub(self, rhs: P) -> Self::Output {
        self.0.sub(rhs).into()
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
    P: PointReqsForDirectedLine,
{
    type PointType = P;

    fn two_different_arbitrary_points_on_line(&self) -> [Self::PointType; 2] {
        todo!()
    }
}

impl<P> DirectedLineOps for DirectedLine<P>
where
    P: PointReqsForDirectedLine,
    DirectedLine<P>: LineOps<PointType = P>,
{
    fn two_points_on_line_in_order(&self) -> [Self::PointType; 2] {
        todo!()
    }
}

// impl<L> Reversible for L
// where
//     L: DirectedLineOps + DirectedLineConstructors,
impl<P: PointReqsForDirectedLine> Reversible for DirectedLine<P> {
    fn reversed(&self) -> Self {
        todo!()
    }
}

pub trait DirectedLineConstructors {
    type _PointType: PointReqsForDirectedLine;

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

impl<P: PointReqsForDirectedLine> DirectedLineConstructors for DirectedLine<P> {
    type _PointType = P;

    fn try_new_from_directed_line(
        line: impl DirectedLineOps<PointType = Self::_PointType>,
    ) -> Result<Self, String>
    where
        Self: Sized,
    {
        todo!()
    }
}

impl<P: PointReqsForDirectedLine> QuarterTurnRotatable for DirectedLine<P> {
    fn quarter_rotated_ccw(&self, quarter_turns_ccw: impl Into<NormalizedOrthoAngle>) -> Self {
        self.0.quarter_rotated_ccw(quarter_turns_ccw).into()
    }
}
