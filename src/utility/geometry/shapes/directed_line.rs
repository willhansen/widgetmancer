use crate::utility::*;

trait_alias!(pub trait PointReqsForDirectedLine = PointReqsForTwoDifferentPoints);
trait_alias!(trait PointReqs = PointReqsForDirectedLine);

#[derive(Clone, PartialEq, Debug, Copy, Hash, Eq)]
pub struct DirectedLine<PointType: PointReqs>(TwoDifferentPoints<PointType>);

impl<P: PointReqs> DirectedLine<P> {
    fn new(value: TwoDifferentPoints<P>) -> Self {
        Self(value)
    }
}

impl_abstraction_for_newtype!(DirectedLine<P: PointReqs>, base=TwoDifferentPoints<P>);

impl_translate_for_newtype!(DirectedLine<P: PointReqs>);

pub trait OperationsForDirectedLine<P: PointReqs>:
    LineOps<P> + Reversible + ConstructorsForDirectedLine<P>
{
    fn two_points_on_line_in_order(&self) -> [P; 2];
    fn arbitrary_vector_along_line(&self) -> P {
        let [p1, p2] = self.two_points_on_line_in_order();
        p2 - p1
    }
    fn direction(&self) -> FAngle {
        self.arbitrary_vector_along_line()
            .better_angle_from_x_axis()
    }
    fn arbitrary_point_clockwise_of_line(&self) -> P {
        self.arbitrary_point_on_shape() + self.arbitrary_vector_along_line().quarter_rotated_ccw(-1)
    }
    fn arbitrary_point_anticlockwise_of_line(&self) -> P {
        self.arbitrary_point_on_shape() + self.arbitrary_vector_along_line().quarter_rotated_ccw(1)
    }
    fn arbitrary_point_right_of_line(&self) -> P {
        self.arbitrary_point_clockwise_of_line()
    }
    fn arbitrary_point_left_of_line(&self) -> P {
        self.arbitrary_point_anticlockwise_of_line()
    }
    fn point_is_on_right(&self, p: P) -> bool {
        let [p1, p2] = self.two_points_on_line_in_order();
        three_points_are_clockwise(p1, p2, p)
    }
}

impl_operations_for_line_for_delegate!(DirectedLine<P: PointReqs>, accessor=0);
impl_operations_for_directed_line_for_delegate!(DirectedLine<P: PointReqs>, accessor=0);

impl_constructors_for_line_for_newtype!(DirectedLine<P: PointReqs>, base= TwoDifferentPoints<P>);

// impl<L> Reversible for L
// where
//     L: OperationsForDirectedLine + DirectedLineConstructors,
impl<P: PointReqs> Reversible for DirectedLine<P> {
    fn reversed(&self) -> Self {
        todo!()
    }
}

pub trait ConstructorsForDirectedLine<P: PointReqs>:
    Sized + ConstructorsForTwoDifferentPoints<P>
{
    fn from_two_ordered_points_on_line(p1: P, p2: P) -> Self
    where
        Self: Sized,
    {
        Self::try_new_from_two_ordered_points_on_line(p1, p2).unwrap()
    }
    fn try_new_from_two_ordered_points_on_line(p1: P, p2: P) -> Result<Self, String>
    where
        Self: Sized,
    {
        let line = TwoDifferentPoints::<P>::from_two_points(p1, p2);
        Self::try_new_from_directed_line(line)
    }
    fn new_from_directed_line(line: impl OperationsForDirectedLine<P>) -> Self
    where
        Self: Sized,
    {
        Self::try_new_from_directed_line(line).unwrap()
    }
    fn choose_arbitrary_direction_for_line(line: impl LineOps<P>) -> Self {
        let [p1, p2] = line.two_different_arbitrary_points_on_line();
        Self::from_two_points(p1, p2)
    }
    fn try_new_from_directed_line(line: impl OperationsForDirectedLine<P>) -> Result<Self, String>
    where
        Self: Sized;
    fn from_point_and_unit_step_in_direction(
        point: impl Into<P>,
        direction: impl Into<FAngle>,
    ) -> Self
    where
        Self: OperationsForTwoDifferentPoints<P>,
        P: FloatCoordinateOps,
    {
        let p1 = point.into();
        let v = P::unit_vector_from_angle(direction.into());
        let p2 = p1 + v;
        Self::from_two_ordered_points_on_line(p1, p2)
    }
    // TODO: maybe move to DirectedLineConstructors
    fn from_point_and_vector(point: impl Into<P>, direction: impl Into<P>) -> Self {
        let p1 = point.into();
        let v = direction.into();
        let p2 = p1 + v;
        Self::from_two_points_allowing_snap_along_line(p1, p2)
    }
}

impl<P: PointReqs> AbstractsTo<Line<P>> for DirectedLine<P> {
    fn set_with_abstraction(&self, val: &Line<P>) -> Self {
        // TODO: round trip conversion tests: Convert from base type to abstract type, then set the
        // base type from the abstract type again.  Should be unchanged in all cases
        todo!()
    }
}
impl<P: PointReqs> AbstractionOf<TwoDifferentPoints<P>> for DirectedLine<P> {}

impl<P: PointReqs, T> ConstructorsForDirectedLine<P> for T
where
    T: AbstractionOf<DirectedLine<P>>,
{
    fn try_new_from_directed_line(line: impl OperationsForDirectedLine<P>) -> Result<Self, String>
    where
        Self: Sized,
    {
        Ok(DirectedLine::<P>::try_new_from_directed_line(line)?.into())
    }
}

impl<P: PointReqs> QuarterTurnRotatable for DirectedLine<P> {
    fn quarter_rotated_ccw(&self, quarter_turns_ccw: impl Into<NormalizedOrthoAngle>) -> Self {
        self.0.quarter_rotated_ccw(quarter_turns_ccw).into()
    }
}
