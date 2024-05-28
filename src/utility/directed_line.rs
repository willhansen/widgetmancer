use crate::utility::*;

trait_alias_macro!(pub trait PointReqsForDirectedLine = PointReqsForTwoDifferentPoints);
trait_alias_macro!(trait PointReqs = PointReqsForDirectedLine);

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

// TODO: abstraction instead of newtype (why?)
//macro_rules! impl_directed_line_ops_for_abstraction
// NOTE: The `$($accessor)+` thing is to generalize over a member (like `self.0`) and a getter (like `self.thing()`)
macro_rules! impl_operations_for_directed_line_for_delegate {
    ($type:ident<P: $traitparam:ident>, accessor=$($accessor:tt)+) => {
        impl<P: $traitparam> OperationsForDirectedLine<P> for $type<P> {
            fn two_points_on_line_in_order(&self) -> [P; 2] {
                self.$($accessor)+.two_points_on_line_in_order()
            }
        }
    };
}
pub(crate) use impl_operations_for_directed_line_for_delegate;

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
        Self::from_two_exact_points(p1, p2)
    }
    fn try_new_from_directed_line(line: impl OperationsForDirectedLine<P>) -> Result<Self, String>
    where
        Self: Sized;
    fn from_point_and_angle(point: impl Into<P>, direction: impl Into<FAngle>) -> Self
    where
        Self: TwoPointsWithRestriction<P>,
        P: FloatCoordinateOps,
    {
        let p1 = point.into();
        let v = P::unit_vector_from_angle(direction.into());
        let p2 = p1 + v;
        Self::new_from_two_ordered_points_on_line(p1, p2)
    }
    // TODO: maybe move to DirectedLineConstructors
    fn from_point_and_vector(point: impl Into<P>, direction: impl Into<P>) -> Self {
        let p1 = point.into();
        let v = direction.into();
        let p2 = p1 + v;
        Self::from_two_points_allowing_snap_along_line(p1, p2)
    }
}

impl_constructors_for_two_different_points_for_abstraction!(DirectedLine<P: PointReqs>, base= TwoDifferentPoints<P>);

macro_rules! impl_constructors_for_directed_line_for_newtype {
    ($type:ident<P: $traitparam:ident>, base= $BaseType:ident<P>) => {
        impl<P: $traitparam> ConstructorsForDirectedLine<P> for $type<P> {
            fn try_new_from_directed_line(
                line: impl OperationsForDirectedLine<P>,
            ) -> Result<Self, String>
            where
                Self: Sized,
            {
                Ok($BaseType::<P>::try_new_from_directed_line(line)?.into())
            }
        }
    };
}
pub(crate) use impl_constructors_for_directed_line_for_newtype;

impl_constructors_for_directed_line_for_newtype!(DirectedLine<P: PointReqs>, base=TwoDifferentPoints<P>);

impl<P: PointReqs> QuarterTurnRotatable for DirectedLine<P> {
    fn quarter_rotated_ccw(&self, quarter_turns_ccw: impl Into<NormalizedOrthoAngle>) -> Self {
        self.0.quarter_rotated_ccw(quarter_turns_ccw).into()
    }
}
