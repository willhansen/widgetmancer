use crate::utility::*;

trait_alias!(pub trait PointReqs = two_different_points::PointReqs);

#[derive(Clone, PartialEq, Debug, Copy, Hash, Eq)]
pub struct Shape<PointType: PointReqs>(TwoDifferentPoints<PointType>);

impl<P: PointReqs> Shape<P> {
    fn new(value: TwoDifferentPoints<P>) -> Self {
        Self(value)
    }
}

impl_abstraction_for_newtype!(Shape<P: PointReqs>, base=TwoDifferentPoints<P>);

translate::impl_for_newtype!(Shape<P: PointReqs>);

pub trait Operations<P: PointReqs>: line::Operations<P> + Reversible + Constructors<P> {
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

impl<P: PointReqs> line::Operations<P> for Shape<P> {
    fn two_different_arbitrary_points_on_line(&self) -> [P; 2] {
        self.two_points_on_line_in_order()
    }
}
impl<P: PointReqs> Operations<P> for Shape<P> {
    fn two_points_on_line_in_order(&self) -> [P; 2] {
        [self.0.p1(), self.0.p2()]
    }
}
// directed_line::impl_operations_for_newtype!(Shape<P: PointReqs>);

// line::impl_constructors_for_newtype!(Shape<P: PointReqs>, base= TwoDifferentPoints<P>);
impl<P: PointReqs> Constructors<P> for Shape<P> {
    fn try_new_from_directed_line(line: impl Operations<P>) -> Result<Self, String>
    where
        Self: Sized {
        todo!()
    }
}
// impl<P: PointReqs> line::Constructors<P> for Shape<P> {
//     type TargetShape;
// }

// impl<L> Reversible for L
// where
//     L: Operations + Constructors,
impl<P: PointReqs> Reversible for Shape<P> {
    fn reversed(&self) -> Self {
        todo!()
    }
}

pub trait Constructors<P: PointReqs>: Sized + two_different_points::Constructors<P> {
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
    fn new_from_directed_line(line: impl Operations<P>) -> Self
    where
        Self: Sized,
    {
        Self::try_new_from_directed_line(line).unwrap()
    }
    fn choose_arbitrary_direction_for_line(line: impl line::Operations<P>) -> Self {
        let [p1, p2] = line.two_different_arbitrary_points_on_line();
        Self::from_two_points(p1, p2)
    }
    fn try_new_from_directed_line(line: impl Operations<P>) -> Result<Self, String>
    where
        Self: Sized;
    // TODO: maybe move to Constructors
    fn from_point_and_vector(point: impl Into<P>, direction: impl Into<P>) -> Self {
        let p1 = point.into();
        let v = direction.into();
        let p2 = p1 + v;
        Self::from_two_points_allowing_snap_along_line(p1, p2)
    }
}

impl<P: PointReqs> AbstractsTo<Line<P>> for Shape<P> {
    fn set_with_abstraction(&self, val: &Line<P>) -> Self {
        // TODO: round trip conversion tests: Convert from base type to abstract type, then set the
        // base type from the abstract type again.  Should be unchanged in all cases
        todo!()
    }
}

impl<P: PointReqs> Into<Line<P>> for Shape<P> {
    fn into(self) -> Line<P> {
        Line::<P>::from_line(self)
    }
}

impl<P: PointReqs, T> Constructors<P> for T
where
    T: AbstractionOf<Shape<P>>,
{
    fn try_new_from_directed_line(line: impl Operations<P>) -> Result<Self, String>
    where
        Self: Sized,
    {
        Ok(Shape::<P>::try_new_from_directed_line(line)?.into())
    }
}

impl<P: PointReqs> QuarterTurnRotatable for Shape<P> {
    fn quarter_rotated_ccw(&self, quarter_turns_ccw: impl Into<NormalizedOrthoAngle>) -> Self {
        self.0.quarter_rotated_ccw(quarter_turns_ccw).into()
    }
}
