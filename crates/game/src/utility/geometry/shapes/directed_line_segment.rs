use crate::utility::*;

trait_alias!(pub trait PointReqs = signed_coordinate::Operations);

#[derive(Clone, PartialEq, Debug, Copy, Hash, Eq)]
pub struct Shape<PointType: PointReqs>(TwoDifferentPoints<PointType>);

pub trait Operations<P: PointReqs>:
    directed_line::Operations<P> + line_segment::Operations<P>
{
    fn endpoints_in_order(&self) -> [P; 2] {
        P::points_sorted_along_axis(self.endpoints_in_arbitrary_order(), self.direction())
            .into_iter()
            .collect_vec()
            .try_into()
            .unwrap()
    }
    fn start(&self) -> P {
        self.endpoints_in_order()[0]
    }
    fn end(&self) -> P {
        self.endpoints_in_order()[1]
    }
}
impl<T, P: PointReqs> Operations<P> for T where
    T: directed_line::Operations<P> + line_segment::Operations<P>
{
}
pub trait Constructors<P: PointReqs>{}

impl<P: PointReqs> Constructors<P> for self::Shape<P> {}

impl<P: PointReqs> From<TwoDifferentPoints<P>> for Shape<P> {
    fn from(value: TwoDifferentPoints<P>) -> Self {
        Self(value)
    }
}
impl<P: PointReqs> AbstractsTo<LineSegment<P>> for Shape<P> {
    fn set_with_abstraction(&self, val: &LineSegment<P>) -> Self {
        todo!()
    }
}
