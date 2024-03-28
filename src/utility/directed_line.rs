use crate::utility::*;

pub trait DirectedLine: Line + DirectedLineLike {
    fn from_other_directed_line<OtherLine>(other: OtherLine) -> Self
    where
        OtherLine: DirectedLineLike<PointType = Self::PointType>,
    {
        Self::from_point_array(other.two_points_on_line_in_order())
    }
    fn reversed(&self) -> Self {
        let [p1, p2] = self.two_points_on_line_in_order();
        Self::new_from_two_points_on_line(p2, p1)
    }
}
impl<L> DirectedLine for L where L: Line + DirectedLineLike {}
