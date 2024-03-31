use crate::utility::*;

pub trait DirectedLine: Line + DirectedLineLike + Reversible {
    fn from_other_directed_line<OtherLine>(other: OtherLine) -> Self
    where
        OtherLine: DirectedLineLike<PointType = Self::PointType>,
    {
        Self::from_point_array(other.two_points_on_line_in_order())
    }
}
impl<L> DirectedLine for L where L: Line + DirectedLineLike + Reversible {}
