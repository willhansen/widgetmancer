use crate::utility::*;

#[derive(Clone, PartialEq, Debug, Copy, Hash, Eq)]
pub struct DirectedLine<PointType: CoordinateOps>(TwoDifferentPoints<PointType>);

pub trait DirectedLineOps: LineOps + Reversible {
    // TODO: maybe no constructors in these operation collections?

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
    P: CoordinateOps,
{
    type PointType = P;

    fn two_different_arbitrary_points_on_line(&self) -> [Self::PointType; 2] {
        todo!()
    }
}

impl<P> DirectedLineOps for DirectedLine<P>
where
    P: CoordinateOps,
    DirectedLine<P>: LineOps<PointType = P>,
{
    fn two_points_on_line_in_order(&self) -> [Self::PointType; 2] {
        todo!()
    }
}

// impl<L> Reversible for L
// where
//     L: DirectedLineOps + DirectedLineConstructors,
impl<P: CoordinateOps> Reversible for DirectedLine<P> {
    fn reversed(&self) -> Self {
        todo!()
    }
}
