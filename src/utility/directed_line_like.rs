use crate::utility::*;

pub trait DirectedLineLike: LineLike {
    fn two_points_on_line_in_order(&self) -> [Self::PointType; 2];
    fn arbitrary_vector_along_line(&self) -> Self::PointType {
        let [p1, p2] = self.two_points_on_line_in_order();
        p2 - p1
    }
    fn direction(&self) -> FAngle {
        self.arbitrary_vector_along_line()
            .better_angle_from_x_axis()
    }
    fn from_other_directed_line<OtherLine>(other: OtherLine) -> Self
    where
        OtherLine: DirectedLineLike<PointType = Self::PointType>,
    {
        Self::from_array(other.two_points_on_line_in_order())
    }
    fn reversed(&self) -> Self {
        let [p1, p2] = self.two_points_on_line_in_order();
        Self::new_from_two_points_on_line(p2, p1)
    }
    fn arbitrary_point_clockwise_of_line(&self) -> Self::PointType {
        self.arbitrary_point_on_line() + self.arbitrary_vector_along_line().quarter_rotated_ccw(-1)
    }
    fn arbitrary_point_anticlockwise_of_line(&self) -> Self::PointType {
        self.arbitrary_point_on_line() + self.arbitrary_vector_along_line().quarter_rotated_ccw(1)
    }
    fn arbitrary_point_right_of_line(&self) -> Self::PointType {
        self.arbitrary_point_clockwise_of_line()
    }
    fn arbitrary_point_left_of_line(&self) -> Self::PointType {
        self.arbitrary_point_anticlockwise_of_line()
    }
}
