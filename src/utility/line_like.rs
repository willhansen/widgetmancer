use crate::utility::*;

pub trait LineLike: Sized + Copy + QuarterTurnRotatable + Debug {
    type PointType: SignedCoordinate;
    // type DataType = <Self::PointType as Coordinate>::DataType;
    fn new_from_two_points_on_line(
        p1: impl Into<Self::PointType>,
        p2: impl Into<Self::PointType>,
    ) -> Self;

    fn two_different_arbitrary_points_on_line(&self) -> [Self::PointType; 2];
    fn from_array(points: [Self::PointType; 2]) -> Self {
        Self::new_from_two_points_on_line(points[0], points[1])
    }

    fn arbitrary_point_on_line(&self) -> Self::PointType {
        self.two_different_arbitrary_points_on_line()[0]
    }

    fn from_other_line<OtherLine>(other_line: OtherLine) -> Self
    where
        OtherLine: LineLike<PointType = Self::PointType>,
    {
        let [p1, p2] = other_line.two_different_arbitrary_points_on_line();
        Self::new_from_two_points_on_line(p1, p2)
    }

    fn new_horizontal(y: <Self::PointType as Coordinate>::DataType) -> Self {
        Self::new_from_two_points_on_line(
            Self::PointType::new(<Self::PointType as Coordinate>::DataType::zero(), y),
            Self::PointType::new(<Self::PointType as Coordinate>::DataType::one(), y),
        )
    }
    fn new_vertical(x: <Self::PointType as Coordinate>::DataType) -> Self {
        Self::new_from_two_points_on_line(
            Self::PointType::new(x, <Self::PointType as Coordinate>::DataType::zero()),
            Self::PointType::new(x, <Self::PointType as Coordinate>::DataType::one()),
        )
    }
    fn new_through_origin(second_point: impl Into<Self::PointType>) -> Self {
        Self::new_from_two_points_on_line(
            <Self::PointType as euclid::num::Zero>::zero(),
            second_point,
        )
    }
    fn from_point_and_vector(
        point: impl Into<Self::PointType>,
        direction: impl Into<Self::PointType>,
    ) -> Self {
        let p1 = point.into();
        let v = direction.into();
        let p2 = p1 + v;
        Self::new_from_two_points_on_line(p1, p2)
    }
    fn is_orthogonal(&self) -> bool {
        let [p1, p2] = self.two_different_arbitrary_points_on_line();
        p1.x() == p2.x() || p1.y() == p2.y()
    }
    fn x_intercept(&self) -> Option<f32> {
        if self.is_vertical() {
            let p = self.arbitrary_point_on_line();
            return Some(p.to_f32().x());
        }
        if self.is_horizontal() {
            return None;
        }
        Some(-self.y_intercept().unwrap() / self.slope().unwrap())
    }
    fn y_intercept(&self) -> Option<f32> {
        if self.is_vertical() {
            return None;
        }
        let p = self.arbitrary_point_on_line().to_f32();
        Some(p.y() - self.slope().unwrap() * p.x())
    }
    fn is_vertical(&self) -> bool {
        let [p1, p2] = self.two_different_arbitrary_points_on_line();
        p1.x() == p2.x()
    }
    fn is_horizontal(&self) -> bool {
        let [p1, p2] = self.two_different_arbitrary_points_on_line();
        p1.y() == p2.y()
    }
    fn slope(&self) -> Option<f32> {
        if self.is_vertical() {
            return None;
        }
        let [p1, p2] = self
            .two_different_arbitrary_points_on_line()
            .map(|a| a.to_f32());
        let (l, r) = if p1.x() < p2.x() { (p1, p2) } else { (p2, p1) };
        Some((r.y() - l.y()) / (r.x() - l.x()))
    }
}
