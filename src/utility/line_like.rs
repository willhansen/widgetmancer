use crate::utility::*;

/// 1D shapes in the 2D plane
/// includes LineSegments, Rays, and EndlessLines
pub trait LineLike:
    Sized
    + Copy
    + QuarterTurnRotatable
    + Debug
    + Sub<Self::PointType, Output = Self>
    + Add<Self::PointType, Output = Self>
{
    type PointType: SignedCoordinate;
    // type DataType = <Self::PointType as Coordinate>::DataType;
    fn two_different_arbitrary_points_on_line(&self) -> [Self::PointType; 2];

    fn arbitrary_point_on_line(&self) -> Self::PointType {
        self.two_different_arbitrary_points_on_line()[0]
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
    fn parallel_directions_as_vectors(&self) -> [Self::PointType; 2] {
        let [p1, p2] = self.two_different_arbitrary_points_on_line();
        [p2 - p1, p1 - p2]
    }
    fn parallel_directions(&self) -> [Angle<f32>; 2] {
        self.parallel_directions_as_vectors()
            .map(|p| p.better_angle_from_x_axis())
    }
    fn perpendicular_directions(&self) -> [Angle<f32>; 2] {
        self.parallel_directions().map(|d| d.turned_right())
    }
    fn to_line<T: Line<PointType = Self::PointType>>(&self) -> T {
        T::from_line_like(*self)
    }
}
