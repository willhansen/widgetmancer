use crate::utility::*;

pub trait Ray {
    type PointType: SignedCoordinate;
    fn new_from_point_and_dir(point: Self::PointType, dir: FAngle) -> Self;
    fn point(&self) -> Self::PointType;
    fn angle(&self) -> FAngle;
    fn point_on_ray(
        &self,
        dist_from_start: <Self::PointType as Coordinate>::DataType,
    ) -> Self::PointType
    where
        Self::PointType: FloatCoordinate,
    {
        self.point() + Self::PointType::from_angle_and_length(self.angle(), dist_from_start)
    }
    fn line<T: DirectedLineTrait>(&self) -> T {
        T::from_point_and_vector(self.point(), self.angle())
    }
}
