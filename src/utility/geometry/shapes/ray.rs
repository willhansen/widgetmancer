use crate::utility::*;

trait_alias!(pub trait PointReqs = FloatCoordinateOps);

pub trait Shape<P: PointReqs> {
    // TODO: allow for intCoordinate-based rays
    fn new_from_point_and_dir(point: P, dir: FAngle) -> Self;
    fn point(&self) -> P;
    fn angle(&self) -> FAngle;
    fn point_on_ray(&self, dist_from_start: <P as CoordinateOps>::DataType) -> P {
        self.point() + P::from_angle_and_length(self.angle(), dist_from_start)
    }
    fn line<T: directed_line::Operations<P>>(&self) -> T {
        T::from_point_and_vector(self.point(), P::unit_vector_from_angle(self.angle()))
    }
}
pub trait Operations<P: PointReqs>: Constructors<P> {}
pub trait Constructors<P: PointReqs> {}
