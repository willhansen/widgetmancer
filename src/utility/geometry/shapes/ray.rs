use crate::utility::*;

trait_alias!(pub trait PointReqs = float_coordinate::Operations);

pub struct Shape<PointType: PointReqs>(TwoDifferentPoints<PointType>);

pub trait Operations<P: PointReqs>: Constructors<P> {
    // TODO: allow for intCoordinate-based rays
    fn new_from_point_and_dir(point: P, dir: FAngle) -> Self;
    fn point(&self) -> P;
    fn angle(&self) -> FAngle;
    fn point_on_ray(&self, dist_from_start: <P as coordinate::Operations>::DataType) -> P {
        self.point() + P::from_angle_and_length(self.angle(), dist_from_start)
    }
    fn line<T: directed_line::Operations<P>>(&self) -> T {
        T::from_point_and_vector(self.point(), P::unit_vector_from_angle(self.angle()))
    }
}
pub trait Constructors<P: PointReqs> {}
