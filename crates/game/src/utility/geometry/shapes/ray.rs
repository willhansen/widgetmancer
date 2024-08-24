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

impl<P: PointReqs> From<two_different_points::Shape<P>> for Shape<P> {
    fn from(value: two_different_points::Shape<P>) -> Self {
        todo!()
    }
}
impl<P: PointReqs> AbstractsTo<Line<P>> for Shape<P> {
    fn set_with_abstraction(&self, val: &Line<P>) -> Self {
        todo!()
    }
}
