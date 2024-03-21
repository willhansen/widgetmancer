use crate::utility::*;

pub trait AngularEdgeOfCenteredArc {
    fn edge_direction(&self) -> FAngle;
    fn edge_ray<T: Coordinate>(&self) -> impl RayTrait<T>;
    fn border_as_halfplane<T: Coordinate>(&self) -> HalfPlane<FloatLineTrait<T>>;
    fn outside_angular_direction(&self) -> AngularDirection;
    fn inside_angular_direction(&self) -> AngularDirection {
        -self.outside_angular_direction()
    }
    fn is_cw_edge(&self) -> bool {
        self.outside_angular_direction() == AngularDirection::CW
    }
    fn is_ccw_edge(&self) -> bool {
        !self.is_cw_edge()
    }
}

impl<P> AngularEdgeOfCenteredArc<P> for (FAngle, AngularDirection) {}

#[derive(Default, Debug, Clone, PartialEq, CopyGetters)]
#[get_copy = "pub"]
// TODO: might be unused
pub struct DirectionalAngularEdge {
    angle: Angle<f32>,
    is_clockwise_edge: bool,
}

impl DirectionalAngularEdge {
    pub fn new(angle: Angle<f32>, is_clockwise_edge: bool) -> Self {
        DirectionalAngularEdge {
            angle: standardize_angle(angle),
            is_clockwise_edge,
        }
    }
    pub fn flipped(&self) -> Self {
        DirectionalAngularEdge {
            angle: self.angle,
            is_clockwise_edge: !self.is_clockwise_edge,
        }
    }
    pub fn direction_to_inside(&self) -> Angle<f32> {
        let rotation_sign = if self.is_clockwise_edge { 1.0 } else { -1.0 };
        standardize_angle(self.angle + Angle::degrees(rotation_sign * 90.0))
    }
}
