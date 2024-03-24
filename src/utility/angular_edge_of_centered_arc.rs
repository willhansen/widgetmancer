use crate::utility::*;

pub trait AngularEdgeOfCenteredArc {
    fn new(edge_angle: FAngle, which_edge: AngularDirection) -> Self;
    fn angle(&self) -> FAngle;
    // fn edge_ray<T: Ray>(&self) -> T;
    // fn border_as_halfplane<T: HalfPlane>(&self) -> T;
    fn outside_angular_direction(&self) -> AngularDirection;
    fn flipped(&self) -> Self
    where
        Self: Sized,
    {
        Self::new(self.angle(), -self.outside_angular_direction())
    }
    fn inside_angular_direction(&self) -> AngularDirection {
        -self.outside_angular_direction()
    }
    fn cw_direction(&self) -> FAngle {
        self.angle().turned_right()
    }
    fn ccw_direction(&self) -> FAngle {
        self.cw_direction().turned_back()
    }
    fn outside_direction(&self) -> FAngle {
        if self.is_cw_edge() {
            self.cw_direction()
        } else {
            self.ccw_direction()
        }
    }
    fn inside_direction(&self) -> FAngle {
        self.outside_direction().turned_back()
    }
    fn is_cw_edge(&self) -> bool {
        self.outside_angular_direction() == AngularDirection::CW
    }
    fn is_ccw_edge(&self) -> bool {
        !self.is_cw_edge()
    }
    fn intersection_with_relative_square(
        &self,
        rel_square: impl IntCoordinate,
    ) -> Option<HalfPlaneCuttingWorldSquare> {
        todo!()
    }
}

// impl AngularEdgeOfCenteredArc for (FAngle, AngularDirection) {}
#[derive(Debug, Copy, Clone, PartialEq)]
pub struct ArcEdge(FAngle, AngularDirection);
impl AngularEdgeOfCenteredArc for ArcEdge {
    fn new(edge_angle: FAngle, which_edge: AngularDirection) -> Self {
        Self(edge_angle, which_edge)
    }

    fn angle(&self) -> FAngle {
        self.0
    }

    fn outside_angular_direction(&self) -> AngularDirection {
        self.1
    }
}
