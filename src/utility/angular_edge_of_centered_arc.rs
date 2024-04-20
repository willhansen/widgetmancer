use crate::utility::*;

pub trait AngularEdgeOfCenteredArc {
    fn new(edge_angle: FAngle, which_edge: AngularDirection) -> Self;
    fn angle(&self) -> FAngle;
    fn edge_ray<R: Ray>(&self) -> R {
        R::new_from_point_and_dir(R::PointType::zero(), self.angle())
    }
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
    fn intersection_with_relative_square<SquareType: IntCoordinateOps>(
        &self,
        rel_square: SquareType,
    ) -> Option<HalfPlaneCuttingSquare<SquareType>> {
        if rel_square.is_zero() {
            return None;
        }

        if rel_square.position_on_axis(self.angle()) < 0.0 {
            return None;
        }

        let point_on_inside_of_edge =
            SquareType::Floating::unit_vector_from_angle(self.inside_direction());

        let edge_ray: TwoDifferentPoints<SquareType::Floating> = self.edge_ray();
        let line =
            TwoPointsOnDifferentFacesOfGridSquare::<SquareType::Floating>::try_new_from_line_and_square(
                edge_ray, rel_square,
            )
            .ok()?;

        Some(
            HalfPlaneCuttingSquare::<SquareType>::new_from_line_and_point_on_half_plane(
                line,
                point_on_inside_of_edge,
            ),
        )
    }
}

// impl AngularEdgeOfCenteredArc for (FAngle, AngularDirection) {}
#[derive(Debug, Copy, Clone, PartialEq)]
pub struct AngularEdge(FAngle, AngularDirection);
impl AngularEdgeOfCenteredArc for AngularEdge {
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
