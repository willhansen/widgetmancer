use crate::utility::*;

// A trait intended to only be used while replacing one partial square visibility type with another, to allow testing for common functionality in the transition.
pub trait PartialSquareVisibilityOps: Copy {
    // TODO: make a Complement trait
    fn complement(&self) -> Self;
    fn half_visible(shadow_direction: Angle<f32>) -> Self {
        // TODO: may be backwards
        let shadow_direction = standardize_angle_with_zero_mid(shadow_direction);
        let shadow_line = TwoPointsOnDifferentFacesOfCenteredUnitSquare::new_through_origin(
            LocalSquarePoint::unit_vector_from_angle(shadow_direction.turned_left()),
        );
        Self::new_from_visible_half_plane(
            HalfPlaneCuttingLocalSquare::new_from_line_and_point_on_half_plane(
                shadow_line,
                LocalSquarePoint::unit_vector_from_angle(shadow_direction),
            ),
        )
    }
    fn new_from_visible_half_plane(visible_portion: HalfPlaneCuttingLocalSquare) -> Self;
    fn new_orthogonal_half_visible(which_half_visible: impl Into<OrthogonalDirection>) -> Self
    where
        Self: Sized,
    {
        Self::half_visible((-which_half_visible.into()).angle().into())
    }
}
