use crate::utility::*;

// A trait intended to only be used while replacing one partial square visibility type with another, to allow testing for common functionality in the transition.
pub trait PartialSquareVisibilityOps: Copy {
    // TODO: make a Complement trait
    fn complement(&self) -> Self;
    fn half_visible(shadow_direction: Angle<f32>) -> Self;
    fn new_from_visible_half_plane(visible_portion: HalfPlaneCuttingLocalSquare) -> Self;
    fn new_orthogonal_half_visible(which_half_visible: impl Into<OrthogonalDirection>) -> Self
    where
        Self: Sized,
    {
        Self::half_visible((-which_half_visible.into()).angle().into())
    }
}
