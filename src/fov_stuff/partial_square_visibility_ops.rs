use crate::utility::*;

pub trait PartialSquareVisibilityOps: Copy {
    // TODO: make a Complement trait
    fn complement(&self) -> Self;
    fn half_visible(shadow_direction: Angle<f32>) -> Self;
    fn new_orthogonal_half_visible(which_half_visible: impl Into<OrthogonalDirection>) -> Self
    where
        Self: Sized,
    {
        Self::half_visible((-which_half_visible.into()).angle().into())
    }
}
