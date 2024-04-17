use crate::fov_stuff::*;
use derive_more::Constructor;

// TODO: more than one cone
#[derive(Clone, Constructor, Debug)]
pub struct PartialSquareVisibilityFromFovCones(UnboundConvexPolygon<LineCuttingLocalSquare>);

impl PartialSquareVisibilityFromFovCones {}

impl_quarter_turn_rotatable_for_newtype!(PartialSquareVisibilityFromFovCones);

impl PartialSquareVisibilityOps for PartialSquareVisibilityFromFovCones {
    fn complement(&self) -> Self {
        todo!()
    }

    fn where_border_touches_unit_square(&self) -> Vec<LocalSquarePoint> {
        todo!()
    }

    fn is_within_distance_of_covering_centered_unit_square(&self, distance: f32) -> bool {
        todo!()
    }

    fn combined_increasing_visibility(&self, other: &Self) -> SquareVisibility<Self> {
        todo!()
    }

    fn new_from_visible_half_plane(visible_portion: HalfPlaneCuttingLocalSquare) -> Self {
        todo!()
    }
}

#[cfg(test)]
mod tests {
    use ntest::assert_about_eq;

    use super::*;
}
