use crate::{
    angled_blocks::angle_block_char_complement, fov_stuff::*,
    graphics::drawable::PartialVisibilityDrawable,
};

#[derive(PartialEq, Clone, Copy, Debug)]
pub struct PartialSquareVisibilityByOneVisibleHalfPlane(HalfPlaneCuttingLocalSquare);

impl PartialSquareVisibilityByOneVisibleHalfPlane {
    pub fn new(x: HalfPlaneCuttingLocalSquare) -> Self {
        Self(x)
    }
    pub fn half_plane(&self) -> HalfPlaneCuttingLocalSquare {
        self.0
    }
}
impl PartialSquareVisibilityOps for PartialSquareVisibilityByOneVisibleHalfPlane {
    fn new_from_visible_half_plane(visible_portion: HalfPlaneCuttingLocalSquare) -> Self {
        Self(visible_portion)
    }
    fn complement(&self) -> Self {
        Self(self.0.complement())
    }
    fn where_border_touches_unit_square(&self) -> Vec<LocalSquarePoint> {
        self.0
            .dividing_line()
            .ordered_line_intersections_with_centered_unit_square()
    }
    fn is_within_distance_of_covering_centered_unit_square(&self, distance: f32) -> bool {
        self.0
            .fully_covers_centered_unit_square_with_tolerance(distance)
            .is_partial()
    }
}

impl_quarter_turn_rotatable_for_newtype!(PartialSquareVisibilityByOneVisibleHalfPlane);
