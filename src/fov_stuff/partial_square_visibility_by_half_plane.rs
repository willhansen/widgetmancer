use crate::{
    angled_blocks::angle_block_char_complement, fov_stuff::*,
    graphics::drawable::PartialVisibilityDrawable,
};

#[derive(PartialEq, Clone, Copy, Debug)]
pub struct PartialSquareVisibilityByOneVisibleHalfPlane(HalfPlaneCuttingLocalSquare);

impl PartialSquareVisibilityOps for PartialSquareVisibilityByOneVisibleHalfPlane {
    fn complement(&self) -> Self {
        Self(self.0.complement())
    }
    fn half_visible(shadow_direction: Angle<f32>) -> Self {
        // TODO: may be backwards
        let shadow_direction = standardize_angle_with_zero_mid(shadow_direction);
        let shadow_line = TwoPointsOnDifferentFacesOfCenteredUnitSquare::new_through_origin(
            LocalSquarePoint::unit_vector_from_angle(shadow_direction.turned_left()),
        );
        Self(
            HalfPlaneCuttingLocalSquare::new_from_line_and_point_on_half_plane(
                shadow_line,
                LocalSquarePoint::unit_vector_from_angle(shadow_direction),
            ),
        )
    }
}

impl PartialSquareVisibilityByOneVisibleHalfPlane {
    pub fn new(x: HalfPlaneCuttingLocalSquare) -> Self {
        Self(x)
    }
    pub fn half_plane(&self) -> HalfPlaneCuttingLocalSquare {
        self.0
    }
    pub fn is_within_distance_of_covering_centered_unit_square(&self, distance: f32) -> bool {
        self.0
            .fully_covers_centered_unit_square_with_tolerance(distance)
            .is_partial()
    }
    pub fn where_border_touches_unit_square(&self) -> Vec<LocalSquarePoint> {
        self.0
            .dividing_line()
            .ordered_line_intersections_with_centered_unit_square()
    }
}
