use crate::fov_stuff::*;

#[derive(Clone, Copy, Constructor, Debug)]
pub struct PartialSquareVisibilityFromFovCones {
    // TODO: more than one cone
    visible_cone: ConeEdgeIntersectionsWithCenteredUnitSquare,
}

impl PartialSquareVisibilityFromFovCones {
    pub fn from_cones(c: Vec<ConeEdgeIntersectionsWithCenteredUnitSquare>) -> Self {
        Self { visible_cone: c[0] }
    }
    pub fn cones(&self) -> Vec<ConeEdgeIntersectionsWithCenteredUnitSquare> {
        vec![self.visible_cone]
    }

    pub fn is_fully_visible(&self) -> bool {
        todo!()
    }
    pub fn from_single_visible_arc(rel_square: WorldStep, visible_arc: AngleInterval) -> Self {
        todo!()
    }
}
impl QuarterTurnRotatable for PartialSquareVisibilityFromFovCones {
    fn quarter_rotated_ccw(&self, quarter_turns_ccw: impl Into<NormalizedOrthoAngle>) -> Self {
        let quarter_turns_ccw = quarter_turns_ccw.into();
        Self::from_cones(self.cones().quarter_rotated_ccw(quarter_turns_ccw))
    }
}

impl PartialSquareVisibilityOps for PartialSquareVisibilityFromFovCones {
    fn complement(&self) -> Self {
        todo!()
    }

    fn half_visible(shadow_direction: Angle<f32>) -> Self {
        todo!()
    }

    fn new_from_visible_half_plane(visible_portion: HalfPlaneCuttingLocalSquare) -> Self {
        todo!()
    }
}
