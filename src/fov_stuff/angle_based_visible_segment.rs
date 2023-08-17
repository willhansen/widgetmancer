use crate::fov_stuff::{
    single_shadow_square_visibility_from_one_view_arc, RelativeSquareVisibilityMap,
    SquareVisibility,
};
use crate::utility::angle_interval::{AngleInterval, PartialAngleInterval};
use crate::utility::coordinate_frame_conversions::{StepSet, WorldStep};
use crate::utility::{
    better_angle_from_x_axis, faces_away_from_center_at_rel_square, CoordToString,
    RelativeSquareWithOrthogonalDir, RigidTransform, RigidlyTransformable, STEP_ZERO,
};
use euclid::point2;
use itertools::Itertools;
use ordered_float::OrderedFloat;
use std::collections::HashSet;

use super::fence::RelativeFenceFullyVisibleFromOriginGoingCcw;

#[derive(Debug, Clone, PartialEq)]
pub struct AngleBasedVisibleSegment {
    visible_angle_interval: PartialAngleInterval,
    start_internal_relative_face: Option<RelativeSquareWithOrthogonalDir>,
    end_fence: RelativeFenceFullyVisibleFromOriginGoingCcw,
}
impl AngleBasedVisibleSegment {
    pub fn validate(&self) {
        if !self.start_face_spans_angle_interval()
            || !self.end_fence_fully_covers_angle_interval_with_no_overlap()
        {
            panic!("INVALID VISIBLE AREA SEGMENT: {:?}", self);
        }
    }
    pub fn end_fence(&self) -> &RelativeFenceFullyVisibleFromOriginGoingCcw {
        &self.end_fence
    }

    fn end_fence_fully_covers_angle_interval_with_no_overlap(&self) -> bool {
        match self.end_fence.spanned_angle_from_origin() {
            AngleInterval::Empty => false,
            AngleInterval::FullCircle => true,
            AngleInterval::PartialArc(arc) => {
                arc.fully_contains_interval_including_edge_overlaps(self.visible_angle_interval)
            }
        }
    }
    fn start_face_spans_angle_interval(&self) -> bool {
        self.start_internal_relative_face.is_none()
            || self.start_internal_relative_face.is_some_and(
                |face: RelativeSquareWithOrthogonalDir| {
                    !face.line().parallel_directions().iter().any(|&line_angle| {
                        !self
                            .visible_angle_interval
                            .contains_or_touches_angle(line_angle)
                    })
                },
            )
    }
    pub fn from_relative_face(relative_face: impl Into<RelativeSquareWithOrthogonalDir>) -> Self {
        let actual_face = relative_face.into();
        Self {
            visible_angle_interval: PartialAngleInterval::from_relative_square_face(actual_face),
            start_internal_relative_face: None,
            end_fence: RelativeFenceFullyVisibleFromOriginGoingCcw::from_ccw_relative_edges(vec![
                actual_face,
            ]),
        }
    }
    pub fn from_relative_square(step: WorldStep) -> Self {
        let faces = faces_away_from_center_at_rel_square(step);
        Self {
            visible_angle_interval: PartialAngleInterval::from_relative_square(step),
            start_internal_relative_face: None,
            end_fence: RelativeFenceFullyVisibleFromOriginGoingCcw::from_unordered_relative_edges(
                faces,
            ),
        }
    }
    pub fn with_weakly_applied_start_face(
        &self,
        relative_face: RelativeSquareWithOrthogonalDir,
    ) -> Self {
        if self.start_internal_relative_face.is_some() {
            return self.clone();
        }

        let thing = Self {
            start_internal_relative_face: Some(relative_face),
            ..self.clone()
        };
        assert!(thing.start_face_spans_angle_interval());
        thing
    }
    pub fn with_visible_angle_interval(&self, angle_interval: PartialAngleInterval) -> Self {
        Self {
            visible_angle_interval: angle_interval,
            ..self.clone()
        }
    }
    pub fn get_touching_relative_squares(&self) -> StepSet {
        self.touched_squares_going_outwards_and_ccw().collect()
    }
    pub fn visibility_of_single_square(&self, rel_square: WorldStep) -> SquareVisibility {
        todo!()
    }
    fn rel_square_is_after_start_line(&self, rel_square: WorldStep) -> bool {
        if let Some(line) = self.start_internal_relative_face.map(|face| face.line()) {
            // TODO: generalize to allow passing in the relative squares, not needing absolute points
            line.same_side_of_line(rel_square.to_point().to_f32(), point2(0.0, 0.0))
        } else {
            true
        }
    }
    fn rel_square_is_before_end_fence(&self, rel_square: WorldStep) -> bool {
        self.end_fence().is_radially_inside_fence(rel_square)
    }
    fn rel_square_is_past_furthest_part_of_end_fence(&self, rel_square: WorldStep) -> bool {
        todo!()
    }
    // if there are several equally distant, selects one somehow
    fn furthest_overlapping_square(&self) -> WorldStep {
        self.end_fence.furthest_inside_square()
    }
    /// This iterator ends when squares in the segment run out
    pub fn touched_squares_going_outwards_and_ccw(&self) -> impl Iterator<Item = WorldStep> + '_ {
        let max_square_length = self.furthest_overlapping_square().square_length();
        self.visible_angle_interval
            .touched_squares_going_outwards_and_ccw()
            .take_while(move |&rel_square| dbg!(rel_square.square_length()) <= max_square_length)
            .filter(|&rel_square| self.rel_square_is_after_start_line(rel_square))
            .filter(|&rel_square| self.rel_square_is_before_end_fence(rel_square))
    }
    pub fn to_square_visibilities(&self) -> RelativeSquareVisibilityMap {
        // A visible segment has two edges to it's view arc, and those are the only things that can split one of these squares.
        // Watch out for the wraparound case.
        self.touched_squares_going_outwards_and_ccw()
            .map(|rel_square| {
                (
                    rel_square,
                    single_shadow_square_visibility_from_one_view_arc(
                        self.visible_angle_interval,
                        rel_square,
                    )
                    .unwrap(),
                )
                //TODO: change shadow type
                // SquareVisibilityFromPointSource::from_single_visible_arc(
                //     rel_square,
                //     self.visible_angle_interval,
                // ),
            })
            .collect()
    }
}
// TODO: Applying a trait by simply calling the functions on every component member of a struct should be automated.
impl RigidlyTransformable for AngleBasedVisibleSegment {
    fn apply_rigid_transform(&self, tf: RigidTransform) -> Self {
        Self {
            visible_angle_interval: self.visible_angle_interval.apply_rigid_transform(tf),
            start_internal_relative_face: self
                .start_internal_relative_face
                .map(|face| face.apply_rigid_transform(tf)),
            end_fence: self.end_fence.apply_rigid_transform(tf),
        }
    }
}
#[cfg(test)]
mod tests {
    use crate::utility::STEP_RIGHT;

    use super::*;
    #[test]
    fn test_squares_touched_by_angle_based_visible_segment__simple_horizontal() {
        let seg = AngleBasedVisibleSegment::from_relative_face((STEP_RIGHT * 5, STEP_RIGHT));
        assert_eq!(
            seg.get_touching_relative_squares(),
            (0..=5).map(|i| STEP_RIGHT * i).collect()
        )
    }
}
