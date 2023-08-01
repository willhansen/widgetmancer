use crate::fov_stuff::{
    single_shadow_square_visibility_from_one_view_arc, RelativeSquareVisibilityMap,
    SquareVisibility,
};
use crate::utility::angle_interval::PartialAngleInterval;
use crate::utility::coordinate_frame_conversions::{StepSet, WorldStep};
use crate::utility::{
    better_angle_from_x_axis, faces_away_from_center_at_rel_square,
    RelativeSquareWithOrthogonalDir, RigidTransform, RigidlyTransformable,
};
use itertools::Itertools;
use ordered_float::OrderedFloat;
use std::collections::HashSet;

#[derive(Debug, Clone, PartialEq)]
pub struct AngleBasedVisibleSegment {
    visible_angle_interval: PartialAngleInterval,
    start_internal_relative_face: Option<RelativeSquareWithOrthogonalDir>,
    end_internal_relative_faces: HashSet<RelativeSquareWithOrthogonalDir>,
}
impl AngleBasedVisibleSegment {
    pub fn validate(&self) {
        if !self.start_face_spans_angle_interval()
            || !self.end_faces_fully_cover_angle_interval_with_no_overlap()
        {
            panic!("INVALID VISIBLE AREA SEGMENT: {:?}", self);
        }
    }
    fn end_faces_fully_cover_angle_interval_with_no_overlap(&self) -> bool {
        let end_faces_going_clockwise =
            self.end_internal_relative_faces
                .iter()
                .sorted_by_key(|face| {
                    OrderedFloat(better_angle_from_x_axis(face.face_center_point()).radians)
                });
        todo!()
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
            end_internal_relative_faces: HashSet::from([actual_face]),
        }
    }
    pub fn from_relative_square(step: WorldStep) -> Self {
        let faces = faces_away_from_center_at_rel_square(step);
        Self {
            visible_angle_interval: PartialAngleInterval::from_relative_square(step),
            start_internal_relative_face: None,
            end_internal_relative_faces: faces,
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
        for step in self
            .visible_angle_interval
            .touched_squares_going_outwards_and_ccw()
        {
            todo!()
        }
        todo!()
    }
    pub fn visibility_of_single_square(&self, rel_square: WorldStep) -> SquareVisibility {
        todo!()
    }
    fn rel_square_is_after_start_line(&self, rel_square: WorldStep) -> bool {
        todo!()
    }
    fn rel_square_is_before_end_fence(&self, rel_square: WorldStep) -> bool {
        todo!()
    }
    fn rel_square_is_past_furthest_part_of_end_fence(&self, rel_square: WorldStep) -> bool {
        todo!()
    }
    pub fn touched_squares_going_outwards_and_ccw(&self) -> impl Iterator<Item = WorldStep> + '_ {
        self.visible_angle_interval
            .touched_squares_going_outwards_and_ccw()
            .filter(|&rel_square| self.rel_square_is_after_start_line(rel_square))
            .filter(|&rel_square| self.rel_square_is_before_end_fence(rel_square))
            .take_while(|&rel_square| {
                !self.rel_square_is_past_furthest_part_of_end_fence(rel_square)
            })
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
            end_internal_relative_faces: self
                .end_internal_relative_faces
                .iter()
                .map(|face| face.apply_rigid_transform(tf))
                .collect(),
        }
    }
}
