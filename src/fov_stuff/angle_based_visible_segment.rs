use crate::fov_stuff::{LocalVisibilityMap, SquareVisibility};
use crate::utility::angle_interval::{AngleInterval, PartialAngleInterval};
use crate::utility::circular_interval::circular_merging;
use crate::utility::coordinate_frame_conversions::{StepSet, WorldStep};
use crate::utility::coordinates::FAngle;
use crate::utility::poses::RelativeFace;
use crate::utility::{
    better_angle_from_x_axis, faces_away_from_center_at_rel_square, CoordToString,
    RelativeSquareWithOrthogonalDir, RigidTransform, RigidlyTransformable, STEP_ZERO,
};
use euclid::{point2, Angle};
use itertools::Itertools;
use ordered_float::OrderedFloat;
use std::collections::HashSet;
use std::fmt::{Debug, Formatter};

use super::fence::{Fence, RelativeFenceFullyVisibleFromOriginGoingCcw};
use super::square_visibility::RelativeSquareVisibilityFunctions;

#[derive(Clone, PartialEq)]
pub struct AngleBasedVisibleSegment {
    visible_arc: AngleInterval,
    end_fence: RelativeFenceFullyVisibleFromOriginGoingCcw,
    start_internal_relative_face: Option<RelativeSquareWithOrthogonalDir>,
}
impl AngleBasedVisibleSegment {
    pub fn new(arc: impl Into<AngleInterval>, end_fence: impl Into<Fence>) -> Self {
        Self::new_with_optional_start_face(arc, end_fence, Option::<RelativeFace>::None)
    }
    fn default_angle_tolerance() -> FAngle {
        Angle::radians(1e-6)
    }

    pub fn new_with_start_face(
        arc: impl Into<AngleInterval>,
        end_fence: Fence,
        start_face: impl Into<RelativeFace>,
    ) -> Self {
        Self::new_with_optional_start_face(arc, end_fence, Some(start_face.into()))
    }
    pub fn new_with_optional_start_face(
        arc: impl Into<AngleInterval>,
        end_fence: impl Into<Fence>,
        optional_start_face: Option<impl Into<RelativeFace>>,
    ) -> Self {
        let x = Self {
            visible_arc: arc.into(),
            end_fence: end_fence.into(),
            start_internal_relative_face: optional_start_face
                .map(|y| y.into().flipped_to_face_origin()),
        };
        x.validate();
        x
    }
    pub fn validate(&self) {
        if !self.start_face_spans_angle_interval() {
            panic!("START FACE DOES NOT SPAN ARC:\n{:?}", self);
        }
        if !self.end_fence_fully_covers_angle_interval() {
            panic!("END FENCE DOES NOT SPAN ARC:\n{:?}", self);
        }
    }
    pub fn end_fence(&self) -> &RelativeFenceFullyVisibleFromOriginGoingCcw {
        &self.end_fence
    }

    fn end_fence_fully_covers_angle_interval(&self) -> bool {
        // TODO: use standard tolerance
        self.end_fence
            .spanned_angle_from_origin()
            .contains_arc_with_tolerance(self.visible_arc, Angle::radians(0.01))
            .is_at_least_partial()
    }
    fn start_face_spans_angle_interval(&self) -> bool {
        if self.start_internal_relative_face.is_none() {
            return true;
        }

        let start_face = self.start_internal_relative_face.unwrap();

        let any_line_end_points_within_angle_interval = start_face
            .face_line_segment()
            .parallel_directions()
            .iter()
            .any(|&line_angle| {
                let interval_includes_line_end = self
                    .visible_arc
                    .contains_angle_with_tolerance(line_angle, todo!())
                    .is_at_least_partial();
                interval_includes_line_end
            });

        let start_line = start_face.face_line_segment();
        let vector_to_line_from_origin = start_line.normal_vector_from_origin();
        if vector_to_line_from_origin.square_length() == 0.0 {
            return false;
        }

        let angle_span_of_extended_line_as_seen_from_origin =
            PartialAngleInterval::from_center_and_width(
                better_angle_from_x_axis(vector_to_line_from_origin),
                FAngle::degrees(180.0),
            );

        angle_span_of_extended_line_as_seen_from_origin
            .contains_arc_with_tolerance(self.visible_arc, Self::default_angle_tolerance())
            .is_at_least_partial()
    }
    pub fn from_relative_face(relative_face: impl Into<RelativeSquareWithOrthogonalDir>) -> Self {
        let actual_face = relative_face.into();
        Self::new(
            PartialAngleInterval::from_relative_square_face(actual_face),
            RelativeFenceFullyVisibleFromOriginGoingCcw::from_faces_in_ccw_order(vec![actual_face]),
        )
    }
    pub fn from_relative_square(step: impl Into<WorldStep>) -> Self {
        let step = step.into();
        let arc = PartialAngleInterval::from_relative_square(step);

        let faces = faces_away_from_center_at_rel_square(step);
        let end_fence =
            RelativeFenceFullyVisibleFromOriginGoingCcw::from_unordered_relative_edges(faces);

        Self::new(arc, end_fence)
    }
    pub fn from_arc_and_fence_radius(arc: AngleInterval, fence_radius: u32) -> Self {
        Self::new(arc, Fence::from_radius_and_arc(fence_radius, arc))
    }
    pub fn with_weakly_applied_start_face(
        &self,
        relative_face: impl Into<RelativeSquareWithOrthogonalDir>,
    ) -> Self {
        if self.start_internal_relative_face.is_some() {
            return self.clone();
        }

        self.with_start_face(relative_face)
    }
    pub fn with_start_face(&self, relative_face: impl Into<RelativeFace>) -> Self {
        Self::new_with_start_face(self.visible_arc, self.end_fence.clone(), relative_face)
    }
    pub fn with_arc(&self, arc: PartialAngleInterval) -> Self {
        Self::new_with_optional_start_face(
            arc,
            self.end_fence.clone(),
            self.start_internal_relative_face,
        )
    }
    pub fn get_touching_relative_squares(&self) -> StepSet {
        self.touched_squares_going_outwards_and_ccw().collect()
    }
    pub fn visibility_of_single_square(&self, rel_square: WorldStep) -> SquareVisibility {
        todo!()
    }
    pub fn combine_multiple(unsorted_segments: impl IntoIterator<Item = Self>) -> Vec<Self> {
        let sorted_ccw = unsorted_segments.into_iter().sorted_by_key(|segment| {
            OrderedFloat(match segment.visible_arc {
                AngleInterval::PartialArc(partial_arc) => partial_arc.center_angle().radians,
                _ => 0.0,
            })
        });

        let reduction_function = |a: &Self, b: &Self| -> Option<Self> { a.combined_with(b) };

        circular_merging(sorted_ccw, reduction_function)
    }
    pub fn combined_with(&self, other: &Self) -> Option<Self> {
        let a = self;
        let b = other;

        let same_start_line = a.start_internal_relative_face.is_some_and(|a_start_face| {
            b.start_internal_relative_face
                .is_some_and(|b_start_face| a_start_face.face_is_on_same_line(b_start_face))
        });
        let both_no_start_line =
            a.start_internal_relative_face.is_none() && b.start_internal_relative_face.is_none();

        if !(both_no_start_line || same_start_line) {
            return None;
        }

        let common_start_line = a.start_internal_relative_face;

        let maybe_combined_arc: Option<AngleInterval> =
            self.visible_arc.combine_if_touching_panic_if_overlapping(
                other.visible_arc,
                Self::default_angle_tolerance(),
            );
        if maybe_combined_arc.is_none() {
            return None;
        }
        let combined_arc = maybe_combined_arc.unwrap();

        let maybe_combined_fence: Option<Fence> = self
            .end_fence
            .try_concatenate_allowing_one_edge_of_overlap(&other.end_fence)
            .ok();

        if maybe_combined_fence.is_none() {
            return None;
        }
        let combined_fence = maybe_combined_fence.unwrap();

        Some(Self::new_with_optional_start_face(
            combined_arc,
            combined_fence,
            common_start_line,
        ))
    }
    fn rel_square_is_after_start_line(&self, rel_square: WorldStep) -> bool {
        if let Some(line) = self
            .start_internal_relative_face
            .map(|face| face.face_line_segment())
        {
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
        self.visible_arc
            .touched_squares_going_outwards_and_ccw()
            .take_while(move |&rel_square| rel_square.square_length() <= max_square_length)
            .filter(|&rel_square| self.rel_square_is_after_start_line(rel_square))
            .filter(|&rel_square| self.rel_square_is_before_end_fence(rel_square))
    }
    pub fn to_square_visibilities(&self) -> LocalVisibilityMap {
        // A visible segment has two edges to it's view arc, and those are the only things that can split one of these squares.
        // Watch out for the wraparound case.
        self.touched_squares_going_outwards_and_ccw()
            .map(|rel_square| {
                (
                    rel_square,
                    SquareVisibility::from_relative_square_and_view_arc(
                        self.visible_arc,
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

impl Debug for AngleBasedVisibleSegment {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "visible angle: {:?}\n\
            start line: {:?}\n\
            end fence: {:?}\n\
            ",
            self.visible_arc, self.start_internal_relative_face, self.end_fence
        )
    }
}
// TODO: Applying a trait by simply calling the functions on every component member of a struct should be automated.
impl RigidlyTransformable for AngleBasedVisibleSegment {
    fn apply_rigid_transform(&self, tf: RigidTransform) -> Self {
        Self::new_with_optional_start_face(
            self.visible_arc.apply_rigid_transform(tf),
            self.end_fence.apply_rigid_transform(tf),
            self.start_internal_relative_face
                .map(|face| face.apply_rigid_transform(tf)),
        )
    }
}
#[cfg(test)]
mod tests {
    use euclid::vec2;
    use ntest::assert_about_eq;

    use crate::{
        fov_stuff::{fence::Fence, square_visibility::ViewRoundable},
        utility::{
            coordinate_frame_conversions::{STEP_DOWN, STEP_LEFT, STEP_RIGHT, STEP_UP},
            coordinates::FVector,
        },
    };

    use super::*;
    #[test]
    fn test_squares_touched_by_angle_based_visible_segment__simple_horizontal() {
        let seg = AngleBasedVisibleSegment::from_relative_face((STEP_RIGHT * 5, STEP_RIGHT));
        assert_eq!(
            seg.get_touching_relative_squares(),
            (0..=5).map(|i| STEP_RIGHT * i).collect()
        )
    }

    #[test]
    fn test_getting_square_visibilities__includes_partial_squares() {
        let segment = AngleBasedVisibleSegment::from_relative_face((STEP_RIGHT * 2, STEP_RIGHT));
        let visibilities = segment.to_square_visibilities();
        assert!(visibilities
            .get(&STEP_ZERO)
            .is_some_and(SquareVisibility::is_fully_visible));
        assert!(visibilities
            .get(&STEP_RIGHT)
            .is_some_and(SquareVisibility::is_only_partially_visible));
        assert!(visibilities
            .get(&(STEP_RIGHT * 2))
            .is_some_and(SquareVisibility::is_only_partially_visible));
        assert!(visibilities.get(&(STEP_RIGHT * 3)).is_none());
    }
    #[test]
    fn test_getting_square_visibilities__from_visible_square() {
        let segment = AngleBasedVisibleSegment::from_relative_square(STEP_RIGHT * 3);
        let visibilities = segment.to_square_visibilities();
        assert!(visibilities.get(&vec2(0, 0)).unwrap().is_fully_visible());
        assert!(visibilities
            .get(&vec2(1, 0))
            .unwrap()
            .is_only_partially_visible());
        assert!(visibilities
            .get(&vec2(2, 0))
            .unwrap()
            .is_only_partially_visible());
        assert!(visibilities.get(&vec2(3, 0)).unwrap().is_fully_visible());
        assert_eq!(visibilities.len(), 4);
    }
    #[test]
    fn test_start_face_spans_angle_interval() {
        let segment = AngleBasedVisibleSegment::from_relative_square(STEP_DOWN * 5);
        assert!(segment.start_face_spans_angle_interval());
        let with_face = segment.with_start_face((STEP_DOWN * 2, STEP_UP));
        assert!(with_face.start_face_spans_angle_interval());
    }
    #[test]
    #[should_panic]
    fn test_create_with_invalid_fence() {
        AngleBasedVisibleSegment::new(
            PartialAngleInterval::from_degrees(10.0, 20.0),
            Fence::from_one_edge(((5, 0), STEP_RIGHT)),
        );
    }
    #[test]
    #[should_panic]
    fn test_create_with_invalid_start_face__offset() {
        AngleBasedVisibleSegment::new_with_start_face(
            PartialAngleInterval::from_degrees(0.0, 1.0),
            Fence::from_one_edge(((5, 0), STEP_RIGHT)),
            ((-1, 0), STEP_LEFT),
        );
    }
    #[test]
    #[should_panic]
    fn test_create_with_invalid_start_face__angle() {
        let end_fence =
            Fence::from_faces_in_ccw_order([((2, -1), STEP_RIGHT), ((2, 0), STEP_RIGHT)]);
        let segment = AngleBasedVisibleSegment::new_with_start_face(
            end_fence.spanned_angle_from_origin().try_into().unwrap(),
            end_fence,
            ((1, 0), STEP_DOWN),
        );
    }
    #[test]
    fn test_face_direction_agnostic() {
        let arc = PartialAngleInterval::from_degrees(0.0, 1.0);

        let a = AngleBasedVisibleSegment::new_with_start_face(
            arc,
            Fence::from_one_edge(((5, 0), STEP_RIGHT)),
            ((1, 0), STEP_RIGHT),
        );

        let b = AngleBasedVisibleSegment::new_with_start_face(
            arc,
            Fence::from_one_edge(((6, 0), STEP_LEFT)),
            ((2, 0), STEP_LEFT),
        );

        assert_eq!(a, b);
    }
    #[test]
    fn test_combine_two__valid() {
        let rel_square = STEP_RIGHT * 5 + STEP_UP * 2;
        let a_face: RelativeFace = (rel_square, STEP_RIGHT).into();
        let b_face = a_face.strafed_left();
        let start_face = (STEP_RIGHT * 2, STEP_LEFT);
        let mut a =
            AngleBasedVisibleSegment::from_relative_face(a_face).with_start_face(start_face);
        let mut b =
            AngleBasedVisibleSegment::from_relative_face(b_face).with_start_face(start_face);

        dbg!(&a, &b);
        let c: AngleBasedVisibleSegment = a.combined_with(&b).unwrap();

        //  🄱
        //  🄰

        assert_eq!(c.visible_arc.cw(), a.visible_arc.cw());
        assert_eq!(c.visible_arc.ccw(), b.visible_arc.ccw());

        assert_eq!(c.end_fence, Fence::new(vec![a_face, b_face,]));
    }
    #[test]
    #[should_panic]
    fn test_combine_two__fail_because_overlap() {
        let a = AngleBasedVisibleSegment::from_relative_square((5, 0));
        let b = AngleBasedVisibleSegment::from_relative_square((10, 1));
        a.combined_with(&b);
    }
    #[test]
    fn test_combine_two__fail_because_not_touching() {
        let a = AngleBasedVisibleSegment::from_relative_square((5, 0));
        let b = AngleBasedVisibleSegment::from_relative_square((5, 2));
        assert!(a.combined_with(&b).is_none());
    }
    #[test]
    fn test_combine_two__fail_because_different_start_lines() {
        let a = AngleBasedVisibleSegment::from_relative_square((5, 0))
            .with_start_face(((1, 0), STEP_RIGHT));
        let b = AngleBasedVisibleSegment::from_relative_square((5, 1))
            .with_start_face(((2, 0), STEP_RIGHT));
        assert!(a.combined_with(&b).is_none());
    }
    #[test]
    fn test_combine_two__fail_because_end_fences_can_not_connect() {
        let a = AngleBasedVisibleSegment::new(
            PartialAngleInterval::from_degrees(0.0, 1.0),
            ((5, 0), STEP_RIGHT),
        );
        let b = AngleBasedVisibleSegment::new(
            PartialAngleInterval::from_degrees(1.0, 2.0),
            ((6, 0), STEP_RIGHT),
        );
        assert!(a.combined_with(&b).is_none());
    }
    #[test]
    fn test_from_relative_square__does_not_crash() {
        AngleBasedVisibleSegment::from_relative_square((5, 2));
    }
    #[test]
    fn test_from_relative_square__is_correct() {
        let segment = AngleBasedVisibleSegment::from_relative_square((5, 2));
        assert_about_eq!(
            segment.visible_arc.cw().radians,
            better_angle_from_x_axis(FVector::new(5.5, 1.5)).radians,
            1e-4
        );
        assert_about_eq!(
            segment.visible_arc.ccw().radians,
            better_angle_from_x_axis(FVector::new(4.5, 2.5)).radians,
            1e-4
        );
        assert_eq!(segment.start_internal_relative_face, None);
        assert_eq!(
            segment.end_fence,
            Fence::from_faces_in_ccw_order([((5, 2), STEP_RIGHT), ((5, 3), STEP_DOWN)])
        );
    }
    #[test]
    #[should_panic]
    fn test_no_excess_fence() {
        AngleBasedVisibleSegment::new(
            AngleInterval::from_degrees(0.0, 1.0),
            Fence::from_faces_in_ccw_order([(3, 0, STEP_RIGHT), (3, 1, STEP_RIGHT)]),
        );
    }
}
