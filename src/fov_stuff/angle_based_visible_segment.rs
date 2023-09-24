use crate::fov_stuff::{LocalVisibilityMap, SquareVisibility};
use crate::utility::angle_interval::{AngleInterval, PartialAngleInterval};
use crate::utility::circular_interval::circular_merging;
use crate::utility::coordinate_frame_conversions::{StepSet, WorldStep};
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
    visible_angle_interval: PartialAngleInterval,
    start_internal_relative_face: Option<RelativeSquareWithOrthogonalDir>,
    end_fence: RelativeFenceFullyVisibleFromOriginGoingCcw,
}
impl AngleBasedVisibleSegment {
    pub fn new(arc: PartialAngleInterval, end_fence: impl Into<Fence>) -> Self {
        let x = Self {
            visible_angle_interval: arc,
            start_internal_relative_face: None,
            end_fence: end_fence.into(),
        };
        x.validate();
        x
    }

    pub fn new_with_start_face(
        arc: PartialAngleInterval,
        end_fence: Fence,
        start_face: impl Into<RelativeFace>,
    ) -> Self {
        let x = Self {
            visible_angle_interval: arc,
            start_internal_relative_face: Some(start_face.into()),
            end_fence,
        };
        x.validate();
        x
    }
    pub fn new_with_optional_start_face(
        arc: PartialAngleInterval,
        end_fence: Fence,
        optional_start_face: Option<impl Into<RelativeFace>>,
    ) -> Self {
        if let Some(start_face) = optional_start_face {
            Self::new_with_start_face(arc, end_fence, start_face)
        } else {
            Self::new(arc, end_fence)
        }
    }
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
        if self.start_internal_relative_face.is_none() {
            return true;
        }

        let start_face = self.start_internal_relative_face.unwrap();

        let interval_includes_any_line_end = start_face
            .face_line_segment()
            .parallel_directions()
            .iter()
            .any(|&line_angle| {
                let interval_includes_line_end = self
                    .visible_angle_interval
                    .contains_or_touches_angle(line_angle);
                interval_includes_line_end
            });
        !interval_includes_any_line_end
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
        relative_face: impl Into<RelativeSquareWithOrthogonalDir>,
    ) -> Self {
        if self.start_internal_relative_face.is_some() {
            return self.clone();
        }

        self.with_start_face(relative_face)
    }
    pub fn with_start_face(&self, relative_face: impl Into<RelativeFace>) -> Self {
        Self::new_with_start_face(
            self.visible_angle_interval,
            self.end_fence.clone(),
            relative_face,
        )
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
    pub fn combine_multiple(unsorted_segments: impl IntoIterator<Item = Self>) -> Vec<Self> {
        let sorted_ccw = unsorted_segments.into_iter().sorted_by_key(|segment| {
            OrderedFloat(segment.visible_angle_interval.center_angle().radians)
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

        let maybe_combined_arc: Option<PartialAngleInterval> = self
            .visible_angle_interval
            .combine_touching_panic_overlapping(other.visible_angle_interval, Angle::degrees(0.1));
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
        self.visible_angle_interval
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

impl Debug for AngleBasedVisibleSegment {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "visible angle: {:?}\n\
            start line: {:?}\n\
            end fence: {:?}\n\
            ",
            self.visible_angle_interval, self.start_internal_relative_face, self.end_fence
        )
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
    use euclid::vec2;

    use crate::{
        fov_stuff::{fence::Fence, square_visibility::ViewRoundable},
        utility::coordinate_frame_conversions::{STEP_DOWN, STEP_LEFT, STEP_RIGHT, STEP_UP},
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
    fn test_create_with_invalid_start_face() {
        AngleBasedVisibleSegment::new_with_start_face(
            PartialAngleInterval::from_degrees(0.0, 1.0),
            Fence::from_one_edge(((5, 0), STEP_RIGHT)),
            ((-1, 0), STEP_LEFT),
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
        let a_square = rel_square;
        let b_square = a_square + STEP_UP;
        let start_face = (STEP_RIGHT * 2, STEP_LEFT);
        let mut a =
            AngleBasedVisibleSegment::from_relative_square(a_square).with_start_face(start_face);
        let mut b =
            AngleBasedVisibleSegment::from_relative_square(b_square).with_start_face(start_face);

        let c: AngleBasedVisibleSegment = a.combined_with(&b).unwrap();

        //  🄱
        //  🄰

        assert_eq!(c.visible_angle_interval.cw(), a.visible_angle_interval.cw());
        assert_eq!(
            c.visible_angle_interval.ccw(),
            b.visible_angle_interval.ccw()
        );

        assert_eq!(
            c.end_fence,
            Fence::new(vec![
                (a_square, STEP_DOWN),
                (a_square, STEP_RIGHT),
                (b_square, STEP_RIGHT),
                (b_square, STEP_UP)
            ])
        );
    }
    #[test]
    fn test_combine_two__fail_because_overlap() {
        let a = AngleBasedVisibleSegment::from_relative_square((5, 0));
        let b = AngleBasedVisibleSegment::from_relative_square((10, 1));
        assert!(a.combined_with(&b).is_none());
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
            PartialAngleInterval::from_degrees(0.0, 45.0),
            ((5, 0), STEP_RIGHT),
        );
        let b = AngleBasedVisibleSegment::new(
            PartialAngleInterval::from_degrees(-45.0, 0.0),
            ((6, 0), STEP_RIGHT),
        );
        todo!()
    }
}
