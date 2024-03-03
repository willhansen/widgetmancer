use crate::fov_stuff::{LocalSquareVisibilityMap, SquareVisibility};
use crate::utility::angle_interval::AngleInterval;
use crate::utility::circular_interval::circular_merging;
use crate::utility::coordinate_frame_conversions::{StepSet, WorldStep, STEP_LEFT};
use crate::utility::coordinates::*;
use crate::utility::line::UndirectedFloatLineTrait;
use crate::utility::poses::RelativeFace;
use crate::utility::{
    better_angle_from_x_axis, faces_away_from_center_at_rel_square, CoordToString,
    RelativeSquareWithOrthogonalDir, RigidTransform, RigidlyTransformable, STEP_ZERO,
};
use euclid::{point2, Angle};
use itertools::{all, Itertools};
use ordered_float::OrderedFloat;
use std::collections::HashSet;
use std::fmt::{Debug, Formatter};

use super::fence::{Fence, RelativeFenceFullyVisibleFromOriginGoingCcw};
use super::square_visibility::RelativeSquareVisibilityFunctions;
use super::NARROWEST_VIEW_CONE_ALLOWED_IN_DEGREES;

// #[portrait::derive(QuarterTurnRotatable with portrait::derive_delegate)]
#[derive(Clone, PartialEq)]
pub struct AngleBasedVisibleSegment {
    arc: AngleInterval,
    end_fence: RelativeFenceFullyVisibleFromOriginGoingCcw,
    start_internal_relative_face: Option<RelativeSquareWithOrthogonalDir>,
}
impl QuarterTurnRotatable for AngleBasedVisibleSegment {
    fn quarter_rotated_ccw(&self, quarter_turns_ccw: impl Into<QuarterTurnsCcw> + Copy) -> Self {
        Self::new_with_optional_start_face(
            self.arc.quarter_rotated_ccw(quarter_turns_ccw),
            self.end_fence.quarter_rotated_ccw(quarter_turns_ccw),
            self.start_internal_relative_face
                .map(|x| x.quarter_revolved_ccw_around_origin(quarter_turns_ccw)),
        )
    }
}
impl AngleBasedVisibleSegment {
    pub fn new(arc: impl Into<AngleInterval>, end_fence: impl Into<Fence>) -> Self {
        Self::new_with_optional_start_face(arc, end_fence, Option::<RelativeFace>::None)
    }
    fn default_angle_tolerance() -> FAngle {
        Angle::radians(1e-6)
    }

    pub fn arc(&self) -> AngleInterval {
        self.arc
    }

    pub fn contains_angle_degrees(&self, degrees: f32) -> bool {
        self.arc
            .contains_angle(FAngle::degrees(degrees), FAngle::degrees(0.0))
            .is_true()
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
            arc: arc.into(),
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
        if self.end_fence_has_extra_segments() {
            panic!("END FENCE HAS EXTRA SEGMENTS:\n{:?}", self);
        }
    }
    pub fn end_fence(&self) -> &RelativeFenceFullyVisibleFromOriginGoingCcw {
        &self.end_fence
    }

    fn end_fence_has_extra_segments(&self) -> bool {
        if self.end_fence.num_edges() < 2 {
            return false;
        }

        let points_that_should_be_in_arc = [1, -2].map(|i| self.end_fence.point_by_index(i));
        all(points_that_should_be_in_arc, |point| {
            self.arc
                .contains_angle(
                    better_angle_from_x_axis(point),
                    Self::default_angle_tolerance(),
                )
                .is_false()
        })
    }
    fn end_fence_fully_covers_angle_interval(&self) -> bool {
        self.end_fence
            .spanned_angle_from_origin()
            .contains_arc(self.arc, Self::default_angle_tolerance())
            .is_at_least_partial()
    }
    fn start_face_spans_angle_interval(&self) -> bool {
        if self.start_internal_relative_face.is_none() {
            return true; // This behavior may not be consistent with the function's name
        }

        let start_face = self.start_internal_relative_face.unwrap();
        let start_line = start_face.face_line_segment();
        let vector_to_line_from_origin = start_line.normal_vector_from_origin();
        if vector_to_line_from_origin.square_length() == 0.0 {
            return false;
        }

        let angle_span_of_extended_line_as_seen_from_origin = AngleInterval::from_center_and_width(
            better_angle_from_x_axis(vector_to_line_from_origin),
            FAngle::degrees(180.0),
        );

        angle_span_of_extended_line_as_seen_from_origin
            .contains_arc(self.arc, Self::default_angle_tolerance())
            .is_at_least_partial()
    }
    pub fn from_relative_face(relative_face: impl Into<RelativeSquareWithOrthogonalDir>) -> Self {
        let actual_face = relative_face.into();
        Self::new(
            AngleInterval::from_relative_square_face(actual_face),
            RelativeFenceFullyVisibleFromOriginGoingCcw::from_faces_in_ccw_order(vec![actual_face]),
        )
    }
    pub fn from_relative_square(step: impl Into<WorldStep>) -> Self {
        let step = step.into();
        let arc = AngleInterval::from_relative_square(step);

        let faces = faces_away_from_center_at_rel_square(step);
        let end_fence =
            RelativeFenceFullyVisibleFromOriginGoingCcw::from_unordered_relative_edges(faces);

        Self::new(arc, end_fence)
    }
    pub fn from_arc_and_fence_radius(arc: AngleInterval, fence_radius: u32) -> Self {
        Self::new(arc, Fence::from_radius_of_square_and_arc(fence_radius, arc))
    }
    pub fn new_full_circle(fence_radius: u32) -> Self {
        let arc = AngleInterval::FullCircle;
        Self::new(arc, Fence::from_radius_of_square_and_arc(fence_radius, arc))
    }
    pub fn narrow_segment_to_right(first_square: u32, last_square: u32) -> Self {
        let segment = Self::from_arc_and_fence_radius(
            AngleInterval::from_degrees(
                -NARROWEST_VIEW_CONE_ALLOWED_IN_DEGREES,
                NARROWEST_VIEW_CONE_ALLOWED_IN_DEGREES,
            ),
            last_square,
        );
        if first_square == 0 {
            segment
        } else {
            segment.with_start_face((first_square as i32, 0, STEP_LEFT))
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
        Self::new_with_start_face(self.arc, self.end_fence.clone(), relative_face)
    }
    pub fn with_arc(&self, arc: AngleInterval) -> Self {
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
        // dbg!("================================================================================================");
        let sorted_ccw = unsorted_segments
            .into_iter()
            .sorted_by_key(|segment| {
                OrderedFloat(match segment.arc {
                    AngleInterval::PartialArc(partial_arc) => partial_arc.center_angle().radians,
                    _ => 0.0,
                })
            })
            .inspect(|x| {
                // dbg!(x.arc); // asdfasdf
            })
            .collect_vec();

        let reduction_function = |a: &Self, b: &Self| -> Option<Self> { a.combined_with(b) };
        // dbg!(reduction_function(&sorted_ccw[0], &sorted_ccw[1])); // asdfasdf

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

        let maybe_combined_arc: Option<AngleInterval> = self
            .arc
            .combine_if_touching_panic_if_overlapping(other.arc, Self::default_angle_tolerance());
        if maybe_combined_arc.is_none() {
            return None;
        }
        let combined_arc = maybe_combined_arc.unwrap();

        let maybe_combined_fence: Option<Fence> = self.end_fence.try_union(&other.end_fence).ok();

        // dbg!(&self.end_fence, &other.end_fence, &maybe_combined_fence); // asdfasdf

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
    fn rel_square_is_after_start_line(&self, rel_square: impl Into<WorldStep>) -> bool {
        let rel_square = rel_square.into();
        if let Some(line) = self
            .start_internal_relative_face
            .map(|face| face.face_line_segment())
        {
            // TODO: generalize to allow passing in the relative squares, not needing absolute points
            !line.same_side_of_line(rel_square.to_point().to_f32(), point2(0.0, 0.0))
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
        self.arc
            .touched_squares_going_outwards_and_ccw()
            .take_while(move |&rel_square| rel_square.square_length() <= max_square_length)
            .filter(|&rel_square| self.rel_square_is_after_start_line(rel_square))
            .filter(|&rel_square| self.rel_square_is_before_end_fence(rel_square))
    }
    pub fn to_local_square_visibility_map(&self) -> LocalSquareVisibilityMap {
        // A visible segment has two edges to its view arc, and those are the only things that can split one of these squares.
        // Watch out for the wraparound case.
        self.touched_squares_going_outwards_and_ccw()
            .filter(|rel_square| rel_square.square_length() > 0) // skip the center square
            .map(|rel_square| {
                (
                    rel_square,
                    SquareVisibility::from_relative_square_and_view_arc(self.arc, rel_square),
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
            self.arc, self.start_internal_relative_face, self.end_fence
        )
    }
}

#[cfg(test)]
mod tests {
    use euclid::vec2;
    use ntest::{assert_about_eq, assert_false, assert_true};

    use crate::{
        fov_stuff::{fence::Fence, square_visibility::ViewRoundable},
        utility::{
            coordinate_frame_conversions::{STEP_DOWN, STEP_LEFT, STEP_RIGHT, STEP_UP},
            coordinates::FVector,
            general_utility::{as_set, set_of_keys},
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
        let visibilities = segment.to_local_square_visibility_map();
        assert!(visibilities.get(&STEP_ZERO).is_none());
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
        let visibilities = segment.to_local_square_visibility_map();
        assert!(visibilities.get(&vec2(0, 0)).is_none());
        assert!(visibilities
            .get(&vec2(1, 0))
            .unwrap()
            .is_only_partially_visible());
        assert!(visibilities
            .get(&vec2(2, 0))
            .unwrap()
            .is_only_partially_visible());
        assert!(visibilities.get(&vec2(3, 0)).unwrap().is_fully_visible());
        assert_eq!(visibilities.len(), 3);
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
            AngleInterval::from_degrees(10.0, 20.0),
            Fence::from_one_edge(((5, 0), STEP_RIGHT)),
        );
    }
    #[test]
    #[should_panic]
    fn test_create_with_invalid_start_face__offset() {
        AngleBasedVisibleSegment::new_with_start_face(
            AngleInterval::from_degrees(0.0, 1.0),
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
            end_fence.spanned_angle_from_origin(),
            end_fence,
            ((1, 0), STEP_DOWN),
        );
    }
    #[test]
    fn test_face_direction_agnostic() {
        let arc = AngleInterval::from_degrees(0.0, 1.0);

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

        let c: AngleBasedVisibleSegment = a.combined_with(&b).unwrap();

        //  🄱
        //  🄰

        assert_eq!(c.arc.cw(), a.arc.cw());
        assert_eq!(c.arc.ccw(), b.arc.ccw());

        assert_eq!(
            c.end_fence,
            Fence::from_faces_in_ccw_order(vec![a_face, b_face,])
        );
    }
    #[test]
    fn test_combine_two__full_circle() {
        let segments = [(45.0, -45.0), (-45.0, 45.0)].map(|deg| {
            AngleBasedVisibleSegment::from_arc_and_fence_radius(
                AngleInterval::from_degrees(deg.0, deg.1),
                5,
            )
        });
        let result = segments[0].combined_with(&segments[1]).unwrap();
        assert!(result.arc == AngleInterval::FullCircle);
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
            AngleInterval::from_degrees(0.0, 1.0),
            ((5, 0), STEP_RIGHT),
        );
        let b = AngleBasedVisibleSegment::new(
            AngleInterval::from_degrees(1.0, 2.0),
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
            segment.arc.cw().radians,
            better_angle_from_x_axis(FVector::new(5.5, 1.5)).radians,
            1e-4
        );
        assert_about_eq!(
            segment.arc.ccw().radians,
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
    #[test]
    fn test_local_square_visibility_map_does_not_include_start_square() {
        let segment = AngleBasedVisibleSegment::from_relative_square((5, 4));
        let map = segment.to_local_square_visibility_map();

        assert_false!(map.contains_key(&(0, 0).into()));
    }
    #[test]
    fn test_local_square_visibility_map_respects_start_line() {
        let view_segment: AngleBasedVisibleSegment =
            AngleBasedVisibleSegment::narrow_segment_to_right(4, 5);
        let map = view_segment.to_local_square_visibility_map();
        assert_eq!(set_of_keys(&map), as_set([(4, 0), (5, 0)]));
    }
    #[test]
    fn test_outward_going_square_iteration_respects_start_line() {
        let view_segment: AngleBasedVisibleSegment =
            AngleBasedVisibleSegment::narrow_segment_to_right(4, 5);
        let mut iter = view_segment.touched_squares_going_outwards_and_ccw();
        assert_eq!(iter.next(), Some((4, 0).into()));
        assert_eq!(iter.next(), Some((5, 0).into()));
        assert_eq!(iter.next(), None);
    }
    #[test]
    fn test_check_if_rel_square_is_after_start_line() {
        let view_segment: AngleBasedVisibleSegment =
            AngleBasedVisibleSegment::narrow_segment_to_right(4, 5);
        assert_true!(view_segment.rel_square_is_after_start_line((4, 0)));
        assert_true!(view_segment.rel_square_is_after_start_line((5, 0)));
        assert_false!(view_segment.rel_square_is_after_start_line((0, 0)));
        assert_false!(view_segment.rel_square_is_after_start_line((1, 0)));
        assert_false!(view_segment.rel_square_is_after_start_line((2, 0)));
        assert_false!(view_segment.rel_square_is_after_start_line((3, 0)));
    }
    #[test]
    fn test_start_line_orientation_does_not_matter() {
        let seg = AngleBasedVisibleSegment::narrow_segment_to_right(0, 5);
        let seg1 = seg.with_start_face((3, 0, STEP_RIGHT));
        let seg2 = seg.with_start_face((4, 0, STEP_LEFT));
        assert_eq!(
            seg1.to_local_square_visibility_map(),
            seg2.to_local_square_visibility_map()
        );
    }
    #[test]
    fn test_combine_multiple__full_circle() {
        let radius = 3;
        vec![
            vec![(45.0, -45.0), (-45.0, 45.0)],
            vec![(45.0, -45.0), (0.0, 45.0), (-45.0, 0.0)],
        ]
        .into_iter()
        .for_each(|ranges| {
            let segments = ranges.into_iter().map(|deg| {
                AngleBasedVisibleSegment::from_arc_and_fence_radius(
                    AngleInterval::from_degrees(deg.0, deg.1),
                    radius,
                )
            });
            let result = AngleBasedVisibleSegment::combine_multiple(segments);
            assert_eq!(result.len(), 1);
            assert_eq!(result[0], AngleBasedVisibleSegment::new_full_circle(radius));
        });
    }
}
