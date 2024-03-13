use std::borrow::Borrow;
use std::collections::{HashMap, HashSet};
use std::f32::consts::{PI, TAU};
use std::fmt::{Debug, Display, Formatter};
use std::ops::{Add, Neg, Sub};

use euclid::Angle;
use getset::CopyGetters;
use itertools::Itertools;
use ntest::assert_false;
use num::traits::FloatConst;
use ordered_float::OrderedFloat;
use termion::cursor::Left;

use crate::fov_stuff::OctantFOVSquareSequenceIter;
use crate::utility::coordinate_frame_conversions::{WorldMove, WorldStep};
use crate::utility::round_robin_iterator::round_robin;
use crate::utility::{
    abs_angle_distance, QuarterTurnsCcw, standardize_angle, Octant, OrthogonalWorldStep,
    RelativeSquareWithOrthogonalDir, SquareWithOrthogonalDir, ORTHOGONAL_STEPS, STEP_DOWN_LEFT,
    STEP_DOWN_RIGHT, STEP_UP_LEFT, STEP_UP_RIGHT, STEP_ZERO,
};
use crate::{vec2, FloatCoordinate};

use super::bool_with_partial::BoolWithPartial;
use super::coordinates::QuarterTurnRotatable;
use super::poses::RelativeFace;
use super::{FAngle, RigidTransform, RigidlyTransformable};

#[derive(Default, Debug, Clone, PartialEq, CopyGetters)]
#[get_copy = "pub"]
pub struct DirectionalAngularEdge {
    angle: Angle<f32>,
    is_clockwise_edge: bool,
}

impl DirectionalAngularEdge {
    pub fn new(angle: Angle<f32>, is_clockwise_edge: bool) -> Self {
        DirectionalAngularEdge {
            angle: standardize_angle(angle),
            is_clockwise_edge,
        }
    }
    pub fn flipped(&self) -> Self {
        DirectionalAngularEdge {
            angle: self.angle,
            is_clockwise_edge: !self.is_clockwise_edge,
        }
    }
    pub fn direction_to_inside(&self) -> Angle<f32> {
        let rotation_sign = if self.is_clockwise_edge { 1.0 } else { -1.0 };
        standardize_angle(self.angle + Angle::degrees(rotation_sign * 90.0))
    }
}

// #[portrait::derive(QuarterTurnRotatable with portrait::derive_delegate)]
#[derive(Copy, Clone, PartialEq, CopyGetters)]
#[get_copy = "pub"]
pub struct PartialAngleInterval {
    // TODO: make not public
    pub clockwise_end: Angle<f32>,
    pub anticlockwise_end: Angle<f32>,
}
impl Display for PartialAngleInterval {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "({}° to {}°)",
            self.clockwise_end.to_degrees(),
            self.anticlockwise_end.to_degrees()
        )
    }
}
// TODO: have a macro make this code
impl QuarterTurnRotatable for PartialAngleInterval {
    fn quarter_rotated_ccw(
        &self,
        quarter_turns_ccw: impl Into<QuarterTurnsCcw> + Copy,
    ) -> Self {
        Self::from_angles(
            self.clockwise_end.quarter_rotated_ccw(quarter_turns_ccw),
            self.anticlockwise_end
                .quarter_rotated_ccw(quarter_turns_ccw),
        )
    }
}
// TODO: have a macro make this code
impl RigidlyTransformable for PartialAngleInterval {
    fn apply_rigid_transform(&self, tf: RigidTransform) -> Self {
        self.rotated_quarter_turns(tf.rotation())
    }
}

// TODO: remove this if portrait works
// impl QuarterTurnRotatable for PartialAngleInterval {
//     fn quarter_rotated_ccw(&self, quarter_turns_ccw: impl Into<QuarterTurnsCcw>) -> Self {
//         let quarter_turns = quarter_turns.into();
//         PartialAngleInterval {
//             clockwise_end: quarter_turns.rotate_angle(self.clockwise_end),
//             anticlockwise_end: quarter_turns.rotate_angle(self.anticlockwise_end),
//         }
//     }
// }

impl PartialAngleInterval {
    #[deprecated(note = "don't use defaults in utility module")]
    const DEFAULT_TOLERANCE_RADIANS: f32 = 1e-6;

    #[deprecated(note = "don't use defaults in utility module")]
    pub fn default_tolerance() -> FAngle {
        Angle::radians(Self::DEFAULT_TOLERANCE_RADIANS)
    }

    pub fn from_angles(cw: Angle<f32>, ccw: Angle<f32>) -> Self {
        let (cw, ccw) = (standardize_angle(cw), standardize_angle(ccw));
        assert_ne!(cw, ccw);
        Self {
            clockwise_end: cw,
            anticlockwise_end: ccw,
        }
    }

    pub fn cw(&self) -> FAngle {
        self.clockwise_end
    }
    pub fn ccw(&self) -> FAngle {
        self.anticlockwise_end
    }
    pub fn from_degrees(cw: f32, ccw: f32) -> Self {
        Self::from_angles(Angle::degrees(cw), Angle::degrees(ccw))
    }
    pub fn from_radians(clockwise_end_in_radians: f32, anticlockwise_end_in_radians: f32) -> Self {
        Self::from_angles(
            Angle::radians(clockwise_end_in_radians),
            Angle::radians(anticlockwise_end_in_radians),
        )
    }
    pub fn from_center_and_width(center_angle: FAngle, width: FAngle) -> Self {
        Self::from_angles(center_angle - width / 2.0, center_angle + width / 2.0)
    }
    pub fn to_degrees(&self) -> (f32, f32) {
        (
            self.clockwise_end.to_degrees(),
            self.anticlockwise_end.to_degrees(),
        )
    }
    pub fn to_radians(&self) -> (f32, f32) {
        (self.clockwise_end.radians, self.anticlockwise_end.radians)
    }
    pub fn from_octant(octant: Octant) -> Self {
        let n = octant.number();
        let positive_y = n < 4;

        let step_length = PI / 4.0;
        Self::from_radians(n as f32 * step_length, (n + 1) as f32 * step_length)
    }

    pub fn from_relative_square(relative_square: impl Into<WorldStep>) -> Self {
        let relative_square = relative_square.into();
        assert_ne!(relative_square, vec2(0, 0));
        let rel_square_center = relative_square.to_f32();
        let rel_square_corners: Vec<WorldMove> = vec![
            rel_square_center + STEP_UP_RIGHT.to_f32() * 0.5,
            rel_square_center + STEP_UP_LEFT.to_f32() * 0.5,
            rel_square_center + STEP_DOWN_LEFT.to_f32() * 0.5,
            rel_square_center + STEP_DOWN_RIGHT.to_f32() * 0.5,
        ];

        let center_angle = rel_square_center.better_angle_from_x_axis();
        let corner_angles: Vec<Angle<f32>> = rel_square_corners
            .iter()
            .map(|rel_corner_point| rel_corner_point.better_angle_from_x_axis())
            .collect();

        let most_clockwise = corner_angles
            .iter()
            .min_by_key(|&&c| OrderedFloat(center_angle.angle_to(c).radians))
            .unwrap();
        let least_clockwise = corner_angles
            .iter()
            .max_by_key(|&&c| OrderedFloat(center_angle.angle_to(c).radians))
            .unwrap();

        PartialAngleInterval {
            anticlockwise_end: *least_clockwise,
            clockwise_end: *most_clockwise,
        }
    }
    pub fn from_relative_square_face(rel_face: impl Into<RelativeFace>) -> Self {
        let (relative_square, face_direction): (WorldStep, OrthogonalWorldStep) =
            rel_face.into().into();
        let square_center = relative_square.to_f32();
        let face_center = square_center + face_direction.step().to_f32() / 2.0;
        let face_corners = [1, -1].map(|sign| {
            face_center + (face_direction.step().to_f32() / 2.0).quarter_rotated_ccw(sign)
        });

        let center_angle = face_center.better_angle_from_x_axis();
        let face_corner_angles = face_corners.map(|x| x.better_angle_from_x_axis());

        let first_corner_angle_is_more_clockwise =
            center_angle.angle_to(face_corner_angles[0]).radians < 0.0;

        if first_corner_angle_is_more_clockwise {
            PartialAngleInterval::from_angles(face_corner_angles[0], face_corner_angles[1])
        } else {
            PartialAngleInterval::from_angles(face_corner_angles[1], face_corner_angles[0])
        }
    }

    pub fn would_combine_with_other_partial_arc_to_full_circle(
        &self,
        other: Self,
        tolerance: FAngle,
    ) -> BoolWithPartial {
        self.contains_partial_arc(other.complement(), tolerance)
    }

    pub fn complement(&self) -> Self {
        PartialAngleInterval::from_angles(self.anticlockwise_end, self.clockwise_end)
    }

    #[deprecated(note = "use version with tolerance instead")]
    pub fn at_least_fully_overlaps(&self, other: PartialAngleInterval) -> bool {
        self.num_contained_or_touching_edges(other) == 2 && self.width() >= other.width()
    }
    pub fn overlaps_partial_arc(&self, other: Self, tolerance: Angle<f32>) -> BoolWithPartial {
        self.contains_angle(other.cw(), tolerance)
            .or(self.contains_angle(other.center_angle(), tolerance))
            .or(self.contains_angle(other.ccw(), tolerance))
            .or(other.contains_angle(self.cw(), tolerance))
            .or(other.contains_angle(self.center_angle(), tolerance))
            .or(other.contains_angle(self.ccw(), tolerance))
    }

    #[deprecated(note = "use version with tolerance instead")]
    pub fn overlapping_but_not_exactly_touching(&self, other: PartialAngleInterval) -> bool {
        // self overlap
        if other == *self {
            return true;
        }

        self.contains_angle_not_including_edges(other.anticlockwise_end)
            || (self.contains_angle_not_including_edges(other.clockwise_end)
                && self.anticlockwise_end != other.clockwise_end)
            || other.contains_angle_not_including_edges(self.anticlockwise_end)
            || (other.contains_angle_not_including_edges(self.clockwise_end)
                && other.anticlockwise_end != self.clockwise_end)
    }

    #[deprecated(note = "use version with tolerance instead")]
    pub fn touches_or_overlaps(&self, other: PartialAngleInterval) -> bool {
        self.num_contained_or_touching_edges(other) > 0
            || other.num_contained_or_touching_edges(*self) > 0
    }

    // TODO: Add tolerance
    fn num_contained_or_touching_edges(&self, other: PartialAngleInterval) -> u32 {
        let mut sum = 0;
        if self.contains_or_touches_angle(other.anticlockwise_end) {
            sum += 1;
        }
        if self.contains_or_touches_angle(other.clockwise_end) {
            sum += 1;
        }
        sum
    }
    #[deprecated(note = "use version with tolerance instead")]
    fn num_contained_not_touching_edges(&self, other: PartialAngleInterval) -> u32 {
        let mut sum = 0;
        if self.contains_angle_not_including_edges(other.anticlockwise_end) {
            sum += 1;
        }
        if self.contains_angle_not_including_edges(other.clockwise_end) {
            sum += 1;
        }
        sum
    }
    pub fn has_wraparound_double_overlap(
        &self,
        other: PartialAngleInterval,
        tolerance: FAngle,
    ) -> BoolWithPartial {
        self.contains_angle(other.cw(), tolerance)
            .and(self.contains_angle(other.ccw(), tolerance))
            .and(other.contains_angle(self.cw(), tolerance))
            .and(other.contains_angle(self.ccw(), tolerance))
    }
    #[deprecated(note = "use version with tolerance instead")]
    pub fn partially_overlaps_other_while_including_edges(
        &self,
        other: PartialAngleInterval,
    ) -> bool {
        let contained_in_self = self.num_contained_or_touching_edges(other);
        let contained_in_other = other.num_contained_or_touching_edges(*self);
        contained_in_self >= 1 && contained_in_other >= 1
    }
    pub fn edge_of_this_overlapped_by(
        &self,
        other: PartialAngleInterval,
    ) -> DirectionalAngularEdge {
        if !self.partially_overlaps_other_while_including_edges(other) {
            panic!("no overlap between {} and {}", self, other);
        }
        let is_clockwise_end = other.contains_or_touches_angle(self.clockwise_end);
        DirectionalAngularEdge {
            angle: if is_clockwise_end {
                self.clockwise_end
            } else {
                self.anticlockwise_end
            },
            is_clockwise_edge: is_clockwise_end,
        }
    }
    pub fn edge_of_this_deeper_in(&self, other: PartialAngleInterval) -> DirectionalAngularEdge {
        assert!(other.fully_contains_interval_excluding_edge_overlaps(*self));
        let clockwise_dist =
            PartialAngleInterval::from_angles(other.clockwise_end, self.clockwise_end).width();
        let anticlockwise_dist =
            PartialAngleInterval::from_angles(self.anticlockwise_end, other.anticlockwise_end)
                .width();
        let clockwise_edge_is_deeper = clockwise_dist.radians > anticlockwise_dist.radians;
        DirectionalAngularEdge {
            angle: if clockwise_edge_is_deeper {
                self.clockwise_end
            } else {
                self.anticlockwise_end
            },
            is_clockwise_edge: clockwise_edge_is_deeper,
        }
    }
    #[deprecated(note = "use version with tolerance instead")]
    fn exactly_touches_arc(&self, other: PartialAngleInterval) -> bool {
        let edges_touch = self.clockwise_end == other.anticlockwise_end
            || other.clockwise_end == self.anticlockwise_end;

        let contains_other_edge = self.contains_angle_not_including_edges(other.clockwise_end)
            || self.contains_angle_not_including_edges(other.anticlockwise_end);

        edges_touch && !contains_other_edge
    }
    #[deprecated(note = "use version with tolerance instead")]
    pub fn overlaps_or_touches(&self, other: PartialAngleInterval) -> bool {
        self.overlapping_but_not_exactly_touching(other) || self.exactly_touches_arc(other)
    }

    #[deprecated(note = "use version with tolerance instead")]
    fn exactly_touches_angle(&self, angle: Angle<f32>) -> bool {
        self.clockwise_end == angle || angle == self.anticlockwise_end
    }
    #[deprecated(note = "use version with tolerance instead")]
    fn contains_angle_not_including_edges(&self, angle: Angle<f32>) -> bool {
        if self.exactly_touches_angle(angle) {
            return false;
        }

        self.center_angle().angle_to(angle).radians.abs() < self.width().radians / 2.0
    }
    #[deprecated(note = "use version with tolerance instead")]
    pub fn contains_or_touches_angle(&self, angle: Angle<f32>) -> bool {
        // both edges count
        if self.exactly_touches_angle(angle) {
            return true;
        }

        self.center_angle().angle_to(angle).radians.abs() <= self.width().radians / 2.0
    }
    pub fn contains_angle(&self, angle: Angle<f32>, tolerance: Angle<f32>) -> BoolWithPartial {
        let angle = standardize_angle(angle);

        if angle == self.cw() || angle == self.ccw() {
            return BoolWithPartial::Partial;
        }

        BoolWithPartial::from_less_than_with_tolerance(
            self.center_angle().angle_to(angle).radians.abs(),
            self.width().radians / 2.0,
            tolerance.radians.abs(),
        )
    }

    pub fn width(&self) -> Angle<f32> {
        if self.clockwise_end == self.anticlockwise_end {
            return Angle::radians(0.0);
        }

        let short_width = self.anticlockwise_end.angle_to(self.clockwise_end);
        let interval_is_less_than_half_circle = short_width.radians < 0.0;
        let full_width = if interval_is_less_than_half_circle {
            Angle::radians(short_width.radians.abs())
        } else {
            Angle::radians(TAU - short_width.radians.abs())
        };
        assert!(full_width.radians >= 0.0);
        full_width
    }

    pub fn half_width(&self) -> FAngle {
        self.width() / 2.0
    }

    pub fn narrowed(&self, smallerness: FAngle) -> Self {
        Self::from_center_and_width(self.center_angle(), self.width() - smallerness * 2.0)
    }

    pub fn center_angle(&self) -> Angle<f32> {
        let mid_angle = Angle::radians(self.clockwise_end.radians + self.width().radians / 2.0);
        mid_angle
    }

    pub fn fully_contains_interval_excluding_edge_overlaps(
        &self,
        other: PartialAngleInterval,
    ) -> bool {
        let contains_other_edges = self.contains_angle_not_including_edges(other.anticlockwise_end)
            && self.contains_angle_not_including_edges(other.clockwise_end);
        let other_does_not_contain_these_edges = !other
            .contains_angle_not_including_edges(self.clockwise_end)
            && !other.contains_angle_not_including_edges(self.anticlockwise_end);

        contains_other_edges && other_does_not_contain_these_edges
    }

    // TODO: get rid of all the non-toleranced float comparison functions
    #[deprecated(note = "use contains_arc instead")]
    pub fn fully_contains_interval_including_edge_overlaps(&self, other: Self) -> bool {
        self.contains_partial_arc(other, FAngle::degrees(0.0))
            .is_at_least_partial()
    }
    pub fn contains_partial_arc(&self, other: Self, tolerance: FAngle) -> BoolWithPartial {
        // fully contains other if other does not touch self's complement
        self.complement()
            .overlaps_partial_arc(other, tolerance)
            .not()
    }
    pub fn most_overlapped_edge_of_self(
        &self,
        other: PartialAngleInterval,
    ) -> DirectionalAngularEdge {
        assert!(self.touches_or_overlaps(other));

        // Select edge of self closest to the other's center
        let dist_from_clockwise_edge = abs_angle_distance(self.clockwise_end, other.center_angle());
        let dist_from_anticlockwise_edge =
            abs_angle_distance(self.anticlockwise_end, other.center_angle());
        let clockwise_is_closer = dist_from_clockwise_edge < dist_from_anticlockwise_edge;
        DirectionalAngularEdge::new(
            if clockwise_is_closer {
                self.clockwise_end
            } else {
                self.anticlockwise_end
            },
            clockwise_is_closer,
        )
    }
    // TODO: replace with implementation of QuarterTurnRotatable trait
    pub fn rotated_quarter_turns(
        &self,
        quarter_turns: impl Into<QuarterTurnsCcw> + Copy,
    ) -> Self {
        self.quarter_rotated_ccw(quarter_turns)
    }
    pub fn rotated_ccw(&self, d_angle: Angle<f32>) -> Self {
        PartialAngleInterval::from_angles(
            self.clockwise_end + d_angle,
            self.anticlockwise_end + d_angle,
        )
    }

    pub fn split_into_octants_in_ccw_order(&self) -> Vec<Self> {
        if self.is_fully_near_one_octant_boundary() {
            return vec![];
        }
        if self.is_in_one_octant() {
            return vec![*self];
        }

        let start_octant: Octant = Octant::from_angle_with_tie_break_toward_ccw(self.clockwise_end);
        let end_octant: Octant =
            Octant::from_angle_with_tie_break_toward_cw(self.anticlockwise_end);

        let segment_in_first_octant = Self::from_angles(
            self.clockwise_end,
            Self::from_octant(start_octant).anticlockwise_end,
        );

        let segment_in_last_octant = Self::from_angles(
            Self::from_octant(end_octant).clockwise_end,
            self.anticlockwise_end,
        );

        let mut segments_to_return = vec![segment_in_first_octant];

        let mut oct = start_octant.next_ccw();
        while oct != end_octant {
            segments_to_return.push(PartialAngleInterval::from_octant(oct));
            oct = oct.next_ccw();
        }
        segments_to_return.push(segment_in_last_octant);

        segments_to_return
    }
    pub fn touched_rel_squares_going_outwards_in_one_octant(
        &self,
    ) -> impl Iterator<Item = WorldStep> {
        assert!(self.is_in_one_octant());
        todo!();
        ORTHOGONAL_STEPS.into_iter()
    }
    pub fn touched_rel_squares_going_outwards_in_one_octant_with_placeholders(
        &self,
    ) -> impl Iterator<Item = Option<WorldStep>> {
        assert!(self.is_in_one_octant(), "self: {:?}", self);
        let squares_in_octant_iter =
            OctantFOVSquareSequenceIter::new_from_center(self.octant().unwrap());
        let cloned_arc = self.clone();
        let squares_in_arc_iter = squares_in_octant_iter.map(move |step| {
            if step == STEP_ZERO {
                return Some(step);
            }
            if cloned_arc.overlapping_but_not_exactly_touching(Self::from_relative_square(step)) {
                Some(step)
            } else {
                None
            }
        });
        squares_in_arc_iter
    }
    fn is_fully_near_one_octant_boundary(&self) -> bool {
        let is_smaller_than_octant = self.width() < Angle::degrees(10.0);
        let both_ends_near_boundaries = Octant::near_octant_boundary(self.clockwise_end)
            && Octant::near_octant_boundary(self.anticlockwise_end);
        is_smaller_than_octant && both_ends_near_boundaries
    }
    pub fn is_in_one_octant(&self) -> bool {
        self.octant().is_some()
    }
    pub fn octant(&self) -> Option<Octant> {
        Octant::all_octants_going_ccw().find(|octant| {
            Self::from_octant(*octant)
                .contains_partial_arc(*self, Self::default_tolerance())
                .is_at_least_partial()
        })
    }
}

impl Debug for PartialAngleInterval {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "\n\
            \tradians: {:?}\n\
            \tdegrees: {:?}",
            self.to_radians(),
            self.to_degrees(),
        )
    }
}

#[cfg(test)]
mod tests {
    use itertools::iproduct;
    use ntest::{assert_about_eq, assert_false, timeout};
    use num::zero;
    use pretty_assertions::{assert_eq, assert_ne};
    use strum::IntoEnumIterator;

    use crate::{
        fov_stuff::{rasterized_field_of_view::RasterizedFieldOfViewFunctions, FieldOfView},
        utility::{
            coordinates::opposite_angle, relative_interval_location::RelativeIntervalLocation,
            STEP_DOWN, STEP_RIGHT, STEP_UP,
        },
    };

    use super::*;

    fn default_angle_tolerance_for_tests() -> FAngle {
        FAngle::degrees(0.001)
    }

    #[test]
    fn num_contained_edges_is_symmetric__weird_wrapping_case() {
        let arc_a = PartialAngleInterval::from_degrees(45.0, 0.0);
        let arc_b = PartialAngleInterval::from_degrees(-45.0, 45.0);
        assert_eq!(
            arc_a.num_contained_or_touching_edges(arc_b),
            arc_b.num_contained_or_touching_edges(arc_a)
        );
    }
    #[test]
    fn num_contained_edges_is_symmetric__regular_case() {
        let arc_a = PartialAngleInterval::from_degrees(0.0, 20.0);
        let arc_b = PartialAngleInterval::from_degrees(10.0, 30.0);
        assert_eq!(
            arc_a.num_contained_or_touching_edges(arc_b),
            arc_b.num_contained_or_touching_edges(arc_a)
        );
    }
    #[test]
    fn test_most_overlapped_edge_of_arc() {
        assert_eq!(
            PartialAngleInterval::from_degrees(135.0, 90.0)
                .most_overlapped_edge_of_self(PartialAngleInterval::from_degrees(45.0, 135.0)),
            DirectionalAngularEdge::new(Angle::degrees(90.0), false)
        );
    }
    #[test]
    fn test_arc_exact_touch() {
        let cw = Angle::degrees(5.0);
        let ccw = Angle::degrees(25.0);
        let d = Angle::degrees(1.0);

        let arc = PartialAngleInterval::from_angles(cw, ccw);
        let arc_extend_cw = PartialAngleInterval::from_angles(cw - d, ccw);
        let arc_retract_cw = PartialAngleInterval::from_angles(cw + d, ccw);
        let arc_extend_ccw = PartialAngleInterval::from_angles(cw, ccw + d);
        let arc_retract_ccw = PartialAngleInterval::from_angles(cw, ccw - d);
        let arc_extend_both = PartialAngleInterval::from_angles(cw - d, ccw + d);
        let arc_retract_both = PartialAngleInterval::from_angles(cw + d, ccw - d);

        assert!(arc.exactly_touches_arc(arc_extend_ccw.complement()));
        assert_false!(arc.exactly_touches_arc(arc_retract_ccw.complement()));

        assert_false!(arc.exactly_touches_arc(arc));
        assert!(arc.exactly_touches_arc(arc.complement()))
    }
    #[test]
    fn test_is_in_one_octant() {
        // easy case
        assert!(PartialAngleInterval::from_degrees(5.0, 10.0).is_in_one_octant());
        // easy fail case
        assert_false!(PartialAngleInterval::from_degrees(-5.0, 10.0).is_in_one_octant());
        // fill octant
        assert!(PartialAngleInterval::from_degrees(0.0, 45.0).is_in_one_octant());
        assert!(PartialAngleInterval::from_degrees(-45.0, 0.0).is_in_one_octant());
        // fill octant's complement
        assert_false!(PartialAngleInterval::from_degrees(45.0, 0.0).is_in_one_octant());
    }
    #[test]
    fn test_is_in_one_octant__with_tolerance() {
        let err = 1e-6;
        assert!(PartialAngleInterval::from_degrees(0.0 - err, 45.0 + err).is_in_one_octant());
        let big_err = 1e-2;
        assert_false!(
            PartialAngleInterval::from_degrees(0.0 - big_err, 45.0 + big_err).is_in_one_octant()
        );
    }

    #[test]
    fn test_get_containing_octant__exact_octant() {
        assert_eq!(
            PartialAngleInterval::from_degrees(0.0, 45.0)
                .octant()
                .unwrap()
                .number(),
            0
        );
    }
    #[test]
    fn test_get_containing_octant__in_one() {
        assert_eq!(
            PartialAngleInterval::from_degrees(100.0, 120.0)
                .octant()
                .unwrap()
                .number(),
            2
        );
    }
    #[test]
    fn test_get_containing_octant__not_in_an_octant() {
        assert!(PartialAngleInterval::from_degrees(0.0, 120.0)
            .octant()
            .is_none());
    }
    #[test]
    fn test_get_containing_octant__wraparound_case() {
        assert!(PartialAngleInterval::from_degrees(-10.0, -20.0)
            .octant()
            .is_none());
    }
    #[test]
    fn test_sub_interval__fully_within() {
        assert!(PartialAngleInterval::from_degrees(0.0, 30.0)
            .fully_contains_interval_including_edge_overlaps(PartialAngleInterval::from_degrees(
                5.0, 25.0
            )));
    }
    #[test]
    fn test_sub_interval__touch_one_edge() {
        assert!(PartialAngleInterval::from_degrees(0.0, 30.0)
            .fully_contains_interval_including_edge_overlaps(PartialAngleInterval::from_degrees(
                0.0, 25.0
            )));
    }
    #[test]
    fn test_sub_interval__exact_match() {
        assert!(PartialAngleInterval::from_degrees(0.0, 30.0)
            .fully_contains_interval_including_edge_overlaps(PartialAngleInterval::from_degrees(
                0.0, 30.0
            )));
    }
    #[test]
    fn test_sub_interval__tricky_wraparound() {
        let arc = PartialAngleInterval::from_degrees(0.0, 30.0);
        assert_false!(arc.fully_contains_interval_including_edge_overlaps(arc.complement()));
    }
    #[test]
    fn test_outward_and_ccw_squares_in_one_octant_with_placeholders() {
        let arc = PartialAngleInterval::from_degrees(0.0, 0.01);
        let iter = arc.touched_rel_squares_going_outwards_in_one_octant_with_placeholders();
        let expected = vec![
            Some((0, 0)),
            Some((1, 0)),
            None, // 1,1
            Some((2, 0)),
            None, // 2,1
            None, // 2,2
            Some((3, 0)),
            None, // 3,1
            None, // 3,2
        ]
        .into_iter()
        .map(|t| t.map(|x| x.into()))
        .collect_vec();
        assert_eq!(iter.take(expected.len()).collect_vec(), expected);
    }
    #[test]
    fn test_overlapping_arcs_with_tolerance__barely_touching__observed_failure() {
        let a = -6.340192;
        let b = 18.434948;
        let c = 29.054604;
        assert!(PartialAngleInterval::from_degrees(a, b)
            .overlaps_partial_arc(
                PartialAngleInterval::from_degrees(b, c),
                FAngle::degrees(0.0)
            )
            .is_partial());
    }
    #[test]
    fn test_overlapping_arcs_with_tolerance__narrow_full_overlap_case() {
        let a = PartialAngleInterval::from_degrees(0.0, 20.0);
        let b = PartialAngleInterval::from_degrees(6.0, 7.0);
        let tolerance = FAngle::degrees(0.01);
        assert!(a.overlaps_partial_arc(b, tolerance).is_true());
        assert!(b.overlaps_partial_arc(a, tolerance).is_true());
        assert!(a.overlaps_partial_arc(a, tolerance).is_true());
        assert!(b.overlaps_partial_arc(b, tolerance).is_true());
    }
    #[test]
    fn test_overlapping_arcs_with_tolerance__all_cases_at_once() {
        let a = PartialAngleInterval::from_degrees(10.0, 20.0);
        let tolerance = FAngle::degrees(0.01);

        use RelativeIntervalLocation::*;

        let offset_endpoints_cw_ccw = |arc: &PartialAngleInterval,

                                       cw_pos: RelativeIntervalLocation,
                                       ccw_pos: RelativeIntervalLocation|
         -> [FAngle; 2] {
            let pos_to_ccw_angle_offset = |pos| {
                tolerance
                    * 0.5
                    * match pos {
                        After => 3.0,
                        End => 1.0,
                        Inside => 0.0,
                        Start => -1.0,
                        Before => -3.0,
                    }
            };
            [
                arc.center_angle() - arc.half_width() + pos_to_ccw_angle_offset(cw_pos),
                arc.center_angle() + arc.half_width() + pos_to_ccw_angle_offset(ccw_pos),
            ]
        };

        let all_relative_position_pairs: Vec<(RelativeIntervalLocation, RelativeIntervalLocation)> =
            iproduct!(
                RelativeIntervalLocation::iter(),
                RelativeIntervalLocation::iter()
            )
            .collect();

        let opposite_center = opposite_angle(a.center_angle());

        for (pos_relative_to_cw_end_of_self, pos_relative_to_ccw_end_of_self) in
            all_relative_position_pairs
        {
            let [offset_cw_angle_of_self, offset_ccw_angle_of_self] = offset_endpoints_cw_ccw(
                &a,
                pos_relative_to_cw_end_of_self,
                pos_relative_to_ccw_end_of_self,
            );
            let offset_at_cw_end_of_self_should_be_inside =
                pos_relative_to_cw_end_of_self.is_after_grey_zone();
            let offset_at_ccw_end_of_self_should_be_inside =
                pos_relative_to_ccw_end_of_self.is_before_grey_zone();

            struct TestData {
                cw_end_of_other: FAngle,
                ccw_end_of_other: FAngle,
                correct_overlap_result: BoolWithPartial,
                case_name: String,
            }

            let other_cw_ccw_pairs_and_should_be_overlappings: [TestData; 4] = [
                TestData {
                    cw_end_of_other: opposite_center,
                    ccw_end_of_other: offset_cw_angle_of_self,
                    correct_overlap_result: offset_at_cw_end_of_self_should_be_inside,
                    case_name: "opposite to cw end".to_owned(),
                },
                TestData {
                    cw_end_of_other: offset_ccw_angle_of_self,
                    ccw_end_of_other: opposite_center,
                    correct_overlap_result: offset_at_ccw_end_of_self_should_be_inside,
                    case_name: "ccw end to opposite".to_owned(),
                },
                TestData {
                    cw_end_of_other: offset_ccw_angle_of_self,
                    ccw_end_of_other: offset_cw_angle_of_self,
                    correct_overlap_result: offset_at_ccw_end_of_self_should_be_inside
                        .or(offset_at_cw_end_of_self_should_be_inside),
                    case_name: "ccw end to cw end.  almost complement".to_owned(),
                },
                TestData {
                    cw_end_of_other: offset_cw_angle_of_self,
                    ccw_end_of_other: offset_ccw_angle_of_self,
                    correct_overlap_result: BoolWithPartial::True,
                    case_name: "cw end to ccw end.  almost identical".to_owned(),
                },
            ];

            other_cw_ccw_pairs_and_should_be_overlappings
                .into_iter()
                .for_each(|case_data| {
                    let other_arc = PartialAngleInterval::from_angles(case_data.cw_end_of_other, case_data.ccw_end_of_other);
                    let measured_overlap_result =
                        a.overlaps_partial_arc(other_arc, tolerance);
                    assert_eq!(
                        measured_overlap_result,
                        case_data.correct_overlap_result,
                        "\n\ncase: {}\nrel_cw_of_self: {}\nrel_ccw_of_self: {}\na: {}\nb: {}\nMeasured overlap result: {}\nCorrect overlap result: {}",
                        case_data.case_name,
                        pos_relative_to_cw_end_of_self,
                        pos_relative_to_ccw_end_of_self,
                        &a,
                        &other_arc,
                        measured_overlap_result,
                        case_data.correct_overlap_result
                    );
                });
        }
    }
    #[test]
    fn test_partial_overlap() {
        let arc_a = PartialAngleInterval::from_degrees(0.0, 20.0);
        let arc_b = PartialAngleInterval::from_degrees(-25.0, 5.0);

        assert!(arc_a.partially_overlaps_other_while_including_edges(arc_b));
        assert!(arc_b.partially_overlaps_other_while_including_edges(arc_a));
    }

    #[test]
    fn test_interval_overlap_does_not_include_full_overlap() {
        let interval_b = PartialAngleInterval::from_degrees(5.0, 15.0);
        let interval_a = PartialAngleInterval::from_degrees(7.0, 10.0);
        assert!(
            !interval_a.partially_overlaps_other_while_including_edges(interval_b),
            "should not count full overlaps"
        );
        assert!(
            !interval_b.partially_overlaps_other_while_including_edges(interval_a),
            "should not count full overlaps"
        );
    }

    #[test]
    fn test_partial_overlap_on_both_ends() {
        let wrapping_angle = PartialAngleInterval::from_degrees(45.0, 0.0);
        let zero_center_angle = PartialAngleInterval::from_degrees(-45.0, 45.0);
        assert!(wrapping_angle.partially_overlaps_other_while_including_edges(zero_center_angle));
    }

    #[test]
    fn test_interval_overlap_edges() {
        let interval_a = PartialAngleInterval::from_degrees(0.0, 10.0);
        let interval_c = PartialAngleInterval::from_degrees(10.0, 50.0);
        assert!(
            !interval_a.overlapping_but_not_exactly_touching(interval_c),
            "touching edges should not count as overlap"
        );
    }

    #[test]
    fn test_interval_contains_angle() {
        assert!(
            PartialAngleInterval::from_degrees(0.0, 10.0)
                .contains_or_touches_angle(Angle::degrees(5.0)),
            "simple case"
        );
        assert!(
            !PartialAngleInterval::from_degrees(0.0, 10.0)
                .contains_or_touches_angle(Angle::degrees(15.0)),
            "simple outside bounds case"
        );
        assert!(
            PartialAngleInterval::from_degrees(0.0, 10.0)
                .contains_or_touches_angle(Angle::degrees(10.0)),
            "On left bound should be inside"
        );
        assert!(
            PartialAngleInterval::from_degrees(0.0, 10.0)
                .contains_or_touches_angle(Angle::degrees(0.0)),
            "On right bound should ALSO be inside"
        );
        assert!(
            !PartialAngleInterval::from_degrees(10.0, 0.0)
                .contains_or_touches_angle(Angle::degrees(5.0)),
            "outside a large arc"
        );
        assert!(
            PartialAngleInterval::from_degrees(10.0, 0.0)
                .contains_or_touches_angle(Angle::degrees(180.0)),
            "inside a large arc"
        );
    }
    #[test]
    fn test_interval_contains_angle_with_tolerance() {
        use BoolWithPartial::*;

        let t = |low, high, x, tol, result| {
            assert_eq!(
                PartialAngleInterval::from_degrees(low, high)
                    .contains_angle(Angle::degrees(x), Angle::degrees(tol)),
                result,
                "\n\nlow: {}\nhigh: {}\nx: {}\ntolerance: {}",
                low,
                high,
                x,
                tol
            );
        };
        // explicitly not testing the end points of tolerance
        t(0.0, 10.0, 5.0, 1.0, True);
        t(0.0, 10.0, 0.0, 1.0, Partial);
        t(0.0, 10.0, -1.0, 1.1, Partial);
        t(0.0, 10.0, -1.1, 1.0, False);
        t(-5.0, 10.0, -1.1, 1.0, True);
        t(-5.0, 10.0, -7.0, 1.0, False);
        t(-5.0, 10.0, 9.0, 1.1, Partial);
        t(-5.0, 10.0, 10.0, 1.0, Partial);
        t(-5.0, 10.0, 11.0, 1.1, Partial);
        t(-5.0, 10.0, 12.0, 1.0, False);

        // around 180
        t(170.0, 190.0, 175.0, 1.0, True);
        t(170.0, 190.0, 185.0, 1.0, True);

        // tolerance crosses seam
        t(170.0, 179.0, 181.0, 5.0, Partial);
        t(-179.0, -170.0, 179.0, 5.0, Partial);
    }

    #[ignore = "zero width partial arc no longer valid"]
    #[test]
    fn test_width_of_zero_width_arc() {
        assert_eq!(
            PartialAngleInterval::from_degrees(0.0, 0.0)
                .width()
                .to_degrees(),
            0.0
        );
    }

    #[test]
    fn test_interval_fully_contain_other_interval() {
        assert!(
            PartialAngleInterval::from_degrees(-10.0, 10.0)
                .fully_contains_interval_excluding_edge_overlaps(
                    PartialAngleInterval::from_degrees(-5.0, 5.0)
                ),
            "simple positive"
        );

        assert!(
            !PartialAngleInterval::from_degrees(-10.0, 10.0)
                .fully_contains_interval_excluding_edge_overlaps(
                    PartialAngleInterval::from_degrees(5.0, -5.0)
                ),
            "contains endpoints, but not the middle"
        );

        assert!(
            PartialAngleInterval::from_degrees(5.0, -5.0)
                .fully_contains_interval_excluding_edge_overlaps(
                    PartialAngleInterval::from_degrees(10.0, -10.0)
                ),
            "big angle fully contained"
        );
    }
    #[test]
    fn test_interval_fully_contain_other_interval__should_not_match_self() {
        let arc = PartialAngleInterval::from_degrees(0.0, 10.0);
        assert_false!(arc.fully_contains_interval_excluding_edge_overlaps(arc));
    }

    #[test]
    fn test_interval_fully_contain_other_interval__edge_cases() {
        let z = 70.374;
        let a = 50.342;
        let aa = 30.342;
        let ab = 20.342;
        let b = 3.14567;
        let c = 1.2345;
        let base_interval = PartialAngleInterval::from_degrees(b, a);
        let touching_below = PartialAngleInterval::from_degrees(c, b);
        let touching_above = PartialAngleInterval::from_degrees(a, z);
        let overlapping_below = PartialAngleInterval::from_degrees(c, aa);
        let overlapping_above = PartialAngleInterval::from_degrees(aa, z);
        let inside_touching_start = PartialAngleInterval::from_degrees(aa, a);
        let inside_touching_end = PartialAngleInterval::from_degrees(b, aa);
        let edges_in_but_wraparound = PartialAngleInterval::from_degrees(aa, ab);
        let complementary = PartialAngleInterval::from_degrees(a, b);
        let complementary_but_overlapping_top = PartialAngleInterval::from_degrees(aa, b);
        let complementary_but_overlapping_bottom = PartialAngleInterval::from_degrees(a, ab);

        assert!(!base_interval.fully_contains_interval_excluding_edge_overlaps(base_interval)); // controversial
        assert!(!base_interval.fully_contains_interval_excluding_edge_overlaps(touching_below));
        assert!(!base_interval.fully_contains_interval_excluding_edge_overlaps(touching_above));
        assert!(!base_interval.fully_contains_interval_excluding_edge_overlaps(overlapping_below));
        assert!(!base_interval.fully_contains_interval_excluding_edge_overlaps(overlapping_above));
        assert!(
            !base_interval.fully_contains_interval_excluding_edge_overlaps(inside_touching_start)
        );
        assert!(!base_interval.fully_contains_interval_excluding_edge_overlaps(inside_touching_end));
        assert_eq!(
            base_interval.fully_contains_interval_excluding_edge_overlaps(inside_touching_start),
            base_interval.fully_contains_interval_excluding_edge_overlaps(inside_touching_end)
        );
        assert!(
            !base_interval.fully_contains_interval_excluding_edge_overlaps(edges_in_but_wraparound)
        );
        assert!(!base_interval.fully_contains_interval_excluding_edge_overlaps(complementary));
        assert!(!base_interval
            .fully_contains_interval_excluding_edge_overlaps(complementary_but_overlapping_top));
        assert!(!base_interval
            .fully_contains_interval_excluding_edge_overlaps(complementary_but_overlapping_bottom));
    }

    #[test]
    fn test_center_angle_of_interval() {
        assert_about_eq!(
            PartialAngleInterval::from_degrees(10.0, 20.0)
                .center_angle()
                .to_degrees(),
            15.0
        );
        assert_about_eq!(
            PartialAngleInterval::from_degrees(-10.0, 10.0)
                .center_angle()
                .to_degrees(),
            0.0
        );
        assert_about_eq!(
            PartialAngleInterval::from_degrees(10.0, -10.0)
                .center_angle()
                .to_degrees(),
            180.0
        );
    }
    #[test]
    fn test_angle_interval_width() {
        assert_about_eq!(
            PartialAngleInterval::from_degrees(10.0, 20.0)
                .width()
                .to_degrees(),
            10.0
        );
        assert_about_eq!(
            PartialAngleInterval::from_degrees(-10.0, 10.0)
                .width()
                .to_degrees(),
            20.0
        );
        assert_about_eq!(
            PartialAngleInterval::from_degrees(10.0, -10.0)
                .width()
                .to_degrees(),
            340.0,
            1e-4
        );
    }

    #[test]
    fn test_angle_interval_from_square() {
        let arc = PartialAngleInterval::from_relative_square(vec2(1, 0));
        assert_about_eq!(arc.clockwise_end.to_degrees(), -45.0);
        assert_about_eq!(arc.anticlockwise_end.to_degrees(), 45.0);

        let arc = PartialAngleInterval::from_relative_square(vec2(0, 1));
        assert_about_eq!(arc.clockwise_end.to_degrees(), 45.0);
        assert_about_eq!(arc.anticlockwise_end.to_degrees(), 135.0);

        let arc = PartialAngleInterval::from_relative_square(vec2(-1, 0));
        assert_about_eq!(arc.clockwise_end.to_degrees(), 135.0);
        assert_about_eq!(arc.anticlockwise_end.to_degrees(), -135.0);

        let arc = PartialAngleInterval::from_relative_square(vec2(0, -1));
        assert_about_eq!(arc.clockwise_end.to_degrees(), -135.0);
        assert_about_eq!(arc.anticlockwise_end.to_degrees(), -45.0);
    }

    #[test]
    fn test_partial_overlap_includes_almost_peaking_out() {
        assert!(PartialAngleInterval::from_degrees(10.0, 20.0)
            .partially_overlaps_other_while_including_edges(PartialAngleInterval::from_degrees(
                10.0, 15.0,
            )));
        assert!(PartialAngleInterval::from_degrees(10.0, 20.0)
            .partially_overlaps_other_while_including_edges(PartialAngleInterval::from_degrees(
                15.0, 20.0,
            )));
        assert!(PartialAngleInterval::from_degrees(10.0, 20.0)
            .partially_overlaps_other_while_including_edges(PartialAngleInterval::from_degrees(
                10.0, 20.0,
            )));
    }

    #[test]
    fn test_partial_overlap_includes_almost_peaking_out__wraparound_case() {
        assert!(PartialAngleInterval::from_degrees(315.0, 270.0)
            .partially_overlaps_other_while_including_edges(PartialAngleInterval::from_degrees(
                225.0, 315.0,
            )));
    }

    #[test]
    fn test_angle_wraparound_invariance() {
        assert!(PartialAngleInterval::from_degrees(315.0, 270.0)
            .partially_overlaps_other_while_including_edges(PartialAngleInterval::from_degrees(
                280.0, 315.0,
            )));

        assert!(PartialAngleInterval::from_degrees(315.0, 270.0)
            .partially_overlaps_other_while_including_edges(PartialAngleInterval::from_degrees(
                280.0 - 360.0,
                315.0 - 360.0,
            )));
    }

    #[test]
    fn test_arc_from_square_face_is_smallish() {
        ORTHOGONAL_STEPS.into_iter().for_each(|step| {
            assert!(
                PartialAngleInterval::from_relative_square_face((vec2(3, 6), step))
                    .width()
                    .to_degrees()
                    < 45.0
            )
        });
    }

    #[test]
    fn test_arc_from_square_face__observed_failure_at_right_face_of_one_block_right() {
        assert_about_eq!(
            PartialAngleInterval::from_relative_square_face((STEP_RIGHT, STEP_RIGHT))
                .anticlockwise_end
                .to_degrees(),
            PartialAngleInterval::from_relative_square(STEP_RIGHT * 2)
                .anticlockwise_end
                .to_degrees()
        );
    }
    #[test]
    fn test_arc_at_least_fully_overlap() {
        let cw = Angle::degrees(5.0);
        let ccw = Angle::degrees(25.0);
        let d = Angle::degrees(1.0);

        let arc = PartialAngleInterval::from_angles(cw, ccw);
        let arc_extend_cw = PartialAngleInterval::from_angles(cw - d, ccw);
        let arc_retract_cw = PartialAngleInterval::from_angles(cw + d, ccw);
        let arc_extend_ccw = PartialAngleInterval::from_angles(cw, ccw + d);
        let arc_retract_ccw = PartialAngleInterval::from_angles(cw, ccw - d);
        let arc_extend_both = PartialAngleInterval::from_angles(cw - d, ccw + d);
        let arc_retract_both = PartialAngleInterval::from_angles(cw + d, ccw - d);

        assert!(arc.at_least_fully_overlaps(arc));

        assert_false!(arc.at_least_fully_overlaps(arc_extend_cw));
        assert_false!(arc.at_least_fully_overlaps(arc_extend_ccw));
        assert_false!(arc.at_least_fully_overlaps(arc_extend_both));
        assert!(arc.at_least_fully_overlaps(arc_retract_cw));
        assert!(arc.at_least_fully_overlaps(arc_retract_ccw));
        assert!(arc.at_least_fully_overlaps(arc_retract_both));

        //commutation
        assert!(arc_extend_cw.at_least_fully_overlaps(arc));
        assert!(arc_extend_ccw.at_least_fully_overlaps(arc));
        assert!(arc_extend_both.at_least_fully_overlaps(arc));
        assert_false!(arc_retract_cw.at_least_fully_overlaps(arc));
        assert_false!(arc_retract_ccw.at_least_fully_overlaps(arc));
        assert_false!(arc_retract_both.at_least_fully_overlaps(arc));

        // complements
        assert_false!(arc.at_least_fully_overlaps(arc.complement()));

        assert_false!(arc.at_least_fully_overlaps(arc_extend_cw.complement()));
        assert_false!(arc.at_least_fully_overlaps(arc_extend_ccw.complement()));
        assert_false!(arc.at_least_fully_overlaps(arc_retract_cw.complement()));
        assert_false!(arc.at_least_fully_overlaps(arc_retract_ccw.complement()));
    }

    #[test]
    fn test_angle_interval_from_octant() {
        // Are the exact values really important?
        let octant_start_end_rad_deg = vec![
            (0, (0.0, PI / 4.0), (0.0, 45.0)),
            (1, (PI / 4.0, PI / 2.0), (45.0, 90.0)),
            (2, (PI / 2.0, PI * 3.0 / 4.0), (90.0, 135.0)),
            (3, (PI * 3.0 / 4.0, PI), (135.0, 180.0)),
            (4, (PI, -PI * 3.0 / 4.0), (180.0, -135.0)),
            (5, (-PI * 3.0 / 4.0, -PI / 2.0), (-135.0, -90.0)),
            (6, (-PI / 2.0, -PI / 4.0), (-90.0, -45.0)),
            (7, (-PI / 4.0, 0.0), (-45.0, 0.0)),
        ];
        octant_start_end_rad_deg.into_iter().for_each(
            |(octant, (rad_start, rad_end), (deg_start, deg_end))| {
                let interval = PartialAngleInterval::from_octant(Octant::new(octant));
                let start = interval.clockwise_end();
                let end = interval.anticlockwise_end();
                assert_about_eq!(start.radians, rad_start);
                assert_about_eq!(end.radians, rad_end);
                assert_about_eq!(start.to_degrees(), deg_start, 1e-4);
                assert_about_eq!(end.to_degrees(), deg_end, 1e-4);
            },
        )
    }

    #[test]
    fn test_split_into_octants__one_octant() {
        let single_octant_degrees = vec![
            (0.0, 5.0),
            (0.0, 45.0),
            (45.0, 90.0),
            (89.0, 90.0),
            (-30.0, 0.0),
            (-30.0, -1.0),
        ];

        single_octant_degrees.iter().for_each(|p| {
            let arc = PartialAngleInterval::from_degrees(p.0, p.1);
            assert_eq!(arc.split_into_octants_in_ccw_order()[0], arc);
        })
    }
    #[test]
    fn test_split_into_octants__more_than_one() {
        let arc = PartialAngleInterval::from_degrees(0.0, 91.0);
        let parts = arc.split_into_octants_in_ccw_order();
        assert_eq!(parts.len(), 3);
        assert_about_eq!(parts[0].clockwise_end().to_degrees(), 0.0);
        assert_about_eq!(parts[0].anticlockwise_end().to_degrees(), 45.0);
        assert_about_eq!(parts[1].clockwise_end().to_degrees(), 45.0);
        assert_about_eq!(parts[1].anticlockwise_end().to_degrees(), 90.0);
        assert_about_eq!(parts[2].clockwise_end().to_degrees(), 90.0);
        assert_about_eq!(parts[2].anticlockwise_end().to_degrees(), 91.0);
    }
    #[test]
    fn test_split_into_octants__slight_tolerance() {
        let err = 1e-6;
        assert_eq!(
            PartialAngleInterval::from_degrees(0.0 - err, 45.0 + err)
                .split_into_octants_in_ccw_order()
                .len(),
            1
        );
        assert_eq!(
            PartialAngleInterval::from_degrees(-1.0 - err, 45.0 + err)
                .split_into_octants_in_ccw_order()
                .len(),
            2
        );
        assert_eq!(
            PartialAngleInterval::from_degrees(0.0 - err, 180.0 + err)
                .split_into_octants_in_ccw_order()
                .len(),
            4
        );
    }

    // TODO: These edge cases are suspect
    #[test]
    fn test_split_into_octants__edge_cases() {
        let err = PartialAngleInterval::default_tolerance() / 2.0;
        let angle = Angle::degrees(0.0);

        // should have zero octants, because given interval is very small
        assert_eq!(
            PartialAngleInterval::from_angles(angle - err, angle + err)
                .split_into_octants_in_ccw_order()
                .len(),
            0
        );
        // should be full circle, because given interval is very large
        assert_eq!(
            PartialAngleInterval::from_angles(angle + err, angle - err)
                .split_into_octants_in_ccw_order()
                .len(),
            8
        );
    }
    #[test]
    fn test_split_into_octants__all_the_way_around_case() {
        let arc = PartialAngleInterval::from_degrees(10.0, 5.0);
        let parts = arc.split_into_octants_in_ccw_order();
        assert_eq!(parts.len(), 9);
        assert_about_eq!(parts[0].clockwise_end().to_degrees(), 10.0);
        assert_about_eq!(parts[0].anticlockwise_end().to_degrees(), 45.0);
        assert_about_eq!(parts[8].clockwise_end().to_degrees(), 0.0);
        assert_about_eq!(parts[8].anticlockwise_end().to_degrees(), 5.0);
    }
    #[test]
    fn test_contains_partial_arc__observed_error() {
        let a = PartialAngleInterval::from_degrees(20.0, 0.0);
        let b = PartialAngleInterval::from_degrees(40.0, 30.0);
        let t = default_angle_tolerance_for_tests();
        assert!(a.contains_partial_arc(b, t).is_false());
    }
    #[test]
    fn test_contains_partial_arc__wide_intervals() {
        use BoolWithPartial::*;
        let a = PartialAngleInterval::from_degrees(10.0, 0.0);
        let b = PartialAngleInterval::from_degrees(30.0, 11.0);
        let t = default_angle_tolerance_for_tests();
        assert_eq!(
            a.contains_partial_arc(b.rotated_ccw(FAngle::degrees(0.0)), t),
            False
        );
        assert_eq!(
            a.contains_partial_arc(b.rotated_ccw(FAngle::degrees(-1.0)), t),
            False
        );
        assert_eq!(
            a.contains_partial_arc(b.rotated_ccw(FAngle::degrees(-2.0)), t),
            False
        );
        assert_eq!(
            a.contains_partial_arc(b.rotated_ccw(FAngle::degrees(-2.0)), t),
            False
        );
    }
}
