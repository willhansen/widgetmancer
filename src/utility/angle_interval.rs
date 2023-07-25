use std::collections::{HashMap, HashSet};
use std::f32::consts::{PI, TAU};
use std::fmt::{Display, Formatter};
use std::ops::{Add, Neg, Sub};

use euclid::{default, vec2, Angle};
use getset::CopyGetters;
use itertools::Itertools;
use ntest::assert_false;
use num::traits::FloatConst;
use ordered_float::OrderedFloat;
use termion::cursor::Left;

use crate::fov_stuff::{OctantFOVSquareSequenceIter, SquareVisibilityFromOneLargeShadow};
use crate::utility::coordinate_frame_conversions::{WorldMove, WorldStep};
use crate::utility::{
    abs_angle_distance, better_angle_from_x_axis, rotated_n_quarter_turns_counter_clockwise,
    standardize_angle, Octant, OrthogonalWorldStep, QuarterTurnsAnticlockwise,
    RelativeSquareWithOrthogonalDir, SquareWithOrthogonalDir, ORTHOGONAL_STEPS, STEP_DOWN_LEFT,
    STEP_DOWN_RIGHT, STEP_UP_LEFT, STEP_UP_RIGHT,
};
#[derive(Debug, Copy, Clone, PartialEq)]
pub enum AngleInterval {
    Empty,
    FullCircle,
    Partial(PartialAngleInterval),
}
#[derive(Debug, Copy, Clone, PartialEq, CopyGetters)]
#[get_copy = "pub"]
pub struct PartialAngleInterval {
    clockwise_end: Angle<f32>,
    anticlockwise_end: Angle<f32>,
}

impl PartialAngleInterval {
    pub fn new_interval(clockwise_end: Angle<f32>, anticlockwise_end: Angle<f32>) -> Self {
        let (cw, ccw) = (
            standardize_angle(clockwise_end),
            standardize_angle(anticlockwise_end),
        );
        assert_ne!(cw, ccw);
        Self {
            clockwise_end: cw,
            anticlockwise_end: ccw,
        }
    }
    pub fn from_degrees(clockwise_end_in_degrees: f32, anticlockwise_end_in_degrees: f32) -> Self {
        Self::new_interval(
            Angle::degrees(clockwise_end_in_degrees),
            Angle::degrees(anticlockwise_end_in_degrees),
        )
    }
    pub fn from_radians(clockwise_end_in_radians: f32, anticlockwise_end_in_radians: f32) -> Self {
        Self::new_interval(
            Angle::radians(clockwise_end_in_radians),
            Angle::radians(anticlockwise_end_in_radians),
        )
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

    pub fn from_relative_square(relative_square: WorldStep) -> Self {
        assert_ne!(relative_square, vec2(0, 0));
        let rel_square_center = relative_square.to_f32();
        let rel_square_corners: Vec<WorldMove> = vec![
            rel_square_center + STEP_UP_RIGHT.to_f32() * 0.5,
            rel_square_center + STEP_UP_LEFT.to_f32() * 0.5,
            rel_square_center + STEP_DOWN_LEFT.to_f32() * 0.5,
            rel_square_center + STEP_DOWN_RIGHT.to_f32() * 0.5,
        ];

        let center_angle = better_angle_from_x_axis(rel_square_center);
        let corner_angles: Vec<Angle<f32>> = rel_square_corners
            .iter()
            .map(|rel_corner_point| better_angle_from_x_axis(*rel_corner_point))
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
    pub fn from_relative_square_face(rel_face: impl Into<RelativeSquareWithOrthogonalDir>) -> Self {
        let (relative_square, face_direction): (WorldStep, OrthogonalWorldStep) =
            rel_face.into().into();
        let square_center = relative_square.to_f32();
        let face_center = square_center + face_direction.step().to_f32() / 2.0;
        let face_corners = [1, -1].map(|sign| {
            face_center
                + rotated_n_quarter_turns_counter_clockwise(
                    face_direction.step().to_f32() / 2.0,
                    sign,
                )
        });

        let center_angle = better_angle_from_x_axis(face_center);
        let face_corner_angles = face_corners.map(better_angle_from_x_axis);

        let first_corner_angle_is_more_clockwise =
            center_angle.angle_to(face_corner_angles[0]).radians < 0.0;

        if first_corner_angle_is_more_clockwise {
            PartialAngleInterval::new_interval(face_corner_angles[0], face_corner_angles[1])
        } else {
            PartialAngleInterval::new_interval(face_corner_angles[1], face_corner_angles[0])
        }
    }

    pub fn intersection(&self, other: Self) -> Self {
        assert!(self.overlapping_but_not_exactly_touching(other));
        assert_false!(self.has_wraparound_double_overlap_not_counting_touching(other));

        PartialAngleInterval {
            anticlockwise_end: if self.contains_angle_not_including_edges(other.anticlockwise_end) {
                other.anticlockwise_end
            } else {
                self.anticlockwise_end
            },
            clockwise_end: if self.contains_angle_not_including_edges(other.clockwise_end) {
                other.clockwise_end
            } else {
                self.clockwise_end
            },
        }
    }

    pub fn union(&self, other: Self) -> Self {
        assert!(self.overlaps_or_touches(other));
        let result = PartialAngleInterval {
            anticlockwise_end: if self.contains_or_touches_angle(other.anticlockwise_end) {
                self.anticlockwise_end
            } else {
                other.anticlockwise_end
            },
            clockwise_end: if self.contains_or_touches_angle(other.clockwise_end) {
                self.clockwise_end
            } else {
                other.clockwise_end
            },
        };
        //println!("A:     {}\nB:     {}\nA + B: {}", self, other, result);
        result
    }
    pub fn complement(&self) -> Self {
        PartialAngleInterval::new_interval(self.anticlockwise_end, self.clockwise_end)
    }

    pub fn at_least_fully_overlaps(&self, other: PartialAngleInterval) -> bool {
        self.num_contained_or_touching_edges(other) == 2 && self.width() >= other.width()
    }

    fn partially_or_fully_overlaps_without_exactly_touching(
        &self,
        other: PartialAngleInterval,
    ) -> bool {
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

    pub fn touches_or_overlaps(&self, other: PartialAngleInterval) -> bool {
        self.num_contained_or_touching_edges(other) > 0
            || other.num_contained_or_touching_edges(*self) > 0
    }

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
    fn has_wraparound_double_overlap_not_counting_touching(
        &self,
        other: PartialAngleInterval,
    ) -> bool {
        self.num_contained_not_touching_edges(other) == 2
            && other.num_contained_not_touching_edges(*self) == 2
    }
    pub fn partially_overlaps_other_while_including_edges(
        &self,
        other: PartialAngleInterval,
    ) -> bool {
        let contained_in_self = self.num_contained_or_touching_edges(other);
        let contained_in_other = other.num_contained_or_touching_edges(*self);
        contained_in_self >= 1 && contained_in_other >= 1
    }
    pub fn subtract(&self, other: PartialAngleInterval) -> Vec<PartialAngleInterval> {
        if !self.touches_or_overlaps(other) {
            return vec![self.clone()];
        }

        let mut split_results = vec![];
        if self.contains_or_touches_angle(other.clockwise_end)
            && self.clockwise_end != other.clockwise_end
        {
            let below_interval =
                PartialAngleInterval::new_interval(self.clockwise_end, other.clockwise_end);
            split_results.push(below_interval);
        }
        if self.contains_or_touches_angle(other.anticlockwise_end)
            && self.anticlockwise_end != other.anticlockwise_end
        {
            let above_interval =
                PartialAngleInterval::new_interval(other.anticlockwise_end, self.anticlockwise_end);
            split_results.push(above_interval);
        }
        split_results
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
            PartialAngleInterval::new_interval(other.clockwise_end, self.clockwise_end).width();
        let anticlockwise_dist =
            PartialAngleInterval::new_interval(self.anticlockwise_end, other.anticlockwise_end)
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
    fn exactly_touches_arc(&self, other: PartialAngleInterval) -> bool {
        let edges_touch = self.clockwise_end == other.anticlockwise_end
            || other.clockwise_end == self.anticlockwise_end;

        let contains_other_edge = self.contains_angle_not_including_edges(other.clockwise_end)
            || self.contains_angle_not_including_edges(other.anticlockwise_end);

        edges_touch && !contains_other_edge
    }
    pub fn overlaps_or_touches(&self, other: PartialAngleInterval) -> bool {
        self.partially_or_fully_overlaps_without_exactly_touching(other)
            || self.exactly_touches_arc(other)
    }
    pub fn overlapping_but_not_exactly_touching(&self, other: PartialAngleInterval) -> bool {
        self.partially_or_fully_overlaps_without_exactly_touching(other)
            && !self.exactly_touches_arc(other)
    }
    pub fn overlaps_other_by_at_least_this_much(
        &self,
        other: PartialAngleInterval,
        thresh: Angle<f32>,
    ) -> bool {
        let self_minus_other = self.subtract(other);

        let sum_of_widths_of_self_minus_other = self_minus_other
            .iter()
            .map(|angle_interval| angle_interval.width())
            .sum();

        let overlap = self.width() - sum_of_widths_of_self_minus_other;

        overlap.radians >= thresh.radians
    }

    fn exactly_touches_angle(&self, angle: Angle<f32>) -> bool {
        self.clockwise_end == angle || angle == self.anticlockwise_end
    }
    fn contains_angle_not_including_edges(&self, angle: Angle<f32>) -> bool {
        if self.exactly_touches_angle(angle) {
            return false;
        }

        self.center_angle().angle_to(angle).radians.abs() < self.width().radians / 2.0
    }
    pub(crate) fn contains_or_touches_angle(&self, angle: Angle<f32>) -> bool {
        // both edges count
        if self.exactly_touches_angle(angle) {
            return true;
        }

        self.center_angle().angle_to(angle).radians.abs() <= self.width().radians / 2.0
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
    pub fn other_is_sub_interval_of_this(&self, other: Self) -> bool {
        self.contains_or_touches_angle(other.anticlockwise_end)
            && self.contains_or_touches_angle(other.clockwise_end)
            && self.contains_or_touches_angle(other.center_angle())
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
    pub fn rotated_quarter_turns(&self, quarter_turns: QuarterTurnsAnticlockwise) -> Self {
        PartialAngleInterval {
            clockwise_end: quarter_turns.rotate_angle(self.clockwise_end),
            anticlockwise_end: quarter_turns.rotate_angle(self.anticlockwise_end),
        }
    }
    pub fn rotated(&self, d_angle: Angle<f32>) -> Self {
        PartialAngleInterval::new_interval(
            self.clockwise_end + d_angle,
            self.anticlockwise_end + d_angle,
        )
    }
    pub fn touched_squares_going_outwards_and_ccw(&self) -> impl Iterator {
        self.split_into_octants_in_ccw_order()
            .into_iter()
            .map(|arc| arc.touched_rel_squares_going_outwards_in_one_octant())
            .collect_vec();

        todo!();
        0..4
    }
    fn split_into_octants_in_ccw_order(&self) -> Vec<Self> {
        todo!()
    }
    fn touched_rel_squares_going_outwards_in_one_octant(&self) -> impl Iterator<Item = WorldStep> {
        assert!(self.in_one_octant());
        todo!();
        ORTHOGONAL_STEPS.into_iter()
    }
    fn touched_rel_squares_going_outwards_in_one_octant_with_placeholders(
        &self,
    ) -> impl Iterator<Item = Option<WorldStep>> + '_ {
        assert!(self.in_one_octant());
        OctantFOVSquareSequenceIter::new_from_center(self.octant().unwrap()).map(|step| {
            if self.partially_or_fully_overlaps_without_exactly_touching(
                Self::from_relative_square(step),
            ) {
                Some(step)
            } else {
                None
            }
        })
    }
    fn in_one_octant(&self) -> bool {
        self.octant().is_some()
    }
    fn octant(&self) -> Option<Octant> {
        Octant::all_octants()
            .find(|octant| Self::from_octant(*octant).other_is_sub_interval_of_this(*self))
    }
}

impl Add for PartialAngleInterval {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        self.union(rhs)
    }
}

impl Sub for PartialAngleInterval {
    type Output = Vec<Self>;

    fn sub(self, rhs: Self) -> Self::Output {
        self.subtract(rhs)
    }
}

impl Neg for PartialAngleInterval {
    type Output = Self;

    fn neg(self) -> Self::Output {
        self.complement()
    }
}

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

#[derive(Default, Debug, Clone, PartialEq)]
#[deprecated]
pub struct AngleIntervalSet {
    intervals: Vec<PartialAngleInterval>,
}

impl AngleIntervalSet {
    pub fn new() -> Self {
        AngleIntervalSet { intervals: vec![] }
    }

    fn standardize(&mut self) {
        if (0..=1).contains(&self.intervals.len()) {
            return;
        }
        // sort by start angle
        self.intervals
            .sort_by_key(|i| OrderedFloat(i.clockwise_end.radians));
        let mut new_intervals = vec![];
        let mut accumulating_interval = self.intervals[0];
        for i in 1..self.intervals.len() {
            let interval = self.intervals[i];
            if accumulating_interval.overlaps_or_touches(interval) {
                accumulating_interval = accumulating_interval.union(interval);
            } else {
                new_intervals.push(accumulating_interval);
                accumulating_interval = interval;
            }
            let is_last_interval = i == self.intervals.len() - 1;
            let is_only_interval = new_intervals.is_empty();
            if is_last_interval {
                if !is_only_interval && accumulating_interval.overlaps_or_touches(new_intervals[0])
                {
                    new_intervals[0] = new_intervals[0].union(accumulating_interval);
                } else {
                    new_intervals.push(accumulating_interval);
                }
            }
        }
        self.intervals = new_intervals;
    }

    fn is_valid(&self) -> bool {
        let sorted = self.intervals.is_sorted_by_key(|i| i.clockwise_end);
        if !sorted {
            return false;
        }
        if self.intervals.len() < 2 {
            return true;
        }
        let has_bad_overlap = self
            .intervals
            .iter()
            .circular_tuple_windows()
            .any(|(&interval, &next_interval)| interval.overlaps_or_touches(next_interval));
        !has_bad_overlap
    }

    pub fn add_interval(&mut self, interval: PartialAngleInterval) {
        self.intervals.push(interval);
        self.standardize();
    }
    pub fn fully_contains_interval(&self, interval: PartialAngleInterval) -> bool {
        self.intervals
            .iter()
            .any(|i| i.fully_contains_interval_excluding_edge_overlaps(interval))
    }
    pub fn partially_or_fully_overlaps_interval(&self, interval: PartialAngleInterval) -> bool {
        self.intervals.iter().any(|i: &PartialAngleInterval| {
            i.partially_or_fully_overlaps_without_exactly_touching(interval)
        })
    }
    pub fn partially_overlaps_interval(&self, interval: PartialAngleInterval) -> bool {
        self.most_overlapped_edge_of_set(interval).is_some()
    }
    pub fn most_overlapped_edge_of_set(
        &self,
        interval: PartialAngleInterval,
    ) -> Option<DirectionalAngularEdge> {
        // TODO: don't just get the first one
        self.intervals
            .iter()
            .filter_map(|&arc_from_set: &PartialAngleInterval| {
                if arc_from_set.partially_overlaps_other_while_including_edges(interval) {
                    Some(arc_from_set.edge_of_this_overlapped_by(interval))
                } else if interval.fully_contains_interval_excluding_edge_overlaps(arc_from_set) {
                    Some(arc_from_set.edge_of_this_deeper_in(interval))
                } else {
                    None
                }
            })
            .next()
    }
}

impl Display for PartialAngleInterval {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "From {:.1}° to {:.1}°",
            self.clockwise_end.to_degrees(),
            self.anticlockwise_end.to_degrees()
        )
    }
}

impl Display for AngleIntervalSet {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "contained_intervals:")?;
        if self.intervals.is_empty() {
            write!(f, "\n    none")?;
        }
        for interval in &self.intervals {
            write!(f, "\n    {}", interval)?;
        }
        Ok(())
    }
}

impl AngleInterval {
    pub fn complement(&self) -> Self {
        match self {
            AngleInterval::Empty => Self::FullCircle,
            AngleInterval::FullCircle => Self::Empty,
            AngleInterval::Partial(partial) => Self::Partial(PartialAngleInterval::new_interval(
                partial.anticlockwise_end,
                partial.clockwise_end,
            )),
        }
    }
}

#[cfg(test)]
mod tests {
    use ntest::{assert_about_eq, assert_false};
    use num::zero;
    use pretty_assertions::{assert_eq, assert_ne};

    use crate::utility::{STEP_DOWN, STEP_RIGHT, STEP_UP};

    use super::*;

    #[test]
    fn test_interval_overlap() {
        let interval_b = PartialAngleInterval::from_degrees(5.0, 15.0);
        let interval_a = PartialAngleInterval::from_degrees(0.0, 10.0);

        assert!(
            interval_a.partially_or_fully_overlaps_without_exactly_touching(interval_a),
            "self overlap"
        );
        assert!(
            interval_b.partially_or_fully_overlaps_without_exactly_touching(interval_b),
            "other self overlap"
        );
        assert!(
            interval_a.partially_or_fully_overlaps_without_exactly_touching(interval_b),
            "basic overlap"
        );
        assert!(
            interval_b.partially_or_fully_overlaps_without_exactly_touching(interval_a),
            "commutative"
        );
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
    fn test_interval_overlap_edges() {
        let interval_a = PartialAngleInterval::from_degrees(0.0, 10.0);
        let interval_c = PartialAngleInterval::from_degrees(10.0, 50.0);
        assert!(
            !interval_a.partially_or_fully_overlaps_without_exactly_touching(interval_c),
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
    fn test_angle_interval_union() {
        assert_eq!(
            PartialAngleInterval::from_degrees(80.0, 100.0)
                .union(PartialAngleInterval::from_degrees(40.0, 90.0)),
            PartialAngleInterval::from_degrees(40.0, 100.0),
            "from overlap"
        );

        assert_eq!(
            PartialAngleInterval::from_degrees(80.0, 100.0)
                .union(PartialAngleInterval::from_degrees(40.0, 80.0)),
            PartialAngleInterval::from_degrees(40.0, 100.0),
            "from exactly touching"
        );
    }

    #[test]
    fn test_angle_interval_intersection__simple_overlap() {
        assert_eq!(
            PartialAngleInterval::from_degrees(80.0, 100.0)
                .intersection(PartialAngleInterval::from_degrees(40.0, 90.0)),
            PartialAngleInterval::from_degrees(80.0, 90.0),
            "from overlap"
        );
    }

    #[test]
    #[should_panic]
    fn test_angle_interval_intersection__no_overlap() {
        PartialAngleInterval::from_degrees(95.0, 100.0)
            .intersection(PartialAngleInterval::from_degrees(40.0, 90.0));
    }

    #[test]
    fn test_angle_interval_intersection__full_overlap() {
        let small = PartialAngleInterval::from_degrees(80.0, 100.0);
        let big = PartialAngleInterval::from_degrees(60.0, 120.0);
        assert_eq!(big.intersection(small), small);
        assert_eq!(small.intersection(big), small);
    }

    #[test]
    #[should_panic]
    fn test_angle_interval_intersection__wraparound_double_overlap() {
        let small = PartialAngleInterval::from_degrees(80.0, 100.0);
        let big = PartialAngleInterval::from_degrees(60.0, 120.0);
        big.intersection(small.complement());
    }

    #[test]
    fn test_angle_interval_set__standardization() {
        let mut angle_interval_set = AngleIntervalSet {
            intervals: vec![
                PartialAngleInterval::from_degrees(10.0, 15.0),
                PartialAngleInterval::from_degrees(15.0, 30.0),
            ],
        };
        assert!(!angle_interval_set.is_valid());
        angle_interval_set.standardize();
        assert!(angle_interval_set.is_valid());
        assert_eq!(
            angle_interval_set,
            AngleIntervalSet {
                intervals: vec![PartialAngleInterval::from_degrees(10.0, 30.0)]
            }
        );
    }

    #[test]
    fn test_angle_interval_set__standardize_with_one() {
        let interval = PartialAngleInterval::from_degrees(15.0, 30.0);
        let mut angle_interval_set = AngleIntervalSet {
            intervals: vec![interval],
        };
        assert_false!(angle_interval_set.intervals.is_empty());
        angle_interval_set.standardize();
        assert_false!(angle_interval_set.intervals.is_empty());
    }

    #[ignore = "moving away from angle sets"]
    #[test]
    fn test_angle_interval_set__overlap_and_adding() {
        let mut angle_interval_set = AngleIntervalSet::default();
        assert!(angle_interval_set.intervals.is_empty());

        let interval_a = PartialAngleInterval::from_degrees(30.0, 45.0);
        let interval_b = PartialAngleInterval::from_degrees(10.0, 35.0);
        assert_false!(angle_interval_set.partially_or_fully_overlaps_interval(interval_a));
        angle_interval_set.add_interval(interval_b);

        assert_false!(angle_interval_set.intervals.is_empty());

        //println!("interval a: {}", interval_a);
        //println!("interval b: {}", interval_b);
        //println!("the set: {}", angle_interval_set);
        assert!(angle_interval_set.partially_or_fully_overlaps_interval(interval_a));
        assert_false!(angle_interval_set.fully_contains_interval(interval_a));

        angle_interval_set.add_interval(interval_a);

        assert!(angle_interval_set.partially_or_fully_overlaps_interval(interval_a));
        assert!(angle_interval_set.fully_contains_interval(interval_a));
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
    fn test_angle_interval_set__standardize_but_no_change_required() {
        let mut the_set = AngleIntervalSet {
            intervals: vec![
                PartialAngleInterval::from_degrees(30.0, 40.0),
                PartialAngleInterval::from_degrees(50.0, 60.0),
                PartialAngleInterval::from_degrees(70.0, 80.0),
            ],
        };
        let before_adding = the_set.clone();
        the_set.add_interval(PartialAngleInterval::from_degrees(33.0, 35.0));
        assert_eq!(before_adding, the_set);
    }

    #[test]
    fn test_angle_interval_set_validity() {
        assert!(AngleIntervalSet {
            intervals: vec![PartialAngleInterval::from_degrees(10.0, 20.0)]
        }
        .is_valid());

        assert!(AngleIntervalSet {
            intervals: vec![
                PartialAngleInterval::from_degrees(10.0, 20.0),
                PartialAngleInterval::from_degrees(30.0, 50.0),
            ]
        }
        .is_valid());
        assert!(!AngleIntervalSet {
            intervals: vec![
                PartialAngleInterval::from_degrees(10.0, 20.0),
                PartialAngleInterval::from_degrees(20.0, 50.0),
            ]
        }
        .is_valid());
    }

    #[test]
    fn test_get_angle_endpoint_of_overlapped_region() {
        let interval_set = AngleIntervalSet {
            intervals: vec![PartialAngleInterval::from_degrees(0.0, 20.0)],
        };
        let single_interval = PartialAngleInterval::from_degrees(-25.0, 5.0);

        assert_eq!(
            interval_set.most_overlapped_edge_of_set(single_interval),
            Some(DirectionalAngularEdge {
                angle: Angle::degrees(0.0),
                is_clockwise_edge: true,
            })
        );
    }

    #[test]
    fn test_get_angle_endpoint_of_more_overlapped_region() {
        let interval_set = AngleIntervalSet {
            intervals: vec![
                PartialAngleInterval::from_degrees(0.0, 20.0),
                PartialAngleInterval::from_degrees(22.0, 50.0),
            ],
        };
        let single_interval = PartialAngleInterval::from_degrees(10.0, 25.0);

        assert_eq!(
            interval_set.most_overlapped_edge_of_set(single_interval),
            Some(DirectionalAngularEdge {
                angle: Angle::degrees(20.0),
                is_clockwise_edge: false,
            })
        );
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
    fn test_split_interval_around_interval() {
        let new_arcs = assert_eq!(
            PartialAngleInterval::from_degrees(0.0, 30.0)
                .subtract(PartialAngleInterval::from_degrees(10.0, 20.0)),
            vec![
                PartialAngleInterval::from_degrees(0.0, 10.0),
                PartialAngleInterval::from_degrees(20.0, 30.0),
            ]
        );
    }

    #[test]
    fn test_interval_subtraction__touching_from_inside() {
        let new_arcs = assert_eq!(
            PartialAngleInterval::from_degrees(0.0, 30.0)
                .subtract(PartialAngleInterval::from_degrees(-10.0, 30.0)),
            vec![]
        );
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
    fn test_most_overlapped_edge_of_arc() {
        assert_eq!(
            PartialAngleInterval::from_degrees(135.0, 90.0)
                .most_overlapped_edge_of_self(PartialAngleInterval::from_degrees(45.0, 135.0)),
            DirectionalAngularEdge::new(Angle::degrees(90.0), false)
        );
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

        let arc = PartialAngleInterval::new_interval(cw, ccw);
        let arc_extend_cw = PartialAngleInterval::new_interval(cw - d, ccw);
        let arc_retract_cw = PartialAngleInterval::new_interval(cw + d, ccw);
        let arc_extend_ccw = PartialAngleInterval::new_interval(cw, ccw + d);
        let arc_retract_ccw = PartialAngleInterval::new_interval(cw, ccw - d);
        let arc_extend_both = PartialAngleInterval::new_interval(cw - d, ccw + d);
        let arc_retract_both = PartialAngleInterval::new_interval(cw + d, ccw - d);

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
    fn test_arc_exact_touch() {
        let cw = Angle::degrees(5.0);
        let ccw = Angle::degrees(25.0);
        let d = Angle::degrees(1.0);

        let arc = PartialAngleInterval::new_interval(cw, ccw);
        let arc_extend_cw = PartialAngleInterval::new_interval(cw - d, ccw);
        let arc_retract_cw = PartialAngleInterval::new_interval(cw + d, ccw);
        let arc_extend_ccw = PartialAngleInterval::new_interval(cw, ccw + d);
        let arc_retract_ccw = PartialAngleInterval::new_interval(cw, ccw - d);
        let arc_extend_both = PartialAngleInterval::new_interval(cw - d, ccw + d);
        let arc_retract_both = PartialAngleInterval::new_interval(cw + d, ccw - d);

        assert!(arc.exactly_touches_arc(arc_extend_ccw.complement()));
        assert_false!(arc.exactly_touches_arc(arc_retract_ccw.complement()));

        assert_false!(arc.exactly_touches_arc(arc));
        assert!(arc.exactly_touches_arc(arc.complement()))
    }

    #[test]
    fn test_angle_interval_from_octant() {
        // Are the exact values really important?
        let octant_start_end = vec![
            (0, 0.0, PI / 4.0),
            (1, PI / 4.0, PI / 2.0),
            (2, PI / 2.0, PI * 3.0 / 4.0),
            (3, PI * 3.0 / 4.0, PI),
            (4, PI, -PI * 3.0 / 4.0),
            (5, -PI * 3.0 / 4.0, -PI / 2.0),
            (6, -PI / 2.0, -PI / 4.0),
            (7, -PI / 4.0, 0.0),
        ];
        octant_start_end
            .into_iter()
            .for_each(|(octant, start, end)| {
                assert_about_eq!(
                    PartialAngleInterval::from_octant(Octant::new(octant))
                        .clockwise_end
                        .radians,
                    start
                );
                assert_about_eq!(
                    PartialAngleInterval::from_octant(Octant::new(octant))
                        .anticlockwise_end
                        .radians,
                    end
                );
            })
    }

    #[test]
    fn test_overlap_at_least_this_much() {
        let arc1 = PartialAngleInterval::from_degrees(0.0, 90.0);
        let arc2 = PartialAngleInterval::from_degrees(90.0, 180.0);

        assert_false!(arc1.overlaps_other_by_at_least_this_much(arc2, Angle::degrees(5.0)));
        assert_false!(arc1.overlaps_other_by_at_least_this_much(
            arc2.rotated(Angle::degrees(-4.9)),
            Angle::degrees(5.0)
        ));
        assert!(arc1.overlaps_other_by_at_least_this_much(
            arc2.rotated(Angle::degrees(-5.1)),
            Angle::degrees(5.0),
        ));
        assert_false!(arc1.overlaps_other_by_at_least_this_much(
            arc2.rotated(Angle::degrees(5.1)),
            Angle::degrees(5.0)
        ));
        // TODO: more cases here
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
        let arc = PartialAngleInterval::from_degrees(0.0, 90.0);
        todo!()
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
    fn test_get_containing_octant__tricky_case() {
        assert!(PartialAngleInterval::from_degrees(-10.0, -20.0)
            .octant()
            .is_none());
    }
    #[test]
    fn test_sub_interval__fully_within() {
        assert!(PartialAngleInterval::from_degrees(0.0, 30.0)
            .other_is_sub_interval_of_this(PartialAngleInterval::from_degrees(5.0, 25.0)));
    }
    #[test]
    fn test_sub_interval__touch_one_edge() {
        assert!(PartialAngleInterval::from_degrees(0.0, 30.0)
            .other_is_sub_interval_of_this(PartialAngleInterval::from_degrees(0.0, 25.0)));
    }
    #[test]
    fn test_sub_interval__exact_match() {
        assert!(PartialAngleInterval::from_degrees(0.0, 30.0)
            .other_is_sub_interval_of_this(PartialAngleInterval::from_degrees(0.0, 30.0)));
    }
    #[test]
    fn test_sub_interval__tricky_wraparound() {
        let arc = PartialAngleInterval::from_degrees(0.0, 30.0);
        assert_false!(arc.other_is_sub_interval_of_this(arc.complement()));
    }
}
