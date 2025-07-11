use std::collections::HashSet;
use std::f32::consts::{PI, TAU};
use std::fmt::{Display, Formatter};
use std::ops::{Add, Sub};

use euclid::{default, vec2, Angle};
use getset::CopyGetters;
use itertools::Itertools;
use ntest::assert_false;
use num::traits::FloatConst;
use ordered_float::OrderedFloat;

use crate::coordinate_frame_conversions::{WorldMove, WorldStep};
use crate::{
    abs_angle_distance, better_angle_from_x_axis, rotated_n_quarter_turns_counter_clockwise,
    standardize_angle, Octant, OrthogonalWorldStep, QuarterTurnsAnticlockwise, ORTHOGONAL_STEPS,
    STEP_DOWN_LEFT, STEP_DOWN_RIGHT, STEP_UP_LEFT, STEP_UP_RIGHT,
};

#[derive(Default, Debug, Copy, Clone, PartialEq, CopyGetters)]
#[get_copy = "pub"]
pub struct AngleInterval {
    clockwise_end: Angle<f32>,
    anticlockwise_end: Angle<f32>,
}

impl AngleInterval {
    pub fn new(clockwise_end: Angle<f32>, anticlockwise_end: Angle<f32>) -> Self {
        AngleInterval {
            clockwise_end: standardize_angle(clockwise_end),
            anticlockwise_end: standardize_angle(anticlockwise_end),
        }
    }
    pub fn from_degrees(clockwise_end_in_degrees: f32, anticlockwise_end_in_degrees: f32) -> Self {
        Self::new(
            Angle::degrees(clockwise_end_in_degrees),
            Angle::degrees(anticlockwise_end_in_degrees),
        )
    }
    pub fn from_radians(clockwise_end_in_radians: f32, anticlockwise_end_in_radians: f32) -> Self {
        Self::new(
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

    pub fn from_square(relative_square: WorldStep) -> Self {
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

        AngleInterval {
            anticlockwise_end: *least_clockwise,
            clockwise_end: *most_clockwise,
        }
    }
    pub fn from_square_face(
        relative_square: WorldStep,
        face_direction: OrthogonalWorldStep,
    ) -> Self {
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
            AngleInterval::new(face_corner_angles[0], face_corner_angles[1])
        } else {
            AngleInterval::new(face_corner_angles[1], face_corner_angles[0])
        }
    }

    pub fn intersection(&self, other: Self) -> Self {
        assert!(self.overlapping_but_not_exactly_touching(other));
        assert_false!(self.has_wraparound_double_overlap_not_counting_touching(other));

        AngleInterval {
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
        let result = AngleInterval {
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
        AngleInterval::new(self.anticlockwise_end, self.clockwise_end)
    }

    pub fn at_least_fully_overlaps(&self, other: AngleInterval) -> bool {
        self.num_contained_or_touching_edges(other) == 2 && self.width() >= other.width()
    }

    fn partially_or_fully_overlaps_without_exactly_touching(&self, other: AngleInterval) -> bool {
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

    pub fn touches_or_overlaps(&self, other: AngleInterval) -> bool {
        self.num_contained_or_touching_edges(other) > 0
            || other.num_contained_or_touching_edges(*self) > 0
    }

    fn num_contained_or_touching_edges(&self, other: AngleInterval) -> u32 {
        let mut sum = 0;
        if self.contains_or_touches_angle(other.anticlockwise_end) {
            sum += 1;
        }
        if self.contains_or_touches_angle(other.clockwise_end) {
            sum += 1;
        }
        sum
    }
    fn num_contained_not_touching_edges(&self, other: AngleInterval) -> u32 {
        let mut sum = 0;
        if self.contains_angle_not_including_edges(other.anticlockwise_end) {
            sum += 1;
        }
        if self.contains_angle_not_including_edges(other.clockwise_end) {
            sum += 1;
        }
        sum
    }
    fn has_wraparound_double_overlap_not_counting_touching(&self, other: AngleInterval) -> bool {
        self.num_contained_not_touching_edges(other) == 2
            && other.num_contained_not_touching_edges(*self) == 2
    }
    pub fn partially_overlaps_other_while_including_edges(&self, other: AngleInterval) -> bool {
        let contained_in_self = self.num_contained_or_touching_edges(other);
        let contained_in_other = other.num_contained_or_touching_edges(*self);
        contained_in_self >= 1 && contained_in_other >= 1
    }
    pub fn subtract(&self, other: AngleInterval) -> Vec<AngleInterval> {
        if !self.touches_or_overlaps(other) {
            return vec![self.clone()];
        }

        let mut split_results = vec![];
        if self.contains_or_touches_angle(other.clockwise_end)
            && self.clockwise_end != other.clockwise_end
        {
            let below_interval = AngleInterval::new(self.clockwise_end, other.clockwise_end);
            split_results.push(below_interval);
        }
        if self.contains_or_touches_angle(other.anticlockwise_end)
            && self.anticlockwise_end != other.anticlockwise_end
        {
            let above_interval =
                AngleInterval::new(other.anticlockwise_end, self.anticlockwise_end);
            split_results.push(above_interval);
        }
        split_results
    }
    pub fn edge_of_this_overlapped_by(&self, other: AngleInterval) -> DirectionalAngularEdge {
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
    pub fn edge_of_this_deeper_in(&self, other: AngleInterval) -> DirectionalAngularEdge {
        assert!(other.fully_contains_interval(*self));
        let clockwise_dist = AngleInterval::new(other.clockwise_end, self.clockwise_end).width();
        let anticlockwise_dist =
            AngleInterval::new(self.anticlockwise_end, other.anticlockwise_end).width();
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
    fn exactly_touches_arc(&self, other: AngleInterval) -> bool {
        let edges_touch = self.clockwise_end == other.anticlockwise_end
            || other.clockwise_end == self.anticlockwise_end;

        let contains_other_edge = self.contains_angle_not_including_edges(other.clockwise_end)
            || self.contains_angle_not_including_edges(other.anticlockwise_end);

        edges_touch && !contains_other_edge
    }
    pub fn overlaps_or_touches(&self, other: AngleInterval) -> bool {
        self.partially_or_fully_overlaps_without_exactly_touching(other)
            || self.exactly_touches_arc(other)
    }
    pub fn overlapping_but_not_exactly_touching(&self, other: AngleInterval) -> bool {
        self.partially_or_fully_overlaps_without_exactly_touching(other)
            && !self.exactly_touches_arc(other)
    }
    pub fn overlaps_other_by_at_least_this_much(
        &self,
        other: AngleInterval,
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
    fn contains_or_touches_angle(&self, angle: Angle<f32>) -> bool {
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

    pub fn fully_contains_interval(&self, other: AngleInterval) -> bool {
        let contains_other_edges = self.contains_angle_not_including_edges(other.anticlockwise_end)
            && self.contains_angle_not_including_edges(other.clockwise_end);
        let other_does_not_contain_these_edges = !other
            .contains_angle_not_including_edges(self.clockwise_end)
            && !other.contains_angle_not_including_edges(self.anticlockwise_end);

        contains_other_edges && other_does_not_contain_these_edges
    }
    pub fn most_overlapped_edge_of_self(&self, other: AngleInterval) -> DirectionalAngularEdge {
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
    pub fn rotated_quarter_turns(&self, quarter_turns: QuarterTurnsAnticlockwise) -> Self {
        AngleInterval {
            clockwise_end: quarter_turns.rotate_angle(self.clockwise_end),
            anticlockwise_end: quarter_turns.rotate_angle(self.anticlockwise_end),
        }
    }
    pub fn rotated(&self, d_angle: Angle<f32>) -> Self {
        AngleInterval::new(
            self.clockwise_end + d_angle,
            self.anticlockwise_end + d_angle,
        )
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
    intervals: Vec<AngleInterval>,
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

    pub fn add_interval(&mut self, interval: AngleInterval) {
        self.intervals.push(interval);
        self.standardize();
    }
    pub fn fully_contains_interval(&self, interval: AngleInterval) -> bool {
        self.intervals
            .iter()
            .any(|i| i.fully_contains_interval(interval))
    }
    pub fn partially_or_fully_overlaps_interval(&self, interval: AngleInterval) -> bool {
        self.intervals.iter().any(|i: &AngleInterval| {
            i.partially_or_fully_overlaps_without_exactly_touching(interval)
        })
    }
    pub fn partially_overlaps_interval(&self, interval: AngleInterval) -> bool {
        self.most_overlapped_edge_of_set(interval).is_some()
    }
    pub fn most_overlapped_edge_of_set(
        &self,
        interval: AngleInterval,
    ) -> Option<DirectionalAngularEdge> {
        // TODO: don't just get the first one
        self.intervals
            .iter()
            .filter_map(|&arc_from_set: &AngleInterval| {
                if arc_from_set.partially_overlaps_other_while_including_edges(interval) {
                    Some(arc_from_set.edge_of_this_overlapped_by(interval))
                } else if interval.fully_contains_interval(arc_from_set) {
                    Some(arc_from_set.edge_of_this_deeper_in(interval))
                } else {
                    None
                }
            })
            .next()
    }
}

impl Display for AngleInterval {
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

#[cfg(test)]
mod tests {
    use ntest::{assert_about_eq, assert_false};
    use num::zero;
    use pretty_assertions::{assert_eq, assert_ne};

    use crate::{STEP_DOWN, STEP_RIGHT, STEP_UP};

    use super::*;

    #[test]
    fn test_interval_overlap() {
        let interval_b = AngleInterval::from_degrees(5.0, 15.0);
        let interval_a = AngleInterval::from_degrees(0.0, 10.0);

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
        let arc_a = AngleInterval::from_degrees(0.0, 20.0);
        let arc_b = AngleInterval::from_degrees(-25.0, 5.0);

        assert!(arc_a.partially_overlaps_other_while_including_edges(arc_b));
        assert!(arc_b.partially_overlaps_other_while_including_edges(arc_a));
    }

    #[test]
    fn test_interval_overlap_does_not_include_full_overlap() {
        let interval_b = AngleInterval::from_degrees(5.0, 15.0);
        let interval_a = AngleInterval::from_degrees(7.0, 10.0);
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
        let wrapping_angle = AngleInterval::from_degrees(45.0, 0.0);
        let zero_center_angle = AngleInterval::from_degrees(-45.0, 45.0);
        assert!(wrapping_angle.partially_overlaps_other_while_including_edges(zero_center_angle));
    }

    #[test]
    fn num_contained_edges_is_symmetric__weird_wrapping_case() {
        let arc_a = AngleInterval::from_degrees(45.0, 0.0);
        let arc_b = AngleInterval::from_degrees(-45.0, 45.0);
        assert_eq!(
            arc_a.num_contained_or_touching_edges(arc_b),
            arc_b.num_contained_or_touching_edges(arc_a)
        );
    }

    #[test]
    fn num_contained_edges_is_symmetric__regular_case() {
        let arc_a = AngleInterval::from_degrees(0.0, 20.0);
        let arc_b = AngleInterval::from_degrees(10.0, 30.0);
        assert_eq!(
            arc_a.num_contained_or_touching_edges(arc_b),
            arc_b.num_contained_or_touching_edges(arc_a)
        );
    }

    #[test]
    fn test_interval_overlap_edges() {
        let interval_a = AngleInterval::from_degrees(0.0, 10.0);
        let interval_c = AngleInterval::from_degrees(10.0, 50.0);
        assert!(
            !interval_a.partially_or_fully_overlaps_without_exactly_touching(interval_c),
            "touching edges should not count as overlap"
        );
    }

    #[test]
    fn test_interval_contains_angle() {
        assert!(
            AngleInterval::from_degrees(0.0, 10.0).contains_or_touches_angle(Angle::degrees(5.0)),
            "simple case"
        );
        assert!(
            !AngleInterval::from_degrees(0.0, 10.0).contains_or_touches_angle(Angle::degrees(15.0)),
            "simple outside bounds case"
        );
        assert!(
            AngleInterval::from_degrees(0.0, 10.0).contains_or_touches_angle(Angle::degrees(10.0)),
            "On left bound should be inside"
        );
        assert!(
            AngleInterval::from_degrees(0.0, 10.0).contains_or_touches_angle(Angle::degrees(0.0)),
            "On right bound should ALSO be inside"
        );
        assert!(
            !AngleInterval::from_degrees(10.0, 0.0).contains_or_touches_angle(Angle::degrees(5.0)),
            "outside a large arc"
        );
        assert!(
            AngleInterval::from_degrees(10.0, 0.0).contains_or_touches_angle(Angle::degrees(180.0)),
            "inside a large arc"
        );
        assert!(
            !AngleInterval::from_degrees(0.0, 0.0).contains_or_touches_angle(Angle::degrees(180.0)),
            "directly across from zero width interval"
        );
    }

    #[test]
    fn test_width_of_zero_width_arc() {
        assert_eq!(
            AngleInterval::from_degrees(0.0, 0.0).width().to_degrees(),
            0.0
        );
    }

    #[test]
    fn test_angle_interval_union() {
        assert_eq!(
            AngleInterval::from_degrees(80.0, 100.0).union(AngleInterval::from_degrees(40.0, 90.0)),
            AngleInterval::from_degrees(40.0, 100.0),
            "from overlap"
        );

        assert_eq!(
            AngleInterval::from_degrees(80.0, 100.0).union(AngleInterval::from_degrees(40.0, 80.0)),
            AngleInterval::from_degrees(40.0, 100.0),
            "from exactly touching"
        );
    }

    #[test]
    fn test_angle_interval_intersection__simple_overlap() {
        assert_eq!(
            AngleInterval::from_degrees(80.0, 100.0)
                .intersection(AngleInterval::from_degrees(40.0, 90.0)),
            AngleInterval::from_degrees(80.0, 90.0),
            "from overlap"
        );
    }

    #[test]
    #[should_panic]
    fn test_angle_interval_intersection__no_overlap() {
        AngleInterval::from_degrees(95.0, 100.0)
            .intersection(AngleInterval::from_degrees(40.0, 90.0));
    }

    #[test]
    fn test_angle_interval_intersection__full_overlap() {
        let small = AngleInterval::from_degrees(80.0, 100.0);
        let big = AngleInterval::from_degrees(60.0, 120.0);
        assert_eq!(big.intersection(small), small);
        assert_eq!(small.intersection(big), small);
    }

    #[test]
    #[should_panic]
    fn test_angle_interval_intersection__wraparound_double_overlap() {
        let small = AngleInterval::from_degrees(80.0, 100.0);
        let big = AngleInterval::from_degrees(60.0, 120.0);
        big.intersection(small.complement());
    }

    #[test]
    fn test_angle_interval_set__standardization() {
        let mut angle_interval_set = AngleIntervalSet {
            intervals: vec![
                AngleInterval::from_degrees(10.0, 15.0),
                AngleInterval::from_degrees(15.0, 30.0),
            ],
        };
        assert!(!angle_interval_set.is_valid());
        angle_interval_set.standardize();
        assert!(angle_interval_set.is_valid());
        assert_eq!(
            angle_interval_set,
            AngleIntervalSet {
                intervals: vec![AngleInterval::from_degrees(10.0, 30.0)]
            }
        );
    }

    #[test]
    fn test_angle_interval_set__standardize_with_one() {
        let interval = AngleInterval::from_degrees(15.0, 30.0);
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

        let interval_a = AngleInterval::from_degrees(30.0, 45.0);
        let interval_b = AngleInterval::from_degrees(10.0, 35.0);
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
            AngleInterval::from_degrees(-10.0, 10.0)
                .fully_contains_interval(AngleInterval::from_degrees(-5.0, 5.0)),
            "simple positive"
        );

        assert!(
            !AngleInterval::from_degrees(-10.0, 10.0)
                .fully_contains_interval(AngleInterval::from_degrees(5.0, -5.0)),
            "contains endpoints, but not the middle"
        );

        assert!(
            AngleInterval::from_degrees(5.0, -5.0)
                .fully_contains_interval(AngleInterval::from_degrees(10.0, -10.0)),
            "big angle fully contained"
        );
    }

    #[test]
    fn test_interval_fully_contain_other_interval__edge_cases() {
        let z = 70.374;
        let a = 50.342;
        let aa = 30.342;
        let ab = 20.342;
        let b = 3.14567;
        let c = 1.2345;
        let base_interval = AngleInterval::from_degrees(b, a);
        let touching_below = AngleInterval::from_degrees(c, b);
        let touching_above = AngleInterval::from_degrees(a, z);
        let overlapping_below = AngleInterval::from_degrees(c, aa);
        let overlapping_above = AngleInterval::from_degrees(aa, z);
        let inside_touching_start = AngleInterval::from_degrees(aa, a);
        let inside_touching_end = AngleInterval::from_degrees(b, aa);
        let edges_in_but_wraparound = AngleInterval::from_degrees(aa, ab);
        let complementary = AngleInterval::from_degrees(a, b);
        let complementary_but_overlapping_top = AngleInterval::from_degrees(aa, b);
        let complementary_but_overlapping_bottom = AngleInterval::from_degrees(a, ab);

        assert!(!base_interval.fully_contains_interval(base_interval)); // controversial
        assert!(!base_interval.fully_contains_interval(touching_below));
        assert!(!base_interval.fully_contains_interval(touching_above));
        assert!(!base_interval.fully_contains_interval(overlapping_below));
        assert!(!base_interval.fully_contains_interval(overlapping_above));
        assert!(!base_interval.fully_contains_interval(inside_touching_start));
        assert!(!base_interval.fully_contains_interval(inside_touching_end));
        assert_eq!(
            base_interval.fully_contains_interval(inside_touching_start),
            base_interval.fully_contains_interval(inside_touching_end)
        );
        assert!(!base_interval.fully_contains_interval(edges_in_but_wraparound));
        assert!(!base_interval.fully_contains_interval(complementary));
        assert!(!base_interval.fully_contains_interval(complementary_but_overlapping_top));
        assert!(!base_interval.fully_contains_interval(complementary_but_overlapping_bottom));
    }

    #[test]
    fn test_angle_interval_set__standardize_but_no_change_required() {
        let mut the_set = AngleIntervalSet {
            intervals: vec![
                AngleInterval::from_degrees(30.0, 40.0),
                AngleInterval::from_degrees(50.0, 60.0),
                AngleInterval::from_degrees(70.0, 80.0),
            ],
        };
        let before_adding = the_set.clone();
        the_set.add_interval(AngleInterval::from_degrees(33.0, 35.0));
        assert_eq!(before_adding, the_set);
    }

    #[test]
    fn test_angle_interval_set_validity() {
        assert!(AngleIntervalSet {
            intervals: vec![AngleInterval::from_degrees(10.0, 20.0)]
        }
        .is_valid());

        assert!(AngleIntervalSet {
            intervals: vec![
                AngleInterval::from_degrees(10.0, 20.0),
                AngleInterval::from_degrees(30.0, 50.0),
            ]
        }
        .is_valid());
        assert!(!AngleIntervalSet {
            intervals: vec![
                AngleInterval::from_degrees(10.0, 20.0),
                AngleInterval::from_degrees(20.0, 50.0),
            ]
        }
        .is_valid());
    }

    #[test]
    fn test_get_angle_endpoint_of_overlapped_region() {
        let interval_set = AngleIntervalSet {
            intervals: vec![AngleInterval::from_degrees(0.0, 20.0)],
        };
        let single_interval = AngleInterval::from_degrees(-25.0, 5.0);

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
                AngleInterval::from_degrees(0.0, 20.0),
                AngleInterval::from_degrees(22.0, 50.0),
            ],
        };
        let single_interval = AngleInterval::from_degrees(10.0, 25.0);

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
            AngleInterval::from_degrees(10.0, 20.0)
                .center_angle()
                .to_degrees(),
            15.0
        );
        assert_about_eq!(
            AngleInterval::from_degrees(-10.0, 10.0)
                .center_angle()
                .to_degrees(),
            0.0
        );
        assert_about_eq!(
            AngleInterval::from_degrees(10.0, -10.0)
                .center_angle()
                .to_degrees(),
            180.0
        );
    }

    #[test]
    fn test_angle_interval_width() {
        assert_about_eq!(
            AngleInterval::from_degrees(10.0, 20.0).width().to_degrees(),
            10.0
        );
        assert_about_eq!(
            AngleInterval::from_degrees(-10.0, 10.0)
                .width()
                .to_degrees(),
            20.0
        );
        assert_about_eq!(
            AngleInterval::from_degrees(10.0, -10.0)
                .width()
                .to_degrees(),
            340.0,
            1e-4
        );
    }

    #[test]
    fn test_angle_interval_from_square() {
        let arc = AngleInterval::from_square(vec2(1, 0));
        assert_about_eq!(arc.clockwise_end.to_degrees(), -45.0);
        assert_about_eq!(arc.anticlockwise_end.to_degrees(), 45.0);

        let arc = AngleInterval::from_square(vec2(0, 1));
        assert_about_eq!(arc.clockwise_end.to_degrees(), 45.0);
        assert_about_eq!(arc.anticlockwise_end.to_degrees(), 135.0);

        let arc = AngleInterval::from_square(vec2(-1, 0));
        assert_about_eq!(arc.clockwise_end.to_degrees(), 135.0);
        assert_about_eq!(arc.anticlockwise_end.to_degrees(), -135.0);

        let arc = AngleInterval::from_square(vec2(0, -1));
        assert_about_eq!(arc.clockwise_end.to_degrees(), -135.0);
        assert_about_eq!(arc.anticlockwise_end.to_degrees(), -45.0);
    }

    #[test]
    fn test_split_interval_around_interval() {
        let new_arcs = assert_eq!(
            AngleInterval::from_degrees(0.0, 30.0)
                .subtract(AngleInterval::from_degrees(10.0, 20.0)),
            vec![
                AngleInterval::from_degrees(0.0, 10.0),
                AngleInterval::from_degrees(20.0, 30.0),
            ]
        );
    }

    #[test]
    fn test_interval_subtraction__touching_from_inside() {
        let new_arcs = assert_eq!(
            AngleInterval::from_degrees(0.0, 30.0)
                .subtract(AngleInterval::from_degrees(-10.0, 30.0)),
            vec![]
        );
    }

    #[test]
    fn test_partial_overlap_includes_almost_peaking_out() {
        assert!(AngleInterval::from_degrees(10.0, 20.0)
            .partially_overlaps_other_while_including_edges(AngleInterval::from_degrees(
                10.0, 15.0,
            )));
        assert!(AngleInterval::from_degrees(10.0, 20.0)
            .partially_overlaps_other_while_including_edges(AngleInterval::from_degrees(
                15.0, 20.0,
            )));
        assert!(AngleInterval::from_degrees(10.0, 20.0)
            .partially_overlaps_other_while_including_edges(AngleInterval::from_degrees(
                10.0, 20.0,
            )));
    }

    #[test]
    fn test_partial_overlap_includes_almost_peaking_out__wraparound_case() {
        assert!(AngleInterval::from_degrees(315.0, 270.0)
            .partially_overlaps_other_while_including_edges(AngleInterval::from_degrees(
                225.0, 315.0,
            )));
    }

    #[test]
    fn test_angle_wraparound_invariance() {
        assert!(AngleInterval::from_degrees(315.0, 270.0)
            .partially_overlaps_other_while_including_edges(AngleInterval::from_degrees(
                280.0, 315.0,
            )));

        assert!(AngleInterval::from_degrees(315.0, 270.0)
            .partially_overlaps_other_while_including_edges(AngleInterval::from_degrees(
                280.0 - 360.0,
                315.0 - 360.0,
            )));
    }

    #[test]
    fn test_most_overlapped_edge_of_arc() {
        assert_eq!(
            AngleInterval::from_degrees(135.0, 90.0)
                .most_overlapped_edge_of_self(AngleInterval::from_degrees(45.0, 135.0)),
            DirectionalAngularEdge::new(Angle::degrees(90.0), false)
        );
    }

    #[test]
    fn test_arc_from_square_face_is_smallish() {
        ORTHOGONAL_STEPS.into_iter().for_each(|step| {
            assert!(
                AngleInterval::from_square_face(vec2(3, 6), step.into())
                    .width()
                    .to_degrees()
                    < 45.0
            )
        });
    }

    #[test]
    fn test_arc_from_square_face__observed_failure_at_right_face_of_one_block_right() {
        assert_about_eq!(
            AngleInterval::from_square_face(STEP_RIGHT, STEP_RIGHT.into())
                .anticlockwise_end
                .to_degrees(),
            AngleInterval::from_square(STEP_RIGHT * 2)
                .anticlockwise_end
                .to_degrees()
        );
    }

    #[test]
    fn test_arc_at_least_fully_overlap() {
        let cw = Angle::degrees(5.0);
        let ccw = Angle::degrees(25.0);
        let d = Angle::degrees(1.0);

        let arc = AngleInterval::new(cw, ccw);
        let arc_extend_cw = AngleInterval::new(cw - d, ccw);
        let arc_retract_cw = AngleInterval::new(cw + d, ccw);
        let arc_extend_ccw = AngleInterval::new(cw, ccw + d);
        let arc_retract_ccw = AngleInterval::new(cw, ccw - d);
        let arc_extend_both = AngleInterval::new(cw - d, ccw + d);
        let arc_retract_both = AngleInterval::new(cw + d, ccw - d);

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

        let arc = AngleInterval::new(cw, ccw);
        let arc_extend_cw = AngleInterval::new(cw - d, ccw);
        let arc_retract_cw = AngleInterval::new(cw + d, ccw);
        let arc_extend_ccw = AngleInterval::new(cw, ccw + d);
        let arc_retract_ccw = AngleInterval::new(cw, ccw - d);
        let arc_extend_both = AngleInterval::new(cw - d, ccw + d);
        let arc_retract_both = AngleInterval::new(cw + d, ccw - d);

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
                    AngleInterval::from_octant(Octant::new(octant))
                        .clockwise_end
                        .radians,
                    start
                );
                assert_about_eq!(
                    AngleInterval::from_octant(Octant::new(octant))
                        .anticlockwise_end
                        .radians,
                    end
                );
            })
    }

    #[test]
    fn test_overlap_at_least_this_much() {
        let arc1 = AngleInterval::from_degrees(0.0, 90.0);
        let arc2 = AngleInterval::from_degrees(90.0, 180.0);

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
}
