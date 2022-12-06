use std::ops::{Add, Sub};

use euclid::Angle;
use itertools::Itertools;
use ordered_float::OrderedFloat;
use termion::cursor::Left;

#[derive(Default, Debug, Copy, Clone, PartialEq)]
pub struct LeftClosedClockwiseAngleInterval {
    pub start_angle: Angle<f32>,
    pub end_angle: Angle<f32>,
}

impl LeftClosedClockwiseAngleInterval {
    fn new(start_angle: Angle<f32>, end_angle: Angle<f32>) -> Self {
        LeftClosedClockwiseAngleInterval {
            start_angle,
            end_angle,
        }
    }
    fn from_degrees(start_angle_deg: f32, end_angle_deg: f32) -> Self {
        Self::new(
            Angle::degrees(start_angle_deg),
            Angle::degrees(end_angle_deg),
        )
    }
    fn union(&self, other: LeftClosedClockwiseAngleInterval) -> Self {
        assert!(self.overlaps(other) || self.exactly_touches(other));
        let result = LeftClosedClockwiseAngleInterval {
            start_angle: if self.contains_or_touches_angle(other.start_angle) {
                self.start_angle
            } else {
                other.start_angle
            },
            end_angle: if self.contains_or_touches_angle(other.end_angle) {
                self.end_angle
            } else {
                other.end_angle
            },
        };
        dbg!(result);
        result
    }
    fn overlaps(&self, other: LeftClosedClockwiseAngleInterval) -> bool {
        self.contains_angle(other.start_angle)
            || (self.contains_angle(other.end_angle) && self.start_angle != other.end_angle)
            || other.contains_angle(self.start_angle)
            || (other.contains_angle(self.end_angle) && other.start_angle != self.end_angle)
    }
    fn exactly_touches(&self, other: LeftClosedClockwiseAngleInterval) -> bool {
        self.end_angle == other.start_angle || other.end_angle == self.start_angle
    }
    fn overlaps_or_touches(&self, other: LeftClosedClockwiseAngleInterval) -> bool {
        self.overlaps(other) || self.exactly_touches(other)
    }
    fn contains_angle(&self, angle: Angle<f32>) -> bool {
        // special case for zero length interval
        // TODO: full circle arc instead?
        if self.start_angle == self.end_angle {
            return angle == self.start_angle;
        }

        let interval_is_less_than_half_circle =
            self.start_angle.angle_to(self.end_angle).radians < 0.0;
        //dbg!( "---------", self.start_angle, self.end_angle, angle, self.start_angle.angle_to(angle), self.end_angle.angle_to(angle) );
        if interval_is_less_than_half_circle {
            self.start_angle.angle_to(angle).radians <= 0.0
                && self.end_angle.angle_to(angle).radians > 0.0
        } else {
            self.start_angle.angle_to(angle).radians <= 0.0
                || self.end_angle.angle_to(angle).radians > 0.0
        }
    }
    fn contains_or_touches_angle(&self, angle: Angle<f32>) -> bool {
        self.contains_angle(angle) || self.end_angle == angle
    }

    fn fully_contains_interval(&self, other: LeftClosedClockwiseAngleInterval) -> bool {
        let contains_other_edges = self.contains_angle(other.start_angle)
            && self.contains_or_touches_angle(other.end_angle);
        let other_firmly_contains_any_of_these_edges = (other.contains_angle(self.start_angle)
            && other.start_angle != self.start_angle)
            || other.contains_angle(self.end_angle);
        contains_other_edges && !other_firmly_contains_any_of_these_edges
    }
}

#[derive(Default, Debug, Clone, PartialEq)]
pub struct AngleIntervalSet {
    intervals: Vec<LeftClosedClockwiseAngleInterval>,
}

impl AngleIntervalSet {
    pub fn new() -> Self {
        AngleIntervalSet { intervals: vec![] }
    }

    fn standardize(&mut self) {
        // sort by start angle
        self.intervals
            .sort_by_key(|i| OrderedFloat(i.start_angle.radians));
        while !self.is_valid() {
            todo!()
        }
    }

    fn is_valid(&self) -> bool {
        let sorted = self.intervals.is_sorted_by_key(|i| i.start_angle);
        if !sorted {
            return false;
        }
        let has_bad_overlap = self
            .intervals
            .iter()
            .circular_tuple_windows()
            .any(|(&interval, &next_interval)| interval.overlaps_or_touches(next_interval));
        if has_bad_overlap {
            return false;
        }
        return true;
    }

    pub fn add_interval(&mut self, interval: LeftClosedClockwiseAngleInterval) {
        self.intervals.push(interval);
        self.standardize();
    }
    pub fn fully_contains_interval(&self, interval: LeftClosedClockwiseAngleInterval) -> bool {
        self.intervals
            .iter()
            .any(|i| i.fully_contains_interval(interval))
    }
    pub fn overlaps_interval(&self, interval: LeftClosedClockwiseAngleInterval) -> bool {
        self.intervals.iter().any(|i| i.overlaps(interval))
    }
}

#[cfg(test)]
mod tests {
    use ntest::assert_false;
    use pretty_assertions::{assert_eq, assert_ne};

    use super::*;

    #[test]
    fn test_interval_overlap() {
        let interval_b = LeftClosedClockwiseAngleInterval::from_degrees(15.0, 5.0);
        let interval_a = LeftClosedClockwiseAngleInterval::from_degrees(10.0, 0.0);

        assert!(interval_a.overlaps(interval_a), "self overlap");
        assert!(interval_b.overlaps(interval_b), "other self overlap");
        assert!(interval_a.overlaps(interval_b), "basic overlap");
        assert!(interval_b.overlaps(interval_a), "commutative");
    }

    #[test]
    fn test_interval_overlap_edges() {
        let interval_a = LeftClosedClockwiseAngleInterval::from_degrees(10.0, 0.0);
        let interval_c = LeftClosedClockwiseAngleInterval::from_degrees(50.0, 10.0);
        assert!(
            !interval_a.overlaps(interval_c),
            "touching edges should not count as overlap"
        );
    }

    #[test]
    fn test_interval_contains_angle() {
        assert!(
            LeftClosedClockwiseAngleInterval::from_degrees(10.0, 0.0)
                .contains_angle(Angle::degrees(5.0)),
            "simple case"
        );
        assert!(
            !LeftClosedClockwiseAngleInterval::from_degrees(10.0, 0.0)
                .contains_angle(Angle::degrees(15.0)),
            "simple outside bounds case"
        );
        assert!(
            LeftClosedClockwiseAngleInterval::from_degrees(10.0, 0.0)
                .contains_angle(Angle::degrees(10.0)),
            "On left bound should be inside"
        );
        assert!(
            !LeftClosedClockwiseAngleInterval::from_degrees(10.0, 0.0)
                .contains_angle(Angle::degrees(0.0)),
            "On right bound should NOT be inside"
        );
        assert!(
            !LeftClosedClockwiseAngleInterval::from_degrees(0.0, 10.0)
                .contains_angle(Angle::degrees(5.0)),
            "outside a large arc"
        );
        assert!(
            LeftClosedClockwiseAngleInterval::from_degrees(0.0, 10.0)
                .contains_angle(Angle::degrees(180.0)),
            "inside a large arc"
        );
        assert!(
            !LeftClosedClockwiseAngleInterval::from_degrees(0.0, 0.0)
                .contains_angle(Angle::degrees(180.0)),
            "directly across from zero width interval"
        );
    }
    #[test]
    fn test_angle_interval_union() {
        assert_eq!(
            LeftClosedClockwiseAngleInterval::from_degrees(100.0, 80.0)
                .union(LeftClosedClockwiseAngleInterval::from_degrees(90.0, 40.0)),
            LeftClosedClockwiseAngleInterval::from_degrees(100.0, 40.0),
            "from overlap"
        );

        assert_eq!(
            LeftClosedClockwiseAngleInterval::from_degrees(100.0, 80.0)
                .union(LeftClosedClockwiseAngleInterval::from_degrees(80.0, 40.0)),
            LeftClosedClockwiseAngleInterval::from_degrees(100.0, 40.0),
            "from exactly touching"
        );
    }

    #[test]
    fn test_angle_interval_set() {
        let mut angle_interval_set = AngleIntervalSet::default();
        let interval = LeftClosedClockwiseAngleInterval::from_degrees(45.0, 30.0);
        assert_false!(angle_interval_set.overlaps_interval(interval));
        angle_interval_set.add_interval(LeftClosedClockwiseAngleInterval::from_degrees(35.0, 10.0));

        assert!(angle_interval_set.overlaps_interval(interval));
        assert_false!(angle_interval_set.fully_contains_interval(interval));

        angle_interval_set.add_interval(interval);

        assert!(angle_interval_set.overlaps_interval(interval));
        assert!(angle_interval_set.fully_contains_interval(interval));
    }

    #[test]
    fn test_interval_fully_contain_other_interval() {
        assert!(
            LeftClosedClockwiseAngleInterval::from_degrees(10.0, -10.0)
                .fully_contains_interval(LeftClosedClockwiseAngleInterval::from_degrees(5.0, -5.0)),
            "simple positive"
        );

        assert!(
            !LeftClosedClockwiseAngleInterval::from_degrees(10.0, -10.0)
                .fully_contains_interval(LeftClosedClockwiseAngleInterval::from_degrees(-5.0, 5.0)),
            "contains endpoints, but not the middle"
        );

        assert!(
            LeftClosedClockwiseAngleInterval::from_degrees(-5.0, 5.0).fully_contains_interval(
                LeftClosedClockwiseAngleInterval::from_degrees(-10.0, 10.0)
            ),
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
        let base_interval = LeftClosedClockwiseAngleInterval::from_degrees(a, b);
        let touching_below = LeftClosedClockwiseAngleInterval::from_degrees(b, c);
        let touching_above = LeftClosedClockwiseAngleInterval::from_degrees(z, a);
        let overlapping_below = LeftClosedClockwiseAngleInterval::from_degrees(aa, c);
        let overlapping_above = LeftClosedClockwiseAngleInterval::from_degrees(z, aa);
        let inside_touching_start = LeftClosedClockwiseAngleInterval::from_degrees(a, aa);
        let inside_touching_end = LeftClosedClockwiseAngleInterval::from_degrees(aa, b);
        let edges_in_but_wraparound = LeftClosedClockwiseAngleInterval::from_degrees(ab, aa);
        let complementary = LeftClosedClockwiseAngleInterval::from_degrees(b, a);
        let complementary_but_overlapping_top =
            LeftClosedClockwiseAngleInterval::from_degrees(b, aa);
        let complementary_but_overlapping_bottom =
            LeftClosedClockwiseAngleInterval::from_degrees(ab, a);

        assert!(base_interval.fully_contains_interval(base_interval));
        assert!(!base_interval.fully_contains_interval(touching_below));
        assert!(!base_interval.fully_contains_interval(touching_above));
        assert!(!base_interval.fully_contains_interval(overlapping_below));
        assert!(!base_interval.fully_contains_interval(overlapping_above));
        assert!(base_interval.fully_contains_interval(inside_touching_start));
        assert!(base_interval.fully_contains_interval(inside_touching_end));
        assert!(!base_interval.fully_contains_interval(edges_in_but_wraparound));
        assert!(!base_interval.fully_contains_interval(complementary));
        assert!(!base_interval.fully_contains_interval(complementary_but_overlapping_top));
        assert!(!base_interval.fully_contains_interval(complementary_but_overlapping_bottom));
    }
}
