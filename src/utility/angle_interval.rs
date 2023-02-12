use std::collections::HashSet;
use std::fmt::{Display, Formatter};
use std::ops::{Add, Sub};

use euclid::Angle;
use itertools::Itertools;
use ordered_float::OrderedFloat;
use termion::cursor::Left;

use crate::fov_stuff::PartialVisibilityOfASquare;

#[derive(Default, Debug, Copy, Clone, PartialEq)]
pub struct AngleInterval {
    pub anticlockwise_end: Angle<f32>,
    pub clockwise_end: Angle<f32>,
}

impl AngleInterval {
    fn new(clockwise_end: Angle<f32>, anticlockwise_end: Angle<f32>) -> Self {
        AngleInterval {
            anticlockwise_end,
            clockwise_end,
        }
    }
    pub fn from_degrees(clockwise_end_in_degrees: f32, anticlockwise_end_in_degrees: f32) -> Self {
        Self::new(
            Angle::degrees(clockwise_end_in_degrees),
            Angle::degrees(anticlockwise_end_in_degrees),
        )
    }
    pub fn from_octant(octant_number: i32) -> Self {
        let reduced_octant = octant_number.rem_euclid(8);
        let low_degrees = 45 * reduced_octant;
        let high_degrees = low_degrees + 45;
        Self::from_degrees(low_degrees as f32, high_degrees as f32)
    }
    fn union(&self, other: AngleInterval) -> Self {
        assert!(self.partially_or_fully_overlaps(other) || self.exactly_touches(other));
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

    fn partially_or_fully_overlaps(&self, other: AngleInterval) -> bool {
        self.contains_angle(other.anticlockwise_end)
            || (self.contains_angle(other.clockwise_end)
                && self.anticlockwise_end != other.clockwise_end)
            || other.contains_angle(self.anticlockwise_end)
            || (other.contains_angle(self.clockwise_end)
                && other.anticlockwise_end != self.clockwise_end)
    }
    fn num_contained_edges(&self, other: AngleInterval) -> u32 {
        let mut sum = 0;
        if self.contains_angle(other.anticlockwise_end) {
            sum += 1;
        }
        if self.contains_angle(other.clockwise_end) {
            sum += 1;
        }
        sum
    }
    pub fn partially_overlaps(&self, other: AngleInterval) -> bool {
        self.num_contained_edges(other) == 1 && other.num_contained_edges(*self) == 1
    }
    pub fn split_around_arc(&self, other: AngleInterval) -> Vec<AngleInterval> {
        todo!()
    }
    fn edge_of_this_overlapped_by(&self, other: AngleInterval) -> Option<DirectionalAngleEdge> {
        if !self.partially_overlaps(other) {
            return None;
        }
        let is_clockwise_end = other.contains_angle(self.clockwise_end);
        Some(DirectionalAngleEdge {
            end_angle: if is_clockwise_end {
                self.clockwise_end
            } else {
                self.anticlockwise_end
            },
            is_low_end: is_clockwise_end,
        })
    }
    fn exactly_touches(&self, other: AngleInterval) -> bool {
        self.clockwise_end == other.anticlockwise_end
            || other.clockwise_end == self.anticlockwise_end
    }
    fn overlaps_or_touches(&self, other: AngleInterval) -> bool {
        self.partially_or_fully_overlaps(other) || self.exactly_touches(other)
    }
    fn contains_angle(&self, angle: Angle<f32>) -> bool {
        // special case for zero length interval
        // TODO: full circle arc instead?
        if self.anticlockwise_end == self.clockwise_end {
            return angle == self.anticlockwise_end;
        }

        let interval_is_less_than_half_circle =
            self.anticlockwise_end.angle_to(self.clockwise_end).radians < 0.0;
        if interval_is_less_than_half_circle {
            self.anticlockwise_end.angle_to(angle).radians <= 0.0
                && self.clockwise_end.angle_to(angle).radians > 0.0
        } else {
            self.anticlockwise_end.angle_to(angle).radians <= 0.0
                || self.clockwise_end.angle_to(angle).radians > 0.0
        }
    }
    fn contains_or_touches_angle(&self, angle: Angle<f32>) -> bool {
        self.contains_angle(angle) || self.clockwise_end == angle
    }

    pub fn fully_contains_interval(&self, other: AngleInterval) -> bool {
        let contains_other_edges = self.contains_angle(other.anticlockwise_end)
            && self.contains_or_touches_angle(other.clockwise_end);
        let other_firmly_contains_any_of_these_edges = (other
            .contains_angle(self.anticlockwise_end)
            && other.anticlockwise_end != self.anticlockwise_end)
            || other.contains_angle(self.clockwise_end);
        contains_other_edges && !other_firmly_contains_any_of_these_edges
    }
}

#[derive(Default, Debug, Clone, PartialEq)]
pub struct DirectionalAngleEdge {
    pub end_angle: Angle<f32>,
    pub is_low_end: bool,
}

#[derive(Default, Debug, Clone, PartialEq)]
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
        self.intervals
            .iter()
            .any(|i: &AngleInterval| i.partially_or_fully_overlaps(interval))
    }
    pub fn partially_overlaps_interval(&self, interval: AngleInterval) -> bool {
        self.most_overlapped_edge_of_set(interval).is_some()
    }
    pub fn most_overlapped_edge_of_set(
        &self,
        interval: AngleInterval,
    ) -> Option<DirectionalAngleEdge> {
        // TODO: don't just get the first one
        self.intervals
            .iter()
            .filter_map(|set_interval: &AngleInterval| {
                set_interval.edge_of_this_overlapped_by(interval)
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
    use ntest::assert_false;
    use pretty_assertions::{assert_eq, assert_ne};

    use super::*;

    #[test]
    fn test_interval_overlap() {
        let interval_b = AngleInterval::from_degrees(5.0, 15.0);
        let interval_a = AngleInterval::from_degrees(0.0, 10.0);

        assert!(
            interval_a.partially_or_fully_overlaps(interval_a),
            "self overlap"
        );
        assert!(
            interval_b.partially_or_fully_overlaps(interval_b),
            "other self overlap"
        );
        assert!(
            interval_a.partially_or_fully_overlaps(interval_b),
            "basic overlap"
        );
        assert!(
            interval_b.partially_or_fully_overlaps(interval_a),
            "commutative"
        );
    }

    #[test]
    fn test_interval_overlap_does_not_include_full_overlap() {
        let interval_b = AngleInterval::from_degrees(5.0, 15.0);
        let interval_a = AngleInterval::from_degrees(7.0, 10.0);
        assert!(
            !interval_a.partially_overlaps(interval_b),
            "should not count full overlaps"
        );
        assert!(
            !interval_b.partially_overlaps(interval_a),
            "should not count full overlaps"
        );
    }

    #[test]
    fn test_interval_overlap_edges() {
        let interval_a = AngleInterval::from_degrees(0.0, 10.0);
        let interval_c = AngleInterval::from_degrees(10.0, 50.0);
        assert!(
            !interval_a.partially_or_fully_overlaps(interval_c),
            "touching edges should not count as overlap"
        );
    }

    #[test]
    fn test_interval_contains_angle() {
        assert!(
            AngleInterval::from_degrees(0.0, 10.0).contains_angle(Angle::degrees(5.0)),
            "simple case"
        );
        assert!(
            !AngleInterval::from_degrees(0.0, 10.0).contains_angle(Angle::degrees(15.0)),
            "simple outside bounds case"
        );
        assert!(
            AngleInterval::from_degrees(0.0, 10.0).contains_angle(Angle::degrees(10.0)),
            "On left bound should be inside"
        );
        assert!(
            !AngleInterval::from_degrees(0.0, 10.0).contains_angle(Angle::degrees(0.0)),
            "On right bound should NOT be inside"
        );
        assert!(
            !AngleInterval::from_degrees(10.0, 0.0).contains_angle(Angle::degrees(5.0)),
            "outside a large arc"
        );
        assert!(
            AngleInterval::from_degrees(10.0, 0.0).contains_angle(Angle::degrees(180.0)),
            "inside a large arc"
        );
        assert!(
            !AngleInterval::from_degrees(0.0, 0.0).contains_angle(Angle::degrees(180.0)),
            "directly across from zero width interval"
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
            Some(DirectionalAngleEdge {
                end_angle: Angle::degrees(0.0),
                is_low_end: true
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
            Some(DirectionalAngleEdge {
                end_angle: Angle::degrees(20.0),
                is_low_end: false
            })
        );
    }
}
