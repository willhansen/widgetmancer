use std::borrow::Borrow;
use std::fmt::{Debug, Display, Formatter};
use crate::utility::*;


/// Combines elements in a circular fashion
/// Order independent
/// Search term "circular arc graph" may be relevant
pub fn circular_merging<T>(
    sorted_data: impl IntoIterator<Item = T>,
    merge_function: fn(&T, &T) -> Option<T>,
) -> Vec<T> {
    let mut output = vec![];
    sorted_data.into_iter().for_each(|t| {
        if output.is_empty() {
            output.push(t);
            return;
        }
        let last_placed = output.pop().unwrap();

        if let Some(combined) = merge_function(&last_placed, &t) {
            output.push(combined);
        } else {
            output.push(last_placed);
            output.push(t);
        }
    });
    if output.len() >= 2 {
        let last = output.last().unwrap();
        let first = output.first().unwrap();
        if let Some(combined) = merge_function(&last, &first) {
            output.pop();
            output.remove(0);
            output.insert(0, combined);
        }
    }
    output
}

type I32interval = (i32, i32);

pub fn circular_merge_intervals_mod10(
    sorted_data: impl IntoIterator<Item = I32interval>,
) -> Vec<I32interval> {
    circular_merging(sorted_data, try_combine_circular_intervals_mod10)
}

pub fn circular_merge_intervals_mod10_no_overlap(
    sorted_data: impl IntoIterator<Item = I32interval>,
) -> Vec<I32interval> {
    circular_merging(sorted_data, try_combine_circular_intervals_mod10_no_overlap)
}

pub fn try_combine_circular_intervals_mod10(
    a: &I32interval,
    b: &I32interval,
) -> Option<I32interval> {
    try_combine_circular_intervals_allowing_overlap(a, b, 10)
}
pub fn try_combine_circular_intervals_mod10_no_overlap(
    a: &I32interval,
    b: &I32interval,
) -> Option<I32interval> {
    try_combine_circular_intervals(a, b, 10, false)
}
pub fn try_combine_circular_intervals(
    a: impl Borrow<I32interval>,
    b: impl Borrow<I32interval>,
    modulo: u32,
    allow_overlap: bool,
) -> Option<I32interval> {
    let a = standardize_interval(a, modulo);
    let b = standardize_interval(b, modulo);

    match intervals_are_overlapping(a, b, modulo) {
        BoolWithPartial::True => {
            if !allow_overlap {
                panic!("Disallowed overlap found:\na: {:?}\nb: {:?}", a, b);
            }
        }
        BoolWithPartial::Partial => (),
        BoolWithPartial::False => return None,
    }

    if interval_is_full(a) || interval_is_full(b) || intervals_sum_to_full(a, b) {
        return Some(full_interval());
    }

    // Feel like this should be a matrix somehow
    let a0_in_b = in_or_touching_looping_interval(a.0, b, modulo);
    let a1_in_b = in_or_touching_looping_interval(a.1, b, modulo);

    let start = if a0_in_b { b.0 } else { a.0 };
    let end = if a1_in_b { b.1 } else { a.1 };

    Some((start, end))
}

pub fn try_combine_circular_intervals_allowing_overlap(
    a: impl Borrow<I32interval>,
    b: impl Borrow<I32interval>,
    modulo: u32,
) -> Option<I32interval> {
    try_combine_circular_intervals(a, b, modulo, true)
}
pub fn try_combine_circular_intervals_no_overlap(
    a: impl Borrow<I32interval>,
    b: impl Borrow<I32interval>,
    modulo: u32,
) -> Option<I32interval> {
    try_combine_circular_intervals(a, b, modulo, false)
}

fn full_interval() -> I32interval {
    (0, 0)
}

fn interval_is_full(x: I32interval) -> bool {
    x.0 == x.1
}

fn intervals_sum_to_full(a: I32interval, b: I32interval) -> bool {
    a.1 == b.0 && b.1 == a.0
}

fn intervals_are_overlapping(a: I32interval, b: I32interval, modulo: u32) -> BoolWithPartial {
    in_looping_interval(a.0, b, modulo)
        .or(in_looping_interval(a.1, b, modulo))
        .or(in_looping_interval(b.0, a, modulo))
        .or(in_looping_interval(b.1, a, modulo))
}

fn in_or_touching_looping_interval(val: i32, interval: I32interval, modulo: u32) -> bool {
    let val = val.rem_euclid(modulo as i32);
    let interval = standardize_interval(interval, modulo);
    position_relative_to_circular_interval(val, interval, modulo).in_closed_interval()
}
fn in_looping_interval(val: i32, interval: I32interval, modulo: u32) -> BoolWithPartial {
    let val = val.rem_euclid(modulo as i32);
    let interval = standardize_interval(interval, modulo);
    position_relative_to_circular_interval(val, interval, modulo).in_interval()
}
fn position_relative_to_circular_interval(
    val: i32,
    interval: I32interval,
    modulo: u32,
) -> RelativeIntervalLocation {
    let interval = standardize_interval(interval, modulo);
    let val = val.rem_euclid(modulo as i32);

    if interval_is_full(interval) {
        RelativeIntervalLocation::Inside
    } else if val == interval.0 {
        RelativeIntervalLocation::Start
    } else if val == interval.1 {
        RelativeIntervalLocation::End
    } else {
        let is_inside = if interval_wraps_around(interval, modulo) {
            // is loop around
            val < interval.1 || interval.0 < val
        } else {
            // no loop around
            interval.0 < val && val < interval.1
        };
        if is_inside {
            RelativeIntervalLocation::Inside
        } else {
            // TODO: make a version of the enum more compatible with periodicity
            RelativeIntervalLocation::Before
        }
    }
}

fn interval_wraps_around(interval: I32interval, modulo: u32) -> bool {
    let interval = standardize_interval(interval, modulo);

    interval.0 >= interval.1
}

fn standardize_interval(interval: impl Borrow<I32interval>, modulo: u32) -> I32interval {
    let interval = (
        interval.borrow().0.rem_euclid(modulo as i32),
        interval.borrow().1.rem_euclid(modulo as i32),
    );
    if interval_is_full(interval) {
        full_interval()
    } else {
        interval
    }
}

#[cfg(test)]
mod tests {

    use super::*;

    #[test]
    fn test_combine_circular_intervals__simple_touching() {
        assert_eq!(
            try_combine_circular_intervals_allowing_overlap((0, 1), (1, 2), 10),
            Some((0, 2)),
        );
    }
    #[test]
    fn test_combine_circular_intervals__simple_overlap() {
        for i in 10..20 {
            assert_eq!(
                try_combine_circular_intervals_allowing_overlap((0, 3), (1, 7), i),
                Some((0, 7)),
            );
        }
    }
    #[test]
    fn test_combine_circular_intervals__duplicate_input() {
        assert_eq!(
            try_combine_circular_intervals_allowing_overlap((2, 5), (2, 5), 10),
            Some((2, 5)),
        );
    }
    #[test]
    fn test_combine_circular_intervals__full_containment() {
        assert_eq!(
            try_combine_circular_intervals_allowing_overlap((2, 5), (1, 7), 10),
            Some((1, 7)),
        );
    }
    #[test]
    fn test_combine_circular_intervals__full_containment_crossing_end() {
        assert_eq!(
            try_combine_circular_intervals_allowing_overlap((6, 8), (5, 2), 10),
            Some((5, 2)),
        );
    }
    #[test]
    fn test_combine_circular_intervals__not_touching() {
        assert_eq!(
            try_combine_circular_intervals_allowing_overlap((1, 3), (4, 7), 10),
            None,
        );
    }
    #[test]
    fn test_combine_circular_intervals__combine_while_one_crosses_end() {
        assert_eq!(
            try_combine_circular_intervals_allowing_overlap((6, 8), (8, 2), 10),
            Some((6, 2)),
        );
    }
    #[test]
    fn test_combine_circular_intervals__two_half_intervals_combine_to_full() {
        assert_eq!(
            try_combine_circular_intervals_allowing_overlap((0, 5), (5, 0), 10),
            Some((0, 0)),
        );
    }
    #[test]
    fn test_combine_circular_intervals__two_half_intervals_combine_to_full__and_modulo() {
        assert_eq!(
            try_combine_circular_intervals_allowing_overlap((1, 6), (506, 9001), 10),
            Some((0, 0)),
        );
    }
    #[test]
    fn test_combine_circular_intervals__two_full_intervals() {
        assert_eq!(
            try_combine_circular_intervals_allowing_overlap((0, 0), (8, 8), 10),
            Some((0, 0)),
        );
    }
    #[test]
    fn test_combine_circular_intervals__full_intervals_have_canonical_representation() {
        assert_eq!(
            try_combine_circular_intervals_allowing_overlap((5, 5), (8, 8), 10),
            Some((0, 0)),
        );
    }
    #[test]
    fn test_combine_circular_intervals__inputs_are_standardized() {
        assert_eq!(
            try_combine_circular_intervals_allowing_overlap((5, 16), (26, -2), 10),
            Some((5, 8)),
        );
    }
    #[test]
    fn test_combine_circular_intervals__other_interval_lengths() {
        assert_eq!(
            try_combine_circular_intervals_allowing_overlap((5, 6), (6, 8), 5),
            Some((0, 3)),
        );
        assert_eq!(
            try_combine_circular_intervals_allowing_overlap((5, 6), (6, 8), 6),
            Some((5, 2)),
        );
        assert_eq!(
            try_combine_circular_intervals_allowing_overlap((5, 6), (6, 8), 7),
            Some((5, 1)),
        );
        assert_eq!(
            try_combine_circular_intervals_allowing_overlap((5, 6), (6, 8), 8),
            Some((5, 0)),
        );
        assert_eq!(
            try_combine_circular_intervals_allowing_overlap((5, 6), (6, 8), 9000),
            Some((5, 8)),
        );
    }

    #[test]
    fn test_circular_merging__simple_case() {
        let data = vec![(0, 1), (2, 3), (3, 5), (6, 0)];
        let correct_output = vec![(6, 1), (2, 5)];
        let output = circular_merge_intervals_mod10(data);
        assert_eq!(output, correct_output);
    }
    #[test]
    fn test_circular_merging__no_merges() {
        let data = vec![(0, 1), (2, 3)];
        let correct_output = vec![(0, 1), (2, 3)];
        let output = circular_merge_intervals_mod10(data);
        assert_eq!(output, correct_output);
    }
    #[test]
    fn test_circular_merging__merge_to_full_circle() {
        let data = vec![(0, 2), (2, 3), (3, 0)];
        let correct_output = vec![full_interval()];
        let output = circular_merge_intervals_mod10(data);
        assert_eq!(output, correct_output);
    }
    #[test]
    #[should_panic]
    fn test_circular_merging__disallowing_overlaps() {
        let data = vec![(0, 2), (1, 3)];
        circular_merge_intervals_mod10_no_overlap(data);
    }
}
