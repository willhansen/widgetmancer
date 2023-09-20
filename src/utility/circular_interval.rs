pub fn circular_reduction_with_failable_operator<T>(
    sorted_data: impl IntoIterator<Item = T>,
    reduction_function: fn(&T, &T) -> Option<T>,
) -> Vec<T> {
    todo!()
}

pub fn try_combine_circular_intervals(
    a: (i32, i32),
    b: (i32, i32),
    cycle_mod: u32,
) -> Option<(i32, i32)> {
    todo!()
}

fn in_or_touching_looping_interval(interval: (i32, i32), val: i32, cycle_mod: u32) -> bool {
    let val = val.rem_euclid(cycle_mod as i32);
    let interval = (
        interval.0.rem_euclid(cycle_mod as i32),
        interval.1.rem_euclid(cycle_mod as i32),
    );
    if interval.0 == interval.1 {
        // full loop
        true
    } else if interval.0 < interval.1 {
        // no loop around
        interval.0 <= val && val <= interval.1
    } else {
        // is loop around
        val <= interval.0 || interval.1 <= val
    }
}
fn position_relative_to_circular_interval(val: i32, interval: (i32, i32), modulo: u32) -> PositionInInterval

#[cfg(test)]
mod tests {

    use super::*;

    #[test]
    fn test_combine_circular_intervals__simple_touching() {
        assert_eq!(
            try_combine_circular_intervals((0, 1), (1, 2), 10),
            Some((0, 2)),
        );
    }
    #[test]
    fn test_combine_circular_intervals__simple_overlap() {
        for i in 10..20 {
            assert_eq!(
                try_combine_circular_intervals((0, 3), (1, 7), i),
                Some((0, 7)),
            );
        }
    }
    #[test]
    fn test_combine_circular_intervals__duplicate_input() {
        assert_eq!(
            try_combine_circular_intervals((2, 5), (2, 5), 10),
            Some((2, 5)),
        );
    }
    #[test]
    fn test_combine_circular_intervals__full_containment() {
        assert_eq!(
            try_combine_circular_intervals((2, 5), (1, 7), 10),
            Some((1, 7)),
        );
    }
    #[test]
    fn test_combine_circular_intervals__full_containment_crossing_end() {
        assert_eq!(
            try_combine_circular_intervals((6, 8), (5, 2), 10),
            Some((5, 2)),
        );
    }
    #[test]
    fn test_combine_circular_intervals__not_touching() {
        assert_eq!(try_combine_circular_intervals((1, 3), (4, 7), 10), None,);
    }
    #[test]
    fn test_combine_circular_intervals__combine_while_one_crosses_end() {
        assert_eq!(
            try_combine_circular_intervals((6, 8), (8, 2), 10),
            Some((6, 2)),
        );
    }
    #[test]
    fn test_combine_circular_intervals__two_half_intervals_combine_to_full() {
        assert_eq!(
            try_combine_circular_intervals((0, 5), (5, 0), 10),
            Some((0, 0)),
        );
    }
    #[test]
    fn test_combine_circular_intervals__two_full_intervals() {
        assert_eq!(
            try_combine_circular_intervals((0, 0), (8, 8), 10),
            Some((0, 0)),
        );
    }
    #[test]
    fn test_combine_circular_intervals__full_intervals_have_canonical_representation() {
        assert_eq!(
            try_combine_circular_intervals((5, 5), (8, 8), 10),
            Some((0, 0)),
        );
    }
    #[test]
    fn test_combine_circular_intervals__inputs_are_standardized() {
        assert_eq!(
            try_combine_circular_intervals((5, 16), (26, -8), 10),
            Some((5, 8)),
        );
    }
    #[test]
    fn test_combine_circular_intervals__other_interval_lengths() {
        assert_eq!(
            try_combine_circular_intervals((5, 6), (6, 8), 5),
            Some((5, 3)),
        );
        assert_eq!(
            try_combine_circular_intervals((5, 6), (6, 8), 6),
            Some((5, 2)),
        );
        assert_eq!(
            try_combine_circular_intervals((5, 6), (6, 8), 7),
            Some((5, 1)),
        );
        assert_eq!(
            try_combine_circular_intervals((5, 6), (6, 8), 8),
            Some((5, 0)),
        );
        assert_eq!(
            try_combine_circular_intervals((5, 6), (6, 8), 9000),
            Some((5, 8)),
        );
    }

    #[test]
    fn test_circular_reduction__simple_case() {
        let data = vec![(0, 1), (2, 3), (3, 5), (6, 9)];
        let output = circular_reduction_with_failable_operator(data, |&a, &b| {
            try_combine_circular_intervals(a, b, 10)
        });
        let correct_output = vec![(6, 1), (2, 5)];
        assert_eq!(output, correct_output);
    }
}
