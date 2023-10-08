use super::{coordinate_frame_conversions::*, coordinates::*, general_utility::*};

#[derive(Clone, Hash, Debug, Eq, Copy)]
pub struct Quadrant(pub i32);

impl PartialEq for Quadrant {
    fn eq(&self, other: &Self) -> bool {
        self.0.rem_euclid(4) == other.0.rem_euclid(4)
    }
}
pub fn quadrants_of_rel_square(
    can_be_rel_square: impl Into<WorldStep> + Copy,
) -> HashSet<Quadrant> {
    // A square on an axis is in two quadrants
    // The origin is in all 4
    (0..4)
        .map(|i| Quadrant(i))
        .filter(|&q| rel_square_is_in_quadrant_or_on_adjacent_axis(can_be_rel_square, q))
        .collect()
}
pub fn rel_square_is_in_quadrant_or_on_adjacent_axis(
    can_be_rel_square: impl Into<WorldStep>,
    quadrant: Quadrant,
) -> bool {
    let rel_square: WorldStep = can_be_rel_square.into();
    let dir = direction_of_quadrant(quadrant);

    todo!()
}
pub fn direction_of_quadrant(quadrant: Quadrant) -> WorldStep {
    STEP_UP_RIGHT.rotated(QuarterTurnsCcw::new(quadrant.0))
}
#[cfg(test)]
mod tests {

    use ntest::{assert_about_eq, assert_false, assert_true, timeout};
    use pretty_assertions::{assert_eq, assert_ne};

    use super::*;

    #[test]
    #[ignore = "Not yet implemented"]
    fn test_quadrants_of_relative_square() {
        let point_quadrants: Vec<((i32, i32), Vec<i32>)> = vec![
            ((0, 0), vec![0, 1, 2, 3]),
            ((1, 0), vec![0, 3]),
            ((1, 1), vec![0]),
            ((0, 1), vec![0, 1]),
            ((0, -100), vec![3, 2]),
        ];
        point_quadrants.into_iter().for_each(|(p, v)| {
            assert_eq!(
                quadrants_of_rel_square(p),
                v.iter().map(|&q| Quadrant(q)).collect()
            );
        });
    }
}
