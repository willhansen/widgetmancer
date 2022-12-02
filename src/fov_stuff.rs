use std::collections::{HashMap, HashSet};

use euclid::{vec2, Angle};
use ordered_float::OrderedFloat;

use crate::glyph::glyph_constants::SPACE;
use crate::utility::{
    octant_to_outward_and_across_directions, SquareSet, WorldMove, WorldPoint, WorldSquare,
    WorldSquareGlyphMap, STEP_DOWN_LEFT, STEP_DOWN_RIGHT, STEP_UP_LEFT, STEP_UP_RIGHT,
};

pub struct Line {
    p1: WorldPoint,
    p2: WorldPoint,
}

pub struct PartialVisibilityOfASquare {
    chosen_dividing_line_on_left_char: Option<Line>,
    chosen_dividing_line_on_right_char: Option<Line>,
}

pub struct FovResult {
    fully_visible_squares: SquareSet,
    partially_visible_squares: HashMap<WorldSquare, PartialVisibilityOfASquare>,
}

impl FovResult {
    pub fn as_glyph_mask(&self) -> WorldSquareGlyphMap {
        todo!()
    }
}

pub struct ClosedClockWiseAngleInterval {
    start_angle: Angle<f32>,
    end_angle: Angle<f32>,
}

pub fn field_of_view_from_square(
    start_square: WorldSquare,
    sight_blockers: SquareSet,
) -> FovResult {
    let sight_radius = 8;
    for octant_number in 0..8 {
        let (outward_dir, across_dir) = octant_to_outward_and_across_directions(octant_number);
        // skip the central square
        for outward_steps in 1..sight_radius {
            for across_steps in 0..=outward_steps {
                let square = start_square + outward_dir * outward_steps + across_dir * across_steps;
                let square_angle_interval = angle_interval_of_square(start_square, square);
            }
        }
    }
    todo!()
}

pub fn angle_interval_of_square(
    sight_center: WorldSquare,
    blocking_square: WorldSquare,
) -> ClosedClockWiseAngleInterval {
    assert_ne!(sight_center, blocking_square);
    let relative_square = blocking_square - sight_center;
    let rel_square_center = relative_square.to_f32();
    let rel_square_corners: Vec<WorldMove> = vec![
        rel_square_center + STEP_UP_RIGHT.to_f32() * 0.5,
        rel_square_center + STEP_UP_LEFT.to_f32() * 0.5,
        rel_square_center + STEP_DOWN_LEFT.to_f32() * 0.5,
        rel_square_center + STEP_DOWN_RIGHT.to_f32() * 0.5,
    ];

    let center_angle = rel_square_center.angle_from_x_axis();
    let corner_angles: Vec<Angle<f32>> = rel_square_corners
        .iter()
        .map(|rel_corner_point| rel_corner_point.angle_from_x_axis())
        .collect();

    let most_clockwise = corner_angles
        .iter()
        .min_by_key(|&&c| OrderedFloat(center_angle.angle_to(c).radians))
        .unwrap();
    let least_clockwise = corner_angles
        .iter()
        .max_by_key(|&&c| OrderedFloat(center_angle.angle_to(c).radians))
        .unwrap();

    ClosedClockWiseAngleInterval {
        start_angle: *least_clockwise,
        end_angle: *most_clockwise,
    }
}
#[cfg(test)]
mod tests {
    use ntest::assert_false;
    use pretty_assertions::{assert_eq, assert_ne};
    use rgb::RGB8;

    use super::*;
    #[test]
    fn test_square_view_angle() {}
}
