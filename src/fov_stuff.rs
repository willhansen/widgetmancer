use std::collections::{HashMap, HashSet};

use crate::glyph::angled_blocks::line_and_inside_point_to_angled_block_character;
use crate::glyph::{DoubleGlyph, Glyph};
use euclid::{vec2, Angle};
use ordered_float::OrderedFloat;

use crate::glyph::glyph_constants::{BLACK, RED, SPACE};
use crate::utility::angle_interval::{AngleInterval, AngleIntervalSet};
use crate::utility::{
    octant_to_outward_and_across_directions, point_clockwise_of_line,
    world_point_to_local_character_point, world_square_to_left_world_character_square, SquareSet,
    WorldLine, WorldMove, WorldPoint, WorldSquare, WorldSquareGlyphMap, STEP_DOWN_LEFT,
    STEP_DOWN_RIGHT, STEP_RIGHT, STEP_UP_LEFT, STEP_UP_RIGHT,
};

pub struct PartialVisibilityOfASquare {
    pub chosen_dividing_line_on_left_char: Option<WorldLine>,
    pub chosen_dividing_line_on_right_char: Option<WorldLine>,
}

const SIGHT_RADIUS: u32 = 8;

#[derive(Default)]
pub struct FovResult {
    pub fully_visible_squares: SquareSet,
    pub partially_visible_squares: HashMap<WorldSquare, PartialVisibilityOfASquare>,
}

impl FovResult {
    pub fn partially_visible_squares_as_glyph_mask(&self) -> WorldSquareGlyphMap {
        let mut the_map = WorldSquareGlyphMap::new();

        self.partially_visible_squares
            .iter()
            .for_each(|(&square, partial_visibility)| {
                the_map.insert(
                    square,
                    square_and_partial_visibility_to_glyphs(square, partial_visibility),
                );
            });
        the_map
    }
    pub fn at_least_partially_visible_squares(&self) -> SquareSet {
        let partial_vis: SquareSet = self.partially_visible_squares.keys().copied().collect();
        self.fully_visible_squares
            .union(&partial_vis)
            .copied()
            .collect()
    }
}

pub fn square_and_partial_visibility_to_glyphs(
    square: WorldSquare,
    partial_visibility: &PartialVisibilityOfASquare,
) -> DoubleGlyph {
    let optional_lines = vec![
        &partial_visibility.chosen_dividing_line_on_left_char,
        &partial_visibility.chosen_dividing_line_on_right_char,
    ];
    let left_character_square = world_square_to_left_world_character_square(square);
    let character_squares = vec![
        left_character_square,
        left_character_square + STEP_RIGHT.cast_unit(),
    ];
    (0..2)
        .map(|i| {
            let optional_line = &optional_lines[i];
            let character_square = character_squares[i];

            if let Some(line) = optional_line {
                let clockwise_point = point_clockwise_of_line(line);
                let angle_char = line_and_inside_point_to_angled_block_character(
                    world_point_to_local_character_point(line.p1, character_square),
                    world_point_to_local_character_point(line.p2, character_square),
                    world_point_to_local_character_point(clockwise_point, character_square),
                );
                Glyph::fg_only(angle_char, RED)
            } else {
                Glyph::transparent_glyph()
            }
        })
        .collect::<Vec<Glyph>>()
        .try_into()
        .unwrap()
}

pub fn field_of_view_from_square(
    start_square: WorldSquare,
    sight_blockers: &HashSet<WorldSquare>,
) -> FovResult {
    let mut fov_result = FovResult::default();
    fov_result.fully_visible_squares.insert(start_square);

    for octant_number in 0..8 {
        let (outward_dir, across_dir) = octant_to_outward_and_across_directions(octant_number);
        let mut blocked_arcs = AngleIntervalSet::new();
        // skip the central square
        for outward_steps in 1..SIGHT_RADIUS {
            for across_steps in 0..=outward_steps {
                let square = start_square
                    + outward_dir * outward_steps as i32
                    + across_dir * across_steps as i32;
                let square_angle_interval = angle_interval_of_square(start_square, square);
                if blocked_arcs.fully_contains_interval(square_angle_interval) {
                    continue;
                } else if sight_blockers.contains(&square) {
                    blocked_arcs.add_interval(square_angle_interval);
                    // TODO: partially visible blocks (just see one side)
                    //fov_result.fully_visible_squares.insert(square);
                } else if blocked_arcs.overlaps_interval(square_angle_interval) {
                    // TODO: partial visibility
                    //fov_result.fully_visible_squares.insert(square);
                } else {
                    // fully visible
                    fov_result.fully_visible_squares.insert(square);
                }
            }
        }
    }
    fov_result
}

pub fn angle_interval_of_square(
    sight_center: WorldSquare,
    blocking_square: WorldSquare,
) -> AngleInterval {
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

    AngleInterval {
        anticlockwise_end: *least_clockwise,
        clockwise_end: *most_clockwise,
    }
}

#[cfg(test)]
mod tests {
    use euclid::point2;
    use ntest::{assert_about_eq, assert_false};
    use pretty_assertions::{assert_eq, assert_ne};
    use rgb::RGB8;

    use super::*;

    #[test]
    fn test_square_view_angle__horizontal() {
        let view_angle = angle_interval_of_square(point2(0, 0), point2(3, 0));
        let correct_start_angle = WorldMove::new(2.5, 0.5).angle_from_x_axis();
        let correct_end_angle = WorldMove::new(2.5, -0.5).angle_from_x_axis();

        assert_about_eq!(
            view_angle.anticlockwise_end.radians,
            correct_start_angle.radians
        );
        assert_about_eq!(view_angle.clockwise_end.radians, correct_end_angle.radians);
    }

    #[test]
    fn test_square_view_angle__diagonalish() {
        let view_angle = angle_interval_of_square(point2(0, 0), point2(5, 3));
        let correct_start_angle = WorldMove::new(4.5, 3.5).angle_from_x_axis();
        let correct_end_angle = WorldMove::new(5.5, 2.5).angle_from_x_axis();

        assert_about_eq!(
            view_angle.anticlockwise_end.radians,
            correct_start_angle.radians
        );
        assert_about_eq!(view_angle.clockwise_end.radians, correct_end_angle.radians);
    }

    #[test]
    fn test_field_of_view_with_no_obstacles() {
        let start_square = point2(5, 5);
        let fov_result = field_of_view_from_square(start_square, &SquareSet::default());
        assert!(fov_result.partially_visible_squares.is_empty());
        assert!(fov_result.fully_visible_squares.contains(&start_square));
        let square_area = (SIGHT_RADIUS * 2 + 1).pow(2);
        assert_eq!(fov_result.fully_visible_squares.len(), square_area as usize);
    }
}
