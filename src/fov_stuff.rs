use std::collections::{HashMap, HashSet};

use crate::glyph::angled_blocks::half_plane_to_angled_block_character;
use crate::glyph::{DoubleGlyph, Glyph};
use euclid::{point2, vec2, Angle};
use ordered_float::OrderedFloat;

use crate::glyph::glyph_constants::{BLACK, CYAN, DARK_CYAN, OUT_OF_SIGHT_COLOR, RED, SPACE};
use crate::utility::angle_interval::{AngleInterval, AngleIntervalSet};
use crate::utility::coordinate_frame_conversions::*;
use crate::utility::{
    is_clockwise, octant_to_outward_and_across_directions, rotate_point_around_point, HalfPlane,
    Line, WorldLine, STEP_DOWN_LEFT, STEP_DOWN_RIGHT, STEP_RIGHT, STEP_UP_LEFT, STEP_UP_RIGHT,
};

#[derive(Clone, PartialEq, Debug, Copy)]
pub struct PartialVisibilityOfASquare {
    pub right_char_shadow: HalfPlane<f32, CharacterGridInLocalCharacterFrame>,
    pub left_char_shadow: HalfPlane<f32, CharacterGridInLocalCharacterFrame>,
}
impl PartialVisibilityOfASquare {
    pub fn get(&self, i: usize) -> &HalfPlane<f32, CharacterGridInLocalCharacterFrame> {
        match i {
            0 => &self.left_char_shadow,
            1 => &self.right_char_shadow,
            _ => panic!("tried getting invalid character of square: {}", i),
        }
    }
    pub fn to_glyphs(&self) -> DoubleGlyph {
        let left_character_square = world_square_to_left_world_character_square(point2(0, 0));
        let character_squares = vec![
            left_character_square,
            left_character_square + STEP_RIGHT.cast_unit(),
        ];
        (0..2)
            .map(|i| {
                let half_plane = self.get(i);
                let character_square = character_squares[i];
                let angle_char = half_plane_to_angled_block_character(*half_plane);
                Glyph::fg_only(angle_char, OUT_OF_SIGHT_COLOR)
            })
            .collect::<Vec<Glyph>>()
            .try_into()
            .unwrap()
    }
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
                the_map.insert(square, partial_visibility.to_glyphs());
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

pub fn field_of_view_from_square(
    start_square: WorldSquare,
    sight_blockers: &HashSet<WorldSquare>,
) -> FovResult {
    let mut fov_result = FovResult::default();
    fov_result.fully_visible_squares.insert(start_square);

    for octant_number in 0..8 {
        let (outward_dir, across_dir) = octant_to_outward_and_across_directions(octant_number);
        let mut shadows = AngleIntervalSet::new();
        // skip the central square
        for outward_steps in 1..=SIGHT_RADIUS {
            for across_steps in 0..=outward_steps {
                let square = start_square
                    + outward_dir * outward_steps as i32
                    + across_dir * across_steps as i32;
                let shadow_for_this_square = angle_interval_of_square(square - start_square);
                if shadows.fully_contains_interval(shadow_for_this_square) {
                    continue;
                } else if sight_blockers.contains(&square) {
                    shadows.add_interval(shadow_for_this_square);
                    // TODO: partially visible blocks (just see one side)
                    // fully in view for now
                    fov_result.fully_visible_squares.insert(square);
                } else if shadows.partially_overlaps_interval(shadow_for_this_square) {
                    // Partial overlap case
                    let square_relative_to_start_square: WorldStep = square - start_square;
                    let shadows_on_characters =
                        visibility_of_shadowed_square(&shadows, square_relative_to_start_square);

                    // partially visible
                    fov_result
                        .partially_visible_squares
                        .insert(square, shadows_on_characters);
                } else {
                    // fully visible
                    fov_result.fully_visible_squares.insert(square);
                }
            }
        }
    }
    fov_result
}

fn visibility_of_shadowed_square(
    shadows: &AngleIntervalSet,
    square_relative_to_shadow_center: WorldStep,
) -> PartialVisibilityOfASquare {
    let overlapped_shadow_edge = shadows
        .most_overlapped_edge_of_set(angle_interval_of_square(square_relative_to_shadow_center))
        .unwrap();

    let shadow_line_from_center = Line {
        p1: point2(0.0, 0.0),
        p2: point2(
            overlapped_shadow_edge.end_angle.radians.cos(),
            overlapped_shadow_edge.end_angle.radians.sin(),
        ),
    };
    let extra_rotation_for_shadow_point = Angle::degrees(1.0)
        * if overlapped_shadow_edge.is_low_end {
            1.0
        } else {
            -1.0
        };
    let point_in_shadow = rotate_point_around_point(
        shadow_line_from_center.p1,
        shadow_line_from_center.p2,
        extra_rotation_for_shadow_point,
    );

    let shadow_half_plane = HalfPlane::new(shadow_line_from_center, point_in_shadow);

    // do a few forbidden conversions here.
    // TODO: FIX
    let left_character_square = world_square_to_left_world_character_square(
        square_relative_to_shadow_center.to_point().cast_unit(),
    );
    let right_character_square = left_character_square + STEP_RIGHT.cast_unit();
    let left_char_shadow =
        world_half_plane_to_local_character_half_plane(shadow_half_plane, left_character_square);
    let right_char_shadow =
        world_half_plane_to_local_character_half_plane(shadow_half_plane, right_character_square);

    PartialVisibilityOfASquare {
        left_char_shadow,
        right_char_shadow,
    }
}

pub fn angle_interval_of_square(relative_square: WorldStep) -> AngleInterval {
    assert_ne!(relative_square, vec2(0, 0));
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
    use crate::glyph::glyph_constants::FULL_BLOCK;
    use crate::glyph::DoubleGlyphFunctions;
    use crate::utility::{STEP_DOWN, STEP_UP};
    use euclid::point2;
    use itertools::Itertools;
    use ntest::{assert_about_eq, assert_false};
    use pretty_assertions::{assert_eq, assert_ne};
    use rgb::RGB8;

    use super::*;

    #[test]
    fn test_square_view_angle__horizontal() {
        let view_angle = angle_interval_of_square(vec2(3, 0));
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
        let view_angle = angle_interval_of_square(vec2(5, 3));
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
    #[test]
    fn test_field_of_view_includes_blocks() {
        let start_square = point2(5, 5);
        let block_square = point2(5, 7);
        let blocks = SquareSet::from([block_square]);
        let fov_result = field_of_view_from_square(start_square, &blocks);
        assert!(fov_result.fully_visible_squares.contains(&block_square));
        assert!(fov_result
            .fully_visible_squares
            .contains(&(block_square + STEP_DOWN)));
        assert!(!fov_result
            .fully_visible_squares
            .contains(&(block_square + STEP_UP)));
    }
    #[test]
    fn test_partial_squares_look_partial() {
        let start_square = point2(5, 5);
        let block_square = start_square + STEP_DOWN * 2;
        let blocks = SquareSet::from([block_square]);
        let fov_result = field_of_view_from_square(start_square, &blocks);
        assert!(!fov_result.partially_visible_squares.is_empty());
        assert!(fov_result.partially_visible_squares.iter().all(
            |(&square, partial_visibility): (&WorldSquare, &PartialVisibilityOfASquare)| {
                partial_visibility
                    .to_glyphs()
                    .iter()
                    .any(|glyph: &Glyph| !glyph.looks_solid())
            }
        ));
    }
    #[test]
    fn test_diagonal_shadow_looks_diagonal() {
        let start_square = point2(5, 5);
        let block_square = start_square + STEP_RIGHT;
        let blocks = SquareSet::from([block_square]);
        let fov_result = field_of_view_from_square(start_square, &blocks);
        for i in 1..=5 {
            let square = start_square + STEP_UP_RIGHT * i;
            let partial_visibility = fov_result.partially_visible_squares.get(&square).unwrap();
            println!("{}", partial_visibility.to_glyphs().to_clean_string());
            //dbg!(partial_visibility);
            let string = partial_visibility.to_glyphs().to_clean_string();
            println!("{}", string);
            // one of these two is right.  Not sure which
            assert!(["🭈🭄", "🭊🭂"].contains(&&*string));
        }
    }
    #[test]
    fn test_single_square_is_shadowed_correctly_on_diagonal() {
        let mut shadows = AngleIntervalSet::new();
        shadows.add_interval(AngleInterval::from_degrees(0.0, 45.0));
        let square_relative_to_center = vec2(1, 1);
        let visibility = visibility_of_shadowed_square(&shadows, square_relative_to_center);
        let string = visibility.to_glyphs().to_clean_string();
        println!("{}", string);
        dbg!(visibility);
        assert!(["🭈🭄", "🭊🭂"].contains(&&*string));
    }
    #[test]
    fn test_partial_visibility_to_glyphs() {
        let partial_visibility = PartialVisibilityOfASquare {
            left_char_shadow: HalfPlane::new(
                Line {
                    p1: point2(-0.5, -0.5),
                    p2: point2(1.5, 0.5),
                },
                point2(2.0, 0.0),
            ),
            right_char_shadow: HalfPlane::new(
                Line {
                    p1: point2(-1.5, -0.5),
                    p2: point2(0.5, 0.5),
                },
                point2(2.0, 0.0),
            ),
        };

        let string = partial_visibility.to_glyphs().to_clean_string();
        assert!(["🭈🭄", "🭊🭂"].contains(&&*string));
    }
    #[test]
    fn test_partial_visibility_to_glyphs__data_from_failure() {
        let partial_visibility = PartialVisibilityOfASquare {
            right_char_shadow: HalfPlane {
                dividing_line: Line {
                    p1: point2(-2.5, -1.0),
                    p2: point2(-1.0857865, -0.29289323),
                },
                point_on_half_plane: point2(-1.0610383, -0.30548787),
            },
            left_char_shadow: HalfPlane {
                dividing_line: Line {
                    p1: point2(-1.5, -1.0),
                    p2: point2(-0.08578646, -0.29289323),
                },
                point_on_half_plane: point2(-0.061038256, -0.30548787),
            },
        };
        assert!(is_clockwise(
            partial_visibility.left_char_shadow.dividing_line.p1,
            partial_visibility.left_char_shadow.dividing_line.p2,
            partial_visibility.left_char_shadow.point_on_half_plane
        ));
        assert!(is_clockwise(
            partial_visibility.right_char_shadow.dividing_line.p1,
            partial_visibility.right_char_shadow.dividing_line.p2,
            partial_visibility.right_char_shadow.point_on_half_plane
        ));
        let string = partial_visibility.to_glyphs().to_clean_string();
        assert!(["🭈🭄", "🭊🭂"].contains(&&*string));
    }
    #[test]
    fn test_observed_bright_spot_in_shadow() {
        let player_square = point2(3, 3);
        let block_square = player_square + STEP_UP_RIGHT * 2;
        let test_square = block_square + STEP_UP;

        let fov_result = field_of_view_from_square(player_square, &SquareSet::from([block_square]));
        let visibility_of_test_square = fov_result
            .partially_visible_squares
            .get(&test_square)
            .unwrap();
        assert_eq!(
            visibility_of_test_square
                .to_glyphs()
                .to_clean_string()
                .chars()
                .nth(1)
                .unwrap(),
            FULL_BLOCK
        );
    }
}
