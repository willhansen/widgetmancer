use std::collections::{HashMap, HashSet};

use euclid::{point2, vec2, Angle};
use itertools::all;
use ordered_float::OrderedFloat;

use crate::glyph::angled_blocks::half_plane_to_angled_block_character;
use crate::glyph::glyph_constants::{
    BLACK, CYAN, DARK_CYAN, FULL_BLOCK, OUT_OF_SIGHT_COLOR, RED, SPACE,
};
use crate::glyph::{DoubleGlyph, DoubleGlyphFunctions, Glyph};
use crate::piece::MAX_PIECE_RANGE;
use crate::portal_geometry::PortalGeometry;
use crate::utility::angle_interval::{AngleInterval, AngleIntervalSet};
use crate::utility::coordinate_frame_conversions::*;
use crate::utility::{
    direction_from_angle, intersection, is_clockwise, line_intersections_with_centered_unit_square,
    line_intersects_with_centered_unit_square, octant_to_outward_and_across_directions,
    rotate_point_around_point, same_side_of_line, set_of_keys, union, HalfPlane, Line, WorldLine,
    STEP_DOWN_LEFT, STEP_DOWN_RIGHT, STEP_RIGHT, STEP_UP_LEFT, STEP_UP_RIGHT, STEP_ZERO,
};

#[derive(Clone, PartialEq, Debug, Copy)]
pub struct PartialVisibilityOfASquare {
    pub right_char_shadow: Option<CharacterShadow>,
    pub left_char_shadow: Option<CharacterShadow>,
}

type CharacterShadow = HalfPlane<f32, CharacterGridInLocalCharacterFrame>;

impl PartialVisibilityOfASquare {
    pub fn get(&self, i: usize) -> &Option<CharacterShadow> {
        match i {
            0 => &self.left_char_shadow,
            1 => &self.right_char_shadow,
            _ => panic!("tried getting invalid character of square: {}", i),
        }
    }

    pub fn fully_visible() -> Self {
        PartialVisibilityOfASquare {
            right_char_shadow: None,
            left_char_shadow: None,
        }
    }

    pub fn shadows(&self) -> Vec<Option<CharacterShadow>> {
        vec![self.left_char_shadow, self.right_char_shadow]
    }

    pub fn to_glyphs(&self) -> DoubleGlyph {
        let left_character_square = world_square_to_left_world_character_square(point2(0, 0));
        let character_squares = vec![
            left_character_square,
            left_character_square + STEP_RIGHT.cast_unit(),
        ];
        self.shadows()
            .iter()
            .map(|optional_shadow| {
                if let Some(half_plane) = optional_shadow {
                    let angle_char = half_plane_to_angled_block_character(*half_plane);
                    Glyph::fg_only(angle_char, OUT_OF_SIGHT_COLOR)
                } else {
                    Glyph::transparent_glyph()
                }
            })
            .collect::<Vec<Glyph>>()
            .try_into()
            .unwrap()
    }

    pub fn combine(&self, other: &Self) -> Self {
        let complement_tolerance = 1e-6;
        let combined_shadows: Vec<Option<HalfPlane<_, _>>> = (0..2)
            .map(|i| {
                let self_shadow = *self.get(i);
                let other_shadow = *other.get(i);
                if self_shadow.is_none() && other_shadow.is_none() {
                    None
                } else if self_shadow.is_some() && other_shadow.is_none() {
                    self_shadow
                } else if self_shadow.is_none() && other_shadow.is_some() {
                    other_shadow
                } else if self_shadow
                    .unwrap()
                    .is_about_complementary_to(other_shadow.unwrap(), complement_tolerance)
                {
                    None
                } else {
                    // TODO: better combination method than taking just one
                    self_shadow
                }
            })
            .collect();

        PartialVisibilityOfASquare {
            left_char_shadow: combined_shadows[0],
            right_char_shadow: combined_shadows[1],
        }
    }
    pub fn is_fully_visible(&self) -> bool {
        self.to_glyphs().looks_solid()
            && self.shadows().iter().all(|optional_shadow| {
                optional_shadow.is_none()
                    || optional_shadow.is_some_and(|shadow| !shadow.covers_origin())
            })
    }
    pub fn is_fully_visible__old_version(&self) -> bool {
        self.shadows()
            .iter()
            .all(|shadow_optional: &Option<CharacterShadow>| {
                shadow_optional.is_none()
                    || shadow_optional.is_some_and(|shadow: CharacterShadow| {
                        !line_intersects_with_centered_unit_square(shadow.dividing_line)
                            && !shadow.covers_origin()
                    })
            })
    }
    pub fn is_fully_non_visible(&self) -> bool {
        self.to_glyphs().looks_solid()
            && self
                .shadows()
                .iter()
                .all(|optional_shadow| optional_shadow.is_some_and(|shadow| shadow.covers_origin()))
    }
    pub fn is_fully_non_visible__old_version(&self) -> bool {
        self.shadows()
            .iter()
            .all(|shadow_optional: &Option<CharacterShadow>| {
                shadow_optional.is_some_and(|shadow: CharacterShadow| {
                    !line_intersects_with_centered_unit_square(shadow.dividing_line)
                        && shadow.covers_origin()
                })
            })
    }
}

#[derive(Default, Debug)]
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
        let partial_vis: SquareSet = self.squares_with_partials();
        self.fully_visible_squares
            .union(&partial_vis)
            .copied()
            .collect()
    }
    pub fn squares_with_partials(&self) -> SquareSet {
        self.partially_visible_squares.keys().copied().collect()
    }
    pub fn combine(&self, other: Self) -> Self {
        type PartialVisibilityMap = HashMap<WorldSquare, PartialVisibilityOfASquare>;

        let squares_with_non_conflicting_partials: SquareSet = self
            .squares_with_partials()
            .symmetric_difference(&other.squares_with_partials())
            .copied()
            .collect();

        let self_non_conflicting_partials: PartialVisibilityMap = self
            .partially_visible_squares
            .clone()
            .into_iter()
            .filter(|(square, partial)| squares_with_non_conflicting_partials.contains(square))
            .collect();

        let other_non_conflicting_partials: PartialVisibilityMap = other
            .partially_visible_squares
            .clone()
            .into_iter()
            .filter(|(square, partial)| squares_with_non_conflicting_partials.contains(square))
            .collect();

        let all_squares_with_partials: SquareSet = self
            .squares_with_partials()
            .union(&other.squares_with_partials())
            .copied()
            .collect();

        let squares_with_conflicting_partials: SquareSet = all_squares_with_partials
            .difference(&squares_with_non_conflicting_partials)
            .copied()
            .collect();

        let combined_partials: PartialVisibilityMap = squares_with_conflicting_partials
            .into_iter()
            .map(|square| {
                let combined_partial = self
                    .partially_visible_squares
                    .get(&square)
                    .unwrap()
                    .combine(other.partially_visible_squares.get(&square).unwrap());
                (square, combined_partial)
            })
            .collect();

        let conflicting_partials_that_combine_to_full_visibility: SquareSet = combined_partials
            .iter()
            .filter(|(square, partial)| partial.is_fully_visible())
            .map(|(square, partial)| square)
            .copied()
            .collect();

        let conflicting_partials_that_remain_partials: PartialVisibilityMap = combined_partials
            .into_iter()
            .filter(|(square, partial)| !partial.is_fully_visible())
            .collect();

        let mut all_partials: PartialVisibilityMap = self_non_conflicting_partials;
        all_partials.extend(other_non_conflicting_partials);
        all_partials.extend(conflicting_partials_that_remain_partials);

        let all_fully_visible: SquareSet = union(
            &union(&self.fully_visible_squares, &other.fully_visible_squares),
            &conflicting_partials_that_combine_to_full_visibility,
        );

        let squares_somehow_both_fully_and_partially_visible =
            intersection(&set_of_keys(&all_partials), &all_fully_visible);
        assert!(squares_somehow_both_fully_and_partially_visible.is_empty());

        FovResult {
            fully_visible_squares: all_fully_visible,
            partially_visible_squares: all_partials,
        }
    }
}

struct OctantFOVSquareSequenceIter {
    outward_dir: WorldStep,
    across_dir: WorldStep,
    outward_steps: u32,
    across_steps: u32,
}

impl OctantFOVSquareSequenceIter {
    // one_before_starting_square is useful for skipping the vec2(0, 0) square.
    pub fn new(octant_number: i32, one_before_starting_square: WorldStep) -> Self {
        let (outward_dir, across_dir) = octant_to_outward_and_across_directions(octant_number);
        let prev_square_outward_steps = outward_dir.dot(one_before_starting_square) as u32;
        let prev_square_across_steps = across_dir.dot(one_before_starting_square) as u32;

        let (outward_steps, across_steps) =
            Self::next_out_and_across_steps(prev_square_outward_steps, prev_square_across_steps);

        OctantFOVSquareSequenceIter {
            outward_dir,
            across_dir,
            outward_steps,
            across_steps,
        }
    }

    fn next_out_and_across_steps(outward_steps: u32, across_steps: u32) -> (u32, u32) {
        if across_steps == outward_steps {
            (outward_steps + 1, 0)
        } else {
            (outward_steps, across_steps + 1)
        }
    }
}

impl Iterator for OctantFOVSquareSequenceIter {
    type Item = WorldStep;

    fn next(&mut self) -> Option<Self::Item> {
        let relative_square = self.outward_dir * self.outward_steps as i32
            + self.across_dir * self.across_steps as i32;

        (self.outward_steps, self.across_steps) =
            Self::next_out_and_across_steps(self.outward_steps, self.across_steps);

        Some(relative_square)
    }
}

pub fn field_of_view_within_arc_in_single_octant(
    sight_blockers: &HashSet<WorldSquare>,
    portal_geometry: &PortalGeometry,
    center_square: WorldSquare,
    radius: u32,
    octant_number: i32,
    view_arc: AngleInterval,
    start_checking_after_this_square_in_the_fov_sequence: WorldStep,
) -> FovResult {
    let mut fov_result = FovResult::default();
    for relative_square in OctantFOVSquareSequenceIter::new(
        octant_number,
        start_checking_after_this_square_in_the_fov_sequence,
    ) {
        if relative_square.x.abs() > radius as i32 || relative_square.y.abs() > radius as i32 {
            break;
        }

        let absolute_square = center_square + relative_square;

        let view_arc_of_this_square = AngleInterval::from_square(relative_square);

        if relative_square == vec2(0, 1) {
            dbg!(
                &absolute_square,
                view_arc.to_degrees(),
                view_arc_of_this_square.to_degrees()
            );
        }
        dbg!("asdfasdf");
        if view_arc.fully_contains_interval(view_arc_of_this_square) {
            fov_result.fully_visible_squares.insert(absolute_square);
        } else if view_arc.partially_overlaps(view_arc_of_this_square)
            || view_arc_of_this_square.fully_contains_interval(view_arc)
        {
            if relative_square == vec2(0, 1) {
                dbg!("partial_coverage");
            }
            let partial_visibility_for_square =
                partial_visibility_of_square_from_one_view_arc(view_arc, relative_square);
            let is_actually_fully_visible = partial_visibility_for_square.is_fully_visible();
            let is_actually_not_visible = partial_visibility_for_square.is_fully_non_visible();
            if is_actually_fully_visible {
                fov_result.fully_visible_squares.insert(absolute_square);
            } else if is_actually_not_visible {
                // do nothing
            } else {
                fov_result
                    .partially_visible_squares
                    .insert(absolute_square, partial_visibility_for_square);
            }
        }

        let is_sight_blocker = sight_blockers.contains(&absolute_square);
        if is_sight_blocker {
            // split arc and recurse
            let view_arcs_around_blocker: Vec<AngleInterval> =
                view_arc.split_around_arc(view_arc_of_this_square);
            view_arcs_around_blocker
                .into_iter()
                .filter(|new_sub_arc| new_sub_arc.width().to_degrees() > 0.01)
                .for_each(|new_sub_arc| {
                    fov_result = fov_result.combine(field_of_view_within_arc_in_single_octant(
                        sight_blockers,
                        portal_geometry,
                        center_square,
                        radius,
                        octant_number,
                        new_sub_arc,
                        relative_square,
                    ));
                });
        }
        // TODO: portals
    }
    if octant_number == 1 {
        dbg!(&fov_result.partially_visible_squares.keys());
    }
    fov_result
}

pub fn single_octant_field_of_view(
    sight_blockers: &HashSet<WorldSquare>,
    portal_geometry: &PortalGeometry,
    center_square: WorldSquare,
    radius: u32,
    octant_number: i32,
) -> FovResult {
    //arc.next_relative_square_in_octant_sequence(first_relative_square_in_sequence);
    //let octant: i32 = arc.octant().expect("arc not confined to octant");
    let mut fov_result = FovResult::default();
    fov_result.fully_visible_squares.insert(center_square);
    let full_octant_arc = AngleInterval::from_octant(octant_number);
    fov_result.combine(field_of_view_within_arc_in_single_octant(
        sight_blockers,
        portal_geometry,
        center_square,
        radius,
        octant_number,
        full_octant_arc,
        STEP_ZERO,
    ))
}

pub fn portal_aware_field_of_view_from_square(
    center_square: WorldSquare,
    radius: u32,
    sight_blockers: &HashSet<WorldSquare>,
    portal_geometry: &PortalGeometry,
) -> FovResult {
    (0..8).fold(
        FovResult::default(),
        |fov_result_accumulator: FovResult, octant_number: i32| {
            let new_fov_result = single_octant_field_of_view(
                sight_blockers,
                portal_geometry,
                center_square,
                radius,
                octant_number,
            );
            fov_result_accumulator.combine(new_fov_result)
        },
    )
}

pub fn field_of_view_from_square(
    start_square: WorldSquare,
    radius: u32,
    sight_blockers: &HashSet<WorldSquare>,
) -> FovResult {
    portal_aware_field_of_view_from_square(
        start_square,
        radius,
        sight_blockers,
        &PortalGeometry::default(),
    )
}

fn point_in_view_arc(view_arc: AngleInterval) -> WorldMove {
    direction_from_angle(view_arc.center_angle()).cast_unit()
}

fn partial_visibility_of_square_from_one_view_arc(
    visibility_arc: AngleInterval,
    square_relative_to_center: WorldStep,
) -> PartialVisibilityOfASquare {
    let square_arc = AngleInterval::from_square(square_relative_to_center);
    assert!(
        visibility_arc.partially_overlaps(square_arc)
            || square_arc.fully_contains_interval(visibility_arc)
    );

    let shadow_arc = visibility_arc.complement();
    let mut shadows = AngleIntervalSet::new();
    shadows.add_interval(shadow_arc);
    let overlapped_shadow_edge = shadows.most_overlapped_edge_of_set(square_arc).unwrap();

    let shadow_line_from_center = Line {
        p1: point2(0.0, 0.0),
        p2: point2(
            overlapped_shadow_edge.angle.radians.cos(),
            overlapped_shadow_edge.angle.radians.sin(),
        ),
    };
    let extra_rotation_for_shadow_point = Angle::degrees(1.0)
        * if overlapped_shadow_edge.is_clockwise_edge {
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
        square_relative_to_center.to_point().cast_unit(),
    );
    let right_character_square = left_character_square + STEP_RIGHT.cast_unit();
    let left_char_shadow = Some(world_half_plane_to_local_character_half_plane(
        shadow_half_plane,
        left_character_square,
    ));
    let right_char_shadow = Some(world_half_plane_to_local_character_half_plane(
        shadow_half_plane,
        right_character_square,
    ));

    PartialVisibilityOfASquare {
        left_char_shadow,
        right_char_shadow,
    }
}

#[deprecated(note = "use AngleInterval::from_square instead")]
pub fn angle_interval_of_square(relative_square: WorldStep) -> AngleInterval {
    AngleInterval::from_square(relative_square)
}

#[cfg(test)]
mod tests {
    use euclid::point2;
    use itertools::Itertools;
    use ntest::{assert_about_eq, assert_false};
    use pretty_assertions::{assert_eq, assert_ne};
    use rgb::RGB8;

    use crate::glyph::glyph_constants::FULL_BLOCK;
    use crate::glyph::DoubleGlyphFunctions;
    use crate::utility::{
        angle_from_better_x_axis, line_intersections_with_centered_unit_square, STEP_DOWN,
        STEP_LEFT, STEP_UP,
    };

    use super::*;

    const SIGHT_RADIUS: u32 = 16;

    #[test]
    fn test_square_view_angle__horizontal() {
        let view_angle = angle_interval_of_square(vec2(3, 0));
        let correct_start_angle = angle_from_better_x_axis(WorldMove::new(2.5, 0.5));
        let correct_end_angle = angle_from_better_x_axis(WorldMove::new(2.5, -0.5));

        assert_about_eq!(
            view_angle.anticlockwise_end.radians,
            correct_start_angle.radians
        );
        assert_about_eq!(view_angle.clockwise_end.radians, correct_end_angle.radians);
    }

    #[test]
    fn test_square_view_angle__diagonalish() {
        let view_angle = angle_interval_of_square(vec2(5, 3));
        let correct_start_angle = angle_from_better_x_axis(WorldMove::new(4.5, 3.5));
        let correct_end_angle = angle_from_better_x_axis(WorldMove::new(5.5, 2.5));

        assert_about_eq!(
            view_angle.anticlockwise_end.radians,
            correct_start_angle.radians
        );
        assert_about_eq!(view_angle.clockwise_end.radians, correct_end_angle.radians);
    }

    #[test]
    fn test_field_of_view_with_no_obstacles() {
        let start_square = point2(5, 5);
        let fov_result =
            field_of_view_from_square(start_square, SIGHT_RADIUS, &SquareSet::default());
        //dbg!(&fov_result.partially_visible_squares);
        assert!(fov_result.partially_visible_squares.is_empty());
        assert!(fov_result.fully_visible_squares.contains(&start_square));
        let square_area = (SIGHT_RADIUS * 2 + 1).pow(2);
        assert_eq!(fov_result.fully_visible_squares.len(), square_area as usize);
    }

    #[test]
    fn test_small_field_of_view_with_no_obstacles() {
        let start_square = point2(5, 5);
        let radius = 2;
        let fov_result = portal_aware_field_of_view_from_square(
            start_square,
            radius,
            &SquareSet::default(),
            &PortalGeometry::default(),
        );

        dbg!(
            &fov_result.fully_visible_squares.get(&point2(5, 4)),
            &fov_result.fully_visible_squares.get(&point2(5, 6)),
        );
        //dbg!(&fov_result.partially_visible_squares.get(&point2(4, 3)));

        assert!(fov_result.partially_visible_squares.is_empty());
        assert!(fov_result.fully_visible_squares.contains(&start_square));
        let square_area = (radius * 2 + 1).pow(2);
        assert_eq!(fov_result.fully_visible_squares.len(), square_area as usize);
    }

    #[test]
    fn test_partially_visible_square_knows_if_its_fully_visible() {
        // Data from failure case
        let partial = PartialVisibilityOfASquare {
            right_char_shadow: Some(HalfPlane {
                dividing_line: Line {
                    p1: point2(1.5, 2.0),
                    p2: point2(0.08578634, 1.2928933),
                },
                point_on_half_plane: point2(0.061038017, 1.3054879),
            }),
            left_char_shadow: Some(HalfPlane {
                dividing_line: Line {
                    p1: point2(2.5, 2.0),
                    p2: point2(1.0857863, 1.2928933),
                },
                point_on_half_plane: point2(1.061038, 1.3054879),
            }),
        };
        assert!(partial.to_glyphs().looks_solid());
        assert_eq!(partial.to_glyphs().to_clean_string(), "  ");
        assert!(partial.is_fully_visible());
    }

    #[test]
    fn test_field_of_view_includes_blocks() {
        let start_square = point2(5, 5);
        let block_square = point2(5, 7);
        let blocks = SquareSet::from([block_square]);
        let fov_result = field_of_view_from_square(start_square, SIGHT_RADIUS, &blocks);
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
        let fov_result = field_of_view_from_square(start_square, SIGHT_RADIUS, &blocks);
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
        let fov_result = field_of_view_from_square(start_square, SIGHT_RADIUS, &blocks);
        for i in 1..=5 {
            let square = start_square + STEP_UP_RIGHT * i;
            let partial_visibility = fov_result.partially_visible_squares.get(&square).unwrap();
            //dbg!(partial_visibility);
            let string = partial_visibility.to_glyphs().to_clean_string();
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
        //dbg!(visibility);
        assert!(["🭈🭄", "🭊🭂"].contains(&&*string));
    }

    #[test]
    fn test_partial_visibility_to_glyphs() {
        let partial_visibility = PartialVisibilityOfASquare {
            left_char_shadow: Some(HalfPlane::new(
                Line {
                    p1: point2(-0.5, -0.5),
                    p2: point2(1.5, 0.5),
                },
                point2(2.0, 0.0),
            )),
            right_char_shadow: Some(HalfPlane::new(
                Line {
                    p1: point2(-1.5, -0.5),
                    p2: point2(0.5, 0.5),
                },
                point2(2.0, 0.0),
            )),
        };

        let string = partial_visibility.to_glyphs().to_clean_string();
        assert!(["🭈🭄", "🭊🭂"].contains(&&*string));
    }

    #[test]
    fn test_partial_visibility_to_glyphs__data_from_failure() {
        let partial_visibility = PartialVisibilityOfASquare {
            right_char_shadow: Some(HalfPlane {
                dividing_line: Line {
                    p1: point2(-2.5, -1.0),
                    p2: point2(-1.0857865, -0.29289323),
                },
                point_on_half_plane: point2(-1.0610383, -0.30548787),
            }),
            left_char_shadow: Some(HalfPlane {
                dividing_line: Line {
                    p1: point2(-1.5, -1.0),
                    p2: point2(-0.08578646, -0.29289323),
                },
                point_on_half_plane: point2(-0.061038256, -0.30548787),
            }),
        };
        assert!(is_clockwise(
            partial_visibility
                .left_char_shadow
                .unwrap()
                .dividing_line
                .p1,
            partial_visibility
                .left_char_shadow
                .unwrap()
                .dividing_line
                .p2,
            partial_visibility
                .left_char_shadow
                .unwrap()
                .point_on_half_plane,
        ));
        assert!(is_clockwise(
            partial_visibility
                .right_char_shadow
                .unwrap()
                .dividing_line
                .p1,
            partial_visibility
                .right_char_shadow
                .unwrap()
                .dividing_line
                .p2,
            partial_visibility
                .right_char_shadow
                .unwrap()
                .point_on_half_plane,
        ));
        let string = partial_visibility.to_glyphs().to_clean_string();
        assert!(["🭈🭄", "🭊🭂"].contains(&&*string));
    }

    #[test]
    fn test_observed_bright_spot_in_shadow() {
        let player_square = point2(3, 3);
        let block_square = player_square + STEP_UP_RIGHT * 2;
        let test_square = block_square + STEP_UP;

        let fov_result = field_of_view_from_square(
            player_square,
            SIGHT_RADIUS,
            &SquareSet::from([block_square]),
        );
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

    #[test]
    fn complementary_partial_squares_combine_to_full_visibility() {
        let line = Line::new(point2(0.0, 0.0), point2(1.0, 1.0));
        let p1 = point2(0.0, 1.0);
        let p2 = point2(1.0, 0.0);

        let half_plane_1 = HalfPlane::new(line, p1);
        let half_plane_2 = HalfPlane::new(line, p2);

        let partial_1 = PartialVisibilityOfASquare {
            right_char_shadow: Some(half_plane_1),
            left_char_shadow: Some(half_plane_1),
        };
        let partial_2 = PartialVisibilityOfASquare {
            right_char_shadow: Some(half_plane_2),
            left_char_shadow: Some(half_plane_2),
        };

        let combined_partial = partial_1.combine(&partial_2);
        assert!(combined_partial.is_fully_visible());
    }

    #[test]
    fn test_fov_square_sequence() {
        // right, up
        assert_eq!(
            OctantFOVSquareSequenceIter::new(0, STEP_ZERO).next(),
            Some(STEP_RIGHT)
        );
        // up, left
        assert_eq!(
            OctantFOVSquareSequenceIter::new(2, STEP_UP * 2).next(),
            Some(STEP_UP * 2 + STEP_LEFT)
        );
    }
    #[test]
    fn test_fov_square_sequence__detailed() {
        let mut sequence = OctantFOVSquareSequenceIter::new(1, STEP_ZERO);
        let correct_sequence = [
            vec2(0, 1),
            vec2(1, 1),
            vec2(0, 2),
            vec2(1, 2),
            vec2(2, 2),
            vec2(0, 3),
        ];
        assert_eq!(sequence.next_chunk::<6>().unwrap(), correct_sequence);
    }
}
