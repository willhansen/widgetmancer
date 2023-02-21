use std::collections::{HashMap, HashSet};
use std::f32::consts::PI;

use derive_getters::Getters;
use euclid::{point2, vec2, Angle};
use itertools::all;
use ntest::assert_false;
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
    intersection, is_clockwise, line_intersections_with_centered_unit_square,
    line_intersects_with_centered_unit_square, octant_to_outward_and_across_directions,
    rotate_point_around_point, same_side_of_line, set_of_keys, standardize_angle, union,
    unit_vector_from_angle, HalfPlane, Line, WorldLine, STEP_DOWN_LEFT, STEP_DOWN_RIGHT,
    STEP_RIGHT, STEP_UP_LEFT, STEP_UP_RIGHT, STEP_ZERO,
};

#[derive(Clone, PartialEq, Debug, Copy, Getters)]
pub struct PartialVisibilityOfASquare {
    left_char_shadow: Option<CharacterShadow>,
    right_char_shadow: Option<CharacterShadow>,
    tie_break_bias_direction: Angle<f32>,
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
    pub fn new(
        left_char_shadow: Option<CharacterShadow>,
        right_char_shadow: Option<CharacterShadow>,
    ) -> Self {
        PartialVisibilityOfASquare {
            left_char_shadow,
            right_char_shadow,
            tie_break_bias_direction: Angle::degrees(45.0),
        }
    }

    pub fn rebiased(&self, bias_angle: Angle<f32>) -> Self {
        PartialVisibilityOfASquare {
            left_char_shadow: self.left_char_shadow,
            right_char_shadow: self.right_char_shadow,
            tie_break_bias_direction: standardize_angle(bias_angle),
        }
    }

    pub fn fully_visible() -> Self {
        PartialVisibilityOfASquare::new(None, None)
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
                    let angle_char = half_plane_to_angled_block_character(
                        *half_plane,
                        self.tie_break_bias_direction,
                    );
                    Glyph::fg_only(angle_char, OUT_OF_SIGHT_COLOR)
                } else {
                    Glyph::transparent_glyph()
                }
            })
            .collect::<Vec<Glyph>>()
            .try_into()
            .unwrap()
    }

    pub fn combine_while_increasing_visibility(&self, other: &Self) -> Self {
        let complement_tolerance = 1e-6;
        let combined_shadows: Vec<Option<HalfPlane<_, _>>> = (0..2)
            .map(|i| {
                let self_shadow = *self.get(i);
                let other_shadow = *other.get(i);
                if self_shadow.is_none() || other_shadow.is_none() {
                    None
                } else {
                    if self_shadow
                        .unwrap()
                        .is_about_complementary_to(other_shadow.unwrap(), complement_tolerance)
                    {
                        None
                    } else {
                        // TODO: better combination method than taking just one
                        self_shadow
                    }
                }
            })
            .collect();

        PartialVisibilityOfASquare {
            left_char_shadow: combined_shadows[0],
            right_char_shadow: combined_shadows[1],
            tie_break_bias_direction: self.tie_break_bias_direction,
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

        let target = WorldSquare::new(5, 6);

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
                    .combine_while_increasing_visibility(
                        other.partially_visible_squares.get(&square).unwrap(),
                    );
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
        assert!(
            squares_somehow_both_fully_and_partially_visible.is_empty(),
            "{:?}",
            squares_somehow_both_fully_and_partially_visible
        );

        FovResult {
            fully_visible_squares: all_fully_visible,
            partially_visible_squares: all_partials,
        }
    }

    pub fn departialized(&self) -> Self {
        let partial_squares_that_are_actually_fully_visible: SquareSet = self
            .partially_visible_squares
            .iter()
            .filter(|(square, partial)| partial.is_fully_visible())
            .map(|(square, partial)| square)
            .cloned()
            .collect();
        let partial_squares_that_are_actually_not_visible: SquareSet = self
            .partially_visible_squares
            .iter()
            .filter(|(square, partial)| partial.is_fully_non_visible())
            .map(|(square, partial)| square)
            .cloned()
            .collect();

        let mut new_partials = self.partially_visible_squares.clone();
        union(
            &partial_squares_that_are_actually_fully_visible,
            &partial_squares_that_are_actually_not_visible,
        )
        .iter()
        .for_each(|square| {
            new_partials.remove(square);
        });

        let new_visible = union(
            &partial_squares_that_are_actually_fully_visible,
            &self.fully_visible_squares,
        );
        FovResult {
            partially_visible_squares: new_partials,
            fully_visible_squares: new_visible,
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

        if view_arc.fully_contains_interval(view_arc_of_this_square) {
            fov_result.fully_visible_squares.insert(absolute_square);
        } else if view_arc.overlaps_or_touches(view_arc_of_this_square) {
            let partial_visibility_for_square =
                partial_visibility_of_square_from_one_view_arc(view_arc, relative_square);
            fov_result
                .partially_visible_squares
                .insert(absolute_square, partial_visibility_for_square);
        } else {
            continue;
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
                    let sub_arc_fov = field_of_view_within_arc_in_single_octant(
                        sight_blockers,
                        portal_geometry,
                        center_square,
                        radius,
                        octant_number,
                        new_sub_arc,
                        relative_square,
                    );
                    fov_result = fov_result.combine(sub_arc_fov);
                });
            break;
        }

        // TODO: portals
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
    let full_octant_arc = AngleInterval::from_octant(octant_number);
    let mut fov_result = field_of_view_within_arc_in_single_octant(
        sight_blockers,
        portal_geometry,
        center_square,
        radius,
        octant_number,
        full_octant_arc,
        STEP_ZERO,
    );
    fov_result.fully_visible_squares.insert(center_square);
    fov_result
}

pub fn portal_aware_field_of_view_from_square(
    center_square: WorldSquare,
    radius: u32,
    sight_blockers: &HashSet<WorldSquare>,
    portal_geometry: &PortalGeometry,
) -> FovResult {
    (0..8)
        .fold(
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
        .departialized()
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
    unit_vector_from_angle(view_arc.center_angle()).cast_unit()
}

fn partial_visibility_of_square_from_one_view_arc(
    visibility_arc: AngleInterval,
    square_relative_to_center: WorldStep,
) -> PartialVisibilityOfASquare {
    let square_arc = AngleInterval::from_square(square_relative_to_center);
    assert!(visibility_arc.touches_or_overlaps(square_arc));

    let shadow_arc = visibility_arc.complement();
    let overlapped_shadow_edge = shadow_arc.most_overlapped_edge_of_self(square_arc);

    let shadow_line_from_center: WorldLine = Line {
        p1: point2(0.0, 0.0),
        p2: unit_vector_from_angle(*overlapped_shadow_edge.angle())
            .to_point()
            .cast_unit(),
    };
    let point_in_shadow: WorldPoint = unit_vector_from_angle(shadow_arc.center_angle())
        .to_point()
        .cast_unit();

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

    let partial = PartialVisibilityOfASquare::new(left_char_shadow, right_char_shadow)
        .rebiased(overlapped_shadow_edge.direction_to_inside());
    //Angle::radians(visibility_arc.center_angle().radians + PI),

    partial
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

    use crate::glyph::angled_blocks::{
        angle_block_chars_are_horizontally_continuous, angled_block_char_to_snap_points_map,
        angled_block_flip_y, SnapGridPoint,
    };
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
            view_angle.anticlockwise_end().radians,
            correct_start_angle.radians
        );
        assert_about_eq!(
            view_angle.clockwise_end().radians,
            correct_end_angle.radians
        );
    }

    #[test]
    fn test_square_view_angle__diagonalish() {
        let view_angle = angle_interval_of_square(vec2(5, 3));
        let correct_start_angle = angle_from_better_x_axis(WorldMove::new(4.5, 3.5));
        let correct_end_angle = angle_from_better_x_axis(WorldMove::new(5.5, 2.5));

        assert_about_eq!(
            view_angle.anticlockwise_end().radians,
            correct_start_angle.radians
        );
        assert_about_eq!(
            view_angle.clockwise_end().radians,
            correct_end_angle.radians
        );
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

        //dbg!(&fov_result.partially_visible_squares.get(&point2(4, 3)));

        dbg!(set_of_keys(&fov_result.partially_visible_squares));
        assert!(fov_result.partially_visible_squares.is_empty());
        assert!(fov_result.fully_visible_squares.contains(&start_square));
        let square_area = (radius * 2 + 1).pow(2);
        assert_eq!(fov_result.fully_visible_squares.len(), square_area as usize);
    }

    #[test]
    fn test_partially_visible_square_knows_if_its_fully_visible() {
        // Data from failure case
        let partial = PartialVisibilityOfASquare::new(
            Some(HalfPlane {
                dividing_line: Line {
                    p1: point2(2.5, 2.0),
                    p2: point2(1.0857863, 1.2928933),
                },
                point_on_half_plane: point2(1.061038, 1.3054879),
            }),
            Some(HalfPlane {
                dividing_line: Line {
                    p1: point2(1.5, 2.0),
                    p2: point2(0.08578634, 1.2928933),
                },
                point_on_half_plane: point2(0.061038017, 1.3054879),
            }),
        );
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
        let interval = AngleInterval::from_degrees(0.0, 45.0).complement();
        let square_relative_to_center = vec2(1, 1);
        let visibility =
            partial_visibility_of_square_from_one_view_arc(interval, square_relative_to_center);
        let string = visibility.to_glyphs().to_clean_string();
        //dbg!(visibility);
        assert!(["🭈🭄", "🭊🭂"].contains(&&*string));
    }

    #[test]
    fn test_partial_visibility_to_glyphs() {
        let partial_visibility = PartialVisibilityOfASquare::new(
            Some(HalfPlane::new(
                Line {
                    p1: point2(-0.5, -0.5),
                    p2: point2(1.5, 0.5),
                },
                point2(2.0, 0.0),
            )),
            Some(HalfPlane::new(
                Line {
                    p1: point2(-1.5, -0.5),
                    p2: point2(0.5, 0.5),
                },
                point2(2.0, 0.0),
            )),
        );

        let string = partial_visibility.to_glyphs().to_clean_string();
        assert!(["🭈🭄", "🭊🭂"].contains(&&*string));
    }

    #[test]
    fn test_partial_visibility_to_glyphs__data_from_failure() {
        let partial_visibility = PartialVisibilityOfASquare::new(
            Some(HalfPlane {
                dividing_line: Line {
                    p1: point2(-1.5, -1.0),
                    p2: point2(-0.08578646, -0.29289323),
                },
                point_on_half_plane: point2(-0.061038256, -0.30548787),
            }),
            Some(HalfPlane {
                dividing_line: Line {
                    p1: point2(-2.5, -1.0),
                    p2: point2(-1.0857865, -0.29289323),
                },
                point_on_half_plane: point2(-1.0610383, -0.30548787),
            }),
        );
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
        assert!(half_plane_1.is_about_complementary_to(half_plane_2, 1e-6));

        let partial_1 = PartialVisibilityOfASquare::new(Some(half_plane_1), Some(half_plane_1));
        let partial_2 = PartialVisibilityOfASquare::new(Some(half_plane_2), Some(half_plane_2));

        let combined_partial = partial_1.combine_while_increasing_visibility(&partial_2);
        dbg!(
            &partial_1.to_glyphs().to_clean_string(),
            &partial_2.to_glyphs().to_clean_string(),
            &combined_partial.to_glyphs().to_clean_string()
        );
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

    #[test]
    fn test_partial_visibility_of_one_square__one_step_up() {
        let arc = AngleInterval::from_degrees(90.0, 135.0);
        let square = WorldStep::new(0, 1);
        let partial = partial_visibility_of_square_from_one_view_arc(arc, square);
        assert!(!partial.is_fully_visible());
        assert_eq!(
            partial.to_glyphs().to_clean_string(),
            [SPACE, FULL_BLOCK].into_iter().collect::<String>()
        );
    }

    #[test]
    fn test_visibility_near_two_blocks() {
        let mid_square = point2(5, 5);
        let sight_blockers =
            HashSet::from([mid_square + STEP_RIGHT * 4, mid_square + STEP_RIGHT * 5]);
        let fov_result = single_octant_field_of_view(
            &sight_blockers,
            &PortalGeometry::default(),
            mid_square,
            10,
            0,
        );
        let visible_square = mid_square + STEP_RIGHT * 5 + STEP_UP * 2;
        assert!(fov_result
            .fully_visible_squares
            .contains(&(visible_square + STEP_LEFT)));
        assert!(fov_result.fully_visible_squares.contains(&visible_square));
    }

    #[test]
    fn test_no_sharp_corners_in_shadows__mid_square() {
        let player_square = point2(5, 5);
        let block_square = player_square + STEP_DOWN_LEFT;
        let sight_blockers = HashSet::from([block_square]);
        let fov_result = field_of_view_from_square(player_square, 20, &sight_blockers);

        fov_result
            .partially_visible_squares
            .iter()
            .map(
                |(square, partial): (&WorldSquare, &PartialVisibilityOfASquare)| {
                    (square, partial.to_glyphs().to_clean_string())
                },
            )
            .for_each(|(square, char_string): (&WorldSquare, String)| {
                let chars: Vec<char> = char_string.chars().collect();
                assert_eq!(chars.len(), 2);
                assert!(
                    angle_block_chars_are_horizontally_continuous(chars[0], chars[1]),
                    "square: {:?}, chars: {}",
                    square,
                    char_string
                );
            });
    }

    fn assert_shadow_is_horizontally_continuous(partial: PartialVisibilityOfASquare) {
        let chars = partial
            .to_glyphs()
            .to_clean_string()
            .chars()
            .collect::<Vec<char>>();
        assert!(
            angle_block_chars_are_horizontally_continuous(chars[0], chars[1],),
            "chars: {}{}",
            chars[0],
            chars[1]
        );
    }

    fn partial_from_block_and_square(
        block_square: WorldStep,
        shadowed_square: WorldStep,
    ) -> PartialVisibilityOfASquare {
        partial_visibility_of_square_from_one_view_arc(
            AngleInterval::from_square(block_square).complement(),
            shadowed_square,
        )
    }

    #[test]
    fn test_partial_visibility_of_one_square__observed_discontinuity_1() {
        assert_shadow_is_horizontally_continuous(partial_from_block_and_square(
            STEP_DOWN_LEFT,
            vec2(-1, -3),
        ));
    }
    #[test]
    fn test_partial_visibility_of_one_square__observed_discontinuity_2() {
        assert_shadow_is_horizontally_continuous(partial_from_block_and_square(
            STEP_DOWN_RIGHT,
            vec2(9, -3),
        ));
    }
    #[test]
    fn test_partial_visibility_of_one_square__observed_discontinuity_3() {
        assert_shadow_is_horizontally_continuous(partial_from_block_and_square(
            STEP_UP_LEFT,
            vec2(-14, 5),
        ));
    }
    #[test]
    fn test_partial_visibility_of_one_square__observed_discontinuity_4() {
        assert_shadow_is_horizontally_continuous(partial_from_block_and_square(
            STEP_RIGHT * 2,
            vec2(9, 3),
        ));
        assert_shadow_is_horizontally_continuous(partial_from_block_and_square(
            STEP_RIGHT * 2,
            vec2(9, -3),
        ));
    }
    #[test]
    fn test_partial_visibility_of_one_square__observed_random_discontinuity() {
        // highest i observed before failure: 9
        for i in 0..30 {
            dbg!(i);
            assert_shadow_is_horizontally_continuous(partial_from_block_and_square(
                STEP_DOWN_LEFT,
                vec2(-14, -5),
            ));
        }
    }
    #[test]
    fn test_vertical_shadow_symmetry() {
        let block_square = STEP_RIGHT * 3;
        let above_glyphs =
            partial_from_block_and_square(block_square, block_square + STEP_UP).to_glyphs();
        let below_glyphs =
            partial_from_block_and_square(block_square, block_square + STEP_DOWN).to_glyphs();
        assert_eq!(
            above_glyphs[0].character,
            angled_block_flip_y(below_glyphs[0].character)
        );
        assert_eq!(
            above_glyphs[1].character,
            angled_block_flip_y(below_glyphs[1].character)
        );
    }

    // "🭈🭄"
    // "🭏🬽"
    // "🭠🭘"
    // "🭣🭕"

    #[test]
    fn test_shadows_have_concave_bias_for_angled_blocks() {
        let block_shadow_string_tuples = vec![
            (STEP_LEFT, STEP_UP_LEFT, "🭏🬽"),
            (STEP_LEFT, STEP_DOWN_LEFT, "🭠🭘"),
            (STEP_RIGHT, STEP_UP_RIGHT, "🭈🭄"),
            (STEP_RIGHT, STEP_DOWN_RIGHT, "🭣🭕"),
            (STEP_DOWN, STEP_DOWN_LEFT, "🭈🭄"),
            (STEP_DOWN, STEP_DOWN_RIGHT, "🭏🬽"),
            (STEP_UP, STEP_UP_LEFT, "🭣🭕"),
            (STEP_UP, STEP_UP_RIGHT, "🭠🭘"),
            (STEP_LEFT * 2, STEP_LEFT * 2 + STEP_UP, "🬽 "),
            (STEP_LEFT * 2, STEP_LEFT * 2 + STEP_DOWN, "🭘 "),
            (STEP_RIGHT * 2, STEP_RIGHT * 2 + STEP_UP, " 🭈"),
            (STEP_RIGHT * 2, STEP_RIGHT * 2 + STEP_DOWN, " 🭣"),
            (STEP_UP * 2, STEP_UP * 2 + STEP_LEFT, " 🭦"),
            (STEP_UP * 2, STEP_UP * 2 + STEP_RIGHT, "🭛 "),
            (STEP_DOWN * 2, STEP_DOWN * 2 + STEP_LEFT, " 🭋"),
            (STEP_DOWN * 2, STEP_DOWN * 2 + STEP_RIGHT, "🭀 "),
        ];
        block_shadow_string_tuples.into_iter().for_each(
            |(block_square, shadow_square, correct_string)| {
                assert_eq!(
                    partial_from_block_and_square(block_square, shadow_square)
                        .to_glyphs()
                        .to_clean_string(),
                    correct_string,
                    "block_square: {:?}, shadow_square: {:?}, correct_string: {}",
                    block_square,
                    shadow_square,
                    correct_string,
                );
            },
        );
    }
}
