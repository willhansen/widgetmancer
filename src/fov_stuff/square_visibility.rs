use crate::fov_stuff::square_visibility_from_one_large_shadow::SquareVisibilityFromOneLargeShadow;
use crate::fov_stuff::*;
use crate::glyph::angled_blocks::angle_block_char_complement;
use crate::glyph::glyph_constants::{GREY, RED};
use crate::glyph::DoubleGlyphFunctions;
use crate::graphics::drawable::{
    Drawable, DrawableEnum, PartialVisibilityDrawable, SolidColorDrawable,
};
use crate::utility::*;
use derive_more::Constructor;
use euclid::Angle;
use itertools::Itertools;
use std::collections::HashMap;
use std::fmt::{Debug, Formatter};

// TODO: merge with SquareVisibilityOperations?
pub trait ViewRoundable {
    fn rounded_towards_full_visibility(&self, tolerance_length: f32) -> Self;
}

// TODO: should this be a trait? (yes, because the halfplane square visibility is going to be swapped out, with these functions being common between the two)
pub trait SquareVisibilityOperations: QuarterTurnRotatable + ViewRoundable {
    // visibility checks
    fn is_fully_visible(&self) -> bool;
    fn is_not_visible(&self) -> bool;
    fn is_at_least_partially_visible(&self) -> bool;
    fn is_only_partially_visible(&self) -> bool;
    fn is_nearly_or_fully_visible(&self, tolerance_length: f32) -> bool;
    fn is_just_barely_fully_visible(&self, tolerance_length: f32) -> bool;
    fn point_is_visible(&self, point: impl Into<LocalSquarePoint> + Copy) -> bool; // should return bool with partial for being on edge?

    // creators
    #[deprecated(note = "just use the enum")]
    fn new_fully_visible() -> Self;
    fn new_partially_visible(visible_portion: HalfPlaneCuttingLocalSquare) -> Self;
    fn new_from_visible_half_plane(visible_portion: LocalSquareHalfPlane) -> Self;
    fn new_top_half_visible() -> Self;
    fn new_bottom_half_visible() -> Self;
    fn from_relative_square_and_view_arc(
        view_arc: impl Into<AngleInterval>,
        rel_square: impl Into<WorldStep>,
    ) -> SquareVisibility;

    // other
    fn overlaps(&self, other: Self, tolerance: f32) -> bool;
    fn combined_increasing_visibility(&self, other: &Self) -> Self;
    fn as_string(&self) -> String;
    fn high_res_string(&self, output_diameter: u32) -> String;
    // TODO: add tolerance to these two?
    fn about_equal(&self, other: Self) -> bool;
    fn about_complementary(&self, other: Self) -> bool;
    fn is_visually_complementary_to(&self, other: Self) -> bool;
}

pub trait SquareVisibilityFunctions: QuarterTurnRotatable {
    fn is_fully_visible(&self) -> bool;
    fn from_single_visible_arc(rel_square: WorldStep, visible_arc: AngleInterval) -> Self;
    fn rounded_toward_full_visibility(&self, tolerance: f32) -> Self;
}

// TODO: change to enum with for fully visible, not visible, and partially visible
pub type SquareVisibility = SquareVisibilityFromOneLargeShadow;

#[derive(Clone, Constructor, Debug)]
pub struct PartialSquareVisibilityFromPointSource {
    visible_at_cw_extreme: bool,
    visibility_switch_angles_going_ccw: Vec<Angle<f32>>,
    this_square_from_view_center: WorldStep,
}

impl PartialSquareVisibilityFromPointSource {
    pub fn is_fully_visible(&self) -> bool {
        todo!()
    }
    pub fn from_single_visible_arc(rel_square: WorldStep, visible_arc: AngleInterval) -> Self {
        todo!()
    }
}

impl QuarterTurnRotatable for PartialSquareVisibilityFromPointSource {
    fn quarter_rotated_ccw(&self, quarter_turns_ccw: impl Into<NormalizedOrthoAngle>) -> Self {
        let quarter_turns_ccw = quarter_turns_ccw.into();
        let mut the_clone = self.clone();
        the_clone.visibility_switch_angles_going_ccw = the_clone
            .visibility_switch_angles_going_ccw
            .iter()
            .map(|angle: &Angle<f32>| angle.quarter_rotated_ccw(quarter_turns_ccw))
            .collect_vec();
        the_clone
    }
}

pub type LocalSquareVisibilityMap = HashMap<WorldStep, SquareVisibility>;

pub trait SquareVisibilityMapFunctions: ViewRoundable {
    fn combined_while_increasing_visibility(&self, other: &Self) -> Self;
    fn add_fully_visible_relative_square(&mut self, rel_square: impl Into<WorldStep>);
    fn new_empty() -> Self;
    fn new_with_only_center_visible() -> Self;
}

impl ViewRoundable for LocalSquareVisibilityMap {
    fn rounded_towards_full_visibility(&self, tolerance: f32) -> Self {
        self.iter()
            .map(|(&step, vis)| (step, vis.rounded_towards_full_visibility(tolerance)))
            .collect()
    }
}

impl SquareVisibilityMapFunctions for LocalSquareVisibilityMap {
    fn combined_while_increasing_visibility(&self, other: &Self) -> Self {
        let mut combined_vis_map = self.clone();
        other.iter().for_each(|entry_to_add| {
            let (key, value_to_add) = entry_to_add;
            let mut existing_entry = combined_vis_map.entry(*key);
            match existing_entry {
                std::collections::hash_map::Entry::Occupied(mut e) => {
                    e.insert(e.get().combined_increasing_visibility(value_to_add));
                }
                std::collections::hash_map::Entry::Vacant(e) => {
                    e.insert(*value_to_add);
                }
            }
        });
        combined_vis_map
    }

    fn add_fully_visible_relative_square(&mut self, rel_square: impl Into<WorldStep>) {
        self.insert(rel_square.into(), SquareVisibility::new_fully_visible());
    }
    fn new_empty() -> Self {
        Self::new()
    }
    fn new_with_only_center_visible() -> Self {
        let mut new_thing = Self::new_empty();
        new_thing.add_fully_visible_relative_square((0, 0));
        new_thing
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        glyph::glyph_constants::{FULL_BLOCK, SPACE},
        vec2,
    };
    use ntest::{assert_false, timeout};

    #[test]
    fn test_square_visibility_knows_if_its_fully_visible() {
        let partial = SquareVisibilityFromOneLargeShadow::new_from_visible_half_plane(
            HalfPlane::new_from_line_and_point_on_half_plane(
                TwoDifferentPoints::new(point2(-5.0, 2.0), point2(5.0, 2.2928933)),
                point2(-12.061038, -1.3054879),
            ),
        );
        assert!(partial.is_fully_visible());
    }
    #[test]
    fn test_single_square_is_shadowed_correctly_on_diagonal() {
        let interval = PartialAngleInterval::from_degrees(0.0, 45.0).complement();
        let square_relative_to_center = vec2(1, 1);
        let visibility = SquareVisibility::from_relative_square_and_view_arc(
            interval,
            square_relative_to_center,
        );
        let string = PartialVisibilityDrawable::from_square_visibility(visibility)
            .to_glyphs()
            .to_clean_string();
        assert_eq!(&string, "🭞🭚");
    }
    #[test]
    fn complementary_partial_squares_combine_to_full_visibility() {
        let line = TwoDifferentPoints::new_from_two_unordered_points_on_line(
            point2(0.0, 0.0),
            point2(1.0, 1.0),
        );
        let p1 = point2(0.0, 1.0);
        let p2 = point2(1.0, 0.0);

        let half_plane_1 = HalfPlane::new_from_line_and_point_on_half_plane(line, p1);
        let half_plane_2 = HalfPlane::new_from_line_and_point_on_half_plane(line, p2);
        assert!(half_plane_1.about_complementary(half_plane_2, 1e-6));

        let partial_1 =
            SquareVisibilityFromOneLargeShadow::new_from_visible_half_plane(half_plane_1);
        let partial_2 =
            SquareVisibilityFromOneLargeShadow::new_from_visible_half_plane(half_plane_2);

        let combined_partial = partial_1.combined_increasing_visibility(&partial_2);
        assert!(combined_partial.is_fully_visible());
    }
    #[test]
    fn test_partial_visibility_of_one_square__one_step_up() {
        let arc = PartialAngleInterval::from_degrees(90.0, 135.0);
        let square = WorldStep::new(0, 1);
        let partial = SquareVisibility::from_relative_square_and_view_arc(arc, square);
        assert!(!partial.is_fully_visible());
        assert_eq!(
            PartialVisibilityDrawable::from_square_visibility(partial)
                .to_glyphs()
                .to_clean_string(),
            [FULL_BLOCK, SPACE].into_iter().collect::<String>()
        );
    }
    #[test]
    #[should_panic]
    fn test_one_shadow__should_fail_to_make_not_visible() {
        let non_vis = SquareVisibility::new_partially_visible(
            HalfPlaneCuttingLocalSquare::top_half_plane().extended(-0.6),
        );
    }
    #[test]
    fn test_one_shadow__fully_visible() {
        let vis = SquareVisibility::new_fully_visible();

        assert!(!vis.is_only_partially_visible());
        assert!(vis.is_at_least_partially_visible());
        assert!(vis.is_fully_visible());
    }

    #[test]
    fn test_one_shadow__only_partially_visible() {
        let vis = SquareVisibility::new_partially_visible(
            HalfPlaneCuttingLocalSquare::top_half_plane().extended(0.5 - 1e-3),
        );
        assert!(vis.is_only_partially_visible());
        assert!(vis.is_at_least_partially_visible());
    }
    #[test]
    fn test_one_shadow__diagonal_partially_visible() {
        let line = TwoDifferentPoints::new_from_two_unordered_points_on_line(
            point2(0.0, 0.0),
            point2(1.0, 1.0),
        );
        let p1 = point2(0.0, 1.0);

        let half_plane_1 = HalfPlane::new_from_line_and_point_on_half_plane(line, p1);
        let partial_1 =
            SquareVisibilityFromOneLargeShadow::new_from_visible_half_plane(half_plane_1);
        assert!(partial_1.is_only_partially_visible());
    }
    #[test]
    fn test_one_shadow__almost_fully_visible() {
        let vis = SquareVisibility::new_partially_visible(
            HalfPlaneCuttingLocalSquare::top_half_plane().extended(0.5 - 1e-3),
        );

        assert!(!vis.is_just_barely_fully_visible(0.0));
        assert!(!vis.is_just_barely_fully_visible(1e-4));
        assert!(vis.is_just_barely_fully_visible(1e-2));
    }
    #[test]
    fn test_square_visibility_overlap__simple_non_overlap() {
        let up = SquareVisibility::new_partially_visible(
            HalfPlaneCuttingLocalSquare::new_from_line_and_point_on_half_plane(
                TwoPointsOnDifferentFacesOfCenteredUnitSquare::new_horizontal(0.4),
                (0.0, 1.0),
            ),
        );
        let down = SquareVisibility::PartiallyVisible(
            HalfPlaneCuttingLocalSquare::new_from_line_and_point_on_half_plane(
                TwoPointsOnDifferentFacesOfCenteredUnitSquare::new_horizontal(0.3),
                (0.0, -1.0),
            ),
        );
        assert_false!(up.overlaps(down, 1e-5));
        assert_false!(down.overlaps(up, 1e-5));
        assert_false!(up.overlaps(up.complement(), 1e-5));
    }
    #[test]
    fn test_square_visibility_overlap__simple_overlap() {
        let vis1 = SquareVisibility::new_partially_visible(
            HalfPlaneCuttingLocalSquare::new_from_line_and_point_on_half_plane(
                TwoPointsOnDifferentFacesOfCenteredUnitSquare::new_horizontal(-0.3),
                (0.0, 1.0),
            ),
        );
        let vis2 = SquareVisibility::new_partially_visible(
            HalfPlaneCuttingLocalSquare::new_from_line_and_point_on_half_plane(
                TwoPointsOnDifferentFacesOfCenteredUnitSquare::new_horizontal(0.2),
                (0.0, -1.0),
            ),
        );
        assert!(vis1.overlaps(vis2, 1e-5));
        assert!(vis2.overlaps(vis1, 1e-5));
    }
    #[test]
    fn test_view_arc_source_is_not_visible_by_default() {
        assert!(SquareVisibility::from_relative_square_and_view_arc(
            AngleInterval::from_degrees(0.0, 45.0),
            (0, 0)
        )
        .is_not_visible());
    }
    #[test]
    fn test_view_arc_source_is_not_visible_by_default__full_circle() {
        let arc =
            SquareVisibility::from_relative_square_and_view_arc(AngleInterval::FullCircle, (0, 0));
        assert!(arc.is_not_visible());
    }
    #[test]
    fn test_debug_draw_arc() {
        let arc =
            SquareVisibility::from_relative_square_and_view_arc(AngleInterval::FullCircle, (0, 0));
        dbg!(arc); // NOTE: keep this debug statement because it's part of the test
    }
    // TODO: find easier ways to generally get vectors pointing in cardinal directions with any type and unit
    #[test]
    fn test_square_visibility__if_visible_should_have_intersections_with_unit_square() {
        let hp =
            LocalSquareHalfPlane::new_from_normal_vector_going_from_origin_to_inside_edge_of_border((0.5, 0.5)).extended(-0.1);
        let vis = SquareVisibility::new_from_visible_half_plane(hp);
        let unit_square_intersections = vis.where_border_touches_unit_square();
        assert!(vis.is_nearly_or_fully_visible(0.15));
        assert!(vis.is_only_partially_visible());
        assert_eq!(unit_square_intersections.len(), 2);
    }
}
