use crate::glyph::angled_blocks::angle_block_char_complement;
use crate::glyph::glyph_constants::{GREY, RED};
use crate::glyph::DoubleGlyphFunctions;
use crate::graphics::drawable::{
    Drawable, DrawableEnum, PartialVisibilityDrawable, SolidColorDrawable,
};
use crate::utility::angle_interval::*;
use crate::utility::coordinate_frame_conversions::*;
use crate::utility::coordinates::{
    better_angle_from_x_axis, Coordinate, FAngle, OrthogonalWorldStep,
};
use crate::utility::general_utility::*;
use crate::utility::halfplane::*;
use crate::utility::partial_angle_interval::PartialAngleInterval;
use crate::utility::{
    king_step_distance, number_to_hue_rotation, standardize_angle, unit_vector_from_angle,
    HalfPlane, Line, QuarterTurnRotatable, QuarterTurnsCcw, WorldLine, STEP_ZERO,
};
use derive_more::Constructor;
use euclid::{point2, Angle};
use itertools::Itertools;
use std::collections::HashMap;
use std::fmt::{Debug, Formatter};

pub trait ViewRoundable {
    fn rounded_towards_full_visibility(&self, tolerance_length: f32) -> Self;
}

// TODO: should this be a trait?
pub trait RelativeSquareVisibilityFunctions: QuarterTurnRotatable + ViewRoundable {
    // visibility checks
    fn is_fully_visible(&self) -> bool;
    fn is_at_least_partially_visible(&self) -> bool;
    fn is_only_partially_visible(&self) -> bool;
    fn is_nearly_or_fully_visible(&self, tolerance_length: f32) -> bool;
    fn is_just_barely_fully_visible(&self, tolerance_length: f32) -> bool;

    // creators
    fn new_fully_visible() -> Self;
    fn new_partially_visible(visible_portion: LocalSquareHalfPlane) -> Self;
    fn from_visible_half_plane(visible_portion: LocalSquareHalfPlane) -> Self;
    fn new_top_half_visible() -> Self;
    fn new_bottom_half_visible() -> Self;
    fn from_relative_square_and_view_arc(
        view_arc: impl Into<AngleInterval>,
        rel_square: impl Into<WorldStep>,
    ) -> Option<SquareVisibility>;

    // other
    fn overlaps(&self, other: Self, tolerance: f32) -> bool;
    fn combined_increasing_visibility(&self, other: &Self) -> Self;
    fn as_string(&self) -> String;
    // TODO: add tolerance to these two?
    fn is_about_equal_to(&self, other: Self) -> bool;
    fn is_about_complementary_to(&self, other: Self) -> bool;
    fn is_visually_complementary_to(&self, other: Self) -> bool;
}

#[derive(PartialEq, Clone, Copy, Constructor)]
pub struct SquareVisibilityFromOneLargeShadow {
    // TODO: have more than one half plane (two?)
    visible_portion: Option<LocalSquareHalfPlane>,
}

impl SquareVisibilityFromOneLargeShadow {
    pub(crate) fn visible_portion(&self) -> Option<LocalSquareHalfPlane> {
        self.visible_portion
    }
    pub fn complement(&self) -> Option<Self> {
        if self.is_fully_visible() {
            None
        } else {
            Some(Self {
                visible_portion: Some(self.visible_portion.unwrap().complement()),
            })
        }
    }
    pub fn where_border_touches_unit_square(&self) -> Vec<LocalSquarePoint> {
        self.visible_portion()
            .unwrap()
            .dividing_line()
            .line_intersections_with_centered_unit_square()
    }

    pub fn new_orthogonal_half_visible(which_half_visible: impl Into<OrthogonalWorldStep>) -> Self {
        Self::half_visible(better_angle_from_x_axis(
            (-which_half_visible.into()).step().to_f32(),
        ))
    }

    fn half_visible(mut shadow_direction: Angle<f32>) -> Self {
        // todo: may be backwards
        shadow_direction = standardize_angle(shadow_direction);
        Self::new_partially_visible(HalfPlane::from_line_and_point_on_half_plane(
            Line::new(
                point2(0.0, 0.0),
                unit_vector_from_angle(shadow_direction)
                    .quarter_rotated_ccw(1)
                    .to_point()
                    .cast_unit(),
            ),
            unit_vector_from_angle(shadow_direction)
                .to_point()
                .cast_unit(),
        ))
    }
}

impl ViewRoundable for SquareVisibilityFromOneLargeShadow {
    fn rounded_towards_full_visibility(&self, tolerance_length: f32) -> Self {
        if self.is_nearly_or_fully_visible(tolerance_length) {
            Self::new_fully_visible()
        } else {
            self.clone()
        }
    }
}

impl RelativeSquareVisibilityFunctions for SquareVisibilityFromOneLargeShadow {
    fn is_fully_visible(&self) -> bool {
        self.visible_portion.is_none()
    }

    fn is_at_least_partially_visible(&self) -> bool {
        self.is_fully_visible() || self.is_only_partially_visible()
    }
    fn is_only_partially_visible(&self) -> bool {
        self.visible_portion.is_some()
    }
    fn is_nearly_or_fully_visible(&self, tolerance: f32) -> bool {
        self.is_fully_visible() || self.is_just_barely_fully_visible(tolerance)
    }
    fn is_just_barely_fully_visible(&self, tolerance: f32) -> bool {
        self.visible_portion.is_some_and(|v: LocalSquareHalfPlane| {
            v.fully_covers_centered_unit_square_with_tolerance(tolerance)
                .is_partial()
        })
    }
    fn new_fully_visible() -> Self {
        SquareVisibilityFromOneLargeShadow {
            visible_portion: None,
        }
    }

    fn new_partially_visible(visible_portion: LocalSquareHalfPlane) -> Self {
        assert!(visible_portion.at_least_partially_covers_unit_square());
        assert!(visible_portion
            .fully_covers_centered_unit_square()
            .is_false());
        SquareVisibilityFromOneLargeShadow {
            visible_portion: Some(visible_portion),
        }
    }

    fn from_visible_half_plane(visible_portion: LocalSquareHalfPlane) -> Self {
        if visible_portion
            .fully_covers_centered_unit_square()
            .is_true()
        {
            Self::new_fully_visible()
        } else if visible_portion.at_least_partially_covers_unit_square() {
            Self::new_partially_visible(visible_portion)
        } else {
            panic!("half plane does not even partially cover the unit square at (0,0)")
        }
    }
    fn new_top_half_visible() -> Self {
        Self::half_visible(Angle::degrees(270.0))
    }

    fn new_bottom_half_visible() -> Self {
        Self::half_visible(Angle::degrees(90.0))
    }
    fn from_relative_square_and_view_arc(
        view_arc: impl Into<AngleInterval>,
        rel_square: impl Into<WorldStep>,
    ) -> Option<Self> {
        let tolerance = FAngle::degrees(0.01); // TODO: double check this
        let rel_square = rel_square.into(); // TODO: tired of writing this out a bunch
        let partial_view_arc = match view_arc.into() {
            AngleInterval::Empty => return None,
            AngleInterval::FullCircle => return Some(SquareVisibility::new_fully_visible()),
            AngleInterval::PartialArc(partial) => partial,
        };
        if rel_square == STEP_ZERO {
            // return Some(SquareVisibility::new_fully_visible());
            return None;
        }
        let square_arc = PartialAngleInterval::from_relative_square(rel_square);
        if partial_view_arc
            .contains_partial_arc(square_arc, tolerance)
            .is_at_least_partial()
        {
            Some(SquareVisibility::new_fully_visible())
        } else if partial_view_arc
            .overlaps_partial_arc(square_arc, tolerance)
            .is_at_least_partial()
        {
            // TODO: double check tolerance choice on this if

            let shadow_arc = partial_view_arc.complement();
            let overlapped_shadow_edge = shadow_arc.most_overlapped_edge_of_self(square_arc);

            let shadow_line_from_center: WorldLine = Line {
                p1: point2(0.0, 0.0),
                p2: unit_vector_from_angle(overlapped_shadow_edge.angle())
                    .to_point()
                    .cast_unit(),
            };
            let point_in_shadow: WorldPoint = unit_vector_from_angle(shadow_arc.center_angle())
                .to_point()
                .cast_unit();

            let shadow_half_plane = HalfPlane::from_line_and_point_on_half_plane(
                shadow_line_from_center,
                point_in_shadow,
            );
            let square_shadow = world_half_plane_to_local_square_half_plane(
                shadow_half_plane,
                rel_square.to_point(),
            );

            if square_shadow
                .fully_covers_centered_unit_square()
                .is_at_least_partial()
            {
                None
            } else if square_shadow.at_least_partially_covers_unit_square() {
                Some(SquareVisibilityFromOneLargeShadow::new_partially_visible(
                    square_shadow.complement(),
                ))
            } else {
                Some(SquareVisibilityFromOneLargeShadow::new_fully_visible())
            }
        } else {
            None
        }
    }
    fn overlaps(&self, other: Self, tolerance: f32) -> bool {
        if self.is_fully_visible() || other.is_fully_visible() {
            return true;
        }
        self.visible_portion
            .unwrap()
            .overlaps_other_inside_centered_unit_square_with_tolerance(
                &other.visible_portion.unwrap(),
                tolerance,
            )
            .is_true()
    }
    fn combined_increasing_visibility(&self, other: &Self) -> Self {
        if self.is_fully_visible() || other.is_fully_visible() {
            Self::new_fully_visible()
        } else if self
            .visible_portion
            .unwrap()
            .is_about_complementary_to(other.visible_portion.unwrap(), 1e-6)
        {
            Self::new_fully_visible()
        } else {
            let depth_a = self
                .visible_portion
                .unwrap()
                .depth_of_point_in_half_plane(point2(0.0, 0.0));
            let depth_b = other
                .visible_portion
                .unwrap()
                .depth_of_point_in_half_plane(point2(0.0, 0.0));

            if depth_a > depth_b {
                self.clone()
            } else {
                other.clone()
            }
        }
    }
    fn as_string(&self) -> String {
        if self.is_fully_visible() {
            "  ".to_string()
        } else {
            let fg_color = GREY;
            PartialVisibilityDrawable::from_shadowed_drawable(
                &SolidColorDrawable::new(fg_color),
                *self,
            )
            .to_glyphs()
            .to_clean_string()
        }
    }

    fn is_about_equal_to(&self, other: Self) -> bool {
        (self.is_fully_visible() && other.is_fully_visible())
            || self
                .complement()
                .is_some_and(|complement| complement.is_about_complementary_to(other))
    }
    fn is_about_complementary_to(&self, other: Self) -> bool {
        return if self.is_fully_visible() {
            !other.is_at_least_partially_visible()
        } else if other.is_fully_visible() {
            !self.is_at_least_partially_visible()
        } else {
            self.visible_portion
                .unwrap()
                .is_about_complementary_to(other.visible_portion.unwrap(), 1e-6)
        };
    }

    fn is_visually_complementary_to(&self, other: Self) -> bool {
        self.as_string()
            .chars()
            .zip(other.as_string().chars())
            .all(|(c1, c2)| angle_block_char_complement(c1) == c2)
    }
}
impl QuarterTurnRotatable for SquareVisibilityFromOneLargeShadow {
    fn quarter_rotated_ccw(&self, quarter_turns_ccw: impl Into<QuarterTurnsCcw> + Copy) -> Self {
        Self {
            visible_portion: self
                .visible_portion
                .map(|half_plane| half_plane.quarter_rotated_ccw(quarter_turns_ccw)),
        }
    }
}

impl Debug for SquareVisibilityFromOneLargeShadow {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{:#?}\n\
             \tchars: '{}'",
            self.visible_portion,
            self.as_string()
        )
    }
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
    fn quarter_rotated_ccw(&self, quarter_turns_ccw: impl Into<QuarterTurnsCcw>) -> Self {
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
    use crate::glyph::glyph_constants::{FULL_BLOCK, SPACE};
    use euclid::vec2;
    use ntest::{assert_false, timeout};

    #[test]
    fn test_square_visibility_knows_if_its_fully_visible() {
        let partial = SquareVisibilityFromOneLargeShadow::from_visible_half_plane(
            HalfPlane::from_line_and_point_on_half_plane(
                Line {
                    p1: point2(-5.0, 2.0),
                    p2: point2(5.0, 2.2928933),
                },
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
        let string = PartialVisibilityDrawable::from_square_visibility(visibility.unwrap())
            .to_glyphs()
            .to_clean_string();
        assert_eq!(&string, "🭞🭚");
    }
    #[test]
    fn complementary_partial_squares_combine_to_full_visibility() {
        let line = Line::new(point2(0.0, 0.0), point2(1.0, 1.0));
        let p1 = point2(0.0, 1.0);
        let p2 = point2(1.0, 0.0);

        let half_plane_1 = HalfPlane::from_line_and_point_on_half_plane(line, p1);
        let half_plane_2 = HalfPlane::from_line_and_point_on_half_plane(line, p2);
        assert!(half_plane_1.is_about_complementary_to(half_plane_2, 1e-6));

        let partial_1 = SquareVisibilityFromOneLargeShadow::from_visible_half_plane(half_plane_1);
        let partial_2 = SquareVisibilityFromOneLargeShadow::from_visible_half_plane(half_plane_2);

        let combined_partial = partial_1.combined_increasing_visibility(&partial_2);
        assert!(combined_partial.is_fully_visible());
    }
    #[test]
    fn test_partial_visibility_of_one_square__one_step_up() {
        let arc = PartialAngleInterval::from_degrees(90.0, 135.0);
        let square = WorldStep::new(0, 1);
        let partial = SquareVisibility::from_relative_square_and_view_arc(arc, square);
        assert!(!partial.unwrap().is_fully_visible());
        assert_eq!(
            PartialVisibilityDrawable::from_square_visibility(partial.unwrap())
                .to_glyphs()
                .to_clean_string(),
            [FULL_BLOCK, SPACE].into_iter().collect::<String>()
        );
    }
    #[test]
    #[should_panic]
    fn test_one_shadow__should_fail_to_make_not_visible() {
        let non_vis = SquareVisibility::new_partially_visible(
            LocalSquareHalfPlane::top_half_plane().extended(-0.6),
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
            LocalSquareHalfPlane::top_half_plane().extended(0.5 - 1e-3),
        );
        assert!(vis.is_only_partially_visible());
        assert!(vis.is_at_least_partially_visible());
    }
    #[test]
    fn test_one_shadow__almost_fully_visible() {
        let vis = SquareVisibility::new_partially_visible(
            LocalSquareHalfPlane::top_half_plane().extended(0.5 - 1e-3),
        );

        assert!(!vis.is_just_barely_fully_visible(0.0));
        assert!(!vis.is_just_barely_fully_visible(1e-4));
        assert!(vis.is_just_barely_fully_visible(1e-2));
    }
    #[test]
    fn test_square_visibility_overlap__simple_non_overlap() {
        let vis1 = SquareVisibility::new_partially_visible(
            LocalSquareHalfPlane::from_line_and_point_on_half_plane(
                Line::new_horizontal(0.4),
                (0.0, 1.0),
            ),
        );
        let vis2 = SquareVisibility::new_partially_visible(
            LocalSquareHalfPlane::from_line_and_point_on_half_plane(
                Line::new_horizontal(0.3),
                (0.0, -1.0),
            ),
        );
        assert_false!(vis1.overlaps(vis2, 1e-5));
        assert_false!(vis2.overlaps(vis1, 1e-5));
        assert_false!(vis1.overlaps(vis1.complement().unwrap(), 1e-5));
    }
    #[test]
    fn test_square_visibility_overlap__simple_overlap() {
        let vis1 = SquareVisibility::new_partially_visible(
            LocalSquareHalfPlane::from_line_and_point_on_half_plane(
                Line::new_horizontal(-0.3),
                (0.0, 1.0),
            ),
        );
        let vis2 = SquareVisibility::new_partially_visible(
            LocalSquareHalfPlane::from_line_and_point_on_half_plane(
                Line::new_horizontal(0.2),
                (0.0, -1.0),
            ),
        );
        assert!(vis1.overlaps(vis2, 1e-5));
        assert!(vis2.overlaps(vis1, 1e-5));
    }
    #[test]
    fn test_view_arc_source_is_not_visible_by_default() {
        assert_eq!(
            SquareVisibility::from_relative_square_and_view_arc(
                AngleInterval::from_degrees(0.0, 45.0),
                (0, 0)
            ),
            None
        );
    }
}
