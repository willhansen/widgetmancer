use crate::glyph::angled_blocks::angle_block_char_complement;
use crate::glyph::glyph_constants::{GREY, RED};
use crate::glyph::DoubleGlyphFunctions;
use crate::graphics::drawable::{
    Drawable, DrawableEnum, PartialVisibilityDrawable, SolidColorDrawable,
};
use crate::utility::*;
use crate::{point2, DirectedFloatLineTrait};
use derive_more::Constructor;
use euclid::Angle;
use itertools::Itertools;
use std::collections::HashMap;
use std::fmt::{Debug, Formatter};

pub trait ViewRoundable {
    fn rounded_towards_full_visibility(&self, tolerance_length: f32) -> Self;
}

// TODO: should this be a trait? (yes, because the halfplane square visibility is going to be swapped out, with these functions being common between the two)
pub trait RelativeSquareVisibilityFunctions: QuarterTurnRotatable + ViewRoundable {
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
    fn new_partially_visible(visible_portion: LocalSquareHalfPlane) -> Self;
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
    // TODO: add tolerance to these two?
    fn about_equal(&self, other: Self) -> bool;
    fn about_complementary(&self, other: Self) -> bool;
    fn is_visually_complementary_to(&self, other: Self) -> bool;
}

#[derive(PartialEq, Clone, Copy)]
pub enum SquareVisibilityFromOneLargeShadow {
    FullyVisible,
    // TODO: have more than one half plane (two?)
    PartiallyVisible(LocalSquareHalfPlane),
    // PartiallyVisible(LocalSquareHalfPlaneWithBorderOnUnitSquare), // TODO
    NotVisible,
}

impl SquareVisibilityFromOneLargeShadow {
    pub(crate) fn visible_portion(&self) -> Option<LocalSquareHalfPlane> {
        match self {
            Self::PartiallyVisible(v) => Some(*v),
            _ => None,
        }
    }
    pub fn complement(&self) -> Self {
        match self {
            SquareVisibilityFromOneLargeShadow::FullyVisible => Self::NotVisible,
            SquareVisibilityFromOneLargeShadow::PartiallyVisible(v) => {
                Self::PartiallyVisible(v.complement())
            }
            SquareVisibilityFromOneLargeShadow::NotVisible => Self::FullyVisible,
        }
    }
    pub fn where_border_touches_unit_square(&self) -> Vec<LocalSquarePoint> {
        if self.is_fully_visible() {
            return vec![];
        }
        self.visible_portion()
            .unwrap()
            .dividing_line()
            .ordered_line_intersections_with_centered_unit_square()
    }

    pub fn new_orthogonal_half_visible(which_half_visible: impl Into<OrthogonalDirection>) -> Self {
        Self::half_visible((-which_half_visible.into()).angle().into())
    }

    fn half_visible(mut shadow_direction: Angle<f32>) -> Self {
        // todo: may be backwards
        shadow_direction = standardize_angle(shadow_direction);
        Self::new_partially_visible(HalfPlane::new_from_line_and_point_on_half_plane(
            TwoDifferentPoints::<LocalSquarePoint>::new_from_two_points(
                point2(0.0, 0.0),
                LocalSquarePoint::unit_vector_from_angle(shadow_direction)
                    .quarter_rotated_ccw(1)
                    .cast_unit(),
            ),
            LocalSquarePoint::unit_vector_from_angle(shadow_direction).cast_unit(),
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
        matches!(self, Self::FullyVisible)
    }
    fn is_not_visible(&self) -> bool {
        matches!(self, Self::NotVisible)
    }

    fn is_at_least_partially_visible(&self) -> bool {
        !self.is_not_visible()
    }
    fn is_only_partially_visible(&self) -> bool {
        matches!(self, Self::PartiallyVisible(_))
    }
    fn is_nearly_or_fully_visible(&self, tolerance: f32) -> bool {
        self.is_fully_visible() || self.is_just_barely_fully_visible(tolerance)
    }
    fn is_just_barely_fully_visible(&self, tolerance: f32) -> bool {
        self.visible_portion()
            .is_some_and(|v: LocalSquareHalfPlane| {
                v.fully_covers_centered_unit_square_with_tolerance(tolerance)
                    .is_partial()
            })
    }
    fn new_fully_visible() -> Self {
        Self::FullyVisible
    }

    fn new_partially_visible(visible_portion: LocalSquareHalfPlane) -> Self {
        assert!(visible_portion.at_least_partially_covers_unit_square());
        assert!(visible_portion
            .fully_covers_centered_unit_square()
            .is_false());
        SquareVisibilityFromOneLargeShadow::PartiallyVisible(visible_portion)
    }

    fn new_from_visible_half_plane(visible_portion: LocalSquareHalfPlane) -> Self {
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
    ) -> Self {
        let rel_square: WorldStep = rel_square.into(); // TODO: tired of writing this out a bunch
        let angle_tolerance = FAngle::degrees(0.01); // TODO: double check this, maybe standardize
        let length_tolerance = rel_square.to_f32().length() * angle_tolerance.radians;
        let partial_view_arc = match view_arc.into() {
            AngleInterval::Empty => return Self::NotVisible,
            AngleInterval::FullCircle => return Self::FullyVisible,
            AngleInterval::PartialArc(partial) => partial,
        };
        if rel_square == STEP_ZERO {
            // return Some(SquareVisibility::new_fully_visible());
            return Self::NotVisible;
        }
        let square_arc = PartialAngleInterval::from_relative_square(rel_square);
        if partial_view_arc
            .contains_partial_arc(square_arc, angle_tolerance)
            .is_at_least_partial()
        {
            Self::FullyVisible
        } else if partial_view_arc
            .overlaps_partial_arc(square_arc, angle_tolerance)
            .is_at_least_partial()
        {
            // TODO: double check tolerance choice on this "if"

            let shadow_arc = partial_view_arc.complement();
            let overlapped_shadow_edge = shadow_arc.most_overlapped_edge_of_self(square_arc);

            let shadow_line_from_center: TwoDifferentWorldPoints =
                TwoDifferentPoints::new_from_two_points(
                    point2(0.0, 0.0),
                    WorldPoint::unit_vector_from_angle(overlapped_shadow_edge.angle()).cast_unit(),
                );
            let point_in_shadow: WorldPoint =
                WorldPoint::unit_vector_from_angle(shadow_arc.center_angle()).cast_unit();

            let shadow_half_plane = HalfPlane::new_from_line_and_point_on_half_plane(
                shadow_line_from_center,
                point_in_shadow,
            );
            let square_shadow =
                world_half_plane_to_local_square_half_plane(shadow_half_plane, rel_square);

            let shadow_coverage_of_unit_square =
                square_shadow.coverage_of_centered_unit_square_with_tolerance(length_tolerance);

            match shadow_coverage_of_unit_square {
                RelativeIntervalLocation::MORE_THAN_FULL
                | RelativeIntervalLocation::EXACTLY_FULL => Self::NotVisible,
                RelativeIntervalLocation::PARTIALLY_FULL => {
                    Self::PartiallyVisible(square_shadow.complement().try_into().unwrap())
                }
                RelativeIntervalLocation::EXACTLY_EMPTY
                | RelativeIntervalLocation::LESS_THAN_EMPTY => Self::FullyVisible,
            }
        } else {
            Self::NotVisible
        }
    }

    fn overlaps(&self, other: Self, tolerance: f32) -> bool {
        match self {
            SquareVisibilityFromOneLargeShadow::FullyVisible => match other {
                SquareVisibilityFromOneLargeShadow::NotVisible => false,
                _ => true,
            },
            SquareVisibilityFromOneLargeShadow::PartiallyVisible(v1) => match other {
                SquareVisibilityFromOneLargeShadow::FullyVisible => true,
                SquareVisibilityFromOneLargeShadow::PartiallyVisible(v2) => v1
                    .overlaps_other_inside_centered_unit_square_with_tolerance(&v2, tolerance)
                    .is_true(),

                SquareVisibilityFromOneLargeShadow::NotVisible => false,
            },
            SquareVisibilityFromOneLargeShadow::NotVisible => false,
        }
    }
    fn combined_increasing_visibility(&self, other: &Self) -> Self {
        match self {
            SquareVisibilityFromOneLargeShadow::FullyVisible => Self::FullyVisible,
            SquareVisibilityFromOneLargeShadow::PartiallyVisible(v1) => match other {
                SquareVisibilityFromOneLargeShadow::FullyVisible => *self,
                SquareVisibilityFromOneLargeShadow::PartiallyVisible(v2) => {
                    if v1.about_complementary(*v2, 1e-6) {
                        Self::FullyVisible
                    } else {
                        let depth_a = v1.depth_of_point_in_half_plane(point2(0.0, 0.0));
                        let depth_b = v2.depth_of_point_in_half_plane(point2(0.0, 0.0));

                        if depth_a > depth_b {
                            *self
                        } else {
                            *other
                        }
                    }
                }
                SquareVisibilityFromOneLargeShadow::NotVisible => *self,
            },
            SquareVisibilityFromOneLargeShadow::NotVisible => *other,
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

    fn about_equal(&self, other: Self) -> bool {
        match self {
            SquareVisibilityFromOneLargeShadow::FullyVisible => other.is_fully_visible(),
            SquareVisibilityFromOneLargeShadow::PartiallyVisible(v1) => other
                .visible_portion()
                .is_some_and(|v2| v1.about_equal(v2, 1e-6)), // TODO: standardize tolerance
            SquareVisibilityFromOneLargeShadow::NotVisible => other.is_not_visible(),
        }
    }
    fn about_complementary(&self, other: Self) -> bool {
        self.complement().about_equal(other)
    }

    fn is_visually_complementary_to(&self, other: Self) -> bool {
        self.as_string()
            .chars()
            .zip(other.as_string().chars())
            .all(|(c1, c2)| angle_block_char_complement(c1) == c2)
    }

    fn point_is_visible(&self, point: impl Into<LocalSquarePoint> + Copy) -> bool {
        assert!(point_is_in_centered_unit_square_with_tolerance(point, 0.0).is_at_least_partial());
        match self {
            SquareVisibilityFromOneLargeShadow::FullyVisible => true,
            SquareVisibilityFromOneLargeShadow::PartiallyVisible(v) => {
                v.at_least_partially_covers_point(point)
            }
            SquareVisibilityFromOneLargeShadow::NotVisible => false,
        }
    }
}
impl QuarterTurnRotatable for SquareVisibilityFromOneLargeShadow {
    fn quarter_rotated_ccw(&self, quarter_turns_ccw: impl Into<NormalizedOrthoAngle>) -> Self {
        match self {
            SquareVisibilityFromOneLargeShadow::PartiallyVisible(v) => {
                Self::PartiallyVisible(v.quarter_rotated_ccw(quarter_turns_ccw))
            }
            _ => *self,
        }
    }
}

impl Debug for SquareVisibilityFromOneLargeShadow {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            SquareVisibilityFromOneLargeShadow::FullyVisible => write!(f, "Fully Visible"),

            SquareVisibilityFromOneLargeShadow::PartiallyVisible(v) => {
                write!(f, "Partially visible: {:#?}", v)
            }
            SquareVisibilityFromOneLargeShadow::NotVisible => write!(f, "Not Visible"),
        }?;
        write!(
            f,
            "\n\
             \tchars: '{}'",
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
                TwoDifferentPoints::new_from_two_points(point2(-5.0, 2.0), point2(5.0, 2.2928933)),
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
        let line = TwoDifferentPoints::new_from_two_points(point2(0.0, 0.0), point2(1.0, 1.0));
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
            LocalSquareHalfPlane::new_from_line_and_point_on_half_plane(
                TwoDifferentPoints::new_horizontal(0.4),
                (0.0, 1.0),
            ),
        );
        let vis2 = SquareVisibility::new_partially_visible(
            LocalSquareHalfPlane::new_from_line_and_point_on_half_plane(
                TwoDifferentPoints::new_horizontal(0.3),
                (0.0, -1.0),
            ),
        );
        assert_false!(vis1.overlaps(vis2, 1e-5));
        assert_false!(vis2.overlaps(vis1, 1e-5));
        assert_false!(vis1.overlaps(vis1.complement(), 1e-5));
    }
    #[test]
    fn test_square_visibility_overlap__simple_overlap() {
        let vis1 = SquareVisibility::new_partially_visible(
            LocalSquareHalfPlane::new_from_line_and_point_on_half_plane(
                TwoDifferentPoints::new_horizontal(-0.3),
                (0.0, 1.0),
            ),
        );
        let vis2 = SquareVisibility::new_partially_visible(
            LocalSquareHalfPlane::new_from_line_and_point_on_half_plane(
                TwoDifferentPoints::new_horizontal(0.2),
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
    // TODO: find easier ways to generally get vectors pointing in cardinal directions with any type and unit
    #[test]
    fn test_square_visibility__if_visible_should_have_intersections_with_unit_square() {
        let hp =
            LocalSquareHalfPlane::new_from_normal_vector_going_from_origin_to_inside_edge_of_border((1.0, 1.0)).extended(-0.1);
        let vis = SquareVisibility::new_from_visible_half_plane(hp);
        let unit_square_intersections = vis.where_border_touches_unit_square();
        assert!(vis.is_nearly_or_fully_visible(0.01));
        assert!(vis.is_only_partially_visible());
        assert!(vis.where_border_touches_unit_square().len() > 0);
    }
}
