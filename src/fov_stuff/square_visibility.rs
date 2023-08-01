use crate::glyph::angled_blocks::angle_block_char_complement;
use crate::glyph::glyph_constants::{GREY, RED};
use crate::glyph::DoubleGlyphFunctions;
use crate::graphics::drawable::{
    Drawable, DrawableEnum, PartialVisibilityDrawable, SolidColorDrawable,
};
use crate::utility::angle_interval::AngleInterval;
use crate::utility::coordinate_frame_conversions::{
    SquareGridInLocalSquareFrame, WorldSquare, WorldStep,
};
use crate::utility::{
    king_distance, number_to_hue_rotation, rotated_n_quarter_turns_counter_clockwise,
    standardize_angle, unit_vector_from_angle, HalfPlane, Line, QuarterTurnRotatable,
    QuarterTurnsAnticlockwise,
};
use derive_more::Constructor;
use euclid::{point2, Angle};
use itertools::Itertools;
use std::collections::HashMap;
use std::fmt::{Debug, Formatter};

pub trait RelativeSquareVisibilityTrait: QuarterTurnRotatable {
    fn is_fully_visible(&self) -> bool;
    fn is_at_least_partially_visible(&self) -> bool;
    fn new_fully_visible() -> Self;
    fn new_partially_visible(visible_portion: LocalSquareHalfPlane) -> Self;
    fn from_visible_half_plane(visible_portion: LocalSquareHalfPlane) -> Option<Box<Self>>;
    fn top_half_visible() -> Self;
    fn bottom_half_visible() -> Self;
    fn combined_increasing_visibility(&self, other: &Self) -> Self;
    fn as_string(&self) -> String;
    fn is_about_complementary_to(&self, other: Self) -> bool;
    fn is_visually_complementary_to(&self, other: Self) -> bool;
}

pub type LocalSquareHalfPlane = HalfPlane<f32, SquareGridInLocalSquareFrame>;

#[derive(Clone, Copy, Constructor)]
pub struct SquareVisibilityFromOneLargeShadow {
    // TODO: have more than one half plane (two?)
    visible_portion: Option<LocalSquareHalfPlane>,
}

impl SquareVisibilityFromOneLargeShadow {
    pub fn is_fully_visible(&self) -> bool {
        self.visible_portion.is_none()
    }
    pub fn is_nearly_but_not_fully_visible(&self, tolerance: f32) -> bool {
        self.visible_portion
            .is_some_and(|v: LocalSquareHalfPlane| v.fully_covers_expanded_unit_square(-tolerance))
    }
    pub fn is_at_least_partially_visible(&self) -> bool {
        self.visible_portion.is_some_and(|visible_half_plane| {
            visible_half_plane.at_least_partially_covers_unit_square()
        })
    }
    pub fn is_nearly_or_fully_visible(&self, tolerance: f32) -> bool {
        self.is_fully_visible() || self.is_nearly_but_not_fully_visible(tolerance)
    }

    pub fn visible_portion(&self) -> Option<LocalSquareHalfPlane> {
        self.visible_portion
    }

    pub fn new_fully_visible() -> Self {
        SquareVisibilityFromOneLargeShadow {
            visible_portion: None,
        }
    }

    pub fn new_partially_visible(visible_portion: LocalSquareHalfPlane) -> Self {
        assert!(visible_portion.at_least_partially_covers_unit_square());
        assert!(!visible_portion.fully_covers_unit_square());
        SquareVisibilityFromOneLargeShadow {
            visible_portion: Some(visible_portion),
        }
    }
    pub fn from_visible_half_plane(visible_portion: LocalSquareHalfPlane) -> Option<Self> {
        if visible_portion.fully_covers_unit_square() {
            Some(Self::new_fully_visible())
        } else if visible_portion.at_least_partially_covers_unit_square() {
            Some(Self::new_partially_visible(visible_portion))
        } else {
            None
        }
    }

    fn half_visible(mut shadow_direction: Angle<f32>) -> Self {
        // todo: may be backwards
        shadow_direction = standardize_angle(shadow_direction);
        Self::new_partially_visible(HalfPlane::from_line_and_point_on_half_plane(
            Line::new(
                point2(0.0, 0.0),
                rotated_n_quarter_turns_counter_clockwise(
                    unit_vector_from_angle(shadow_direction),
                    1,
                )
                .to_point()
                .cast_unit(),
            ),
            unit_vector_from_angle(shadow_direction)
                .to_point()
                .cast_unit(),
        ))
    }
    pub fn top_half_visible() -> Self {
        Self::half_visible(Angle::degrees(270.0))
    }
    pub fn bottom_half_visible() -> Self {
        Self::half_visible(Angle::degrees(90.0))
    }
    pub fn combined_increasing_visibility(&self, other: &Self) -> Self {
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
    pub fn as_string(&self) -> String {
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
    pub fn is_about_complementary_to(&self, other: Self) -> bool {
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

    pub fn is_visually_complementary_to(&self, other: Self) -> bool {
        self.as_string()
            .chars()
            .zip(other.as_string().chars())
            .all(|(c1, c2)| angle_block_char_complement(c1) == c2)
    }
}
impl QuarterTurnRotatable for SquareVisibilityFromOneLargeShadow {
    fn rotated(&self, quarter_turns_anticlockwise: QuarterTurnsAnticlockwise) -> Self {
        Self {
            visible_portion: self
                .visible_portion
                .map(|half_plane| half_plane.rotated(quarter_turns_anticlockwise)),
        }
    }
}

impl Debug for SquareVisibilityFromOneLargeShadow {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{:#?}\n\
             \tchars: {}",
            self.visible_portion,
            self.as_string()
        )
    }
}

pub trait SquareVisibilityFunctions: QuarterTurnRotatable {
    fn is_fully_visible(&self) -> bool;
    fn from_single_visible_arc(rel_square: WorldStep, visible_arc: AngleInterval) -> Self;
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
    fn rotated(&self, quarter_turns_anticlockwise: QuarterTurnsAnticlockwise) -> Self {
        let mut the_clone = self.clone();
        the_clone.visibility_switch_angles_going_ccw = the_clone
            .visibility_switch_angles_going_ccw
            .iter()
            .map(|angle: &Angle<f32>| angle.rotated(quarter_turns_anticlockwise))
            .collect_vec();
        the_clone
    }
}

pub type RelativeSquareVisibilityMap = HashMap<WorldStep, SquareVisibility>;

pub trait SquareVisibilityMapFunctions {
    fn combined_with_while_increasing_visibility(&self, other: &Self) -> Self;
    fn rounded_towards_full_visibility(&self, tolerance: f32) -> Self;

    fn add_fully_visible_absolute_square(&mut self, square: WorldSquare);
    fn add_fully_visible_relative_square(&mut self, rel_square: WorldStep);
}

impl SquareVisibilityMapFunctions for RelativeSquareVisibilityMap {
    fn combined_with_while_increasing_visibility(&self, other: &Self) -> Self {
        todo!()
    }

    fn rounded_towards_full_visibility(&self, tolerance: f32) -> Self {
        todo!();
        // let mut the_clone = self.clone();
        // the_clone.visible_relative_squares_in_main_view_only = the_clone
        //     .visible_relative_squares_in_main_view_only
        //     .into_iter()
        //     .map(|(rel_square, visibility): (WorldStep, SquareVisibility)| {
        //         (
        //             rel_square,
        //             if visibility.is_nearly_or_fully_visible(tolerance) {
        //                 SquareVisibility::new_fully_visible()
        //             } else {
        //                 visibility
        //             },
        //         )
        //     })
        //     .collect();
        // the_clone.transformed_sub_fovs = the_clone
        //     .transformed_sub_fovs
        //     .into_iter()
        //     .map(|sub_fov: FieldOfView| {
        //         sub_fov.with_all_squares_rounded_towards_full_visibility(tolerance)
        //     })
        //     .collect();
        // the_clone
    }

    fn add_fully_visible_absolute_square(&mut self, square: WorldSquare) {
        todo!()
    }

    fn add_fully_visible_relative_square(&mut self, rel_square: WorldStep) {
        todo!()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::fov_stuff::single_shadow_square_visibility_from_one_view_arc;
    use crate::glyph::glyph_constants::{FULL_BLOCK, SPACE};
    use crate::utility::angle_interval::PartialAngleInterval;
    use euclid::vec2;

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
        )
        .unwrap();
        assert!(partial.is_fully_visible());
    }
    #[test]
    fn test_single_square_is_shadowed_correctly_on_diagonal() {
        let interval = PartialAngleInterval::from_degrees(0.0, 45.0).complement();
        let square_relative_to_center = vec2(1, 1);
        let visibility =
            single_shadow_square_visibility_from_one_view_arc(interval, square_relative_to_center);
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

        let partial_1 =
            SquareVisibilityFromOneLargeShadow::from_visible_half_plane(half_plane_1).unwrap();
        let partial_2 =
            SquareVisibilityFromOneLargeShadow::from_visible_half_plane(half_plane_2).unwrap();

        let combined_partial = partial_1.combined_increasing_visibility(&partial_2);
        assert!(combined_partial.is_fully_visible());
    }
    #[test]
    fn test_partial_visibility_of_one_square__one_step_up() {
        let arc = PartialAngleInterval::from_degrees(90.0, 135.0);
        let square = WorldStep::new(0, 1);
        let partial = single_shadow_square_visibility_from_one_view_arc(arc, square);
        assert!(!partial.unwrap().is_fully_visible());
        assert_eq!(
            PartialVisibilityDrawable::from_square_visibility(partial.unwrap())
                .to_glyphs()
                .to_clean_string(),
            [FULL_BLOCK, SPACE].into_iter().collect::<String>()
        );
    }
}
