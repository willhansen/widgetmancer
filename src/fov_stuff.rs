use std::backtrace::Backtrace;
use std::collections::{HashMap, HashSet};
use std::f32::consts::PI;
use std::fmt::{Debug, Formatter};

use derive_more::Constructor;
use euclid::{point2, vec2, Angle};
use getset::CopyGetters;
use itertools::*;
use ntest::assert_false;
use num::abs;
use ordered_float::OrderedFloat;

use crate::glyph::angled_blocks::{
    angle_block_char_complement, half_plane_to_angled_block_character,
};
use crate::glyph::glyph_constants::{
    BLACK, CYAN, DARK_CYAN, FULL_BLOCK, GREY, OUT_OF_SIGHT_COLOR, RED, SPACE, WHITE,
};
use crate::glyph::{DoubleGlyph, DoubleGlyphFunctions, Glyph};
use crate::graphics;
use crate::graphics::drawable::DrawableEnum::SolidColor;
use crate::graphics::drawable::{
    Drawable, DrawableEnum, PartialVisibilityDrawable, SolidColorDrawable, TextDrawable,
};
use crate::piece::MAX_PIECE_RANGE;
use crate::portal_geometry::{Portal, PortalGeometry};
use crate::utility::angle_interval::PartialAngleInterval;
use crate::utility::coordinate_frame_conversions::*;
use crate::utility::*;

const NARROWEST_VIEW_CONE_ALLOWED_IN_DEGREES: f32 = 0.001;

pub trait SquareVisibilityTrait: QuarterTurnRotatable {
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

// TODO: change to enum with for fully visible, not visible, and partially visible
pub type SquareVisibility = SquareVisibilityFromOneLargeShadow;

#[derive(Clone, Constructor, Debug)]
pub struct SquareVisibilityFromPointSource {
    visible_before_first_angle: bool,
    visibility_switch_angles_going_ccw: Vec<Angle<f32>>,
    point_source_from_square_center: WorldMove,
}

impl SquareVisibilityFromPointSource {
    pub fn is_fully_visible(&self) -> bool {
        todo!()
    }
}

impl QuarterTurnRotatable for SquareVisibilityFromPointSource {
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

type StepVisibilityMap = HashMap<WorldStep, SquareVisibility>;

pub trait StepVisibilityMapFunctions {
    fn combined_with_while_increasing_visibility(&self, other: &Self) -> Self;
    fn rounded_towards_full_visibility(&self, tolerance: f32) -> Self;

    fn add_fully_visible_absolute_square(&mut self, square: WorldSquare);
    fn add_fully_visible_relative_square(&mut self, rel_square: WorldStep);
}

impl StepVisibilityMapFunctions for StepVisibilityMap {
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

#[derive(Debug, Clone)]
pub struct PositionedSquareVisibilityInFov {
    square_visibility_in_absolute_frame: SquareVisibility,
    relative_square: WorldStep,
    absolute_square: WorldSquare,
    //step_in_fov_sequence: u32,
    portal_depth: u32,
    portal_rotation: QuarterTurnsAnticlockwise,
}

impl PositionedSquareVisibilityInFov {
    pub fn one_portal_deeper(
        &self,
        forward_rotation_through_portal: QuarterTurnsAnticlockwise,
    ) -> Self {
        PositionedSquareVisibilityInFov {
            portal_depth: self.portal_depth + 1,
            portal_rotation: self.portal_rotation + forward_rotation_through_portal,
            relative_square: rotated_n_quarter_turns_counter_clockwise(
                self.relative_square,
                -forward_rotation_through_portal.quarter_turns(),
            ),
            ..self.clone()
        }
    }
    pub fn square_visibility_in_absolute_frame(&self) -> SquareVisibility {
        self.square_visibility_in_absolute_frame
    }
    pub fn portal_depth(&self) -> u32 {
        self.portal_depth
    }
    pub fn portal_rotation(&self) -> QuarterTurnsAnticlockwise {
        self.portal_rotation
    }
    pub fn absolute_square(&self) -> WorldSquare {
        self.absolute_square
    }
    pub fn relative_square(&self) -> WorldStep {
        self.relative_square
    }
    pub fn new_in_top_view(
        square_visibility: SquareVisibility,
        absolute_square: WorldSquare,
        relative_square: WorldStep,
    ) -> Self {
        PositionedSquareVisibilityInFov {
            square_visibility_in_absolute_frame: square_visibility,
            relative_square,
            absolute_square,
            portal_depth: 0,
            portal_rotation: Default::default(),
        }
    }
    pub fn square_visibility_in_relative_frame(&self) -> SquareVisibility {
        self.square_visibility_in_absolute_frame
            .rotated(-self.portal_rotation)
    }
}

pub type LocalSquareHalfPlane = HalfPlane<f32, SquareGridInLocalSquareFrame>;

#[derive(Debug, Clone, PartialEq)]
pub struct AngleBasedVisibleSegment {
    visible_angle_interval: PartialAngleInterval,
    start_internal_relative_face: Option<RelativeSquareWithOrthogonalDir>,
    end_internal_relative_faces: HashSet<RelativeSquareWithOrthogonalDir>,
}
impl AngleBasedVisibleSegment {
    pub fn validate(&self) {
        if !self.start_face_spans_angle_interval()
            || !self.end_faces_fully_cover_angle_interval_with_no_overlap()
        {
            panic!("INVALID VISIBLE AREA SEGMENT: {:?}", self);
        }
    }
    fn end_faces_fully_cover_angle_interval_with_no_overlap(&self) -> bool {
        let end_faces_going_clockwise =
            self.end_internal_relative_faces
                .iter()
                .sorted_by_key(|face| {
                    OrderedFloat(better_angle_from_x_axis(face.face_center_point()).radians)
                });
        todo!()
    }
    fn start_face_spans_angle_interval(&self) -> bool {
        self.start_internal_relative_face.is_none()
            || self.start_internal_relative_face.is_some_and(
                |face: RelativeSquareWithOrthogonalDir| {
                    !face.line().parallel_directions().iter().any(|&line_angle| {
                        !self
                            .visible_angle_interval
                            .contains_or_touches_angle(line_angle)
                    })
                },
            )
    }
    pub fn from_relative_face(relative_face: impl Into<RelativeSquareWithOrthogonalDir>) -> Self {
        let actual_face = relative_face.into();
        Self {
            visible_angle_interval: PartialAngleInterval::from_relative_square_face(actual_face),
            start_internal_relative_face: None,
            end_internal_relative_faces: HashSet::from([actual_face]),
        }
    }
    pub fn from_relative_square(step: WorldStep) -> Self {
        let faces = faces_away_from_center_at_rel_square(step);
        Self {
            visible_angle_interval: PartialAngleInterval::from_relative_square(step),
            start_internal_relative_face: None,
            end_internal_relative_faces: faces,
        }
    }
    pub fn with_weakly_applied_start_face(
        &self,
        relative_face: RelativeSquareWithOrthogonalDir,
    ) -> Self {
        if self.start_internal_relative_face.is_some() {
            return self.clone();
        }

        let thing = Self {
            start_internal_relative_face: Some(relative_face),
            ..self.clone()
        };
        assert!(thing.start_face_spans_angle_interval());
        thing
    }
    pub fn with_visible_angle_interval(&self, angle_interval: PartialAngleInterval) -> Self {
        Self {
            visible_angle_interval: angle_interval,
            ..self.clone()
        }
    }
    pub fn get_touching_relative_squares(&self) -> StepSet {
        for step in self
            .visible_angle_interval
            .touched_squares_going_outwards_and_ccw()
        {
            todo!()
        }
        todo!()
    }
    pub fn visibility_of_single_square(&self, rel_square: WorldStep) -> SquareVisibility {
        todo!()
    }
    pub fn to_square_visibilities(&self) -> StepVisibilityMap {
        todo!()
    }
}

// TODO: Applying a trait by simply calling the functions on every component member of a struct should be automated.
impl RigidlyTransformable for AngleBasedVisibleSegment {
    fn apply_rigid_transform(&self, tf: RigidTransform) -> Self {
        Self {
            visible_angle_interval: self.visible_angle_interval.apply_rigid_transform(tf),
            start_internal_relative_face: self
                .start_internal_relative_face
                .map(|face| face.apply_rigid_transform(tf)),
            end_internal_relative_faces: self
                .end_internal_relative_faces
                .iter()
                .map(|face| face.apply_rigid_transform(tf))
                .collect(),
        }
    }
}

#[derive(Debug, Clone, Constructor)]
pub struct FieldOfView {
    root_square_with_direction: SquareWithOrthogonalDir,
    visible_segments_in_main_view_only: Vec<AngleBasedVisibleSegment>,
    transformed_sub_fovs: Vec<FieldOfView>,
}

impl FieldOfView {
    pub fn new_empty_fov_with_root(root: SquareWithOrthogonalDir) -> Self {
        FieldOfView {
            root_square_with_direction: root,
            visible_segments_in_main_view_only: Vec::new(),
            transformed_sub_fovs: Vec::new(),
        }
    }
    pub fn new_empty_fov_at(new_center: WorldSquare) -> Self {
        Self::new_empty_fov_with_root(SquareWithOrthogonalDir::from_square_and_step(
            new_center, STEP_UP,
        ))
    }
    pub fn root_square(&self) -> WorldSquare {
        self.root_square_with_direction.square()
    }
    pub fn root_square_with_direction(&self) -> SquareWithOrthogonalDir {
        self.root_square_with_direction
    }
    pub fn view_transform_to(&self, other: &FieldOfView) -> RigidTransform {
        let start = self.root_square_with_direction;
        let end = other.root_square_with_direction;
        RigidTransform::from_start_and_end_poses(start, end)
    }
    pub fn transformed_sub_fovs(&self) -> &Vec<FieldOfView> {
        &self.transformed_sub_fovs
    }
    pub fn add_fully_visible_relative_square(&mut self, step: WorldStep) {
        self.visible_segments_in_main_view_only
            .push(AngleBasedVisibleSegment::from_relative_square(step))
    }
    pub fn add_fully_visible_relative_face(&mut self, face: RelativeSquareWithOrthogonalDir) {
        self.visible_segments_in_main_view_only
            .push(AngleBasedVisibleSegment::from_relative_face(face))
    }
    pub fn fully_visible_relative_squares_in_main_view_only(&self) -> StepSet {
        todo!();
        // self.visible_relative_squares_in_main_view_only
        //     .iter()
        //     .filter(|(square, vis): &(&WorldStep, &SquareVisibility)| vis.is_fully_visible())
        //     .map(|(&square, vis): (&WorldStep, &SquareVisibility)| square)
        //     .collect()
    }
    pub fn fully_visible_relative_squares_including_subviews(&self) -> StepSet {
        let mut all_visible = self.fully_visible_relative_squares_in_main_view_only();

        self.transformed_sub_fovs
            .iter()
            .for_each(|subview: &FieldOfView| {
                let transform_from_subview = subview.view_transform_to(&self);
                let relative_squares_in_sub_frame =
                    subview.fully_visible_relative_squares_in_main_view_only();
                let visible_in_main_frame =
                    transform_from_subview.rotate_steps(&relative_squares_in_sub_frame);
                all_visible = union(&all_visible, &visible_in_main_frame);
            });
        all_visible
    }

    pub fn visible_relative_squares_in_main_view_only(&self) -> StepVisibilityMap {
        self.without_sub_views().rasterized()
    }
    pub fn visible_segments_in_main_view_only(&self) -> &Vec<AngleBasedVisibleSegment> {
        &self.visible_segments_in_main_view_only
    }

    pub fn at_least_partially_visible_relative_squares_including_subviews(&self) -> StepSet {
        let top_view_visible = self.at_least_partially_visible_relative_squares_in_main_view_only();

        let mut all_visible = top_view_visible;

        for sub_view in &self.transformed_sub_fovs {
            let transform_from_sub_view = sub_view.view_transform_to(&self);
            let relative_squares_in_sub_frame =
                sub_view.at_least_partially_visible_relative_squares_in_main_view_only();
            let visible_in_main_frame =
                transform_from_sub_view.rotate_steps(&relative_squares_in_sub_frame);
            visible_in_main_frame.iter().for_each(|&step| {
                all_visible.insert(step);
            });
        }
        all_visible
    }
    pub fn at_least_partially_visible_relative_squares_in_main_view_only(&self) -> StepSet {
        todo!();
        //set_of_keys(&self.visible_relative_squares_in_main_view_only)
    }

    pub fn only_partially_visible_relative_squares_in_main_view_only(&self) -> StepSet {
        todo!();
        // self.visible_relative_squares_in_main_view_only
        //     .iter()
        //     .filter(|(square, vis)| !vis.is_fully_visible())
        //     .map(|(&square, vis)| square)
        //     .collect()
    }
    pub fn visibilities_of_partially_visible_squares_in_main_view_only(
        &self,
    ) -> HashMap<WorldStep, SquareVisibilityFromOneLargeShadow> {
        todo!();
        // self.visible_relative_squares_in_main_view_only
        //     .iter()
        //     .filter(|(square, vis)| !vis.is_fully_visible())
        //     .map(|(square, vis)| (square.clone(), vis.clone()))
        //     .collect()
    }

    fn combined_main_view_only(&self, other: &Self) -> Self {
        assert_eq!(
            self.root_square_with_direction,
            other.root_square_with_direction
        );

        // TODO: combine visible segments where possible

        // let squares_visible_in_only_one_view: StepSet = self
        //     .at_least_partially_visible_relative_squares_in_main_view_only()
        //     .symmetric_difference(
        //         &other.at_least_partially_visible_relative_squares_in_main_view_only(),
        //     )
        //     .copied()
        //     .collect();
        //
        // let visibility_of_squares_only_visible_in_self: StepVisibilityMap = self
        //     .visible_relative_squares_in_main_view_only
        //     .clone()
        //     .into_iter()
        //     .filter(|(square, partial)| squares_visible_in_only_one_view.contains(square))
        //     .collect();
        //
        // let visibility_of_squares_only_visible_in_other: StepVisibilityMap = other
        //     .visible_relative_squares_in_main_view_only
        //     .clone()
        //     .into_iter()
        //     .filter(|(square, partial)| squares_visible_in_only_one_view.contains(square))
        //     .collect();
        //
        // let all_visible_squares: StepSet = self
        //     .at_least_partially_visible_relative_squares_in_main_view_only()
        //     .union(&other.at_least_partially_visible_relative_squares_in_main_view_only())
        //     .copied()
        //     .collect();
        //
        // let squares_visible_in_both_views: StepSet = all_visible_squares
        //     .difference(&squares_visible_in_only_one_view)
        //     .copied()
        //     .collect();
        //
        // let visibility_of_squares_visible_in_both_views: StepVisibilityMap =
        //     squares_visible_in_both_views
        //         .into_iter()
        //         .map(|square| {
        //             let partial_a = self
        //                 .visible_relative_squares_in_main_view_only
        //                 .get(&square)
        //                 .unwrap();
        //             let partial_b = other
        //                 .visible_relative_squares_in_main_view_only
        //                 .get(&square)
        //                 .unwrap();
        //             let combined = partial_a.combined_increasing_visibility(&partial_b);
        //             (square, combined)
        //         })
        //         .collect();
        //
        // let mut all_visibilities: StepVisibilityMap = visibility_of_squares_only_visible_in_self;
        // all_visibilities.extend(visibility_of_squares_only_visible_in_other);
        // all_visibilities.extend(visibility_of_squares_visible_in_both_views);

        FieldOfView {
            root_square_with_direction: self.root_square_with_direction,
            visible_segments_in_main_view_only: concat([
                self.visible_segments_in_main_view_only.clone(),
                other.visible_segments_in_main_view_only.clone(),
            ]),
            transformed_sub_fovs: vec![],
        }
    }

    pub fn with_weakly_applied_start_line(
        &self,
        start_face: RelativeSquareWithOrthogonalDir,
    ) -> Self {
        Self {
            visible_segments_in_main_view_only: self
                .visible_segments_in_main_view_only
                .iter()
                .map(|segment| segment.with_weakly_applied_start_face(start_face))
                .collect(),
            transformed_sub_fovs: self
                .transformed_sub_fovs
                .iter()
                .map(|sub_fov| {
                    let tf_to_sub_fov = self.view_transform_to(sub_fov);
                    // This transform needs to be done because a start line facing up in the main view may need to be represented as facing right(for example) in a sub-view that has gone through a portal.
                    let start_face_in_sub_fov = tf_to_sub_fov.transform_relative_pose(start_face);
                    sub_fov.with_weakly_applied_start_line(start_face_in_sub_fov)
                })
                .collect(),
            ..self.clone()
        }
    }

    fn combined_sub_fovs(
        sub_fovs_1: &Vec<FieldOfView>,
        sub_fovs_2: &Vec<FieldOfView>,
    ) -> Vec<FieldOfView> {
        let mut clone1 = sub_fovs_1.clone();
        let mut clone2 = sub_fovs_2.clone();
        clone1.append(&mut clone2);

        let combined_sub_fovs = clone1;

        let grouped_by_root = combined_sub_fovs
            .into_iter()
            .into_group_map_by(|fov: &FieldOfView| fov.root_square_with_direction);

        let combined_by_root: Vec<FieldOfView> = grouped_by_root
            .into_iter()
            .map(
                |(root, fov_list): (SquareWithOrthogonalDir, Vec<FieldOfView>)| {
                    fov_list.iter().fold(
                        Self::new_empty_fov_with_root(root),
                        |acc: FieldOfView, next_fov: &FieldOfView| acc.combined_with(next_fov),
                    )
                },
            )
            .collect();

        combined_by_root
    }

    pub fn combined_with(&self, other: &Self) -> Self {
        assert_eq!(
            self.root_square_with_direction,
            other.root_square_with_direction
        );

        let mut top_view_combined_fov = self.combined_main_view_only(other);

        top_view_combined_fov.transformed_sub_fovs =
            Self::combined_sub_fovs(&self.transformed_sub_fovs, &other.transformed_sub_fovs);

        top_view_combined_fov
    }

    fn without_sub_views(&self) -> Self {
        FieldOfView {
            transformed_sub_fovs: vec![],
            ..self.clone()
        }
    }

    pub fn can_fully_and_seamlessly_see_relative_square(&self, step: WorldStep) -> bool {
        let visibility = self.visibilities_of_relative_square(step);
        return visibility.len() == 1
            && visibility
                .get(0)
                .unwrap()
                .square_visibility_in_absolute_frame()
                .is_fully_visible();
    }

    pub fn can_see_relative_square(&self, step: WorldStep) -> bool {
        !self.visibilities_of_relative_square(step).is_empty()
    }

    pub fn visibilities_of_absolute_square(
        &self,
        world_square: WorldSquare,
    ) -> Vec<PositionedSquareVisibilityInFov> {
        let rel_square: WorldStep = world_square - self.root_square();
        // Due to portals, this may see the same square multiple times
        let mut visibilities = vec![];
        if let Some(visibility_in_untransformed_view) =
            self.visibility_of_relative_square_in_main_view(rel_square)
        {
            visibilities.push(PositionedSquareVisibilityInFov::new_in_top_view(
                visibility_in_untransformed_view,
                world_square,
                rel_square,
            ));
        }

        self.transformed_sub_fovs
            .iter()
            .for_each(|sub_fov: &FieldOfView| {
                let forward_rotation = self.view_transform_to(sub_fov).rotation();
                let mut sub_visibilities = sub_fov
                    .visibilities_of_absolute_square(world_square)
                    .into_iter()
                    .map(|pos_vis: PositionedSquareVisibilityInFov| {
                        pos_vis.one_portal_deeper(forward_rotation)
                    })
                    .collect_vec();
                visibilities.append(&mut sub_visibilities);
            });
        visibilities
    }
    pub fn can_see_absolute_square(&self, world_square: WorldSquare) -> bool {
        !self
            .visibilities_of_absolute_square(world_square)
            .is_empty()
    }

    pub fn relative_to_absolute_from_main_view_only(
        &self,
        rel_square: WorldStep,
    ) -> Option<WorldSquare> {
        if self
            .visible_relative_squares_in_main_view_only()
            .keys()
            .contains(&rel_square)
        {
            Some(self.root_square() + rel_square)
        } else {
            None
        }
    }

    fn visibility_of_relative_square_in_main_view(
        &self,
        relative_square: WorldStep,
    ) -> Option<SquareVisibility> {
        self.visible_relative_squares_in_main_view_only()
            .get(&relative_square)
            .cloned()
    }

    fn visibilities_of_relative_square_in_one_sub_view(
        &self,
        relative_square: WorldStep,
        sub_view: &FieldOfView,
    ) -> Vec<PositionedSquareVisibilityInFov> {
        let view_transform_to_sub_view = self.view_transform_to(sub_view);

        let rotation_moving_forward_through_portal: QuarterTurnsAnticlockwise =
            view_transform_to_sub_view.rotation();

        let rotated_relative_square = rotated_n_quarter_turns_counter_clockwise(
            relative_square,
            rotation_moving_forward_through_portal.quarter_turns(),
        );

        let visibilities_in_frame_of_sub_view =
            sub_view.visibilities_of_relative_square(rotated_relative_square);

        let visibilities_in_frame_of_main_view = visibilities_in_frame_of_sub_view
            .iter()
            .map(|pos_vis: &PositionedSquareVisibilityInFov| {
                pos_vis.one_portal_deeper(rotation_moving_forward_through_portal)
            })
            .collect_vec();

        visibilities_in_frame_of_main_view
    }

    pub fn visibilities_of_relative_square(
        &self,
        relative_square: WorldStep,
    ) -> Vec<PositionedSquareVisibilityInFov> {
        let mut visibilities: Vec<PositionedSquareVisibilityInFov> = vec![];

        if let Some(top_level_visibility) =
            self.visibility_of_relative_square_in_main_view(relative_square)
        {
            let absolute_square = self.root_square() + relative_square;
            visibilities.push(PositionedSquareVisibilityInFov::new_in_top_view(
                top_level_visibility,
                absolute_square,
                relative_square,
            ));
        }
        let mut sub_view_visibilities: Vec<PositionedSquareVisibilityInFov> = self
            .transformed_sub_fovs
            .iter()
            .map(|sub_fov| {
                self.visibilities_of_relative_square_in_one_sub_view(relative_square, sub_fov)
            })
            .flatten()
            .collect_vec();
        visibilities.append(&mut sub_view_visibilities);

        visibilities
    }

    fn sorted_by_draw_order(
        visibilities: Vec<PositionedSquareVisibilityInFov>,
    ) -> Vec<PositionedSquareVisibilityInFov> {
        // TODO: The sorting here may be insufficient to prevent ambiguity (and thus flashing)
        visibilities
            .into_iter()
            .sorted_by_key(|pos_vis| pos_vis.portal_depth())
            .collect_vec()
    }

    pub fn drawable_at_relative_square(
        &self,
        relative_square: WorldStep,
        maybe_drawable_map: Option<&HashMap<WorldSquare, DrawableEnum>>,
        tint_portals: bool,
        render_portals_with_line_of_sight: bool,
    ) -> Option<DrawableEnum> {
        let visibilities: Vec<PositionedSquareVisibilityInFov> =
            Self::sorted_by_draw_order(if render_portals_with_line_of_sight {
                self.visibilities_of_relative_square(relative_square)
            } else {
                self.visibilities_of_absolute_square(self.root_square() + relative_square)
            });
        let maybe_drawable: Option<DrawableEnum> = visibilities
            .iter()
            .filter(|&vis: &&PositionedSquareVisibilityInFov| {
                if let Some(drawable_map) = maybe_drawable_map {
                    drawable_map.contains_key(&vis.absolute_square())
                } else {
                    true
                }
            })
            .map(|positioned_visibility: &PositionedSquareVisibilityInFov| {
                let mut drawable: DrawableEnum = if let Some(drawable_map) = maybe_drawable_map {
                    drawable_map
                        .get(&positioned_visibility.absolute_square())
                        .unwrap()
                        .rotated(-positioned_visibility.portal_rotation)
                } else {
                    // SolidColorDrawable::new(GREY).to_enum()
                    // SolidColorDrawable::new(number_to_hue_rotation(
                    //     better_angle_from_x_axis(
                    //         (positioned_visibility.absolute_square - self.root_square()).to_f32(),
                    //     )
                    //     .to_degrees(),
                    //     360.0,
                    // ))
                    // .to_enum()
                    SolidColorDrawable::new(number_to_hue_rotation(
                        king_distance(positioned_visibility.absolute_square - self.root_square())
                            as f32,
                        10.0,
                    ))
                    .to_enum()
                };
                if !positioned_visibility
                    .square_visibility_in_absolute_frame()
                    .is_fully_visible()
                {
                    drawable = DrawableEnum::PartialVisibility(
                        PartialVisibilityDrawable::from_shadowed_drawable(
                            &drawable,
                            if render_portals_with_line_of_sight {
                                positioned_visibility.square_visibility_in_relative_frame()
                            } else {
                                positioned_visibility.square_visibility_in_absolute_frame()
                            },
                        ),
                    )
                };
                if tint_portals {
                    drawable = drawable.tinted(
                        RED,
                        //number_to_color(positioned_visibility.portal_depth()),
                        (0.1 * positioned_visibility.portal_depth() as f32).min(1.0),
                    );
                }
                drawable
            })
            .reduce(|bottom, top| top.drawn_over(&bottom));
        maybe_drawable
    }
    pub fn rasterized(&self) -> StepVisibilityMap {
        self.all_view_segments_in_relative_frame()
            .iter()
            .map(|segment: &AngleBasedVisibleSegment| segment.to_square_visibilities())
            .reduce(|a, b| a.combined_with_while_increasing_visibility(&b))
            .unwrap()
    }
    fn all_view_segments_in_relative_frame(&self) -> Vec<AngleBasedVisibleSegment> {
        concat([
            self.visible_segments_in_main_view_only.clone(),
            self.transformed_sub_fovs
                .iter()
                .flat_map(|sub_fov| {
                    self.other_converted_to_this_frame(sub_fov)
                        .all_view_segments_in_relative_frame()
                })
                .collect_vec(),
        ])
    }

    fn other_converted_to_this_frame(&self, other: &Self) -> Self {
        other.apply_rigid_transform(other.view_transform_to(self))
    }
}

impl RigidlyTransformable for FieldOfView {
    fn apply_rigid_transform(&self, tf: RigidTransform) -> Self {
        FieldOfView::new(
            self.root_square_with_direction().apply_rigid_transform(tf),
            self.visible_segments_in_main_view_only()
                .iter()
                .map(|seg| seg.apply_rigid_transform(tf))
                .collect_vec(),
            self.transformed_sub_fovs()
                .iter()
                .map(|seg| seg.apply_rigid_transform(tf))
                .collect_vec(),
        )
    }
}

#[derive(Clone, PartialEq, Eq, Debug, Copy)]
pub struct OctantFOVSquareSequenceIter {
    outward_dir: OrthogonalWorldStep,
    across_dir: OrthogonalWorldStep,
    outward_steps: u32,
    across_steps: u32,
}

impl OctantFOVSquareSequenceIter {
    pub fn new_from_center(octant: Octant) -> Self {
        let (outward_dir, across_dir) = octant.outward_and_across_directions();

        OctantFOVSquareSequenceIter {
            outward_dir,
            across_dir,
            outward_steps: 0,
            across_steps: 0,
        }
    }

    pub fn octant(&self) -> Octant {
        Octant::from_outward_and_across_directions(self.outward_dir, self.across_dir)
    }

    // todo: change to implementation of QuarterTurnRotatable trait
    pub fn rotated(&self, quarter_turns: QuarterTurnsAnticlockwise) -> Self {
        Self {
            outward_dir: self.outward_dir.rotated(quarter_turns),
            across_dir: self.across_dir.rotated(quarter_turns),
            ..self.clone()
        }
    }
}

impl Iterator for OctantFOVSquareSequenceIter {
    type Item = WorldStep;

    fn next(&mut self) -> Option<Self::Item> {
        let relative_square = self.outward_dir.step() * self.outward_steps as i32
            + self.across_dir.step() * self.across_steps as i32;

        self.across_steps += 1;
        if self.across_steps > self.outward_steps {
            self.outward_steps += 1;
            self.across_steps = 0;
        }

        Some(relative_square)
    }
}

pub fn field_of_view_within_arc_in_single_octant(
    sight_blockers: &SquareSet,
    portal_geometry: &PortalGeometry,
    oriented_center_square: SquareWithOrthogonalDir,
    radius: u32,
    view_arc: PartialAngleInterval,
    mut steps_in_octant_iter: OctantFOVSquareSequenceIter,
) -> FieldOfView {
    let mut fov_result = FieldOfView::new_empty_fov_with_root(oriented_center_square);
    let octant = steps_in_octant_iter.octant();

    loop {
        let relative_square = steps_in_octant_iter.next().unwrap();
        let square_is_out_of_range = king_distance(relative_square) > radius;
        if square_is_out_of_range {
            break;
        }

        // in an octant, every square has two possible view-blocking sides (on the far side of the square)
        let face_directions = [
            steps_in_octant_iter.outward_dir,
            steps_in_octant_iter.across_dir,
        ];
        let absolute_square = oriented_center_square.square() + relative_square;

        let mut view_blocking_arc_for_this_square: Option<PartialAngleInterval> = None;

        for face_direction in face_directions {
            let relative_face: RelativeSquareWithOrthogonalDir =
                (relative_square, face_direction).into();
            let absolute_face: SquareWithOrthogonalDir = (
                oriented_center_square.square() + relative_square,
                face_direction,
            )
                .into();

            let face_has_portal = portal_geometry
                .get_portal_by_entrance(absolute_face)
                .is_some();

            let square_blocks_sight = sight_blockers.contains(&absolute_square);

            let face_is_on_edge_of_sight_radius =
                king_distance(relative_face.stepped().square()) > radius;

            let face_blocks_sight =
                face_has_portal || square_blocks_sight || face_is_on_edge_of_sight_radius;

            let view_arc_of_face = PartialAngleInterval::from_relative_square_face(relative_face);

            let face_is_at_least_partially_visible = view_arc_of_face
                .overlaps_other_by_at_least_this_much(
                    view_arc,
                    Angle::degrees(NARROWEST_VIEW_CONE_ALLOWED_IN_DEGREES),
                );

            if !face_is_at_least_partially_visible {
                continue;
            }

            let visible_arc_of_face = view_arc.intersection(view_arc_of_face);

            if !face_blocks_sight {
                continue;
            }

            view_blocking_arc_for_this_square = Some(
                view_blocking_arc_for_this_square
                    .map_or(view_arc_of_face, |old_val| old_val + view_arc_of_face),
            );

            // create a segment ending at this face
            let visible_segment_up_to_relative_face =
                AngleBasedVisibleSegment::from_relative_face(relative_face)
                    .with_visible_angle_interval(visible_arc_of_face);

            fov_result
                .visible_segments_in_main_view_only
                .push(visible_segment_up_to_relative_face);

            // portals are on inside faces of squares, while stepping out of a square, so if a block has a sight blocker and a portal, the sight blocker takes priority
            let face_has_visible_portal = face_has_portal && !square_blocks_sight;
            if !face_has_visible_portal {
                continue;
            }

            // go into applicable portals
            let portal = portal_geometry
                .get_portal_by_entrance(absolute_face)
                .unwrap();

            let transform = portal.get_transform();
            let transformed_center = oriented_center_square.apply_rigid_transform(transform);
            // in a relative view, the portal exit is the same line as the portal entrance
            let relative_portal_exit: RelativeSquareWithOrthogonalDir =
                relative_face.stepped().turned_back();
            let relative_portal_exit_in_sub_fov =
                transform.transform_relative_pose(relative_portal_exit);
            let sub_arc_fov = field_of_view_within_arc_in_single_octant(
                sight_blockers,
                portal_geometry,
                transformed_center,
                radius,
                visible_arc_of_face.rotated_quarter_turns(transform.rotation()),
                steps_in_octant_iter.rotated(transform.rotation()),
            )
            .with_weakly_applied_start_line(relative_portal_exit_in_sub_fov);
            fov_result.transformed_sub_fovs.push(sub_arc_fov);
        }
        if view_blocking_arc_for_this_square.is_none() {
            continue;
        }
        // split arc around the blocked faces
        let view_arcs_on_either_side_of_blocked_faces =
            view_arc - view_blocking_arc_for_this_square.unwrap();

        view_arcs_on_either_side_of_blocked_faces
            .into_iter()
            .filter(|new_sub_arc| {
                new_sub_arc.width().to_degrees() > NARROWEST_VIEW_CONE_ALLOWED_IN_DEGREES
            })
            .for_each(|new_sub_arc| {
                let sub_arc_fov = field_of_view_within_arc_in_single_octant(
                    sight_blockers,
                    portal_geometry,
                    oriented_center_square,
                    radius,
                    new_sub_arc,
                    steps_in_octant_iter,
                );
                fov_result = fov_result.combined_with(&sub_arc_fov);
            });
        break;

        // let maybe_visibility_of_this_square: Option<SquareVisibility> =
        //     if relative_square == STEP_ZERO {
        //         Some(SquareVisibility::new_fully_visible())
        //     } else {
        //         visibility_of_square(view_arc, relative_square)
        //     };

        // if let Some(visibility_of_this_square) = maybe_visibility_of_this_square {
        //     fov_result.add_visible_square(relative_square, visibility_of_this_square);
        // } else {
        //     continue;
        // }
    }
    fov_result
}

pub fn single_octant_field_of_view(
    center_square: WorldSquare,
    radius: u32,
    octant: Octant,
    sight_blockers: &HashSet<WorldSquare>,
    portal_geometry: &PortalGeometry,
) -> FieldOfView {
    //arc.next_relative_square_in_octant_sequence(first_relative_square_in_sequence);
    //let octant: i32 = arc.octant().expect("arc not confined to octant");
    let fov_result = field_of_view_within_arc_in_single_octant(
        sight_blockers,
        portal_geometry,
        SquareWithOrthogonalDir::from_square_and_step(center_square, STEP_UP),
        radius,
        PartialAngleInterval::from_octant(octant),
        OctantFOVSquareSequenceIter::new_from_center(octant),
    );
    fov_result
}

pub fn portal_aware_field_of_view_from_square(
    center_square: WorldSquare,
    radius: u32,
    sight_blockers: &SquareSet,
    portal_geometry: &PortalGeometry,
) -> FieldOfView {
    (0..8).fold(
        FieldOfView::new_empty_fov_at(center_square),
        |fov_result_accumulator: FieldOfView, octant_number: i32| {
            let new_fov_result = single_octant_field_of_view(
                center_square,
                radius,
                Octant::new(octant_number),
                sight_blockers,
                portal_geometry,
            );
            let combined_fov = fov_result_accumulator.combined_with(&new_fov_result);
            combined_fov
        },
    )
}

fn point_in_view_arc(view_arc: PartialAngleInterval) -> WorldMove {
    unit_vector_from_angle(view_arc.center_angle()).cast_unit()
}

fn visibility_of_square(
    view_arc: PartialAngleInterval,
    rel_square: WorldStep,
) -> Option<SquareVisibility> {
    let square_arc = PartialAngleInterval::from_relative_square(rel_square);
    if view_arc.at_least_fully_overlaps(square_arc) {
        Some(SquareVisibility::new_fully_visible())
    } else if view_arc.overlapping_but_not_exactly_touching(square_arc) {
        single_shadow_square_visibility_from_one_view_arc(view_arc, rel_square)
    } else {
        None
    }
}

fn point_source_shadow_square_visibility_from_one_view_arc(
    vis_arc: PartialAngleInterval,
    rel_square: WorldStep,
) -> Option<SquareVisibility> {
    todo!()
}

fn single_shadow_square_visibility_from_one_view_arc(
    visibility_arc: PartialAngleInterval,
    square_relative_to_center: WorldStep,
) -> Option<SquareVisibilityFromOneLargeShadow> {
    let square_arc = PartialAngleInterval::from_relative_square(square_relative_to_center);
    assert!(visibility_arc.touches_or_overlaps(square_arc));

    let shadow_arc = visibility_arc.complement();
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

    let shadow_half_plane =
        HalfPlane::from_line_and_point_on_half_plane(shadow_line_from_center, point_in_shadow);
    let square_shadow = world_half_plane_to_local_square_half_plane(
        shadow_half_plane,
        square_relative_to_center.to_point(),
    );

    if square_shadow.fully_covers_unit_square() {
        None
    } else if square_shadow.at_least_partially_covers_unit_square() {
        Some(SquareVisibilityFromOneLargeShadow::new_partially_visible(
            square_shadow.complement(),
        ))
    } else {
        Some(SquareVisibilityFromOneLargeShadow::new_fully_visible())
    }
}

fn print_fov(fov: &FieldOfView, radius: u32, render_portals_with_line_of_sight: bool) {
    let center_drawable = TextDrawable::new("@@", WHITE, GREY, true);
    let r = radius as i32;
    (-r..=r).for_each(|neg_y| {
        let y = -neg_y;
        (-r..=r).for_each(|x| {
            let rel_square: WorldStep = vec2(x, y);
            let maybe_drawable = fov.drawable_at_relative_square(
                rel_square,
                None,
                true,
                render_portals_with_line_of_sight,
            );
            let to_draw: DrawableEnum = if let Some(drawable) = maybe_drawable {
                drawable
            } else {
                SolidColorDrawable::new(OUT_OF_SIGHT_COLOR).to_enum()
            };

            print!(
                "{}",
                if rel_square == STEP_ZERO {
                    center_drawable.drawn_over(&to_draw).to_enum()
                } else {
                    to_draw
                }
                .to_glyphs()
                .to_string()
            );
        });
        print!("\n");
    });
}

pub fn print_fov_as_relative(fov: &FieldOfView, radius: u32) {
    print_fov(fov, radius, true)
}

pub fn print_fov_as_absolute(fov: &FieldOfView, radius: u32) {
    print_fov(fov, radius, false)
}

#[cfg(test)]
mod tests {
    use euclid::point2;
    use itertools::Itertools;
    use ntest::{assert_about_eq, assert_false};
    use pretty_assertions::{assert_eq, assert_ne};
    use rgb::RGB8;

    use crate::glyph::angled_blocks::{
        angle_block_char_complement, angle_block_chars_are_horizontally_continuous,
        angled_block_char_to_snap_points_map, angled_block_flip_y, SnapGridPoint,
    };
    use crate::glyph::glyph_constants::{FULL_BLOCK, GREEN};
    use crate::glyph::DoubleGlyphFunctions;
    use crate::utility::{
        better_angle_from_x_axis, QuarterTurnsAnticlockwise, SquareWithKingDir,
        SquareWithOrthogonalDir, STEP_DOWN, STEP_LEFT, STEP_UP,
    };

    use super::*;

    const SIGHT_RADIUS: u32 = 16;

    #[test]
    fn test_square_view_angle__horizontal() {
        let view_angle = PartialAngleInterval::from_relative_square(vec2(3, 0));
        let correct_start_angle = better_angle_from_x_axis(WorldMove::new(2.5, 0.5));
        let correct_end_angle = better_angle_from_x_axis(WorldMove::new(2.5, -0.5));

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
        let view_angle = PartialAngleInterval::from_relative_square(vec2(5, 3));
        let correct_start_angle = better_angle_from_x_axis(WorldMove::new(4.5, 3.5));
        let correct_end_angle = better_angle_from_x_axis(WorldMove::new(5.5, 2.5));

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
        let fov_radius = 10;
        let fov_result = portal_aware_field_of_view_from_square(
            start_square,
            fov_radius,
            &SquareSet::default(),
            &PortalGeometry::default(),
        );
        assert!(fov_result
            .only_partially_visible_relative_squares_in_main_view_only()
            .is_empty());
        assert!(fov_result.can_fully_and_seamlessly_see_relative_square(STEP_ZERO));
        let square_area = (fov_radius * 2 + 1).pow(2);
        assert_eq!(
            fov_result
                .fully_visible_relative_squares_in_main_view_only()
                .len(),
            square_area as usize
        );
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

        print_fov_as_relative(&fov_result, 5);
        assert!(fov_result
            .only_partially_visible_relative_squares_in_main_view_only()
            .is_empty());
        assert!(fov_result.can_fully_and_seamlessly_see_relative_square(STEP_ZERO));
        let square_area = (radius * 2 + 1).pow(2);
        assert_eq!(
            fov_result
                .fully_visible_relative_squares_in_main_view_only()
                .len(),
            square_area as usize
        );
    }

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
    fn test_field_of_view_includes_blocks() {
        let start_square = point2(5, 5);
        let block_step = STEP_UP * 2;
        let block_square = start_square + block_step;
        let blocks = SquareSet::from([block_square]);
        let fov_result = portal_aware_field_of_view_from_square(
            start_square,
            5,
            &blocks,
            &PortalGeometry::default(),
        );
        assert!(fov_result.can_fully_and_seamlessly_see_relative_square(block_step));
        assert!(fov_result.can_fully_and_seamlessly_see_relative_square(block_step + STEP_DOWN));
        assert_false!(fov_result.can_fully_and_seamlessly_see_relative_square(block_step + STEP_UP));
    }

    #[test]
    fn test_partial_squares_look_partial() {
        let start_square = point2(5, 5);
        let block_square = start_square + STEP_DOWN * 2;
        let blocks = SquareSet::from([block_square]);
        let fov_result = portal_aware_field_of_view_from_square(
            start_square,
            SIGHT_RADIUS,
            &blocks,
            &PortalGeometry::default(),
        );
        assert!(!fov_result
            .only_partially_visible_relative_squares_in_main_view_only()
            .is_empty());
    }

    #[test]
    fn test_diagonal_shadow_looks_diagonal() {
        let start_square = point2(5, 5);
        let block_square = start_square + STEP_RIGHT;
        let blocks = SquareSet::from([block_square]);
        let fov_result = portal_aware_field_of_view_from_square(
            start_square,
            SIGHT_RADIUS,
            &blocks,
            &PortalGeometry::default(),
        );
        for i in 1..=5 {
            let step = STEP_UP_RIGHT * i;
            let square_visibility = fov_result
                .visible_relative_squares_in_main_view_only()
                .get(&step)
                .unwrap()
                .clone();
            let string = PartialVisibilityDrawable::from_square_visibility(square_visibility)
                .to_glyphs()
                .to_clean_string();
            assert_eq!(&string, "🭞🭚");
        }
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
    fn test_observed_bright_spot_in_shadow() {
        let player_square = point2(3, 3);
        let block_square = player_square + STEP_UP_RIGHT * 2;
        let test_rel_square = (block_square + STEP_UP) - player_square;

        let fov_result = portal_aware_field_of_view_from_square(
            player_square,
            SIGHT_RADIUS,
            &SquareSet::from([block_square]),
            &PortalGeometry::default(),
        );
        let visibility_of_test_square = fov_result
            .visibilities_of_relative_square(test_rel_square)
            .get(0)
            .unwrap()
            .clone();
        assert_eq!(
            PartialVisibilityDrawable::from_square_visibility(
                visibility_of_test_square.square_visibility_in_absolute_frame()
            )
            .to_glyphs()
            .to_clean_string()
            .chars()
            .nth(1)
            .unwrap(),
            SPACE
        );
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
    fn test_fov_square_sequence__detailed() {
        let mut sequence = OctantFOVSquareSequenceIter::new_from_center(Octant::new(1));
        let correct_sequence = [
            vec2(0, 0),
            vec2(0, 1),
            vec2(1, 1),
            vec2(0, 2),
            vec2(1, 2),
            vec2(2, 2),
            vec2(0, 3),
        ];
        assert_eq!(sequence.next_chunk::<7>().unwrap(), correct_sequence);
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

    #[test]
    fn test_visibility_near_two_blocks() {
        let mid_square = point2(5, 5);
        let sight_blockers =
            HashSet::from([mid_square + STEP_RIGHT * 4, mid_square + STEP_RIGHT * 5]);
        let fov_result = single_octant_field_of_view(
            mid_square,
            10,
            Octant::new(0),
            &sight_blockers,
            &PortalGeometry::default(),
        );
        let visible_rel_square = STEP_RIGHT * 5 + STEP_UP * 2;
        assert!(fov_result
            .can_fully_and_seamlessly_see_relative_square((visible_rel_square + STEP_LEFT)));
        assert!(fov_result.can_fully_and_seamlessly_see_relative_square(visible_rel_square));
    }

    #[test]
    fn test_no_sharp_corners_in_shadows__mid_square() {
        let player_square = point2(5, 5);
        let block_square = player_square + STEP_DOWN_LEFT;
        let sight_blockers = HashSet::from([block_square]);
        let fov_result = portal_aware_field_of_view_from_square(
            player_square,
            20,
            &sight_blockers,
            &PortalGeometry::default(),
        );

        fov_result
            .visible_relative_squares_in_main_view_only()
            .iter()
            .filter(|(step, vis)| !vis.is_fully_visible())
            .map(
                |(step, square_vis): (&WorldStep, &SquareVisibilityFromOneLargeShadow)| {
                    (
                        step,
                        PartialVisibilityDrawable::from_square_visibility(*square_vis)
                            .to_glyphs()
                            .to_clean_string(),
                    )
                },
            )
            .for_each(|(step, char_string): (&WorldStep, String)| {
                let chars: Vec<char> = char_string.chars().collect();
                assert_eq!(chars.len(), 2);
                assert!(
                    angle_block_chars_are_horizontally_continuous(chars[0], chars[1]),
                    "square: {:?}, chars: {}",
                    step,
                    char_string
                );
            });
    }

    fn assert_shadow_is_horizontally_continuous(glyphs: DoubleGlyph) {
        let chars = glyphs.to_clean_string().chars().collect::<Vec<char>>();
        assert!(
            angle_block_chars_are_horizontally_continuous(chars[0], chars[1]),
            "chars: {}{}",
            chars[0],
            chars[1]
        );
    }

    fn square_visibility_from_block_and_square(
        block_square: WorldStep,
        shadowed_square: WorldStep,
    ) -> Option<SquareVisibilityFromOneLargeShadow> {
        single_shadow_square_visibility_from_one_view_arc(
            PartialAngleInterval::from_relative_square(block_square).complement(),
            shadowed_square,
        )
    }

    #[test]
    fn test_partial_visibility_of_one_square__observed_discontinuity_1() {
        assert_shadow_is_horizontally_continuous(
            PartialVisibilityDrawable::from_square_visibility(
                square_visibility_from_block_and_square(STEP_DOWN_LEFT, vec2(-1, -3)).unwrap(),
            )
            .to_glyphs(),
        );
    }

    #[test]
    fn test_partial_visibility_of_one_square__observed_discontinuity_2() {
        assert_shadow_is_horizontally_continuous(
            PartialVisibilityDrawable::from_square_visibility(
                square_visibility_from_block_and_square(STEP_DOWN_RIGHT, vec2(9, -3)).unwrap(),
            )
            .to_glyphs(),
        );
    }

    #[test]
    fn test_partial_visibility_of_one_square__observed_discontinuity_3() {
        assert_shadow_is_horizontally_continuous(
            PartialVisibilityDrawable::from_square_visibility(
                square_visibility_from_block_and_square(STEP_UP_LEFT, vec2(-14, 5)).unwrap(),
            )
            .to_glyphs(),
        );
    }

    #[test]
    fn test_partial_visibility_of_one_square__observed_discontinuity_4() {
        assert_shadow_is_horizontally_continuous(
            PartialVisibilityDrawable::from_square_visibility(
                square_visibility_from_block_and_square(STEP_RIGHT * 2, vec2(9, 3)).unwrap(),
            )
            .to_glyphs(),
        );
        assert_shadow_is_horizontally_continuous(
            PartialVisibilityDrawable::from_square_visibility(
                square_visibility_from_block_and_square(STEP_RIGHT * 2, vec2(9, -3)).unwrap(),
            )
            .to_glyphs(),
        );
    }

    #[test]
    fn test_partial_visibility_of_one_square__observed_random_discontinuity() {
        // highest i observed before failure: 9
        for i in 0..30 {
            assert_shadow_is_horizontally_continuous(
                PartialVisibilityDrawable::from_square_visibility(
                    square_visibility_from_block_and_square(STEP_DOWN_LEFT, vec2(-14, -5)).unwrap(),
                )
                .to_glyphs(),
            );
        }
    }

    #[test]
    fn test_vertical_shadow_symmetry() {
        let block_square = STEP_RIGHT * 3;
        let above_glyphs = PartialVisibilityDrawable::from_square_visibility(
            square_visibility_from_block_and_square(block_square, block_square + STEP_UP).unwrap(),
        )
        .to_glyphs();
        let below_glyphs = PartialVisibilityDrawable::from_square_visibility(
            square_visibility_from_block_and_square(block_square, block_square + STEP_DOWN)
                .unwrap(),
        )
        .to_glyphs();
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
            |(block_square, shadow_square, shadow_string)| {
                let complement_string: String = shadow_string
                    .chars()
                    .map(angle_block_char_complement)
                    .collect();
                assert_eq!(
                    PartialVisibilityDrawable::from_square_visibility(
                        square_visibility_from_block_and_square(block_square, shadow_square)
                            .unwrap()
                    )
                    .to_glyphs()
                    .to_clean_string(),
                    complement_string,
                    "block_square: {:?}, shadow_square: {:?}, correct_string: {}",
                    block_square,
                    shadow_square,
                    complement_string,
                );
            },
        );
    }

    #[test]
    fn test_get_mapping_from_fov_result() {
        let center: WorldSquare = point2(5, 5);
        let mut fov_result = FieldOfView::new_empty_fov_at(center);
        let relative_square = vec2(2, 2);
        todo!();
        //fov_result.add_fully_visible_square(relative_square);

        let square_visibility = fov_result.visibilities_of_relative_square(relative_square);
        assert!(square_visibility
            .get(0)
            .unwrap()
            .square_visibility_in_absolute_frame()
            .is_fully_visible());
        assert!(square_visibility
            .get(0)
            .unwrap()
            .square_visibility_in_absolute_frame()
            .visible_portion
            .is_none());
    }

    #[test]
    fn test_really_narrow_fov_through_a_portal() {
        let mut portal_geometry = PortalGeometry::default();
        let center = point2(10, 20);
        let dx_to_portal_square = 3;
        let entrance_square = center + STEP_RIGHT * dx_to_portal_square;
        let exit_square = center + STEP_LEFT * 5;

        portal_geometry.create_portal(
            SquareWithOrthogonalDir::from_square_and_step(entrance_square, STEP_RIGHT),
            SquareWithOrthogonalDir::from_square_and_step(exit_square, STEP_DOWN),
        );

        let clockwise_end_in_degrees = 0.1;
        let view_arc = PartialAngleInterval::from_degrees(
            clockwise_end_in_degrees,
            clockwise_end_in_degrees + NARROWEST_VIEW_CONE_ALLOWED_IN_DEGREES * 2.0,
        );

        let radius = 10;
        let fov_result = field_of_view_within_arc_in_single_octant(
            &Default::default(),
            &portal_geometry,
            SquareWithOrthogonalDir::from_square_and_step(center, STEP_UP),
            radius,
            view_arc,
            OctantFOVSquareSequenceIter::new_from_center(Octant::new(0)),
        );

        let should_be_visible_relative_squares: StepSet =
            (1..=10).into_iter().map(|dx| STEP_RIGHT * dx).collect();
        let should_be_visible_before_portal: SquareSet = (1..=dx_to_portal_square)
            .into_iter()
            .map(|dx| center + STEP_RIGHT * dx)
            .collect();
        let squares_after_portal = radius - (dx_to_portal_square.abs() as u32 + 1);
        let should_be_visible_after_portal: SquareSet = (0..squares_after_portal as i32)
            .into_iter()
            .map(|dy| exit_square + STEP_DOWN * dy)
            .collect();

        assert_eq!(
            fov_result
                .fully_visible_relative_squares_in_main_view_only()
                .len(),
            1
        );
        assert_eq!(
            fov_result
                .fully_visible_relative_squares_including_subviews()
                .len(),
            1
        );
        assert_eq!(
            fov_result
                .at_least_partially_visible_relative_squares_including_subviews()
                .len(),
            radius as usize + 1
        );
        should_be_visible_relative_squares.iter().for_each(|step| {
            assert!(fov_result.can_see_relative_square(*step));
        });
        should_be_visible_before_portal.iter().for_each(|square| {
            assert!(
                fov_result.can_see_absolute_square(*square),
                "square: {}",
                square.to_string()
            );
        });
        should_be_visible_after_portal.iter().for_each(|square| {
            assert!(
                fov_result.can_see_absolute_square(*square),
                "square: {}",
                square.to_string()
            );
        });
    }

    #[test]
    fn test_sub_view_through_portal_has_correct_transform() {
        let mut portal_geometry = PortalGeometry::default();
        let center = point2(-15, 50);
        let portal_entrance =
            SquareWithOrthogonalDir::from_square_and_step(center + STEP_RIGHT, STEP_RIGHT);
        let portal_exit =
            SquareWithOrthogonalDir::from_square_and_step(center + STEP_DOWN_LEFT * 15, STEP_DOWN);
        portal_geometry.create_portal(portal_entrance, portal_exit);

        let fov_result = single_octant_field_of_view(
            center,
            3,
            Octant::new(0),
            &Default::default(),
            &portal_geometry,
        );

        assert_eq!(fov_result.transformed_sub_fovs.len(), 1);
        assert_eq!(
            fov_result.view_transform_to(&fov_result.transformed_sub_fovs[0]),
            RigidTransform::from_start_and_end_poses(portal_entrance, portal_exit.stepped_back())
        );
        assert_eq!(
            fov_result.transformed_sub_fovs[0]
                .root_square_with_direction
                .square(),
            portal_exit.square() + STEP_UP * 2
        );
    }

    #[test]
    fn test_one_octant_with_one_portal() {
        let mut portal_geometry = PortalGeometry::default();
        let center = point2(-15, 50);
        let portal_entrance =
            SquareWithOrthogonalDir::from_square_and_step(center + STEP_RIGHT, STEP_RIGHT);
        let portal_exit =
            SquareWithOrthogonalDir::from_square_and_step(center + STEP_DOWN_LEFT * 15, STEP_DOWN);
        portal_geometry.create_portal(portal_entrance, portal_exit);

        let fov_result = single_octant_field_of_view(
            center,
            3,
            Octant::new(0),
            &Default::default(),
            &portal_geometry,
        );

        assert_eq!(fov_result.transformed_sub_fovs.len(), 1);
        // Not fully visible because only one octant
        assert!(fov_result.can_see_relative_square(STEP_RIGHT * 2));
        assert_false!(fov_result.can_see_absolute_square(portal_entrance.square() + STEP_RIGHT));
        assert!(fov_result.can_see_absolute_square(portal_exit.square()));

        assert_false!(fov_result.transformed_sub_fovs[0]
            .can_fully_and_seamlessly_see_relative_square(STEP_ZERO));
    }

    #[test]
    fn test_sub_fov_view_transform() {
        let sub_center = SquareWithOrthogonalDir::from_square_and_step(point2(1, 0), STEP_RIGHT);
        let mut sub_fov = FieldOfView::new_empty_fov_at(sub_center.square());
        sub_fov.root_square_with_direction = sub_center;

        let main_center = SquareWithOrthogonalDir::from_square_and_step(point2(50, 0), STEP_UP);
        let mut main_fov = FieldOfView::new_empty_fov_at(main_center.square());
        main_fov.root_square_with_direction = main_center;

        let target_square: WorldSquare = point2(1, 4);

        todo!();
        //sub_fov.add_fully_visible_square(target_square - sub_fov.root_square());

        main_fov.transformed_sub_fovs.push(sub_fov);

        let rel_from_main = STEP_LEFT * 4;

        assert!(main_fov.can_fully_and_seamlessly_see_relative_square(rel_from_main));
        assert!(main_fov.can_see_absolute_square(point2(1, 4)));

        assert_eq!(
            main_fov.at_least_partially_visible_relative_squares_including_subviews(),
            StepSet::from([rel_from_main])
        )
    }

    #[test]
    fn test_square_fully_covered_by_face() {
        let view_arc_of_face =
            PartialAngleInterval::from_relative_square_face((STEP_RIGHT, STEP_RIGHT));
        let square = STEP_RIGHT * 2;

        let visibility = visibility_of_square(view_arc_of_face, square);
        assert!(visibility.unwrap().is_fully_visible());
    }

    #[test]
    fn test_square_fully_not_covered_by_adjacent() {
        let view_arc_of_face =
            PartialAngleInterval::from_relative_square_face((STEP_UP_RIGHT, STEP_RIGHT));
        let square = STEP_RIGHT * 2;

        let visibility = visibility_of_square(view_arc_of_face, square);
        assert!(visibility.is_none());
    }

    #[test]
    fn test_square_fully_inside_view_arc__near_edge() {
        let square = vec2(1, -2);
        let arc = PartialAngleInterval::from_radians(-PI / 2.0, -PI / 4.0);
        assert!(visibility_of_square(arc, square)
            .unwrap()
            .is_fully_visible());
    }

    #[test]
    fn test_portal_pose_transform() {
        let entrance = SquareWithOrthogonalDir::from_square_and_step(point2(3, 4), STEP_RIGHT);
        let exit = SquareWithOrthogonalDir::from_square_and_step(point2(50, 70), STEP_DOWN);
        let portal = Portal::new(entrance, exit);

        let transform = portal.get_transform();
        assert_eq!(transform.translation(), vec2(47, 67));
        assert_eq!(transform.rotation(), QuarterTurnsAnticlockwise::new(3));

        let entrance_offset_and_direction_exit_offset_and_direction = vec![
            (STEP_LEFT, STEP_UP, STEP_UP * 2, STEP_RIGHT),
            (STEP_LEFT, STEP_RIGHT, STEP_UP * 2, STEP_DOWN),
            (STEP_ZERO, STEP_RIGHT, STEP_UP, STEP_DOWN),
            (
                STEP_UP + STEP_LEFT * 2,
                STEP_DOWN,
                STEP_RIGHT + STEP_UP * 3,
                STEP_LEFT,
            ),
        ];
        for (
            offset_from_entrance,
            direction_near_entrance,
            offset_from_exit,
            direction_near_exit,
        ) in entrance_offset_and_direction_exit_offset_and_direction
        {
            let actual_center = entrance
                .with_offset(offset_from_entrance)
                .with_direction(direction_near_entrance);
            let virtual_center_at_exit =
                actual_center.apply_rigid_transform(portal.get_transform());
            let correct_center_at_exit = exit
                .with_offset(offset_from_exit)
                .with_direction(direction_near_exit);
            assert_eq!(virtual_center_at_exit, correct_center_at_exit);
        }
    }

    #[test]
    fn test_simple_fov_combination() {
        let main_center = point2(5, 5);
        let mut fov_1 = FieldOfView::new_empty_fov_at(main_center);
        let mut fov_2 = FieldOfView::new_empty_fov_at(main_center);
        fov_1.add_fully_visible_relative_square(STEP_RIGHT);
        fov_2.add_fully_visible_relative_square(STEP_UP);

        let combined = fov_1.combined_with(&fov_2);

        assert_eq!(
            combined
                .fully_visible_relative_squares_including_subviews()
                .len(),
            2
        );
        assert_eq!(
            combined
                .fully_visible_relative_squares_in_main_view_only()
                .len(),
            2
        );
    }
    #[test]
    fn test_combined_fovs_combine_visibility__faces_on_one_square() {
        (0..5).for_each(|dy| {
            let main_step = STEP_LEFT * 7 + STEP_UP * dy;
            let main_center = point2(5, 5);
            let fovs = faces_away_from_center_at_rel_square(main_step)
                .iter()
                .map(|&rel_face| {
                    let mut fov = FieldOfView::new_empty_fov_at(main_center);
                    fov.add_fully_visible_relative_face(rel_face);
                    fov
                })
                .collect_vec();

            let test_step = main_step + STEP_RIGHT;
            let test_square = main_center + test_step;

            fovs.iter().for_each(|fov| {
                assert_false!(fov.visibilities_of_absolute_square(test_square)[0]
                    .square_visibility_in_absolute_frame
                    .is_fully_visible())
            });

            let merged_fov = fovs
                .iter()
                .cloned()
                .reduce(|a, b| a.combined_with(&b))
                .unwrap();

            assert!(merged_fov.visibilities_of_absolute_square(test_square)[0]
                .square_visibility_in_absolute_frame
                .is_fully_visible());
        });
    }

    #[test]
    fn test_combined_fovs_combine_visibility__full_squares() {
        let main_center = point2(5, 5);
        let horizontal_offset = 12;
        let fovs = (-1..=1)
            .map(|dy| {
                let mut fov = FieldOfView::new_empty_fov_at(main_center);
                fov.add_fully_visible_relative_square(vec2(horizontal_offset, dy));
                fov
            })
            .collect_vec();

        let test_step = STEP_RIGHT * (horizontal_offset - 1);
        let test_square = main_center + test_step;

        fovs.iter().for_each(|fov| {
            assert_false!(fov.visibilities_of_absolute_square(test_square)[0]
                .square_visibility_in_absolute_frame
                .is_fully_visible())
        });

        let merged_fov = fovs
            .iter()
            .cloned()
            .reduce(|a, b| a.combined_with(&b))
            .unwrap();

        assert!(merged_fov.visibilities_of_absolute_square(test_square)[0]
            .square_visibility_in_absolute_frame
            .is_fully_visible());
    }

    #[test]
    fn test_sub_fovs_in_combining_fovs_might_also_combine() {
        let main_center = point2(5, 5);
        let other_center = point2(15, 5);

        let mut fov_1 = FieldOfView::new_empty_fov_at(main_center);
        let mut fov_2 = FieldOfView::new_empty_fov_at(main_center);
        let mut sub_fov_1 = FieldOfView::new_empty_fov_at(other_center);
        let mut sub_fov_2 = FieldOfView::new_empty_fov_at(other_center);

        let rel_square = STEP_RIGHT * 3;
        todo!();
        // sub_fov_1
        //     .visible_relative_squares_in_main_view_only
        //     .insert(rel_square, SquareVisibility::top_half_visible());
        // sub_fov_2
        //     .visible_relative_squares_in_main_view_only
        //     .insert(rel_square, SquareVisibility::bottom_half_visible());

        fov_1.transformed_sub_fovs.push(sub_fov_1);
        fov_2.transformed_sub_fovs.push(sub_fov_2);

        let combined_fov = fov_1.combined_with(&fov_2);

        assert_eq!(combined_fov.transformed_sub_fovs.len(), 1);
        assert_eq!(
            combined_fov
                .fully_visible_relative_squares_including_subviews()
                .len(),
            1
        );
        assert_eq!(
            combined_fov
                .at_least_partially_visible_relative_squares_including_subviews()
                .len(),
            1
        );
        assert!(combined_fov.can_fully_and_seamlessly_see_relative_square(rel_square));
    }

    #[test]
    fn test_fov_relative_to_absolute__top_level() {
        let main_center = point2(5, 5);

        let mut fov = FieldOfView::new_empty_fov_at(main_center);

        let rel_square = STEP_DOWN_LEFT * 3;
        let correct_abs_square = main_center + rel_square;

        fov.add_fully_visible_relative_square(rel_square);

        let visibility = fov
            .visibilities_of_relative_square(rel_square)
            .iter()
            .next()
            .unwrap()
            .clone();
        let abs_square = visibility.absolute_square;
        assert_eq!(abs_square, correct_abs_square);
        assert_eq!(
            fov.at_least_partially_visible_relative_squares_including_subviews()
                .len(),
            1
        );
        assert!(visibility
            .square_visibility_in_absolute_frame()
            .is_fully_visible());
    }

    #[test]
    fn test_fov_relative_to_absolute__sub_view_no_rotation() {
        let main_center = point2(5, 5);
        let sub_center = point2(34, -7);

        let mut fov = FieldOfView::new_empty_fov_at(main_center);
        let mut sub_fov = FieldOfView::new_empty_fov_at(sub_center);

        let rel_square = STEP_DOWN_LEFT * 3;
        let abs_square = sub_center + rel_square;

        sub_fov.add_fully_visible_relative_square(rel_square);

        fov.transformed_sub_fovs.push(sub_fov);

        assert_eq!(
            fov.visibilities_of_relative_square(rel_square)
                .iter()
                .next()
                .unwrap()
                .absolute_square,
            abs_square
        );
    }

    #[test]
    fn test_fov_relative_to_absolute__sub_view_with_rotation() {
        let main_center = point2(5, 5);
        let sub_center = point2(34, -7);

        let mut fov = FieldOfView::new_empty_fov_at(main_center);

        let quarter_turns = 3;

        let sub_fov_direction = rotated_n_quarter_turns_counter_clockwise(
            fov.root_square_with_direction.direction().step(),
            quarter_turns,
        );
        let mut sub_fov = FieldOfView::new_empty_fov_with_root(
            SquareWithOrthogonalDir::from_square_and_step(sub_center, sub_fov_direction),
        );

        let rel_square = STEP_DOWN_LEFT * 3;
        let rotated_rel_square =
            rotated_n_quarter_turns_counter_clockwise(rel_square, quarter_turns);
        let abs_square = sub_center + rotated_rel_square;

        sub_fov.add_fully_visible_relative_square(rotated_rel_square);
        fov.transformed_sub_fovs.push(sub_fov);

        assert_eq!(
            fov.visibilities_of_relative_square(rel_square)
                .iter()
                .next()
                .unwrap()
                .absolute_square,
            abs_square
        );
    }

    #[test]
    fn test_partial_visibility_in_blindspot_of_nearly_full_arc() {
        let rel_square = vec2(4, 4);
        // These values are from an observed failure.  NOT ARBITRARY
        let arc = PartialAngleInterval::new_interval(
            Angle::radians(0.7853978),
            Angle::radians(0.7853982),
        );
        let visibility = single_shadow_square_visibility_from_one_view_arc(arc, rel_square);
    }

    #[test]
    fn test_visibility_of_multiple_squares_in_one_square() {
        let main_center = point2(5, 5);
        let other_center = point2(15, 5);

        let mut fov_1 = FieldOfView::new_empty_fov_at(main_center);
        let mut sub_fov_1 = FieldOfView::new_empty_fov_at(other_center);

        todo!();
        let rel_square = STEP_RIGHT * 3;
        // fov_1
        //     .visible_relative_squares_in_main_view_only
        //     .insert(rel_square, SquareVisibility::bottom_half_visible());
        // sub_fov_1
        //     .visible_relative_squares_in_main_view_only
        //     .insert(rel_square, SquareVisibility::top_half_visible());

        fov_1.transformed_sub_fovs.push(sub_fov_1);

        assert_eq!(fov_1.visibilities_of_relative_square(rel_square).len(), 2);
        assert_eq!(fov_1.visibilities_of_relative_square(rel_square).len(), 2);
    }

    #[test]
    fn test_rounding_towards_full_visibility() {
        let mut fov = FieldOfView::new_empty_fov_at(point2(0, 0));
        fov.add_fully_visible_relative_square(STEP_RIGHT);

        todo!();
        // fov.add_visible_square(
        //     STEP_UP,
        //     SquareVisibility::new_partially_visible(
        //         LocalSquareHalfPlane::top_half_plane().extended(0.5 - 1e-2),
        //     ),
        // );
        // fov.add_visible_square(
        //     STEP_DOWN,
        //     SquareVisibility::new_partially_visible(
        //         LocalSquareHalfPlane::top_half_plane().extended(0.5 - 1e-4),
        //     ),
        // );
        assert_eq!(fov.visible_relative_squares_in_main_view_only().len(), 3);
        assert_eq!(
            fov.fully_visible_relative_squares_in_main_view_only().len(),
            1
        );
        assert_eq!(
            fov.only_partially_visible_relative_squares_in_main_view_only()
                .len(),
            2
        );

        // let rounded_fov = fov.rasterized().rounded_towards_full_visibility(1e-3);
        // assert_eq!(
        //     rounded_fov
        //         .visible_relative_squares_in_main_view_only()
        //         .len(),
        //     3
        // );
        // assert_eq!(
        //     rounded_fov
        //         .fully_visible_relative_squares_in_main_view_only()
        //         .len(),
        //     2
        // );
        // assert_eq!(
        //     rounded_fov
        //         .only_partially_visible_relative_squares_in_main_view_only()
        //         .len(),
        //     1
        // );
        // assert!(rounded_fov
        //     .fully_visible_relative_squares_in_main_view_only()
        //     .contains(&STEP_DOWN));
        // assert!(rounded_fov
        //     .only_partially_visible_relative_squares_in_main_view_only()
        //     .contains(&STEP_UP));
    }

    #[test]
    fn test_center_of_fov_is_visible() {
        let square = point2(4, 5);
        let fov = portal_aware_field_of_view_from_square(
            square,
            0,
            &Default::default(),
            &Default::default(),
        );
        assert_eq!(fov.visible_relative_squares_in_main_view_only().len(), 1);
        assert!(fov
            .visible_relative_squares_in_main_view_only()
            .get(&STEP_ZERO)
            .unwrap()
            .is_fully_visible());
    }

    #[ignore = "not a priority for the time being"]
    #[test]
    fn test_adjacent_wall_all_fully_visible() {
        let player_square = point2(5, 5);
        let rel_blocks = (0..10).map(|i| STEP_RIGHT + STEP_UP * i).collect_vec();
        let abs_squares = rel_blocks.iter().map(|v| player_square + *v).collect_vec();
        let blocks = SquareSet::from_iter(abs_squares);
        let fov =
            portal_aware_field_of_view_from_square(player_square, 5, &blocks, &Default::default());

        print_fov_as_relative(&fov, 5);
        rel_blocks.iter().for_each(|rel_block| {
            assert!(
                fov.can_fully_and_seamlessly_see_relative_square(*rel_block),
                "rel_block: {:?}",
                rel_block
            )
        });
    }

    #[test]
    fn test_octant_edge_has_correct_rotation() {
        // let new_fov_result = get_fov_for_first_quadrant_going_through_wide_turning_portal();
        let center = point2(5, 5);
        let new_fov_result = single_octant_field_of_view(
            center,
            1,
            Octant::new(0),
            &Default::default(),
            &Default::default(),
        );
        print_fov_as_relative(&new_fov_result, 2);
        let visibilities_of_one_right = new_fov_result.visibilities_of_relative_square(STEP_RIGHT);
        assert_eq!(visibilities_of_one_right.len(), 1);
        let the_positioned_visibility = visibilities_of_one_right[0].clone();
        assert_eq!(the_positioned_visibility.portal_depth, 0);
        assert_eq!(
            the_positioned_visibility.portal_rotation,
            QuarterTurnsAnticlockwise::new(0)
        );
        let the_square_visibility = the_positioned_visibility.square_visibility_in_absolute_frame;
        assert_about_eq!(
            the_square_visibility
                .visible_portion
                .unwrap()
                .dividing_line()
                .angle_with_positive_x_axis()
                .to_degrees(),
            0.0
        );
        let the_drawable = PartialVisibilityDrawable::from_shadowed_drawable(
            &SolidColorDrawable::new(RED),
            the_square_visibility,
        );
        let the_glyphs = the_drawable.to_glyphs();
        let the_clean_string = the_glyphs.to_clean_string();
        assert_eq!(the_clean_string, "🬎🬎");
    }

    #[test]
    fn test_one_square_seen_through_seam_of_wide_portal_is_fully_visible() {
        let center = point2(5, 5);
        let mut portal_geometry = PortalGeometry::default();

        let entrance_left_end = SquareWithOrthogonalDir::from_square_and_step(center, STEP_RIGHT);
        let portal_step = STEP_RIGHT * 2;
        let exit_left_end = SquareWithOrthogonalDir::from_square_and_step(
            entrance_left_end.square() + STEP_RIGHT + portal_step,
            STEP_RIGHT,
        );

        (0..2).for_each(|i| {
            portal_geometry.create_double_sided_two_way_portal(
                entrance_left_end.strafed_left_n(i),
                exit_left_end.strafed_left_n(i),
            );
        });
        // let new_fov_result = single_octant_field_of_view(
        //     &Default::default(),
        //     &portal_geometry,
        //     center,
        //     4,
        //     Octant::new(0),
        // );
        let new_fov_result = portal_aware_field_of_view_from_square(
            center,
            5,
            &Default::default(),
            &portal_geometry,
        );

        print_fov_as_relative(&new_fov_result, 7);

        assert_eq!(new_fov_result.transformed_sub_fovs.len(), 1);
        let test_square = STEP_UP_RIGHT;
        let visibilities_of_test_square =
            new_fov_result.visibilities_of_relative_square(test_square);
        assert_eq!(visibilities_of_test_square.len(), 1);
        assert!(new_fov_result.can_fully_and_seamlessly_see_relative_square(test_square));
        let the_positioned_visibility = visibilities_of_test_square[0].clone();
        assert_eq!(the_positioned_visibility.portal_depth, 1);
        assert_eq!(
            the_positioned_visibility.portal_rotation,
            QuarterTurnsAnticlockwise::new(0)
        );
        let the_square_visibility = the_positioned_visibility.square_visibility_in_relative_frame();
        assert!(the_square_visibility.is_fully_visible());
    }

    #[test]
    fn test_no_seam_for_wide_rotated_portal() {
        let center = point2(5, 5);
        let mut portal_geometry = PortalGeometry::default();

        let entrance = SquareWithOrthogonalDir::from_square_and_step(
            center + STEP_RIGHT * 2 + STEP_UP,
            STEP_RIGHT,
        );
        let exit = SquareWithOrthogonalDir::from_square_and_step(
            entrance.square() + STEP_UP_LEFT * 50,
            STEP_UP,
        );

        (0..2).for_each(|i| {
            // TODO: why does this need to be double sided AND two way?
            portal_geometry.create_double_sided_two_way_portal(
                entrance.strafed_right_n(i),
                exit.strafed_right_n(i),
            );
        });
        let new_fov_result = single_octant_field_of_view(
            center,
            8,
            Octant::new(0),
            &Default::default(),
            &portal_geometry,
        );
        // let new_fov_result = portal_aware_field_of_view_from_square(
        //     center,
        //     8,
        //     &Default::default(),
        //     &portal_geometry,
        // );

        print_fov_as_relative(&new_fov_result, 10);
        print_fov_as_absolute(&new_fov_result, 10);

        assert_eq!(new_fov_result.transformed_sub_fovs.len(), 1);

        let test_square = STEP_RIGHT * 3;

        let visibilities_of_test_square =
            new_fov_result.visibilities_of_relative_square(test_square);
        assert_eq!(visibilities_of_test_square.len(), 1);
        let the_positioned_visibility = visibilities_of_test_square[0].clone();
        assert_eq!(the_positioned_visibility.portal_depth, 1);
        assert_eq!(
            the_positioned_visibility.portal_rotation,
            QuarterTurnsAnticlockwise::new(1)
        );
        let the_square_visibility = the_positioned_visibility.square_visibility_in_relative_frame();
        assert_false!(the_square_visibility.is_fully_visible());
        let the_drawable = PartialVisibilityDrawable::from_shadowed_drawable(
            &SolidColorDrawable::new(RED),
            the_square_visibility,
        );
        let the_glyphs = the_drawable.to_glyphs();
        let the_clean_string = the_glyphs.to_clean_string();
        assert_eq!(the_clean_string, "🬎🬎");
    }
    #[test]
    fn test_squares_touched_by_angle_based_visible_segment__simple_horizontal() {
        let seg = AngleBasedVisibleSegment::from_relative_face((STEP_RIGHT * 5, STEP_RIGHT));
        assert_eq!(
            seg.get_touching_relative_squares(),
            (0..=5).map(|i| STEP_RIGHT * i).collect()
        )
    }
}
