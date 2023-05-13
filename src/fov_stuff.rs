use std::collections::{HashMap, HashSet};
use std::f32::consts::PI;

use derive_more::Constructor;
use euclid::{point2, vec2, Angle};
use getset::CopyGetters;
use itertools::*;
use ntest::assert_false;
use num::abs;
use ordered_float::OrderedFloat;

use crate::glyph::angled_blocks::half_plane_to_angled_block_character;
use crate::glyph::glyph_constants::{
    BLACK, CYAN, DARK_CYAN, FULL_BLOCK, OUT_OF_SIGHT_COLOR, RED, SPACE, WHITE,
};
use crate::glyph::{DoubleGlyph, DoubleGlyphFunctions, Glyph};
use crate::graphics;
use crate::graphics::drawable::{
    Drawable, DrawableEnum, PartialVisibilityDrawable, SolidColorDrawable, TextDrawable,
};
use crate::piece::MAX_PIECE_RANGE;
use crate::portal_geometry::{Portal, PortalGeometry, RigidTransform};
use crate::utility::angle_interval::AngleInterval;
use crate::utility::coordinate_frame_conversions::*;
use crate::utility::*;

type StepVisibilityMap = HashMap<WorldStep, SquareVisibility>;

#[derive(Clone, Debug, Copy, Constructor)]
pub struct SquareVisibility {
    visible_portion: Option<LocalSquareHalfPlane>,
}

impl SquareVisibility {
    pub fn is_fully_visible(&self) -> bool {
        self.visible_portion.is_none()
    }
    pub fn is_only_nearly_fully_visible(&self, tolerance: f32) -> bool {
        self.visible_portion
            .is_some_and(|v: LocalSquareHalfPlane| v.fully_covers_expanded_unit_square(-tolerance))
    }
    pub fn is_nearly_or_fully_visible(&self, tolerance: f32) -> bool {
        self.is_fully_visible() || self.is_only_nearly_fully_visible(tolerance)
    }

    pub fn visible_portion(&self) -> Option<LocalSquareHalfPlane> {
        self.visible_portion
    }

    pub fn new_fully_visible() -> Self {
        SquareVisibility {
            visible_portion: None,
        }
    }

    pub fn new_partially_visible(visible_portion: LocalSquareHalfPlane) -> Self {
        assert!(visible_portion.at_least_partially_covers_unit_square());
        assert!(!visible_portion.fully_covers_unit_square());
        SquareVisibility {
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

    pub fn rotated(&self, quarter_turns_anticlockwise: i32) -> Self {
        let mut cloned = self.clone();
        cloned.visible_portion = cloned
            .visible_portion
            .map(|vis: LocalSquareHalfPlane| vis.rotated(quarter_turns_anticlockwise));
        cloned
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
            // TODO: better combination method
            self.clone()
        }
    }
}

#[deprecated(note = "Use shadowDrawable instead")]
#[derive(Clone, Debug, Copy, CopyGetters)]
#[get_copy = "pub"]
pub struct PartialVisibilityOfASquare {
    left_char_shadow: Option<CharacterShadow>,
    right_char_shadow: Option<CharacterShadow>,
    tie_break_bias_direction: Angle<f32>,
}

type CharacterShadow = HalfPlane<f32, CharacterGridInLocalCharacterFrame>;
pub type LocalSquareHalfPlane = HalfPlane<f32, SquareGridInLocalSquareFrame>;

#[derive(Debug, Clone)]
pub struct FieldOfView {
    root_square_with_direction: SquareWithOrthogonalDir,
    visible_relative_squares_in_main_view_only: StepVisibilityMap,
    transformed_sub_fovs: Vec<FieldOfView>,
}

impl FieldOfView {
    pub fn new_empty_fov_with_root(root: SquareWithOrthogonalDir) -> Self {
        FieldOfView {
            root_square_with_direction: root,
            visible_relative_squares_in_main_view_only: Default::default(),
            transformed_sub_fovs: vec![],
        }
    }
    pub fn new_empty_fov_at(new_center: WorldSquare) -> Self {
        Self::new_empty_fov_with_root(SquareWithOrthogonalDir::from_square_and_dir(
            new_center, STEP_UP,
        ))
    }
    pub fn root_square(&self) -> WorldSquare {
        self.root_square_with_direction.square()
    }
    pub fn view_transform_to(&self, other: &FieldOfView) -> RigidTransform {
        let start = self.root_square_with_direction;
        let end = other.root_square_with_direction;
        RigidTransform::from_start_and_end_poses(start, end)
    }
    pub fn sub_fovs(&self) -> &Vec<FieldOfView> {
        &self.transformed_sub_fovs
    }
    pub fn fully_visible_relative_squares_in_main_view_only(&self) -> StepSet {
        self.visible_relative_squares_in_main_view_only
            .iter()
            .filter(|(square, vis): &(&WorldStep, &SquareVisibility)| vis.is_fully_visible())
            .map(|(&square, vis): (&WorldStep, &SquareVisibility)| square)
            .collect()
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

    pub fn rounded_towards_full_visibility(&self, tolerance: f32) -> Self {
        let mut the_clone = self.clone();
        the_clone.visible_relative_squares_in_main_view_only = the_clone
            .visible_relative_squares_in_main_view_only
            .into_iter()
            .map(|(rel_square, visibility): (WorldStep, SquareVisibility)| {
                (
                    rel_square,
                    if visibility.is_nearly_or_fully_visible(tolerance) {
                        SquareVisibility::new_fully_visible()
                    } else {
                        visibility
                    },
                )
            })
            .collect();
        the_clone.transformed_sub_fovs = the_clone
            .transformed_sub_fovs
            .into_iter()
            .map(|sub_fov: FieldOfView| sub_fov.rounded_towards_full_visibility(tolerance))
            .collect();
        the_clone
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
        set_of_keys(&self.visible_relative_squares_in_main_view_only)
    }

    pub fn only_partially_visible_relative_squares_in_main_view_only(&self) -> StepSet {
        self.visible_relative_squares_in_main_view_only
            .iter()
            .filter(|(square, vis)| !vis.is_fully_visible())
            .map(|(&square, vis)| square)
            .collect()
    }
    pub fn visibilities_of_partially_visible_squares_in_main_view_only(
        &self,
    ) -> HashMap<WorldStep, SquareVisibility> {
        self.visible_relative_squares_in_main_view_only
            .iter()
            .filter(|(square, vis)| !vis.is_fully_visible())
            .map(|(square, vis)| (square.clone(), vis.clone()))
            .collect()
    }

    fn combined_main_view_only(&self, other: &Self) -> Self {
        assert_eq!(
            self.root_square_with_direction,
            other.root_square_with_direction
        );

        let squares_visible_in_only_one_view: StepSet = self
            .at_least_partially_visible_relative_squares_in_main_view_only()
            .symmetric_difference(
                &other.at_least_partially_visible_relative_squares_in_main_view_only(),
            )
            .copied()
            .collect();

        let visibility_of_squares_only_visible_in_self: StepVisibilityMap = self
            .visible_relative_squares_in_main_view_only
            .clone()
            .into_iter()
            .filter(|(square, partial)| squares_visible_in_only_one_view.contains(square))
            .collect();

        let visibility_of_squares_only_visible_in_other: StepVisibilityMap = other
            .visible_relative_squares_in_main_view_only
            .clone()
            .into_iter()
            .filter(|(square, partial)| squares_visible_in_only_one_view.contains(square))
            .collect();

        let all_visible_squares: StepSet = self
            .at_least_partially_visible_relative_squares_in_main_view_only()
            .union(&other.at_least_partially_visible_relative_squares_in_main_view_only())
            .copied()
            .collect();

        let squares_visible_in_both_views: StepSet = all_visible_squares
            .difference(&squares_visible_in_only_one_view)
            .copied()
            .collect();

        let visibility_of_squares_visible_in_both_views: StepVisibilityMap =
            squares_visible_in_both_views
                .into_iter()
                .map(|square| {
                    let partial_a = self
                        .visible_relative_squares_in_main_view_only
                        .get(&square)
                        .unwrap();
                    let partial_b = other
                        .visible_relative_squares_in_main_view_only
                        .get(&square)
                        .unwrap();
                    let combined = partial_a.combined_increasing_visibility(&partial_b);
                    (square, combined)
                })
                .collect();

        let mut all_visibilities: StepVisibilityMap = visibility_of_squares_only_visible_in_self;
        all_visibilities.extend(visibility_of_squares_only_visible_in_other);
        all_visibilities.extend(visibility_of_squares_visible_in_both_views);

        FieldOfView {
            root_square_with_direction: self.root_square_with_direction,
            visible_relative_squares_in_main_view_only: all_visibilities,
            transformed_sub_fovs: vec![],
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
            root_square_with_direction: self.root_square_with_direction,
            visible_relative_squares_in_main_view_only: self
                .visible_relative_squares_in_main_view_only
                .clone(),
            transformed_sub_fovs: vec![],
        }
    }

    pub fn can_fully_see_relative_square_as_single_square(&self, step: WorldStep) -> bool {
        let visibility = self.visibilities_of_relative_square_rotated_to_main_view(step);
        return visibility.len() == 1 && visibility.get(0).unwrap().is_fully_visible();
    }

    pub fn can_see_relative_square(&self, step: WorldStep) -> bool {
        !self
            .visibilities_of_relative_square_rotated_to_main_view(step)
            .is_empty()
    }

    pub fn visibilities_of_absolute_square(
        &self,
        world_square: WorldSquare,
    ) -> Vec<SquareVisibility> {
        // Due to portals, this may see the same square multiple times
        let rel_to_root = world_square - self.root_square();
        let mut visibilities = vec![];
        if let Some(visibility_in_untransformed_view) =
            self.visibility_of_relative_square_in_main_view(rel_to_root)
        {
            visibilities.push(visibility_in_untransformed_view);
        }

        self.transformed_sub_fovs
            .iter()
            .for_each(|sub_fov: &FieldOfView| {
                let mut sub_visibilities = sub_fov.visibilities_of_absolute_square(world_square);
                visibilities.append(&mut sub_visibilities);
            });
        visibilities
    }
    pub fn can_see_absolute_square(&self, world_square: WorldSquare) -> bool {
        !self
            .visibilities_of_absolute_square(world_square)
            .is_empty()
    }

    pub fn relative_to_absolute_with_top_view_first(
        &self,
        rel_square: WorldStep,
    ) -> Vec<(WorldSquare, SquareVisibility)> {
        let mut absolute_squares: Vec<(WorldSquare, SquareVisibility)> = Default::default();
        if let Some(main_view_absolute_square) =
            self.relative_to_absolute_from_main_view_only(rel_square)
        {
            let visibility = self
                .visibility_of_relative_square_in_main_view(rel_square)
                .unwrap();
            absolute_squares.push((main_view_absolute_square, visibility));
        }

        let mut results_from_sub_fovs: HashMap<WorldSquare, SquareVisibility> = self
            .transformed_sub_fovs
            .iter()
            .map(|fov: &FieldOfView| {
                // TODO: have this rotation done in one function only
                let rotation = self.view_transform_to(fov).rotation();
                let rotated_rel_square = rotation.rotate_vector(rel_square);
                fov.relative_to_absolute_with_top_view_first(rotated_rel_square)
            })
            .flatten()
            .collect();

        absolute_squares.extend(results_from_sub_fovs);

        absolute_squares
    }
    pub fn relative_to_absolute_from_main_view_only(
        &self,
        rel_square: WorldStep,
    ) -> Option<WorldSquare> {
        if self
            .visible_relative_squares_in_main_view_only
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
        self.visible_relative_squares_in_main_view_only
            .get(&relative_square)
            .copied()
    }

    fn visibility_of_relative_square_in_sub_views_rotated_to_main_view(
        &self,
        relative_square: WorldStep,
    ) -> Vec<SquareVisibility> {
        self.transformed_sub_fovs
            .iter()
            .map(|sub_fov| {
                self.transformed_visibilities_of_relative_square_in_one_sub_view(
                    relative_square,
                    sub_fov,
                )
            })
            .flatten()
            .collect_vec()
    }

    fn transformed_visibilities_of_relative_square_in_one_sub_view(
        &self,
        relative_square: WorldStep,
        sub_view: &FieldOfView,
    ) -> Vec<SquareVisibility> {
        let view_transform_to_sub_view = self.view_transform_to(sub_view);

        let quarter_rotations: QuarterTurnsAnticlockwise = view_transform_to_sub_view.rotation();

        let rotated_relative_square = rotated_n_quarter_turns_counter_clockwise(
            relative_square,
            quarter_rotations.quarter_turns(),
        );

        let rotated_visibilities =
            sub_view.visibilities_of_relative_square_rotated_to_main_view(rotated_relative_square);

        let derotated_visibility = rotated_visibilities
            .iter()
            .map(|v| v.rotated(-quarter_rotations.quarter_turns()))
            .collect_vec();

        derotated_visibility
    }

    pub fn visibilities_of_relative_square_rotated_to_main_view(
        &self,
        relative_square: WorldStep,
    ) -> Vec<SquareVisibility> {
        let mut visibilities: Vec<SquareVisibility> = vec![];

        if let Some(top_level_visibility) =
            self.visibility_of_relative_square_in_main_view(relative_square)
        {
            visibilities.push(top_level_visibility);
        }

        let mut sub_view_visibilities: Vec<SquareVisibility> =
            self.visibility_of_relative_square_in_sub_views_rotated_to_main_view(relative_square);
        visibilities.append(&mut sub_view_visibilities);

        visibilities
    }

    pub fn add_visible_square(&mut self, relative_square: WorldStep, visibility: SquareVisibility) {
        self.visible_relative_squares_in_main_view_only
            .insert(relative_square, visibility);
    }

    pub fn add_fully_visible_square(&mut self, relative_square: WorldStep) {
        self.visible_relative_squares_in_main_view_only
            .insert(relative_square, SquareVisibility::new_fully_visible());
    }
}

struct OctantFOVSquareSequenceIter {
    outward_dir: WorldStep,
    across_dir: WorldStep,
    next_step_number: u32,
}

impl OctantFOVSquareSequenceIter {
    // one_before_starting_square is useful for skipping the vec2(0, 0) square.
    pub fn new(octant: Octant, next_step_number: u32) -> Self {
        let (outward_dir, across_dir) = octant.outward_and_across_directions();

        OctantFOVSquareSequenceIter {
            outward_dir,
            across_dir,
            next_step_number,
        }
    }

    fn next_out_and_across_steps(next_step_number: u32) -> (u32, u32) {
        // area = y + (x-1)/2 * x
        // dx = x-1, dy = y-1, i = area-2

        let area: i32 = next_step_number as i32 + 1;
        let x: i32 = (0.5 + (0.25 + 2.0 * area as f32).sqrt()).ceil() as i32 - 1;
        let y: i32 = area - ((x - 1) as f32 / 2.0 * x as f32).round() as i32;
        let dx: u32 = x as u32 - 1;
        let dy: u32 = y as u32 - 1;
        (dx, dy)
    }
}

impl Iterator for OctantFOVSquareSequenceIter {
    type Item = WorldStep;

    fn next(&mut self) -> Option<Self::Item> {
        let (outward_steps, across_steps) = Self::next_out_and_across_steps(self.next_step_number);
        let relative_square =
            self.outward_dir * outward_steps as i32 + self.across_dir * across_steps as i32;

        self.next_step_number += 1;

        Some(relative_square)
    }
}

pub fn field_of_view_within_arc_in_single_octant(
    sight_blockers: &SquareSet,
    portal_geometry: &PortalGeometry,
    oriented_center_square: SquareWithOrthogonalDir,
    radius: u32,
    octant: Octant,
    view_arc: AngleInterval,
    mut next_step_in_fov_sequence: u32,
) -> FieldOfView {
    let mut fov_result = FieldOfView::new_empty_fov_with_root(oriented_center_square);

    // TODO: Stop being an iterator, just be a function
    let rel_squares_in_fov_sequence =
        OctantFOVSquareSequenceIter::new(octant, next_step_in_fov_sequence);

    for relative_square in rel_squares_in_fov_sequence {
        next_step_in_fov_sequence += 1;
        let out_of_range =
            relative_square.x.abs() > radius as i32 || relative_square.y.abs() > radius as i32;
        if out_of_range {
            break;
        }

        let absolute_square = oriented_center_square.square() + relative_square;

        let view_arc_of_this_square = if relative_square == STEP_ZERO {
            AngleInterval::from_octant(octant)
        } else {
            AngleInterval::from_square(relative_square)
        };

        let maybe_visibility_of_this_square: Option<SquareVisibility> =
            if relative_square == STEP_ZERO {
                Some(SquareVisibility::new_fully_visible())
            } else {
                visibility_of_square(view_arc, relative_square)
            };

        if let Some(visibility_of_this_square) = maybe_visibility_of_this_square {
            fov_result.add_visible_square(relative_square, visibility_of_this_square);
        } else {
            continue;
        }

        let square_blocks_sight = sight_blockers.contains(&absolute_square);
        let square_has_portal = portal_geometry.square_has_portal_entrance(absolute_square);

        let maybe_view_blocking_arc: Option<AngleInterval>;

        // portals are on inside faces of squares, while stepping out of a square, so if a block has a sight blocker and a portal, the sight blocker takes priority
        if square_blocks_sight {
            maybe_view_blocking_arc = Some(view_arc_of_this_square);
        } else if square_has_portal {
            let portals_for_square: Vec<Portal> =
                portal_geometry.portals_entering_from_square(absolute_square);

            // portals are only visible if you see the entrance from the correct side
            let portals_at_square_facing_viewer: Vec<Portal> = portals_for_square
                .into_iter()
                .filter(|portal| {
                    unit_vector_from_angle(view_arc.center_angle())
                        .cast_unit()
                        .dot(portal.entrance().direction_vector().to_f32())
                        > 0.0
                })
                .collect();

            // Break the view arc around the (up to 2) portals and recurse
            let portal_view_arcs: HashMap<Portal, AngleInterval> = portals_at_square_facing_viewer
                .iter()
                .map(|&portal: &Portal| {
                    (
                        portal.clone(),
                        AngleInterval::from_square_face(
                            relative_square,
                            portal.entrance().direction_vector(),
                        ),
                    )
                })
                .collect();

            let visible_portals_in_sight: HashMap<Portal, AngleInterval> = portal_view_arcs
                .clone()
                .into_iter()
                .filter(|(portal, portal_arc): &(Portal, AngleInterval)| {
                    portal_arc.overlapping_but_not_exactly_touching(view_arc)
                })
                .collect();

            visible_portals_in_sight.iter().for_each(
                |(&portal, &portal_view_arc): (&Portal, &AngleInterval)| {
                    let transform = portal.get_transform();
                    let transformed_center = transform.transform_pose(oriented_center_square);
                    let visible_arc_of_portal = view_arc.intersection(portal_view_arc);
                    let transformed_visible_arc_of_portal =
                        transform.transform_arc(visible_arc_of_portal);
                    let mut sub_arc_fov = field_of_view_within_arc_in_single_octant(
                        sight_blockers,
                        portal_geometry,
                        transformed_center,
                        radius,
                        transform.transform_octant(octant),
                        transformed_visible_arc_of_portal,
                        next_step_in_fov_sequence,
                    );
                    fov_result.transformed_sub_fovs.push(sub_arc_fov);
                },
            );

            // a max of two portals are visible in one square, touching each other at a corner
            if portals_at_square_facing_viewer.len() > 0 {
                // TODO: turn this into a constructor for angleintervals
                let combined_view_arc_of_portals: AngleInterval = if portal_view_arcs.len() == 1 {
                    *portal_view_arcs.values().next().unwrap()
                } else if portal_view_arcs.len() == 2 {
                    let arcs = portal_view_arcs.values().next_chunk::<2>().unwrap();
                    arcs[0].union(*arcs[1])
                } else {
                    panic!(
                        "There should be exactly one or two portals here.  found {}: {:?}",
                        portal_view_arcs.len(),
                        portal_view_arcs
                    );
                };
                maybe_view_blocking_arc = Some(combined_view_arc_of_portals);
            } else {
                maybe_view_blocking_arc = None;
            };
        } else {
            maybe_view_blocking_arc = None;
        }

        if let Some(view_blocking_arc) = maybe_view_blocking_arc {
            // split arc and recurse
            let view_arcs_around_blocker: Vec<AngleInterval> = view_arc.subtract(view_blocking_arc);
            view_arcs_around_blocker
                .into_iter()
                .filter(|new_sub_arc| new_sub_arc.width().to_degrees() > 0.01)
                .for_each(|new_sub_arc| {
                    let sub_arc_fov = field_of_view_within_arc_in_single_octant(
                        sight_blockers,
                        portal_geometry,
                        oriented_center_square,
                        radius,
                        octant,
                        new_sub_arc,
                        next_step_in_fov_sequence,
                    );
                    fov_result = fov_result.combined_with(&sub_arc_fov);
                });
            break;
        }
    }
    fov_result
}

pub fn single_octant_field_of_view(
    sight_blockers: &HashSet<WorldSquare>,
    portal_geometry: &PortalGeometry,
    center_square: WorldSquare,
    radius: u32,
    octant: Octant,
) -> FieldOfView {
    //arc.next_relative_square_in_octant_sequence(first_relative_square_in_sequence);
    //let octant: i32 = arc.octant().expect("arc not confined to octant");
    let mut fov_result = field_of_view_within_arc_in_single_octant(
        sight_blockers,
        portal_geometry,
        SquareWithOrthogonalDir::from_square_and_dir(center_square, STEP_UP),
        radius,
        octant,
        AngleInterval::from_octant(octant),
        0,
    );
    fov_result.add_fully_visible_square(STEP_ZERO);
    fov_result
}

pub fn portal_aware_field_of_view_from_square(
    center_square: WorldSquare,
    radius: u32,
    sight_blockers: &SquareSet,
    portal_geometry: &PortalGeometry,
) -> FieldOfView {
    (0..8)
        .fold(
            FieldOfView::new_empty_fov_at(center_square),
            |fov_result_accumulator: FieldOfView, octant_number: i32| {
                let new_fov_result = single_octant_field_of_view(
                    sight_blockers,
                    portal_geometry,
                    center_square,
                    radius,
                    Octant::new(octant_number),
                );
                let combined_fov = fov_result_accumulator.combined_with(&new_fov_result);
                combined_fov
            },
        )
        .rounded_towards_full_visibility(1e-3)
}

fn point_in_view_arc(view_arc: AngleInterval) -> WorldMove {
    unit_vector_from_angle(view_arc.center_angle()).cast_unit()
}

fn visibility_of_square(
    view_arc: AngleInterval,
    rel_square: WorldStep,
) -> Option<SquareVisibility> {
    let square_arc = AngleInterval::from_square(rel_square);
    if view_arc.at_least_fully_overlaps(square_arc) {
        Some(SquareVisibility::new_fully_visible())
    } else if view_arc.overlapping_but_not_exactly_touching(square_arc) {
        square_visibility_from_one_view_arc(view_arc, rel_square)
    } else {
        None
    }
}

fn square_visibility_from_one_view_arc(
    visibility_arc: AngleInterval,
    square_relative_to_center: WorldStep,
) -> Option<SquareVisibility> {
    let square_arc = AngleInterval::from_square(square_relative_to_center);
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
        Some(SquareVisibility::new_partially_visible(
            square_shadow.complement(),
        ))
    } else {
        Some(SquareVisibility::new_fully_visible())
    }
}

fn print_fov(fov: &FieldOfView, radius: u32) {
    let r = radius as i32;
    (-r..=r).for_each(|neg_y| {
        let y = -neg_y;
        (-r..r).for_each(|x| {
            let rel_square: WorldStep = vec2(x, y);
            let visibilities_for_square =
                fov.visibilities_of_relative_square_rotated_to_main_view(rel_square);

            let any_visible = visibilities_for_square.len() > 0;
            let to_draw = if any_visible {
                let visible_base = SolidColorDrawable::new(WHITE).to_enum();
                visibilities_for_square.into_iter().fold(
                    visible_base,
                    |base: DrawableEnum, visibility: SquareVisibility| {
                        if visibility.is_fully_visible() {
                            base
                        } else {
                            PartialVisibilityDrawable::from_partially_visible_drawable(
                                &base, visibility,
                            )
                            .to_enum()
                        }
                    },
                )
            } else {
                SolidColorDrawable::new(BLACK).to_enum()
            };

            print!("{}", to_draw.to_glyphs().to_string());
        });
        print!("\n");
    });
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
        better_angle_from_x_axis, line_intersections_with_centered_unit_square,
        QuarterTurnsAnticlockwise, SquareWithAdjacentDir, SquareWithOrthogonalDir, STEP_DOWN,
        STEP_LEFT, STEP_UP,
    };

    use super::*;

    const SIGHT_RADIUS: u32 = 16;

    #[test]
    fn test_square_view_angle__horizontal() {
        let view_angle = AngleInterval::from_square(vec2(3, 0));
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
        let view_angle = AngleInterval::from_square(vec2(5, 3));
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
        dbg!(&fov_result.visible_relative_squares_in_main_view_only);
        assert!(fov_result
            .only_partially_visible_relative_squares_in_main_view_only()
            .is_empty());
        assert!(fov_result.can_fully_see_relative_square_as_single_square(STEP_ZERO));
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

        dbg!(fov_result.fully_visible_relative_squares_in_main_view_only());
        assert!(fov_result
            .only_partially_visible_relative_squares_in_main_view_only()
            .is_empty());
        assert!(fov_result.can_fully_see_relative_square_as_single_square(STEP_ZERO));
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
        let partial = SquareVisibility::from_visible_half_plane(
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
        assert!(fov_result.can_fully_see_relative_square_as_single_square(block_step));
        assert!(fov_result.can_fully_see_relative_square_as_single_square(block_step + STEP_DOWN));
        assert_false!(
            fov_result.can_fully_see_relative_square_as_single_square(block_step + STEP_UP)
        );
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
                .visible_relative_squares_in_main_view_only
                .get(&step)
                .unwrap();
            //dbg!(partial_visibility);
            let string = PartialVisibilityDrawable::from_square_visibility(*square_visibility)
                .to_glyphs()
                .to_clean_string();
            assert_eq!(&string, "🭞🭚");
        }
    }

    #[test]
    fn test_single_square_is_shadowed_correctly_on_diagonal() {
        let interval = AngleInterval::from_degrees(0.0, 45.0).complement();
        let square_relative_to_center = vec2(1, 1);
        let visibility = square_visibility_from_one_view_arc(interval, square_relative_to_center);
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
            .visibilities_of_relative_square_rotated_to_main_view(test_rel_square)
            .get(0)
            .unwrap()
            .clone();
        assert_eq!(
            PartialVisibilityDrawable::from_square_visibility(visibility_of_test_square)
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

        let partial_1 = SquareVisibility::from_visible_half_plane(half_plane_1).unwrap();
        let partial_2 = SquareVisibility::from_visible_half_plane(half_plane_2).unwrap();

        let combined_partial = partial_1.combined_increasing_visibility(&partial_2);
        //dbg!( &partial_1.to_glyphs().to_clean_string(), &partial_2.to_glyphs().to_clean_string(), &combined_partial.to_glyphs().to_clean_string() );
        assert!(combined_partial.is_fully_visible());
    }

    #[test]
    fn test_fov_square_sequence__detailed() {
        let mut sequence = OctantFOVSquareSequenceIter::new(Octant::new(1), 0);
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
        let arc = AngleInterval::from_degrees(90.0, 135.0);
        let square = WorldStep::new(0, 1);
        let partial = square_visibility_from_one_view_arc(arc, square);
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
            &sight_blockers,
            &PortalGeometry::default(),
            mid_square,
            10,
            Octant::new(0),
        );
        let visible_rel_square = STEP_RIGHT * 5 + STEP_UP * 2;
        assert!(fov_result
            .can_fully_see_relative_square_as_single_square((visible_rel_square + STEP_LEFT)));
        assert!(fov_result.can_fully_see_relative_square_as_single_square(visible_rel_square));
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
            .visible_relative_squares_in_main_view_only
            .iter()
            .filter(|(step, vis)| !vis.is_fully_visible())
            .map(|(step, square_vis): (&WorldStep, &SquareVisibility)| {
                (
                    step,
                    PartialVisibilityDrawable::from_square_visibility(*square_vis)
                        .to_glyphs()
                        .to_clean_string(),
                )
            })
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
    ) -> Option<SquareVisibility> {
        square_visibility_from_one_view_arc(
            AngleInterval::from_square(block_square).complement(),
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
            //dbg!(i);
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
        fov_result.add_fully_visible_square(relative_square);

        let square_visibility =
            fov_result.visibilities_of_relative_square_rotated_to_main_view(relative_square);
        assert!(square_visibility.get(0).unwrap().is_fully_visible());
        assert!(square_visibility.get(0).unwrap().visible_portion.is_none());
    }

    #[test]
    fn test_really_narrow_fov_through_a_portal() {
        let mut portal_geometry = PortalGeometry::default();
        let center = point2(10, 20);
        let dx_to_portal_square = 3;
        let entrance_square = center + STEP_RIGHT * dx_to_portal_square;
        let exit_square = center + STEP_LEFT * 5;

        portal_geometry.create_portal(
            SquareWithOrthogonalDir::new(entrance_square, STEP_RIGHT),
            SquareWithOrthogonalDir::new(exit_square, STEP_DOWN),
        );

        let view_arc = AngleInterval::from_degrees(0.1, 0.11);

        let radius = 10;
        let mut fov_result = field_of_view_within_arc_in_single_octant(
            &Default::default(),
            &portal_geometry,
            SquareWithOrthogonalDir::from_square_and_dir(center, STEP_UP),
            radius,
            Octant::new(0),
            view_arc,
            0,
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
                point_to_string(*square)
            );
        });
        should_be_visible_after_portal.iter().for_each(|square| {
            assert!(
                fov_result.can_see_absolute_square(*square),
                "square: {}",
                point_to_string(*square)
            );
        });
    }

    #[test]
    fn test_sub_view_through_portal_has_correct_transform() {
        let mut portal_geometry = PortalGeometry::default();
        let center = point2(-15, 50);
        let portal_entrance = SquareWithOrthogonalDir::new(center + STEP_RIGHT, STEP_RIGHT);
        let portal_exit = SquareWithOrthogonalDir::new(center + STEP_DOWN_LEFT * 15, STEP_DOWN);
        portal_geometry.create_portal(portal_entrance, portal_exit);

        let fov_result = single_octant_field_of_view(
            &Default::default(),
            &portal_geometry,
            center,
            3,
            Octant::new(0),
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
        let portal_entrance = SquareWithOrthogonalDir::new(center + STEP_RIGHT, STEP_RIGHT);
        let portal_exit = SquareWithOrthogonalDir::new(center + STEP_DOWN_LEFT * 15, STEP_DOWN);
        portal_geometry.create_portal(portal_entrance, portal_exit);

        let fov_result = single_octant_field_of_view(
            &Default::default(),
            &portal_geometry,
            center,
            3,
            Octant::new(0),
        );

        assert_eq!(fov_result.transformed_sub_fovs.len(), 1);
        // Not fully visible because only one octant
        assert!(fov_result.can_see_relative_square(STEP_RIGHT * 2));
        assert_false!(fov_result.can_see_absolute_square(portal_entrance.square() + STEP_RIGHT));
        assert!(fov_result.can_see_absolute_square(portal_exit.square()));

        assert_false!(fov_result.transformed_sub_fovs[0]
            .can_fully_see_relative_square_as_single_square(STEP_ZERO));
    }

    #[test]
    fn test_sub_fov_view_transform() {
        let sub_center = SquareWithOrthogonalDir::from_square_and_dir(point2(1, 0), STEP_RIGHT);
        let mut sub_fov = FieldOfView::new_empty_fov_at(sub_center.square());
        sub_fov.root_square_with_direction = sub_center;

        let main_center = SquareWithOrthogonalDir::from_square_and_dir(point2(50, 0), STEP_UP);
        let mut main_fov = FieldOfView::new_empty_fov_at(main_center.square());
        main_fov.root_square_with_direction = main_center;

        let target_square = point2(1, 4);

        sub_fov.add_fully_visible_square(target_square - sub_fov.root_square());

        main_fov.transformed_sub_fovs.push(sub_fov);

        let rel_from_main = STEP_LEFT * 4;

        assert!(main_fov.can_fully_see_relative_square_as_single_square(rel_from_main));
        assert!(main_fov.can_see_absolute_square(point2(1, 4)));

        assert_eq!(
            main_fov.at_least_partially_visible_relative_squares_including_subviews(),
            StepSet::from([rel_from_main])
        )
    }

    #[test]
    fn test_square_fully_covered_by_face() {
        let view_arc_of_face = AngleInterval::from_square_face(STEP_RIGHT, STEP_RIGHT);
        let square = STEP_RIGHT * 2;

        let visibility = visibility_of_square(view_arc_of_face, square);
        assert!(visibility.unwrap().is_fully_visible());
    }

    #[test]
    fn test_square_fully_not_covered_by_adjacent() {
        let view_arc_of_face = AngleInterval::from_square_face(STEP_UP_RIGHT, STEP_RIGHT);
        let square = STEP_RIGHT * 2;

        let visibility = visibility_of_square(view_arc_of_face, square);
        assert!(visibility.is_none());
    }

    #[test]
    fn test_square_fully_inside_view_arc__near_edge() {
        let square = vec2(1, -2);
        let arc = AngleInterval::from_radians(-PI / 2.0, -PI / 4.0);
        assert!(visibility_of_square(arc, square)
            .unwrap()
            .is_fully_visible());
    }

    #[test]
    fn test_octant_step_sequence() {
        let i_x_y = vec![
            (0, 0, 0),
            (1, 1, 0),
            (2, 1, 1),
            (3, 2, 0),
            (4, 2, 1),
            (5, 2, 2),
            (6, 3, 0),
            (7, 3, 1),
            (8, 3, 2),
            (9, 3, 3),
            (10, 4, 0),
            (11, 4, 1),
        ];
        i_x_y.into_iter().for_each(|(i, x, y)| {
            assert_eq!(
                OctantFOVSquareSequenceIter::next_out_and_across_steps(i),
                (x, y)
            )
        });
    }

    #[test]
    fn test_portal_pose_transform() {
        let entrance = SquareWithOrthogonalDir::from_square_and_dir(point2(3, 4), STEP_RIGHT);
        let exit = SquareWithOrthogonalDir::from_square_and_dir(point2(50, 70), STEP_DOWN);
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
            let virtual_center_at_exit = portal.get_transform().transform_pose(actual_center);
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
        fov_1.add_fully_visible_square(STEP_RIGHT);
        fov_2.add_fully_visible_square(STEP_UP);

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
    fn test_combined_fovs_combine_visibility() {
        let main_center = point2(5, 5);
        let mut fov_1 = FieldOfView::new_empty_fov_at(main_center);
        let mut fov_2 = FieldOfView::new_empty_fov_at(main_center);
        let rel_square = STEP_RIGHT * 3;
        fov_1
            .visible_relative_squares_in_main_view_only
            .insert(rel_square, SquareVisibility::top_half_visible());
        fov_2
            .visible_relative_squares_in_main_view_only
            .insert(rel_square, SquareVisibility::bottom_half_visible());

        let combined = fov_1.combined_with(&fov_2);

        assert_eq!(
            combined
                .fully_visible_relative_squares_including_subviews()
                .len(),
            1
        );
        assert!(combined
            .only_partially_visible_relative_squares_in_main_view_only()
            .is_empty());
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
        sub_fov_1
            .visible_relative_squares_in_main_view_only
            .insert(rel_square, SquareVisibility::top_half_visible());
        sub_fov_2
            .visible_relative_squares_in_main_view_only
            .insert(rel_square, SquareVisibility::bottom_half_visible());

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
        assert!(combined_fov.can_fully_see_relative_square_as_single_square(rel_square));
    }

    #[test]
    fn test_fov_relative_to_absolute__top_level() {
        let main_center = point2(5, 5);

        let mut fov = FieldOfView::new_empty_fov_at(main_center);

        let rel_square = STEP_DOWN_LEFT * 3;
        let correct_abs_square = main_center + rel_square;

        fov.add_fully_visible_square(rel_square);

        let &(abs_square, visibility): &(WorldSquare, SquareVisibility) = fov
            .relative_to_absolute_with_top_view_first(rel_square)
            .iter()
            .next()
            .unwrap();
        assert_eq!(abs_square, correct_abs_square);
        assert_eq!(
            fov.at_least_partially_visible_relative_squares_including_subviews()
                .len(),
            1
        );
        assert!(visibility.is_fully_visible());
    }

    #[test]
    fn test_fov_relative_to_absolute__sub_view_no_rotation() {
        let main_center = point2(5, 5);
        let sub_center = point2(34, -7);

        let mut fov = FieldOfView::new_empty_fov_at(main_center);
        let mut sub_fov = FieldOfView::new_empty_fov_at(sub_center);

        let rel_square = STEP_DOWN_LEFT * 3;
        let abs_square = sub_center + rel_square;

        sub_fov.add_fully_visible_square(rel_square);

        fov.transformed_sub_fovs.push(sub_fov);

        assert_eq!(
            fov.relative_to_absolute_with_top_view_first(rel_square)
                .iter()
                .next()
                .unwrap()
                .0,
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
            fov.root_square_with_direction.direction_vector(),
            quarter_turns,
        );
        let mut sub_fov = FieldOfView::new_empty_fov_with_root(SquareWithOrthogonalDir::new(
            sub_center,
            sub_fov_direction,
        ));

        let rel_square = STEP_DOWN_LEFT * 3;
        let rotated_rel_square =
            rotated_n_quarter_turns_counter_clockwise(rel_square, quarter_turns);
        let abs_square = sub_center + rotated_rel_square;

        sub_fov.add_fully_visible_square(rotated_rel_square);
        fov.transformed_sub_fovs.push(sub_fov);

        assert_eq!(
            fov.relative_to_absolute_with_top_view_first(rel_square)
                .iter()
                .next()
                .unwrap()
                .0,
            abs_square
        );
    }

    #[test]
    fn test_partial_visibility_in_blindspot_of_nearly_full_arc() {
        let rel_square = vec2(4, 4);
        // These values are from an observed failure.  NOT ARBITRARY
        let arc = AngleInterval::new(Angle::radians(0.7853978), Angle::radians(0.7853982));
        let visibility = square_visibility_from_one_view_arc(arc, rel_square);
    }

    #[test]
    fn test_visibility_of_multiple_squares_in_one_square() {
        let main_center = point2(5, 5);
        let other_center = point2(15, 5);

        let mut fov_1 = FieldOfView::new_empty_fov_at(main_center);
        let mut sub_fov_1 = FieldOfView::new_empty_fov_at(other_center);

        let rel_square = STEP_RIGHT * 3;
        fov_1
            .visible_relative_squares_in_main_view_only
            .insert(rel_square, SquareVisibility::bottom_half_visible());
        sub_fov_1
            .visible_relative_squares_in_main_view_only
            .insert(rel_square, SquareVisibility::top_half_visible());

        fov_1.transformed_sub_fovs.push(sub_fov_1);

        assert_eq!(
            fov_1
                .visibilities_of_relative_square_rotated_to_main_view(rel_square)
                .len(),
            2
        );
        assert_eq!(
            fov_1
                .relative_to_absolute_with_top_view_first(rel_square)
                .len(),
            2
        );
    }
    #[test]
    fn test_rounding_towards_full_visibility() {
        let mut fov = FieldOfView::new_empty_fov_at(point2(0, 0));
        fov.add_fully_visible_square(STEP_RIGHT);

        fov.add_visible_square(
            STEP_UP,
            SquareVisibility::new_partially_visible(
                LocalSquareHalfPlane::top_half_plane().extended(0.5 - 1e-2),
            ),
        );
        fov.add_visible_square(
            STEP_DOWN,
            SquareVisibility::new_partially_visible(
                LocalSquareHalfPlane::top_half_plane().extended(0.5 - 1e-4),
            ),
        );
        assert_eq!(fov.visible_relative_squares_in_main_view_only.len(), 3);
        assert_eq!(
            fov.fully_visible_relative_squares_in_main_view_only().len(),
            1
        );
        assert_eq!(
            fov.only_partially_visible_relative_squares_in_main_view_only()
                .len(),
            2
        );

        let rounded_fov = fov.rounded_towards_full_visibility(1e-3);
        assert_eq!(
            rounded_fov.visible_relative_squares_in_main_view_only.len(),
            3
        );
        assert_eq!(
            rounded_fov
                .fully_visible_relative_squares_in_main_view_only()
                .len(),
            2
        );
        assert_eq!(
            rounded_fov
                .only_partially_visible_relative_squares_in_main_view_only()
                .len(),
            1
        );
        assert!(rounded_fov
            .fully_visible_relative_squares_in_main_view_only()
            .contains(&STEP_DOWN));
        assert!(rounded_fov
            .only_partially_visible_relative_squares_in_main_view_only()
            .contains(&STEP_UP));
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
        assert_eq!(fov.visible_relative_squares_in_main_view_only.len(), 1);
        assert!(fov
            .visible_relative_squares_in_main_view_only
            .get(&STEP_ZERO)
            .unwrap()
            .is_fully_visible());
    }
    #[test]
    fn test_adjacent_wall_all_fully_visible() {
        let player_square = point2(5, 5);
        let rel_blocks = (0..10).map(|i| STEP_RIGHT + STEP_UP * i).collect_vec();
        let abs_squares = rel_blocks.iter().map(|v| player_square + *v).collect_vec();
        let blocks = SquareSet::from_iter(abs_squares);
        let fov =
            portal_aware_field_of_view_from_square(player_square, 5, &blocks, &Default::default());

        print_fov(&fov, 5);
        rel_blocks.iter().for_each(|rel_block| {
            assert!(
                fov.can_fully_see_relative_square_as_single_square(*rel_block),
                "rel_block: {:?}",
                rel_block
            )
        });
    }
}
