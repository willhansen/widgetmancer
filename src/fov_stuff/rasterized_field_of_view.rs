use crate::fov_stuff::square_visibility::LocalSquareVisibilityMap;
use crate::fov_stuff::square_visibility::SquareVisibility;
use crate::fov_stuff::square_visibility::SquareVisibilityMapFunctions;
use crate::fov_stuff::square_visibility::{RelativeSquareVisibilityFunctions, ViewRoundable};
use crate::glyph::glyph_constants::RED;
use crate::graphics::drawable::{
    Drawable, DrawableEnum, PartialVisibilityDrawable, SolidColorDrawable,
};
use crate::utility::coordinate_frame_conversions::{SquareSet, StepSet, WorldSquare, WorldStep};
use crate::utility::poses::SquareWithOrthogonalDir;
use crate::utility::RigidTransform;
use crate::utility::RigidlyTransformable;
use crate::utility::{
    king_step_distance, number_to_hue_rotation, CoordToString, QuarterTurnRotatable,
    QuarterTurnsCcw, SimpleResult, TupleClone, STEP_ZERO,
};
use ambassador::delegatable_trait;
use derive_more::Constructor;
use itertools::Itertools;
use std::collections::{HashMap, HashSet};

// TODO: rename?  Should it be "target" as in "target a thing to draw", or "source" as in "the source of what to draw"?
#[derive(Clone, Copy, Hash, Eq, PartialEq, Debug)]
struct TopDownPortalTarget {
    absolute_square: WorldSquare,
    portal_depth: u32,
    portal_rotation_to_target: QuarterTurnsCcw,
}
type LocallyPositionedTopDownPortalTarget = (WorldStep, TopDownPortalTarget);

#[derive(Clone, Copy, Debug)]
pub struct TopDownPortal {
    relative_position: WorldStep,
    target: TopDownPortalTarget,
    shape_in_exit_frame: TopDownPortalShape,
}

/// Key metaphor is that the portal is no longer from player to square, it is now screen to square, in a top-down fashion, so it can be rendered correctly.
/// TODO: maybe precalculate indexes
#[derive(PartialEq, Clone, Default, Debug)]
pub struct RasterizedFieldOfView {
    pub view_root: SquareWithOrthogonalDir,
    map_of_top_down_portal_shapes_by_coordinates: UniqueTopDownPortals,
}

// these are the public functions.
pub trait RasterizedFieldOfViewFunctions {
    // creation
    fn from_local_visibility_map(root: WorldSquare, vis_map: &LocalSquareVisibilityMap) -> Self;

    // adding
    fn add_fully_visible_local_relative_square(&mut self, relative_square: WorldStep);
    fn try_add_visible_local_relative_square(
        &mut self,
        relative_square: WorldStep,
        visibility: &TopDownPortalShape,
    ) -> SimpleResult;
    fn add_visible_local_relative_square(
        &mut self,
        relative_square: WorldStep,
        visibility: &TopDownPortalShape,
    );

    // getting
    fn root_square(&self) -> WorldSquare;

    // visibility maps
    fn visibilities_of_partially_visible_squares_in_main_view_only(
        &self,
    ) -> LocalSquareVisibilityMap;
    fn visibility_map_of_local_relative_squares(&self) -> LocalSquareVisibilityMap;

    // visible relative_squares
    fn fully_visible_relative_squares(&self) -> StepSet;
    fn fully_visible_local_relative_squares(&self) -> StepSet;
    fn only_partially_visible_local_relative_squares(&self) -> StepSet;
    fn number_of_visible_relative_squares(&self) -> u32;
    fn number_of_fully_visible_relative_squares(&self) -> u32;
    fn visible_local_relative_squares(&self) -> StepSet;
    fn visible_relative_squares_including_center(&self) -> StepSet;

    // visible absolute squares

    // checks on a relative square
    fn relative_square_is_fully_visible(&self, step: impl Into<WorldStep>) -> bool;
    fn relative_square_is_only_partially_visible(&self, step: WorldStep) -> bool;
    fn relative_square_is_only_locally_visible(&self, step: WorldStep) -> bool;
    fn relative_square_is_visible(&self, relative_square: impl Into<WorldStep>) -> bool;
    fn lone_portal_depth_for_relative_square_or_panic(&self, relative_square: WorldStep) -> u32;
    fn lone_portal_rotation_for_relative_square_or_panic(
        &self,
        relative_square: WorldStep,
    ) -> QuarterTurnsCcw;
    fn lone_square_visibility_rotated_to_absolute_frame_for_relative_square_or_panic(
        &self,
        relative_square: WorldStep,
    ) -> SquareVisibility;
    fn lone_square_visibility_rotated_to_relative_frame_for_relative_square_or_panic(
        &self,
        relative_square: WorldStep,
    ) -> SquareVisibility;
    fn shapes_of_visibilities_of_relative_square_rotated_to_local_frame(
        &self,
        relative_square: WorldStep,
    ) -> Vec<TopDownPortalShape>;
    fn top_down_portals_for_relative_square(
        &self,
        relative_square: WorldStep,
    ) -> Vec<TopDownPortal>;
    fn visible_local_absolute_square_for_relative_square(
        &self,
        rel_square: WorldStep,
    ) -> Option<WorldSquare>;
    fn absolute_squares_visible_at_relative_square(&self, relative_square: WorldStep) -> SquareSet;

    // checks on an absolute square
    fn absolute_square_is_visible(&self, world_square: WorldSquare) -> bool;
    fn top_down_portals_for_absolute_square(
        &self,
        absolute_square: WorldSquare,
    ) -> Vec<TopDownPortal>;
    fn times_absolute_square_is_fully_visible(&self, absolute_square: WorldSquare) -> usize;
    fn times_absolute_square_is_visible(&self, absolute_square: WorldSquare) -> usize;

    // modifying
    fn as_seen_through_portal_from_pose(
        &self,
        new_view_root: SquareWithOrthogonalDir,
        portal_transform_from_other_to_self: RigidTransform,
    ) -> Self;
    fn combined_with(&self, other: &Self) -> Self;
    fn rounded_towards_full_visibility(&self, tolerance: f32) -> Self;
}
type TopDownPortalShape = SquareVisibility;

type UniqueTopDownPortals = HashMap<LocallyPositionedTopDownPortalTarget, TopDownPortalShape>;

#[derive(Clone, Debug)]
/// A RasterizedFieldOfView with the additional guarantee of all top-down-portals being on one square
struct SquareOfTopDownPortals(RasterizedFieldOfView);

#[derive(Clone, Debug)]
struct DirectConnectionToLocalSquare(TopDownPortal);

impl SquareOfTopDownPortals {
    fn at(&mut self, relative_square: WorldStep) {
        todo!()
    }
    fn new_direct_local_connection(
        relative_position: WorldStep,
        target_square: WorldSquare,
        visibility: &TopDownPortalShape,
    ) -> Self {
        DirectConnectionToLocalSquare::new(relative_position, target_square, visibility).into()
    }
    fn lone_absolute_square_or_panic(&self) -> WorldSquare {
        self.lone_top_down_portal_or_panic().target.absolute_square
    }
    fn lone_portal_depth_or_panic(&self) -> u32 {
        self.lone_top_down_portal_or_panic().target.portal_depth
    }
    fn lone_portal_rotation_or_panic(&self) -> QuarterTurnsCcw {
        self.lone_top_down_portal_or_panic()
            .target
            .portal_rotation_to_target
    }
    fn lone_square_visibility_rotated_to_exit_frame_or_panic(&self) -> TopDownPortalShape {
        self.lone_top_down_portal_or_panic().shape_in_exit_frame
    }
    fn lone_square_visibility_rotated_to_entrance_frame_or_panic(&self) -> TopDownPortalShape {
        self.lone_top_down_portal_or_panic()
            .shape_in_entrance_frame()
    }

    fn lone_top_down_portal_or_panic(&self) -> TopDownPortal {
        let portal_count = self
            .0
            .map_of_top_down_portal_shapes_by_coordinates
            .iter()
            .count();
        if portal_count == 1 {
            self.0
                .map_of_top_down_portal_shapes_by_coordinates
                .iter()
                .next()
                .unwrap()
                .into()
        } else if portal_count > 1 {
            panic!(
                "More than one visibility found.  ambiguous call.  self: {:?}",
                self
            )
        } else {
            panic!(
                "No visibilities.  this shouldn't be possible. self: {:?}",
                self
            )
        }
    }

    fn one_portal_deeper(&self, forward_rotation_through_portal: QuarterTurnsCcw) -> Self {
        todo!()
        // Self::from(self.top_down_portals.iter().map(|x| {
        //     Into::<TopDownPortal>::into(x.tuple_clone())
        //         .one_portal_deeper(forward_rotation_through_portal)
        // }))

        // relative_square: rotated_n_quarter_turns_counter_clockwise(
        //     self.relative_square,
        //     -forward_rotation_through_portal.quarter_turns(),
        // ),
    }
    fn relative_position(&self) -> WorldStep {
        // all the contained top down portals should have the same relative square.
        self.0
            .map_of_top_down_portal_shapes_by_coordinates
            .keys()
            .next()
            .unwrap()
            .0
    }
    fn top_down_portals(&self) -> Vec<TopDownPortal> {
        self.0
            .map_of_top_down_portal_shapes_by_coordinates
            .iter()
            .map(|x| x.into())
            .collect()
    }
}
impl FromIterator<TopDownPortal> for SquareOfTopDownPortals {
    fn from_iter<T: IntoIterator<Item = TopDownPortal>>(iter: T) -> Self {
        // TODO: double check the use of default pose here
        RasterizedFieldOfView::from_top_down_portal_iter(SquareWithOrthogonalDir::default(), iter)
            .try_into()
            .unwrap()
    }
}
// impl FromIterator<TopDownPortal> for RasterizedFieldOfView {
//     fn from_iter<T: IntoIterator<Item = TopDownPortal>>(iter: T) -> Self {
//         if has_portal_entrance_overlaps {
//             panic!("can't create topdownified field of view because of overlapping top down portal entrances")
//         }
//         new_fov
//     }
// }
impl TryFrom<RasterizedFieldOfView> for SquareOfTopDownPortals {
    type Error = ();

    fn try_from(value: RasterizedFieldOfView) -> Result<Self, Self::Error> {
        if value
            .top_down_portal_iter()
            .map(|x| x.relative_position())
            .all_equal()
        {
            Ok(Self(value))
        } else {
            Err(())
        }
    }
}

impl ViewRoundable for SquareOfTopDownPortals {
    fn rounded_towards_full_visibility(&self, tolerance: f32) -> Self {
        Self::from_iter(
            self.0
                .map_of_top_down_portal_shapes_by_coordinates
                .iter()
                .map(
                    |(&draw_target_coordinates, &square_visibility_in_absolute_frame)| {
                        let top_down_portal: TopDownPortal = (
                            draw_target_coordinates,
                            square_visibility_in_absolute_frame
                                .rounded_towards_full_visibility(tolerance),
                        )
                            .into();
                        top_down_portal
                    },
                ),
        )
    }
}

impl RasterizedFieldOfViewFunctions for RasterizedFieldOfView {
    fn from_local_visibility_map(root: WorldSquare, vis_map: &LocalSquareVisibilityMap) -> Self {
        let mut new_thing = Self::new_centered_at(root);
        vis_map.iter().for_each(|(rel_square, visibility)| {
            new_thing.try_add_visible_local_relative_square(*rel_square, visibility);
        });
        new_thing
    }
    fn add_fully_visible_local_relative_square(&mut self, relative_square: WorldStep) {
        self.add_top_down_portal(
            self.new_direct_connection_to_local_square(
                relative_square,
                &TopDownPortalShape::new_fully_visible(),
            )
            .top_down_portal(),
        );
    }

    fn try_add_visible_local_relative_square(
        &mut self,
        relative_square: WorldStep,
        visibility: &TopDownPortalShape,
    ) -> SimpleResult {
        // make sure there isn't a positioned visibility in the main view in the same relative square already
        if self.can_see_local_relative_square(relative_square) {
            return Err(());
        }

        self.map_of_top_down_portal_shapes_by_coordinates.extend(
            SquareOfTopDownPortals::new_direct_local_connection(
                relative_square,
                self.relative_to_local_absolute_square(relative_square),
                visibility,
            )
            .0
            .map_of_top_down_portal_shapes_by_coordinates,
        );
        Ok(())
    }
    fn add_visible_local_relative_square(
        &mut self,
        relative_square: WorldStep,
        visibility: &TopDownPortalShape,
    ) {
        self.try_add_visible_local_relative_square(relative_square, visibility)
            .expect(&format!(
                "Failed to add square: {}",
                relative_square.to_string()
            ));
    }

    fn root_square(&self) -> WorldSquare {
        // Will panic if no visible square at relative origin
        self.visible_local_absolute_square_for_relative_square(STEP_ZERO)
            .unwrap()
    }

    fn visibilities_of_partially_visible_squares_in_main_view_only(
        &self,
    ) -> LocalSquareVisibilityMap {
        self.filtered(true, true, false).visibility_map()
    }

    fn visibility_map_of_local_relative_squares(&self) -> LocalSquareVisibilityMap {
        self.filtered(true, true, true).visibility_map()
    }
    fn fully_visible_relative_squares(&self) -> StepSet {
        self.filtered(false, false, true)
            .visible_relative_squares_including_center()
    }

    fn fully_visible_local_relative_squares(&self) -> StepSet {
        self.filtered(true, false, true)
            .visible_relative_squares_including_center()
    }

    fn only_partially_visible_local_relative_squares(&self) -> StepSet {
        self.filtered(true, true, false)
            .visible_relative_squares_including_center()
    }

    fn number_of_visible_relative_squares(&self) -> u32 {
        self.visible_relative_squares_including_center().len() as u32
    }

    fn number_of_fully_visible_relative_squares(&self) -> u32 {
        self.fully_visible_relative_squares().len() as u32
    }

    fn visible_local_relative_squares(&self) -> StepSet {
        self.filtered(true, true, true)
            .visible_relative_squares_including_center()
    }
    fn visible_relative_squares_including_center(&self) -> StepSet {
        self.map_of_top_down_portal_shapes_by_coordinates
            .iter()
            .map(|(coord, vis)| coord.0)
            .collect()
    }

    fn relative_square_is_fully_visible(&self, step: impl Into<WorldStep>) -> bool {
        let step = step.into();
        let top_down_portals = self.top_down_portals_for_relative_square(step);
        return top_down_portals.len() == 1
            && top_down_portals[0].shape_in_exit_frame.is_fully_visible();
    }

    fn relative_square_is_only_partially_visible(&self, step: WorldStep) -> bool {
        self.relative_square_is_visible(step) && !self.relative_square_is_fully_visible(step)
    }
    // TODO: does name imply that the square is not also visible through any portals?
    fn relative_square_is_only_locally_visible(&self, step: WorldStep) -> bool {
        let top_down_views = self.top_down_portals_for_relative_square(step);
        !top_down_views.is_empty()
            && top_down_views
                .into_iter()
                .all(|portal| portal.portal_depth() == 0)
    }

    fn relative_square_is_visible(&self, step: impl Into<WorldStep>) -> bool {
        let step = step.into();
        self.visible_relative_squares_including_center()
            .contains(&step)
    }
    fn lone_portal_depth_for_relative_square_or_panic(&self, relative_square: WorldStep) -> u32 {
        todo!()
    }
    fn lone_portal_rotation_for_relative_square_or_panic(
        &self,
        relative_square: WorldStep,
    ) -> QuarterTurnsCcw {
        todo!()
    }
    fn lone_square_visibility_rotated_to_absolute_frame_for_relative_square_or_panic(
        &self,
        relative_square: WorldStep,
    ) -> SquareVisibility {
        let views = self.top_down_portals_for_relative_square(relative_square);
        if views.len() != 1 {
            panic!(
                "Relative square {} has {} views, not 1",
                relative_square.to_string(),
                views.len()
            );
        }
        views[0].shape_rotated_to_absolute_frame()
    }

    fn lone_square_visibility_rotated_to_relative_frame_for_relative_square_or_panic(
        &self,
        relative_square: WorldStep,
    ) -> SquareVisibility {
        todo!()
    }

    fn shapes_of_visibilities_of_relative_square_rotated_to_local_frame(
        &self,
        relative_square: WorldStep,
    ) -> Vec<TopDownPortalShape> {
        self.top_down_portals_for_relative_square(relative_square)
            .iter()
            .map(|x| x.shape_in_entrance_frame())
            .collect()
    }
    fn top_down_portals_for_relative_square(
        &self,
        relative_square: WorldStep,
    ) -> Vec<TopDownPortal> {
        self.map_of_top_down_portal_shapes_by_coordinates
            .iter()
            .filter(|(coord, vis)| coord.0 == relative_square)
            .map(|x| x.into())
            .collect()
    }
    // in main view, so no portals involved, so one-to-one assumption is valid
    fn visible_local_absolute_square_for_relative_square(
        &self,
        rel_square: WorldStep,
    ) -> Option<WorldSquare> {
        self.existing_direct_connection_to_local_square(rel_square)
            .map(|direct_connection| direct_connection.0.target.absolute_square)
    }

    fn absolute_squares_visible_at_relative_square(&self, relative_square: WorldStep) -> SquareSet {
        self.top_down_portals_for_relative_square(relative_square)
            .iter()
            .map(|portal| portal.target.absolute_square)
            .collect()
    }
    fn absolute_square_is_visible(&self, world_square: WorldSquare) -> bool {
        !self
            .top_down_portals_for_absolute_square(world_square)
            .is_empty()
    }

    fn top_down_portals_for_absolute_square(
        &self,
        absolute_square: WorldSquare,
    ) -> Vec<TopDownPortal> {
        self.map_of_top_down_portal_shapes_by_coordinates
            .iter()
            .filter(|(coords, vis)| coords.1.absolute_square == absolute_square)
            .map(|(&positioned_coords, &vis)| {
                TopDownPortal::new(positioned_coords.0, positioned_coords.1, vis)
            })
            .collect()
    }
    fn times_absolute_square_is_fully_visible(&self, absolute_square: WorldSquare) -> usize {
        self.top_down_portals_for_absolute_square(absolute_square)
            .into_iter()
            .filter(|positioned_top_down_portal| {
                positioned_top_down_portal
                    .shape_in_exit_frame
                    .is_fully_visible()
            })
            .count()
    }
    fn times_absolute_square_is_visible(&self, absolute_square: WorldSquare) -> usize {
        self.top_down_portals_for_absolute_square(absolute_square)
            .len()
    }

    /// Note: does not account for any field of view limitations of the portal.  The entire rasterized field of view is propagated through
    fn as_seen_through_portal_from_pose(
        &self,
        new_view_root: SquareWithOrthogonalDir,
        portal_transform_from_other_to_self: RigidTransform,
    ) -> Self {
        // self root is used only for finding how the top-down portals look from the new view root, and is otherwise discarded
        todo!();

        // Self::from_top_down_portal_iter(self.top_down_portal_iter().map(|top_down_portal| {
        //     top_down_portal.one_portal_deeper(portal_transform_from_other_to_self)
        // }))
    }

    fn combined_with(&self, other: &Self) -> Self {
        todo!()
    }

    fn rounded_towards_full_visibility(&self, tolerance: f32) -> Self {
        Self::from_top_down_portal_iter(
            self.view_root,
            self.top_down_portal_iter()
                .map(|x| x.rounded_towards_full_visibility(tolerance)),
        )
    }
}

// These are only the private functions, put the public ones in the trait for easier visibility
impl RasterizedFieldOfView {
    fn new_with_one_top_down_portal(
        view_root: impl Into<SquareWithOrthogonalDir>,
        top_down_portal: TopDownPortal,
    ) -> Self {
        let mut thing = Self::new_empty_with_view_root(view_root);
        thing.add_top_down_portal(top_down_portal);
        thing
    }
    fn new_empty_with_view_root(view_root: impl Into<SquareWithOrthogonalDir>) -> Self {
        Self {
            view_root: view_root.into(),
            map_of_top_down_portal_shapes_by_coordinates: Default::default(),
        }
    }
    fn add_partially_visible_local_relative_square(
        &mut self,
        step: WorldStep,
        vis: &TopDownPortalShape,
    ) {
        self.add_top_down_portal(
            self.new_direct_connection_to_local_square(step, vis)
                .top_down_portal(),
        );
    }
    fn add_top_down_portal(&mut self, portal: TopDownPortal) {
        let new_positioned_portal_target: LocallyPositionedTopDownPortalTarget =
            portal.positioned_target();
        let new_portal_shape: TopDownPortalShape = portal.shape_in_exit_frame;

        // TODO: put this part in its own function
        todo!();
        let existing_shapes = self
            .top_down_portals_for_relative_square(new_positioned_portal_target.0)
            .iter()
            .map(|portal| portal.shape_in_entrance_frame())
            .collect_vec();
        let found_overlap = existing_shapes
            .into_iter()
            .any(|existing_shape| new_portal_shape.overlaps(&existing_shape, 1e-5));
        assert!(!found_overlap);

        self.map_of_top_down_portal_shapes_by_coordinates
            .insert(new_positioned_portal_target, new_portal_shape);
    }

    fn can_see_local_relative_square(&self, rel_square: WorldStep) -> bool {
        self.map_of_top_down_portal_shapes_by_coordinates
            .keys()
            .any(|positioned_vis| {
                positioned_vis.1.portal_depth() == 0 && positioned_vis.0 == rel_square
            })
    }
    // main view, so one-to-one
    fn existing_direct_connection_to_local_square(
        &self,
        relative_square: WorldStep,
    ) -> Option<DirectConnectionToLocalSquare> {
        self.local_view_only()
            .map_of_top_down_portal_shapes_by_coordinates
            .iter()
            .find(|(positioned_target, shape)| positioned_target.0 == relative_square)
            .map(|(positioned_target, shape)| {
                DirectConnectionToLocalSquare::new(
                    positioned_target.0,
                    positioned_target.1.absolute_square,
                    shape,
                )
            })
    }
    fn from_top_down_portal_iter(
        view_root: SquareWithOrthogonalDir,
        iter: impl IntoIterator<Item = TopDownPortal>,
    ) -> Self {
        Self {
            view_root,
            map_of_top_down_portal_shapes_by_coordinates: iter
                .into_iter()
                .map(|x| x.split())
                .collect(),
        }
    }
    // TODO: delete this after the refactor (currently keeping for reference)
    // fn visibilities_of_relative_square_in_one_sub_view(
    //     &self,
    //     relative_square: WorldStep,
    //     sub_view: &FieldOfView,
    // ) -> Vec<LocallyPositionedNonOverlappingDrawTargetsFromOneSquare> {
    //     let view_transform_to_sub_view = self.view_transform_to(sub_view);
    //
    //     let rotation_moving_forward_through_portal: QuarterTurnsCcw =
    //         view_transform_to_sub_view.rotation();
    //
    //     let rotated_relative_square = rotated_n_quarter_turns_counter_clockwise(
    //         relative_square,
    //         rotation_moving_forward_through_portal.quarter_turns(),
    //     );
    //
    //     let visibilities_in_frame_of_sub_view =
    //         sub_view.visibilities_of_relative_square(rotated_relative_square);
    //
    //     let visibilities_in_frame_of_main_view = visibilities_in_frame_of_sub_view
    //         .iter()
    //         .map(|pos_vis: &LocallyPositionedNonOverlappingDrawTargetsFromOneSquare| {
    //             pos_vis.one_portal_deeper(rotation_moving_forward_through_portal)
    //         })
    //         .collect_vec();
    //
    //     visibilities_in_frame_of_main_view
    // }
    fn local_view_only(&self) -> Self {
        self.filtered(true, true, true)
    }
    fn set_root_square(&mut self, new_root: WorldSquare) {
        self.map_of_top_down_portal_shapes_by_coordinates.extend(
            SquareOfTopDownPortals::new_direct_local_connection(
                STEP_ZERO,
                new_root,
                &TopDownPortalShape::new_fully_visible(),
            )
            .0
            .map_of_top_down_portal_shapes_by_coordinates,
        );
    }
    fn new_centered_at(new_root: WorldSquare) -> Self {
        let mut new_fov = RasterizedFieldOfView::default();
        new_fov.set_root_square(new_root);
        new_fov
    }

    fn new_direct_connection_to_local_square(
        &self,
        relative_square: WorldStep,
        top_down_portal_shape: &TopDownPortalShape,
    ) -> DirectConnectionToLocalSquare {
        DirectConnectionToLocalSquare::new(
            relative_square,
            self.relative_to_local_absolute_square(relative_square),
            top_down_portal_shape,
        )
    }
    fn filtered(
        &self,
        local_only: bool,
        include_partially_visible: bool,
        include_fully_visible: bool,
    ) -> Self {
        let filtered_portal_map = self
            .portal_map()
            .iter()
            .filter(|(coord, vis)| {
                let is_local = coord.1.portal_depth == 0;
                if local_only && !is_local {
                    return false;
                }
                (include_partially_visible && vis.is_only_partially_visible())
                    || (include_fully_visible && vis.is_fully_visible())
            })
            .map(|x| x.tuple_clone())
            .collect();
        Self {
            map_of_top_down_portal_shapes_by_coordinates: filtered_portal_map,
            ..self.clone()
        }
    }

    fn portal_map(&self) -> &UniqueTopDownPortals {
        &self.map_of_top_down_portal_shapes_by_coordinates
    }

    fn visibility_map(&self) -> LocalSquareVisibilityMap {
        self.portal_map()
            .into_iter()
            .map(|(&coord, &vis)| (coord.0, vis))
            .collect()
    }

    fn portal_map_of_local_partially_visible_squares(&self) -> UniqueTopDownPortals {
        self.filtered(true, true, false).portal_map().clone()
    }

    fn relative_to_local_absolute_square(&self, relative_square: WorldStep) -> WorldSquare {
        self.root_square() + relative_square
    }

    fn top_down_portals(&self) -> Vec<TopDownPortal> {
        self.top_down_portal_iter().collect()
    }

    fn top_down_portal_iter(&self) -> impl Iterator<Item = TopDownPortal> + '_ {
        self.map_of_top_down_portal_shapes_by_coordinates
            .iter()
            .map(Into::<TopDownPortal>::into)
    }
}

impl TopDownPortal {
    fn new(
        rel_pos: impl Into<WorldStep>,
        target: TopDownPortalTarget,
        shape_in_exit_frame: TopDownPortalShape,
    ) -> Self {
        TopDownPortal {
            relative_position: rel_pos.into(),
            target,
            shape_in_exit_frame,
        }
    }
    fn split(&self) -> (LocallyPositionedTopDownPortalTarget, TopDownPortalShape) {
        (
            (self.relative_position, self.target),
            self.shape_in_exit_frame,
        )
    }
    fn one_portal_deeper(&self, forward_portal_rotation: QuarterTurnsCcw) -> Self {
        Self {
            target: self.target.one_portal_deeper(forward_portal_rotation),
            ..self.clone()
        }
    }
    pub fn shape_in_entrance_frame(&self) -> SquareVisibility {
        self.shape_in_exit_frame
            .rotated(-self.target.portal_rotation_to_target)
    }
    pub fn shape_rotated_to_absolute_frame(&self) -> SquareVisibility {
        self.shape_in_exit_frame
    }
    pub fn portal_depth(&self) -> u32 {
        self.target.portal_depth
    }
    pub fn portal_rotation_to_target(&self) -> QuarterTurnsCcw {
        self.target.portal_rotation_to_target
    }

    pub fn target_square(&self) -> WorldSquare {
        self.target.absolute_square
    }
    pub fn relative_position(&self) -> WorldStep {
        self.relative_position
    }
    fn positioned_target(&self) -> (WorldStep, TopDownPortalTarget) {
        (self.relative_position, self.target)
    }
    fn has_entrance_overlap_with(&self, other: Self) -> bool {
        todo!()
    }
    fn any_have_entrance_overlap(portals: impl IntoIterator<Item = Self>) -> bool {
        let has_portal_entrance_overlaps = portals
            .into_iter()
            .into_group_map_by(|top_down_portal| top_down_portal.relative_position())
            .iter()
            .any(|(rel_square, top_down_portals_for_square)| {
                let tolerance = 1e-5; // TODO: standardize
                top_down_portals_for_square
                    .iter()
                    .combinations(2)
                    .any(|portals| {
                        // TODO: check entrance vs exit frame overlap check is correct
                        portals[0]
                            .shape_in_exit_frame
                            .overlaps(&portals[1].shape_in_exit_frame, tolerance)
                    })
            });
        has_portal_entrance_overlaps
    }
}

impl TopDownPortalTarget {
    fn portal_depth(&self) -> u32 {
        self.portal_depth
    }
    fn new(
        absolute_square: impl Into<WorldSquare>,
        portal_depth: u32,
        portal_rotation_to_target: impl Into<QuarterTurnsCcw>,
    ) -> Self {
        Self {
            absolute_square: absolute_square.into(),
            portal_depth,
            portal_rotation_to_target: portal_rotation_to_target.into(),
        }
    }
    fn new_local(absolute_square: WorldSquare) -> Self {
        Self::new(absolute_square, 0, 0)
    }
    fn one_portal_deeper(&self, forward_rotation_through_portal: QuarterTurnsCcw) -> Self {
        Self {
            portal_depth: self.portal_depth + 1,
            portal_rotation_to_target: self.portal_rotation_to_target
                + forward_rotation_through_portal,
            ..self.clone()
        }
    }
}
// impl<T> CanBeLocallyPositioned for T
// where
//     LocallyPositioned<T>: HasLocalPosition,
// {
//     type LocallyPositionedSelf = LocallyPositioned<T>;
//     fn at(&self, local_relative_square: WorldStep) -> Self::LocallyPositionedSelf {
//         Self::LocallyPositionedSelf::position_locally(local_relative_square, self)
//     }
// }

// impl CanBeLocallyPositioned for TopDownPortalTarget {
//     type LocallyPositionedSelf = LocallyPositioned<TopDownPortalTarget>;

//     fn at(&self, local_relative_square: WorldStep) -> Self::LocallyPositionedSelf {
//         LocallyPositioned {
//             local_relative_square,
//             contents: *self,
//         }
//     }
// }

impl From<(WorldStep, TopDownPortalTarget, TopDownPortalShape)> for TopDownPortal {
    fn from(value: (WorldStep, TopDownPortalTarget, TopDownPortalShape)) -> Self {
        Self {
            relative_position: value.0,
            target: value.1,
            shape_in_exit_frame: value.2,
        }
    }
}
impl From<(WorldStep, TopDownPortalTarget, &TopDownPortalShape)> for TopDownPortal {
    fn from(value: (WorldStep, TopDownPortalTarget, &TopDownPortalShape)) -> Self {
        (value.0, value.1, value.2.clone()).into()
    }
}
impl From<(&(WorldStep, TopDownPortalTarget), &TopDownPortalShape)> for TopDownPortal {
    fn from(value: (&(WorldStep, TopDownPortalTarget), &TopDownPortalShape)) -> Self {
        value.tuple_clone().into()
    }
}
impl From<((WorldStep, TopDownPortalTarget), TopDownPortalShape)> for TopDownPortal {
    fn from(value: ((WorldStep, TopDownPortalTarget), TopDownPortalShape)) -> Self {
        (value.0 .0, value.0 .1, value.1).into()
    }
}

impl ViewRoundable for TopDownPortal {
    fn rounded_towards_full_visibility(&self, tolerance: f32) -> Self {
        TopDownPortal {
            shape_in_exit_frame: self
                .shape_in_exit_frame
                .rounded_towards_full_visibility(tolerance),
            ..self.clone()
        }
    }
}

impl DirectConnectionToLocalSquare {
    fn new(
        relative_position: WorldStep,
        target_square: WorldSquare,
        visibility: &SquareVisibility,
    ) -> Self {
        Self(TopDownPortal {
            relative_position,
            target: TopDownPortalTarget::new_local(target_square),
            shape_in_exit_frame: visibility.clone(),
        })
    }
    fn target_square(&self) -> WorldSquare {
        self.0.target.absolute_square
    }
    fn top_down_portal(&self) -> TopDownPortal {
        self.0.clone()
    }
}

impl From<DirectConnectionToLocalSquare> for SquareOfTopDownPortals {
    fn from(value: DirectConnectionToLocalSquare) -> Self {
        Self::from(value.top_down_portal())
    }
}
impl From<TopDownPortal> for SquareOfTopDownPortals {
    fn from(value: TopDownPortal) -> Self {
        Self::from_iter(vec![value])
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::utility::{
        halfplane::LocalSquareHalfPlane, RigidTransform, STEP_DOWN, STEP_RIGHT, STEP_UP,
    };
    use euclid::point2;
    use ntest::{assert_false, assert_true, timeout};

    #[test]
    fn test_center_square_is_always_visible() {
        let mut rasterized_fov = RasterizedFieldOfView::new_centered_at(point2(5, 5));
        assert_eq!(rasterized_fov.portal_map().len(), 1);
        assert_eq!(
            rasterized_fov
                .top_down_portals_for_relative_square(STEP_ZERO)
                .len(),
            1
        );
        assert_true!(rasterized_fov.relative_square_is_fully_visible(STEP_ZERO));
    }

    #[test]
    fn test_rounding_towards_full_visibility() {
        let mut rasterized_fov = RasterizedFieldOfView::new_centered_at(point2(5, 5));

        rasterized_fov.add_fully_visible_local_relative_square(STEP_RIGHT);

        rasterized_fov.add_partially_visible_local_relative_square(
            STEP_UP,
            &TopDownPortalShape::new_partially_visible(
                LocalSquareHalfPlane::top_half_plane().extended(0.5 - 1e-2),
            ),
        );
        rasterized_fov.add_partially_visible_local_relative_square(
            STEP_DOWN,
            &TopDownPortalShape::new_partially_visible(
                LocalSquareHalfPlane::top_half_plane().extended(0.5 - 1e-4),
            ),
        );
        assert_eq!(rasterized_fov.visible_local_relative_squares().len(), 4);
        assert_eq!(
            rasterized_fov.fully_visible_local_relative_squares().len(),
            2
        );
        assert_eq!(
            rasterized_fov
                .only_partially_visible_local_relative_squares()
                .len(),
            2
        );

        let rounded_fov = rasterized_fov.rounded_towards_full_visibility(1e-3);
        assert_eq!(rounded_fov.visible_local_relative_squares().len(), 4);
        assert_eq!(rounded_fov.fully_visible_local_relative_squares().len(), 3);
        assert_eq!(
            rounded_fov
                .only_partially_visible_local_relative_squares()
                .len(),
            1
        );
        assert!(rounded_fov
            .fully_visible_local_relative_squares()
            .contains(&STEP_DOWN));
        assert!(rounded_fov
            .only_partially_visible_local_relative_squares()
            .contains(&STEP_UP));
    }
    #[test]
    fn test_add_a_view_of_a_square() {
        let mut rasterized_fov = RasterizedFieldOfView::new_centered_at(point2(5, 5));

        rasterized_fov.add_top_down_portal(TopDownPortal {
            relative_position: (4, 5).into(),
            target: TopDownPortalTarget {
                absolute_square: (10, 10).into(),
                portal_depth: 1,
                portal_rotation_to_target: QuarterTurnsCcw::default(),
            },
            shape_in_exit_frame: TopDownPortalShape::new_fully_visible(),
        })
    }
    #[test]
    #[should_panic]
    fn test_add_a_view_of_a_square__that_overlaps_another_view() {
        let mut rasterized_fov = RasterizedFieldOfView::new_centered_at(point2(5, 5));

        rasterized_fov.add_top_down_portal(TopDownPortal {
            relative_position: (4, 5).into(),
            target: TopDownPortalTarget {
                absolute_square: (10, 10).into(),
                portal_depth: 1,
                portal_rotation_to_target: QuarterTurnsCcw::default(),
            },
            shape_in_exit_frame: TopDownPortalShape::new_fully_visible(),
        });
        rasterized_fov.add_top_down_portal(TopDownPortal {
            relative_position: (4, 5).into(),
            target: TopDownPortalTarget {
                absolute_square: (90, 10).into(),
                portal_depth: 1,
                portal_rotation_to_target: QuarterTurnsCcw::default(),
            },
            shape_in_exit_frame: TopDownPortalShape::new_top_half_visible(),
        });
    }
    #[test]
    #[should_panic]
    fn test_add_a_view_of_a_square__that_overlaps_another_view__slightly() {
        let mut rasterized_fov = RasterizedFieldOfView::new_centered_at(point2(5, 5));

        rasterized_fov.add_top_down_portal(TopDownPortal {
            relative_position: (4, 5).into(),
            target: TopDownPortalTarget {
                absolute_square: (10, 10).into(),
                portal_depth: 1,
                portal_rotation_to_target: QuarterTurnsCcw::default(),
            },
            shape_in_exit_frame: TopDownPortalShape::new_bottom_half_visible(),
        });
        rasterized_fov.add_top_down_portal(TopDownPortal {
            relative_position: (4, 5).into(),
            target: TopDownPortalTarget {
                absolute_square: (90, 10).into(),
                portal_depth: 1,
                portal_rotation_to_target: QuarterTurnsCcw::default(),
            },
            shape_in_exit_frame: TopDownPortalShape::from_visible_half_plane(
                LocalSquareHalfPlane::top_half_plane().extended(0.001),
            ),
        });
    }
    #[test]
    fn test_add_visible_local_relative_square() {
        let mut rfov = RasterizedFieldOfView::new_centered_at(point2(5, 5));
        let vis = SquareVisibility::new_top_half_visible();
        rfov.add_visible_local_relative_square(STEP_RIGHT, &vis);
        assert_eq!(rfov.number_of_visible_relative_squares(), 2);
        rfov.try_add_visible_local_relative_square(STEP_RIGHT * 2, &vis)
            .expect("");
        assert_eq!(rfov.number_of_visible_relative_squares(), 3);
        rfov.try_add_visible_local_relative_square(STEP_RIGHT, &vis)
            .expect_err("");
        assert_eq!(rfov.number_of_visible_relative_squares(), 3);
    }
    #[test]
    #[should_panic]
    fn test_no_overlapping_top_down_portal_entrances() {
        let portal1 = TopDownPortal::new(
            STEP_UP,
            TopDownPortalTarget {
                absolute_square: point2(5, 5),
                portal_depth: 3,
                portal_rotation_to_target: QuarterTurnsCcw::default(),
            },
            SquareVisibility::new_fully_visible(),
        );
        let portal2 = TopDownPortal::new(
            STEP_UP,
            TopDownPortalTarget {
                absolute_square: point2(5, 7),
                portal_depth: 5,
                portal_rotation_to_target: QuarterTurnsCcw::default(),
            },
            SquareVisibility::new_fully_visible(),
        );
        SquareOfTopDownPortals::from_iter(vec![portal1, portal2]);
    }
    #[test]
    #[should_panic]
    fn test_top_down_potals_in_one_square_are_in_the_same_square() {
        let portal1 = TopDownPortal::new(
            STEP_UP,
            TopDownPortalTarget {
                absolute_square: point2(5, 5),
                portal_depth: 3,
                portal_rotation_to_target: QuarterTurnsCcw::default(),
            },
            SquareVisibility::new_fully_visible(),
        );
        let portal2 = TopDownPortal::new(
            STEP_UP,
            TopDownPortalTarget {
                absolute_square: point2(5, 7),
                portal_depth: 5,
                portal_rotation_to_target: QuarterTurnsCcw::default(),
            },
            SquareVisibility::new_fully_visible(),
        );
        SquareOfTopDownPortals::from_iter(vec![portal1, portal2]);
    }
    #[test]
    fn test_top_down_portal_one_portal_deeper() {
        let top_down_portal = TopDownPortal::new(
            (5, 3),
            TopDownPortalTarget::new((20, 205), 5, 2),
            SquareVisibility::new_partially_visible(LocalSquareHalfPlane::down(0.0)),
        );

        let forward_tf_through_portal =
            RigidTransform::from_start_and_end_poses((3, 4, STEP_RIGHT), (5, 8, STEP_UP));

        assert_eq!(forward_tf_through_portal.rotation(), 1.into());
        assert_eq!(forward_tf_through_portal.translation(), (2, 4).into());

        let deeper_top_down_portal =
            top_down_portal.one_portal_deeper(forward_tf_through_portal.rotation());

        assert_eq!(
            deeper_top_down_portal.target.absolute_square,
            top_down_portal.target.absolute_square,
        );
        assert_eq!(
            deeper_top_down_portal.shape_in_exit_frame,
            top_down_portal.shape_in_exit_frame
        );
        assert_eq!(
            deeper_top_down_portal.target.portal_rotation_to_target,
            3.into()
        );
        assert_eq!(deeper_top_down_portal.target.portal_depth, 6);
        assert_eq!(
            deeper_top_down_portal.relative_position,
            top_down_portal.relative_position
        );
    }
    #[test]
    fn test_rasterized_field_of_view_with_one_square_seen_through_portal__identity_case() {
        let top_down_portal = TopDownPortal::new(
            (5, 3),
            TopDownPortalTarget::new((20, 205), 5, 2),
            SquareVisibility::new_partially_visible(LocalSquareHalfPlane::down(0.0)),
        );

        let old_fov_root_pose: SquareWithOrthogonalDir = (2, 1, STEP_UP).into();
        let new_fov_root_pose: SquareWithOrthogonalDir = old_fov_root_pose.clone();

        let old_rasterized_fov =
            RasterizedFieldOfView::new_with_one_top_down_portal(old_fov_root_pose, top_down_portal);

        let forward_tf_through_portal = RigidTransform::identity();

        assert_eq!(forward_tf_through_portal.rotation(), 0.into());
        assert_eq!(forward_tf_through_portal.translation(), (0, 0).into());

        let new_rasterized_fov = old_rasterized_fov
            .as_seen_through_portal_from_pose(new_fov_root_pose, forward_tf_through_portal);

        assert_eq!(new_rasterized_fov.top_down_portals().len(), 1);

        let deeper_top_down_portal = new_rasterized_fov.top_down_portal_iter().next().unwrap();

        assert_eq!(
            deeper_top_down_portal.target.absolute_square,
            top_down_portal.target.absolute_square,
        );
        assert_eq!(
            deeper_top_down_portal.shape_in_exit_frame,
            top_down_portal.shape_in_exit_frame
        );
        assert_eq!(
            deeper_top_down_portal.target.portal_rotation_to_target,
            top_down_portal.target.portal_rotation_to_target
        );
        assert_eq!(
            deeper_top_down_portal.target.portal_depth,
            top_down_portal.target.portal_depth + 1
        );
        assert_eq!(
            deeper_top_down_portal.relative_position,
            top_down_portal.relative_position
        );
    }
    #[test]
    fn test_rasterized_field_of_view_with_one_square_seen_through_portal__new_fov_root_translated()
    {
        todo!();
    }
    #[test]
    fn test_rasterized_field_of_view_with_one_square_seen_through_portal__portal_translated() {
        todo!();
    }
    #[test]
    fn test_rasterized_field_of_view_with_one_square_seen_through_portal__portal_moved_and_rotated()
    {
        todo!();
    }
    #[test]
    fn test_rasterized_field_of_view_with_one_square_seen_through_portal__portal_and_new_fov_root_both_moved_and_rotated(
    ) {
        todo!();
    }
    #[test]
    fn test_rasterized_field_of_view_with_one_square_seen_through_portal__translate_rotate_twice() {
        let top_down_portal = TopDownPortal::new(
            (5, 3),
            TopDownPortalTarget::new((20, 205), 5, 2),
            SquareVisibility::new_partially_visible(LocalSquareHalfPlane::down(0.0)),
        );

        let fov_center = (2, 1, STEP_UP);

        let fov = RasterizedFieldOfView::new_with_one_top_down_portal(fov_center, top_down_portal);

        let forward_tf_through_portal =
            RigidTransform::from_start_and_end_poses((3, 4, STEP_RIGHT), (5, 8, STEP_UP));

        assert_eq!(forward_tf_through_portal.rotation(), 1.into());
        assert_eq!(forward_tf_through_portal.translation(), (2, 4).into());

        let new_fov_root_pose: SquareWithOrthogonalDir = (5, 5, STEP_RIGHT).into();

        let new_rasterized_fov =
            fov.as_seen_through_portal_from_pose(new_fov_root_pose, forward_tf_through_portal);

        let deeper_top_down_portal = new_rasterized_fov.top_down_portal_iter().next().unwrap();

        assert_eq!(
            deeper_top_down_portal.target.absolute_square,
            top_down_portal.target.absolute_square,
        );
        assert_eq!(
            deeper_top_down_portal.shape_in_exit_frame,
            top_down_portal.shape_in_exit_frame
        );
        assert_eq!(
            deeper_top_down_portal.target.portal_rotation_to_target,
            3.into()
        );
        assert_eq!(deeper_top_down_portal.target.portal_depth, 6);
        assert_eq!(deeper_top_down_portal.relative_position, (-2, 4).into());
        todo!();
    }

    fn get_generic_top_down_portal() -> TopDownPortal {
        TopDownPortal::new(
            (5, 5),
            TopDownPortalTarget::new((20, 10), 3, 0),
            SquareVisibility::new_top_half_visible(),
        )
    }

    fn two_top_down_portals_with_overlapping_entrances_but_not_exits() -> Vec<TopDownPortal> {
        let mut p = get_generic_top_down_portal();
        let p1 = p.clone();
        p.target.portal_rotation_to_target += 1.into();
        p.target.absolute_square += STEP_DOWN;
        [p1, p].into()
    }
    // Note that the exits do overlap
    fn two_top_down_portals_on_one_square_with_overlapping_exits_but_not_entrances(
    ) -> Vec<TopDownPortal> {
        let mut p = get_generic_top_down_portal();
        let p1 = p.clone();
        p.target.portal_rotation_to_target += 2.into();
        [p1, p].into()
    }

    #[test]
    fn test_confirm_top_down_portal_overlap_test_data() {
        let overlapping = two_top_down_portals_with_overlapping_entrances_but_not_exits();
        let non_overlapping =
            two_top_down_portals_on_one_square_with_overlapping_exits_but_not_entrances();

        // TODO: test for exit overlaps
        assert_true!(overlapping[0].has_entrance_overlap_with(overlapping[1]));
        assert_false!(non_overlapping[0].has_entrance_overlap_with(non_overlapping[1]));
    }

    #[test]
    #[should_panic]
    fn test_prevent_top_down_portal_entrance_overlap__add_individually() {
        let portals = two_top_down_portals_with_overlapping_entrances_but_not_exits();
        let mut rfov = RasterizedFieldOfView::new_empty_with_view_root((5, 5, STEP_UP));
        portals
            .into_iter()
            .for_each(|p| rfov.add_top_down_portal(p))
    }
    #[test]
    #[should_panic]
    fn test_prevent_top_down_portal_entrance_overlap__add_all_at_once() {
        let portals = two_top_down_portals_with_overlapping_entrances_but_not_exits();
        RasterizedFieldOfView::from_top_down_portal_iter(Default::default(), portals);
    }
}
