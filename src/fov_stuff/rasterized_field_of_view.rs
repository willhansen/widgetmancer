use crate::fov_stuff::square_visibility::{
    RelativeSquareVisibilityMap, RelativeSquareVisibilityTrait, SquareVisibility, ViewRoundable,
};
use crate::glyph::glyph_constants::RED;
use crate::graphics::drawable::{
    Drawable, DrawableEnum, PartialVisibilityDrawable, SolidColorDrawable,
};
use crate::utility::coordinate_frame_conversions::{SquareSet, StepSet, WorldSquare, WorldStep};
use crate::utility::{
    king_distance, number_to_hue_rotation, rotated_n_quarter_turns_counter_clockwise,
    CoordToString, QuarterTurnRotatable, QuarterTurnsAnticlockwise, TupleClone, STEP_ZERO,
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
    portal_rotation_to_target: QuarterTurnsAnticlockwise,
}
type PositionedTopDownPortalTarget = (WorldStep, TopDownPortalTarget);

#[derive(Clone, Debug)]
pub struct TopDownPortal {
    relative_position: WorldStep,
    target: TopDownPortalTarget,
    shape: TopDownPortalShape,
}

// Key metaphor is that the portal is no longer from player to square, it is now screen to square, in a top-down fashion, so it can be rendered correctly.
// This struct and trait are the only public things in this module
// TODO: maybe precalculate indexes
#[derive(Clone, Default, Debug)]
pub struct TopDownifiedFieldOfView {
    map_of_top_down_portal_shapes_by_coordinates: UniqueTopDownPortals,
}

pub trait TopDownifiedFieldOfViewInterface {
    fn absolute_square_is_visible(&self, world_square: WorldSquare) -> bool;
    fn absolute_squares_visible_at_relative_square(&self, relative_square: WorldStep) -> SquareSet;
    fn add_fully_visible_local_relative_square(&mut self, relative_square: WorldStep);
    fn add_visible_local_relative_square(
        &mut self,
        relative_square: WorldStep,
        visibility: &TopDownPortalShape,
    );
    fn as_seen_through_portal_by(&self, other: &Self) -> Self;
    fn combined_with(&self, other: &Self) -> Self;
    fn from_visibility_map_of_main_view(
        root: WorldSquare,
        vis_map: &RelativeSquareVisibilityMap,
    ) -> Self;
    fn fully_visible_local_relative_squares(&self) -> StepSet;
    fn fully_visible_relative_squares(&self) -> StepSet;
    fn lone_portal_depth_for_relative_square_or_panic(&self, relative_square: WorldStep) -> u32;
    fn lone_portal_rotation_for_relative_square_or_panic(
        &self,
        relative_square: WorldStep,
    ) -> QuarterTurnsAnticlockwise;
    fn lone_square_visibility_rotated_to_absolute_frame_for_relative_square_or_panic(
        &self,
        relative_square: WorldStep,
    ) -> SquareVisibility;
    fn lone_square_visibility_rotated_to_relative_frame_for_relative_square_or_panic(
        &self,
        relative_square: WorldStep,
    ) -> SquareVisibility;
    fn number_of_visible_relative_squares(&self) -> u32;
    fn only_partially_visible_local_relative_squares(&self) -> StepSet;
    fn relative_square_is_fully_visible(&self, step: WorldStep) -> bool;
    fn relative_square_is_only_locally_visible(&self, step: WorldStep) -> bool;
    fn relative_square_is_visible(&self, relative_square: WorldStep) -> bool;
    fn root_square(&self) -> WorldSquare;
    fn rounded_towards_full_visibility(&self, tolerance: f32) -> Self;
    fn shapes_of_visibilities_of_relative_square_rotated_to_local_frame(
        &self,
        relative_square: WorldStep,
    ) -> Vec<TopDownPortalShape>;
    fn times_absolute_square_is_fully_visible(&self, absolute_square: WorldSquare) -> usize;
    fn times_absolute_square_is_visible(&self, absolute_square: WorldSquare) -> usize;

    fn top_down_portals_for_absolute_square(
        &self,
        absolute_square: WorldSquare,
    ) -> Vec<TopDownPortal>;
    fn top_down_portals_for_relative_square(
        &self,
        relative_square: WorldStep,
    ) -> Vec<TopDownPortal>;
    fn visibilities_of_partially_visible_squares_in_main_view_only(
        &self,
    ) -> RelativeSquareVisibilityMap;
    fn visibility_map_of_local_relative_squares(&self) -> RelativeSquareVisibilityMap;
    fn visible_local_absolute_square_for_relative_square(
        &self,
        rel_square: WorldStep,
    ) -> Option<WorldSquare>;
    fn visible_local_relative_squares(&self) -> StepSet;

    fn visible_relative_squares(&self) -> StepSet;
}
type TopDownPortalShape = SquareVisibility;

type UniqueTopDownPortals = HashMap<PositionedTopDownPortalTarget, TopDownPortalShape>;

#[derive(Clone, Debug)]
struct SquareOfTopDownPortals {
    map_of_top_down_portal_shapes_by_coordinates: UniqueTopDownPortals,
}

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
    fn lone_portal_rotation_or_panic(&self) -> QuarterTurnsAnticlockwise {
        self.lone_top_down_portal_or_panic()
            .target
            .portal_rotation_to_target
    }
    fn lone_square_visibility_rotated_to_absolute_frame_or_panic(&self) -> TopDownPortalShape {
        self.lone_top_down_portal_or_panic().shape
    }
    fn lone_square_visibility_rotated_to_relative_frame_or_panic(&self) -> TopDownPortalShape {
        self.lone_top_down_portal_or_panic()
            .shape_rotated_to_relative_frame()
    }

    fn lone_top_down_portal_or_panic(&self) -> TopDownPortal {
        let portal_count = self
            .map_of_top_down_portal_shapes_by_coordinates
            .iter()
            .count();
        if portal_count == 1 {
            self.map_of_top_down_portal_shapes_by_coordinates
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

    fn one_portal_deeper(
        &self,
        forward_rotation_through_portal: QuarterTurnsAnticlockwise,
    ) -> Self {
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
        self.map_of_top_down_portal_shapes_by_coordinates
            .keys()
            .next()
            .unwrap()
            .0
    }
    fn top_down_portals(&self) -> Vec<TopDownPortal> {
        self.map_of_top_down_portal_shapes_by_coordinates
            .iter()
            .map(|x| x.into())
            .collect()
    }
    fn from_vec(vec: Vec<TopDownPortal>) -> Self {
        todo!()
    }
}

impl ViewRoundable for SquareOfTopDownPortals {
    fn rounded_towards_full_visibility(&self, tolerance: f32) -> Self {
        Self {
            map_of_top_down_portal_shapes_by_coordinates: self
                .map_of_top_down_portal_shapes_by_coordinates
                .iter()
                .map(
                    |(&draw_target_coordinates, &square_visibility_in_absolute_frame)| {
                        (
                            draw_target_coordinates,
                            square_visibility_in_absolute_frame
                                .rounded_towards_full_visibility(tolerance),
                        )
                    },
                )
                .collect(),
        }
    }
}

impl TopDownifiedFieldOfViewInterface for TopDownifiedFieldOfView {
    fn absolute_square_is_visible(&self, world_square: WorldSquare) -> bool {
        !self
            .top_down_portals_for_absolute_square(world_square)
            .is_empty()
    }
    fn absolute_squares_visible_at_relative_square(&self, relative_square: WorldStep) -> SquareSet {
        self.top_down_portals_for_relative_square(relative_square)
            .iter()
            .map(|portal| portal.target.absolute_square)
            .collect()
    }

    fn add_fully_visible_local_relative_square(&mut self, relative_square: WorldStep) {
        self.add_top_down_portal(
            &self
                .new_direct_connection_to_local_square(
                    relative_square,
                    &TopDownPortalShape::new_fully_visible(),
                )
                .top_down_portal(),
        );
    }

    fn add_visible_local_relative_square(
        &mut self,
        relative_square: WorldStep,
        visibility: &TopDownPortalShape,
    ) {
        // make sure there isn't a positioned visibility in the main view in the same relative square already
        assert!(!self.can_see_local_relative_square(relative_square));

        self.map_of_top_down_portal_shapes_by_coordinates.extend(
            SquareOfTopDownPortals::new_direct_local_connection(
                relative_square,
                self.relative_to_local_absolute_square(relative_square),
                visibility,
            )
            .map_of_top_down_portal_shapes_by_coordinates,
        );
    }

    fn as_seen_through_portal_by(&self, other: &Self) -> Self {
        todo!()
    }

    fn combined_with(&self, other: &Self) -> Self {
        todo!()
    }
    fn from_visibility_map_of_main_view(
        root: WorldSquare,
        vis_map: &RelativeSquareVisibilityMap,
    ) -> Self {
        let mut new_thing = Self::new_centered_at(root);
        vis_map.iter().for_each(|(rel_square, visibility)| {
            new_thing.add_visible_local_relative_square(*rel_square, visibility);
        });
        new_thing
    }

    fn fully_visible_local_relative_squares(&self) -> StepSet {
        self.fully_visible_relative_squares(true)
    }

    fn fully_visible_relative_squares(&self) -> StepSet {
        self.fully_visible_relative_squares(false)
    }

    fn lone_portal_depth_for_relative_square_or_panic(&self, relative_square: WorldStep) -> u32 {
        todo!()
    }

    fn lone_portal_rotation_for_relative_square_or_panic(
        &self,
        relative_square: WorldStep,
    ) -> QuarterTurnsAnticlockwise {
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

    fn number_of_visible_relative_squares(&self) -> u32 {
        self.visible_relative_squares().len() as u32
    }

    fn only_partially_visible_local_relative_squares(&self) -> StepSet {
        self.positioned_visibilities_of_only_partially_visible_squares_in_main_view_only()
            .iter()
            .map(|(coord, vis)| coord.0)
            .collect()
    }

    fn relative_square_is_fully_visible(&self, step: WorldStep) -> bool {
        let top_down_portals = self.top_down_portals_for_relative_square(step);
        return top_down_portals.len() == 1 && top_down_portals[0].shape.is_fully_visible();
    }
    // TODO: does name imply that the square is not also visible through any portals?
    fn relative_square_is_only_locally_visible(&self, step: WorldStep) -> bool {
        let top_down_views = self.top_down_portals_for_relative_square(step);
        !top_down_views.is_empty()
            && top_down_views
                .into_iter()
                .all(|portal| portal.portal_depth() == 0)
    }
    fn relative_square_is_visible(&self, step: WorldStep) -> bool {
        self.visible_relative_squares().contains(&step)
    }

    fn root_square(&self) -> WorldSquare {
        // Will panic if no visible square at relative origin
        self.visible_local_absolute_square_for_relative_square(STEP_ZERO)
            .unwrap()
    }

    fn rounded_towards_full_visibility(&self, tolerance: f32) -> Self {
        Self::from_top_down_portal_iter(
            self.top_down_portal_iter()
                .map(|x| x.rounded_towards_full_visibility(tolerance)),
        )
    }
    fn shapes_of_visibilities_of_relative_square_rotated_to_local_frame(
        &self,
        relative_square: WorldStep,
    ) -> Vec<TopDownPortalShape> {
        self.top_down_portals_for_relative_square(relative_square)
            .iter()
            .map(|x| x.shape_rotated_to_relative_frame())
            .collect()
    }
    fn times_absolute_square_is_fully_visible(&self, absolute_square: WorldSquare) -> usize {
        self.top_down_portals_for_absolute_square(absolute_square)
            .into_iter()
            .filter(|positioned_top_down_portal| {
                positioned_top_down_portal.shape.is_fully_visible()
            })
            .count()
    }

    fn times_absolute_square_is_visible(&self, absolute_square: WorldSquare) -> usize {
        self.top_down_portals_for_absolute_square(absolute_square)
            .len()
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
    fn visibilities_of_partially_visible_squares_in_main_view_only(
        &self,
    ) -> RelativeSquareVisibilityMap {
        self.positioned_visibilities_of_only_partially_visible_squares_in_main_view_only()
            .iter()
            .map(|(coord, &vis)| (coord.0, vis))
            .collect()
    }
    fn visibility_map_of_local_relative_squares(&self) -> RelativeSquareVisibilityMap {
        todo!();
    }

    // in main view, so no portals involved, so one-to-one assumption is valid
    fn visible_local_absolute_square_for_relative_square(
        &self,
        rel_square: WorldStep,
    ) -> Option<WorldSquare> {
        self.existing_direct_connection_to_local_square(rel_square)
            .map(|direct_connection| direct_connection.0.target.absolute_square)
    }

    fn visible_local_relative_squares(&self) -> StepSet {
        self.local_view_only()
            .map_of_top_down_portal_shapes_by_coordinates
            .iter()
            .map(|(coord, vis)| coord.0)
            .collect()
    }

    fn visible_relative_squares(&self) -> StepSet {
        self.map_of_top_down_portal_shapes_by_coordinates
            .iter()
            .map(|(coord, vis)| coord.0)
            .collect()
    }
}

impl TopDownifiedFieldOfView {
    fn add_partially_visible_local_relative_square(
        &mut self,
        step: WorldStep,
        vis: &TopDownPortalShape,
    ) {
        self.add_top_down_portal(
            &self
                .new_direct_connection_to_local_square(step, vis)
                .top_down_portal(),
        );
    }
    fn add_top_down_portal(&mut self, portal: &TopDownPortal) {
        let positioned_portal_target: PositionedTopDownPortalTarget = portal.positioned_target();
        let portal_shape: TopDownPortalShape = portal.shape;
        self.map_of_top_down_portal_shapes_by_coordinates
            .insert(positioned_portal_target, portal_shape);
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
    fn from_top_down_portal_iter(iter: impl Iterator<Item = TopDownPortal>) -> Self {
        todo!()
    }
    // TODO: delete this after the refactor (currently keeping for reference)
    // fn visibilities_of_relative_square_in_one_sub_view(
    //     &self,
    //     relative_square: WorldStep,
    //     sub_view: &FieldOfView,
    // ) -> Vec<LocallyPositionedNonOverlappingDrawTargetsFromOneSquare> {
    //     let view_transform_to_sub_view = self.view_transform_to(sub_view);
    //
    //     let rotation_moving_forward_through_portal: QuarterTurnsAnticlockwise =
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
    fn fully_visible_relative_squares(&self, local_space_only: bool) -> StepSet {
        if local_space_only {
            self.local_view_only()
        } else {
            self.clone()
        }
        .map_of_top_down_portal_shapes_by_coordinates
        .iter()
        .filter(|(positioned_target, shape)| shape.is_fully_visible())
        .map(|(positioned_target, shape)| positioned_target.0)
        .collect()
    }
    fn local_view_only(&self) -> Self {
        let filtered_map = self
            .map_of_top_down_portal_shapes_by_coordinates
            .iter()
            .filter(|(coord, vis)| coord.1.portal_depth == 0)
            .map(|(&a, &b)| (a, b))
            .collect();
        Self {
            map_of_top_down_portal_shapes_by_coordinates: filtered_map,
        }
    }
    fn set_root_square(&mut self, new_root: WorldSquare) {
        self.map_of_top_down_portal_shapes_by_coordinates.extend(
            SquareOfTopDownPortals::new_direct_local_connection(
                STEP_ZERO,
                new_root,
                &TopDownPortalShape::new_fully_visible(),
            )
            .map_of_top_down_portal_shapes_by_coordinates,
        );
    }
    fn new_centered_at(new_root: WorldSquare) -> Self {
        let mut new_fov = TopDownifiedFieldOfView::default();
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
    fn positioned_visibilities(&self) -> &UniqueTopDownPortals {
        &self.map_of_top_down_portal_shapes_by_coordinates
    }

    // TODO: these names are getting long.  parameterize the logic.
    fn positioned_visibilities_of_only_partially_visible_squares_in_main_view_only(
        &self,
    ) -> UniqueTopDownPortals {
        self.local_view_only()
            .map_of_top_down_portal_shapes_by_coordinates
            .iter()
            .filter(|(coord, vis)| coord.1.portal_depth == 0)
            .filter(|(coord, vis)| vis.is_only_partially_visible())
            .map(|(&a, &b)| (a, b))
            .collect()
    }

    fn relative_to_local_absolute_square(&self, relative_square: WorldStep) -> WorldSquare {
        self.root_square() + relative_square
    }

    fn top_down_portal_iter(&self) -> impl Iterator<Item = TopDownPortal> + '_ {
        self.map_of_top_down_portal_shapes_by_coordinates
            .iter()
            .map(Into::<TopDownPortal>::into)
    }
}

impl TopDownPortal {
    fn new(rel_pos: WorldStep, target: TopDownPortalTarget, shape: TopDownPortalShape) -> Self {
        TopDownPortal {
            relative_position: rel_pos,
            target,
            shape,
        }
    }
    fn one_portal_deeper(
        &self,
        forward_rotation_through_portal: QuarterTurnsAnticlockwise,
    ) -> Self {
        Self {
            target: self
                .target
                .one_portal_deeper(forward_rotation_through_portal),
            ..self.clone()
        }
    }
    pub fn shape_rotated_to_relative_frame(&self) -> SquareVisibility {
        self.shape.rotated(-self.target.portal_rotation_to_target)
    }
    pub fn shape_rotated_to_absolute_frame(&self) -> SquareVisibility {
        self.shape
    }
    pub fn portal_depth(&self) -> u32 {
        self.target.portal_depth
    }
    pub fn portal_rotation_to_target(&self) -> QuarterTurnsAnticlockwise {
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
}

impl SquareOfTopDownPortals {}

impl TopDownPortalTarget {
    fn portal_depth(&self) -> u32 {
        self.portal_depth
    }
    fn new_local(absolute_square: WorldSquare) -> Self {
        Self {
            absolute_square,
            portal_depth: 0,
            portal_rotation_to_target: QuarterTurnsAnticlockwise::default(),
        }
    }
    fn one_portal_deeper(
        &self,
        forward_rotation_through_portal: QuarterTurnsAnticlockwise,
    ) -> Self {
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
            shape: value.2,
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
        (value.0 .0, value.0 .1, value.1.clone()).into()
    }
}

impl ViewRoundable for TopDownPortal {
    fn rounded_towards_full_visibility(&self, tolerance: f32) -> Self {
        TopDownPortal {
            shape: self.shape.rounded_towards_full_visibility(tolerance),
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
            shape: visibility.clone(),
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
        Self {
            map_of_top_down_portal_shapes_by_coordinates: UniqueTopDownPortals::from([(
                (value.relative_position, value.target),
                value.shape,
            )]),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::fov_stuff::square_visibility::LocalSquareHalfPlane;
    use crate::utility::{RigidTransform, STEP_DOWN, STEP_RIGHT, STEP_UP};
    use euclid::point2;
    use ntest::{assert_true, timeout};

    #[test]
    fn test_center_square_is_always_visible() {
        let mut rasterized_fov = TopDownifiedFieldOfView::new_centered_at(point2(5, 5));
        assert_eq!(rasterized_fov.positioned_visibilities().len(), 1);
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
        let mut rasterized_fov = TopDownifiedFieldOfView::new_centered_at(point2(5, 5));

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
        let mut rasterized_fov = TopDownifiedFieldOfView::new_centered_at(point2(5, 5));

        rasterized_fov.add_top_down_portal(&TopDownPortal {
            relative_position: (4, 5).into(),
            target: TopDownPortalTarget {
                absolute_square: (10, 10).into(),
                portal_depth: 1,
                portal_rotation_to_target: QuarterTurnsAnticlockwise::default(),
            },
            shape: TopDownPortalShape::new_fully_visible(),
        })
    }
    #[test]
    #[should_panic]
    fn test_no_overlapping_portals_in_one_square() {
        let portal1 = TopDownPortal::new(
            STEP_UP,
            TopDownPortalTarget {
                absolute_square: point2(5, 5),
                portal_depth: 3,
                portal_rotation_to_target: QuarterTurnsAnticlockwise::default(),
            },
            SquareVisibility::new_fully_visible(),
        );
        let portal2 = TopDownPortal::new(
            STEP_UP,
            TopDownPortalTarget {
                absolute_square: point2(5, 7),
                portal_depth: 5,
                portal_rotation_to_target: QuarterTurnsAnticlockwise::default(),
            },
            SquareVisibility::new_fully_visible(),
        );
        SquareOfTopDownPortals::from_vec(vec![portal1, portal2]);
    }
    #[test]
    #[should_panic]
    fn test_top_down_potals_in_one_square_are_in_the_same_square() {
        let portal1 = TopDownPortal::new(
            STEP_UP,
            TopDownPortalTarget {
                absolute_square: point2(5, 5),
                portal_depth: 3,
                portal_rotation_to_target: QuarterTurnsAnticlockwise::default(),
            },
            SquareVisibility::new_fully_visible(),
        );
        let portal2 = TopDownPortal::new(
            STEP_UP,
            TopDownPortalTarget {
                absolute_square: point2(5, 7),
                portal_depth: 5,
                portal_rotation_to_target: QuarterTurnsAnticlockwise::default(),
            },
            SquareVisibility::new_fully_visible(),
        );
        SquareOfTopDownPortals::from_vec(vec![portal1, portal2]);
    }
}
