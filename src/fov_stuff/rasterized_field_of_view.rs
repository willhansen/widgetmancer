use crate::fov_stuff::square_visibility::{
    RelativeSquareVisibilityMap, RelativeSquareVisibilityTrait, SquareVisibility, ViewRoundable,
};
use crate::glyph::glyph_constants::RED;
use crate::graphics::drawable::{
    Drawable, DrawableEnum, PartialVisibilityDrawable, SolidColorDrawable,
};
use crate::utility::coordinate_frame_conversions::{StepSet, WorldSquare, WorldStep};
use crate::utility::{
    king_distance, number_to_hue_rotation, rotated_n_quarter_turns_counter_clockwise,
    QuarterTurnRotatable, QuarterTurnsAnticlockwise, TupleClone, STEP_ZERO,
};
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

#[derive(Clone, Copy, Hash, Eq, PartialEq, Debug)]
struct LocallyPositioned<T> {
    local_relative_square: WorldStep,
    contents: T,
}

trait HasLocalPosition {}

trait CanBeLocallyPositioned {
    type LocallyPositionedSelf;
    fn at(&self, local_relative_square: WorldStep) -> Self::LocallyPositionedSelf;
}

#[derive(Clone, Debug)]
struct TopDownPortal {
    target: TopDownPortalTarget,
    shape: TopDownPortalShape,
}

// Key metaphor is that the portal is no longer from player to square, it is now screen to square, in a top-down fashion, so it can be rendered correctly.
#[derive(Clone, Default, Constructor, Debug)]
pub struct TopDownifiedFieldOfView(ShapesByTopDownPortalTargets);

type TopDownPortalShape = SquareVisibility;

type ShapesByTopDownPortalTargets =
    HashMap<LocallyPositioned<TopDownPortalTarget>, TopDownPortalShape>;
type ShapesByTopDownPortalTargetsSharingOneRelativeSquare =
    HashMap<TopDownPortalTarget, TopDownPortalShape>;

#[derive(Clone, Debug)]
struct DirectConnectionToLocalSquare(TopDownPortal);

#[derive(Clone, PartialEq, Debug, Default)]
struct NonOverlappingShapesByTopDownPortalTargetsSharingOneRelativeSquare(
    ShapesByTopDownPortalTargetsSharingOneRelativeSquare,
);

type OneRelativeSquareOfTopDownPortals =
    LocallyPositioned<NonOverlappingShapesByTopDownPortalTargetsSharingOneRelativeSquare>;

impl OneRelativeSquareOfTopDownPortals {
    pub fn one_portal_deeper(
        &self,
        forward_rotation_through_portal: QuarterTurnsAnticlockwise,
    ) -> Self {
        OneRelativeSquareOfTopDownPortals {
            contents: self
                .contents
                .one_portal_deeper(forward_rotation_through_portal),
            local_relative_square: rotated_n_quarter_turns_counter_clockwise(
                self.local_relative_square,
                -forward_rotation_through_portal.quarter_turns(),
            ),
            ..self.clone()
        }
    }
    fn lone_positioned_draw_target_or_panic(&self) -> LocallyPositioned<TopDownPortal> {
        self.contents
            .lone_top_down_portal_or_panic()
            .at(self.local_relative_square)
    }
    pub fn lone_square_visibility_in_absolute_frame_or_panic(&self) -> TopDownPortalShape {
        self.contents.lone_top_down_portal_or_panic().shape
    }
    pub fn lone_portal_depth_or_panic(&self) -> u32 {
        self.contents
            .lone_top_down_portal_or_panic()
            .target
            .portal_depth
    }
    pub fn lone_portal_rotation_or_panic(&self) -> QuarterTurnsAnticlockwise {
        self.contents
            .lone_top_down_portal_or_panic()
            .target
            .portal_rotation_to_target
    }
    pub fn lone_absolute_square_or_panic(&self) -> WorldSquare {
        self.contents
            .lone_top_down_portal_or_panic()
            .target
            .absolute_square
    }
    pub fn relative_square(&self) -> WorldStep {
        self.local_relative_square
    }
    pub fn new_in_main_view(
        square_visibility: &TopDownPortalShape,
        absolute_square: WorldSquare,
        relative_square: WorldStep,
    ) -> Self {
        Self {
            local_relative_square: relative_square,
            contents:
                NonOverlappingShapesByTopDownPortalTargetsSharingOneRelativeSquare::from_local_draw_target(
                    square_visibility,
                    absolute_square,
                ),
        }
    }
    pub fn lone_square_visibility_in_relative_frame_or_panic(&self) -> TopDownPortalShape {
        self.lone_square_visibility_in_absolute_frame_or_panic()
            .rotated(-self.lone_portal_rotation_or_panic())
    }
    fn split_into_draw_target_coordinates_and_visibilities(
        &self,
    ) -> HashMap<LocallyPositioned<TopDownPortalTarget>, TopDownPortalShape> {
        self.contents
            .0
            .iter()
            .map(|(&a, &b)| (a.at(self.local_relative_square), b))
            .collect()
    }
}

impl ViewRoundable for OneRelativeSquareOfTopDownPortals {
    fn rounded_towards_full_visibility(&self, tolerance: f32) -> Self {
        Self {
            contents: self.contents.rounded_towards_full_visibility(tolerance),
            local_relative_square: self.local_relative_square,
        }
    }
}
impl ViewRoundable for NonOverlappingShapesByTopDownPortalTargetsSharingOneRelativeSquare {
    fn rounded_towards_full_visibility(&self, tolerance: f32) -> Self {
        Self(
            self.0
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
        )
    }
}

impl TopDownifiedFieldOfView {
    fn new_centered_at(new_root: WorldSquare) -> Self {
        let mut new_thing = TopDownifiedFieldOfView::default();
        new_thing
            .add_visible_square_in_main_view(STEP_ZERO, &TopDownPortalShape::new_fully_visible());
        new_thing
    }
    fn has_visibility_for_position_in_main_view(&self, rel_square: WorldStep) -> bool {
        self.0.keys().any(|positioned_vis| {
            positioned_vis.portal_depth() == 0 && positioned_vis.local_relative_square == rel_square
        })
    }
    fn add_visible_square(&mut self, visibility: &OneRelativeSquareOfTopDownPortals) {
        todo!()
    }
    pub fn add_visible_square_in_main_view(
        &mut self,
        relative_square: WorldStep,
        visibility: &TopDownPortalShape,
    ) {
        // make sure there isn't a positioned visibility in the main view in the same relative square already
        assert!(!self.has_visibility_for_position_in_main_view(relative_square));

        self.0.extend(
            OneRelativeSquareOfTopDownPortals::new_in_main_view(
                visibility,
                self.root_square() + relative_square,
                relative_square,
            )
            .split_into_draw_target_coordinates_and_visibilities(),
        );
    }
    pub fn positioned_visibilities(
        &self,
    ) -> &HashMap<LocallyPositioned<TopDownPortalTarget>, TopDownPortalShape> {
        &self.0
    }
    pub fn from_visibility_map_of_main_view(
        root: WorldSquare,
        vis_map: &RelativeSquareVisibilityMap,
    ) -> Self {
        let mut new_thing = Self::new_centered_at(root);
        vis_map.iter().for_each(|(rel_square, visibility)| {
            new_thing.add_visible_square_in_main_view(*rel_square, visibility);
        });
        new_thing
    }

    pub(crate) fn visibilities_of_absolute_square(
        &self,
        world_square: WorldSquare,
    ) -> ShapesByTopDownPortalTargets {
        self.0
            .iter()
            .filter(|(coords, vis)| coords.contents.absolute_square == world_square)
            .map(|(&coords, &vis)| (coords, vis))
            .collect()
    }

    pub fn root_square(&self) -> WorldSquare {
        // Will panic if no visible square at relative origin
        self.visible_local_absolute_square(STEP_ZERO).unwrap()
    }

    pub(crate) fn visibilities_of_relative_square(
        &self,
        relative_square: WorldStep,
    ) -> Option<OneRelativeSquareOfTopDownPortals> {
        let visibilities_by_draw_coordinates: HashMap<_, _> = self
            .0
            .iter()
            .filter(|(coord, vis)| coord.local_relative_square == relative_square)
            .map(|(&coord, &vis)| (coord.contents, vis))
            .collect();
        if visibilities_by_draw_coordinates.is_empty() {
            None
        } else {
            Some(
                NonOverlappingShapesByTopDownPortalTargetsSharingOneRelativeSquare(
                    visibilities_by_draw_coordinates,
                )
                .at(relative_square),
            )
        }
    }

    pub fn at_least_partially_visible_relative_squares_including_subviews(&self) -> StepSet {
        self.0
            .iter()
            .map(|(coord, vis)| coord.local_relative_square)
            .collect()
    }

    fn main_view_only(&self) -> Self {
        let filtered_map = self
            .0
            .iter()
            .filter(|(coord, vis)| coord.contents.portal_depth == 0)
            .map(|(&a, &b)| (a, b))
            .collect();
        Self(filtered_map)
    }
    pub fn at_least_partially_visible_relative_squares_in_main_view_only(&self) -> StepSet {
        self.main_view_only()
            .0
            .iter()
            .map(|(coord, vis)| coord.local_relative_square)
            .collect()
    }

    pub fn only_partially_visible_relative_squares_in_main_view_only(&self) -> StepSet {
        self.positioned_visibilities_of_only_partially_visible_squares_in_main_view_only()
            .iter()
            .map(|(coord, vis)| coord.local_relative_square)
            .collect()
    }
    // TODO: these names are getting long.  parameterize the logic.
    fn positioned_visibilities_of_only_partially_visible_squares_in_main_view_only(
        &self,
    ) -> HashMap<LocallyPositioned<TopDownPortalTarget>, TopDownPortalShape> {
        self.main_view_only()
            .0
            .iter()
            .filter(|(coord, vis)| coord.contents.portal_depth == 0)
            .filter(|(coord, vis)| vis.is_only_partially_visible())
            .map(|(&a, &b)| (a, b))
            .collect()
    }
    pub fn visibilities_of_partially_visible_squares_in_main_view_only(
        &self,
    ) -> RelativeSquareVisibilityMap {
        self.positioned_visibilities_of_only_partially_visible_squares_in_main_view_only()
            .iter()
            .map(|(coord, &vis)| (coord.local_relative_square, vis))
            .collect()
    }

    pub fn can_fully_and_seamlessly_see_relative_square(&self, step: WorldStep) -> bool {
        let visibilities_of_rel_square = self.visibilities_of_relative_square(step);
        return visibilities_of_rel_square.is_some_and(|v| {
            v.contents.0.len() == 1 && v.contents.0.iter().next().unwrap().1.is_fully_visible()
        });
    }

    pub fn can_see_relative_square(&self, step: WorldStep) -> bool {
        !self.visibilities_of_relative_square(step).is_some()
    }

    pub fn can_see_absolute_square(&self, world_square: WorldSquare) -> bool {
        !self
            .visibilities_of_absolute_square(world_square)
            .is_empty()
    }

    // main view, so one-to-one
    fn existing_direct_connection_to_local_square(
        &self,
        relative_square: WorldStep,
    ) -> Option<DirectConnectionToLocalSquare> {
        self.main_view_only()
            .0
            .iter()
            .find(|(target, shape)| target.local_relative_square == relative_square)
            .map(|(target, shape)| {
                DirectConnectionToLocalSquare::new(target.contents.absolute_square, shape)
            })
    }

    fn new_direct_connection_to_local_square(
        &self,
        relative_square: WorldStep,
        top_down_portal_shape: &TopDownPortalShape,
    ) -> DirectConnectionToLocalSquare {
        DirectConnectionToLocalSquare::new(
            self.root_square() + relative_square,
            top_down_portal_shape,
        )
    }

    // in main view, so no portals involved, so one-to-one assumption is valid
    pub fn visible_local_absolute_square(&self, rel_square: WorldStep) -> Option<WorldSquare> {
        self.existing_direct_connection_to_local_square(rel_square)
            .map(|direct_connection| direct_connection.0.target.absolute_square)
    }

    pub fn relative_square_visibility_map_of_main_view_only(&self) -> RelativeSquareVisibilityMap {
        todo!();
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
    pub fn fully_visible_relative_squares_in_main_view_only(&self) -> StepSet {
        self.fully_visible_relative_squares(true)
    }
    pub fn fully_visible_relative_squares_including_subviews(&self) -> StepSet {
        self.fully_visible_relative_squares(false)
    }
    fn fully_visible_relative_squares(&self, local_space_only: bool) -> StepSet {
        if local_space_only {
            self.main_view_only()
        } else {
            *self
        }
        .0
        .iter()
        .filter(|(target, shape)| shape.is_fully_visible())
        .map(|(target, shape)| target.local_relative_square)
        .collect()
    }
    fn add_top_down_portal(&mut self, relative_position: WorldStep, portal: &TopDownPortal) {
        let positioned_portal_target: LocallyPositioned<TopDownPortalTarget> =
            portal.target.at(relative_position);
        let portal_shape: TopDownPortalShape = portal.shape;
        self.0.insert(positioned_portal_target, portal_shape);
    }
    pub fn add_fully_visible_local_relative_square(&mut self, relative_square: WorldStep) {
        self.add_top_down_portal(
            relative_square,
            &self
                .new_direct_connection_to_local_square(
                    relative_square,
                    &TopDownPortalShape::new_fully_visible(),
                )
                .as_top_down_portal(),
        );
    }
    pub fn add_partially_visible_local_relative_square(
        &mut self,
        step: WorldStep,
        vis: &TopDownPortalShape,
    ) {
        self.add_top_down_portal(
            step,
            &self
                .new_direct_connection_to_local_square(step, vis)
                .as_top_down_portal(),
        );
    }

    pub fn rounded_towards_full_visibility(&self, tolerance: f32) -> Self {
        Self(
            self.0
                .iter()
                .map(|x| {
                    let positioned_top_down_portal: LocallyPositioned<TopDownPortal> =
                        x.tuple_clone().into();
                    let rounded: LocallyPositioned<TopDownPortal> =
                        positioned_top_down_portal.rounded_towards_full_visibility(tolerance);
                    // TODO: tidy this up
                    let tuple = rounded.contents.tuple();
                    (
                        tuple.0.at(positioned_top_down_portal.local_relative_square),
                        tuple.1,
                    )
                })
                .collect(),
        )
    }
    pub fn combined_with(&self, other: &Self) -> Self {
        todo!()
    }
    pub fn as_seen_through_portal_by(&self, other: &Self) -> Self {
        todo!()
    }
}

impl TopDownPortal {
    pub fn one_portal_deeper(
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
    fn tuple(&self) -> (TopDownPortalTarget, TopDownPortalShape) {
        (self.target, self.shape)
    }
}

impl NonOverlappingShapesByTopDownPortalTargetsSharingOneRelativeSquare {
    // simple delegation
    pub fn one_portal_deeper(
        &self,
        forward_rotation_through_portal: QuarterTurnsAnticlockwise,
    ) -> Self {
        Self(
            self.0
                .iter()
                .map(|x| {
                    Into::<TopDownPortal>::into(x.tuple_clone())
                        .one_portal_deeper(forward_rotation_through_portal)
                        .tuple()
                })
                .collect(),
        )
    }
    pub fn from_local_draw_target(
        visibility: &TopDownPortalShape,
        absolute_square: WorldSquare,
    ) -> Self {
        DirectConnectionToLocalSquare::new(absolute_square, visibility).into()
    }
    fn lone_top_down_portal_or_panic(&self) -> TopDownPortal {
        let draw_target_count = self.0.iter().count();
        if draw_target_count == 1 {
            self.0.iter().next().unwrap().tuple_clone().into()
        } else if draw_target_count > 1 {
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
    fn new(portals: impl Into<ShapesByTopDownPortalTargetsSharingOneRelativeSquare>) -> Self {
        todo!("assert shapes are non overlapping");
        Self(portals.into())
    }
}

impl TopDownPortalTarget {
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
impl<T> CanBeLocallyPositioned for T {
    type LocallyPositionedSelf = LocallyPositioned<T>;

    fn at(&self, local_relative_square: WorldStep) -> Self::LocallyPositionedSelf {
        LocallyPositioned {
            local_relative_square,
            contents: *self,
        }
    }
}

impl<T> HasLocalPosition for LocallyPositioned<T> {}

impl From<(TopDownPortalTarget, TopDownPortalShape)> for TopDownPortal {
    fn from(value: (TopDownPortalTarget, TopDownPortalShape)) -> Self {
        Self {
            target: value.0,
            shape: value.1,
        }
    }
}
impl From<(TopDownPortalTarget, &TopDownPortalShape)> for TopDownPortal {
    fn from(value: (TopDownPortalTarget, &TopDownPortalShape)) -> Self {
        (value.0, value.1.clone()).into()
    }
}
impl<A, B, C> From<(LocallyPositioned<A>, B)> for LocallyPositioned<C>
where
    C: From<(A, B)> + CanBeLocallyPositioned<LocallyPositionedSelf = LocallyPositioned<C>>,
{
    fn from(value: (LocallyPositioned<A>, B)) -> Self {
        let c: C = (value.0.contents, value.1).into();
        c.at(value.0.local_relative_square)
    }
}

impl LocallyPositioned<TopDownPortalTarget> {
    pub fn portal_depth(&self) -> u32 {
        self.contents.portal_depth
    }
}
impl ViewRoundable for LocallyPositioned<TopDownPortal> {
    fn rounded_towards_full_visibility(&self, tolerance: f32) -> Self {
        self.contents
            .rounded_towards_full_visibility(tolerance)
            .at(self.local_relative_square)
    }
}
impl ViewRoundable for TopDownPortal {
    fn rounded_towards_full_visibility(&self, tolerance: f32) -> Self {
        TopDownPortal {
            target: self.target,
            shape: self.shape.rounded_towards_full_visibility(tolerance),
        }
    }
}

impl DirectConnectionToLocalSquare {
    fn new(absolute_square: WorldSquare, visibility: &SquareVisibility) -> Self {
        Self(TopDownPortal {
            target: TopDownPortalTarget::new_local(absolute_square),
            shape: visibility.clone(),
        })
    }
    fn target_square(&self) -> WorldSquare {
        self.0.target.absolute_square
    }
    fn as_top_down_portal(&self) -> TopDownPortal {
        self.0
    }
}

impl From<DirectConnectionToLocalSquare>
    for NonOverlappingShapesByTopDownPortalTargetsSharingOneRelativeSquare
{
    fn from(value: DirectConnectionToLocalSquare) -> Self {
        Self::new(value)
    }
}
impl From<DirectConnectionToLocalSquare> for ShapesByTopDownPortalTargetsSharingOneRelativeSquare {
    fn from(value: DirectConnectionToLocalSquare) -> Self {
        Self::from(value.as_top_down_portal())
    }
}
impl From<TopDownPortal> for ShapesByTopDownPortalTargetsSharingOneRelativeSquare {
    fn from(value: TopDownPortal) -> Self {
        Self::from([value.tuple()])
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
                .visibilities_of_relative_square(STEP_ZERO)
                .unwrap()
                .contents
                .0
                .len(),
            1
        );
        assert_true!(rasterized_fov.can_fully_and_seamlessly_see_relative_square(STEP_ZERO));
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
        assert_eq!(
            rasterized_fov
                .at_least_partially_visible_relative_squares_in_main_view_only()
                .len(),
            4
        );
        assert_eq!(
            rasterized_fov
                .fully_visible_relative_squares_in_main_view_only()
                .len(),
            2
        );
        assert_eq!(
            rasterized_fov
                .only_partially_visible_relative_squares_in_main_view_only()
                .len(),
            2
        );

        let rounded_fov = rasterized_fov.rounded_towards_full_visibility(1e-3);
        assert_eq!(
            rounded_fov
                .at_least_partially_visible_relative_squares_in_main_view_only()
                .len(),
            4
        );
        assert_eq!(
            rounded_fov
                .fully_visible_relative_squares_in_main_view_only()
                .len(),
            3
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
    fn test_add_a_view_of_a_square() {
        let mut rasterized_fov = TopDownifiedFieldOfView::new_centered_at(point2(5, 5));

        rasterized_fov.add_visible_square(&OneRelativeSquareOfTopDownPortals {
            local_relative_square: (4, 5).into(),
            contents: NonOverlappingShapesByTopDownPortalTargetsSharingOneRelativeSquare::new(
                TopDownPortal {
                    target: TopDownPortalTarget {
                        absolute_square: (10, 10).into(),
                        portal_depth: 1,
                        portal_rotation_to_target: QuarterTurnsAnticlockwise::default(),
                    },
                    shape: TopDownPortalShape::new_fully_visible(),
                },
            ),
        })
    }
}
