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
    QuarterTurnRotatable, QuarterTurnsAnticlockwise, STEP_ZERO,
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

trait HasLocalPosition {
    type Contents;
    fn local_relative_square(&self) -> WorldStep;
    fn set_local_relative_square(&mut self, rel_square: WorldStep);
    fn position_locally(
        rel_square: WorldStep,
        contents: &Self::Contents,
    ) -> LocallyPositioned<Self::Contents>;
}

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
pub struct TopDownifiedFieldOfView(ShapesForTopDownPortals);

type TopDownPortalShape = SquareVisibility;

type ShapesForTopDownPortals = HashMap<LocallyPositioned<TopDownPortalTarget>, TopDownPortalShape>;
type ShapesForTopDownPortalsSharingOneRelativeSquare =
    HashMap<TopDownPortalTarget, TopDownPortalShape>;

#[derive(Clone, Debug)]
struct DirectConnectionToLocalSquare(TopDownPortal);

#[derive(Clone, PartialEq, Debug, Default)]
struct NonOverlappingShapesForTopDownPortalsSharingOneRelativeSquare(
    ShapesForTopDownPortalsSharingOneRelativeSquare,
);

type OneRelativeSquareOfTopDownPortals =
    LocallyPositioned<NonOverlappingShapesForTopDownPortalsSharingOneRelativeSquare>;

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
            .lone_draw_target_or_panic()
            .at(self.local_relative_square)
    }
    pub fn lone_square_visibility_in_absolute_frame_or_panic(&self) -> TopDownPortalShape {
        self.contents.lone_draw_target_or_panic().shape
    }
    pub fn lone_portal_depth_or_panic(&self) -> u32 {
        self.contents
            .lone_draw_target_or_panic()
            .target
            .portal_depth
    }
    pub fn lone_portal_rotation_or_panic(&self) -> QuarterTurnsAnticlockwise {
        self.contents
            .lone_draw_target_or_panic()
            .target
            .portal_rotation_to_target
    }
    pub fn lone_absolute_square_or_panic(&self) -> WorldSquare {
        self.contents
            .lone_draw_target_or_panic()
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
            contents: NonOverlappingShapesForTopDownPortalsSharingOneRelativeSquare::from_local_draw_target(
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
impl ViewRoundable for NonOverlappingShapesForTopDownPortalsSharingOneRelativeSquare {
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
    ) -> ShapesForTopDownPortals {
        self.0
            .iter()
            .filter(|(coords, vis)| coords.contents.absolute_square == world_square)
            .map(|(&coords, &vis)| (coords, vis))
            .collect()
    }

    pub fn root_square(&self) -> WorldSquare {
        // Will panic if no visible square at relative origin
        self.local_absolute_square(STEP_ZERO).unwrap()
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
                NonOverlappingShapesForTopDownPortalsSharingOneRelativeSquare(
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
    fn direct_connection_to_local_square(
        &self,
        relative_square: WorldStep,
    ) -> Option<LocallyPositioned<DirectConnectionToLocalSquare>> {
        self.main_view_only()
            .0
            .iter()
            .find(|(target, shape)| target.local_relative_square == relative_square)
            .map(|(target, shape)| {
                DirectConnectionToLocalSquare::new(target.contents.absolute_square, shape)
                    .at(relative_square)
            })
    }

    // in main view, so no portals involved, so one-to-one assumption is valid
    pub fn local_absolute_square(&self, rel_square: WorldStep) -> Option<WorldSquare> {
        self.direct_connection_to_local_square(rel_square)
            .map(|direct_connection| direct_connection.contents.target_square())
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
        .map(|(target, shape)| target.local_relative_square())
        .collect()
    }
    pub fn add_fully_visible_relative_square(&mut self, step: WorldStep) {
        self.0
            .push(OneRelativeSquareOfTopDownPortals::new_in_main_view(
                &TopDownPortalShape::new_fully_visible(),
                self.root_square() + step,
                step,
            ));
    }
    pub fn add_partially_visible_relative_square(
        &mut self,
        step: WorldStep,
        vis: &TopDownPortalShape,
    ) {
        self.0
            .push(OneRelativeSquareOfTopDownPortals::new_in_main_view(
                vis,
                self.root_square() + step,
                step,
            ));
    }

    pub fn rounded_towards_full_visibility(&self, tolerance: f32) -> Self {
        Self(
            self.0
                .iter()
                .map(|x| x.rounded_towards_full_visibility(tolerance))
                .collect_vec(),
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
}

impl NonOverlappingShapesForTopDownPortalsSharingOneRelativeSquare {
    // simple delegation
    pub fn one_portal_deeper(
        &self,
        forward_rotation_through_portal: QuarterTurnsAnticlockwise,
    ) -> Self {
        Self(
            self.0
                .iter()
                .map(|x| x.one_portal_deeper(forward_rotation_through_portal))
                .collect_vec(),
        )
    }
    pub fn from_local_draw_target(
        visibility: &TopDownPortalShape,
        absolute_square: WorldSquare,
    ) -> Self {
        Self(HashMap::from([(
            TopDownPortalTarget::new_local(absolute_square),
            visibility,
        )]))
    }
    fn lone_draw_target_or_panic(&self) -> &TopDownPortal {
        let draw_target_count = self.0.iter().count();
        if draw_target_count == 1 {
            self.0.iter().next().unwrap().into()
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
impl<T> CanBeLocallyPositioned for T
where
    LocallyPositioned<Self>: HasLocalPosition,
{
    type LocallyPositionedSelf = LocallyPositioned<T>;
    fn at(&self, local_relative_square: WorldStep) -> Self::LocallyPositionedSelf {
        Self::LocallyPositionedSelf::position_locally(local_relative_square, self)
    }
}

impl<T: Clone> HasLocalPosition for LocallyPositioned<T> {
    type Contents = T;
    fn local_relative_square(&self) -> WorldStep {
        self.local_relative_square
    }

    fn set_local_relative_square(&mut self, rel_square: WorldStep) {
        self.local_relative_square = rel_square;
    }
    fn position_locally(
        rel_square: WorldStep,
        contents: &Self::Contents,
    ) -> LocallyPositioned<Self::Contents> {
        LocallyPositioned {
            local_relative_square: rel_square,
            contents: contents.clone(),
        }
    }
}

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
impl LocallyPositioned<TopDownPortalTarget> {
    pub fn portal_depth(&self) -> u32 {
        self.contents.portal_depth
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
                .len(),
            1
        );
        assert_true!(rasterized_fov.visibilities_of_relative_square(STEP_ZERO)[0]
            .lone_square_visibility_in_relative_frame_or_panic()
            .is_fully_visible());
    }

    #[test]
    fn test_rounding_towards_full_visibility() {
        let mut rasterized_fov = TopDownifiedFieldOfView::new_centered_at(point2(5, 5));

        rasterized_fov.add_fully_visible_relative_square(STEP_RIGHT);

        rasterized_fov.add_partially_visible_relative_square(
            STEP_UP,
            &TopDownPortalShape::new_partially_visible(
                LocalSquareHalfPlane::top_half_plane().extended(0.5 - 1e-2),
            ),
        );
        rasterized_fov.add_partially_visible_relative_square(
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
            contents:
                NonOverlappingShapesForTopDownPortalsSharingOneRelativeSquare::from_draw_target(
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
