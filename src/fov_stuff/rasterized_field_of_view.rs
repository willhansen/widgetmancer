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
struct DrawTargetCoordinates {
    absolute_square: WorldSquare,
    portal_depth: u32,
    portal_rotation_to_target: QuarterTurnsAnticlockwise,
}

#[derive(Clone, Copy, Hash, Eq, PartialEq, Debug)]
struct LocallyPositioned<T> {
    local_relative_square: WorldStep,
    contents: T,
}

trait PositionedLocally {
    fn local_relative_square(&self) -> WorldStep;
    fn set_local_relative_square(&mut self, rel_square: WorldStep);
    fn position_locally<T>(rel_square: WorldStep, contents: &T) -> LocallyPositioned<T>;
}

type LocallyPositionedDrawTargetCoordinates = LocallyPositioned<DrawTargetCoordinates>;

#[derive(Clone, Debug)]
struct DrawTarget {
    draw_target_coordinates: DrawTargetCoordinates,
    square_visibility_in_absolute_frame: SquareVisibility,
}

type LocallyPositionedDrawTarget = LocallyPositioned<DrawTarget>;

#[derive(Clone, Constructor, Debug)]
pub struct RasterizedFieldOfView(HashMap<LocallyPositionedDrawTargetCoordinates, SquareVisibility>);

#[derive(Clone, PartialEq, Debug, Default)]
struct NonOverlappingDrawTargetsFromOneSquare(HashMap<DrawTargetCoordinates, SquareVisibility>);

type LocallyPositionedNonOverlappingDrawTargetsFromOneSquare =
    LocallyPositioned<NonOverlappingDrawTargetsFromOneSquare>;

impl LocallyPositionedNonOverlappingDrawTargetsFromOneSquare {
    pub fn one_portal_deeper(
        &self,
        forward_rotation_through_portal: QuarterTurnsAnticlockwise,
    ) -> Self {
        LocallyPositionedNonOverlappingDrawTargetsFromOneSquare {
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
    fn lone_positioned_draw_target_or_panic(&self) -> LocallyPositionedDrawTarget {
        LocallyPositioned::<DrawTarget>::position_locally(
            self.local_relative_square,
            self.contents.lone_draw_target_or_panic(),
        )
    }
    pub fn lone_square_visibility_in_absolute_frame_or_panic(&self) -> SquareVisibility {
        self.contents
            .lone_draw_target_or_panic()
            .square_visibility_in_absolute_frame
    }
    pub fn lone_portal_depth_or_panic(&self) -> u32 {
        self.contents
            .lone_draw_target_or_panic()
            .draw_target_coordinates
            .portal_depth
    }
    pub fn lone_portal_rotation_or_panic(&self) -> QuarterTurnsAnticlockwise {
        self.contents
            .lone_draw_target_or_panic()
            .draw_target_coordinates
            .portal_rotation_to_target
    }
    pub fn lone_absolute_square_or_panic(&self) -> WorldSquare {
        self.contents
            .lone_draw_target_or_panic()
            .draw_target_coordinates
            .absolute_square
    }
    pub fn relative_square(&self) -> WorldStep {
        self.local_relative_square
    }
    pub fn new_in_main_view(
        square_visibility: &SquareVisibility,
        absolute_square: WorldSquare,
        relative_square: WorldStep,
    ) -> Self {
        Self {
            local_relative_square: relative_square,
            contents: NonOverlappingDrawTargetsFromOneSquare::new_local_draw_target(
                square_visibility,
                absolute_square,
            ),
        }
    }
    pub fn square_visibility_in_relative_frame(&self) -> SquareVisibility {
        self.square_visibility_in_absolute_frame
            .rotated(-self.portal_rotation)
    }
}

impl ViewRoundable for LocallyPositionedNonOverlappingDrawTargetsFromOneSquare {
    fn rounded_towards_full_visibility(&self, tolerance: f32) -> Self {
        Self {
            square_visibility_in_absolute_frame: self
                .square_visibility_in_absolute_frame
                .rounded_towards_full_visibility(tolerance),
            ..self.clone()
        }
    }
}

impl RasterizedFieldOfView {
    fn new_centered_at(new_root: WorldSquare) -> Self {
        let mut new_thing = RasterizedFieldOfView(vec![]);
        new_thing
            .add_visible_square_in_main_view(STEP_ZERO, &SquareVisibility::new_fully_visible());
        new_thing
    }
    fn has_visibility_for_position_in_main_view(&self, rel_square: WorldStep) -> bool {
        self.0.iter().any(|positioned_vis| {
            positioned_vis.portal_depth == 0 && positioned_vis.relative_square == rel_square
        })
    }
    fn add_visible_square(
        &mut self,
        visibility: &LocallyPositionedNonOverlappingDrawTargetsFromOneSquare,
    ) {
        todo!()
    }
    pub fn add_visible_square_in_main_view(
        &mut self,
        relative_square: WorldStep,
        visibility: &SquareVisibility,
    ) {
        // make sure there isn't a positioned visibility in the main view in the same relative square already
        assert!(!self.has_visibility_for_position_in_main_view(relative_square));

        self.0.push(
            LocallyPositionedNonOverlappingDrawTargetsFromOneSquare::new_in_main_view(
                visibility,
                self.root_square() + relative_square,
                relative_square,
            ),
        );
    }
    pub fn positioned_visibilities(
        &self,
    ) -> &Vec<LocallyPositionedNonOverlappingDrawTargetsFromOneSquare> {
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
    ) -> Vec<LocallyPositionedNonOverlappingDrawTargetsFromOneSquare> {
        self.0
            .iter()
            .filter(|vis| vis.absolute_square == world_square)
            .cloned()
            .collect()
    }

    pub fn root_square(&self) -> WorldSquare {
        // Will panic if no visible square at relative origin
        self.absolute_square_from_relative_square_in_main_view(STEP_ZERO)
            .unwrap()
    }

    pub(crate) fn visibilities_of_relative_square(
        &self,
        relative_square: WorldStep,
    ) -> Vec<LocallyPositionedNonOverlappingDrawTargetsFromOneSquare> {
        self.0
            .iter()
            .filter(|vis| vis.relative_square == relative_square)
            .cloned()
            .collect()
    }

    pub fn at_least_partially_visible_relative_squares_including_subviews(&self) -> StepSet {
        self.0
            .iter()
            .map(|positioned_visibility| positioned_visibility.relative_square)
            .collect()
    }
    pub fn at_least_partially_visible_relative_squares_in_main_view_only(&self) -> StepSet {
        self.0
            .iter()
            .filter(|positioned_visibility| positioned_visibility.portal_depth == 0)
            .map(|positioned_visibility| positioned_visibility.relative_square)
            .collect()
    }

    pub fn only_partially_visible_relative_squares_in_main_view_only(&self) -> StepSet {
        self.positioned_visibilities_of_only_partially_visible_squares_in_main_view_only()
            .iter()
            .map(|positioned_visibility| positioned_visibility.local_relative_square)
            .collect()
    }
    // TODO: these names are getting long.  parameterize the logic.
    fn positioned_visibilities_of_only_partially_visible_squares_in_main_view_only(
        &self,
    ) -> Vec<&LocallyPositionedNonOverlappingDrawTargetsFromOneSquare> {
        self.0
            .iter()
            .filter(|positioned_visibility| positioned_visibility.portal_depth == 0)
            .filter(|positioned_visibility| {
                positioned_visibility
                    .square_visibility_in_absolute_frame
                    .is_only_partially_visible()
            })
            .collect()
    }
    pub fn visibilities_of_partially_visible_squares_in_main_view_only(
        &self,
    ) -> RelativeSquareVisibilityMap {
        self.positioned_visibilities_of_only_partially_visible_squares_in_main_view_only()
            .iter()
            .map(|positioned_visibility| {
                (
                    positioned_visibility.relative_square(),
                    positioned_visibility.square_visibility_in_relative_frame(),
                )
            })
            .collect()
    }

    pub fn can_fully_and_seamlessly_see_relative_square(&self, step: WorldStep) -> bool {
        let visibility = self.visibilities_of_relative_square(step);
        return visibility.len() == 1
            && visibility
                .get(0)
                .unwrap()
                .lone_square_visibility_in_absolute_frame_or_panic()
                .is_fully_visible();
    }

    pub fn can_see_relative_square(&self, step: WorldStep) -> bool {
        !self.visibilities_of_relative_square(step).is_empty()
    }

    pub fn can_see_absolute_square(&self, world_square: WorldSquare) -> bool {
        !self
            .visibilities_of_absolute_square(world_square)
            .is_empty()
    }

    pub fn visibility_of_relative_square_in_main_view(
        &self,
        rel_square: WorldStep,
    ) -> Option<LocallyPositionedNonOverlappingDrawTargetsFromOneSquare> {
        self.0
            .iter()
            .find(|vis| vis.relative_square == rel_square)
            .cloned()
    }

    pub fn absolute_square_from_relative_square_in_main_view(
        &self,
        rel_square: WorldStep,
    ) -> Option<WorldSquare> {
        self.0
            .iter()
            .filter(|vis| vis.relative_square() == rel_square)
            .filter(|vis| vis.portal_depth == 0)
            .map(|vis| vis.absolute_square())
            .next()
    }

    pub fn relative_square_visibility_map_of_main_view_only(&self) -> RelativeSquareVisibilityMap {
        todo!();
    }

    // TODO: delete this after the refactor (currently keeping for reference)
    // fn visibilities_of_relative_square_in_one_sub_view(
    //     &self,
    //     relative_square: WorldStep,
    //     sub_view: &FieldOfView,
    // ) -> Vec<PositionedVisibilityOfSquare> {
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
    //         .map(|pos_vis: &PositionedVisibilityOfSquare| {
    //             pos_vis.one_portal_deeper(rotation_moving_forward_through_portal)
    //         })
    //         .collect_vec();
    //
    //     visibilities_in_frame_of_main_view
    // }
    pub fn fully_visible_relative_squares_in_main_view_only(&self) -> StepSet {
        self.0
            .iter()
            .filter(|vis| vis.portal_depth() == 0)
            .filter(|vis| vis.square_visibility_in_absolute_frame.is_fully_visible())
            .map(|vis| vis.relative_square())
            .collect()
    }
    pub fn fully_visible_relative_squares_including_subviews(&self) -> StepSet {
        self.0
            .iter()
            .filter(|vis| vis.square_visibility_in_absolute_frame.is_fully_visible())
            .map(|vis| vis.relative_square())
            .collect()
    }
    pub fn add_fully_visible_relative_square(&mut self, step: WorldStep) {
        self.0.push(
            LocallyPositionedNonOverlappingDrawTargetsFromOneSquare::new_in_main_view(
                &SquareVisibility::new_fully_visible(),
                self.root_square() + step,
                step,
            ),
        );
    }
    pub fn add_partially_visible_relative_square(
        &mut self,
        step: WorldStep,
        vis: &SquareVisibility,
    ) {
        self.0.push(
            LocallyPositionedNonOverlappingDrawTargetsFromOneSquare::new_in_main_view(
                vis,
                self.root_square() + step,
                step,
            ),
        );
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

impl DrawTarget {
    pub fn one_portal_deeper(
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

impl NonOverlappingDrawTargetsFromOneSquare {
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
    pub fn new_local_draw_target(
        visibility: &SquareVisibility,
        absolute_square: WorldSquare,
    ) -> Self {
        Self(HashMap::from([(
            DrawTargetCoordinates::new_local(absolute_square),
            visibility,
        )]))
    }
    fn lone_draw_target_or_panic(&self) -> &DrawTarget {
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

impl DrawTargetCoordinates {
    fn new_local(absolute_square: WorldSquare) -> Self {
        Self {
            absolute_square,
            portal_depth: 0,
            portal_rotation_to_target: QuarterTurnsAnticlockwise::default(),
        }
    }
}

impl<T: Clone> PositionedLocally for LocallyPositioned<T> {
    fn local_relative_square(&self) -> WorldStep {
        self.local_relative_square
    }

    fn set_local_relative_square(&mut self, rel_square: WorldStep) {
        self.local_relative_square = rel_square;
    }
    fn position_locally(rel_square: WorldStep, contents: &T) -> LocallyPositioned<T> {
        LocallyPositioned {
            local_relative_square: rel_square,
            contents: contents.clone(),
        }
    }
}

impl From<(DrawTargetCoordinates, SquareVisibility)> for DrawTarget {
    fn from(value: (DrawTargetCoordinates, SquareVisibility)) -> Self {
        Self {
            draw_target_coordinates: value.0,
            square_visibility_in_absolute_frame: value.1,
        }
    }
}
impl From<(DrawTargetCoordinates, &SquareVisibility)> for DrawTarget {
    fn from(value: (DrawTargetCoordinates, &SquareVisibility)) -> Self {
        (value.0, value.1.clone()).into()
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
        let mut rasterized_fov = RasterizedFieldOfView::new_centered_at(point2(5, 5));
        assert_eq!(rasterized_fov.positioned_visibilities().len(), 1);
        assert_eq!(
            rasterized_fov
                .visibilities_of_relative_square(STEP_ZERO)
                .len(),
            1
        );
        assert_true!(rasterized_fov.visibilities_of_relative_square(STEP_ZERO)[0]
            .square_visibility_in_relative_frame()
            .is_fully_visible());
    }

    #[test]
    fn test_rounding_towards_full_visibility() {
        let mut rasterized_fov = RasterizedFieldOfView::new_centered_at(point2(5, 5));

        rasterized_fov.add_fully_visible_relative_square(STEP_RIGHT);

        rasterized_fov.add_partially_visible_relative_square(
            STEP_UP,
            &SquareVisibility::new_partially_visible(
                LocalSquareHalfPlane::top_half_plane().extended(0.5 - 1e-2),
            ),
        );
        rasterized_fov.add_partially_visible_relative_square(
            STEP_DOWN,
            &SquareVisibility::new_partially_visible(
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
        let mut rasterized_fov = RasterizedFieldOfView::new_centered_at(point2(5, 5));

        rasterized_fov.add_visible_square(
            &LocallyPositionedNonOverlappingDrawTargetsFromOneSquare {
                contents: todo!(),
                square_visibility_in_absolute_frame: SquareVisibility::new_fully_visible(),
                local_relative_square: (4, 5).into(),
                absolute_square: (10, 10).into(),
                portal_depth: 1,
                portal_rotation: QuarterTurnsAnticlockwise::default(),
            },
        )
    }
}
