use crate::fov_stuff::square_visibility::LocalSquareVisibilityMap;
use crate::fov_stuff::square_visibility::SquareVisibility;
use crate::fov_stuff::square_visibility::SquareVisibilityMapFunctions;
use crate::fov_stuff::square_visibility::{RelativeSquareVisibilityFunctions, ViewRoundable};
use crate::glyph::glyph_constants::RED;
use crate::graphics::drawable::{
    Drawable, DrawableEnum, PartialVisibilityDrawable, SolidColorDrawable,
};
use crate::utility::coordinate_frame_conversions::ORIGIN_POSE;
use crate::utility::coordinate_frame_conversions::STEP_LEFT;
use crate::utility::coordinate_frame_conversions::STEP_UP;
use crate::utility::coordinate_frame_conversions::{SquareSet, StepSet, WorldSquare, WorldStep};
use crate::utility::general_utility::union;
use crate::utility::poses::SquareWithOrthogonalDir;
use crate::utility::poses::StepWithQuarterRotations;
use crate::utility::trait_alias_macro::function_short_name;
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
#[derive(PartialEq, Clone, Debug)]
pub struct RasterizedFieldOfView {
    pub view_root: SquareWithOrthogonalDir,
    map_of_top_down_portal_shapes_by_coordinates: UniqueTopDownPortals,
}

// these are the public functions.
pub trait RasterizedFieldOfViewFunctions {
    // creation
    fn from_local_visibility_map(
        root: impl Into<SquareWithOrthogonalDir>,
        vis_map: &LocalSquareVisibilityMap,
    ) -> Self;

    // adding
    fn add_fully_visible_local_relative_square(&mut self, relative_square: impl Into<WorldStep>);
    fn try_add_visible_local_relative_square(
        &mut self,
        relative_square: impl Into<WorldStep>,
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
    fn visible_relative_squares(&self) -> StepSet;

    // visible absolute squares
    // includes the local and target sides of the top-down portals
    fn visible_absolute_squares(&self) -> SquareSet;
    fn fully_visible_absolute_squares(&self) -> SquareSet;

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
    fn absolute_square_is_fully_visible(&self, world_square: impl Into<WorldSquare>) -> bool;
    fn top_down_portals_for_absolute_square(
        &self,
        absolute_square: impl Into<WorldSquare>,
    ) -> Vec<TopDownPortal>;
    fn times_absolute_square_is_fully_visible(
        &self,
        absolute_square: impl Into<WorldSquare>,
    ) -> usize;
    fn times_absolute_square_is_visible(&self, absolute_square: impl Into<WorldSquare>) -> usize;

    // modifying
    fn as_seen_through_portal_from_other_view_root(
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
    fn from_top_down_portals<T: IntoIterator<Item = TopDownPortal> + Clone>(iter: T) -> Self {
        // TODO: double check the use of default pose here
        RasterizedFieldOfView::from_top_down_portals(ORIGIN_POSE(), iter)
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
            .top_down_portals()
            .into_iter()
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
        Self::from_top_down_portals(
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
    fn from_local_visibility_map(
        root: impl Into<SquareWithOrthogonalDir>,
        vis_map: &LocalSquareVisibilityMap,
    ) -> Self {
        let mut new_thing = Self::new_empty_with_view_root(root);
        vis_map.iter().for_each(|(rel_square, visibility)| {
            new_thing.try_add_visible_local_relative_square(*rel_square, visibility);
        });
        new_thing
    }
    fn add_fully_visible_local_relative_square(&mut self, relative_square: impl Into<WorldStep>) {
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
        relative_square: impl Into<WorldStep>,
        visibility: &TopDownPortalShape,
    ) -> SimpleResult {
        let relative_square = relative_square.into();
        // make sure there isn't a positioned visibility in the main view in the same relative square already
        if self.can_see_local_relative_square(relative_square) {
            return Err(());
        }

        self.map_of_top_down_portal_shapes_by_coordinates.extend(
            SquareOfTopDownPortals::new_direct_local_connection(
                relative_square,
                self.relative_square_to_absolute_square(relative_square),
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
        self.view_root.square()
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
        self.filtered(false, false, true).visible_relative_squares()
    }

    fn fully_visible_local_relative_squares(&self) -> StepSet {
        self.filtered(true, false, true).visible_relative_squares()
    }

    fn only_partially_visible_local_relative_squares(&self) -> StepSet {
        self.filtered(true, true, false).visible_relative_squares()
    }

    fn number_of_visible_relative_squares(&self) -> u32 {
        self.visible_relative_squares().len() as u32
    }

    fn number_of_fully_visible_relative_squares(&self) -> u32 {
        self.fully_visible_relative_squares().len() as u32
    }

    fn visible_local_relative_squares(&self) -> StepSet {
        self.filtered(true, true, true).visible_relative_squares()
    }
    fn visible_relative_squares(&self) -> StepSet {
        self.map_of_top_down_portal_shapes_by_coordinates
            .iter()
            .map(|(coord, vis)| coord.0)
            .collect()
    }
    fn visible_absolute_squares(&self) -> SquareSet {
        self.map_of_top_down_portal_shapes_by_coordinates
            .iter()
            .map(|(coord, vis)| coord.1.absolute_square)
            .collect()
    }
    fn fully_visible_absolute_squares(&self) -> SquareSet {
        self.filtered(false, false, true).visible_absolute_squares()
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
        self.visible_relative_squares().contains(&step)
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
    fn absolute_square_is_fully_visible(&self, world_square: impl Into<WorldSquare>) -> bool {
        self.times_absolute_square_is_fully_visible(world_square) > 0
    }

    fn top_down_portals_for_absolute_square(
        &self,
        absolute_square: impl Into<WorldSquare>,
    ) -> Vec<TopDownPortal> {
        let absolute_square = absolute_square.into();
        self.map_of_top_down_portal_shapes_by_coordinates
            .iter()
            .filter(|(coords, vis)| coords.1.absolute_square == absolute_square)
            .map(|(&positioned_coords, &vis)| {
                TopDownPortal::new(positioned_coords.0, positioned_coords.1, vis)
            })
            .collect()
    }
    fn times_absolute_square_is_fully_visible(
        &self,
        absolute_square: impl Into<WorldSquare>,
    ) -> usize {
        self.top_down_portals_for_absolute_square(absolute_square)
            .into_iter()
            .filter(|positioned_top_down_portal| {
                positioned_top_down_portal
                    .shape_in_exit_frame
                    .is_fully_visible()
            })
            .count()
    }
    fn times_absolute_square_is_visible(&self, absolute_square: impl Into<WorldSquare>) -> usize {
        self.top_down_portals_for_absolute_square(absolute_square)
            .len()
    }

    /// Note: does not account for any field of view limitations of the portal.  The entire rasterized field of view is propagated through
    fn as_seen_through_portal_from_other_view_root(
        &self,
        new_view_root: SquareWithOrthogonalDir,
        portal_transform_from_other_to_self: RigidTransform,
    ) -> Self {
        // self root is used only for finding how the top-down portals look from the new view root, and is otherwise discarded

        let portalled = self.as_seen_through_portal_from_same_relative_position(
            portal_transform_from_other_to_self,
        );
        portalled.as_seen_from_other_local_view_root(new_view_root)
        // self.as_seen_through_portal_from_same_relative_position(portal_transform_from_other_to_self)
        //     .as_seen_from_other_local_view_root(new_view_root)
    }

    fn combined_with(&self, other: &Self) -> Self {
        // This check is unnecessary, self absorbs the other view.
        //assert_eq!(self.view_root, other.view_root);

        Self::from_top_down_portals(
            self.view_root,
            self.top_down_portals()
                .into_iter()
                .chain(other.top_down_portals()),
        )
    }

    fn rounded_towards_full_visibility(&self, tolerance: f32) -> Self {
        Self::from_top_down_portals(
            self.view_root,
            self.top_down_portals()
                .into_iter()
                .map(|x| x.rounded_towards_full_visibility(tolerance)),
        )
    }
}

// These are only the private functions, put the public ones in the trait for easier visibility
impl RasterizedFieldOfView {
    fn as_seen_through_portal_from_same_relative_position(
        &self,
        portal_transform_from_other_to_self: RigidTransform,
    ) -> Self {
        let tf_self_to_other = portal_transform_from_other_to_self.inverse();

        Self::from_top_down_portals(
            self.view_root.apply_rigid_transform(tf_self_to_other),
            self.top_down_portals()
                .into_iter()
                .map(|top_down_portal| top_down_portal.one_portal_deeper()),
        )
    }
    fn as_seen_from_other_local_view_root(
        &self,
        new_view_root: impl Into<SquareWithOrthogonalDir>,
    ) -> Self {
        let new_view_root = new_view_root.into();

        // tf is relative to the new view root
        let tf_new_to_old =
            RigidTransform::relative_transform_from_start_to_end(new_view_root, self.view_root);

        Self::from_top_down_portals(
            new_view_root,
            self.top_down_portals().into_iter().map(|top_down_portal| {
                top_down_portal.with_rigidly_transformed_entrance(tf_new_to_old)
            }),
        )
    }
    fn as_seen_from_oriented_origin(&self) -> Self {
        self.as_seen_from_other_local_view_root(ORIGIN_POSE())
    }
    fn new_with_one_top_down_portal(
        view_root: impl Into<SquareWithOrthogonalDir>,
        top_down_portal: TopDownPortal,
    ) -> Self {
        let mut thing = Self::new_empty_with_view_root(view_root);
        thing.add_top_down_portal(top_down_portal);
        thing
    }
    fn new_with_one_fully_visible_local_square(
        view_root: impl Into<SquareWithOrthogonalDir>,
        relative_square: impl Into<WorldStep>,
    ) -> Self {
        let mut fov = Self::new_empty_with_view_root(view_root);
        fov.add_fully_visible_local_relative_square(relative_square);
        fov
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

        assert!(!portal.has_entrance_overlap_with_any(
            self.top_down_portals_for_relative_square(new_positioned_portal_target.0)
        ));

        self.map_of_top_down_portal_shapes_by_coordinates
            .insert(new_positioned_portal_target, new_portal_shape);
    }
    fn from_view_root_and_one_direct_local_connection(
        view_root: impl Into<SquareWithOrthogonalDir>,
        rel_square: impl Into<WorldStep>,
    ) -> Self {
        let mut thing = Self::new_empty_with_view_root(view_root);
        thing
            .try_add_visible_local_relative_square(
                rel_square,
                &SquareVisibility::new_fully_visible(),
            )
            .unwrap();
        return thing;
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
    fn from_top_down_portals(
        view_root: impl Into<SquareWithOrthogonalDir>,
        iter: impl IntoIterator<Item = TopDownPortal> + Clone,
    ) -> Self {
        let view_root = view_root.into();

        let top_down_portals_with_conflicting_entrances: Vec<[TopDownPortal; 2]> =
            TopDownPortal::find_conflicting_entrances(iter.clone());

        assert!(
            top_down_portals_with_conflicting_entrances.is_empty(),
            "Failed to create rasterized field of view because of conflicting portal entrances: {:#?}",
            top_down_portals_with_conflicting_entrances
        );
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
    fn new_centered_at(new_root: impl Into<WorldSquare>) -> Self {
        RasterizedFieldOfView::new_empty_with_view_root((new_root, STEP_UP))
    }

    fn new_direct_connection_to_local_square(
        &self,
        relative_square: impl Into<WorldStep>,
        top_down_portal_shape: &TopDownPortalShape,
    ) -> DirectConnectionToLocalSquare {
        let relative_square = relative_square.into();
        DirectConnectionToLocalSquare::new(
            relative_square,
            self.relative_square_to_absolute_square(relative_square),
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

    pub fn relative_square_to_absolute_square(
        &self,
        relative_square: impl Into<WorldStep>,
    ) -> WorldSquare {
        let relative_square = relative_square.into();
        let tf_to_absolute_frame =
            RigidTransform::from_start_and_end_poses(self.view_root, ORIGIN_POSE()).inverse();
        relative_square
            .apply_rigid_transform(tf_to_absolute_frame)
            .to_point()
    }
    // TODO: untested
    pub fn absolute_square_to_relative_square(
        &self,
        absolute_square: impl Into<WorldSquare>,
    ) -> WorldStep {
        let absolute_square = absolute_square.into();
        let tf_to_relative_frame =
            RigidTransform::from_start_and_end_poses(self.view_root, ORIGIN_POSE());
        absolute_square
            .apply_rigid_transform(tf_to_relative_frame)
            .to_vector()
    }

    fn top_down_portals(&self) -> Vec<TopDownPortal> {
        self.map_of_top_down_portal_shapes_by_coordinates
            .iter()
            .map(Into::<TopDownPortal>::into)
            .collect()
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
    fn one_portal_deeper(&self) -> Self {
        Self {
            target: self.target.one_portal_deeper(),
            ..self.clone()
        }
    }
    pub fn with_rigidly_transformed_entrance(&self, tf: RigidTransform) -> Self {
        let mut the_copy = self.clone();
        the_copy.target.portal_rotation_to_target = self
            .target
            .portal_rotation_to_target
            .quarter_rotated_ccw(tf.rotation());
        // the_copy.relative_position = self.relative_position + tf.translation();
        the_copy.relative_position = self.relative_position.apply_rigid_transform(tf);
        the_copy
    }
    pub fn shape_in_entrance_frame(&self) -> SquareVisibility {
        self.shape_in_exit_frame
            .quarter_rotated_ccw(-self.target.portal_rotation_to_target)
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
        let tolerance = 1e-5; // TODO: standardize
        self.shape_in_entrance_frame()
            .overlaps(other.shape_in_entrance_frame(), tolerance)
    }
    fn has_entrance_overlap_with_any(&self, others: impl IntoIterator<Item = Self>) -> bool {
        others
            .into_iter()
            .any(|other_top_down_portal| self.has_entrance_overlap_with(other_top_down_portal))
    }
    fn find_conflicting_entrances(portals: impl IntoIterator<Item = Self>) -> Vec<[Self; 2]> {
        portals
            .into_iter()
            .into_group_map_by(|&top_down_portal| top_down_portal.relative_position())
            .into_iter()
            .flat_map(|(rel_square, top_down_portals_for_square)| {
                let conflicting_pairs_in_this_square: Vec<[TopDownPortal; 2]> =
                    top_down_portals_for_square
                        .iter()
                        .combinations(2)
                        .map(|v| [*v[0], *v[1]])
                        .filter(|&portals| {
                            // TODO: check entrance vs exit frame overlap check is correct
                            portals[0].has_entrance_overlap_with(portals[1])
                        })
                        .collect();
                conflicting_pairs_in_this_square
            })
            .collect()
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
    fn one_portal_deeper(&self) -> Self {
        Self {
            portal_depth: self.portal_depth + 1,
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
        relative_position: impl Into<WorldStep>,
        target_square: impl Into<WorldSquare>,
        visibility: &SquareVisibility,
    ) -> Self {
        Self(TopDownPortal {
            relative_position: relative_position.into(),
            target: TopDownPortalTarget::new_local(target_square.into()),
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
        Self::from_top_down_portals(vec![value])
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::utility::{
        coordinate_frame_conversions::STEP_LEFT, halfplane::LocalSquareHalfPlane, RigidTransform,
        STEP_DOWN, STEP_RIGHT, STEP_UP,
    };
    use euclid::point2;
    use ntest::{assert_false, assert_true, timeout};

    #[ignore = "Maybe don't want this"]
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
        rasterized_fov.add_fully_visible_local_relative_square(STEP_ZERO);

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
    fn test_local_coordinates_are_relative_to_root_pose_rotation() {
        let mut rfov = RasterizedFieldOfView::new_empty_with_view_root((5, 3, STEP_RIGHT));
        rfov.add_fully_visible_local_relative_square((-1, 2));
        assert!(rfov.absolute_square_is_fully_visible((7, 4)));
        assert_eq!(rfov.visible_relative_squares(), [(-1, 2).into()].into());
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
        assert_eq!(rfov.number_of_visible_relative_squares(), 1);
        rfov.try_add_visible_local_relative_square(STEP_RIGHT * 2, &vis)
            .expect("");
        assert_eq!(rfov.number_of_visible_relative_squares(), 2);
        rfov.try_add_visible_local_relative_square(STEP_RIGHT, &vis)
            .expect_err("");
        assert_eq!(rfov.number_of_visible_relative_squares(), 2);
    }
    #[test]
    #[should_panic]
    fn test_no_overlapping_top_down_portal_entrances__two_fully_visible() {
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
        SquareOfTopDownPortals::from_top_down_portals(vec![portal1, portal2]);
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
        SquareOfTopDownPortals::from_top_down_portals(vec![portal1, portal2]);
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

        let deeper_top_down_portal = top_down_portal.one_portal_deeper();

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

        let new_rasterized_fov = old_rasterized_fov.as_seen_through_portal_from_other_view_root(
            new_fov_root_pose,
            forward_tf_through_portal,
        );

        assert_eq!(new_rasterized_fov.top_down_portals().len(), 1);

        let deeper_top_down_portal = new_rasterized_fov.top_down_portals()[0];

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
        let top_down_portal = TopDownPortal::new(
            (5, 3),
            TopDownPortalTarget::new((20, 205), 5, 2),
            SquareVisibility::new_bottom_half_visible(),
        );

        let old_fov_root_pose: SquareWithOrthogonalDir = (2, 1, STEP_UP).into();
        let new_fov_root_pose: SquareWithOrthogonalDir =
            old_fov_root_pose.clone().with_offset(STEP_RIGHT * 2);

        let old_rasterized_fov =
            RasterizedFieldOfView::new_with_one_top_down_portal(old_fov_root_pose, top_down_portal);

        let forward_tf_through_portal = RigidTransform::identity();

        let new_rasterized_fov = old_rasterized_fov.as_seen_through_portal_from_other_view_root(
            new_fov_root_pose,
            forward_tf_through_portal,
        );

        assert_eq!(new_rasterized_fov.top_down_portals().len(), 1);

        let deeper_top_down_portal = new_rasterized_fov.top_down_portals()[0];
        assert_eq!(
            deeper_top_down_portal.relative_position,
            top_down_portal.relative_position + STEP_LEFT * 2
        );
    }
    #[test]
    fn test_rasterized_field_of_view_with_one_square_seen_through_portal__portal_moved_and_rotated()
    {
        let top_down_portal = TopDownPortal::new(
            (5, 3),
            TopDownPortalTarget::new((20, 205), 5, 2),
            SquareVisibility::new_partially_visible(LocalSquareHalfPlane::down(0.0)),
        );

        // top-down portal is at (2+5, 1+3) = (7,4)
        let old_fov_root_pose: SquareWithOrthogonalDir = (2, 1, STEP_UP).into();

        let new_fov_root_pose: SquareWithOrthogonalDir = old_fov_root_pose.clone();

        let old_rasterized_fov =
            RasterizedFieldOfView::new_with_one_top_down_portal(old_fov_root_pose, top_down_portal);

        // entrance is two steps ahead of the old_fov_root_pose (which is the new fov root as well, in this case)
        // exit is one step forward and one step right (portal is then behind you)
        let forward_tf_through_portal_new_to_old = RigidTransform::from_start_and_end_poses(
            old_fov_root_pose.stepped_n(2),
            old_fov_root_pose.stepped().turned_right(),
        );

        let new_rasterized_fov = old_rasterized_fov.as_seen_through_portal_from_other_view_root(
            new_fov_root_pose,
            forward_tf_through_portal_new_to_old,
        );

        // old root = (2,1)
        // rel square of test square = (5,3)
        // portal new-side entrance pose = (2,3)
        // portal old-side exit pose = (3,2, facing right)

        // new root = (2,1)
        // old root in new frame = (3, 3, face left)
        // absolute position of test square after transform = (0, 8, face left)
        // relative position of test square to new root after transform (-2, 7), and it faces left

        assert_eq!(new_rasterized_fov.top_down_portals().len(), 1);

        let absolute_deeper_top_down_portal = new_rasterized_fov
            .as_seen_from_oriented_origin()
            .top_down_portals()[0];
        assert_eq!(
            absolute_deeper_top_down_portal.relative_position,
            (0, 8).into()
        );

        let relative_deeper_top_down_portal = new_rasterized_fov.top_down_portals()[0];
        assert_eq!(
            relative_deeper_top_down_portal.relative_position,
            (-2, 7).into()
        );
    }
    #[test]
    fn test_rasterized_field_of_view_with_one_square_seen_through_portal__portal_and_new_fov_root_both_moved_and_rotated(
    ) {
        let top_down_portal = TopDownPortal::new(
            (5, 3),
            TopDownPortalTarget::new((20, 205), 5, 2),
            SquareVisibility::new_partially_visible(LocalSquareHalfPlane::down(0.0)),
        );

        // top-down portal is at (2+5, 1+3) = (7,4)
        let old_fov_root_pose: SquareWithOrthogonalDir = (2, 1, STEP_UP).into();

        let new_fov_root_pose: SquareWithOrthogonalDir = (3, 0, STEP_RIGHT).into();

        let old_rasterized_fov =
            RasterizedFieldOfView::new_with_one_top_down_portal(old_fov_root_pose, top_down_portal);

        let forward_tf_through_portal_new_to_old = RigidTransform::from_start_and_end_poses(
            old_fov_root_pose.stepped_n(2),
            old_fov_root_pose.stepped().turned_right(),
        );

        let new_rasterized_fov = old_rasterized_fov.as_seen_through_portal_from_other_view_root(
            new_fov_root_pose,
            forward_tf_through_portal_new_to_old,
        );

        assert_eq!(new_rasterized_fov.top_down_portals().len(), 1);

        let deeper_top_down_portal = new_rasterized_fov.top_down_portals()[0];
        assert_eq!(deeper_top_down_portal.relative_position, (-8, -3).into());
    }

    fn get_generic_partial_square_top_down_portal() -> TopDownPortal {
        TopDownPortal::new(
            (5, 5),
            TopDownPortalTarget::new((20, 10), 3, 0),
            SquareVisibility::new_top_half_visible(),
        )
    }

    fn two_top_down_portals_with_overlapping_entrances_but_not_exits() -> [TopDownPortal; 2] {
        let mut p = get_generic_partial_square_top_down_portal();
        let p1 = p.clone();
        p.target.portal_rotation_to_target += 1.into();
        p.target.absolute_square += STEP_DOWN;
        [p1, p]
    }
    // Note that the exits do overlap
    fn two_top_down_portals_on_one_square_with_overlapping_exits_but_not_entrances(
    ) -> [TopDownPortal; 2] {
        let mut p = get_generic_partial_square_top_down_portal();
        let p1 = p.clone();
        p.target.portal_rotation_to_target += 2.into();
        [p1, p]
    }

    #[test]
    fn test_confirm_top_down_portal_overlap_test_data() {
        let overlapping = two_top_down_portals_with_overlapping_entrances_but_not_exits();
        let non_overlapping =
            two_top_down_portals_on_one_square_with_overlapping_exits_but_not_entrances();

        // TODO: test for exit overlaps (when it become relevant, I suppose)
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
        RasterizedFieldOfView::from_top_down_portals(ORIGIN_POSE(), portals);
    }
    #[test]
    fn test_local_positions_are_relative_to_view_root_orientation() {
        let rfov = RasterizedFieldOfView::from_view_root_and_one_direct_local_connection(
            (3, 4, STEP_RIGHT),
            (2, 3),
        );

        assert_eq!(
            rfov.relative_square_to_absolute_square((2, 3)),
            (6, 2).into()
        );

        assert_eq!(
            rfov.top_down_portals().first().unwrap().target_square(),
            (6, 2).into()
        );
    }
    #[test]
    fn test_seen_from_other_local_view_root__origin() {
        let rfov = RasterizedFieldOfView::from_view_root_and_one_direct_local_connection(
            (3, 4, STEP_RIGHT),
            (2, 3),
        );

        assert_eq!(
            rfov.as_seen_from_oriented_origin()
                .top_down_portals()
                .first()
                .unwrap()
                .relative_position(),
            (6, 2).into()
        );
    }
    #[test]
    fn test_seen_from_other_local_view_root__non_origin() {
        let rfov = RasterizedFieldOfView::from_view_root_and_one_direct_local_connection(
            (3, 4, STEP_RIGHT),
            (2, 3),
        );
        let new_rfov = rfov.as_seen_from_other_local_view_root((5, 5, STEP_LEFT));
        assert_eq!(
            new_rfov
                .top_down_portals()
                .first()
                .unwrap()
                .relative_position(),
            (-3, -1).into()
        );
        assert_eq!(
            new_rfov
                .as_seen_from_oriented_origin()
                .top_down_portals()
                .first()
                .unwrap()
                .relative_position(),
            (6, 2).into()
        );
    }
    #[test]
    fn test_visible_absolute_squares_only_counts_target_side_of_portal__actual_portal() {
        /// Because the local side might be hidden behind a portal
        let test_portal = get_generic_partial_square_top_down_portal();
        let rfov =
            RasterizedFieldOfView::new_with_one_top_down_portal((3, 4, STEP_RIGHT), test_portal);

        let visible_absolute_squares = rfov.visible_absolute_squares();
        let correct_local_square =
            rfov.relative_square_to_absolute_square(test_portal.relative_position);
        assert_ne!(correct_local_square, test_portal.target_square()); // test data validation
        assert_eq!(visible_absolute_squares.len(), 1);
        assert_false!(visible_absolute_squares.contains(&correct_local_square));
        assert!(visible_absolute_squares.contains(&test_portal.target_square()));
    }
    #[test]
    fn test_visible_absolute_squares_only_counts_target_side_of_portal__direct_local_connection() {
        /// Because the local side might be hidden behind a portal
        let rfov = RasterizedFieldOfView::new_with_one_fully_visible_local_square(
            (3, 4, STEP_RIGHT),
            (5, 5),
        );
        let test_portal = rfov.top_down_portals().iter().next().unwrap().clone();

        let visible_absolute_squares = rfov.visible_absolute_squares();
        let correct_local_square =
            rfov.relative_square_to_absolute_square(test_portal.relative_position);
        assert_eq!(correct_local_square, test_portal.target_square()); // test data validation
        assert_eq!(visible_absolute_squares.len(), 1);
        assert!(visible_absolute_squares.contains(&test_portal.target_square()));
    }
    #[test]
    fn test_visible_absolute_squares_only_counts_target_side_of_portal__local_connection_and_portal_sharing_square(
    ) {
        /// Because the local side might be hidden behind a portal
        let test_portal = get_generic_partial_square_top_down_portal();
        let mut rfov =
            RasterizedFieldOfView::new_with_one_top_down_portal((3, 4, STEP_RIGHT), test_portal);

        // TODO: make this a function in RasterizedFieldOfView
        let direct_local_connection = TopDownPortal::new(
            test_portal.relative_position,
            TopDownPortalTarget::new_local(
                rfov.relative_square_to_absolute_square(test_portal.relative_position),
            ),
            test_portal.shape_in_entrance_frame().complement().unwrap(),
        );
        rfov.add_top_down_portal(direct_local_connection);

        let visible_absolute_squares = rfov.visible_absolute_squares();
        let correct_local_square =
            rfov.relative_square_to_absolute_square(test_portal.relative_position);
        assert_ne!(correct_local_square, test_portal.target_square()); // test data validation
        assert_eq!(visible_absolute_squares.len(), 2);
        assert!(visible_absolute_squares.contains(&correct_local_square));
        assert!(visible_absolute_squares.contains(&test_portal.target_square()));
    }
}
