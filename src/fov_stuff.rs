pub mod angle_based_visible_segment;
pub mod fence;
pub mod rasterized_field_of_view;
pub mod square_visibility;

use std::backtrace::Backtrace;
use std::collections::{HashMap, HashSet};
use std::f32::consts::PI;
use std::fmt::{Debug, Formatter};

use crate::fov_stuff::angle_based_visible_segment::AngleBasedVisibleSegment;
use crate::fov_stuff::rasterized_field_of_view::RasterizedFieldOfView;
use crate::fov_stuff::square_visibility::{
    LocalSquareVisibilityMap, RelativeSquareVisibilityFunctions, SquareVisibility,
    SquareVisibilityFromOneLargeShadow, SquareVisibilityFunctions, SquareVisibilityMapFunctions,
};
use crate::utility::coordinates::{
    king_step_distance, unit_vector_from_angle, OrthogonalWorldStep,
};
use crate::utility::octant::Octant;
use crate::utility::partial_angle_interval::PartialAngleInterval;
use crate::utility::poses::{RelativeSquareWithOrthogonalDir, SquareWithOrthogonalDir};
use derive_more::Constructor;
use euclid::{point2, vec2, Angle};
use getset::CopyGetters;
use itertools::*;
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
use crate::graphics::Graphics;
use crate::piece::MAX_PIECE_RANGE;
use crate::portal_geometry::{Portal, PortalGeometry};
use crate::utility::angle_interval::AngleInterval;
use crate::utility::coordinate_frame_conversions::*;
use crate::utility::*;

use self::rasterized_field_of_view::RasterizedFieldOfViewFunctions;

type Pose = SquareWithOrthogonalDir;

const NARROWEST_VIEW_CONE_ALLOWED_IN_DEGREES: f32 = 0.001;

const DEFAULT_FOV_ROOT_DIRECTION: WorldStep = STEP_UP;

// #[portrait::derive(QuarterTurnRotatable with portrait::derive_delegate)]
#[derive(PartialEq, Debug, Clone, Constructor)]
pub struct FieldOfView {
    root_square_with_direction: SquareWithOrthogonalDir,
    visible_segments_in_main_view_only: Vec<AngleBasedVisibleSegment>,
    transformed_sub_fovs: Vec<FieldOfView>,
}

// TODO: derive_delegate with portrait
impl QuarterTurnRotatable for FieldOfView {
    fn quarter_rotated_ccw(&self, quarter_turns_ccw: impl Into<QuarterTurnsCcw> + Copy) -> Self {
        FieldOfView::new(
            self.root_square_with_direction
                .quarter_revolved_ccw_around_origin(quarter_turns_ccw),
            self.visible_segments_in_main_view_only
                .quarter_rotated_ccw(quarter_turns_ccw),
            self.transformed_sub_fovs
                .quarter_rotated_ccw(quarter_turns_ccw),
        )
    }
}

impl FieldOfView {
    pub fn new_empty_fov_with_root(root: impl Into<SquareWithOrthogonalDir>) -> Self {
        FieldOfView {
            root_square_with_direction: root.into(),
            visible_segments_in_main_view_only: Vec::new(),
            transformed_sub_fovs: Vec::new(),
        }
    }
    pub fn new_empty_fov_at(new_center: impl Into<WorldSquare>) -> Self {
        Self::new_empty_fov_with_root(SquareWithOrthogonalDir::from_square_and_step(
            new_center.into(),
            STEP_UP,
        ))
    }
    pub fn new_with_visible_face(
        center: impl Into<WorldSquare>,
        face: impl Into<RelativeFace>,
    ) -> Self {
        Self::new_empty_fov_at(center.into()).with_fully_visible_relative_face(face)
    }
    pub fn new_with_fully_visible_relative_square(
        center: impl Into<WorldSquare>,
        square: impl Into<WorldStep>,
    ) -> Self {
        Self::new_empty_fov_at(center.into()).with_fully_visible_relative_square(square)
    }
    pub fn new_with_arc_and_radius(
        center: impl Into<WorldSquare>,
        arc: AngleInterval,
        square_radius: u32,
    ) -> Self {
        Self::new_empty_fov_at(center).with_local_visible_segment(
            AngleBasedVisibleSegment::from_arc_and_fence_radius(arc, square_radius),
        )
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
    pub fn add_view_segment_for_fully_visible_relative_square(
        &mut self,
        step: impl Into<WorldStep>,
    ) {
        self.visible_segments_in_main_view_only
            .push(AngleBasedVisibleSegment::from_relative_square(step.into()))
    }
    // passthrough version
    pub fn with_fully_visible_relative_square(mut self, step: impl Into<WorldStep>) -> Self {
        self.add_view_segment_for_fully_visible_relative_square(step);
        self
    }
    pub fn add_fully_visible_relative_face(
        &mut self,
        face: impl Into<RelativeSquareWithOrthogonalDir>,
    ) {
        self.visible_segments_in_main_view_only
            .push(AngleBasedVisibleSegment::from_relative_face(face))
    }
    // passthrough version
    pub fn with_fully_visible_relative_face(mut self, face: impl Into<RelativeFace>) -> Self {
        self.add_fully_visible_relative_face(face);
        self
    }
    pub fn with_local_visible_segment(
        mut self,
        segment: impl Into<AngleBasedVisibleSegment>,
    ) -> Self {
        self.visible_segments_in_main_view_only.push(segment.into());
        self
    }
    pub fn with_smoothly_replaced_view_root(
        mut self,
        view_root: impl Into<SquareWithOrthogonalDir>,
    ) -> Self {
        let view_root = view_root.into();
        let tf_to_new_view_root =
            RigidTransform::from_start_and_end_poses(self.root_square_with_direction, view_root);

        self.root_square_with_direction = view_root;
        self.visible_segments_in_main_view_only = self
            .visible_segments_in_main_view_only
            .iter()
            .map(|x| x.quarter_rotated_ccw(tf_to_new_view_root.inverse().rotation()))
            .collect();
        self.transformed_sub_fovs
            .iter()
            .map(|x| x.apply_rigid_transform(tf_to_new_view_root.inverse()));
        self
    }

    pub fn visible_segments_in_main_view_only(&self) -> &Vec<AngleBasedVisibleSegment> {
        &self.visible_segments_in_main_view_only
    }

    fn combined_main_view_only(&self, other: &Self) -> Self {
        assert_eq!(
            self.root_square_with_direction,
            other.root_square_with_direction
        );

        // TODO: combine visible segments where possible
        // TODO: delete this commented code

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

        let segments = self
            .visible_segments_in_main_view_only
            .iter()
            .chain(other.visible_segments_in_main_view_only.iter())
            .cloned();
        let combined_view_segments = AngleBasedVisibleSegment::combine_multiple(segments);

        FieldOfView {
            root_square_with_direction: self.root_square_with_direction,
            visible_segments_in_main_view_only: combined_view_segments,
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

    /// Two sub-FieldOfViews can be combined if they have the same root position and rotation.
    /// I think the original intent behind this was when two different fovs from adjacent octants travel through a portal, and each of those adjacent top-level local FOVs has a sub-FOV on the other side of the portal, those sub-FOVs can still be combined for correct drawing.
    ///                                      
    ///  Two sight lines through one portal  
    ///                                      
    ///          ○ ══╡ ╞════                 
    ///              a a                     
    ///                                      
    ///                                      

    /// I suspect this may also catch the case of two different FOVs going through two different, but coherent portals (ie the two portals have the same rigid transform).  I don't know if that would be bad or not.
    ///                                      
    ///                                      
    ///    Coherent, but disconnected portals
    ///                                      
    ///                                      
    ///          ┊             │             
    ///          ┊             │             
    ///          ┯a            ┷a            
    ///          │             ┊             
    ///          ○ ──┨┄┄┄┄┄    ◌ ┄┄┠─────    
    ///              b             b         
    ///                                      
    /// One case that needs (?) to be accounted for is similar to the previous one, but one of the lines to the destination frame goes through a second intermediate portal to get there, so the end sub-FOVs have the same root, and the main FOVs have the same root, but the end sub-FOVs have different portal-depth.
    ///                                            
    ///                                            
    ///                                            
    ///         ┊             ┊      │             
    ///         ┊             ┊      │             
    ///         ┊             ┯b     ┷b            
    ///         ┯a            ┷a     ┊             
    ///         │             ┊      ┊             
    ///         ○ ──┨┄┄┄┄┄    ◌      ◌ ┄┄┠─────    
    ///             c                    c         
    ///                                            
    ///                                            
    ///                                            
    /// Another case is where two views have the same root, go through two portals to different places, and go through another two different portals that make them have the same root again.  This time they have the same portal depth, and I'm not exactly sure how to handle it.
    ///                                            
    ///                                            
    ///       With connected portals               
    ///                                            
    ///             a  b                           
    ///         ◌ ┄┄┠──┨┄┄┄┄┄┄                     
    ///                                            
    ///             a  b                           
    ///         ○ ──┨┄┄┠───                        
    ///          🮡──┨┄┄┠───                        
    ///             c  d                           
    ///         ◌                                  
    ///          🮡┄┄┠──┨┄┄┄┄┄┄                     
    ///             c  d                           
    ///                                            
    ///                                            
    ///        With disconnected portals           
    ///                                            
    ///         ┊             ┊      │             
    ///         ┊             ┊      │             
    ///         ┊             ┯b     ┷b            
    ///         ┯a            ┷a     ┊             
    ///         │             ┊      ┊             
    ///         ○ ──┨┄┄┄┄┄    ◌      ◌ ┄┄┄┄┠─────  
    ///             c                      d       
    ///                                            
    ///         ◌ ┄┄┠──┨┄┄┄┄┄┄                     
    ///             c  d                           
    ///                                            
    ///                                            
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

    pub fn combine_multiple(others: impl IntoIterator<Item = Self>) -> Self {
        others
            .into_iter()
            .reduce(|a, b| a.combined_with(&b))
            .unwrap()
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

    pub fn rasterized(&self) -> RasterizedFieldOfView {
        self.rasterized_at_depth(0, 0)
    }

    fn rasterized_at_depth(
        &self,
        starting_portal_depth: u32,
        forward_portal_rotation_to_this_depth: impl Into<QuarterTurnsCcw>,
    ) -> RasterizedFieldOfView {
        let forward_portal_rotation_to_this_depth = forward_portal_rotation_to_this_depth.into();
        // rasterize top level
        let mut rfov_for_given_depth_and_below: RasterizedFieldOfView =
            self.rasterized_main_view(starting_portal_depth, forward_portal_rotation_to_this_depth);
        // rasterize each sub-level
        self.transformed_sub_fovs.iter().for_each(|sub_fov| {
            let next_step_of_portal_rotation = self.view_transform_to(sub_fov).rotation();

            let rasterized_sub_fov = sub_fov.rasterized_at_depth(
                starting_portal_depth + 1,
                forward_portal_rotation_to_this_depth + next_step_of_portal_rotation,
            );

            // let rasterized_and_relocalized_sub_fov = rasterized_sub_fov
            //     .as_seen_through_portal_from_other_view_root(combined.view_root, portal_transform);
            //     // rasterized_and_relocalized_sub_fov.visible_relative_squares()
            // );
            // combined = combined.combined_with(&rasterized_and_relocalized_sub_fov);
            rfov_for_given_depth_and_below =
                rfov_for_given_depth_and_below.absorb(&rasterized_sub_fov);
        });
        rfov_for_given_depth_and_below
    }

    fn rasterized_main_view(
        &self,
        top_level_portal_depth: u32,
        forward_portal_rotation_to_this_depth: impl Into<QuarterTurnsCcw> + Copy,
    ) -> RasterizedFieldOfView {
        let top_down_portals = RasterizedFieldOfView::visibility_map_to_top_down_portals(
            self.root_square_with_direction,
            &self
                .visible_segments_in_main_view_only
                .iter()
                .map(AngleBasedVisibleSegment::to_local_square_visibility_map)
                .fold(LocalSquareVisibilityMap::new_empty(), |a, b| {
                    a.combined_while_increasing_visibility(&b)
                }),
        )
        .into_iter()
        .map(|td_portal| {
            td_portal
                .with_portal_depth(top_level_portal_depth)
                .with_forward_portal_rotation(forward_portal_rotation_to_this_depth)
        })
        .collect_vec();
        RasterizedFieldOfView::from_top_down_portals(
            self.root_square_with_direction,
            top_down_portals,
        )
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
                .map(|seg| seg.quarter_rotated_ccw(tf.rotation()))
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
    pub fn rotated(&self, quarter_turns: QuarterTurnsCcw) -> Self {
        Self {
            outward_dir: self.outward_dir.quarter_rotated_ccw(quarter_turns),
            across_dir: self.across_dir.quarter_rotated_ccw(quarter_turns),
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

// TODO: Break this function into more pieces
pub fn sub_octant_field_of_view(
    sight_blockers: &SquareSet,
    portal_geometry: &PortalGeometry,
    view_root_pose: SquareWithOrthogonalDir,
    radius: u32,
    view_arc: AngleInterval,
    mut steps_in_octant_iter: OctantFOVSquareSequenceIter,
) -> FieldOfView {
    let tolerance = FAngle::degrees(0.001); // TODO: standardize
    assert!(view_arc.is_in_one_octant());
    let mut fov_result = FieldOfView::new_empty_fov_with_root(view_root_pose);
    assert_eq!(steps_in_octant_iter.octant(), view_arc.octant().unwrap());

    loop {
        let relative_square = steps_in_octant_iter.next().unwrap();
        let square_is_out_of_range = king_step_distance(relative_square) > radius;
        if square_is_out_of_range {
            break;
        }

        // in an octant, every square has two possible view-blocking sides (on the far side of the square)
        let face_directions = [
            steps_in_octant_iter.outward_dir,
            steps_in_octant_iter.across_dir,
        ];
        let absolute_square = view_root_pose.square() + relative_square;

        let mut view_blocking_arc_for_this_square: AngleInterval = AngleInterval::Empty;
        for face_direction in face_directions {
            let relative_face: RelativeFace = (relative_square, face_direction).into();
            let absolute_face: Face = (absolute_square, face_direction).into();

            let face_has_portal = portal_geometry
                .get_portal_by_entrance(absolute_face)
                .is_some();

            let square_blocks_sight = sight_blockers.contains(&absolute_square);

            let face_is_on_edge_of_sight_radius =
                king_step_distance(relative_face.stepped().square()) > radius;

            let face_blocks_sight =
                face_has_portal || square_blocks_sight || face_is_on_edge_of_sight_radius;

            let visible_arc_of_face = AngleInterval::from_relative_square_face(relative_face);

            let visible_arc_of_face = view_arc.intersection(
                visible_arc_of_face,
                Angle::degrees(NARROWEST_VIEW_CONE_ALLOWED_IN_DEGREES),
            );

            if visible_arc_of_face.is_empty() {
                continue;
            }

            if !face_blocks_sight {
                continue;
            }

            view_blocking_arc_for_this_square = view_blocking_arc_for_this_square
                .try_combine(visible_arc_of_face, tolerance)
                .unwrap();

            // create a segment ending at this face
            let visible_segment_up_to_relative_face =
                AngleBasedVisibleSegment::from_relative_face(relative_face)
                    .with_arc(visible_arc_of_face);

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

            let forward_portal_transform_to_sub_fov = portal.get_transform();
            let sub_fov_view_root =
                view_root_pose.apply_rigid_transform(forward_portal_transform_to_sub_fov);
            // need to rotate back to standard for compatibility with the sight blockers and portal geometry.
            let sub_fov_view_root_with_standardized_orientation = sub_fov_view_root
                .quarter_rotated_ccw_in_place(-forward_portal_transform_to_sub_fov.rotation());
            // in a relative view, the portal exit is the same line as the portal entrance
            // let relative_portal_exit_in_local_frame = forward_portal_transform_to_sub_fov
            //     .transform_relative_pose(back_of_portal_entrance_in_local_frame);
            let relative_portal_line = relative_face; // in relative frame, entrance and exit are the same square face border
                                                      //todo!("sort out sub_fov rotations");
                                                      // TODO: simplify the rotation nastiness
            let sub_arc_fov_with_standardized_orientation = sub_octant_field_of_view(
                sight_blockers,
                portal_geometry,
                sub_fov_view_root_with_standardized_orientation,
                radius,
                visible_arc_of_face
                    .quarter_rotated_ccw(forward_portal_transform_to_sub_fov.rotation()),
                steps_in_octant_iter.rotated(forward_portal_transform_to_sub_fov.rotation()),
            )
            .with_weakly_applied_start_line(
                relative_portal_line.quarter_revolved_ccw_around_origin(
                    forward_portal_transform_to_sub_fov.rotation(),
                ),
            );
            let corrected_view_root = sub_arc_fov_with_standardized_orientation
                .root_square_with_direction()
                .quarter_rotated_ccw_in_place(forward_portal_transform_to_sub_fov.rotation());
            let sub_arc_fov = sub_arc_fov_with_standardized_orientation
                .with_smoothly_replaced_view_root(corrected_view_root);
            fov_result.transformed_sub_fovs.push(sub_arc_fov);
        }
        if view_blocking_arc_for_this_square.is_empty() {
            continue;
        }
        // split arc around the blocked faces
        let view_arcs_on_either_side_of_blocked_faces =
            view_arc.subtract(view_blocking_arc_for_this_square);
        view_arcs_on_either_side_of_blocked_faces
            .into_iter()
            .filter(|new_sub_arc| {
                new_sub_arc.width().to_degrees() > NARROWEST_VIEW_CONE_ALLOWED_IN_DEGREES
            })
            .for_each(|new_sub_arc| {
                let sub_arc_fov = sub_octant_field_of_view(
                    sight_blockers,
                    portal_geometry,
                    view_root_pose,
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
    let fov_result = sub_octant_field_of_view(
        sight_blockers,
        portal_geometry,
        SquareWithOrthogonalDir::from_square_and_step(center_square, DEFAULT_FOV_ROOT_DIRECTION),
        radius,
        AngleInterval::from_octant(octant),
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
            fov_result_accumulator.combined_with(&new_fov_result)
        },
    )
}

fn point_in_view_arc(view_arc: PartialAngleInterval) -> WorldMove {
    unit_vector_from_angle(view_arc.center_angle()).cast_unit()
}

fn print_fov(fov: &FieldOfView, radius: u32, render_portals_with_line_of_sight: bool) {
    let center_drawable = TextDrawable::new("@@", WHITE, GREY, true);
    let rfov = fov.rasterized();
    let r = radius as i32;
    (-r..=r).for_each(|neg_y| {
        let y = -neg_y;
        (-r..=r).for_each(|x| {
            let rel_square: WorldStep = vec2(x, y);
            let maybe_drawable = Graphics::drawable_at_relative_square(
                &rfov,
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

pub fn debug_print_fov_as_relative(fov: &FieldOfView, radius: u32) {
    print_fov(fov, radius, true)
}

pub fn debug_print_fov_as_absolute(fov: &FieldOfView, radius: u32) {
    print_fov(fov, radius, false)
}

pub fn debug_print_square_set<T: GridCoordinate>(squares: &HashSet<T>) {
    if T::IS_RELATIVE {
        println!("Relative Squares:");
    } else {
        println!("Absolute Squares:");
    }
    let xmax = squares.iter().map(|c| c.x()).max().unwrap() + 1;
    let xmin = squares.iter().map(|c| c.x()).min().unwrap() - 1;
    let ymax = squares.iter().map(|c| c.y()).max().unwrap() + 1;
    let ymin = squares.iter().map(|c| c.y()).min().unwrap() - 1;
    assert!(xmax - xmin < 500);
    assert!(ymax - ymin < 500);

    let y_prefix_width = ymax.to_string().len().max(ymin.to_string().len());
    let x_prefix_width = xmax.to_string().len().max(xmin.to_string().len());
    let top_bottom_x_marker_columns = [true, false].map(|is_top| {
        intersperse(
            (xmin..=xmax).into_iter().map(|x| {
                if is_top {
                    format!("{x:>x_prefix_width$}")
                } else {
                    format!("{x:<x_prefix_width$}")
                }
                .replace("-", "╷")
                .chars()
                .collect_vec()
            }),
            vec![' '; x_prefix_width],
        )
        .collect_vec()
    });

    let top_bottom_x_lines: [Vec<Vec<char>>; 2] =
        top_bottom_x_marker_columns
            .map(transpose)
            .map(|label_group| {
                label_group
                    .into_iter()
                    .map(|row| {
                        let mut v = vec![' '; y_prefix_width];
                        v.extend(row);
                        v
                    })
                    .collect_vec()
            });

    let [x_prefix_lines, x_postfix_lines] = top_bottom_x_lines;
    x_prefix_lines
        .into_iter()
        .for_each(|x_line| println!("{:>y_prefix_width$}", String::from_iter(x_line)));

    (ymin..=ymax).map(|y| (y - ymin) * -1 + ymax).for_each(|y| {
        let line_string: String = (xmin..=xmax)
            .map(|x| {
                if squares.contains(&T::new(x, y)) {
                    String::from_iter([FULL_BLOCK].repeat(2))
                } else {
                    match (x == 0, y == 0) {
                        // (true, true) => "00",
                        // (true, false) => "╺╸",
                        // (false, true) => "╏ ",
                        (true, true) => "╬═",
                        (true, false) => "╫╴",
                        (false, true) => "╪═",
                        (false, false) => {
                            match (x.rem_euclid(5), y.rem_euclid(5)) {
                                (0, 0) => "┼╴",
                                // (2, 2) => ". ",
                                (0, _) => "| ",
                                (_, 0) => "╶╴", //─
                                // (_, _) => "  ",
                                (_, _) => "· ",
                            }
                        }
                    }
                    .to_string()
                }
            })
            .collect();
        let prefix_string: String = format!("{y:>y_prefix_width$}");
        println!("{}{}{}", prefix_string, line_string, y);
    });
    x_postfix_lines
        .into_iter()
        .for_each(|x_line| println!("{:>y_prefix_width$}", String::from_iter(x_line)));
}

#[cfg(test)]
mod tests {
    use crate::utility::poses::faces_away_from_center_at_rel_square;
    use euclid::point2;
    use itertools::Itertools;
    use ntest::{assert_about_eq, assert_false, assert_true, timeout};
    use pretty_assertions::{assert_eq, assert_ne};
    use rgb::RGB8;

    use crate::glyph::angled_blocks::{
        angle_block_char_complement, angle_block_chars_are_horizontally_continuous,
        angled_block_char_to_snap_points_map, angled_block_flip_y, SnapGridPoint,
    };
    use crate::glyph::glyph_constants::{FULL_BLOCK, GREEN};
    use crate::glyph::DoubleGlyphFunctions;
    use crate::utility::{
        better_angle_from_x_axis, QuarterTurnsCcw, SquareWithKingDir, SquareWithOrthogonalDir,
        STEP_DOWN, STEP_LEFT, STEP_UP,
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
        )
        .rasterized();
        assert!(fov_result
            .only_partially_visible_local_relative_squares()
            .is_empty());
        assert_false!(fov_result.relative_square_is_fully_visible(STEP_ZERO));
        assert_false!(fov_result.relative_square_is_visible(STEP_ZERO));

        // Does not include center
        let square_area = (fov_radius * 2 + 1).pow(2) - 1;
        assert_eq!(
            fov_result.fully_visible_local_relative_squares().len(),
            square_area as usize
        );
    }

    #[test]
    fn test_small_field_of_view_with_no_obstacles() {
        let start_square = point2(5, 5);
        let radius = 2;
        let rasterized_fov_result = portal_aware_field_of_view_from_square(
            start_square,
            radius,
            &SquareSet::default(),
            &PortalGeometry::default(),
        )
        .rasterized();

        //print_fov_as_relative(&fov_result, 5);
        assert!(rasterized_fov_result
            .only_partially_visible_local_relative_squares()
            .is_empty());
        assert_false!(rasterized_fov_result.relative_square_is_visible(STEP_ZERO));
        let square_area = (radius * 2 + 1).pow(2) - 1;
        assert_eq!(
            rasterized_fov_result
                .fully_visible_local_relative_squares()
                .len(),
            square_area as usize
        );
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
        )
        .rasterized();
        assert!(fov_result.relative_square_is_fully_visible(block_step));
        assert!(fov_result.relative_square_is_fully_visible(block_step + STEP_DOWN));
        assert_false!(fov_result.relative_square_is_fully_visible(block_step + STEP_UP));
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
        )
        .rasterized();
        assert!(!fov_result
            .only_partially_visible_local_relative_squares()
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
        )
        .rasterized();
        for i in 1..=5 {
            let step = STEP_UP_RIGHT * i;
            let square_visibility = fov_result
                .visibility_map_of_local_relative_squares()
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
    fn test_observed_bright_spot_in_shadow() {
        let player_square = point2(3, 3);
        let block_square = player_square + STEP_UP_RIGHT * 2;
        let test_rel_square = (block_square + STEP_UP) - player_square;

        let fov_result = portal_aware_field_of_view_from_square(
            player_square,
            SIGHT_RADIUS,
            &SquareSet::from([block_square]),
            &PortalGeometry::default(),
        )
        .rasterized();
        let visibilities_of_test_square =
            fov_result.top_down_portal_entrance_shapes_at_relative_square(test_rel_square);
        assert_eq!(visibilities_of_test_square.len(), 1);
        assert_eq!(
            PartialVisibilityDrawable::from_square_visibility(visibilities_of_test_square[0])
                .to_glyphs()
                .to_clean_string()
                .chars()
                .nth(1)
                .unwrap(),
            SPACE
        );
    }

    #[test]
    fn test_fov_square_sequence__detailed() {
        let mut sequence = OctantFOVSquareSequenceIter::new_from_center(Octant::new(1));
        let correct_sequence = vec![
            vec2(0, 0),
            vec2(0, 1),
            vec2(1, 1),
            vec2(0, 2),
            vec2(1, 2),
            vec2(2, 2),
            vec2(0, 3),
        ];
        assert_eq!(
            sequence.take(correct_sequence.len()).collect_vec(),
            correct_sequence
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
        )
        .rasterized();
        let visible_rel_square = STEP_RIGHT * 5 + STEP_UP * 2;
        assert!(fov_result.relative_square_is_fully_visible((visible_rel_square + STEP_LEFT)));
        assert!(fov_result.relative_square_is_fully_visible(visible_rel_square));
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
        )
        .rasterized();

        fov_result
            .visibility_map_of_local_relative_squares()
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
        SquareVisibility::from_relative_square_and_view_arc(
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
        let mut fov = FieldOfView::new_empty_fov_at(center);
        let relative_square = vec2(2, 2);
        fov.add_view_segment_for_fully_visible_relative_square(relative_square);

        assert!(fov
            .rasterized()
            .relative_square_is_fully_visible(relative_square));
    }

    #[test]
    fn test_narrow_fov_through_portal__no_turn() {
        let mut portal_geometry = PortalGeometry::default();
        let center = point2(10, 20);
        let dx_to_portal_square = 3;
        let entrance_pose: Pose = (center + STEP_RIGHT * dx_to_portal_square, STEP_RIGHT).into();
        let exit_pose: Pose = (center + STEP_UP * 5, STEP_RIGHT).into();
        assert_eq!(exit_pose.square(), (10, 25).into());

        portal_geometry.create_portal(entrance_pose, exit_pose);

        let radius = 10;
        let fov = sub_octant_field_of_view(
            &Default::default(),
            &portal_geometry,
            SquareWithOrthogonalDir::from_square_and_step(center, STEP_UP),
            radius,
            narrow_arc_to_right_in_first_octant(),
            OctantFOVSquareSequenceIter::new_from_center(Octant::new(0)),
        );
        let rfov = fov.rasterized();

        let should_be_visible_relative_squares: StepSet =
            (1..=10).into_iter().map(|dx| STEP_RIGHT * dx).collect();
        let should_be_visible_before_portal: SquareSet = (1..=dx_to_portal_square)
            .into_iter()
            .map(|dx| center + STEP_RIGHT * dx)
            .collect();
        let squares_after_portal = radius - (dx_to_portal_square.abs() as u32 + 1);
        let should_be_visible_after_portal: SquareSet = (0..squares_after_portal as i32)
            .into_iter()
            .map(|dx| exit_pose.square() + STEP_RIGHT * dx)
            .collect();

        assert_eq!(rfov.fully_visible_local_relative_squares().len(), 0);
        assert_eq!(rfov.fully_visible_relative_squares().len(), 0);
        assert_eq!(rfov.visible_relative_squares().len(), radius as usize);
        should_be_visible_relative_squares.iter().for_each(|step| {
            assert!(rfov.relative_square_is_visible(*step));
        });
        should_be_visible_before_portal.iter().for_each(|square| {
            assert!(
                rfov.absolute_square_is_visible(*square),
                "square: {}",
                square.to_string()
            );
        });
        should_be_visible_after_portal.iter().for_each(|square| {
            assert!(
                rfov.absolute_square_is_visible(*square),
                "square: {}",
                square.to_string()
            );
        });
    }
    fn narrow_arc_to_right() -> AngleInterval {
        AngleInterval::from_degrees(
            -NARROWEST_VIEW_CONE_ALLOWED_IN_DEGREES,
            NARROWEST_VIEW_CONE_ALLOWED_IN_DEGREES,
        )
    }
    fn narrow_arc_to_right_in_first_octant() -> AngleInterval {
        let clockwise_end_in_degrees = 0.1;
        AngleInterval::from_degrees(
            clockwise_end_in_degrees,
            clockwise_end_in_degrees + NARROWEST_VIEW_CONE_ALLOWED_IN_DEGREES * 2.0,
        )
    }

    fn narrow_fov_right_with_portal_turn_at_distance(
        start: impl Into<WorldSquare>,
        quarter_turns_left: i32,
        turn_at_dist: u32,
        fov_range: u32,
    ) -> FieldOfView {
        let start = start.into();
        let turn_square = start + STEP_RIGHT * turn_at_dist as i32;
        let exit_dir = STEP_RIGHT.quarter_rotated_ccw(quarter_turns_left);
        let portals = PortalGeometry::new().with_portal(
            (turn_square + STEP_LEFT, STEP_RIGHT),
            (turn_square + exit_dir, exit_dir),
        );
        sub_octant_field_of_view(
            &Default::default(),
            &portals,
            SquareWithOrthogonalDir::from_square_and_step(start, DEFAULT_FOV_ROOT_DIRECTION),
            fov_range,
            narrow_arc_to_right_in_first_octant(),
            OctantFOVSquareSequenceIter::new_from_center(Octant::new(0)),
        )
    }

    #[test]
    fn test_narrow_fov_through_portal__with_turn__correct_squares_are_visible() {
        let fov = narrow_fov_right_with_portal_turn_at_distance((0, 2), -1, 4, 5);
        // debug_print_fov_as_relative(&fov, 10);
        // debug_print_fov_as_absolute(&fov, 10);
        let rfov = fov.rasterized();
        // debug_print_square_set(&rfov.visible_relative_squares());
        // debug_print_square_set(&rfov.visible_absolute_squares());
        assert_eq!(
            rfov.visible_relative_squares(),
            as_set([(1, 0), (2, 0), (3, 0), (4, 0), (5, 0)])
        );
        assert_eq!(
            rfov.visible_absolute_squares(),
            as_set([(1, 2), (2, 2), (3, 2), (4, 1), (4, 0)])
        );
    }
    #[test]
    fn test_narrow_fov_through_portal__with_turn__correct_rotation_from_portal() {
        let fov = narrow_fov_right_with_portal_turn_at_distance((0, 2), -1, 4, 5);
        // debug_print_fov_as_relative(&fov, 10);
        // debug_print_fov_as_absolute(&fov, 10);
        let rfov = fov.rasterized();
        // debug_print_square_set(&rfov.visible_relative_squares());
        // debug_print_square_set(&rfov.visible_absolute_squares());
        let range_and_turns = vec![((1..=3), 0), ((4..=5), -1)];
        range_and_turns.into_iter().for_each(|(range, turns)| {
            range.for_each(|dx| {
                assert_eq!(
                    rfov.lone_portal_rotation_for_relative_square_or_panic((dx, 0)),
                    QuarterTurnsCcw::new(turns)
                );
            });
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

        let angle_based_fov_result = single_octant_field_of_view(
            center,
            3,
            Octant::new(0),
            &Default::default(),
            &portal_geometry,
        );
        let fov_result = angle_based_fov_result.rasterized();

        debug_print_fov_as_absolute(&angle_based_fov_result, 5);
        debug_print_fov_as_relative(&angle_based_fov_result, 5);
        debug_print_square_set(&fov_result.visible_relative_squares());
        debug_print_square_set(&fov_result.visible_absolute_squares());

        assert_eq!(angle_based_fov_result.transformed_sub_fovs.len(), 1);
        // Not fully visible because only one octant
        assert!(fov_result.relative_square_is_visible(STEP_RIGHT * 2));
        assert_false!(fov_result.absolute_square_is_visible(portal_entrance.square() + STEP_RIGHT));
        assert!(fov_result.absolute_square_is_visible(portal_exit.square()));

        assert_false!(angle_based_fov_result.transformed_sub_fovs[0]
            .rasterized()
            .relative_square_is_fully_visible(STEP_ZERO));
    }

    #[test]
    fn test_two_portals_to_one_square__short_range_diagonal() {
        let base_square = WorldSquare::new(0, 0);
        let target_rel_square = WorldStep::new(1, 1);

        let x_entrance_pose: SquareWithOrthogonalDir = (1, 0, STEP_UP).into();
        let y_entrance_pose: SquareWithOrthogonalDir = (0, 1, STEP_RIGHT).into();

        (0..4)
            .cartesian_product(0..4)
            .for_each(|(x_exit_quarter_turns, y_exit_quarter_turns)| {
                dbg!(x_exit_quarter_turns, y_exit_quarter_turns);
                let portal_geometry = PortalGeometry::new()
                    .with_portal(
                        x_entrance_pose,
                        x_entrance_pose
                            .stepped()
                            .strafed_right_n(5)
                            .quarter_rotated_ccw_in_place(x_exit_quarter_turns),
                    )
                    .with_portal(
                        y_entrance_pose,
                        y_entrance_pose
                            .stepped()
                            .strafed_left_n(5)
                            .quarter_rotated_ccw_in_place(y_exit_quarter_turns),
                    );
                let fov = portal_aware_field_of_view_from_square(
                    base_square,
                    1,
                    &Default::default(),
                    &portal_geometry,
                );
                let rfov = fov.rasterized();
                debug_print_fov_as_absolute(&fov, 7);
                debug_print_fov_as_relative(&fov, 2);
                // TODO: test for correct number of visible squares, etc?
            })
    }

    #[test]
    fn test_sub_fov_view_transform() {
        let sub_center: SquareWithOrthogonalDir = (3, 1, STEP_RIGHT).into();
        let mut sub_fov = FieldOfView::new_empty_fov_with_root(sub_center);

        let main_center: SquareWithOrthogonalDir = (50, 10, STEP_UP).into();
        let mut main_fov = FieldOfView::new_empty_fov_with_root(main_center);

        let absolute_test_square: WorldSquare = point2(1, 4);
        let test_square_relative_to_sub_fov = sub_fov
            .root_square_with_direction
            .other_square_absolute_to_relative(absolute_test_square);
        assert_eq!(test_square_relative_to_sub_fov, WorldStep::new(-3, -2));

        sub_fov.add_view_segment_for_fully_visible_relative_square(test_square_relative_to_sub_fov);

        let rasterized_sub_fov = sub_fov.rasterized();

        main_fov.transformed_sub_fovs.push(sub_fov);

        let rasterized_fov = main_fov.rasterized();

        assert_eq!(rasterized_sub_fov.view_root, sub_center);

        assert_eq!(
            rasterized_sub_fov.fully_visible_relative_squares(),
            [test_square_relative_to_sub_fov].into()
        );
        assert_eq!(rasterized_sub_fov.fully_visible_absolute_squares().len(), 1);
        assert!(rasterized_sub_fov
            .fully_visible_absolute_squares()
            .contains(&absolute_test_square));

        assert!(rasterized_fov.relative_square_is_visible(test_square_relative_to_sub_fov));
        assert!(rasterized_fov.relative_square_is_fully_visible(test_square_relative_to_sub_fov));
        assert!(rasterized_fov.absolute_square_is_visible(absolute_test_square));
        assert!(rasterized_fov.absolute_square_is_fully_visible(absolute_test_square));

        assert_eq!(rasterized_fov.fully_visible_relative_squares().len(), 1);
    }

    #[test]
    fn test_square_fully_covered_by_face() {
        let view_arc_of_face =
            PartialAngleInterval::from_relative_square_face((STEP_RIGHT, STEP_RIGHT));
        let square = STEP_RIGHT * 2;

        let visibility =
            SquareVisibility::from_relative_square_and_view_arc(view_arc_of_face, square);
        assert!(visibility.unwrap().is_fully_visible());
    }

    #[test]
    fn test_square_fully_not_covered_by_adjacent() {
        let view_arc_of_face =
            PartialAngleInterval::from_relative_square_face((STEP_UP_RIGHT, STEP_RIGHT));
        let square = STEP_RIGHT * 2;

        let visibility =
            SquareVisibility::from_relative_square_and_view_arc(view_arc_of_face, square);
        assert!(visibility.is_none());
    }

    #[test]
    fn test_square_fully_inside_view_arc__near_edge() {
        let square = vec2(1, -2);
        let arc = PartialAngleInterval::from_radians(-PI / 2.0, -PI / 4.0);
        assert!(
            SquareVisibility::from_relative_square_and_view_arc(arc, square)
                .unwrap()
                .is_fully_visible()
        );
    }

    #[test]
    fn test_portal_pose_transform() {
        let entrance = SquareWithOrthogonalDir::from_square_and_step(point2(3, 4), STEP_RIGHT);
        let exit = SquareWithOrthogonalDir::from_square_and_step(point2(50, 70), STEP_DOWN);
        let portal = Portal::new(entrance, exit);

        let transform = portal.get_transform();
        assert_eq!(transform.translation(), vec2(47, 67));
        assert_eq!(transform.rotation(), QuarterTurnsCcw::new(3));

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
        let mut fov_1 =
            FieldOfView::new_with_fully_visible_relative_square(main_center, STEP_RIGHT);
        let mut fov_2 = FieldOfView::new_with_fully_visible_relative_square(main_center, STEP_UP);

        let combined = fov_1.combined_with(&fov_2).rasterized();

        // includes the two added squares, and not the center
        assert_eq!(combined.fully_visible_relative_squares().len(), 2);
        assert_eq!(combined.fully_visible_local_relative_squares().len(), 2);
    }
    #[test]
    fn test_combine_fovs_to_make_full_circle() {
        let main_center = point2(5, 5);
        let radius = 4;
        let mut fov_1 = FieldOfView::new_with_arc_and_radius(
            main_center,
            AngleInterval::from_degrees(10.0, 55.0),
            radius,
        );
        let mut fov_2 = FieldOfView::new_with_arc_and_radius(
            main_center,
            AngleInterval::from_degrees(55.0, 10.0),
            radius,
        );

        let combined_fov = fov_1.combined_with(&fov_2);
        let combined_and_rasterized = combined_fov.rasterized();

        // the "-1" is because it does not include the center
        let expected_area = (radius * 2 + 1).pow(2) - 1;

        assert_eq!(combined_fov.visible_segments_in_main_view_only.len(), 1);
        let lone_segment = combined_fov
            .visible_segments_in_main_view_only
            .first()
            .unwrap();
        assert_true!(lone_segment.arc().is_full());

        assert_eq!(
            combined_and_rasterized
                .fully_visible_relative_squares()
                .len() as u32,
            expected_area
        );
        assert_eq!(
            combined_and_rasterized
                .fully_visible_local_relative_squares()
                .len() as u32,
            expected_area
        );
    }
    #[test]
    fn test_combined_fovs_combine_visibility__faces_on_one_square() {
        // Non-deterministic test
        // TODO: remove outer loop when deterministic
        (0..100).foreach(|i| {
            (0..5).for_each(|dy| {
                let relative_fully_visible_square = STEP_LEFT * 7 + STEP_UP * dy;
                let absolute_fov_center_square = point2(5, 5);
                let absolute_fully_visible_square =
                    absolute_fov_center_square + relative_fully_visible_square;
                let face_fovs = faces_away_from_center_at_rel_square(relative_fully_visible_square)
                    .iter()
                    .map(|&rel_face| {
                        FieldOfView::new_with_visible_face(absolute_fov_center_square, rel_face)
                    })
                    .collect_vec();

                face_fovs
                    .iter()
                    .map(FieldOfView::rasterized)
                    .for_each(|rasterized_fov| {
                        assert!(rasterized_fov.relative_square_is_only_partially_visible(
                            relative_fully_visible_square
                        ));
                        assert_eq!(
                            rasterized_fov
                                .times_absolute_square_is_visible(absolute_fully_visible_square),
                            1
                        );
                        assert_eq!(
                            rasterized_fov.times_absolute_square_is_fully_visible(
                                absolute_fully_visible_square
                            ),
                            0
                        );
                    });

                let merged_fov = FieldOfView::combine_multiple(face_fovs);

                let merged_rfov = merged_fov.rasterized();

                let fov_created_whole = FieldOfView::new_empty_fov_at(absolute_fov_center_square)
                    .with_fully_visible_relative_square(relative_fully_visible_square);
                let rfov_created_whole = fov_created_whole.rasterized();

                assert_eq!(merged_fov, fov_created_whole);
                assert_eq!(merged_rfov, rfov_created_whole);

                assert!(
                    merged_rfov.relative_square_is_fully_visible(relative_fully_visible_square),
                    "Should be fully visible"
                );
                assert_eq!(
                    merged_rfov.times_absolute_square_is_visible(absolute_fully_visible_square),
                    1
                );
                assert_eq!(
                    merged_rfov
                        .times_absolute_square_is_fully_visible(absolute_fully_visible_square),
                    1
                );
            });
        });
    }

    #[test]
    fn test_combined_fovs_combine_visibility__full_squares_from_faces() {
        let fov_center = point2(5, 5);
        let visible_rel_faces = [
            (12, 0, STEP_RIGHT),
            (12, 1, STEP_RIGHT),
            (12, 2, STEP_RIGHT),
            (12, 3, STEP_RIGHT),
        ];
        let rel_squares_that_should_be_fully_visible = [(12, 1), (12, 2)];
        let fovs = visible_rel_faces
            .iter()
            .map(|&rel_face| FieldOfView::new_with_visible_face(fov_center, rel_face))
            .collect_vec();

        let merged_and_rasterized_fov = FieldOfView::combine_multiple(fovs).rasterized();

        rel_squares_that_should_be_fully_visible
            .iter()
            .map(|&rel_square| {
                let rel_square: WorldStep = rel_square.into();
                let abs_square = fov_center + rel_square;
                assert_eq!(
                    merged_and_rasterized_fov.times_absolute_square_is_visible(abs_square),
                    1
                );
                assert_eq!(
                    merged_and_rasterized_fov.times_absolute_square_is_fully_visible(abs_square),
                    1
                );
                assert!(merged_and_rasterized_fov.relative_square_is_visible(rel_square));
                assert!(merged_and_rasterized_fov.relative_square_is_fully_visible(rel_square));
            });
    }

    #[test]
    fn test_sub_fovs_in_combining_fovs_might_also_combine() {
        let main_center = point2(5, 5);
        let sub_center = point2(15, 5);

        let mut main_fov_1 = FieldOfView::new_empty_fov_at(main_center);
        let mut main_fov_2 = FieldOfView::new_empty_fov_at(main_center);
        let mut sub_fov_1 = FieldOfView::new_empty_fov_at(sub_center);
        let mut sub_fov_2 = FieldOfView::new_empty_fov_at(sub_center);

        let rel_square = STEP_UP_RIGHT * 3;
        let faces = faces_away_from_center_at_rel_square(rel_square)
            .into_iter()
            .collect_vec();
        sub_fov_1.add_fully_visible_relative_face(faces[0]);
        sub_fov_2.add_fully_visible_relative_face(faces[1]);

        main_fov_1.transformed_sub_fovs.push(sub_fov_1);
        main_fov_2.transformed_sub_fovs.push(sub_fov_2);

        let combined_fov = main_fov_1.combined_with(&main_fov_2);

        assert_true!(main_fov_1
            .rasterized()
            .relative_square_is_only_partially_visible(rel_square));
        assert_true!(main_fov_2
            .rasterized()
            .relative_square_is_only_partially_visible(rel_square));

        assert_eq!(combined_fov.transformed_sub_fovs.len(), 1);
        // should be the rel_square and the origin
        let combined_rasterized = combined_fov.rasterized();
        assert_eq!(
            combined_rasterized.fully_visible_relative_squares().len(),
            1
        );
        assert_eq!(
            combined_rasterized.fully_visible_relative_squares(),
            as_set([rel_square])
        );
        assert!(combined_rasterized.relative_square_is_fully_visible(rel_square));
    }

    #[test]
    fn test_fov_relative_to_absolute__top_level() {
        let main_center = point2(5, 5);

        let mut fov = FieldOfView::new_empty_fov_at(main_center);

        let rel_square = STEP_DOWN_LEFT * 3;
        let correct_abs_square = main_center + rel_square;

        fov.add_view_segment_for_fully_visible_relative_square(rel_square);

        let rasterized_fov = fov.rasterized();

        assert_eq!(
            rasterized_fov.times_absolute_square_is_visible(correct_abs_square),
            1
        );
        assert_eq!(
            rasterized_fov.times_absolute_square_is_fully_visible(correct_abs_square),
            1
        );
        let visibility = rasterized_fov
            .top_down_portal_entrance_shapes_at_relative_square(rel_square)[0]
            .clone();
        assert_eq!(
            rasterized_fov.absolute_squares_visible_at_relative_square(rel_square),
            SquareSet::from([correct_abs_square])
        );

        // ..vo
        // .vvv
        // vvv.
        // xv..
        // 9 visible squares (not counting start)

        assert_eq!(fov.rasterized().visible_relative_squares().len(), 9);
    }

    #[test]
    fn test_fov_relative_to_absolute__sub_view_no_rotation() {
        let main_center = point2(5, 5);
        let sub_center = point2(34, -7);

        let mut fov = FieldOfView::new_empty_fov_at(main_center);
        let mut sub_fov = FieldOfView::new_empty_fov_at(sub_center);

        let rel_square = STEP_DOWN_LEFT * 3;
        let abs_square = sub_center + rel_square;

        sub_fov.add_view_segment_for_fully_visible_relative_square(rel_square);

        fov.transformed_sub_fovs.push(sub_fov);

        let rfov = fov.rasterized();

        assert_eq!(
            rfov.absolute_squares_visible_at_relative_square(rel_square),
            [abs_square].into()
        );
    }

    #[test]
    fn test_fov_relative_to_absolute__sub_view_with_rotation() {
        let mut fov = FieldOfView::new_empty_fov_at((5, 5));

        let sub_root = fov
            .root_square_with_direction
            .turned_right()
            .at_square((34, -7));
        let mut sub_fov = FieldOfView::new_empty_fov_with_root(sub_root);

        let rel_square = STEP_DOWN_LEFT * 1;

        sub_fov.add_view_segment_for_fully_visible_relative_square(rel_square);
        fov.transformed_sub_fovs.push(sub_fov);

        let rfov = fov.rasterized();

        let correct_abs_square = sub_root.square() + STEP_UP_LEFT;
        assert_eq!(
            rfov.absolute_squares_visible_at_relative_square(rel_square),
            SquareSet::from([correct_abs_square])
        );
    }

    #[test]
    fn test_partial_visibility_in_blindspot_of_nearly_full_arc() {
        let rel_square = vec2(4, 4);
        // These values are from an observed failure.  NOT ARBITRARY
        let arc =
            PartialAngleInterval::from_angles(Angle::radians(0.7853978), Angle::radians(0.7853982));
        let visibility = SquareVisibility::from_relative_square_and_view_arc(arc, rel_square);
    }

    #[test]
    fn test_visibility_of_multiple_absolute_squares_in_one_relative_square__main_view_and_sub_view()
    {
        let main_center = point2(5, 5);
        let other_center = point2(15, 5);

        let mut fov_1 = FieldOfView::new_empty_fov_at(main_center);
        let mut sub_fov_1 = FieldOfView::new_empty_fov_at(other_center);

        let rel_square = STEP_DOWN_LEFT * 3;
        let faces_of_rel_square = faces_away_from_center_at_rel_square(rel_square)
            .into_iter()
            .collect_vec();
        fov_1.add_fully_visible_relative_face(faces_of_rel_square[0]);
        sub_fov_1.add_fully_visible_relative_face(faces_of_rel_square[1]);

        fov_1.transformed_sub_fovs.push(sub_fov_1.clone());

        // debug_print_fov_as_relative(&fov_1, 20);
        // debug_print_fov_as_absolute(&fov_1, 20);
        let rfov = fov_1.rasterized();
        let sub_rfov = sub_fov_1.rasterized();

        assert_eq!(
            rfov.top_down_portal_entrance_shapes_at_relative_square(rel_square)
                .len(),
            2
        );
        assert_eq!(
            sub_rfov
                .top_down_portal_entrance_shapes_at_relative_square(rel_square)
                .len(),
            1
        );
    }

    #[test]
    fn test_center_of_fov_is_not_visible() {
        let square = point2(4, 5);
        let rasterized_fov = portal_aware_field_of_view_from_square(
            square,
            0,
            &Default::default(),
            &Default::default(),
        )
        .rasterized();
        assert_eq!(rasterized_fov.visible_local_relative_squares().len(), 0);
        assert_false!(rasterized_fov.relative_square_is_fully_visible(STEP_ZERO));
        assert_false!(rasterized_fov.relative_square_is_visible(STEP_ZERO));
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

        debug_print_fov_as_relative(&fov, 5);
        rel_blocks.iter().for_each(|rel_block| {
            assert!(
                fov.rasterized()
                    .relative_square_is_fully_visible(*rel_block),
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
        debug_print_fov_as_relative(&new_fov_result, 2);
        let rasterized_fov = new_fov_result.rasterized();
        let test_step = STEP_RIGHT;
        let visibilities_of_one_right =
            rasterized_fov.top_down_portal_entrance_shapes_at_relative_square(test_step);

        // check that the relative square corresponds to exactly one absolute square
        assert_eq!(
            rasterized_fov
                .absolute_squares_visible_at_relative_square(test_step)
                .len(),
            1
        );
        let the_positioned_visibility = visibilities_of_one_right[0].clone();
        // check that the relative square is not reached through any portals
        assert!(rasterized_fov.relative_square_is_only_locally_visible(test_step));

        let the_square_visibility = rasterized_fov
            .lone_square_visibility_in_exit_frame_for_relative_square_or_panic(test_step);
        // check the visibility of the relative square
        assert_about_eq!(
            the_square_visibility
                .visible_portion()
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

        debug_print_fov_as_relative(&new_fov_result, 7);
        debug_print_fov_as_absolute(&new_fov_result, 7);

        assert_eq!(new_fov_result.transformed_sub_fovs.len(), 1);
        let test_square = STEP_UP_RIGHT;
        let rasterized_fov = new_fov_result.rasterized();
        assert!(rasterized_fov.relative_square_is_fully_visible(test_square));
        assert_eq!(
            rasterized_fov.lone_portal_depth_for_relative_square_or_panic(test_square),
            1
        );
        assert_eq!(
            rasterized_fov.lone_portal_rotation_for_relative_square_or_panic(test_square),
            QuarterTurnsCcw::new(0)
        );
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
            entrance.square() + STEP_UP_LEFT * 5,
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

        debug_print_fov_as_relative(&new_fov_result, 10);
        debug_print_fov_as_absolute(&new_fov_result, 10);

        assert_eq!(new_fov_result.transformed_sub_fovs.len(), 1);

        let test_square = STEP_RIGHT * 3;

        let visibilities_of_test_square = new_fov_result
            .rasterized()
            .top_down_portal_entrance_shapes_at_relative_square(test_square);
        let rasterized_fov = new_fov_result.rasterized();
        assert_eq!(
            rasterized_fov
                .top_down_portals_for_relative_square(test_square)
                .len(),
            1
        );
        let the_positioned_visibility = visibilities_of_test_square[0].clone();
        assert_eq!(
            rasterized_fov.lone_portal_depth_for_relative_square_or_panic(test_square),
            1
        );
        assert_eq!(
            rasterized_fov.lone_portal_rotation_for_relative_square_or_panic(test_square),
            QuarterTurnsCcw::new(1)
        );
        let the_square_visibility = rasterized_fov
            .lone_square_visibility_in_entrance_frame_for_relative_square_or_panic(test_square);
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
    fn test_rasterize_segments_for_one_local_square() {
        let root_square = point2(5, 10);
        let mut narrow_fov = FieldOfView::new_empty_fov_with_root((root_square, STEP_UP));
        let dx = 2;
        narrow_fov.add_view_segment_for_fully_visible_relative_square(STEP_RIGHT * 3);

        let rasterized_fov = narrow_fov.rasterized();

        assert_eq!(rasterized_fov.number_of_visible_relative_squares(), 3);
        assert_eq!(rasterized_fov.number_of_fully_visible_relative_squares(), 1);

        assert_false!(rasterized_fov.relative_square_is_visible(vec2(0, 0)));
        assert!(rasterized_fov.relative_square_is_only_partially_visible(vec2(1, 0)));
        assert!(rasterized_fov.relative_square_is_only_partially_visible(vec2(2, 0)));
        assert!(rasterized_fov.relative_square_is_fully_visible(vec2(3, 0)));
    }
    #[test]
    fn test_square_is_not_fully_visible_if_view_arc_starts_in_that_square() {
        assert!(SquareVisibility::from_relative_square_and_view_arc(
            PartialAngleInterval::from_degrees(0.0, 20.0,), // arbitrary angles
            STEP_ZERO
        )
        .is_none())
        // .is_some_and(|vis| vis.is_fully_visible()))
    }
    #[test]
    fn test_rasterize_one_view_segment() {
        let mut fov = FieldOfView::new_empty_fov_at(point2(5, 5));
        fov.add_fully_visible_relative_face(((3, 0), STEP_RIGHT));
        assert_eq!(fov.visible_segments_in_main_view_only.len(), 1);
        assert!(fov.transformed_sub_fovs.is_empty());
        let rasterized = fov.rasterized();
        assert_eq!(rasterized.number_of_visible_relative_squares(), 3);
        assert_eq!(rasterized.number_of_fully_visible_relative_squares(), 0);
        assert_false!(rasterized.relative_square_is_fully_visible(STEP_ZERO));
        assert!(rasterized.relative_square_is_only_partially_visible(vec2(1, 0)));
        assert!(rasterized.relative_square_is_only_partially_visible(vec2(2, 0)));
        assert!(rasterized.relative_square_is_only_partially_visible(vec2(3, 0)));

        assert_eq!(rasterized.view_root, (5, 5, STEP_UP).into());

        assert_eq!(
            rasterized.relative_square_to_absolute_square((2, 0)),
            (7, 5).into()
        );
        assert_eq!(rasterized.times_absolute_square_is_visible((7, 5)), 1);
    }
    #[test]
    fn test_rasterize_empty_fov() {
        let mut fov = FieldOfView::new_empty_fov_at(point2(5, 5));

        assert_eq!(fov.visible_segments_in_main_view_only.len(), 0);
        assert!(fov.transformed_sub_fovs.is_empty());

        let rasterized = fov.rasterized();

        // Center square should be fully visible, even on an empty fov
        assert_eq!(rasterized.number_of_visible_relative_squares(), 0);
        assert_eq!(rasterized.number_of_fully_visible_relative_squares(), 0);
        assert_false!(rasterized.relative_square_is_fully_visible(vec2(0, 0)));
    }

    fn as_set<TT: Into<T>, T: std::hash::Hash + std::cmp::Eq>(
        v: impl IntoIterator<Item = TT>,
    ) -> HashSet<T> {
        v.into_iter().map_into().collect()
    }

    #[test]
    fn test_rasterize_fov_with_narrow_view_segment__with_start_line() {
        let view_segment: AngleBasedVisibleSegment =
            AngleBasedVisibleSegment::from_arc_and_fence_radius(
                AngleInterval::from_degrees(0.0, 0.001),
                5,
            )
            .with_start_face((3, 0, STEP_RIGHT));
        let mut fov = FieldOfView::new_empty_fov_at((5, 7));
        fov.visible_segments_in_main_view_only.push(view_segment);
        let rfov = fov.rasterized();
        assert_eq!(rfov.visible_relative_squares(), as_set([(4, 0), (5, 0)]));
    }
    #[test]
    fn test_rasterize_fov_with_sub_view__should_have_correct_portal_depths() {
        let mut main_fov = FieldOfView::new_with_fully_visible_relative_square((0, 0), (5, 0));
        let mut sub_fov = FieldOfView::new_with_fully_visible_relative_square((20, 20), (0, 5))
            .with_fully_visible_relative_square((0, 5));
        main_fov.transformed_sub_fovs.push(sub_fov);
        let rfov = main_fov.rasterized();

        assert_eq!(
            rfov.lone_portal_depth_for_relative_square_or_panic((1, 0)),
            0
        );
        assert_eq!(
            rfov.lone_portal_depth_for_relative_square_or_panic((0, 1)),
            1
        );
    }
    #[test]
    fn test_observed_entrance_collision_from_rotated_portal() {
        let base_square = WorldSquare::new(0, 0);
        let target_rel_square = WorldStep::new(1, 1);

        let x_entrance_pose: SquareWithOrthogonalDir = (1, 0, STEP_UP).into();
        let y_entrance_pose: SquareWithOrthogonalDir = (0, 1, STEP_RIGHT).into();

        let portal_geometry = PortalGeometry::new()
            .with_portal(
                x_entrance_pose,
                x_entrance_pose
                    .stepped()
                    .strafed_right_n(5)
                    .quarter_rotated_ccw_in_place(0),
            )
            .with_portal(
                y_entrance_pose,
                y_entrance_pose
                    .stepped()
                    .strafed_left_n(5)
                    .quarter_rotated_ccw_in_place(1),
            );
        let fov = portal_aware_field_of_view_from_square(
            base_square,
            1,
            &Default::default(),
            &portal_geometry,
        );
        dbg!(&fov);
        let rfov = fov.rasterized();
        debug_print_fov_as_absolute(&fov, 7);
        debug_print_fov_as_relative(&fov, 2);

        // No asserts, just make sure it doesn't crash
    }
    #[test]
    fn test_observed_angle_interval_related_failure() {
        let base_square = WorldSquare::new(0, 0);
        let target_rel_square = WorldStep::new(1, 1);

        let entrance_pose: SquareWithOrthogonalDir = (0, 1, STEP_RIGHT).into();
        let portal_geometry = PortalGeometry::new().with_portal(
            entrance_pose,
            entrance_pose
                .stepped()
                .strafed_left_n(5)
                .quarter_rotated_ccw_in_place(1),
        );
        let fov = portal_aware_field_of_view_from_square(
            base_square,
            1,
            &Default::default(),
            &portal_geometry,
        );
        let rfov = fov.rasterized();
        debug_print_fov_as_absolute(&fov, 7);
        debug_print_fov_as_relative(&fov, 2);

        // No asserts, just make sure it doesn't crash
    }
    #[test]
    fn test_rasterize_one_rotated_view() {
        let mut base_fov = FieldOfView::new_empty_fov_at((0, 0));
        let mut sub_fov = FieldOfView::new_empty_fov_with_root((5, 0, STEP_LEFT));
        sub_fov.visible_segments_in_main_view_only.push(
            AngleBasedVisibleSegment::from_arc_and_fence_radius(
                AngleInterval::from_degrees(0.0, 45.0),
                5,
            ),
        );
        base_fov.transformed_sub_fovs.push(sub_fov);
        let rfov = base_fov.rasterized();
        debug_print_fov_as_absolute(&base_fov, 6);
        debug_print_fov_as_relative(&base_fov, 4);
        let test_square = (2, 0);
        let top_down_portal = rfov.lone_top_down_portal_for_relative_square_or_panic(test_square);
        dbg!(&top_down_portal);
        let entrance = top_down_portal.shape_in_entrance_frame();
        let exit = top_down_portal.shape_in_exit_frame();
        let l = 0.4;
        let v = |vector: WorldStep| vector.to_f32().to_point().cast_unit() * l;
        assert_true!(entrance.point_is_visible(v(STEP_UP_LEFT)));
        assert_true!(entrance.point_is_visible(v(STEP_UP_RIGHT)));
        assert_false!(entrance.point_is_visible(v(STEP_DOWN_LEFT)));
        assert_false!(entrance.point_is_visible(v(STEP_DOWN_RIGHT)));

        assert_true!(exit.point_is_visible(v(STEP_UP_LEFT)));
        assert_false!(exit.point_is_visible(v(STEP_UP_RIGHT)));
        assert_true!(exit.point_is_visible(v(STEP_DOWN_LEFT)));
        assert_false!(exit.point_is_visible(v(STEP_DOWN_RIGHT)));
    }
}
