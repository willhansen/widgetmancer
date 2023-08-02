pub mod angle_based_visible_segment;
pub mod rasterized_field_of_view;
pub mod square_visibility;

use std::backtrace::Backtrace;
use std::collections::{HashMap, HashSet};
use std::f32::consts::PI;
use std::fmt::{Debug, Formatter};

use crate::fov_stuff::angle_based_visible_segment::AngleBasedVisibleSegment;
use crate::fov_stuff::rasterized_field_of_view::{
    PositionedVisibilityOfSquare, RasterizedFieldOfView,
};
use crate::fov_stuff::square_visibility::{
    RelativeSquareVisibilityMap, RelativeSquareVisibilityTrait, SquareVisibility,
    SquareVisibilityFromOneLargeShadow, SquareVisibilityFunctions, SquareVisibilityMapFunctions,
};
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
use crate::graphics::Graphics;
use crate::piece::MAX_PIECE_RANGE;
use crate::portal_geometry::{Portal, PortalGeometry};
use crate::utility::angle_interval::{AngleInterval, PartialAngleInterval};
use crate::utility::coordinate_frame_conversions::*;
use crate::utility::*;

const NARROWEST_VIEW_CONE_ALLOWED_IN_DEGREES: f32 = 0.001;

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

    pub fn visible_segments_in_main_view_only(&self) -> &Vec<AngleBasedVisibleSegment> {
        &self.visible_segments_in_main_view_only
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

    pub fn rasterized(&self) -> RasterizedFieldOfView {
        todo!();
        // self.all_view_segments_converted_to_relative_frame()
        //     .iter()
        //     .map(|segment: &AngleBasedVisibleSegment| segment.to_square_visibilities())
        //     .reduce(|a, b| a.combined_with_while_increasing_visibility(&b))
        //     .unwrap()
    }
    fn all_view_segments_converted_to_relative_frame(&self) -> Vec<AngleBasedVisibleSegment> {
        concat([
            self.visible_segments_in_main_view_only.clone(),
            self.transformed_sub_fovs
                .iter()
                .flat_map(|sub_fov| {
                    self.other_converted_to_this_frame(sub_fov)
                        .all_view_segments_converted_to_relative_frame()
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

// TODO: Break this function into more pieces
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
    // Returns None if not visible
    let square_arc = PartialAngleInterval::from_relative_square(square_relative_to_center);
    assert!(visibility_arc.touches_or_overlaps(square_arc)); // This invalidates the None return case

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
            let maybe_drawable = Graphics::drawable_at_relative_square(
                &fov.rasterized(),
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
    use crate::fov_stuff::square_visibility::LocalSquareHalfPlane;
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
        better_angle_from_x_axis, QuarterTurnsAnticlockwise, SquareWithKingDir,
        SquareWithOrthogonalDir, STEP_DOWN, STEP_LEFT, STEP_UP,
    };

    use super::*;

    const SIGHT_RADIUS: u32 = 16;

    #[test]
    #[timeout(100)]
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
    #[timeout(100)]
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
    #[timeout(100)]
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
    #[timeout(100)]
    fn test_small_field_of_view_with_no_obstacles() {
        let start_square = point2(5, 5);
        let radius = 2;
        let fov_result = portal_aware_field_of_view_from_square(
            start_square,
            radius,
            &SquareSet::default(),
            &PortalGeometry::default(),
        )
        .rasterized();

        //print_fov_as_relative(&fov_result, 5);
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
    #[timeout(100)]
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
        assert!(fov_result.can_fully_and_seamlessly_see_relative_square(block_step));
        assert!(fov_result.can_fully_and_seamlessly_see_relative_square(block_step + STEP_DOWN));
        assert_false!(fov_result.can_fully_and_seamlessly_see_relative_square(block_step + STEP_UP));
    }

    #[test]
    #[timeout(100)]
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
            .only_partially_visible_relative_squares_in_main_view_only()
            .is_empty());
    }

    #[test]
    #[timeout(100)]
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
                .relative_square_visibility_map_of_main_view_only()
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
    #[timeout(100)]
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
    #[timeout(100)]
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
    #[timeout(100)]
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
        assert!(fov_result
            .can_fully_and_seamlessly_see_relative_square((visible_rel_square + STEP_LEFT)));
        assert!(fov_result.can_fully_and_seamlessly_see_relative_square(visible_rel_square));
    }

    #[test]
    #[timeout(100)]
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
            .relative_square_visibility_map_of_main_view_only()
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
    #[timeout(100)]
    fn test_partial_visibility_of_one_square__observed_discontinuity_1() {
        assert_shadow_is_horizontally_continuous(
            PartialVisibilityDrawable::from_square_visibility(
                square_visibility_from_block_and_square(STEP_DOWN_LEFT, vec2(-1, -3)).unwrap(),
            )
            .to_glyphs(),
        );
    }

    #[test]
    #[timeout(100)]
    fn test_partial_visibility_of_one_square__observed_discontinuity_2() {
        assert_shadow_is_horizontally_continuous(
            PartialVisibilityDrawable::from_square_visibility(
                square_visibility_from_block_and_square(STEP_DOWN_RIGHT, vec2(9, -3)).unwrap(),
            )
            .to_glyphs(),
        );
    }

    #[test]
    #[timeout(100)]
    fn test_partial_visibility_of_one_square__observed_discontinuity_3() {
        assert_shadow_is_horizontally_continuous(
            PartialVisibilityDrawable::from_square_visibility(
                square_visibility_from_block_and_square(STEP_UP_LEFT, vec2(-14, 5)).unwrap(),
            )
            .to_glyphs(),
        );
    }

    #[test]
    #[timeout(100)]
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
    #[timeout(100)]
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
    #[timeout(100)]
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
    #[timeout(100)]
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
    #[timeout(100)]
    fn test_get_mapping_from_fov_result() {
        let center: WorldSquare = point2(5, 5);
        let mut fov = FieldOfView::new_empty_fov_at(center);
        let relative_square = vec2(2, 2);
        fov.add_fully_visible_relative_square(relative_square);

        let square_visibility = fov
            .rasterized()
            .visibilities_of_relative_square(relative_square);
        assert!(square_visibility
            .get(0)
            .unwrap()
            .square_visibility_in_absolute_frame()
            .is_fully_visible());
    }

    #[test]
    #[timeout(100)]
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
        )
        .rasterized();

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
    #[timeout(100)]
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
    #[timeout(100)]
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

        assert_eq!(angle_based_fov_result.transformed_sub_fovs.len(), 1);
        // Not fully visible because only one octant
        assert!(fov_result.can_see_relative_square(STEP_RIGHT * 2));
        assert_false!(fov_result.can_see_absolute_square(portal_entrance.square() + STEP_RIGHT));
        assert!(fov_result.can_see_absolute_square(portal_exit.square()));

        assert_false!(angle_based_fov_result.transformed_sub_fovs[0]
            .rasterized()
            .can_fully_and_seamlessly_see_relative_square(STEP_ZERO));
    }

    #[test]
    #[timeout(100)]
    fn test_sub_fov_view_transform() {
        let sub_center = SquareWithOrthogonalDir::from_square_and_step(point2(1, 0), STEP_RIGHT);
        let mut sub_fov = FieldOfView::new_empty_fov_at(sub_center.square());
        sub_fov.root_square_with_direction = sub_center;

        let main_center = SquareWithOrthogonalDir::from_square_and_step(point2(50, 0), STEP_UP);
        let mut main_fov = FieldOfView::new_empty_fov_at(main_center.square());
        main_fov.root_square_with_direction = main_center;

        let target_square: WorldSquare = point2(1, 4);

        sub_fov.add_fully_visible_relative_square(target_square - sub_fov.root_square());

        main_fov.transformed_sub_fovs.push(sub_fov);

        let rel_from_main = STEP_LEFT * 4;

        let rasterized_fov = main_fov.rasterized();

        assert!(rasterized_fov.can_fully_and_seamlessly_see_relative_square(rel_from_main));
        assert!(rasterized_fov.can_see_absolute_square(point2(1, 4)));

        assert_eq!(
            rasterized_fov.at_least_partially_visible_relative_squares_including_subviews(),
            StepSet::from([rel_from_main])
        )
    }

    #[test]
    #[timeout(100)]
    fn test_square_fully_covered_by_face() {
        let view_arc_of_face =
            PartialAngleInterval::from_relative_square_face((STEP_RIGHT, STEP_RIGHT));
        let square = STEP_RIGHT * 2;

        let visibility = visibility_of_square(view_arc_of_face, square);
        assert!(visibility.unwrap().is_fully_visible());
    }

    #[test]
    #[timeout(100)]
    fn test_square_fully_not_covered_by_adjacent() {
        let view_arc_of_face =
            PartialAngleInterval::from_relative_square_face((STEP_UP_RIGHT, STEP_RIGHT));
        let square = STEP_RIGHT * 2;

        let visibility = visibility_of_square(view_arc_of_face, square);
        assert!(visibility.is_none());
    }

    #[test]
    #[timeout(100)]
    fn test_square_fully_inside_view_arc__near_edge() {
        let square = vec2(1, -2);
        let arc = PartialAngleInterval::from_radians(-PI / 2.0, -PI / 4.0);
        assert!(visibility_of_square(arc, square)
            .unwrap()
            .is_fully_visible());
    }

    #[test]
    #[timeout(100)]
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
    #[timeout(100)]
    fn test_simple_fov_combination() {
        let main_center = point2(5, 5);
        let mut fov_1 = FieldOfView::new_empty_fov_at(main_center);
        let mut fov_2 = FieldOfView::new_empty_fov_at(main_center);
        fov_1.add_fully_visible_relative_square(STEP_RIGHT);
        fov_2.add_fully_visible_relative_square(STEP_UP);

        let combined = fov_1.combined_with(&fov_2).rasterized();

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
    #[timeout(100)]
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
                assert_false!(fov
                    .rasterized()
                    .visibilities_of_absolute_square(test_square)[0]
                    .square_visibility_in_absolute_frame()
                    .is_fully_visible())
            });

            let merged_fov = fovs
                .iter()
                .cloned()
                .reduce(|a, b| a.combined_with(&b))
                .unwrap()
                .rasterized();

            assert!(merged_fov.visibilities_of_absolute_square(test_square)[0]
                .square_visibility_in_absolute_frame()
                .is_fully_visible());
        });
    }

    #[test]
    #[timeout(100)]
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
            assert_false!(fov
                .rasterized()
                .visibilities_of_absolute_square(test_square)[0]
                .square_visibility_in_absolute_frame()
                .is_fully_visible())
        });

        let merged_fov = fovs
            .iter()
            .cloned()
            .reduce(|a, b| a.combined_with(&b))
            .unwrap();

        assert!(merged_fov
            .rasterized()
            .visibilities_of_absolute_square(test_square)[0]
            .square_visibility_in_absolute_frame()
            .is_fully_visible());
    }

    #[test]
    #[timeout(100)]
    fn test_sub_fovs_in_combining_fovs_might_also_combine() {
        let main_center = point2(5, 5);
        let other_center = point2(15, 5);

        let mut fov_1 = FieldOfView::new_empty_fov_at(main_center);
        let mut fov_2 = FieldOfView::new_empty_fov_at(main_center);
        let mut sub_fov_1 = FieldOfView::new_empty_fov_at(other_center);
        let mut sub_fov_2 = FieldOfView::new_empty_fov_at(other_center);

        let rel_square = STEP_UP_RIGHT * 3;
        let faces = faces_away_from_center_at_rel_square(rel_square)
            .into_iter()
            .collect_vec();
        sub_fov_1.add_fully_visible_relative_face(faces[0]);
        sub_fov_2.add_fully_visible_relative_face(faces[1]);

        fov_1.transformed_sub_fovs.push(sub_fov_1);
        fov_2.transformed_sub_fovs.push(sub_fov_2);

        let combined_fov = fov_1.combined_with(&fov_2);

        assert_eq!(combined_fov.transformed_sub_fovs.len(), 1);
        // should be the rel_square and the origin
        let combined_rasterized = combined_fov.rasterized();
        assert_eq!(
            combined_rasterized
                .fully_visible_relative_squares_including_subviews()
                .len(),
            2
        );
        assert!(combined_rasterized.can_fully_and_seamlessly_see_relative_square(rel_square));
        assert!(combined_rasterized.can_fully_and_seamlessly_see_relative_square(STEP_ZERO));
    }

    #[test]
    #[timeout(100)]
    fn test_fov_relative_to_absolute__top_level() {
        let main_center = point2(5, 5);

        let mut fov = FieldOfView::new_empty_fov_at(main_center);

        let rel_square = STEP_DOWN_LEFT * 3;
        let correct_abs_square = main_center + rel_square;

        fov.add_fully_visible_relative_square(rel_square);

        let visibility = fov.rasterized().visibilities_of_relative_square(rel_square)[0].clone();
        let abs_square = visibility.absolute_square();
        assert_eq!(abs_square, correct_abs_square);
        assert_eq!(
            fov.rasterized()
                .at_least_partially_visible_relative_squares_including_subviews()
                .len(),
            1
        );
        assert!(visibility
            .square_visibility_in_absolute_frame()
            .is_fully_visible());
    }

    #[test]
    #[timeout(100)]
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
            fov.rasterized().visibilities_of_relative_square(rel_square)[0].absolute_square(),
            abs_square
        );
    }

    #[test]
    #[timeout(100)]
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
            fov.rasterized().visibilities_of_relative_square(rel_square)[0].absolute_square(),
            abs_square
        );
    }

    #[test]
    #[timeout(100)]
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
    #[timeout(100)]
    fn test_visibility_of_multiple_squares_in_one_square() {
        let main_center = point2(5, 5);
        let other_center = point2(15, 5);

        let mut fov_1 = FieldOfView::new_empty_fov_at(main_center);
        let mut sub_fov_1 = FieldOfView::new_empty_fov_at(other_center);

        let rel_square = STEP_DOWN_LEFT * 3;
        let faces = faces_away_from_center_at_rel_square(rel_square)
            .into_iter()
            .collect_vec();
        fov_1.add_fully_visible_relative_face(faces[0]);
        sub_fov_1.add_fully_visible_relative_face(faces[1]);

        fov_1.transformed_sub_fovs.push(sub_fov_1.clone());

        assert_eq!(
            fov_1
                .rasterized()
                .visibilities_of_relative_square(rel_square)
                .len(),
            2
        );
        assert_eq!(
            sub_fov_1
                .rasterized()
                .visibilities_of_relative_square(rel_square)
                .len(),
            2
        );
    }

    #[test]
    #[timeout(100)]
    fn test_center_of_fov_is_visible() {
        let square = point2(4, 5);
        let fov = portal_aware_field_of_view_from_square(
            square,
            0,
            &Default::default(),
            &Default::default(),
        )
        .rasterized();
        assert_eq!(
            fov.at_least_partially_visible_relative_squares_in_main_view_only()
                .len(),
            1
        );
        assert!(fov
            .visibility_of_relative_square_in_main_view(STEP_ZERO)
            .unwrap()
            .square_visibility_in_absolute_frame()
            .is_fully_visible());
    }

    #[ignore = "not a priority for the time being"]
    #[test]
    #[timeout(100)]
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
                fov.rasterized()
                    .can_fully_and_seamlessly_see_relative_square(*rel_block),
                "rel_block: {:?}",
                rel_block
            )
        });
    }

    #[test]
    #[timeout(100)]
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
        let visibilities_of_one_right = new_fov_result
            .rasterized()
            .visibilities_of_relative_square(STEP_RIGHT);
        assert_eq!(visibilities_of_one_right.len(), 1);
        let the_positioned_visibility = visibilities_of_one_right[0].clone();
        assert_eq!(the_positioned_visibility.portal_depth(), 0);
        assert_eq!(
            the_positioned_visibility.portal_rotation(),
            QuarterTurnsAnticlockwise::new(0)
        );
        let the_square_visibility = the_positioned_visibility.square_visibility_in_absolute_frame();
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
    #[timeout(100)]
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
        let visibilities_of_test_square = new_fov_result
            .rasterized()
            .visibilities_of_relative_square(test_square);
        assert_eq!(visibilities_of_test_square.len(), 1);
        assert!(new_fov_result
            .rasterized()
            .can_fully_and_seamlessly_see_relative_square(test_square));
        let the_positioned_visibility = visibilities_of_test_square[0].clone();
        assert_eq!(the_positioned_visibility.portal_depth(), 1);
        assert_eq!(
            the_positioned_visibility.portal_rotation(),
            QuarterTurnsAnticlockwise::new(0)
        );
        let the_square_visibility = the_positioned_visibility.square_visibility_in_relative_frame();
        assert!(the_square_visibility.is_fully_visible());
    }

    #[test]
    #[timeout(100)]
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

        let visibilities_of_test_square = new_fov_result
            .rasterized()
            .visibilities_of_relative_square(test_square);
        assert_eq!(visibilities_of_test_square.len(), 1);
        let the_positioned_visibility = visibilities_of_test_square[0].clone();
        assert_eq!(the_positioned_visibility.portal_depth(), 1);
        assert_eq!(
            the_positioned_visibility.portal_rotation(),
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
    #[timeout(100)]
    fn test_squares_touched_by_angle_based_visible_segment__simple_horizontal() {
        let seg = AngleBasedVisibleSegment::from_relative_face((STEP_RIGHT * 5, STEP_RIGHT));
        assert_eq!(
            seg.get_touching_relative_squares(),
            (0..=5).map(|i| STEP_RIGHT * i).collect()
        )
    }
}
