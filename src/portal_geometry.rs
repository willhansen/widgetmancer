use std::collections::{HashMap, HashSet};
use std::ops::Add;

use crate::fov_stuff::{AngleBasedVisibleSegment, FieldOfView};
use derive_more::Constructor;
use derive_more::Neg;
use euclid::{point2, Angle};
use getset::CopyGetters;
use itertools::Itertools;
use ntest::assert_false;

use crate::utility::angle_interval::PartialAngleInterval;
use crate::utility::coordinate_frame_conversions::{
    StepSet, WorldMove, WorldPoint, WorldSquare, WorldStep,
};
use crate::utility::{
    better_angle_from_x_axis, first_inside_square_face_hit_by_ray, is_orthogonal,
    ith_projection_of_step, naive_ray_endpoint, revolve_square,
    rotated_n_quarter_turns_counter_clockwise, unit_vector_from_angle, Octant,
    QuarterTurnsAnticlockwise, RelativeSquareWithOrthogonalDir, SquareWithKingDir,
    SquareWithOrthogonalDir, StepWithQuarterRotations, WorldLine, STEP_RIGHT, STEP_ZERO,
};

#[derive(Hash, Clone, Copy, Debug)]
pub struct RigidTransform {
    start_pose: SquareWithOrthogonalDir,
    end_pose: SquareWithOrthogonalDir,
}

impl RigidTransform {
    pub fn from_start_and_end_poses(
        start: SquareWithOrthogonalDir,
        end: SquareWithOrthogonalDir,
    ) -> Self {
        RigidTransform {
            start_pose: start,
            end_pose: end,
        }
    }
    pub fn translation(&self) -> WorldStep {
        (self.end_pose - self.start_pose).stepp()
    }
    pub fn rotation(&self) -> QuarterTurnsAnticlockwise {
        (self.end_pose - self.start_pose).rotation()
    }
    pub fn transform_absolute_pose(
        &self,
        pose: SquareWithOrthogonalDir,
    ) -> SquareWithOrthogonalDir {
        let end_square = revolve_square(pose.square(), self.start_pose.square(), self.rotation())
            + self.translation();

        let end_direction = self.rotation().rotate_vector(pose.direction().step());

        SquareWithOrthogonalDir::from_square_and_step(end_square, end_direction)
    }
    // TODO: maybe test this if sus
    pub fn transform_relative_pose(
        &self,
        pose: RelativeSquareWithOrthogonalDir,
    ) -> RelativeSquareWithOrthogonalDir {
        let end_square = rotated_n_quarter_turns_counter_clockwise(
            pose.square(),
            self.rotation().quarter_turns(),
        );

        let end_direction = self.rotation().rotate_vector(pose.direction().step());

        RelativeSquareWithOrthogonalDir::from_square_and_step(end_square, end_direction)
    }
    pub fn transform_octant(&self, octant: Octant) -> Octant {
        octant.with_n_quarter_turns_anticlockwise(self.rotation())
    }
    pub fn transform_arc(&self, arc: PartialAngleInterval) -> PartialAngleInterval {
        arc.rotated_quarter_turns(self.rotation())
    }
    pub fn rotate_step(&self, step: WorldStep) -> WorldStep {
        self.rotation().rotate_vector(step)
    }
    pub fn rotate_steps(&self, steps: &StepSet) -> StepSet {
        steps
            .iter()
            .map(|&step: &WorldStep| self.rotation().rotate_vector(step))
            .collect()
    }
    pub fn transform_ray(
        &self,
        ray_start: WorldPoint,
        ray_direction: Angle<f32>,
    ) -> (WorldPoint, Angle<f32>) {
        let ray_start_relative_to_tf_start = ray_start - self.start_pose.square().to_f32();
        let dist_from_tf_start = ray_start_relative_to_tf_start.length();
        let start_tf_angle = better_angle_from_x_axis(self.start_pose.direction().step().to_f32());

        let ray_angle_from_tf_start = start_tf_angle.angle_to(ray_direction);
        let end_tf_angle = better_angle_from_x_axis(self.end_pose.direction().step().to_f32());

        let new_ray_direction = end_tf_angle + ray_angle_from_tf_start;

        let new_ray_start = if dist_from_tf_start == 0.0 {
            self.end_pose.square().to_f32()
        } else {
            let tf_start_point = self.start_pose.direction().step().to_f32();
            let position_angle_from_tf_start =
                tf_start_point.angle_to(ray_start_relative_to_tf_start);

            let position_angle_from_tf_end = end_tf_angle + position_angle_from_tf_start;

            self.end_pose.square().to_f32()
                + unit_vector_from_angle(position_angle_from_tf_end).cast_unit()
                    * dist_from_tf_start
        };

        (new_ray_start, new_ray_direction)
    }
}

impl PartialEq for RigidTransform {
    fn eq(&self, other: &Self) -> bool {
        self.transform_absolute_pose(other.start_pose) == other.end_pose
    }
}

impl Eq for RigidTransform {}

impl Default for RigidTransform {
    fn default() -> Self {
        RigidTransform::from_start_and_end_poses(
            SquareWithOrthogonalDir::from_square_and_step(point2(0, 0), STEP_RIGHT),
            SquareWithOrthogonalDir::from_square_and_step(point2(0, 0), STEP_RIGHT),
        )
    }
}

pub trait RigidlyTransformable {
    fn apply_rigid_transform(&self, tf: RigidTransform) -> Self;
}

impl RigidlyTransformable for SquareWithOrthogonalDir {
    fn apply_rigid_transform(&self, tf: RigidTransform) -> Self {
        tf.transform_absolute_pose(*self)
    }
}

#[derive(Hash, Eq, PartialEq, Clone, Copy, Debug, CopyGetters)]
#[get_copy = "pub"]
pub struct Portal {
    entrance: SquareWithOrthogonalDir,
    exit: SquareWithOrthogonalDir,
}

impl Portal {
    pub fn new(entrance: SquareWithOrthogonalDir, exit: SquareWithOrthogonalDir) -> Self {
        //assert!( *entrance.direction() == *exit.direction() || *entrance.direction() == -*exit.direction() );
        assert_ne!(exit, entrance.stepped());
        Portal { entrance, exit }
    }
    pub fn get_transform(&self) -> RigidTransform {
        RigidTransform::from_start_and_end_poses(self.entrance, self.exit.stepped_back())
    }
    pub fn is_coherent_with(&self, other: &Portal) -> bool {
        self.get_transform() == other.get_transform()
    }
}

#[derive(Constructor, Default, Debug)]
pub struct PortalGeometry {
    portal_exits_by_entrance: HashMap<SquareWithOrthogonalDir, SquareWithOrthogonalDir>,
}

impl PortalGeometry {
    pub fn create_portal(
        &mut self,
        entrance_step: SquareWithOrthogonalDir,
        exit_step: SquareWithOrthogonalDir,
    ) {
        assert_false!(self.portal_exits_by_entrance.contains_key(&entrance_step));
        self.portal_exits_by_entrance
            .insert(entrance_step, exit_step);
    }
    pub fn create_double_sided_one_way_portal(
        &mut self,
        entrance_step: SquareWithOrthogonalDir,
        exit_step: SquareWithOrthogonalDir,
    ) {
        self.create_portal(entrance_step, exit_step);
        self.create_portal(
            entrance_step.stepped().turned_back(),
            exit_step.turned_back().stepped(),
        );
    }
    pub fn create_single_sided_two_way_portal(
        &mut self,
        entrance_step: SquareWithOrthogonalDir,
        exit_step: SquareWithOrthogonalDir,
    ) {
        self.create_portal(entrance_step, exit_step);
        self.create_portal(exit_step.reversed(), entrance_step.reversed());
    }
    pub fn create_double_sided_two_way_portal(
        &mut self,
        entrance_step: SquareWithOrthogonalDir,
        exit_step: SquareWithOrthogonalDir,
    ) {
        self.create_single_sided_two_way_portal(entrance_step, exit_step);
        self.create_single_sided_two_way_portal(
            entrance_step.stepped().reversed(),
            exit_step.reversed().stepped(),
        );
    }

    pub fn portal_aware_single_step(
        &self,
        start: SquareWithKingDir,
    ) -> Result<SquareWithKingDir, ()> {
        if let Ok(ortho_start) = SquareWithOrthogonalDir::try_from(start) {
            Ok(
                if let Some(&exit) = self.portal_exits_by_entrance.get(&ortho_start) {
                    exit.into()
                } else {
                    start.stepped()
                },
            )
        } else {
            let diagonal_step = start.direction();
            let x_step = WorldStep::new(diagonal_step.step().x, 0);
            let y_step = WorldStep::new(0, diagonal_step.step().y);
            let start_square = start.square();
            let x_dir_square = start_square + x_step;
            let y_dir_square = start_square + y_step;
            let first_x_entrance =
                SquareWithOrthogonalDir::from_square_and_step(start_square, x_step);
            let second_x_entrance =
                SquareWithOrthogonalDir::from_square_and_step(x_dir_square, y_step);
            let first_y_entrance =
                SquareWithOrthogonalDir::from_square_and_step(start_square, y_step);
            let second_y_entrance =
                SquareWithOrthogonalDir::from_square_and_step(y_dir_square, x_step);

            let maybe_first_x_portal: Option<Portal> =
                self.get_portal_by_entrance(first_x_entrance);
            let maybe_second_x_portal: Option<Portal> =
                self.get_portal_by_entrance(second_x_entrance);
            let maybe_first_y_portal: Option<Portal> =
                self.get_portal_by_entrance(first_y_entrance);
            let maybe_second_y_portal: Option<Portal> =
                self.get_portal_by_entrance(second_y_entrance);

            // miss the first portals (the directly orthogonal ones, unless there are two of them at once
            if let Some(first_x_portal) = maybe_first_x_portal && let Some(first_y_portal) = maybe_first_y_portal {
                if first_x_portal.is_coherent_with(&first_y_portal) {
                    // TODO: account for other second portals on the other side of the first ones.
                    let dest_square = first_x_portal.exit.square() + first_y_portal.exit.direction().step();
                    let dest_dir = first_x_portal.exit.direction().step() + first_y_portal.exit.direction().step();
                    Ok(SquareWithKingDir::from_square_and_step(dest_square, dest_dir))
                } else {
                    // Can't walk through a portal corner that goes to two different places
                    Err(())
                }
            } else {
                // ignore single adjacent portals, just go around them.
                // consider the second portals instead
                if let Some(second_x_portal) = maybe_second_x_portal && let Some(second_y_portal) = maybe_second_y_portal {
                    if second_x_portal.is_coherent_with(&second_y_portal) {
                        let dest_square = second_x_portal.exit.square();
                        let dest_dir = second_x_portal.exit.direction().step() + second_y_portal.exit.direction().step();
                        Ok(SquareWithKingDir::from_square_and_step(dest_square, dest_dir))
                    } else {
                        // Can't step through mismatched corner
                        Err(())
                    }
                } else if let Some(second_x_portal) = maybe_second_x_portal && maybe_second_y_portal.is_none() {
                    // if the second portal is only on the x side, the player steps in the y direction to go through it, but then they need to go left or right on the other side to get to the real destination.
                    // step through, but turn left or right?
                    let y_dir_to_x_dir_is_left = rotated_n_quarter_turns_counter_clockwise(y_step, 1) == x_step;

                    let turn_after_portal = if y_dir_to_x_dir_is_left { 1 } else { -1 };

                    let sideways_dir_after_portal = rotated_n_quarter_turns_counter_clockwise(second_x_portal.exit.direction().step(), turn_after_portal);

                    let dest_square = second_x_portal.exit.square();
                    let dest_dir = second_x_portal.exit.direction().step() + sideways_dir_after_portal;

                    Ok(SquareWithKingDir::from_square_and_step(dest_square, dest_dir))
                } else if let Some(second_y_portal) = maybe_second_y_portal && maybe_second_x_portal.is_none() {
                    let x_dir_to_y_dir_is_left = rotated_n_quarter_turns_counter_clockwise(x_step, 1) == y_step;

                    let turn_after_portal = if x_dir_to_y_dir_is_left { 1 } else { -1 };

                    let sideways_dir_after_portal = rotated_n_quarter_turns_counter_clockwise(second_y_portal.exit.direction().step(), turn_after_portal);

                    let dest_square = second_y_portal.exit.square();
                    let dest_dir = second_y_portal.exit.direction().step() + sideways_dir_after_portal;

                    Ok(SquareWithKingDir::from_square_and_step(dest_square, dest_dir))
                } else {
                    // it's neither
                    Ok(start.stepped())
                }
            }
        }
    }
    pub fn get_portal_by_entrance(&self, entrance: SquareWithOrthogonalDir) -> Option<Portal> {
        self.portal_exits_by_entrance
            .get(&entrance)
            .map(|&exit| Portal::new(entrance, exit))
    }

    pub fn multiple_portal_aware_steps(
        &self,
        start: SquareWithKingDir,
        num_steps: u32,
    ) -> Result<SquareWithKingDir, ()> {
        (0..num_steps).fold(Ok(start), |current_square_and_dir, _| {
            self.portal_aware_single_step(current_square_and_dir?)
        })
    }

    pub fn square_has_portal_entrance(&self, square: WorldSquare) -> bool {
        self.portal_exits_by_entrance
            .iter()
            .any(|(entrance, exit)| entrance.square() == square)
    }

    pub fn portals_entering_from_square(&self, square: WorldSquare) -> Vec<Portal> {
        self.portal_exits_by_entrance
            .iter()
            .filter(
                |(&entrance, &exit): &(&SquareWithOrthogonalDir, &SquareWithOrthogonalDir)| {
                    entrance.square() == square
                },
            )
            .map(
                |(&entrance, &exit): (&SquareWithOrthogonalDir, &SquareWithOrthogonalDir)| {
                    Portal::new(entrance, exit)
                },
            )
            .collect()
    }
    pub fn ray_to_naive_line_segments(
        &self,
        mut start: WorldPoint,
        mut angle: Angle<f32>,
        mut range: f32,
    ) -> Vec<WorldLine> {
        assert!(range > 0.0);
        let mut naive_line_segments = vec![];
        let STEP_BACK_DISTANCE = 0.001;
        while range > 0.0 {
            if let Some((portal_entrance, intersection_point)) =
                self.first_portal_entrance_hit_by_ray(start, angle, range)
            {
                // Step back so the line doesn't end exactly on the border between squares
                let stepped_back_intersection_point =
                    naive_ray_endpoint(intersection_point, angle, -STEP_BACK_DISTANCE);

                let new_line = WorldLine::new(start, stepped_back_intersection_point);
                naive_line_segments.push(new_line);
                range -= new_line.length();

                let portal = self.get_portal_by_entrance(portal_entrance).unwrap();

                start = intersection_point;
                (start, angle) = portal.get_transform().transform_ray(start, angle);
                // Step forward so the line doesn't start exactly on the border between squares
                let stepped_forward_start = naive_ray_endpoint(start, angle, STEP_BACK_DISTANCE);
                start = stepped_forward_start;
            } else {
                naive_line_segments.push(WorldLine::from_ray(start, angle, range));
                range = 0.0;
            }
        }
        naive_line_segments
    }
    fn first_portal_entrance_hit_by_ray(
        &self,
        start: WorldPoint,
        angle: Angle<f32>,
        range: f32,
    ) -> Option<(SquareWithOrthogonalDir, WorldPoint)> {
        let all_entrances: HashSet<SquareWithOrthogonalDir> =
            self.portal_exits_by_entrance.keys().cloned().collect();
        first_inside_square_face_hit_by_ray(start, angle, range, &all_entrances)
    }
}

#[cfg(test)]
mod tests {
    use crate::utility::{
        assert_about_eq_2d, STEP_DOWN, STEP_LEFT, STEP_RIGHT, STEP_UP, STEP_UP_RIGHT,
    };
    use ntest::assert_about_eq;

    use super::*;

    #[test]
    fn test_slide_rotation_transform() {
        let transform = RigidTransform::from_start_and_end_poses(
            SquareWithOrthogonalDir::from_square_and_step(point2(1, 2), STEP_UP),
            SquareWithOrthogonalDir::from_square_and_step(point2(5, 5), STEP_RIGHT),
        );
        let pose1 = SquareWithOrthogonalDir::from_square_and_step(point2(3, 3), STEP_RIGHT);
        let pose2 = SquareWithOrthogonalDir::from_square_and_step(point2(6, 3), STEP_DOWN);
        assert_eq!(transform.transform_absolute_pose(pose1), pose2);
    }
    #[test]
    fn test_ray_through_portal() {
        let mut portal_geometry = PortalGeometry::default();
        portal_geometry.create_portal(
            (WorldSquare::new(3, 3), STEP_UP).into(),
            (WorldSquare::new(6, 5), STEP_RIGHT).into(),
        );
        let ray_segments =
            portal_geometry.ray_to_naive_line_segments(point2(3.0, 2.0), Angle::degrees(90.0), 5.0);
        let correct_points = vec![
            point2(3.0, 2.0),
            point2(3.0, 3.5),
            point2(5.5, 5.0),
            point2(9.0, 5.0),
        ];
        let actual_points = vec![
            ray_segments[0].p1,
            ray_segments[0].p2,
            ray_segments[1].p1,
            ray_segments[1].p2,
        ];
        for i in 0..correct_points.len() {
            // Note that the line segments don't end exactly on the portal, they are stepped back slightly
            assert!(
                (correct_points[i] - actual_points[i]).length() < 0.01,
                "correct: {:?}, actual {:?}",
                correct_points[i],
                actual_points[i]
            );
        }
    }
    #[test]
    fn test_transform_ray__simple_translation() {
        let tf = RigidTransform::from_start_and_end_poses(
            (point2(3, 4), STEP_RIGHT).into(),
            (point2(4, 4), STEP_RIGHT).into(),
        );
        let (new_start, new_direction) = tf.transform_ray(point2(3.0, 4.0), Angle::degrees(90.0));
        assert_about_eq_2d(new_start, point2(4.0, 4.0));
        assert_about_eq!(new_direction.to_degrees(), 90.0);
    }
    #[test]
    fn test_transform_ray__simple_rotation() {
        let tf = RigidTransform::from_start_and_end_poses(
            (point2(3, 4), STEP_RIGHT).into(),
            (point2(3, 4), STEP_DOWN).into(),
        );
        let (new_start, new_direction) = tf.transform_ray(point2(3.0, 4.0), Angle::degrees(45.0));
        assert_about_eq_2d(new_start, point2(3.0, 4.0));
        assert_about_eq!(new_direction.to_degrees(), -45.0);
    }
    #[test]
    fn test_transform_ray__move_and_rotate() {
        let tf = RigidTransform::from_start_and_end_poses(
            (point2(5, 5), STEP_LEFT).into(),
            (point2(-10, 20), STEP_UP).into(),
        );
        let (new_start, new_direction) = tf.transform_ray(point2(5.0, 4.3), Angle::degrees(170.0));
        assert_about_eq_2d(new_start, point2(-10.7, 20.0));
        assert_about_eq!(new_direction.to_degrees(), 80.0, 0.001);
    }
    #[test]
    fn test_ray_to_naive_line_segments__no_counting_behind_portal() {
        let mut portal_geometry = PortalGeometry::default();
        portal_geometry.create_portal(
            (WorldSquare::new(3, 3), STEP_UP).into(),
            (WorldSquare::new(6, 5), STEP_RIGHT).into(),
        );
        let segments =
            portal_geometry.ray_to_naive_line_segments(point2(3.0, 3.0), Angle::degrees(90.0), 1.0);
        let squares = segments
            .iter()
            .flat_map(WorldLine::touched_squares)
            .collect_vec();
        dbg!("asdf", &squares);
        assert_eq!(squares.len(), 2);
    }
}
