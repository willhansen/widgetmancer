use std::collections::{HashMap, HashSet};
use std::ops::Add;

use derive_more::Constructor;
use derive_more::Neg;
use euclid::Angle;
use getset::CopyGetters;
use itertools::Itertools;
use ntest::assert_false;

use crate::utility::*;

#[derive(Hash, Eq, PartialEq, Clone, Copy, Debug, CopyGetters)]
#[get_copy = "pub"]
pub struct Portal {
    entrance: WorldSquareWithOrthogonalDir,
    exit: WorldSquareWithOrthogonalDir,
}

impl Portal {
    pub fn new(entrance: WorldSquareWithOrthogonalDir, exit: WorldSquareWithOrthogonalDir) -> Self {
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

#[derive(Default, Debug)]
pub struct PortalGeometry {
    portal_exits_by_entrance: HashMap<WorldSquareWithOrthogonalDir, WorldSquareWithOrthogonalDir>,
}

impl PortalGeometry {
    pub fn new() -> Self {
        PortalGeometry {
            portal_exits_by_entrance: Default::default(),
        }
    }
    pub fn with_portal(
        mut self,
        entrance_step: impl Into<WorldSquareWithOrthogonalDir>,
        exit_step: impl Into<WorldSquareWithOrthogonalDir>,
    ) -> Self {
        self.create_portal(entrance_step, exit_step);
        self
    }
    pub fn new_with_portal(
        entrance_step: impl Into<WorldSquareWithOrthogonalDir>,
        exit_step: impl Into<WorldSquareWithOrthogonalDir>,
    ) -> Self {
        Self::new().with_portal(entrance_step, exit_step)
    }
    pub fn create_portal(
        &mut self,
        entrance_step: impl Into<WorldSquareWithOrthogonalDir>,
        exit_step: impl Into<WorldSquareWithOrthogonalDir>,
    ) {
        let entrance_step = entrance_step.into();
        let exit_step = exit_step.into();
        assert_false!(self.portal_exits_by_entrance.contains_key(&entrance_step));
        self.portal_exits_by_entrance
            .insert(entrance_step, exit_step);
    }
    pub fn create_double_sided_one_way_portal(
        &mut self,
        entrance_step: WorldSquareWithOrthogonalDir,
        exit_step: WorldSquareWithOrthogonalDir,
    ) {
        self.create_portal(entrance_step, exit_step);
        self.create_portal(
            entrance_step.stepped().turned_back(),
            exit_step.turned_back().stepped(),
        );
    }
    pub fn create_single_sided_two_way_portal(
        &mut self,
        entrance_step: WorldSquareWithOrthogonalDir,
        exit_step: WorldSquareWithOrthogonalDir,
    ) {
        self.create_portal(entrance_step, exit_step);
        self.create_portal(exit_step.reversed(), entrance_step.reversed());
    }
    pub fn create_double_sided_two_way_portal(
        &mut self,
        entrance_step: WorldSquareWithOrthogonalDir,
        exit_step: WorldSquareWithOrthogonalDir,
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
        if let Ok(ortho_start) = WorldSquareWithOrthogonalDir::try_from(start) {
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
                WorldSquareWithOrthogonalDir::from_square_and_step(start_square, x_step);
            let second_x_entrance =
                WorldSquareWithOrthogonalDir::from_square_and_step(x_dir_square, y_step);
            let first_y_entrance =
                WorldSquareWithOrthogonalDir::from_square_and_step(start_square, y_step);
            let second_y_entrance =
                WorldSquareWithOrthogonalDir::from_square_and_step(y_dir_square, x_step);

            let maybe_first_x_portal: Option<Portal> =
                self.get_portal_by_entrance(first_x_entrance);
            let maybe_second_x_portal: Option<Portal> =
                self.get_portal_by_entrance(second_x_entrance);
            let maybe_first_y_portal: Option<Portal> =
                self.get_portal_by_entrance(first_y_entrance);
            let maybe_second_y_portal: Option<Portal> =
                self.get_portal_by_entrance(second_y_entrance);

            // miss the first portals (the directly orthogonal ones, unless there are two of them at once
            if maybe_first_x_portal.is_some() && maybe_first_y_portal.is_some() {
                let first_x_portal = maybe_first_x_portal.unwrap();
                let first_y_portal = maybe_first_y_portal.unwrap();
                if first_x_portal.is_coherent_with(&first_y_portal) {
                    // TODO: account for other second portals on the other side of the first ones.
                    let dest_square = first_x_portal.exit.square()
                        + first_y_portal.exit.direction().to_step::<WorldStep>();
                    let dest_dir = first_x_portal.exit.direction().to_step::<WorldSquare>()
                        + first_y_portal.exit.direction().to_step::<WorldStep>();
                    Ok(SquareWithKingDir::from_square_and_step(
                        dest_square,
                        dest_dir,
                    ))
                } else {
                    // Can't walk through a portal corner that goes to two different places
                    Err(())
                }
            } else {
                // ignore single adjacent portals, just go around them.
                // consider the second portals instead
                if maybe_second_x_portal.is_some() && maybe_second_y_portal.is_some() {
                    let second_x_portal = maybe_second_x_portal.unwrap();
                    let second_y_portal = maybe_second_y_portal.unwrap();
                    if second_x_portal.is_coherent_with(&second_y_portal) {
                        let dest_square = second_x_portal.exit.square();
                        let dest_dir = second_x_portal.exit.direction().to_step::<WorldSquare>()
                            + second_y_portal.exit.direction().to_step::<WorldSquare>();
                        Ok(SquareWithKingDir::from_square_and_step(
                            dest_square,
                            dest_dir,
                        ))
                    } else {
                        // Can't step through mismatched corner
                        Err(())
                    }
                } else if maybe_second_x_portal.is_some() && maybe_second_y_portal.is_none() {
                    let second_x_portal = maybe_second_x_portal.unwrap();
                    // if the second portal is only on the x side, the player steps in the y direction to go through it, but then they need to go left or right on the other side to get to the real destination.
                    // step through, but turn left or right?
                    let y_dir_to_x_dir_is_left = y_step.quarter_rotated_ccw(1) == x_step;

                    let turn_after_portal = if y_dir_to_x_dir_is_left { 1 } else { -1 };

                    let sideways_dir_after_portal: WorldStep = second_x_portal
                        .exit
                        .direction()
                        .quarter_rotated_ccw(turn_after_portal)
                        .to_step();

                    let dest_square = second_x_portal.exit.square();
                    let dest_dir = second_x_portal.exit.direction().to_step::<WorldStep>()
                        + sideways_dir_after_portal;

                    Ok(SquareWithKingDir::from_square_and_step(
                        dest_square,
                        dest_dir,
                    ))
                } else if maybe_second_y_portal.is_some() && maybe_second_x_portal.is_none() {
                    let second_y_portal = maybe_second_y_portal.unwrap();
                    let x_dir_to_y_dir_is_left = x_step.quarter_rotated_ccw(1) == y_step;

                    let turn_after_portal = if x_dir_to_y_dir_is_left { 1 } else { -1 };

                    let sideways_dir_after_portal = second_y_portal
                        .exit
                        .direction()
                        .quarter_rotated_ccw(turn_after_portal)
                        .to_step::<WorldStep>();

                    let dest_square = second_y_portal.exit.square();
                    let dest_dir = second_y_portal.exit.direction().to_step::<WorldStep>()
                        + sideways_dir_after_portal;

                    Ok(SquareWithKingDir::from_square_and_step(
                        dest_square,
                        dest_dir,
                    ))
                } else {
                    // it's neither
                    Ok(start.stepped())
                }
            }
        }
    }
    pub fn get_portal_by_entrance(&self, entrance: WorldSquareWithOrthogonalDir) -> Option<Portal> {
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
                |(&entrance, &exit): &(
                    &WorldSquareWithOrthogonalDir,
                    &WorldSquareWithOrthogonalDir,
                )| { entrance.square() == square },
            )
            .map(
                |(&entrance, &exit): (
                    &WorldSquareWithOrthogonalDir,
                    &WorldSquareWithOrthogonalDir,
                )| { Portal::new(entrance, exit) },
            )
            .collect()
    }
    pub fn ray_to_naive_line_segments(
        &self,
        start: impl Into<WorldPoint>,
        mut angle: Angle<f32>,
        mut range: f32,
    ) -> Vec<TwoDifferentWorldPoints> {
        let mut start = start.into();
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

                let new_line = TwoDifferentWorldPoints::new_from_two_points(
                    start,
                    stepped_back_intersection_point,
                );
                naive_line_segments.push(new_line);
                range -= new_line.length();

                let portal = self.get_portal_by_entrance(portal_entrance).unwrap();

                start = intersection_point;
                (start, angle) = portal.get_transform().transform_ray(start, angle);
                // Step forward so the line doesn't start exactly on the border between squares
                let stepped_forward_start = naive_ray_endpoint(start, angle, STEP_BACK_DISTANCE);
                start = stepped_forward_start;
            } else {
                naive_line_segments.push(TwoDifferentWorldPoints::from_ray(start, angle, range));
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
    ) -> Option<(WorldSquareWithOrthogonalDir, WorldPoint)> {
        let all_entrances: HashSet<WorldSquareWithOrthogonalDir> =
            self.portal_exits_by_entrance.keys().cloned().collect();
        first_inside_square_face_hit_by_ray(start, angle, range, &all_entrances)
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        point2,
        utility::{
            assert_about_eq_2d, RigidlyTransformable, STEP_DOWN, STEP_LEFT, STEP_RIGHT, STEP_UP,
            STEP_UP_RIGHT,
        },
        DirectedLineTrait,
    };
    use ntest::{assert_about_eq, timeout};

    use super::*;

    #[test]
    fn test_slide_rotation_transform() {
        let transform = RigidTransform::from_start_and_end_poses(
            WorldSquareWithOrthogonalDir::from_square_and_step((1, 2), STEP_UP),
            WorldSquareWithOrthogonalDir::from_square_and_step((5, 5), STEP_RIGHT),
        );
        let pose1 = WorldSquareWithOrthogonalDir::from_square_and_step((3, 3), STEP_RIGHT);
        let pose2 = WorldSquareWithOrthogonalDir::from_square_and_step((6, 3), STEP_DOWN);
        assert_eq!(pose1.apply_rigid_transform(transform), pose2);
    }
    #[test]
    fn test_ray_through_portal() {
        let mut portal_geometry = PortalGeometry::default();
        portal_geometry.create_portal((3, 3, UP), (6, 5, RIGHT));
        let ray_segments =
            portal_geometry.ray_to_naive_line_segments(point2(3.0, 2.0), Angle::degrees(90.0), 5.0);
        let correct_points = vec![
            point2(3.0, 2.0),
            point2(3.0, 3.5),
            point2(5.5, 5.0),
            point2(9.0, 5.0),
        ];
        let actual_points = vec![
            ray_segments[0].p1(),
            ray_segments[0].p2(),
            ray_segments[1].p1(),
            ray_segments[1].p2(),
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
        let tf =
            RigidTransform::from_start_and_end_poses(((3, 4), STEP_RIGHT), ((4, 4), STEP_RIGHT));
        let (new_start, new_direction) = tf.transform_ray(point2(3.0, 4.0), Angle::degrees(90.0));
        assert_about_eq_2d(new_start, point2(4.0, 4.0));
        assert_about_eq!(new_direction.to_degrees(), 90.0);
    }
    #[test]
    fn test_transform_ray__simple_rotation() {
        let tf =
            RigidTransform::from_start_and_end_poses(((3, 4), STEP_RIGHT), ((3, 4), STEP_DOWN));
        let (new_start, new_direction) = tf.transform_ray(point2(3.0, 4.0), Angle::degrees(45.0));
        assert_about_eq_2d(new_start, point2(3.0, 4.0));
        assert_about_eq!(new_direction.to_degrees(), -45.0, 1e-5);
    }
    #[test]
    fn test_transform_ray__move_and_rotate() {
        let tf =
            RigidTransform::from_start_and_end_poses(((5, 5), STEP_LEFT), ((-10, 20), STEP_UP));
        let (new_start, new_direction) = tf.transform_ray(point2(5.0, 4.3), Angle::degrees(170.0));
        assert_about_eq_2d(new_start, point2(-10.7, 20.0));
        assert_about_eq!(new_direction.to_degrees(), 80.0, 0.001);
    }
    #[test]
    fn test_ray_to_naive_line_segments__no_counting_behind_portal() {
        let mut portal_geometry = PortalGeometry::default();
        portal_geometry.create_portal((3, 3, UP), (6, 5, RIGHT));
        let segments =
            portal_geometry.ray_to_naive_line_segments(point2(3.0, 3.0), Angle::degrees(90.0), 1.0);
        let squares = segments
            .iter()
            .flat_map(TwoDifferentWorldPoints::touched_squares)
            .collect_vec();
        assert_eq!(squares.len(), 2);
    }
}
