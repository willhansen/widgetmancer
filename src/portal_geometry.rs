use std::collections::HashMap;
use std::ops::Add;

use derive_more::Constructor;
use derive_more::Neg;
use euclid::point2;
use getset::CopyGetters;
use itertools::Itertools;
use ntest::assert_false;

use crate::utility::angle_interval::AngleInterval;
use crate::utility::coordinate_frame_conversions::{StepSet, WorldSquare, WorldStep};
use crate::utility::{
    is_orthogonal, ith_projection_of_step, revolve_square,
    rotated_n_quarter_turns_counter_clockwise, Octant, QuarterTurnsAnticlockwise,
    SquareWithKingDir, SquareWithOrthogonalDir, StepWithQuarterRotations, STEP_RIGHT, STEP_ZERO,
};

#[derive(Hash, Neg, Clone, Copy, Debug)]
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
    pub fn transform_pose(&self, pose: SquareWithOrthogonalDir) -> SquareWithOrthogonalDir {
        let end_square = revolve_square(pose.square(), self.start_pose.square(), self.rotation())
            + self.translation();

        let end_direction = self.rotation().rotate_vector(pose.direction().step());

        SquareWithOrthogonalDir::from_square_and_step(end_square, end_direction)
    }
    pub fn transform_octant(&self, octant: Octant) -> Octant {
        octant.with_n_quarter_turns_anticlockwise(self.rotation())
    }
    pub fn transform_arc(&self, arc: AngleInterval) -> AngleInterval {
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
}

impl PartialEq for RigidTransform {
    fn eq(&self, other: &Self) -> bool {
        self.transform_pose(other.start_pose) == other.end_pose
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
}

#[cfg(test)]
mod tests {
    use crate::utility::{STEP_DOWN, STEP_RIGHT, STEP_UP, STEP_UP_RIGHT};

    use super::*;

    // NO TESTS HERE YET
    #[test]
    fn test_slide_rotation_transform() {
        let transform = RigidTransform::from_start_and_end_poses(
            SquareWithOrthogonalDir::from_square_and_step(point2(1, 2), STEP_UP),
            SquareWithOrthogonalDir::from_square_and_step(point2(5, 5), STEP_RIGHT),
        );
        let pose1 = SquareWithOrthogonalDir::from_square_and_step(point2(3, 3), STEP_RIGHT);
        let pose2 = SquareWithOrthogonalDir::from_square_and_step(point2(6, 3), STEP_DOWN);
        assert_eq!(transform.transform_pose(pose1), pose2);
    }
}
