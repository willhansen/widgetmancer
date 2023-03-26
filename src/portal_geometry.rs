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
    is_orthogonal, revolve_square, rotated_n_quarter_turns_counter_clockwise, Octant,
    QuarterTurnsAnticlockwise, SquareWithAdjacentDir, SquareWithOrthogonalDir,
    StepWithQuarterRotations, STEP_RIGHT, STEP_ZERO,
};

// This is just an affine transform for now.  Colors/blur later
#[derive(Hash, Neg, Clone, Copy, Debug)]
pub struct SlideRotation {
    start_pose: SquareWithOrthogonalDir,
    end_pose: SquareWithOrthogonalDir,
}

impl SlideRotation {
    pub fn from_start_and_end_poses(
        start: SquareWithOrthogonalDir,
        end: SquareWithOrthogonalDir,
    ) -> Self {
        SlideRotation {
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

        let end_direction = self.rotation().rotate_vector(pose.direction_vector());

        SquareWithOrthogonalDir::from_square_and_dir(end_square, end_direction)
    }
    pub fn transform_octant(&self, octant: Octant) -> Octant {
        octant.with_n_quarter_turns_anticlockwise(self.rotation())
    }
    pub fn transform_arc(&self, arc: AngleInterval) -> AngleInterval {
        arc.rotated(self.rotation())
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

impl PartialEq for SlideRotation {
    fn eq(&self, other: &Self) -> bool {
        self.transform_pose(other.start_pose) == other.end_pose
    }
}

impl Eq for SlideRotation {}

impl Default for SlideRotation {
    fn default() -> Self {
        SlideRotation::from_start_and_end_poses(
            SquareWithOrthogonalDir::from_square_and_dir(point2(0, 0), STEP_RIGHT),
            SquareWithOrthogonalDir::from_square_and_dir(point2(0, 0), STEP_RIGHT),
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
    pub fn get_transform(&self) -> SlideRotation {
        SlideRotation::from_start_and_end_poses(self.entrance, self.exit.stepped_back())
    }
}

#[derive(Constructor, Default)]
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

    pub fn portal_aware_single_step(&self, start: SquareWithAdjacentDir) -> SquareWithAdjacentDir {
        if let Ok(ortho_start) = SquareWithOrthogonalDir::try_from(start) {
            if let Some(&exit) = self.portal_exits_by_entrance.get(&ortho_start) {
                exit.into()
            } else {
                start.stepped()
            }
        } else {
            start.stepped()
        }
    }
    pub fn multiple_portal_aware_steps(
        &self,
        start: SquareWithAdjacentDir,
        num_steps: u32,
    ) -> SquareWithAdjacentDir {
        (0..num_steps).fold(start, |current_square_and_dir, _| {
            self.portal_aware_single_step(current_square_and_dir)
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
        let transform = SlideRotation::from_start_and_end_poses(
            SquareWithOrthogonalDir::from_square_and_dir(point2(1, 2), STEP_UP),
            SquareWithOrthogonalDir::from_square_and_dir(point2(5, 5), STEP_RIGHT),
        );
        let pose1 = SquareWithOrthogonalDir::from_square_and_dir(point2(3, 3), STEP_RIGHT);
        let pose2 = SquareWithOrthogonalDir::from_square_and_dir(point2(6, 3), STEP_DOWN);
        assert_eq!(transform.transform_pose(pose1), pose2);
    }
}
