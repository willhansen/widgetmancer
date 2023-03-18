use std::collections::HashMap;
use std::ops::Add;

use derive_getters::Getters;
use derive_more::Constructor;
use itertools::Itertools;
use ntest::assert_false;

use crate::utility::coordinate_frame_conversions::{WorldSquare, WorldStep};
use crate::utility::{
    is_orthogonal, rotated_n_quarter_turns_counter_clockwise, Octant, QuarterTurnsAnticlockwise,
    SquareWithAdjacentDir, SquareWithOrthogonalDir, StepWithQuarterRotations, STEP_ZERO,
};

#[derive(Hash, Eq, PartialEq, Clone, Copy, Debug)]
pub struct ViewTransform(pub StepWithQuarterRotations);

impl ViewTransform {
    pub fn new(
        translation: WorldStep,
        quarter_rotations_anticlockwise: QuarterTurnsAnticlockwise,
    ) -> Self {
        ViewTransform(StepWithQuarterRotations::new(
            translation,
            quarter_rotations_anticlockwise,
        ))
    }
    pub fn transform_pose(&self, pose: SquareWithOrthogonalDir) -> SquareWithOrthogonalDir {
        SquareWithOrthogonalDir::from_square_and_turns(
            pose.square() + *self.0.step(),
            pose.direction_in_quarter_turns() + *self.0.rotation(),
        )
    }
    pub fn transform_octant(&self, octant: Octant) -> Octant {
        octant.with_n_quarter_turns_anticlockwise(*self.0.rotation())
    }
    pub fn from_start_and_end_poses(
        start: SquareWithOrthogonalDir,
        end: SquareWithOrthogonalDir,
    ) -> Self {
        Self(StepWithQuarterRotations::from_direction_squares(start, end))
    }
}

impl Add for ViewTransform {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        ViewTransform(self.0 + rhs.0)
    }
}

impl Default for ViewTransform {
    fn default() -> Self {
        ViewTransform(Default::default())
    }
}

#[derive(Hash, Eq, PartialEq, Clone, Copy, Debug)]
pub struct Portal {
    entrance: SquareWithOrthogonalDir,
    exit: SquareWithOrthogonalDir,
}

impl Portal {
    pub fn entrance(&self) -> SquareWithOrthogonalDir {
        self.entrance
    }
    pub fn exit(&self) -> SquareWithOrthogonalDir {
        self.exit
    }
    pub fn new(entrance: SquareWithOrthogonalDir, exit: SquareWithOrthogonalDir) -> Self {
        //assert!( *entrance.direction() == *exit.direction() || *entrance.direction() == -*exit.direction() );
        assert_ne!(exit, entrance.stepped());
        Portal { entrance, exit }
    }
    pub fn get_transform(&self) -> ViewTransform {
        ViewTransform::from_start_and_end_poses(self.entrance, self.exit)
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
