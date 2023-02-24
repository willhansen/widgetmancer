use std::collections::HashMap;
use std::ops::Add;

use derive_getters::Getters;
use derive_more::Constructor;
use ntest::assert_false;

use crate::utility::coordinate_frame_conversions::{WorldSquare, WorldStep};
use crate::utility::{quarter_turns_counter_clockwise, SquareWithDir, STEP_ZERO};

#[derive(Hash, Eq, PartialEq, Constructor, Getters, Clone, Copy, Debug)]
pub struct ViewTransform {
    translation: WorldStep,
    rotate_180: bool,
}

impl ViewTransform {
    pub fn transform(&self, pose: SquareWithDir) -> SquareWithDir {
        SquareWithDir::new(
            *pose.square() + self.translation,
            *pose.direction() * if self.rotate_180 { -1 } else { 1 },
        )
    }
}

impl Default for ViewTransform {
    fn default() -> Self {
        ViewTransform::new(STEP_ZERO, false)
    }
}

impl Add for ViewTransform {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        ViewTransform::new(
            self.translation + rhs.translation,
            self.rotate_180 ^ rhs.rotate_180,
        )
    }
}

#[derive(Hash, Eq, PartialEq, Getters, Clone, Copy, Debug)]
pub struct Portal {
    entrance: SquareWithDir,
    exit: SquareWithDir,
}

impl Portal {
    pub fn new(entrance: SquareWithDir, exit: SquareWithDir) -> Self {
        assert!(
            *entrance.direction() == *exit.direction()
                || *entrance.direction() == -*exit.direction()
        );
        assert_ne!(entrance, exit);
        Portal { entrance, exit }
    }
    pub fn get_transform(&self) -> ViewTransform {
        ViewTransform::new(
            *self.exit().square() - *self.entrance.square(),
            *self.entrance().direction() == -*self.exit().direction(),
        )
    }
}

#[derive(Constructor, Default)]
pub struct PortalGeometry {
    portal_exits_by_entrance: HashMap<SquareWithDir, SquareWithDir>,
}

impl PortalGeometry {
    pub fn create_portal(&mut self, entrance_step: SquareWithDir, exit_step: SquareWithDir) {
        assert!(entrance_step.is_square_face());
        assert!(exit_step.is_square_face());
        assert_false!(self.portal_exits_by_entrance.contains_key(&entrance_step));
        self.portal_exits_by_entrance
            .insert(entrance_step, exit_step);
    }

    pub fn portal_aware_single_step(&self, start: SquareWithDir) -> SquareWithDir {
        if let Some(&exit) = self.portal_exits_by_entrance.get(&start) {
            exit
        } else {
            start.stepped()
        }
    }
    pub fn multiple_portal_aware_steps(
        &self,
        start: SquareWithDir,
        num_steps: u32,
    ) -> SquareWithDir {
        (0..num_steps).fold(start, |current_square_and_dir, _| {
            self.portal_aware_single_step(current_square_and_dir)
        })
    }

    pub fn square_has_portal_entrance(&self, square: WorldSquare) -> bool {
        self.portal_exits_by_entrance
            .iter()
            .any(|(entrance, exit)| *entrance.square() == square)
    }

    pub fn portals_entering_from_square(&self, square: WorldSquare) -> Vec<Portal> {
        self.portal_exits_by_entrance
            .iter()
            .filter(|(&entrance, &exit): &(&SquareWithDir, &SquareWithDir)| {
                *entrance.square() == square
            })
            .map(|(&entrance, &exit): (&SquareWithDir, &SquareWithDir)| Portal::new(entrance, exit))
            .collect()
    }
}
