use crate::utility::coordinate_frame_conversions::WorldSquare;
use crate::utility::SquareWithDir;
use derive_getters::Getters;
use derive_more::Constructor;
use ntest::assert_false;
use std::collections::HashMap;

#[derive(Hash, Eq, PartialEq, Constructor, Getters, Clone, Copy, Debug)]
pub struct Portal {
    entrance: SquareWithDir,
    exit: SquareWithDir,
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
