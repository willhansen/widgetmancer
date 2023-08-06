use std::collections::HashSet;

use crate::utility::{RelativeSquareWithOrthogonalDir, angle_interval::AngleInterval, RigidlyTransformable, coordinate_frame_conversions::WorldStep};

#[derive(Debug, Clone, PartialEq, Default)]
pub struct RelativeFenceFullyVisibleFromOrigin {
    edges: Vec<RelativeSquareWithOrthogonalDir>
}

pub type Fence = RelativeFenceFullyVisibleFromOrigin;

type Face = RelativeSquareWithOrthogonalDir;

impl RelativeFenceFullyVisibleFromOrigin {
    pub fn from_relative_edges(edges: Vec<impl Into<RelativeSquareWithOrthogonalDir>>) -> Self {
        

        todo!();

    }
    fn add_edge(&mut self, edge: Face){
        if self.edges.is_empty() {
            self.edges.push(edge);
            return;
        } 
        if self.overlaps_edge(edge) {
            panic!("Tried to add overlapping edge to fence: {}", edge)
        }

        if self.can_connect_to_end(edge) {
            self.edges.push(edge)
        } else if self.can_connect_to_start(edge) {
            self.edges.insert(0, edge)
        }
        else {
            panic!("Tried to add edge that can't connect to either end: {}", edge)
        }
        
    }
    fn overlaps_edge(&self, edge: RelativeSquareWithOrthogonalDir) {
        todo!()
    }
    pub fn from_unsorted_relative_edges(edges: HashSet<impl Into<RelativeSquareWithOrthogonalDir>>) -> Self {

        todo!();
    }

    pub fn spanned_angle_from_origin(&self) -> AngleInterval {
        todo!()
    }

    pub fn same_side_of_fence(&self, rel_square_a: WorldStep, rel_square_b: WorldStep) -> bool {
        todo!()
    }
}
impl RigidlyTransformable for RelativeFenceFullyVisibleFromOrigin {
    fn apply_rigid_transform(&self, tf: crate::utility::RigidTransform) -> Self {
        todo!()
    }
}

#[cfg(test)]
mod tests {
    use ntest::timeout;

    use crate::utility::{STEP_UP, STEP_LEFT};

    use super::*;

    #[test]
    #[timeout(1000)]
    fn test_make_a_fence_from_square_faces() {
        Fence::from_relative_edges(vec![((5,5), STEP_UP), ((6,4), STEP_LEFT)]);
    }

    #[test]
    #[timeout(1000)]
    fn test_fail_to_make_a_fence__disconnected() {
        todo!()
    }

    #[test]
    #[timeout(1000)]
    fn test_fail_to_make_a_fence__duplicate_square_edge() {
        todo!()
    }
    #[test]
    #[timeout(1000)]
    fn test_fail_to_make_a_fence__duplicate_edge_from_other_square() {
        todo!()
    }
    #[test]
    #[timeout(1000)]
    fn test_fail_to_make_a_fence__forking_path() {
        todo!()
    }
    
    #[test]
    #[timeout(1000)]
    fn test_fence_fully_visible_from_origin() {
        todo!()
    }
    #[test]
    #[timeout(1000)]
    fn test_fence_not_fully_visible_from_origin() {
        todo!()
    }
}
