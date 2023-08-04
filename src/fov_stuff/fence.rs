use std::collections::HashSet;

use crate::utility::{RelativeSquareWithOrthogonalDir, angle_interval::AngleInterval, RigidlyTransformable, coordinate_frame_conversions::WorldStep};

#[derive(Debug, Clone, PartialEq, Default)]
pub struct RelativeFenceFullyVisibleFromOrigin {
    edges: Vec<RelativeSquareWithOrthogonalDir>
}

impl RelativeFenceFullyVisibleFromOrigin {
    pub fn from_relative_edges(edges: Vec<impl Into<RelativeSquareWithOrthogonalDir>>) -> Self {

        todo!();

    }
    fn add_edge(&mut self, edge: RelativeSquareWithOrthogonalDir){
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

    use super::*;

    #[test]
    #[timeout(1000)]
    fn test_make_a_fence_from_square_faces() {
        todo!()
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
