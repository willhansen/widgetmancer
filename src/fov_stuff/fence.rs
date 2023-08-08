use std::collections::HashSet;

use crate::utility::{
    angle_interval::AngleInterval,
    coordinate_frame_conversions::{WorldMove, WorldPoint, WorldStep},
    RelativeSquareWithOrthogonalDir, RigidlyTransformable,
};

#[derive(Debug, Clone, PartialEq, Default)]
pub struct RelativeFenceFullyVisibleFromOrigin {
    edges: Vec<RelativeSquareWithOrthogonalDir>,
}

pub type Fence = RelativeFenceFullyVisibleFromOrigin;

type Edge = RelativeSquareWithOrthogonalDir;

impl RelativeFenceFullyVisibleFromOrigin {
    pub fn edges(&self) -> &Vec<Edge> {
        &self.edges
    }
    pub fn from_relative_edges(edges: Vec<impl Into<RelativeSquareWithOrthogonalDir>>) -> Self {
        let mut fence = Self::default();
        edges.into_iter().for_each(|edge| fence.add_edge(edge));
        fence
    }
    pub fn from_one_edge(edge: impl Into<RelativeSquareWithOrthogonalDir>) -> Self {
        Self::from_relative_edges(vec![edge])
    }
    fn add_edge(&mut self, can_be_edge: impl Into<Edge>) {
        let edge = can_be_edge.into();
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
        } else {
            panic!(
                "Tried to add edge that can't connect to either end: {}",
                edge
            )
        }
    }
    fn overlaps_edge(&self, edge: Edge) -> bool {
        self.edges
            .iter()
            .any(|own_edge| own_edge.faces_overlap(edge))
    }
    fn can_connect_to_end(&self, edge: Edge) -> bool {
        !self.overlaps_edge(edge) && edge.face_end_point_approx_touches_point(self.end_point())
    }
    fn can_connect_to_start(&self, edge: Edge) -> bool {
        !self.overlaps_edge(edge) && edge.face_end_point_approx_touches_point(self.start_point())
    }

    fn end_point(&self) -> WorldMove {
        if self.edges.is_empty() {
            panic!("no points on an empty fence");
        } else if self.edges.len() == 1 {
            // the choice of which edge is the end is arbitrary, as long as it's different from the start point
            self.edges[0].face_end_points()[0]
        } else {
            let points_from_last_edge = self.edges.last().unwrap().face_end_points();
            let second_last_edge = self.edges.get(self.edges.len() - 2).unwrap();
            points_from_last_edge
                .into_iter()
                .find(|&candidate_endpoint| {
                    !second_last_edge.face_end_point_approx_touches_point(candidate_endpoint)
                })
                .unwrap()
        }
    }
    fn start_point(&self) -> WorldMove {
        if self.edges.is_empty() {
            panic!("no points on an empty fence");
        } else if self.edges.len() == 1 {
            // the choice of which edge is the end is arbitrary, as long as it's different from the start point
            self.edges[0].face_end_points()[1]
        } else {
            let points_from_first_edge = self.edges.first().unwrap().face_end_points();
            let second_edge = self.edges.get(1).unwrap();
            points_from_first_edge
                .into_iter()
                .find(|&candidate_endpoint| {
                    !second_edge.face_end_point_approx_touches_point(candidate_endpoint)
                })
                .unwrap()
        }
    }

    pub fn from_unsorted_relative_edges(
        edges: HashSet<impl Into<RelativeSquareWithOrthogonalDir>>,
    ) -> Self {
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
    use itertools::Itertools;
    use ntest::timeout;
    use pretty_assertions::assert_ne;

    use crate::utility::*;

    use super::*;

    #[test]
    #[timeout(1000)]
    fn test_make_a_fence_from_square_faces() {
        let input = vec![((5, 5), STEP_RIGHT), ((6, 4), STEP_LEFT)];
        let fence = Fence::from_relative_edges(input.clone());
        assert_eq!(fence.edges().len(), input.len());
    }
    #[test]
    #[timeout(1000)]
    fn test_add_to_end_of_fence() {
        let input = vec![((5, 5), STEP_RIGHT), ((6, 4), STEP_LEFT)];
        let mut fence = Fence::from_relative_edges(input.clone());
        fence.add_edge(((6, 4), STEP_DOWN));

        assert_eq!(fence.edges().len(), input.len() + 1);
    }
    #[test]
    #[timeout(1000)]
    fn test_add_to_start_of_fence() {
        let input = vec![((5, 5), STEP_RIGHT), ((6, 4), STEP_LEFT)];
        let mut fence = Fence::from_relative_edges(input.clone());
        fence.add_edge(((5, 6), STEP_RIGHT));

        assert_eq!(fence.edges().len(), input.len() + 1);
    }

    #[test]
    #[timeout(1000)]
    #[should_panic]
    fn test_fail_to_make_a_fence__disconnected() {
        Fence::from_relative_edges(vec![((5, 5), STEP_UP), ((6, 40), STEP_LEFT)]);
    }

    #[test]
    #[timeout(1000)]
    #[should_panic]
    fn test_fail_to_make_a_fence__duplicate_square_edge() {
        Fence::from_relative_edges(vec![((5, 5), STEP_UP), ((5, 5), STEP_UP)]);
    }
    #[test]
    #[timeout(1000)]
    #[should_panic]
    fn test_fail_to_make_a_fence__duplicate_edge_from_other_square() {
        Fence::from_relative_edges(vec![((5, 5), STEP_UP), ((5, 6), STEP_DOWN)]);
    }
    #[test]
    #[timeout(1000)]
    #[should_panic]
    fn test_fail_to_make_a_fence__forking_path() {
        let edges = (0..20).map(|y| ((5, y), STEP_RIGHT)).collect();
        let mut fence = Fence::from_relative_edges(edges);

        fence.add_edge(((5, 5), STEP_UP));
    }

    #[test]
    #[timeout(1000)]
    #[should_panic]
    fn test_fail_to_make_a_fence__not_fully_visible_from_origin() {
        Fence::from_relative_edges(vec![
            ((10, 0), STEP_LEFT),
            ((10, 0), STEP_UP),
            ((10, 0), STEP_RIGHT),
        ]);
    }
    fn full_circle_fence(center_square: impl Into<WorldStep>, radius: u32) -> Fence {
        let r = radius as i32;
        let s = center_square.into();
        let d = 2 * r + 1;
        let mut edges = vec![];
        let start_square: WorldStep = s + Into::<WorldStep>::into((-r, r));
        let mut edge: Edge = (start_square, STEP_UP).into();
        (0..4).for_each(|i| {
            (0..d).for_each(|j| {
                edges.push(edge);
                edge = edge.strafed_right();
            });
            edge = edge.strafed_left().turned_right();
        });
        Fence::from_relative_edges(edges)
    }

    #[test]
    #[timeout(1000)]
    fn test_full_circle_fence() {
        full_circle_fence((0, 2), 5);
    }

    #[test]
    #[timeout(1000)]
    #[should_panic]
    fn test_full_circle_fence__fail_because_origin_is_outside() {}
    #[test]
    #[timeout(1000)]
    #[should_panic]
    fn test_almost_full_circle_fence__fail_because_ends_block_view_of_origin() {}

    #[test]
    #[timeout(1000)]
    fn test_end_and_start_points_of_single_edge_fence_are_different() {
        let fence = Fence::from_one_edge(((5, 5), STEP_RIGHT));
        assert_ne!(fence.start_point(), fence.end_point());
    }

    #[test]
    #[timeout(1000)]
    fn test_fence_from_unordered_edges() {
        Fence::from_unsorted_relative_edges((0..20).map(|y| ((5, y), STEP_RIGHT)).collect());
    }
}
