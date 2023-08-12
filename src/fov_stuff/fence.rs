use std::collections::HashSet;

use itertools::Itertools;
use ordered_float::OrderedFloat;

use crate::rotated_to_have_split_at_max;

use crate::utility::{
    angle_interval::{AngleInterval, PartialAngleInterval},
    better_angle_from_x_axis,
    coordinate_frame_conversions::{WorldMove, WorldPoint, WorldStep},
    RelativeSquareWithOrthogonalDir, RigidlyTransformable,
};
use crate::utility::{in_ccw_order, two_in_ccw_order, SimpleResult};

#[derive(Debug, Clone, PartialEq, Default)]
pub struct RelativeFenceFullyVisibleFromOriginGoingCcw {
    edges: Vec<RelativeSquareWithOrthogonalDir>,
}

pub type Fence = RelativeFenceFullyVisibleFromOriginGoingCcw;

type Edge = RelativeSquareWithOrthogonalDir;

impl RelativeFenceFullyVisibleFromOriginGoingCcw {
    pub fn edges(&self) -> &Vec<Edge> {
        &self.edges
    }
    pub fn from_relative_edges(edges: Vec<impl Into<Edge> + Copy>) -> Self {
        assert!(in_ccw_order(
            &edges
                .iter()
                .map(|&e| Into::<Edge>::into(e).face_center_point())
                .collect_vec()
        ));
        let mut fence = Self::default();
        edges.into_iter().for_each(|edge| fence.add_edge(edge));
        fence
    }
    pub fn from_one_edge(edge: impl Into<RelativeSquareWithOrthogonalDir> + Copy) -> Self {
        Self::from_relative_edges(vec![edge])
    }
    fn try_add_to_ccw_end(&mut self, edge: Edge) -> SimpleResult {
        if self.can_connect_to_ccw_end(edge) {
            self.add_to_ccw_end(edge);
            Ok(())
        } else {
            Err(())
        }
    }
    fn try_add_to_cw_end(&mut self, edge: Edge) -> SimpleResult {
        if self.can_connect_to_cw_end(edge) {
            self.add_to_cw_end(edge);
            Ok(())
        } else {
            Err(())
        }
    }
    fn add_to_ccw_end(&mut self, edge: Edge) {
        self.edges.push(edge)
    }
    fn add_to_cw_end(&mut self, edge: Edge) {
        self.edges.insert(0, edge)
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
        if self.has_angle_overlap_with(edge) {
            panic!("Angle overlap");
        }

        if self.try_add_to_ccw_end(edge).is_err() && self.try_add_to_cw_end(edge).is_err() {
            panic!(
                "Tried to add edge that can't connect to either end:
                edge: {}
                existing: {:#?}",
                edge,
                self.edges()
            )
        }
    }
    fn overlaps_edge(&self, edge: Edge) -> bool {
        self.edges
            .iter()
            .any(|own_edge| own_edge.faces_overlap(edge))
    }
    fn can_connect_to_ccw_end(&self, edge: Edge) -> bool {
        let overlapping = self.overlaps_edge(edge);
        let ends_touch = edge.face_end_point_approx_touches_point(self.ccw_end_point());
        let edge_is_ccw_of_self = two_in_ccw_order(self.ccw_end_point(), edge.face_center_point());
        dbg!(
            "asdf",
            &self,
            &edge,
            overlapping,
            ends_touch,
            edge_is_ccw_of_self,
            edge.face_center_point(),
            self.ccw_end_point()
        );

        !overlapping && ends_touch && edge_is_ccw_of_self
    }
    fn can_connect_to_cw_end(&self, edge: Edge) -> bool {
        let overlapping = self.overlaps_edge(edge);
        let ends_touch = edge.face_end_point_approx_touches_point(self.cw_end_point());
        let edge_is_cw_of_self = two_in_ccw_order(edge.face_center_point(), self.cw_end_point());
        dbg!(
            "asdf",
            &self,
            &edge,
            overlapping,
            ends_touch,
            edge_is_cw_of_self,
            edge.face_center_point(),
            self.cw_end_point()
        );

        !overlapping && ends_touch && edge_is_cw_of_self
    }
    fn has_angle_overlap_with(&self, edge: Edge) -> bool {
        match self.spanned_angle_from_origin() {
            AngleInterval::Empty => false,
            AngleInterval::FullCircle => true,
            AngleInterval::PartialArc(arc) => arc.overlapping_but_not_exactly_touching(
                PartialAngleInterval::from_relative_square_face(edge),
            ),
        }
    }
    fn ccw_end_point(&self) -> WorldMove {
        if self.edges.is_empty() {
            panic!("no points on an empty fence");
        } else if self.edges.len() == 1 {
            // the choice of which edge is the end is arbitrary, as long as it's different from the start point
            self.edges[0].face_end_points_in_ccw_order()[1]
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
    fn cw_end_point(&self) -> WorldMove {
        if self.edges.is_empty() {
            panic!("no points on an empty fence");
        } else if self.edges.len() == 1 {
            // the choice of which edge is the end is arbitrary, as long as it's different from the start point
            self.edges[0].face_end_points_in_ccw_order()[0]
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

    fn edges_sorted_going_ccw(
        edges: HashSet<impl Into<RelativeSquareWithOrthogonalDir>>,
    ) -> Vec<RelativeSquareWithOrthogonalDir> {
        let edges_sorted_by_angle = edges
            .into_iter()
            .map(Into::<RelativeSquareWithOrthogonalDir>::into)
            .sorted_by_key(|edge| {
                OrderedFloat(better_angle_from_x_axis(edge.face_center_point()).radians)
            })
            .collect_vec();

        let edge_angle_gap_function = |a: Edge, b: Edge| {
            a.face_center_point()
                .angle_to(b.face_center_point())
                .radians
                .abs()
        };

        dbg!("asdf", edges_sorted_by_angle.clone());

        rotated_to_have_split_at_max(&edges_sorted_by_angle, edge_angle_gap_function)
    }

    pub fn from_unordered_relative_edges(
        edges: HashSet<impl Into<RelativeSquareWithOrthogonalDir>>,
    ) -> Self {
        Self::from_relative_edges(Self::edges_sorted_going_ccw(edges))
    }

    // TODO: probably make faster
    pub fn spanned_angle_from_origin(&self) -> AngleInterval {
        self.edges
            .iter()
            .map(|x| PartialAngleInterval::from_relative_square_face(*x))
            .fold(AngleInterval::Empty, |acc, x| {
                AngleInterval::union(&acc, &x)
            })
    }

    pub fn same_side_of_fence(&self, rel_square_a: WorldStep, rel_square_b: WorldStep) -> bool {
        todo!()
    }
}
impl RigidlyTransformable for RelativeFenceFullyVisibleFromOriginGoingCcw {
    fn apply_rigid_transform(&self, tf: crate::utility::RigidTransform) -> Self {
        todo!()
    }
}

#[cfg(test)]
mod tests {
    use itertools::Itertools;
    use ntest::{assert_true, timeout};
    use pretty_assertions::assert_ne;

    use crate::utility::*;

    use super::*;

    #[test]
    #[timeout(1000)]
    fn test_make_a_fence_from_square_faces() {
        let input = vec![((6, 4), STEP_LEFT), ((5, 5), STEP_RIGHT)];
        let fence = Fence::from_relative_edges(input.clone());
        assert_eq!(fence.edges().len(), input.len());
    }
    #[test]
    #[timeout(1000)]
    fn test_add_to_cw_end_of_fence() {
        let input = vec![((6, 4), STEP_LEFT), ((5, 5), STEP_RIGHT)];
        let mut fence = Fence::from_relative_edges(input.clone());
        fence.add_edge(((6, 4), STEP_DOWN));

        assert_eq!(fence.edges().len(), input.len() + 1);
    }
    #[test]
    #[timeout(1000)]
    fn test_add_to_ccw_end_of_fence() {
        let input = vec![((6, 4), STEP_LEFT), ((5, 5), STEP_RIGHT)];
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
    fn test_fail_to_make_a_fence__not_ccw() {
        Fence::from_relative_edges(vec![((5, 0), STEP_RIGHT), ((5, -1), STEP_RIGHT)]);
    }

    #[test]
    #[timeout(1000)]
    #[should_panic]
    fn test_fail_to_make_a_fence__edges_not_sequential() {
        Fence::from_relative_edges(vec![
            ((5, 0), STEP_RIGHT),
            ((5, 2), STEP_RIGHT),
            ((5, 1), STEP_RIGHT),
        ]);
    }

    #[test]
    #[timeout(1000)]
    #[should_panic]
    fn test_fail_to_make_a_fence__not_fully_visible_from_origin() {
        Fence::from_relative_edges(vec![((10, 1), STEP_RIGHT), ((10, 1), STEP_DOWN)]);
    }
    fn full_circle_ccw_fence(center_square: impl Into<WorldStep>, radius: u32) -> Fence {
        let r = radius as i32;
        let c = center_square.into();
        let d = 2 * r + 1;
        let mut edges = vec![];
        let start_square: WorldStep = c + Into::<WorldStep>::into((r, r));
        let mut edge: Edge = (start_square, STEP_UP).into();
        (0..4).for_each(|i| {
            (0..d).for_each(|j| {
                edges.push(edge);
                edge = edge.strafed_left();
            });
            edge = edge.strafed_right().turned_left();
        });
        Fence::from_relative_edges(edges)
    }

    #[test]
    #[timeout(1000)]
    fn test_full_circle_fence() {
        full_circle_ccw_fence((0, 2), 5);
    }

    #[test]
    #[timeout(1000)]
    #[should_panic]
    fn test_full_circle_fence__fail_because_origin_is_outside() {
        full_circle_ccw_fence((10, 30), 3);
    }
    #[test]
    #[timeout(1000)]
    #[should_panic]
    fn test_almost_full_circle_fence__fail_because_ends_have_angle_overlap() {
        Fence::from_relative_edges(vec![
            ((0, 1), STEP_UP),
            ((-1, 1), STEP_UP),
            ((-1, 1), STEP_LEFT),
            ((-1, 0), STEP_LEFT),
            ((-1, -1), STEP_LEFT),
            ((-1, -1), STEP_DOWN),
            ((0, -1), STEP_DOWN),
            ((1, -1), STEP_DOWN),
            ((1, -1), STEP_RIGHT),
            ((1, 0), STEP_RIGHT),
            ((1, 1), STEP_RIGHT),
            ((1, 2), STEP_RIGHT),
            ((1, 2), STEP_UP),
            ((0, 2), STEP_UP),
        ]);
    }

    #[test]
    #[timeout(1000)]
    fn test_end_points_of_single_edge_fence_are_in_ccw_order() {
        let edges = ORTHOGONAL_STEPS.map(|step| ((5, 5), step));
        for edge in edges {
            let fence = Fence::from_one_edge(edge);
            assert!(two_in_ccw_order(
                fence.cw_end_point(),
                fence.ccw_end_point()
            ));
        }
    }

    #[test]
    #[timeout(1000)]
    fn test_fence_from_unordered_edges() {
        let edges: Vec<RelativeSquareWithOrthogonalDir> =
            (0..5).map(|y| ((5, y), STEP_RIGHT).into()).collect();
        let fence = Fence::from_unordered_relative_edges(edges.iter().cloned().collect());
        assert_eq!(*fence.edges(), edges);
    }

    #[test]
    #[timeout(1000)]
    fn test_sort_edges_by_ccwness() {
        let edges: Vec<RelativeSquareWithOrthogonalDir> =
            (0..5).map(|y| ((5, y), STEP_RIGHT).into()).collect();
        let resorted_edges = Fence::edges_sorted_going_ccw(edges.iter().cloned().collect());
        assert_eq!(resorted_edges, edges);
    }
}
