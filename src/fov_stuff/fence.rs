use std::collections::HashSet;
use std::fmt::{Debug, Display, Formatter};

use itertools::Itertools;
use ordered_float::OrderedFloat;

use crate::rotated_to_have_split_at_max;

use crate::utility::general_utility::all_true;
use crate::utility::poses::{check_faces_in_ccw_order, RelativeFace, SquareWithOrthogonalDir};
use crate::utility::{
    angle_interval::{AngleInterval, PartialAngleInterval},
    better_angle_from_x_axis,
    coordinate_frame_conversions::{WorldMove, WorldPoint, WorldStep},
    RelativeSquareWithOrthogonalDir, RigidlyTransformable,
};
use crate::utility::{
    check_vectors_in_ccw_order, quadrants_of_rel_square, squares_sharing_face, two_in_ccw_order,
    CoordToString, Quadrant, SimpleResult, STEP_ZERO,
};

#[derive(Clone, PartialEq, Eq, Default)]
pub struct RelativeFenceFullyVisibleFromOriginGoingCcw {
    edges: Vec<RelativeSquareWithOrthogonalDir>,
}

pub type Fence = RelativeFenceFullyVisibleFromOriginGoingCcw;

impl RelativeFenceFullyVisibleFromOriginGoingCcw {
    pub fn edges(&self) -> &Vec<RelativeFace> {
        &self.edges
    }
    pub fn new<T, T2>(faces: T) -> Self
    where
        T: IntoIterator<Item = T2> + Clone,
        T2: Into<RelativeFace> + Copy,
    {
        Self::from_faces_in_ccw_order(faces)
    }
    pub fn from_faces_in_ccw_order(
        faces: impl IntoIterator<Item = impl Into<RelativeFace>> + Clone,
    ) -> Self {
        Self::try_from_faces_in_ccw_order(faces).expect("Failed to make fence")
    }
    pub fn try_from_faces_in_ccw_order(
        faces: impl IntoIterator<Item = impl Into<RelativeFace>> + Clone,
    ) -> Result<Self, String> {
        let ccw_check_result = check_faces_in_ccw_order(faces.clone());
        if ccw_check_result.is_err() {
            return Err(ccw_check_result.err().unwrap());
        }

        let mut new_fence = Self::default();
        let iter = faces.into_iter();
        let edge_adding_result: Result<(), String> =
            iter.map(|edge| new_fence.try_add_edge(edge)).collect();
        match edge_adding_result {
            Ok(_) => Ok(new_fence),
            Err(err_string) => Err(err_string),
        }
    }
    pub fn from_one_edge(edge: impl Into<RelativeFace>) -> Self {
        Self::from_faces_in_ccw_order(vec![edge.into()])
    }
    pub fn from_radius_and_arc(radius: u32, arc: AngleInterval) -> Self {
        todo!()
    }
    fn try_add_to_ccw_end(&mut self, edge: RelativeFace) -> SimpleResult {
        if self.can_connect_to_ccw_end(edge) {
            self.add_to_ccw_end(edge);
            Ok(())
        } else {
            Err(())
        }
    }
    fn try_add_to_cw_end(&mut self, edge: RelativeFace) -> SimpleResult {
        if self.can_connect_to_cw_end(edge) {
            self.add_to_cw_end(edge);
            Ok(())
        } else {
            Err(())
        }
    }
    fn add_to_ccw_end(&mut self, edge: RelativeFace) {
        self.edges.push(edge)
    }
    fn add_to_cw_end(&mut self, edge: RelativeFace) {
        self.edges.insert(0, edge)
    }
    pub fn furthest_inside_square(&self) -> WorldStep {
        self.squares_adjacent_to_fence_edges()
            .unique()
            .max_by_key(|step| step.square_length())
            .unwrap()
    }
    fn squares_adjacent_to_fence_edges(&self) -> impl Iterator<Item = WorldStep> + '_ {
        self.edges
            .iter()
            .cloned()
            .flat_map(|edge| squares_sharing_face(edge))
    }
    fn add_edge_or_panic(&mut self, edge: impl Into<RelativeFace>) {
        self.try_add_edge(edge).expect("Failed to add edge: ");
    }
    fn try_add_edge(&mut self, edge: impl Into<RelativeFace>) -> Result<(), String> {
        let edge = edge.into().flipped_to_face_origin();

        if self.edges.is_empty() {
            self.edges.push(edge);
            return Ok(());
        }
        if self.overlaps_edge(edge) {
            return Err(format!("Tried to add overlapping edge to fence: {}", edge));
        }
        if self.has_angle_overlap_with_edge(edge) {
            return Err(format!(
                "Angle overlap: \nfence: {:?}\nedge: {}",
                self,
                edge.to_string()
            ));
        }

        if self.try_add_to_ccw_end(edge).is_err() && self.try_add_to_cw_end(edge).is_err() {
            return Err(format!(
                "Tried to add edge that can't connect to either end:
                edge: {}
                existing: {:#?}",
                edge,
                self.edges()
            ));
        }
        Ok(())
    }

    fn overlaps_edge(&self, edge: RelativeFace) -> bool {
        self.edges
            .iter()
            .any(|own_edge| own_edge.faces_overlap(edge))
    }
    fn can_connect_to_ccw_end(&self, edge: RelativeFace) -> bool {
        let overlapping = self.overlaps_edge(edge);
        let ends_touch = edge.face_end_point_approx_touches_point(self.ccw_end_point());
        let edge_is_ccw_of_self =
            two_in_ccw_order(self.ccw_end_point(), edge.center_point_of_face());

        !overlapping && ends_touch && edge_is_ccw_of_self
    }
    fn can_connect_to_cw_end(&self, edge: RelativeFace) -> bool {
        let overlapping = self.overlaps_edge(edge);
        let ends_touch = edge.face_end_point_approx_touches_point(self.cw_end_point());
        let edge_is_cw_of_self = two_in_ccw_order(edge.center_point_of_face(), self.cw_end_point());

        !overlapping && ends_touch && edge_is_cw_of_self
    }
    fn point_by_index(&self, index: i32) -> WorldMove {
        let num_points = self.edges.len() + 1;
        let positive_point_index: usize = if index >= 0 {
            index
        } else {
            num_points as i32 + index
        } as usize;
        if positive_point_index == 0 {
            self.edges[0].cw_end_of_face()
        } else {
            self.edges[positive_point_index - 1].ccw_end_of_face()
        }
    }

    pub fn try_concatenate_allowing_one_edge_of_overlap(
        &self,
        other: &Self,
    ) -> Result<Self, String> {
        // TODO: refactor this function a bunch

        let a_cw_touching_b_ccw =
            |a: &Fence, b: &Fence| a.point_by_index(0) == b.point_by_index(-1);
        let a_cw_overlapping_b_ccw = |a: &Fence, b: &Fence| {
            a.point_by_index(1) == b.point_by_index(-1)
                && a.point_by_index(0) == b.point_by_index(-2)
        };

        let touch_at_self_cw_ccw = [
            a_cw_touching_b_ccw(self, other),
            a_cw_touching_b_ccw(other, self),
        ];
        let overlap_at_self_cw_ccw = [
            a_cw_overlapping_b_ccw(self, other),
            a_cw_overlapping_b_ccw(other, self),
        ];

        let connect_at_self_cw_ccw =
            [0, 1].map(|i| touch_at_self_cw_ccw[i] || overlap_at_self_cw_ccw[i]);

        let both_ends_connect = all_true(&connect_at_self_cw_ccw);

        let num_to_skip_at_start_of_self = if overlap_at_self_cw_ccw[0] { 1 } else { 0 };
        let num_to_skip_at_start_of_other = if overlap_at_self_cw_ccw[1] { 1 } else { 0 };

        let self_iter = self.edges.iter().skip(num_to_skip_at_start_of_self);
        let other_iter = other.edges.iter().skip(num_to_skip_at_start_of_other);

        let (first_iter, second_iter) = if connect_at_self_cw_ccw[1] {
            (self_iter, other_iter)
        } else {
            (other_iter, self_iter)
        };

        Self::try_from_faces_in_ccw_order(first_iter.cloned().chain(second_iter.cloned()))
    }
    fn has_angle_overlap_with_edge(&self, edge: RelativeFace) -> bool {
        self.spanned_angle_from_origin()
            .overlapping_but_not_exactly_touching(PartialAngleInterval::from_relative_square_face(
                edge,
            ))
    }
    // TODO: add tolerance?
    fn has_non_zero_angle_overlap_with_rel_square(&self, rel_square: WorldStep) -> bool {
        if rel_square == STEP_ZERO {
            return !self.edges.is_empty();
        }
        self.spanned_angle_from_origin()
            .overlapping_but_not_exactly_touching(PartialAngleInterval::from_relative_square(
                rel_square,
            ))
    }
    fn ccw_end_point(&self) -> WorldMove {
        if self.edges.is_empty() {
            panic!("no points on an empty fence");
        } else if self.edges.len() == 1 {
            // the choice of which edge is the end is arbitrary, as long as it's different from the start point
            self.edges[0].end_points_of_face_in_ccw_order()[1]
        } else {
            let points_from_last_edge = self.edges.last().unwrap().end_points_of_face();
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
            self.edges[0].end_points_of_face_in_ccw_order()[0]
        } else {
            let points_from_first_edge = self.edges.first().unwrap().end_points_of_face();
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
                OrderedFloat(better_angle_from_x_axis(edge.center_point_of_face()).radians)
            })
            .collect_vec();

        let edge_angle_gap_function = |a: RelativeFace, b: RelativeFace| {
            a.center_point_of_face()
                .angle_to(b.center_point_of_face())
                .radians
                .abs()
        };

        rotated_to_have_split_at_max(&edges_sorted_by_angle, edge_angle_gap_function)
    }

    pub fn from_unordered_relative_edges(
        edges: HashSet<impl Into<RelativeSquareWithOrthogonalDir>>,
    ) -> Self {
        Self::from_faces_in_ccw_order(Self::edges_sorted_going_ccw(edges))
    }

    pub fn spanned_angle_from_origin(&self) -> AngleInterval {
        if self.cw_end_point() == self.ccw_end_point() {
            AngleInterval::FullCircle
        } else {
            AngleInterval::PartialArc(PartialAngleInterval::from_angles(
                better_angle_from_x_axis(self.cw_end_point()),
                better_angle_from_x_axis(self.ccw_end_point()),
            ))
        }
    }

    pub fn on_same_side_of_fence(
        &self,
        rel_square_a: impl Into<WorldStep>,
        rel_square_b: impl Into<WorldStep>,
    ) -> bool {
        self.is_radially_inside_fence(rel_square_a) == self.is_radially_inside_fence(rel_square_b)
    }
    pub fn is_radially_inside_fence(&self, can_be_rel_square: impl Into<WorldStep>) -> bool {
        let rel_square: WorldStep = can_be_rel_square.into();
        if rel_square == STEP_ZERO {
            return !self.edges.is_empty();
        }
        let fence_edges_with_angle_overlap = self
            .edges
            .iter()
            .filter(|&&fence_edge| {
                PartialAngleInterval::from_relative_square_face(fence_edge)
                    .overlapping_but_not_exactly_touching(
                        PartialAngleInterval::from_relative_square(rel_square),
                    )
            })
            .collect_vec();
        if fence_edges_with_angle_overlap.is_empty() {
            panic!(
                "Square {} has no angle overlap with self.\n\
                \tSelf:   {}\n\
                \tSquare: {}",
                rel_square.to_string(),
                self.spanned_angle_from_origin(),
                PartialAngleInterval::from_relative_square(rel_square)
            );
        }
        fence_edges_with_angle_overlap.iter().any(|&edge| {
            (rel_square.square_length() as f32) < edge.center_point_of_face().square_length()
        })
    }
    pub fn touched_quadrants(&self) -> HashSet<Quadrant> {
        todo!()
    }
    fn isolated_and_extrapolated_to_cover_quadrant(&self, quadrant: Quadrant) -> Fence {
        todo!()
    }
}
impl RigidlyTransformable for RelativeFenceFullyVisibleFromOriginGoingCcw {
    fn apply_rigid_transform(&self, tf: crate::utility::RigidTransform) -> Self {
        todo!()
    }
}

impl<T> From<T> for Fence
where
    T: Into<RelativeFace>,
{
    fn from(value: T) -> Self {
        Self::from_one_edge(value)
    }
}

impl Debug for Fence {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "edges: {:?}\nspanned angle: {}",
            self.edges,
            self.spanned_angle_from_origin()
        )
    }
}

#[cfg(test)]
mod tests {
    use itertools::Itertools;
    use ntest::{assert_false, assert_true};
    use pretty_assertions::assert_ne;

    use crate::utility::*;

    use super::*;

    #[test]
    fn test_make_a_fence_from_square_faces() {
        let input = vec![((6, 4), STEP_LEFT), ((5, 5), STEP_RIGHT)];
        let fence = Fence::from_faces_in_ccw_order(input.clone());
        assert_eq!(fence.edges().len(), input.len());
    }
    #[test]
    fn test_add_to_cw_end_of_fence() {
        let input = vec![((6, 4), STEP_LEFT), ((5, 5), STEP_RIGHT)];
        let mut fence = Fence::from_faces_in_ccw_order(input.clone());
        fence.add_edge_or_panic(((6, 4), STEP_DOWN));

        assert_eq!(fence.edges().len(), input.len() + 1);
    }
    #[test]
    fn test_add_to_ccw_end_of_fence() {
        let input = vec![((6, 4), STEP_LEFT), ((5, 5), STEP_RIGHT)];
        let mut fence = Fence::from_faces_in_ccw_order(input.clone());
        fence.add_edge_or_panic(((5, 6), STEP_RIGHT));

        assert_eq!(fence.edges().len(), input.len() + 1);
    }

    #[test]
    #[should_panic]
    fn test_fail_to_make_a_fence__disconnected() {
        Fence::from_faces_in_ccw_order(vec![((5, 5), STEP_UP), ((6, 40), STEP_LEFT)]);
    }

    #[test]
    #[should_panic]
    fn test_fail_to_make_a_fence__duplicate_square_edge() {
        Fence::from_faces_in_ccw_order(vec![((5, 5), STEP_UP), ((5, 5), STEP_UP)]);
    }
    #[test]
    #[should_panic]
    fn test_fail_to_make_a_fence__duplicate_edge_from_other_square() {
        Fence::from_faces_in_ccw_order(vec![((5, 5), STEP_UP), ((5, 6), STEP_DOWN)]);
    }
    #[test]
    #[should_panic]
    fn test_fail_to_make_a_fence__forking_path() {
        let edges = (0..20).map(|y| ((5, y), STEP_RIGHT));
        let mut fence = Fence::from_faces_in_ccw_order(edges);

        fence.add_edge_or_panic(((5, 5), STEP_UP));
    }
    #[test]
    #[should_panic]
    fn test_fail_to_make_a_fence__not_ccw() {
        Fence::from_faces_in_ccw_order(vec![((5, 0), STEP_RIGHT), ((5, -1), STEP_RIGHT)]);
    }

    #[test]
    #[should_panic]
    fn test_fail_to_make_a_fence__edges_not_sequential() {
        Fence::from_faces_in_ccw_order(vec![
            ((5, 0), STEP_RIGHT),
            ((5, 2), STEP_RIGHT),
            ((5, 1), STEP_RIGHT),
        ]);
    }

    #[test]
    #[should_panic]
    fn test_fail_to_make_a_fence__not_fully_visible_from_origin() {
        Fence::from_faces_in_ccw_order(vec![((10, 1), STEP_RIGHT), ((10, 1), STEP_DOWN)]);
    }
    fn full_circle_ccw_fence(center_square: impl Into<WorldStep>, radius: u32) -> Fence {
        let r = radius as i32;
        let c = center_square.into();
        let d = 2 * r + 1;
        let mut edges = vec![];
        let start_square: WorldStep = c + Into::<WorldStep>::into((r, r));
        let mut edge: RelativeFace = (start_square, STEP_UP).into();
        (0..4).for_each(|i| {
            (0..d).for_each(|j| {
                edges.push(edge);
                edge = edge.strafed_left();
            });
            edge = edge.strafed_right().turned_left();
        });
        Fence::from_faces_in_ccw_order(edges)
    }

    #[test]
    fn test_full_circle_fence() {
        full_circle_ccw_fence((0, 2), 5);
    }

    #[test]
    #[should_panic]
    fn test_full_circle_fence__fail_because_origin_is_outside() {
        full_circle_ccw_fence((10, 30), 3);
    }
    #[test]
    #[should_panic]
    fn test_almost_full_circle_fence__fail_because_ends_have_angle_overlap() {
        Fence::from_faces_in_ccw_order(vec![
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
    fn test_fence_from_unordered_edges() {
        let edges: Vec<RelativeSquareWithOrthogonalDir> =
            (0..5).map(|y| ((5, y), STEP_RIGHT).into()).collect();
        let fence = Fence::from_unordered_relative_edges(edges.iter().cloned().collect());
        assert_eq!(*fence.edges(), edges);
    }

    #[test]
    fn test_sort_edges_by_ccwness() {
        let edges: Vec<RelativeSquareWithOrthogonalDir> =
            (0..5).map(|y| ((5, y), STEP_RIGHT).into()).collect();
        let resorted_edges = Fence::edges_sorted_going_ccw(edges.iter().cloned().collect());
        assert_eq!(resorted_edges, edges);
    }
    #[test]
    fn test_same_side_of_fence() {
        let fence = Fence::from_faces_in_ccw_order(vec![
            ((8, 0), STEP_RIGHT),
            ((8, 1), STEP_RIGHT),
            ((8, 2), STEP_RIGHT),
            ((8, 2), STEP_UP),
            ((7, 2), STEP_UP),
            ((6, 3), STEP_RIGHT),
        ]);

        // basic case
        assert_false!(fence.on_same_side_of_fence((0, 0), (200, 100)));
        assert_true!(fence.on_same_side_of_fence((0, 0), (5, 2)));

        // close to the line
        assert_false!(fence.on_same_side_of_fence((8, 0), (9, 1)));
        assert_false!(fence.on_same_side_of_fence((7, 1), (7, 4)));
    }
    #[test]
    fn test_radially_inside_fence__simple_case() {
        let fence = Fence::from_faces_in_ccw_order(vec![
            ((8, 0), STEP_RIGHT),
            ((8, 1), STEP_RIGHT),
            ((8, 2), STEP_RIGHT),
            ((8, 2), STEP_UP),
            ((7, 2), STEP_UP),
            ((6, 3), STEP_RIGHT),
            ((6, 3), STEP_UP),
            ((5, 3), STEP_UP),
            ((4, 3), STEP_UP),
            ((3, 3), STEP_UP),
            ((2, 3), STEP_UP),
            ((1, 3), STEP_UP),
            ((0, 3), STEP_UP),
        ]);
        let inside_rel_squares = vec![(8, 0), (0, 0), (6, 3), (0, 3), (0, 1), (3, 3)];
        let outside_rel_squares = vec![(9, 0), (5, 4), (30, 5)];
        check_points_inside_outside(&fence, &inside_rel_squares, &outside_rel_squares);
    }
    #[test]
    fn test_radially_inside_fence__short_fence_case() {
        let fence = Fence::from_faces_in_ccw_order(vec![((4, 3), STEP_UP), ((3, 3), STEP_UP)]);
        let inside_rel_squares = vec![(3, 3), (4, 3), (2, 3)];
        let outside_rel_squares = vec![(3, 4), (4, 4)];
        check_points_inside_outside(&fence, &inside_rel_squares, &outside_rel_squares);
    }
    fn check_points_inside_outside(
        fence: &Fence,
        should_be_inside: &Vec<(i32, i32)>,
        should_be_outside: &Vec<(i32, i32)>,
    ) {
        should_be_inside.into_iter().for_each(|&s| {
            assert!(
                fence.is_radially_inside_fence(s),
                "square {:#?} should have been inside",
                s
            )
        });
        should_be_outside.into_iter().for_each(|&s| {
            assert!(
                !fence.is_radially_inside_fence(s),
                "square {:#?} should have been outside",
                s
            )
        });
    }
    #[test]
    fn test_radially_inside_fence__wraparound_case() {
        let fence = Fence::from_faces_in_ccw_order(vec![
            // ((5, 1), STEP_UP),
            // ((4, 1), STEP_UP),
            // ((3, 1), STEP_UP),
            // ((2, 1), STEP_UP),
            ((1, 1), STEP_UP),
            ((0, 1), STEP_UP),
            ((0, 1), STEP_LEFT),
            ((0, 0), STEP_LEFT),
            ((0, 0), STEP_DOWN),
            ((1, 0), STEP_DOWN),
            ((2, 0), STEP_DOWN),
            ((3, 0), STEP_DOWN),
            ((4, 0), STEP_DOWN),
            ((5, 0), STEP_DOWN),
            ((6, 0), STEP_DOWN),
            ((7, 0), STEP_DOWN),
            ((8, 0), STEP_DOWN),
            ((8, 0), STEP_RIGHT),
            ((8, 1), STEP_RIGHT),
            ((8, 2), STEP_RIGHT),
            ((8, 2), STEP_UP),
            ((7, 2), STEP_UP),
            ((6, 3), STEP_RIGHT),
            ((6, 3), STEP_UP),
        ]);

        //  5|
        //   |
        //   |     🭾
        //   |      ▔🭾
        //   🭽▔      ▕
        //  0🭼▁▁▁▁▁▁▁🭿-------------
        //   0    5  89

        // 🭽 🭾 🭼 🭿 ▏▔▁▕

        let inside_rel_squares = vec![(0, 0), (3, 1), (3, 2), (6, 3)];
        let outside_rel_squares = vec![(-1, 0), (3, 4), (6, 4)];
        check_points_inside_outside(&fence, &inside_rel_squares, &outside_rel_squares);
    }
    #[test]
    fn test_radially_inside_fence__minimal_case() {
        let fence = Fence::from_faces_in_ccw_order(vec![((0, 0), STEP_RIGHT), ((0, 0), STEP_UP)]);
        let inside_rel_squares = vec![(0, 0)];
        let outside_rel_squares = vec![(3, 4), (3, 2), (6, 4), (1, 0), (0, 1), (1, 1), (100, 100)];
        check_points_inside_outside(&fence, &inside_rel_squares, &outside_rel_squares);
    }
    #[test]
    fn test_convenient_creation() {
        let x: RelativeFenceFullyVisibleFromOriginGoingCcw = Fence::new(vec![
            ((3, 0), STEP_DOWN),
            ((3, 0), STEP_RIGHT),
            ((3, 1), STEP_RIGHT),
            ((3, 1), STEP_UP),
            ((2, 2), STEP_RIGHT),
            ((2, 2), STEP_UP),
        ]);
    }
    #[test]
    fn test_create_with_midpoints_of_end_edges_passing_through_origin() {
        let a = Fence::from_faces_in_ccw_order([
            ((3, 0), STEP_RIGHT),
            ((3, 1), STEP_RIGHT),
            ((3, 2), STEP_RIGHT),
            ((3, 2), STEP_UP),
            ((2, 2), STEP_UP),
            ((1, 2), STEP_UP),
            ((0, 2), STEP_UP),
            ((-1, 2), STEP_UP),
            ((-1, 2), STEP_LEFT),
            ((-1, 1), STEP_LEFT),
            ((-1, 0), STEP_LEFT),
        ]);
    }
    #[test]
    fn test_try_concatenate__pass__simple() {
        let a = Fence::from_faces_in_ccw_order([((3, 0), STEP_RIGHT), ((3, 1), STEP_RIGHT)]);
        let b = Fence::from_faces_in_ccw_order([((3, 2), STEP_RIGHT), ((3, 3), STEP_RIGHT)]);
        let ab = Fence::from_faces_in_ccw_order([
            ((3, 0), STEP_RIGHT),
            ((3, 1), STEP_RIGHT),
            ((3, 2), STEP_RIGHT),
            ((3, 3), STEP_RIGHT),
        ]);
        assert_eq!(
            a.try_concatenate_allowing_one_edge_of_overlap(&b),
            Ok(ab.clone())
        );
        assert_eq!(b.try_concatenate_allowing_one_edge_of_overlap(&a), Ok(ab));
    }
    #[test]
    fn test_try_concatenate__pass__sum_to_full_surround__no_overlap() {
        let a = Fence::from_faces_in_ccw_order([
            ((3, 0), STEP_RIGHT),
            ((3, 1), STEP_RIGHT),
            ((3, 2), STEP_RIGHT),
            ((3, 2), STEP_UP),
            ((2, 2), STEP_UP),
            ((1, 2), STEP_UP),
            ((0, 2), STEP_UP),
            ((-1, 2), STEP_UP),
            ((-1, 2), STEP_LEFT),
            ((-1, 1), STEP_LEFT),
            ((-1, 0), STEP_LEFT),
        ]);
        let b = Fence::from_faces_in_ccw_order([
            ((-1, -1), STEP_LEFT),
            ((-1, -2), STEP_LEFT),
            ((-1, -2), STEP_DOWN),
            ((0, -2), STEP_DOWN),
            ((1, -2), STEP_DOWN),
            ((2, -2), STEP_DOWN),
            ((3, -2), STEP_DOWN),
            ((3, -2), STEP_RIGHT),
            ((3, -1), STEP_RIGHT),
        ]);
        let ab =
            Fence::from_faces_in_ccw_order(a.edges.iter().cloned().chain(b.edges.iter().cloned()));
        assert_eq!(
            a.try_concatenate_allowing_one_edge_of_overlap(&b),
            Ok(ab.clone())
        );
        assert!(b.try_concatenate_allowing_one_edge_of_overlap(&a).is_ok());
    }
    #[test]
    fn test_no_standard_representation_for_full_circle_fences() {
        let a = Fence::from_faces_in_ccw_order([
            ((3, 0), STEP_RIGHT),
            ((3, 1), STEP_RIGHT),
            ((3, 2), STEP_RIGHT),
            ((3, 2), STEP_UP),
            ((2, 2), STEP_UP),
            ((1, 2), STEP_UP),
            ((0, 2), STEP_UP),
            ((-1, 2), STEP_UP),
            ((-1, 2), STEP_LEFT),
            ((-1, 1), STEP_LEFT),
            ((-1, 0), STEP_LEFT),
        ]);
        let b = Fence::from_faces_in_ccw_order([
            ((-1, -1), STEP_LEFT),
            ((-1, -2), STEP_LEFT),
            ((-1, -2), STEP_DOWN),
            ((0, -2), STEP_DOWN),
            ((1, -2), STEP_DOWN),
            ((2, -2), STEP_DOWN),
            ((3, -2), STEP_DOWN),
            ((3, -2), STEP_RIGHT),
            ((3, -1), STEP_RIGHT),
        ]);
        // No standard representation for full circle fences
        assert_ne!(
            b.try_concatenate_allowing_one_edge_of_overlap(&a),
            a.try_concatenate_allowing_one_edge_of_overlap(&b)
        );
    }
    #[test]
    fn test_try_concatenate__fail__not_touching() {
        let a = Fence::from_faces_in_ccw_order([((3, 0), STEP_RIGHT), ((3, 1), STEP_RIGHT)]);
        let b = Fence::from_faces_in_ccw_order([((3, 3), STEP_RIGHT), ((3, 4), STEP_RIGHT)]);
        assert!(a.try_concatenate_allowing_one_edge_of_overlap(&b).is_err());
    }
    #[test]
    fn test_try_concatenate__fail__radial_distance() {
        let a = Fence::from_faces_in_ccw_order([((3, 0), STEP_RIGHT), ((3, 1), STEP_RIGHT)]);
        let b = Fence::from_faces_in_ccw_order([((2, 3), STEP_DOWN), ((1, 3), STEP_DOWN)]);
        assert!(a.try_concatenate_allowing_one_edge_of_overlap(&b).is_err());
    }
    #[test]
    fn test_try_concatenate__fail__visual_overlap() {
        let a = Fence::from_faces_in_ccw_order([((3, 2), STEP_RIGHT), ((3, 3), STEP_RIGHT)]);
        let b = Fence::from_faces_in_ccw_order([((4, 2), STEP_RIGHT), ((4, 3), STEP_RIGHT)]);
        assert!(a.try_concatenate_allowing_one_edge_of_overlap(&b).is_err());
    }
    #[test]
    fn test_try_concatenate__fail__attempted_unilateral_shortcut() {
        let a = Fence::from_faces_in_ccw_order([
            ((3, 0), STEP_DOWN),
            ((3, 0), STEP_RIGHT),
            ((3, 1), STEP_RIGHT),
        ]);
        let b = Fence::from_faces_in_ccw_order([((3, -2), STEP_RIGHT), ((3, -1), STEP_RIGHT)]);
        assert!(a.try_concatenate_allowing_one_edge_of_overlap(&b).is_err());
    }
    #[test]
    fn test_try_concatenate__pass__one_common_end_face_is_allowed() {
        let a = Fence::from_faces_in_ccw_order([
            ((3, -1), STEP_RIGHT),
            ((3, 0), STEP_RIGHT),
            ((3, 1), STEP_RIGHT),
        ]);
        let b = Fence::from_faces_in_ccw_order([((3, 1), STEP_RIGHT), ((3, 2), STEP_RIGHT)]);
        let ab = Fence::from_faces_in_ccw_order([
            ((3, -1), STEP_RIGHT),
            ((3, 0), STEP_RIGHT),
            ((3, 1), STEP_RIGHT),
            ((3, 2), STEP_RIGHT),
        ]);
        assert_eq!(a.try_concatenate_allowing_one_edge_of_overlap(&b), Ok(ab));
    }
    #[test]
    fn test_try_concatenate__fail__identical_edges_overlapping_too_much() {
        let a = Fence::from_faces_in_ccw_order([
            ((3, -1), STEP_RIGHT),
            ((3, 0), STEP_RIGHT),
            ((3, 1), STEP_RIGHT),
        ]);
        let b = Fence::from_faces_in_ccw_order([
            ((3, 0), STEP_RIGHT),
            ((3, 1), STEP_RIGHT),
            ((3, 2), STEP_RIGHT),
        ]);
        assert!(a.try_concatenate_allowing_one_edge_of_overlap(&b).is_err());
    }
    #[test]
    fn test_point_by_index() {
        let fence = Fence::from_faces_in_ccw_order([
            ((5, 0), STEP_LEFT),
            ((5, 1), STEP_LEFT),
            ((5, 2), STEP_LEFT),
            ((5, 3), STEP_LEFT),
            ((5, 4), STEP_LEFT),
        ]);
        assert_eq!(fence.point_by_index(0), (4.5, -0.5).into());
        assert_eq!(fence.point_by_index(1), (4.5, 0.5).into());
        assert_eq!(fence.point_by_index(-1), (4.5, 4.5).into());
        assert_eq!(fence.point_by_index(-2), (4.5, 3.5).into());
    }
    #[test]
    #[should_panic]
    fn test_point_by_index__off_end_positive() {
        let fence = Fence::from_faces_in_ccw_order([
            ((5, 0), STEP_LEFT),
            ((5, 1), STEP_LEFT),
            ((5, 2), STEP_LEFT),
            ((5, 3), STEP_LEFT),
            ((5, 4), STEP_LEFT),
        ]);
        fence.point_by_index(6);
    }
    #[test]
    #[should_panic]
    fn test_point_by_index__off_end_negative() {
        let fence = Fence::from_faces_in_ccw_order([
            ((5, 0), STEP_LEFT),
            ((5, 1), STEP_LEFT),
            ((5, 2), STEP_LEFT),
            ((5, 3), STEP_LEFT),
            ((5, 4), STEP_LEFT),
        ]);
        fence.point_by_index(-7);
    }
    #[test]
    fn test_fence_has_canonical_representation() {
        assert_eq!(
            Fence::from_one_edge(((5, 2), STEP_DOWN)).edges()[0],
            ((5, 1), STEP_UP).into(),
            "quadrant 1, flip fence"
        );
        assert_eq!(
            Fence::from_one_edge(((5, 2), STEP_UP)).edges()[0],
            ((5, 2), STEP_UP).into(),
            "quadrant 1, no change"
        );
        assert_eq!(
            Fence::from_one_edge(((-5, 2), STEP_RIGHT)).edges()[0],
            ((-4, 2), STEP_LEFT).into(),
            "quadrant 2, flip face"
        );
    }
    #[test]
    fn test_spanned_angle_of_fence__full_circle() {
        let fence = Fence::from_faces_in_ccw_order([
            ((0, 0), STEP_RIGHT),
            ((0, 0), STEP_UP),
            ((0, 0), STEP_LEFT),
            ((0, 0), STEP_DOWN),
        ]);
        assert_eq!(fence.spanned_angle_from_origin(), AngleInterval::FullCircle);
    }
    #[test]
    fn test_spanned_angle_of_fence__partial() {
        let fence = Fence::from_faces_in_ccw_order([((0, 0), STEP_RIGHT)]);
        assert_eq!(
            fence.spanned_angle_from_origin(),
            AngleInterval::from_degrees(-45.0, 45.0)
        );
    }
}
