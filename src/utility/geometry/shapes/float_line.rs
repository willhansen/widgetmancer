use crate::utility::*;

trait_alias!(pub trait PointReqs = float_coordinate::Operations);

/// A traditional line that extends infinitely in both directions, now with floating point coordinates
pub type Shape<UnitType> = Line<Point2D<f32, UnitType>>;

pub trait Operations<P: PointReqs>: line::Operations<P> {
    fn point_is_on_or_normal_to_line_segment(&self, point: P) -> bool {
        let [start_point, end_point] = self.two_different_arbitrary_points_on_line();

        let point_relative_to_start_point = point - start_point;
        let end_point_relative_to_start_point = end_point - start_point;
        let point_is_on_end_side_of_start_point =
            point_relative_to_start_point.dot(end_point_relative_to_start_point) > 0.0;

        let point_relative_to_end_point = point - end_point;
        let point_is_on_start_side_of_end_point =
            point_relative_to_end_point.dot(-end_point_relative_to_start_point) > 0.0;

        point_is_on_end_side_of_start_point && point_is_on_start_side_of_end_point
    }

    fn angle_with_positive_x_axis(&self) -> Angle<f32> {
        let [angle_a, angle_b] = self.parallel_directions();
        if angle_a.radians.cos() < 0.0 {
            angle_b
        } else {
            angle_a
        }
    }

    fn unordered_line_intersections_with_centered_unit_square_with_tolerance(
        &self,
        tolerance: f32,
    ) -> Vec<P> {
        let regular_intersections = self.unordered_line_intersections_with_centered_unit_square();
        if !regular_intersections.is_empty() {
            return regular_intersections;
        }

        let square_corners_and_distances = corner_points_of_centered_unit_square()
            .iter()
            .map(|&p| (p, self.normal_distance_to_point(p)))
            .collect_vec();
        let filtered_by_distance = square_corners_and_distances
            .iter()
            .filter(|(point, dist)| *dist <= tolerance)
            .collect_vec();

        if filtered_by_distance.is_empty() {
            return vec![];
        }

        let grouped_by_distance = filtered_by_distance
            .iter()
            .sorted_by_key(|(point, distance)| OrderedFloat(*distance))
            .group_by(|(point, distance)| distance);
        let closest_points = grouped_by_distance
            .into_iter()
            .next()
            .unwrap()
            .1
            .map(|(point, distance)| *point)
            .collect_vec();
        closest_points
    }
    fn unordered_line_intersections_with_expanded_centered_unit_square(
        &self,
        expansion_length: f32,
    ) -> Vec<P> {
        let [line_point_a, line_point_b] = self.two_different_arbitrary_points_on_line();
        let half_side_length = 0.5 + expansion_length;

        let is_vertical_line = line_point_a.x() == line_point_b.x();
        let is_horizontal_line = line_point_a.y() == line_point_b.y();

        if is_vertical_line {
            let x = line_point_a.x();
            if x.abs() <= half_side_length {
                vec![(x, half_side_length).into(), (x, -half_side_length).into()]
            } else {
                vec![]
            }
        } else if is_horizontal_line {
            let y = line_point_a.y();
            if y.abs() <= half_side_length {
                vec![(half_side_length, y).into(), (-half_side_length, y).into()]
            } else {
                vec![]
            }
        } else {
            // y = mx + b
            let dy = line_point_b.y() - line_point_a.y();
            let dx = line_point_b.x() - line_point_a.x();
            let m = dy / dx;
            // b = y - m*x
            let b = line_point_a.y() - m * line_point_a.x();

            let side_positions = vec![half_side_length, -half_side_length];

            let mut candidate_intersections: Vec<P> = vec![];
            for &x in &side_positions {
                let y = m * x + b;
                if y.abs() <= half_side_length {
                    candidate_intersections.push((x, y).into());
                }
            }
            for y in side_positions {
                let x = (y - b) / m;
                // top and bottom don't catch corners, sides do
                if x.abs() < half_side_length {
                    candidate_intersections.push((x, y).into());
                }
            }
            // this captures the edge case of corners
            // remove duplicates
            match candidate_intersections.len() {
                2 => {
                    if candidate_intersections[0] == candidate_intersections[1] {
                        vec![candidate_intersections[0]]
                    } else {
                        candidate_intersections
                    }
                }
                1 => candidate_intersections,
                0 => vec![],
                _ => furthest_apart_points(candidate_intersections).into(),
            }
        }
    }
    fn unordered_line_intersections_with_centered_unit_square(&self) -> Vec<P> {
        self.unordered_line_intersections_with_expanded_centered_unit_square(0.0)
    }
    fn line_intersects_with_centered_unit_square(&self) -> bool {
        self.intersects_with_expanded_centered_unit_square(0.0)
    }
    fn intersects_with_expanded_centered_unit_square(&self, per_face_extension: f32) -> bool {
        !self
            .unordered_line_intersections_with_expanded_centered_unit_square(per_face_extension)
            .is_empty()
    }

    fn intersection_point_with_other_extended_line(
        &self,
        other: impl line::Operations<P>,
    ) -> Option<P> {
        let [self_p1, self_p2] = self.two_different_arbitrary_points_on_line();
        let [other_p1, other_p2] = other.two_different_arbitrary_points_on_line();

        // Equation from https://en.wikipedia.org/wiki/Line%E2%80%93line_intersection
        let (x1, y1) = self_p1.tuple();
        let (x2, y2) = self_p2.tuple();
        let (x3, y3) = other_p1.tuple();
        let (x4, y4) = other_p2.tuple();

        let a = x1 * y2 - y1 * x2;
        let b = x3 * y4 - y3 * x4;
        let denominator = (x1 - x2) * (y3 - y4) - (y1 - y2) * (x3 - x4);
        if denominator == 0.0 {
            return None;
        }
        let final_x = (a * (x3 - x4) - (x1 - x2) * b) / denominator;
        let final_y = (a * (y3 - y4) - (y1 - y2) * b) / denominator;
        Some((final_x, final_y).into())
    }
}

impl<L, P: PointReqs> Operations<P> for L where L: line::Operations<P> {}
