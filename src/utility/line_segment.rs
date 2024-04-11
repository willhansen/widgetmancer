use rand::{rngs::StdRng, Rng};

use crate::utility::*;

pub trait LineSegmentOps: LineOps {
    fn square_length(&self) -> <Self::PointType as Coordinate>::DataType {
        let [p1, p2] = self.two_different_arbitrary_points_on_line();
        (p1 - p2).square_length()
    }
    fn endpoints_in_arbitrary_order(&self) -> [Self::PointType; 2];
}
impl<P: SignedCoordinate> LineSegmentOps for TwoDifferentPoints<P> {
    fn endpoints_in_arbitrary_order(&self) -> [Self::PointType; 2] {
        [self.p2(), self.p1()] // Order chosen by coin flip
    }
}
pub trait FloatLineSegmentOps: FloatLineOps + LineSegmentOps {
    fn length(&self) -> f32 {
        let [p1, p2] = self.endpoints_in_arbitrary_order();
        (p1 - p2).length()
    }
    fn seeded_random_point_on_line(&self, rng: &mut StdRng) -> Self::PointType {
        let t = rng.gen_range(0.0..=1.0);
        let [p1, p2] = self.endpoints_in_arbitrary_order();
        p1.lerp2d(p2, t)
    }

    fn seeded_random_point_near_line(&self, rng: &mut StdRng, radius: f32) -> Self::PointType {
        // TODO: make more uniform
        self.seeded_random_point_on_line(rng)
            + seeded_rand_radial_offset::<Self::PointType>(rng, radius)
    }

    fn random_point_near_line(&self, radius: f32) -> Self::PointType {
        self.seeded_random_point_near_line(&mut get_new_rng(), radius)
    }

    fn projected_onto_parallel_line_through_origin_with_positive_direction_hint(
        &self,
        positive_direction_hint: FAngle,
    ) -> ClosedInterval<f32> {
        let Some(positive_direction) = self
            .parallel_directions()
            .iter()
            .filter(|&&dir| fangle_dot(dir, positive_direction_hint) > 0.0)
            .next()
        else {
            panic!(
                "direction hint not useful.  hint: {:?}, parallel directions: {:?}",
                positive_direction_hint,
                self.parallel_directions()
            );
        };

        todo!();
    }
    fn projected_onto_parallel_line_through_origin(&self) -> ClosedInterval<f32> {
        let chosen_positive = self.parallel_directions()[0];
        self.projected_onto_parallel_line_through_origin_with_positive_direction_hint(
            chosen_positive,
        )
    }

    fn line_segment_intersection_point(
        &self,
        other: impl LineSegmentOps<PointType = Self::PointType>,
    ) -> Option<Self::PointType> {
        let [a1, a2] = self.endpoints_in_arbitrary_order();
        let [b1, b2] = other.endpoints_in_arbitrary_order();

        // TODO: This is cumbersome.  Should be more concise somehow
        let both_on_same_line = self.point_is_on_line(b1) && self.point_is_on_line(b2);

        if both_on_same_line {
            // Now dealing with one-dimensional intersection.
            // as we only care about a singular intersection point, too much intersection is None

            // TODO: redo this with intervals

            let [a1_1d, a2_1d, b1_1d, b2_1d] = [a1, a2, b1, b2]
                .map(|p| (p - a1).position_on_axis((a2 - a1).better_angle_from_x_axis()));

            let (b_min, b_max) = (
                min_for_partial_ord(b1_1d, b2_1d),
                max_for_partial_ord(b1_1d, b2_1d),
            );
            let (a_min, a_max) = (
                min_for_partial_ord(a1_1d, a2_1d),
                max_for_partial_ord(a1_1d, a2_1d),
            );

            let no_overlap = a_max < b_min || b_max < a_min;

            if no_overlap {
                return None;
            }

            let b1_in_a = b1_1d > a_min && b1_1d < a_max;
            let b2_in_a = b2_1d > a_min && b2_1d < a_max;
            let a1_in_b = a1_1d > b_min && a1_1d < b_max;
            let a2_in_b = a2_1d > b_min && a2_1d < b_max;
            let too_much_overlap = b1_in_a || b2_in_a || a1_in_b || a2_in_b;
            if too_much_overlap {
                return None;
            }

            // Only case left is exact matches.  note that both matching is bad.
            let identical_segments = a_min == b_min && a_max == b_max;
            if identical_segments {
                return None;
            }

            Some(if a1 == b1 || a1 == b2 {
                a1
            } else if a2 == b1 || a2 == b2 {
                a2
            } else {
                panic!(
                    "endpoints do not match: self: {:?}, other: {:?}",
                    &self, &other
                );
            })
        } else {
            let no_intersection = self.same_side_of_line(b1, b2) || other.same_side_of_line(a1, a2);
            if no_intersection {
                None
            } else {
                // intersection confirmed, now just find it.
                self.intersection_point_with_other_extended_line(other)
            }
        }
    }
}
impl<T> FloatLineSegmentOps for T where T: FloatLineOps + LineSegmentOps {}

pub trait DirectedLineSegment: DirectedLineOps + LineSegmentOps {
    fn endpoints_in_order(&self) -> [Self::PointType; 2] {
        Self::PointType::points_sorted_along_axis(
            self.endpoints_in_arbitrary_order(),
            self.direction(),
        )
        .into_iter()
        .collect_vec()
        .try_into()
        .unwrap()
    }
    fn start(&self) -> Self::PointType {
        self.endpoints_in_order()[0]
    }
    fn end(&self) -> Self::PointType {
        self.endpoints_in_order()[1]
    }
}
impl<T> DirectedLineSegment for T where T: DirectedLineOps + LineSegmentOps {}

pub trait DirectedFloatLineSegment: DirectedLineOps + FloatLineSegmentOps {
    fn lerp(&self, t: f32) -> Self::PointType {
        self.start().lerp2d(self.end(), t)
    }
}
impl<T> DirectedFloatLineSegment for T where T: DirectedLineOps + FloatLineSegmentOps {}
#[cfg(test)]
mod tests {

    use super::*;

    #[test]
    fn test_intersection__perpendicular_endpoints_touch() {
        let a = TwoDifferentWorldPoints::from_two_exact_points(point2(5.0, 5.0), point2(10.0, 5.0));
        let b =
            TwoDifferentWorldPoints::from_two_exact_points(point2(10.0, 5.0), point2(10.0, 10.0));
        assert_about_eq_2d(
            a.line_segment_intersection_point(b).unwrap(),
            point2(10.0, 5.0),
        )
    }
    #[test]
    fn test_intersection__easy_orthogonal_hit() {
        assert_about_eq_2d(
            TwoDifferentWorldPoints::new_from_two_ordered_points_on_line(
                point2(0.0, 0.0),
                point2(0.0, 4.0),
            )
            .line_segment_intersection_point(
                TwoDifferentWorldPoints::new_from_two_ordered_points_on_line(
                    point2(-1.0, 1.0),
                    point2(1.0, 1.0),
                ),
            )
            .unwrap(),
            point2(0.0, 1.0),
        )
    }
    #[test]
    fn test_intersection__diagonal_intersection() {
        assert_about_eq_2d(
            TwoDifferentWorldPoints::new_from_two_ordered_points_on_line(
                point2(0.0, 0.0),
                point2(1.0, 1.0),
            )
            .line_segment_intersection_point(
                TwoDifferentWorldPoints::new_from_two_ordered_points_on_line(
                    point2(1.0, 0.0),
                    point2(0.0, 1.0),
                ),
            )
            .unwrap(),
            point2(0.5, 0.5),
        )
    }
    #[test]
    fn test_intersection__miss() {
        assert!(
            TwoDifferentWorldPoints::new_from_two_unordered_points_on_line(
                point2(0.0, 0.0),
                point2(1.0, 1.0)
            )
            .line_segment_intersection_point(
                TwoDifferentWorldPoints::new_from_two_unordered_points_on_line(
                    point2(100.0, 1000.0),
                    point2(10.0, 10.0),
                )
            )
            .is_none()
        )
    }
    #[test]
    fn test_intersection__endpoint_touch_mid_counts() {
        assert_about_eq_2d(
            TwoDifferentWorldPoints::new_from_two_ordered_points_on_line(
                point2(5.0, 5.0),
                point2(7.0, 5.0),
            )
            .line_segment_intersection_point(
                TwoDifferentWorldPoints::new_from_two_ordered_points_on_line(
                    point2(5.5, 5.0),
                    point2(10.0, 10.0),
                ),
            )
            .unwrap(),
            point2(5.5, 5.0),
        )
    }

    #[test]
    fn test_intersection__parallel_endpoints_touch() {
        let line1 =
            TwoDifferentWorldPoints::from_two_exact_points(point2(5.0, 5.0), point2(10.0, 5.0));
        let line2 =
            TwoDifferentWorldPoints::from_two_exact_points(point2(10.0, 5.0), point2(20.0, 5.0));
        assert_about_eq_2d(
            line1.line_segment_intersection_point(line2).unwrap(),
            point2(10.0, 5.0),
        );
        assert_about_eq_2d(
            line1
                .reversed()
                .line_segment_intersection_point(line2)
                .unwrap(),
            point2(10.0, 5.0),
        );
        assert_about_eq_2d(
            line1
                .line_segment_intersection_point(line2.reversed())
                .unwrap(),
            point2(10.0, 5.0),
        );
        assert_about_eq_2d(
            line1
                .reversed()
                .line_segment_intersection_point(line2.reversed())
                .unwrap(),
            point2(10.0, 5.0),
        );
    }
    #[test]
    fn test_intersection__parallel_miss() {
        assert!(
            TwoDifferentWorldPoints::new_from_two_unordered_points_on_line(
                point2(5.0, 5.0),
                point2(10.0, 5.0)
            )
            .line_segment_intersection_point(
                TwoDifferentWorldPoints::new_from_two_unordered_points_on_line(
                    point2(11.0, 5.0),
                    point2(20.0, 5.0),
                )
            )
            .is_none(),
        )
    }
    #[test]
    fn test_intersection__parallel_overlap_does_not_count() {
        assert!(
            TwoDifferentWorldPoints::new_from_two_unordered_points_on_line(
                point2(5.0, 5.0),
                point2(10.0, 5.0)
            )
            .line_segment_intersection_point(
                TwoDifferentWorldPoints::new_from_two_unordered_points_on_line(
                    point2(9.0, 5.0),
                    point2(20.0, 5.0),
                )
            )
            .is_none(),
        )
    }
    #[test]
    fn test_intersection__parallel_full_overlap_does_not_count() {
        assert!(
            TwoDifferentWorldPoints::new_from_two_unordered_points_on_line(
                point2(5.0, 5.0),
                point2(10.0, 5.0)
            )
            .line_segment_intersection_point(
                TwoDifferentWorldPoints::new_from_two_unordered_points_on_line(
                    point2(0.0, 5.0),
                    point2(20.0, 5.0),
                )
            )
            .is_none(),
        )
    }
    #[test]
    fn test_intersection__parallel_exact_overlap_does_not_count() {
        assert!(
            TwoDifferentWorldPoints::new_from_two_unordered_points_on_line(
                point2(5.0, 5.0),
                point2(10.0, 5.0)
            )
            .line_segment_intersection_point(
                TwoDifferentWorldPoints::new_from_two_unordered_points_on_line(
                    point2(5.0, 5.0),
                    point2(10.0, 5.0),
                )
            )
            .is_none(),
        )
    }
}
