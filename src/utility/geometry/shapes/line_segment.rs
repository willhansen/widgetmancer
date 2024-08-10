use rand::{rngs::StdRng, Rng};

use crate::utility::*;

trait_alias!(pub trait PointReqs = signed_coordinate::Operations);

pub trait Operations<P: PointReqs>: line::Operations<P> {
    fn square_length(&self) -> <P as coordinates::Operations>::DataType {
        let [p1, p2] = self.two_different_arbitrary_points_on_line();
        (p1 - p2).square_length()
    }
    fn endpoints_in_arbitrary_order(&self) -> [P; 2];
}
impl<P: PointReqs> Operations<P> for TwoDifferentPoints<P> {
    fn endpoints_in_arbitrary_order(&self) -> [P; 2] {
        [self.p2(), self.p1()] // Order chosen by coin flip
    }
}

#[cfg(test)]
mod tests {

    use super::*;

    #[test]
    fn test_intersection__perpendicular_endpoints_touch() {
        let a = TwoDifferentWorldPoints::from_two_points(point2(5.0, 5.0), point2(10.0, 5.0));
        let b = TwoDifferentWorldPoints::from_two_points(point2(10.0, 5.0), point2(10.0, 10.0));
        assert_about_eq_2d(
            a.line_segment_intersection_point(b).unwrap(),
            point2(10.0, 5.0),
        )
    }
    #[test]
    fn test_intersection__easy_orthogonal_hit() {
        assert_about_eq_2d(
            TwoDifferentWorldPoints::from_two_ordered_points_on_line(
                point2(0.0, 0.0),
                point2(0.0, 4.0),
            )
            .line_segment_intersection_point(
                TwoDifferentWorldPoints::from_two_ordered_points_on_line(
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
            TwoDifferentWorldPoints::from_two_ordered_points_on_line(
                point2(0.0, 0.0),
                point2(1.0, 1.0),
            )
            .line_segment_intersection_point(
                TwoDifferentWorldPoints::from_two_ordered_points_on_line(
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
        assert!(TwoDifferentWorldPoints::from_two_unordered_points_on_line(
            point2(0.0, 0.0),
            point2(1.0, 1.0)
        )
        .line_segment_intersection_point(
            TwoDifferentWorldPoints::from_two_unordered_points_on_line(
                point2(100.0, 1000.0),
                point2(10.0, 10.0),
            )
        )
        .is_none())
    }
    #[test]
    fn test_intersection__endpoint_touch_mid_counts() {
        assert_about_eq_2d(
            TwoDifferentWorldPoints::from_two_ordered_points_on_line(
                point2(5.0, 5.0),
                point2(7.0, 5.0),
            )
            .line_segment_intersection_point(
                TwoDifferentWorldPoints::from_two_ordered_points_on_line(
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
        let line1 = TwoDifferentWorldPoints::from_two_points(point2(5.0, 5.0), point2(10.0, 5.0));
        let line2 = TwoDifferentWorldPoints::from_two_points(point2(10.0, 5.0), point2(20.0, 5.0));
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
        assert!(TwoDifferentWorldPoints::from_two_unordered_points_on_line(
            point2(5.0, 5.0),
            point2(10.0, 5.0)
        )
        .line_segment_intersection_point(
            TwoDifferentWorldPoints::from_two_unordered_points_on_line(
                point2(11.0, 5.0),
                point2(20.0, 5.0),
            )
        )
        .is_none(),)
    }
    #[test]
    fn test_intersection__parallel_overlap_does_not_count() {
        assert!(TwoDifferentWorldPoints::from_two_unordered_points_on_line(
            point2(5.0, 5.0),
            point2(10.0, 5.0)
        )
        .line_segment_intersection_point(
            TwoDifferentWorldPoints::from_two_unordered_points_on_line(
                point2(9.0, 5.0),
                point2(20.0, 5.0),
            )
        )
        .is_none(),)
    }
    #[test]
    fn test_intersection__parallel_full_overlap_does_not_count() {
        assert!(TwoDifferentWorldPoints::from_two_unordered_points_on_line(
            point2(5.0, 5.0),
            point2(10.0, 5.0)
        )
        .line_segment_intersection_point(
            TwoDifferentWorldPoints::from_two_unordered_points_on_line(
                point2(0.0, 5.0),
                point2(20.0, 5.0),
            )
        )
        .is_none(),)
    }
    #[test]
    fn test_intersection__parallel_exact_overlap_does_not_count() {
        assert!(TwoDifferentWorldPoints::from_two_unordered_points_on_line(
            point2(5.0, 5.0),
            point2(10.0, 5.0)
        )
        .line_segment_intersection_point(
            TwoDifferentWorldPoints::from_two_unordered_points_on_line(
                point2(5.0, 5.0),
                point2(10.0, 5.0),
            )
        )
        .is_none(),)
    }
}
