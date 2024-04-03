use rand::{rngs::StdRng, Rng};

use crate::utility::*;

pub trait LineSegment: LineLike {
    fn square_length(&self) -> <Self::PointType as Coordinate>::DataType {
        let [p1, p2] = self.two_different_arbitrary_points_on_line();
        (p1 - p2).square_length()
    }
    fn endpoints_in_arbitrary_order(&self) -> [Self::PointType; 2];
}
impl<P: SignedCoordinate> LineSegment for TwoDifferentPoints<P> {
    fn endpoints_in_arbitrary_order(&self) -> [Self::PointType; 2] {
        [self.p2(), self.p1()] // Order chosen by coin flip
    }
}
pub trait FloatLineSegment: FloatLineLike + LineSegment {
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

    fn projected_onto_parallel_line_through_origin_with_positive_direction_hint(&self, positive_direction_hint: FAngle) -> ClosedInterval<f32> {
        let Some(positive_direction) = self.parallel_directions().iter().filter(|&&dir| fangle_dot(dir, positive_direction_hint) > 0.0).next() else {
            panic!("direction hint not useful.  hint: {:?}, parallel directions: {:?}", positive_direction_hint, self.parallel_directions());
        };

        
        todo!();
    }
    fn projected_onto_parallel_line_through_origin(&self) -> ClosedInterval<f32> {
        let chosen_positive = self.parallel_directions()[0];
        self.projected_onto_parallel_line_through_origin_with_positive_direction_hint(chosen_positive)
    }

    fn intersection_point_with_other_line_segment(
        &self,
        other: impl LineSegment<PointType = Self::PointType>,
    ) -> Option<Self::PointType> {
        let [a1, a2] = self.endpoints_in_arbitrary_order();
        let [b1, b2] = other.endpoints_in_arbitrary_order();

        // TODO: This is cumbersome.  Should be more concise somehow
        let self_extended_line: TwoDifferentPoints<Self::PointType> = self.to_line();
        let both_on_same_line =
            self_extended_line.point_is_on_line(b1) && self_extended_line.point_is_on_line(b2);

        if both_on_same_line {
            // Now dealing with one-dimensional intersection.
            // as we only care about a singular intersection point, too much intersection is None

            // TODO: redo this with intervals

            let [a1_1d, a2_1d, b1_1d, b2_1d] = [a1, a2, b1, b2]
                .map(|p| (p - a1).position_on_axis((a2 - a1).better_angle_from_x_axis()));

            let (b_min, b_max) = (b1_1d.min(b2_1d), b1_1d.max(b2_1d));
            let (a_min, a_max) = (a1_1d.min(a2_1d), a1_1d.max(a2_1d));

            let no_overlap = a_max < b_min || b_max < a_min;

            if no_overlap {
                return None;
            }

            let too_much_overlap = b1_1d > a_min && b1_1d < a_max || 

            if a2 == b1 && on_line_in_this_order(a1, a2, b2) {
                Some(a2)
            } else if a2 == b2 && on_line_in_this_order(a1, a2, b1) {
                Some(a2)
            } else if a1 == b1 && on_line_in_this_order(a2, a1, b2) {
                Some(a1)
            } else if a1 == b2 && on_line_in_this_order(a2, a1, b1) {
                Some(a1)
            } else {
                None
            }
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
impl<T> FloatLineSegment for T where T: FloatLineLike + LineSegment {}

pub trait DirectedLineSegment: DirectedLineLike + LineSegment {
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
impl<T> DirectedLineSegment for T where T: DirectedLineLike + LineSegment {}

pub trait DirectedFloatLineSegment: DirectedLineLike + FloatLineSegment {
    fn lerp(&self, t: f32) -> Self::PointType {
        self.start().lerp2d(self.end(), t)
    }
}
impl<T> DirectedFloatLineSegment for T where T: DirectedLineLike + FloatLineSegment {}
