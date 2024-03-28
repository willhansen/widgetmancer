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

    // TODO: make parameter impl LineSegment
    fn intersection_point_with_other_line_segment(&self, other: &Self) -> Option<Self::PointType> {
        let [self_p1, self_p2] = self.two_different_arbitrary_points_on_line();
        let [other_p1, other_p2] = other.two_different_arbitrary_points_on_line();

        if self.same_side_of_line(other_p1, other_p2) || other.same_side_of_line(self_p1, self_p2) {
            let on_same_line = self
                .to_line::<TwoDifferentPoints<_>>()
                .point_is_on_line(other_p1);
            if !on_same_line {
                return None;
            }
            return if self_p2 == other_p1 && on_line_in_this_order(self_p1, self_p2, other_p2) {
                Some(self_p2)
            } else if self_p2 == other_p2 && on_line_in_this_order(self_p1, self_p2, other_p1) {
                Some(self_p2)
            } else if self_p1 == other_p1 && on_line_in_this_order(self_p2, self_p1, other_p2) {
                Some(self_p1)
            } else if self_p1 == other_p2 && on_line_in_this_order(self_p2, self_p1, other_p1) {
                Some(self_p1)
            } else {
                None
            };
        }
        // from here, we know the line segments are overlapping, including the case of exactly touching
        // A simple line intersection check is all that's left

        self.intersection_point_with_other_extended_line(&other)
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
