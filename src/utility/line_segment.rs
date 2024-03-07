use crate::Coordinate;

pub trait LineSegment {
    type PointType: Coordinate;
    fn square_length(&self) -> <Self::PointType as Coordinate>::DataType {
        let [p1, p2] = self.two_different_arbitrary_points_on_line();
        (p1 - p2).square_length()
    }
    fn endpoints_in_arbitrary_order(&self) -> [Self::PointType; 2];
}
impl<P: Coordinate> LineSegment for TwoDifferentPoints<P> {
    fn endpoints_in_arbitrary_order(&self) -> [Self::PointType; 2] {
        [self.p2, self.p1] // Order chosen by coin flip
    }
}
pub trait FloatLineSegment: FloatLineTrait + LineSegment {
    fn length(&self) -> f32 {
        let [p1, p2] = self.two_different_arbitrary_points_on_line();
        (p1 - p2).length()
    }
    fn seeded_random_point_on_line(&self, rng: &mut StdRng) -> Self::PointType {
        let t = rng.gen_range(0.0..=1.0);
        self.lerp(t)
    }

    fn seeded_random_point_near_line(&self, rng: &mut StdRng, radius: f32) -> Self::PointType {
        // TODO: make more uniform
        self.seeded_random_point_on_line(rng) + seeded_rand_radial_offset(rng, radius).cast_unit()
    }

    fn random_point_near_line(&self, radius: f32) -> Self::PointType {
        self.seeded_random_point_near_line(&mut get_new_rng(), radius)
    }
}
impl<T> FloatLineSegment for T where T: FloatLineTrait + LineSegment {}

pub trait DirectedLineSegment: DirectedLineTrait + LineSegment {}
impl<T> DirectedLineSegment for T where T: DirectedLineTrait + LineSegment {}

pub trait DirectedFloatLineSegment: DirectedLineTrait + FloatLineSegment {
    fn lerp(&self, t: f32) -> Self::PointType {
        self.p1().lerp2d(self.p2(), t)
    }
}
impl<T> DirectedFloatLineSegment for T where T: DirectedLineTrait + FloatLineSegment {}
