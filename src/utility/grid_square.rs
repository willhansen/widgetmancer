use crate::utility::*;
use interval::Interval;

pub trait GridSquare: IntCoordinate {
    fn square_corners(&self) -> [Self::Floating; 4] {
        Self::Floating::new(0.5, 0.5)
            .quadrant_rotations_going_ccw()
            .map(|p| self.square_center() + p)
    }
    fn square_center(&self) -> Self::Floating {
        self.to_f32()
    }
    fn projected_onto_axis(&self, axis: FAngle) -> Interval<Self::Floating> {
        todo!()
    }
}

impl<T: IntCoordinate> GridSquare for T {}
