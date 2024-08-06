use crate::utility::*;

pub trait Operations: int_coordinate::Operations {
    fn square_corners(&self) -> [Self::Floating; 4] {
        Self::Floating::new(0.5, 0.5)
            .quadrant_rotations_going_ccw()
            .map(|p| self.square_center() + p)
    }
    fn square_center(&self) -> Self::Floating {
        self.to_f32()
    }
    fn projected_onto_axis(
        &self,
        axis: FAngle,
    ) -> ClosedInterval<<Self::Floating as coordinate::Operations>::DataType> {
        let all_square_corners_on_axis: [f32; 4] =
            self.square_corners().map(|p| p.position_on_axis(axis));

        ClosedInterval::new(
            all_square_corners_on_axis
                .iter()
                .min_by_key(|&&x| OrderedFloat(x))
                .unwrap()
                .clone(),
            all_square_corners_on_axis
                .iter()
                .max_by_key(|&&x| OrderedFloat(x))
                .unwrap()
                .clone(),
        )
    }
}

impl<T: int_coordinate::Operations> grid_square::Operations for T {}
