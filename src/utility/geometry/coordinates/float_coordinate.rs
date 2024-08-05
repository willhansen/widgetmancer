use crate::utility::*;

pub trait Operations: signed_coordinate::Operations<_DataType = f32, Floating = Self> {
    // TODO: Add tolerance?
    fn on_centered_unit_square(&self) -> bool {
        // NOTE: 0.5 can be exactly represented by floating point numbers
        self.king_length() == 0.5
    }
    // TODO: Add tolerance?
    fn on_a_square_face(&self) -> bool {
        any_true(&[0, 1].map(|i| self.on_square_border_on_axis(i)))
    }
    // TODO: Add tolerance?
    fn on_square_border_on_axis(&self, i: usize) -> bool {
        (self.nth_component(i) - 0.5) % 1.0 == 0.0
    }
    // TODO: Add tolerance?
    fn on_same_square_face(&self, other: Self) -> bool {
        HashSet::<OrthogonalFacingIntPose<Self::OnGrid>>::from_iter(self.touched_square_faces())
            .intersection(&HashSet::from_iter(other.touched_square_faces()))
            .count()
            > 0
    }
    // TODO: Add tolerance?
    fn touched_square_faces(&self) -> HashSet<OrthogonalFacingIntPose<Self::OnGrid>> {
        let on_border_by_axis = [0, 1].map(|i| self.on_square_border_on_axis(i));
        match on_border_by_axis {
            [true, true] => [-1, 1]
                .into_iter()
                .cartesian_product([-1, 1])
                .flat_map(|(x_nudge, y_nudge)| {
                    let nudge_vector = Self::OnGrid::new(x_nudge, y_nudge);
                    let offset_point = *self + nudge_vector.to_f32() * 0.1;
                    let square = offset_point.snap_to_grid();
                    [
                        (square, NonZeroSign::try_from(-x_nudge).unwrap() * RIGHT).into(),
                        (square, NonZeroSign::try_from(-y_nudge).unwrap() * UP).into(),
                    ]
                })
                .collect(),
            [false, false] => hash_set![],
            [x_border, y_border] => {
                let border_axis_index = if x_border { 0 } else { 1 };
                let non_border_axis_index = 1 - border_axis_index;
                let normal_to_border = Self::nth_basis_vector(border_axis_index);
                let one_face = Face::from_square_and_dir(
                    (*self + normal_to_border * 0.1).snap_to_grid(),
                    -normal_to_border.nearest_orthogonal_direction(),
                );
                HashSet::from(one_face.both_sides_of_face())
            }
        }
    }
    fn normalize(&self) -> Self {
        *self / self.length()
    }
    fn round(&self) -> Self {
        Self::new(self.x().round(), self.y().round())
    }
    fn from_angle_and_length(angle: Angle<f32>, length: f32) -> Self {
        Self::new(length * angle.radians.cos(), length * angle.radians.sin())
    }

    fn rotate_around_point(&self, axis_point: Self, angle: Angle<f32>) -> Self {
        axis_point + (*self - axis_point).rotate_vect(angle)
    }

    fn unit_vector_from_angle(angle: Angle<f32>) -> Self {
        Self::new(angle.radians.cos(), angle.radians.sin())
    }
    fn rotate_vect(&self, delta_angle: Angle<f32>) -> Self {
        let start_angle = self.better_angle_from_x_axis();
        let new_angle = start_angle + delta_angle;
        Self::from_angle_and_length(new_angle, self.length())
    }
    fn snap_to_grid(&self) -> Self::OnGrid {
        self.round().to_i32()
    }
    fn nearest_orthogonal_direction(&self) -> OrthogonalDirection {
        OrthogonalDirection::from_angle_hint(self.better_angle_from_x_axis())
    }
    fn lerp2d(&self, target: Self, t: f32) -> Self {
        Self::new(lerp(self.x(), target.x(), t), lerp(self.y(), target.y(), t))
    }
    fn angle_to(&self, other: Self) -> Angle<f32> {
        self.better_angle_from_x_axis()
            .angle_to(other.better_angle_from_x_axis())
    }
    fn about_eq(&self, other: Self, tolerance: Self::DataType) -> bool {
        (*self - other).length() < tolerance
    }
    fn check_about_eq(&self, other: Self) -> OkOrMessage {
        let tolerance = 0.001; // TODO: make parameter
        if self.about_eq(other, tolerance) {
            Ok(())
        } else {
            Err(format!(
                "\nPoints too far apart:\n\tp1: {:?}\n\tp2: {:?}\n",
                self, other
            ))
        }
    }
}

// TODO: convert to auto trait when stable
impl<T> Operations for T where T: signed_coordinate::Operations<_DataType = f32, Floating = T> {}
