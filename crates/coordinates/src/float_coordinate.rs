use map_macro::hash_set;
use misc_utilities::general_utility::any_true;
use std::collections::HashSet;
use angles::*;
use crate::*;
use misc_utilities::general_utility::*;
use signed_coordinate::Operations as SignedCoordinateOperations;
use rand::{rngs::StdRng, Rng};

pub type FCoord = Coord<f32>;

// use crate::utility::*;

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
    fn normalize(&self) -> Self {
        *self / self.length()
    }
    fn round(&self) -> Self {
        Self::new(self.x().round(), self.y().round())
    }
    fn from_angle_and_length(angle: FAngle, length: f32) -> Self {
        Self::new(length * angle.radians.cos(), length * angle.radians.sin())
    }

    fn rotate_around_point(&self, axis_point: Self, angle: FAngle) -> Self {
        axis_point + (*self - axis_point).rotate_vect(angle)
    }

    fn unit_vector_from_angle(angle: FAngle) -> Self {
        Self::new(angle.radians.cos(), angle.radians.sin())
    }
    fn rotate_vect(&self, delta_angle: FAngle) -> Self {
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
    fn angle_to(&self, other: Self) -> FAngle {
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

#[deprecated(note = "coordinates::king_length instead")]
pub fn king_move_distance(step: FCoord) -> f32 {
    step.x().abs().max(step.y().abs())
}

pub fn seeded_rand_radial_offset<P: Operations>(rng: &mut StdRng, radius: f32) -> P {
    let mut v = P::new(10.0, 10.0);
    while v.square_length() > 1.0 {
        v = P::new(rng.gen_range(-1.0..=1.0), rng.gen_range(-1.0..=1.0));
    }
    v * radius
}

pub fn rand_radial_offset(radius: f32) -> FCoord {
    seeded_rand_radial_offset(&mut get_new_rng(), radius)
}

pub fn random_unit_vector() -> FCoord {
    let angle = random_angle();
    FCoord::unit_vector_from_angle(angle)
}
pub fn assert_about_eq_2d<P: float_coordinate::Operations>(p1: P, p2: P) {
    p1.check_about_eq(p2).unwrap();
}
pub fn furthest_apart_points<P: float_coordinate::Operations>(points: Vec<P>) -> [P; 2] {
    assert!(points.len() >= 2);
    let furthest = points
        .iter()
        .combinations(2)
        .max_by_key(|two_points: &Vec<&P>| OrderedFloat((*two_points[0] - *two_points[1]).length()))
        .unwrap();
    let furthest_values: Vec<P> = furthest.into_iter().copied().collect();
    furthest_values.try_into().unwrap()
}
pub fn two_sorted_going_ccw(v: [WorldMove; 2]) -> [WorldMove; 2] {
    if two_points_are_ccw_with_origin(v[0], v[1]) {
        v
    } else {
        [v[1], v[0]]
    }
}
