pub type IPoint = default::Point2D<i32>;
pub type FPoint = default::Point2D<f32>;
pub type IVector = default::Vector2D<i32>;
pub type FVector = default::Vector2D<f32>;

pub fn sign2d<U>(point: Point2D<f32, U>) -> Point2D<f32, U> {
    point2(sign(point.x), sign(point.y))
}

pub fn fraction_part<U>(point: Point2D<f32, U>) -> Point2D<f32, U> {
    (point - point.round()).to_point()
}

pub fn rotated_n_quarter_turns_counter_clockwise<T: Signed + Copy, U>(
    v: Vector2D<T, U>,
    quarter_turns: i32,
) -> Vector2D<T, U> {
    vec2(
        v.x * int_to_T(int_cos(quarter_turns)) - v.y * int_to_T(int_sin(quarter_turns)),
        v.x * int_to_T(int_sin(quarter_turns)) + v.y * int_to_T(int_cos(quarter_turns)),
    )
}
pub fn point_rotated_n_quarter_turns_counter_clockwise<T: Signed + Copy, U>(
    p: Point2D<T, U>,
    quarter_turns: i32,
) -> Point2D<T, U> {
    rotated_n_quarter_turns_counter_clockwise(p.to_vector(), quarter_turns).to_point()
}
pub fn get_4_rotations_of<T: Signed + Copy, U>(v: Vector2D<T, U>) -> Vec<Vector2D<T, U>> {
    (0..4)
        .map(|i| rotated_n_quarter_turns_counter_clockwise(v, i))
        .collect()
}

pub fn get_8_octant_transforms_of<T: Signed + Copy, U>(v: Vector2D<T, U>) -> Vec<Vector2D<T, U>> {
    let transpose = Vector2D::<T, U>::new(v.y, v.x);
    vec![v, transpose]
        .into_iter()
        .map(get_4_rotations_of)
        .flatten()
        .collect()
}

pub trait CoordToString {
    fn to_string(&self) -> String;
}
impl<T: Display, U> CoordToString for Point2D<T, U> {
    fn to_string(&self) -> String {
        format!("(x: {}, y: {})", self.x, self.y)
    }
}

impl<T: Display, U> CoordToString for Vector2D<T, U> {
    fn to_string(&self) -> String {
        format!("(dx: {}, dy: {})", self.x, self.y)
    }
}

impl<T: Signed + Copy, U> QuarterTurnRotatable for Point2D<T, U> {
    fn rotated(&self, quarter_turns_anticlockwise: QuarterTurnsAnticlockwise) -> Self {
        point_rotated_n_quarter_turns_counter_clockwise(
            *self,
            quarter_turns_anticlockwise.quarter_turns,
        )
    }
}

impl<T: Signed + Copy, U> QuarterTurnRotatable for Vector2D<T, U> {
    fn rotated(&self, quarter_turns_anticlockwise: QuarterTurnsAnticlockwise) -> Self {
        rotated_n_quarter_turns_counter_clockwise(*self, quarter_turns_anticlockwise.quarter_turns)
    }
}
pub fn reversed<T: Copy>(v: Vec<T>) -> Vec<T> {
    let mut new_v = v.clone();
    new_v.reverse();
    new_v
}
// TODO: remove use of specific world square units
pub fn king_distance(step: WorldStep) -> u32 {
    step.x.abs().max(step.y.abs()) as u32
}

pub fn round_to_king_step(step: WorldStep) -> WorldStep {
    if step.square_length() == 0 {
        return step;
    }
    let radians_from_plus_x = better_angle_from_x_axis(step.to_f32());
    let eighth_steps_from_plus_x = (radians_from_plus_x.radians * 8.0 / TAU).round();
    let rounded_radians_from_plus_x = Angle::radians(eighth_steps_from_plus_x * TAU / 8.0);

    let float_step = Vector2D::<f32, SquareGridInWorldFrame>::from_angle_and_length(
        rounded_radians_from_plus_x,
        1.5,
    );
    // 1.5 length to allow truncating down to 1 i32 in the diagonal case
    // because 1.5/sqrt(2) > 1.0

    // truncate towards zero intentionally
    float_step.to_i32()
}

pub fn is_king_step(step: WorldStep) -> bool {
    is_orthogonal_king_step(step) || is_diagonal_king_step(step)
}

pub fn is_orthogonal_king_step(step: WorldStep) -> bool {
    step.square_length() == 1
}

pub fn is_diagonal_king_step(step: WorldStep) -> bool {
    step.square_length() == 2
}

pub fn is_orthogonal<T: Signed, U>(v: Vector2D<T, U>) -> bool {
    v.x == T::zero() || v.y == T::zero()
}

pub fn is_diagonal<T: Signed, U>(v: Vector2D<T, U>) -> bool {
    v.x == v.y || v.x == v.y.neg()
}

pub fn is_orthodiagonal<T: Signed + Copy, U>(v: Vector2D<T, U>) -> bool {
    is_orthogonal(v) || is_diagonal(v)
}

pub fn seeded_rand_radial_offset(rng: &mut StdRng, radius: f32) -> default::Vector2D<f32> {
    let mut v = vec2(10.0, 10.0);
    while v.square_length() > 1.0 {
        v.x = rng.gen_range(-1.0..=1.0);
        v.y = rng.gen_range(-1.0..=1.0);
    }
    v * radius
}

pub fn rand_radial_offset(radius: f32) -> default::Vector2D<f32> {
    seeded_rand_radial_offset(&mut get_new_rng(), radius)
}
pub fn random_unit_vector() -> FVector {
    let angle = random_angle();
    unit_vector_from_angle(angle)
}

pub fn unit_vector_from_angle(angle: Angle<f32>) -> FVector {
    vec2(angle.radians.cos(), angle.radians.sin())
}

#[cfg(test)]
mod tests {

    use ntest::{assert_about_eq, assert_false, assert_true, timeout};
    use pretty_assertions::{assert_eq, assert_ne};

    use super::*;
}
