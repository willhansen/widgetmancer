extern crate num;

#[feature(unboxed_closures)]
use std::collections::{HashMap, HashSet};
use std::f32::consts::{PI, TAU};
use std::fmt::{Debug, Display, Formatter};
use std::hash::Hash;
use std::iter::zip;
use std::mem;
use std::ops::{Add, AddAssign, Neg, Sub};
use std::string::ToString;

use ambassador::delegatable_trait;

use approx::AbsDiffEq;
use derive_more::{AddAssign, Constructor, Display, Neg};
use euclid::approxeq::ApproxEq;
use euclid::num::Zero;
use euclid::*;
use getset::CopyGetters;
use itertools::Itertools;
use line_drawing::{Bresenham, Point, Supercover};
use ntest::about_eq;
use num::traits::real::Real;
use num::traits::{Euclid, Signed};
use ordered_float::OrderedFloat;
use rand::rngs::StdRng;
use rand::{Rng, SeedableRng};
use rgb::RGB8;

use crate::glyph::glyph_constants::{
    BLUE, CYAN, FACE_ARROWS, GREEN, GREY, MAGENTA, RED, THIN_TRIANGLE_ARROWS, YELLOW,
};
use crate::piece::PieceType::King;
use crate::{DoubleGlyph, Glyph};

pub mod angle_interval;
pub mod coordinate_frame_conversions;
pub mod coordinates;
pub mod halfplane;
pub mod line;
pub mod octant;
pub mod poses;
pub mod quadrant;
pub mod round_robin_iterator;

pub use self::angle_interval::*;
pub use self::coordinate_frame_conversions::*;
pub use self::coordinates::*;
pub use self::halfplane::*;
pub use self::line::*;
pub use self::octant::*;
pub use self::poses::*;
pub use self::quadrant::*;
pub use self::round_robin_iterator::*;

pub type SimpleResult = Result<(), ()>;

pub const STEP_ZERO: WorldStep = vec2(0, 0);
pub const STEP_UP: WorldStep = vec2(0, 1);
pub const STEP_DOWN: WorldStep = vec2(0, -1);
pub const STEP_RIGHT: WorldStep = vec2(1, 0);
pub const STEP_LEFT: WorldStep = vec2(-1, 0);

pub const STEP_UP_RIGHT: WorldStep = vec2(1, 1);
pub const STEP_UP_LEFT: WorldStep = vec2(-1, 1);
pub const STEP_DOWN_LEFT: WorldStep = vec2(-1, -1);
pub const STEP_DOWN_RIGHT: WorldStep = vec2(1, -1);

pub const ORTHOGONAL_STEPS: [WorldStep; 4] = [STEP_UP, STEP_DOWN, STEP_RIGHT, STEP_LEFT];
pub const DIAGONAL_STEPS: [WorldStep; 4] =
    [STEP_UP_RIGHT, STEP_UP_LEFT, STEP_DOWN_RIGHT, STEP_DOWN_LEFT];
pub const KING_STEPS: [WorldStep; 8] = [
    STEP_UP,
    STEP_DOWN,
    STEP_RIGHT,
    STEP_LEFT,
    STEP_UP_RIGHT,
    STEP_UP_LEFT,
    STEP_DOWN_RIGHT,
    STEP_DOWN_LEFT,
];

pub type WorldLine = Line<f32, SquareGridInWorldFrame>;
pub type WorldSquareLine = Line<i32, SquareGridInWorldFrame>;
pub type LocalCharacterLine = Line<f32, CharacterGridInLocalCharacterFrame>;

pub fn sign(x: f32) -> f32 {
    if x < 0.0 {
        -1.0
    } else if x > 0.0 {
        1.0
    } else {
        0.0
    }
}

pub fn get_by_point<T, U>(grid: &Vec<Vec<T>>, p: Point2D<i32, U>) -> &T {
    &grid[p.x as usize][p.y as usize]
}

pub fn int_to_T<T: Signed>(x: i32) -> T {
    match x {
        1 => T::one(),
        0 => T::zero(),
        -1 => -T::one(),
        _ => panic!(),
    }
}

pub fn int_cos(quarter_periods: i32) -> i32 {
    match quarter_periods.rem_euclid(4) {
        0 => 1,
        1 | 3 => 0,
        2 => -1,
        _ => panic!(),
    }
}

pub fn int_sin(quarter_periods: i32) -> i32 {
    match quarter_periods.rem_euclid(4) {
        0 | 2 => 0,
        1 => 1,
        3 => -1,
        _ => panic!(),
    }
}

pub fn get_new_rng() -> StdRng {
    StdRng::from_rng(rand::thread_rng()).unwrap()
}

pub fn random_event(p: f32) -> bool {
    assert!(p >= 0.0 && p <= 1.0);
    rand::thread_rng().gen_range(0.0..=1.0) < p
}

pub fn random_angle() -> Angle<f32> {
    Angle::degrees(rand::thread_rng().gen_range(0.0..360.0))
}

pub fn random_choice<'a, T>(rng: &'a mut StdRng, v: &'a Vec<T>) -> &'a T {
    v.get(rng.gen_range(0..v.len())).unwrap()
}

pub fn number_to_color(i: u32) -> RGB8 {
    let in_order = vec![GREY, RED, BLUE, GREEN, YELLOW, CYAN, MAGENTA];
    in_order[i as usize % in_order.len()]
}

pub fn lerp(a: f32, b: f32, t: f32) -> f32 {
    a * (1.0 - t) + b * t
}

pub fn derivative(f: fn(f32) -> f32, x: f32, dx: f32) -> f32 {
    if dx == 0.0 {
        panic!("approximate derivatives only!");
    }
    (f(x + dx / 2.0) - f(x - dx / 2.0)) / dx
}

#[deprecated(note = "worldcharactersquareglyphmap is bad")]
pub fn pair_up_character_square_map<T: Clone>(
    character_glyph_map: HashMap<WorldCharacterSquare, T>,
    default_filler: T,
) -> HashMap<WorldSquare, [T; 2]> {
    let mut output_map = HashMap::<WorldSquare, [T; 2]>::new();
    character_glyph_map
        .into_iter()
        .for_each(|(character_square, value)| {
            let world_square = world_character_square_to_world_square(character_square);
            let is_left_value =
                is_world_character_square_left_square_of_world_square(character_square);
            let position_index = if is_left_value { 0 } else { 1 };

            if output_map.contains_key(&world_square) {
                let mut existing_double_value = output_map.get_mut(&world_square).unwrap();
                existing_double_value[position_index] = value;
            } else {
                let mut new_double_value = [default_filler.clone(), default_filler.clone()];
                new_double_value[position_index] = value;
                output_map.insert(world_square, new_double_value);
            }
        });
    output_map
}

pub fn glyph_map_to_string(glyph_map: &WorldCharacterSquareGlyphMap) -> String {
    let top_row = glyph_map.keys().map(|square| square.y).max().unwrap();
    let bottom_row = glyph_map.keys().map(|square| square.y).min().unwrap();
    let left_column = glyph_map.keys().map(|square| square.x).min().unwrap();
    let right_column = glyph_map.keys().map(|square| square.x).max().unwrap();
    let mut string = String::new();
    for bottom_to_top_y in bottom_row..=top_row {
        let y = top_row + bottom_row - bottom_to_top_y;
        for x in left_column..=right_column {
            let square = WorldCharacterSquare::new(x, y);
            let new_part = if let Some(glyph) = glyph_map.get(&square) {
                glyph.to_string()
            } else {
                " ".to_string()
            };

            string += &new_part;
        }
        string += "\n";
    }
    string
}

pub fn map_sum<K, V>(a: HashMap<K, V>, b: HashMap<K, V>) -> HashMap<K, V>
where
    K: Eq + Hash,
    V: Default + std::ops::AddAssign,
{
    let mut sum = HashMap::new();
    for m in [a, b] {
        m.into_iter().for_each(|(k, v): (K, V)| {
            *sum.entry(k).or_default() += v;
        });
    }
    sum
}

pub fn map_neg<K, V>(m: HashMap<K, V>) -> HashMap<K, V>
where
    K: Eq + Hash,
    V: Neg<Output = V>,
{
    m.into_iter().map(|(k, v): (K, V)| (k, -v)).collect()
}

pub fn map_to_signed<K>(m: HashMap<K, u32>) -> HashMap<K, i32>
where
    K: Eq + Hash,
{
    m.into_iter()
        .map(|(k, v): (K, u32)| (k, v as i32))
        .collect()
}

pub fn map_to_float<K>(m: HashMap<K, i32>) -> HashMap<K, f32>
where
    K: Eq + Hash,
{
    m.into_iter()
        .map(|(k, v): (K, i32)| (k, v as f32))
        .collect()
}

pub fn hue_to_rgb(hue_360: f32) -> RGB8 {
    let hue = hue_360.rem_euclid(360.0);
    let x = 1.0 - ((hue / 60.0) % 2.0 - 1.0).abs();
    let float_rgb: (f32, f32, f32) = if hue < 60.0 {
        (1.0, x, 0.0)
    } else if hue < 120.0 {
        (x, 1.0, 0.0)
    } else if hue < 180.0 {
        (0.0, 1.0, x)
    } else if hue < 240.0 {
        (0.0, x, 1.0)
    } else if hue < 300.0 {
        (x, 0.0, 1.0)
    } else {
        (1.0, 0.0, x)
    };
    RGB8::new(
        (float_rgb.0 * 255.0) as u8,
        (float_rgb.1 * 255.0) as u8,
        (float_rgb.2 * 255.0) as u8,
    )
}

pub fn set_of_keys<K, V>(hashmap: &HashMap<K, V>) -> HashSet<K>
where
    K: Clone + Hash + Eq,
{
    hashmap.keys().cloned().collect::<HashSet<K>>()
}

pub fn union<T: Clone + Hash + Eq>(a: &HashSet<T>, b: &HashSet<T>) -> HashSet<T> {
    a.union(b).cloned().collect()
}

pub fn intersection<T: Clone + Hash + Eq>(a: &HashSet<T>, b: &HashSet<T>) -> HashSet<T> {
    a.intersection(b).cloned().collect()
}

pub fn rgb_to_string(rgb: RGB8) -> String {
    format!("( {:>3}, {:>3}, {:>3} )", rgb.r, rgb.g, rgb.b)
}

// TODO: turn into iter
pub fn squares_on_board(size: BoardSize) -> SquareSet {
    (0..size.width)
        .cartesian_product((0..size.height).into_iter())
        .map(|(x, y)| WorldSquare::new(x as i32, y as i32))
        .collect()
}

pub fn tint_color(original_color: RGB8, tint_color: RGB8, mut tint_strength: f32) -> RGB8 {
    // basically just lerp for now, I guess
    tint_strength = tint_strength.clamp(0.0, 1.0);
    let c1: [u8; 3] = original_color.into();
    let c2: [u8; 3] = tint_color.into();
    (0..3)
        .map(|i| lerp(c1[i] as f32, c2[i] as f32, tint_strength).round() as u8)
        .collect_tuple::<(u8, u8, u8)>()
        .unwrap()
        .into()
}

pub fn number_to_hue_rotation(x: f32, period: f32) -> RGB8 {
    hue_to_rgb(x * 360.0 / period)
}

pub fn snap_to_nths(x: f32, denominator: u32) -> f32 {
    (x * denominator as f32).round() / denominator as f32
}
pub fn looping_clamp(a: f32, b: f32, x: f32) -> f32 {
    assert!(a < b);
    ((x - a).rem_euclid(b - a)) + a
}

#[derive(Debug, Clone)]
pub struct BoolArray2D<const WIDTH: usize, const HEIGHT: usize> {
    array: [[bool; WIDTH]; HEIGHT],
}

impl<const WIDTH: usize, const HEIGHT: usize> BoolArray2D<WIDTH, HEIGHT> {
    pub fn from_array(array: [[bool; WIDTH]; HEIGHT]) -> Self {
        Self { array }
    }
    pub fn empty() -> Self {
        Self::from_array([[false; WIDTH]; HEIGHT])
    }
    pub fn get_xy(&self, x: usize, y: usize) -> bool {
        self.get_row_col(Self::y_to_row(y), x)
    }
    pub fn set_xy(&mut self, x: usize, y: usize, val: bool) {
        self.set_row_col(Self::y_to_row(y), x, val);
    }
    fn y_to_row(y: usize) -> usize {
        HEIGHT - 1 - y
    }
    pub fn get_row_col(&self, row: usize, col: usize) -> bool {
        self.array[row][col]
    }
    pub fn set_row_col(&mut self, row: usize, col: usize, val: bool) {
        self.array[row][col] = val;
    }
    pub fn print(&self) {
        for row in 0..HEIGHT {
            let mut line = "".to_string();
            for col in 0..WIDTH {
                let val = self.get_row_col(row, col);
                line += if val { "o" } else { "." };
            }
            println!("{}", line);
        }
    }
    pub fn height() -> usize {
        HEIGHT
    }
    pub fn width() -> usize {
        WIDTH
    }
}
pub type SquareBoolArray2D<const SIZE: usize> = BoolArray2D<SIZE, SIZE>;

impl<const SIZE: usize> SquareBoolArray2D<SIZE> {
    pub fn rotated(&self, quarter_turns: QuarterTurnsAnticlockwise) -> Self {
        let rotation_function = match quarter_turns.quarter_turns() {
            0 => |x, y| (x, y),
            1 => |x, y| (SIZE - 1 - y, x),
            2 => |x, y| (SIZE - 1 - x, SIZE - 1 - y),
            3 => |x, y| (y, SIZE - 1 - x),
            _ => panic!("Malformed parameter"),
        };
        let mut the_clone = self.clone();
        for x in 0..SIZE {
            for y in 0..SIZE {
                let (new_x, new_y) = rotation_function(x, y);
                the_clone.set_xy(new_x, new_y, self.get_xy(x, y));
            }
        }
        the_clone
    }
}

#[derive(Hash, Clone, Copy, Debug)]
pub struct RigidTransform {
    start_pose: SquareWithOrthogonalDir,
    end_pose: SquareWithOrthogonalDir,
}

impl RigidTransform {
    pub fn from_start_and_end_poses(
        start: impl Into<SquareWithOrthogonalDir>,
        end: impl Into<SquareWithOrthogonalDir>,
    ) -> Self {
        RigidTransform {
            start_pose: start.into(),
            end_pose: end.into(),
        }
    }
    pub fn translation(&self) -> WorldStep {
        (self.end_pose - self.start_pose).stepp()
    }
    pub fn rotation(&self) -> QuarterTurnsAnticlockwise {
        (self.end_pose - self.start_pose).rotation()
    }
    // TODO: maybe test this if sus
    pub fn transform_relative_pose(
        &self,
        pose: RelativeSquareWithOrthogonalDir,
    ) -> RelativeSquareWithOrthogonalDir {
        let end_square = rotated_n_quarter_turns_counter_clockwise(
            pose.square(),
            self.rotation().quarter_turns(),
        );

        let end_direction = self.rotation().rotate_vector(pose.direction().step());

        RelativeSquareWithOrthogonalDir::from_square_and_step(end_square, end_direction)
    }
    pub fn transform_octant(&self, octant: Octant) -> Octant {
        octant.with_n_quarter_turns_anticlockwise(self.rotation())
    }
    pub fn rotate_step(&self, step: WorldStep) -> WorldStep {
        self.rotation().rotate_vector(step)
    }
    pub fn rotate_steps(&self, steps: &StepSet) -> StepSet {
        steps
            .iter()
            .map(|&step: &WorldStep| self.rotation().rotate_vector(step))
            .collect()
    }
    pub fn transform_ray(
        &self,
        ray_start: WorldPoint,
        ray_direction: Angle<f32>,
    ) -> (WorldPoint, Angle<f32>) {
        let ray_start_relative_to_tf_start = ray_start - self.start_pose.square().to_f32();
        let dist_from_tf_start = ray_start_relative_to_tf_start.length();
        let start_tf_angle = better_angle_from_x_axis(self.start_pose.direction().step().to_f32());

        let ray_angle_from_tf_start = start_tf_angle.angle_to(ray_direction);
        let end_tf_angle = better_angle_from_x_axis(self.end_pose.direction().step().to_f32());

        let new_ray_direction = end_tf_angle + ray_angle_from_tf_start;

        let new_ray_start = if dist_from_tf_start == 0.0 {
            self.end_pose.square().to_f32()
        } else {
            let tf_start_point = self.start_pose.direction().step().to_f32();
            let position_angle_from_tf_start =
                tf_start_point.angle_to(ray_start_relative_to_tf_start);

            let position_angle_from_tf_end = end_tf_angle + position_angle_from_tf_start;

            self.end_pose.square().to_f32()
                + unit_vector_from_angle(position_angle_from_tf_end).cast_unit()
                    * dist_from_tf_start
        };

        (new_ray_start, new_ray_direction)
    }
}

impl PartialEq for RigidTransform {
    fn eq(&self, other: &Self) -> bool {
        other.start_pose.apply_rigid_transform(*self) == other.end_pose
    }
}

impl Eq for RigidTransform {}

impl Default for RigidTransform {
    fn default() -> Self {
        RigidTransform::from_start_and_end_poses(
            SquareWithOrthogonalDir::from_square_and_step(point2(0, 0), STEP_RIGHT),
            SquareWithOrthogonalDir::from_square_and_step(point2(0, 0), STEP_RIGHT),
        )
    }
}

pub trait RigidlyTransformable {
    fn apply_rigid_transform(&self, tf: RigidTransform) -> Self;
}

pub fn rotated_to_have_split_at_max<T: Copy>(vec: &Vec<T>, f: impl Fn(T, T) -> f32) -> Vec<T> {
    let index_of_new_end: usize = vec
        .iter()
        .cloned()
        .circular_tuple_windows()
        .position_max_by_key(|pair: &(T, T)| OrderedFloat(f(pair.0, pair.1)))
        .unwrap()
        + 1;

    let mut the_clone = vec.clone();
    the_clone.rotate_left(index_of_new_end);
    the_clone
}

pub trait TupleClone {
    type TupleType;
    fn tuple_clone(&self) -> Self::TupleType;
}
impl<A: Clone, B: Clone> TupleClone for (&A, &B) {
    type TupleType = (A, B);

    fn tuple_clone(&self) -> Self::TupleType {
        {
            let x = *self;
            (x.0.clone(), x.1.clone())
        }
    }
}
// TODO: learn macros
impl<A: Clone, B: Clone, C: Clone> TupleClone for (&A, &B, &C) {
    type TupleType = (A, B, C);

    fn tuple_clone(&self) -> Self::TupleType {
        {
            let x = *self;
            (x.0.clone(), x.1.clone(), x.2.clone())
        }
    }
}

#[cfg(test)]
mod tests {
    use std::array::from_fn;

    use ntest::{assert_about_eq, assert_false, assert_true, timeout};
    use pretty_assertions::{assert_eq, assert_ne};
    use rgb::RGB8;

    use crate::fov_stuff::square_visibility::LocalSquareHalfPlane;

    use super::*;

    #[test]
    fn test_pair_up_glyph_map__positions() {
        let character_squares: Vec<WorldCharacterSquare> = vec![
            point2(0, 0),
            point2(1, 0),
            point2(1, 1),
            point2(2, 0),
            point2(2, 1),
        ];

        let mut character_glyph_map = WorldCharacterSquareGlyphMap::new();
        for square in character_squares {
            character_glyph_map.insert(square, Glyph::default_transparent());
        }
        let square_glyph_map =
            pair_up_character_square_map(character_glyph_map, Glyph::transparent_glyph());
        let correct_squares = vec![point2(0, 0), point2(1, 0), point2(0, 1), point2(1, 1)];
        assert_eq!(square_glyph_map.len(), correct_squares.len());
        for square in correct_squares {
            assert!(square_glyph_map.contains_key(&square));
        }
    }

    #[test]
    fn test_pair_up_glyph_map__glyphs() {
        let mut character_glyph_map = WorldCharacterSquareGlyphMap::new();
        let test_glyph = Glyph {
            character: ' ',
            fg_color: RGB8::new(0, 0, 0),
            bg_color: RGB8::new(100, 100, 150),
            bg_transparent: false,
        };
        character_glyph_map.insert(point2(0, 0), test_glyph);
        character_glyph_map.insert(point2(1, 0), test_glyph);
        let square_glyph_map =
            pair_up_character_square_map(character_glyph_map, Glyph::transparent_glyph());
        assert_eq!(square_glyph_map.len(), 1);
        assert_eq!(
            *square_glyph_map.get(&point2(0, 0)).unwrap(),
            [test_glyph; 2]
        );
    }

    #[test]
    fn test_squares_on_board() {
        let size = BoardSize::new(5, 40);
        let squares = squares_on_board(size);
        assert!(squares.contains(&point2(4, 20)));
        assert_false!(squares.contains(&point2(14, 2)));
    }

    #[test]
    fn test_looping_clamp() {
        assert_about_eq!(looping_clamp(0.0, 5.0, 3.0), 3.0); // in range
        assert_about_eq!(looping_clamp(0.0, 5.0, 5.1), 0.1); // above
        assert_about_eq!(looping_clamp(0.0, 5.0, -0.1), 4.9); // below
        assert_about_eq!(looping_clamp(-1.0, 5.0, 11.1), -0.9); // multiple periods above
        assert_about_eq!(looping_clamp(-1.5, 1.5, 2.0), -1.0); // fraction above
    }

    #[test]
    fn test_vec_rotated_to_max() {
        // up and down
        assert_eq!(
            rotated_to_have_split_at_max(&vec![0, 1, 2, 3, 1, 1], |a, b| (a - b).abs() as f32),
            vec![1, 1, 0, 1, 2, 3]
        );

        // find max
        assert_eq!(
            rotated_to_have_split_at_max(&vec![0, 1, 2, 7, 6, 3], |a, b| (a - b).abs() as f32),
            vec![7, 6, 3, 0, 1, 2]
        );

        // no change
        assert_eq!(
            rotated_to_have_split_at_max(&vec![0, 1, 2, 3, 4, 5], |a, b| (a - b).abs() as f32),
            vec![0, 1, 2, 3, 4, 5]
        );
    }
}
