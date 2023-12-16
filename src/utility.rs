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
pub mod bool_with_partial;
pub mod circular_interval;
pub mod coordinate_frame_conversions;
pub mod coordinates;
pub mod general_utility;
pub mod halfplane;
pub mod line;
pub mod octant;
pub mod partial_angle_interval; // TODO: make private and contained within angle_interval
pub mod poses;
pub mod quadrant;
pub mod relative_interval_location;
pub mod round_robin_iterator;
pub mod trait_alias_macro;

pub use self::angle_interval::*;
pub use self::coordinate_frame_conversions::*;
pub use self::coordinates::*;
pub use self::general_utility::*;
pub use self::halfplane::*;
pub use self::line::*;
pub use self::octant::*;
pub use self::poses::*;
pub use self::quadrant::*;
pub use self::round_robin_iterator::*;

pub fn get_by_point<T, U>(grid: &Vec<Vec<T>>, p: Point2D<i32, U>) -> &T {
    &grid[p.x as usize][p.y as usize]
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

impl<const SIZE: usize> QuarterTurnRotatable for SquareBoolArray2D<SIZE> {
    fn quarter_rotated_ccw(&self, quarter_turns_ccw: impl Into<QuarterTurnsCcw>) -> Self {
        let rotation_function = match quarter_turns_ccw.into().quarter_turns() {
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

// TODO: create a relative version of a rigid transform
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
    // Treats the start pose as the origin
    // TODO: move to a relative version of a rigidtransform
    pub fn new_relative_transform_from_start_to_end(
        start: impl Into<SquareWithOrthogonalDir>,
        end: impl Into<SquareWithOrthogonalDir>,
    ) -> Self {
        let start = start.into();
        let end = end.into();

        Self::from_start_and_end_poses(ORIGIN_POSE(), start.other_pose_as_seen_from_self(end))
    }
    pub fn from_rotation(r: impl Into<QuarterTurnsCcw> + Copy) -> Self {
        let p = ORIGIN_POSE();
        Self::from_start_and_end_poses(p, p.quarter_rotated_ccw_in_place(r))
    }
    pub fn identity() -> Self {
        Self::from_start_and_end_poses((0, 0, STEP_UP), (0, 0, STEP_UP))
    }
    pub fn translation(&self) -> WorldStep {
        (self.end_pose - self.start_pose).stepp()
    }
    pub fn rotation(&self) -> QuarterTurnsCcw {
        (self.end_pose - self.start_pose).rotation()
    }
    // TODO: maybe te.st this if sus
    pub fn transform_relative_pose(
        &self,
        pose: RelativeSquareWithOrthogonalDir,
    ) -> RelativeSquareWithOrthogonalDir {
        let end_square = pose
            .square()
            .quarter_rotated_ccw(self.rotation().quarter_turns());

        let end_direction = self.rotation().rotate_vector(pose.direction().step());

        RelativeSquareWithOrthogonalDir::from_square_and_step(end_square, end_direction)
    }
    pub fn inverse(&self) -> Self {
        Self {
            start_pose: self.end_pose,
            end_pose: self.start_pose,
        }
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

pub fn get_by_index<T>(vector: &Vec<T>, index: i32) -> &T {
    let index = if index < 0 {
        vector.len() as i32 + index
    } else {
        index
    } as usize;
    vector.get(index).unwrap()
}

#[cfg(test)]
mod tests {
    use std::array::from_fn;

    use ntest::{assert_about_eq, assert_false, assert_true, timeout};
    use pretty_assertions::{assert_eq, assert_ne};
    use rgb::RGB8;

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
    fn test_parts_of_identity_rigid_transform() {
        let tf = RigidTransform::from_start_and_end_poses((2, 5, STEP_UP), (2, 5, STEP_UP));
        assert_eq!(tf.rotation(), 0.into());
        assert_eq!(tf.translation(), (0, 0).into());
    }
    #[test]
    fn test_inverse_of_identity_rigid_transform() {
        let p = ORIGIN_POSE();
        let tf = RigidTransform::from_start_and_end_poses(p, p).inverse();
        assert_eq!(tf.rotation(), 0.into());
        assert_eq!(tf.translation(), (0, 0).into());
    }
    #[test]
    fn test_relative_rigid_transform() {
        let start = (4, 3, STEP_LEFT);
        let end = (2, 2, STEP_RIGHT);
        let rel_tf = RigidTransform::new_relative_transform_from_start_to_end(start, end);
        assert_eq!(rel_tf.translation(), (-1, 2).into());
        assert_eq!(rel_tf.rotation(), 2.into());
    }
}
