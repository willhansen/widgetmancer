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
use crate::utility::angle_interval::{AngleInterval, PartialAngleInterval};
use crate::utility::coordinate_frame_conversions::*;
use crate::{DoubleGlyph, Glyph};

use self::octant::Octant;

pub mod angle_interval;
pub mod coordinate_frame_conversions;
pub mod octant;
pub mod round_robin_iterator;

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

#[derive(Hash, Default, Debug, Copy, Clone, Eq, PartialEq, CopyGetters, AddAssign)]
#[get_copy = "pub"]
pub struct QuarterTurnsAnticlockwise {
    quarter_turns: i32,
}

impl QuarterTurnsAnticlockwise {
    pub fn new(quarter_turns: i32) -> Self {
        QuarterTurnsAnticlockwise {
            quarter_turns: quarter_turns.rem_euclid(4),
        }
    }
    pub fn to_vector(&self) -> WorldStep {
        rotated_n_quarter_turns_counter_clockwise(STEP_RIGHT, self.quarter_turns)
    }
    pub fn from_vector(dir: WorldStep) -> Self {
        assert!(is_orthogonal(dir));
        QuarterTurnsAnticlockwise::new(if dir.x == 0 {
            if dir.y > 0 {
                1
            } else {
                3
            }
        } else {
            if dir.x > 0 {
                0
            } else {
                2
            }
        })
    }

    pub fn from_start_and_end_directions(start: WorldStep, end: WorldStep) -> Self {
        assert!(is_king_step(start));
        assert!(is_king_step(end));
        // needs to be quarter turn, no eighths
        assert_eq!(is_diagonal(start), is_diagonal(end));

        let d_angle = start.to_f32().angle_to(end.to_f32());
        let quarter_turns = (d_angle.to_degrees() / 90.0).round() as i32;
        Self::new(quarter_turns)
    }

    pub fn rotate_angle(&self, angle: Angle<f32>) -> Angle<f32> {
        standardize_angle(Angle::<f32>::degrees(
            angle.to_degrees() + 90.0 * (self.quarter_turns() as f32),
        ))
    }
    pub fn rotate_vector<T, U>(&self, v: Vector2D<T, U>) -> Vector2D<T, U>
    where
        T: Signed + Copy,
    {
        rotated_n_quarter_turns_counter_clockwise(v, self.quarter_turns)
    }
}

impl Neg for QuarterTurnsAnticlockwise {
    type Output = Self;

    fn neg(self) -> Self::Output {
        QuarterTurnsAnticlockwise::new(-self.quarter_turns)
    }
}

impl Add for QuarterTurnsAnticlockwise {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        Self::new(self.quarter_turns() + rhs.quarter_turns())
    }
}

impl Sub for QuarterTurnsAnticlockwise {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        Self::new(self.quarter_turns() - rhs.quarter_turns())
    }
}

pub trait QuarterTurnRotatable {
    fn rotated(&self, quarter_turns_anticlockwise: QuarterTurnsAnticlockwise) -> Self;
}

impl QuarterTurnRotatable for Angle<f32> {
    fn rotated(&self, quarter_turns_anticlockwise: QuarterTurnsAnticlockwise) -> Self {
        standardize_angle(Angle::radians(
            self.radians + PI / 2.0 * quarter_turns_anticlockwise.quarter_turns as f32,
        ))
    }
}
#[derive(Clone, Hash, Debug, Eq, Copy)]
pub struct Quadrant(pub i32);

impl PartialEq for Quadrant {
    fn eq(&self, other: &Self) -> bool {
        self.0.rem_euclid(4) == other.0.rem_euclid(4)
    }
}

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

pub fn snap_angle_to_diagonal(angle: Angle<f32>) -> Angle<f32> {
    (0..4)
        .map(|i| standardize_angle(Angle::degrees(45.0 + 90.0 * i as f32)))
        .min_by_key(|&snap_angle| OrderedFloat(abs_angle_distance(snap_angle, angle).radians))
        .unwrap()
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

fn furthest_apart_points<U>(points: Vec<Point2D<f32, U>>) -> [Point2D<f32, U>; 2] {
    assert!(points.len() >= 2);
    let furthest = points
        .iter()
        .combinations(2)
        .max_by_key(|two_points: &Vec<&Point2D<f32, U>>| {
            OrderedFloat((*two_points[0] - *two_points[1]).length())
        })
        .unwrap();
    let furthest_values: Vec<Point2D<f32, U>> = furthest.into_iter().copied().collect();
    furthest_values.try_into().unwrap()
}

pub fn three_points_are_clockwise<U>(
    a: Point2D<f32, U>,
    b: Point2D<f32, U>,
    c: Point2D<f32, U>,
) -> bool {
    let ab = b - a;
    let ac = c - a;
    ab.cross(ac) < 0.0
}

pub fn two_in_ccw_order(a: WorldMove, b: WorldMove) -> bool {
    a.cross(b) > 0.0
}

pub fn in_ccw_order(v: &Vec<WorldMove>) -> bool {
    v.iter()
        .tuple_windows()
        .all(|(&a, &b)| two_in_ccw_order(a, b))
}

pub fn on_line<U>(a: Point2D<f32, U>, b: Point2D<f32, U>, c: Point2D<f32, U>) -> bool {
    let ab = b - a;
    let ac = c - a;
    ab.cross(ac) == 0.0
}
pub fn on_line_in_this_order<U>(
    a: Point2D<f32, U>,
    b: Point2D<f32, U>,
    c: Point2D<f32, U>,
) -> bool {
    on_line(a, b, c) && (a - b).length() < (a - c).length()
}

#[deprecated(note = "Invalidated by screen rotation")]
pub fn is_world_character_square_left_square_of_world_square(
    character_square: WorldCharacterSquare,
) -> bool {
    world_square_to_left_world_character_square(world_character_square_to_world_square(
        character_square,
    )) == character_square
}

pub fn rotate_point_around_point<U>(
    axis_point: Point2D<f32, U>,
    moving_point: Point2D<f32, U>,
    angle: Angle<f32>,
) -> Point2D<f32, U> {
    axis_point + rotate_vect(moving_point - axis_point, angle)
}

pub fn cross_correlate_squares_with_steps(
    squares: SquareSet,
    steps: HashSet<WorldStep>,
) -> HashMap<WorldSquare, u32> {
    let mut step_count_map = HashMap::<WorldSquare, u32>::new();
    squares.iter().for_each(|&square| {
        steps
            .iter()
            .map(|&diagonal_step| square + diagonal_step)
            .for_each(|step_square| *step_count_map.entry(step_square).or_default() += 1)
    });
    step_count_map
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

pub fn adjacent_king_steps(dir: WorldStep) -> StepSet {
    assert!(is_king_step(dir));
    if ORTHOGONAL_STEPS.contains(&dir) {
        if dir.x != 0 {
            HashSet::from([dir + STEP_UP, dir + STEP_DOWN])
        } else {
            HashSet::from([dir + STEP_LEFT, dir + STEP_RIGHT])
        }
    } else {
        let no_x = vec2(0, dir.y);
        let no_y = vec2(dir.x, 0);
        HashSet::from([no_x, no_y])
    }
}

#[derive(Clone, Hash, Eq, PartialEq, Neg, Debug, Copy, CopyGetters, Constructor)]
#[get_copy = "pub"]
pub struct StepWithQuarterRotations {
    stepp: WorldStep,
    rotation: QuarterTurnsAnticlockwise,
}

impl StepWithQuarterRotations {
    pub fn from_direction_squares(
        start: SquareWithOrthogonalDir,
        end: SquareWithOrthogonalDir,
    ) -> Self {
        let translation = end.square() - start.square();
        let rotation = end.direction_in_quarter_turns() - start.direction_in_quarter_turns();
        Self::new(translation, rotation)
    }
}

impl Default for StepWithQuarterRotations {
    fn default() -> Self {
        StepWithQuarterRotations::new(STEP_ZERO, QuarterTurnsAnticlockwise::new(0))
    }
}

impl Add for StepWithQuarterRotations {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        StepWithQuarterRotations::new(self.stepp + rhs.stepp, self.rotation + rhs.rotation)
    }
}

#[derive(Clone, Hash, Eq, PartialEq, Copy, CopyGetters)]
#[get_copy = "pub"]
pub struct AbsOrRelSquareWithOrthogonalDir<SquareType: Copy> {
    square: SquareType,
    dir: OrthogonalWorldStep,
}

pub type SquareWithOrthogonalDir = AbsOrRelSquareWithOrthogonalDir<WorldSquare>;
pub type RelativeSquareWithOrthogonalDir = AbsOrRelSquareWithOrthogonalDir<WorldStep>;

impl<SquareType> AbsOrRelSquareWithOrthogonalDir<SquareType>
where
    SquareType: AbsOrRelSquareTrait<SquareType> + Copy + PartialEq,
{
    pub fn direction_in_quarter_turns(&self) -> QuarterTurnsAnticlockwise {
        QuarterTurnsAnticlockwise::from_start_and_end_directions(STEP_RIGHT, self.dir.into())
    }
    pub fn from_square_and_step<S: Into<OrthogonalWorldStep>>(
        square: SquareType,
        direction: S,
    ) -> Self {
        Self {
            square,
            dir: direction.into(),
        }
    }
    pub fn from_square_and_turns(
        square: SquareType,
        quarter_turns: QuarterTurnsAnticlockwise,
    ) -> Self {
        Self::from_square_and_step(square, quarter_turns.to_vector())
    }
    pub fn direction(&self) -> OrthogonalWorldStep {
        self.dir()
    }
    pub fn stepped(&self) -> Self {
        Self::from_square_and_step(self.square + self.direction().step(), self.direction())
    }
    pub fn stepped_n(&self, n: i32) -> Self {
        Self::from_square_and_step(self.square + self.direction().step() * n, self.direction())
    }
    pub fn stepped_back(&self) -> Self {
        Self::from_square_and_step(self.square - self.direction().step(), self.direction())
    }
    pub fn strafed_left(&self) -> Self {
        self.strafed_right_n(-1)
    }
    pub fn strafed_right(&self) -> Self {
        self.strafed_right_n(1)
    }
    pub fn strafed_right_n(&self, n: i32) -> Self {
        Self::from_square_and_step(self.square + self.right().step() * n, self.direction())
    }
    pub fn strafed_left_n(&self, n: i32) -> Self {
        self.strafed_right_n(-n)
    }

    pub fn turned_left(&self) -> Self {
        Self::from_square_and_step(self.square, self.left())
    }
    pub fn turned_right(&self) -> Self {
        Self::from_square_and_step(self.square, self.right())
    }
    fn left(&self) -> OrthogonalWorldStep {
        rotated_n_quarter_turns_counter_clockwise(self.direction().into(), 1).into()
    }
    fn right(&self) -> OrthogonalWorldStep {
        rotated_n_quarter_turns_counter_clockwise(self.direction().into(), 3).into()
    }
    pub fn turned_back(&self) -> Self {
        Self::from_square_and_step(self.square, -self.direction().step())
    }
    pub fn with_offset(&self, offset: WorldStep) -> Self {
        Self::from_square_and_step(self.square + offset, self.dir())
    }
    pub fn with_direction(&self, dir: WorldStep) -> Self {
        Self::from_square_and_step(self.square, dir)
    }
    pub fn reversed(&self) -> Self {
        self.with_direction(-self.direction().step())
    }
    fn as_relative_face(&self) -> RelativeSquareWithOrthogonalDir {
        RelativeSquareWithOrthogonalDir::from_square_and_step(
            self.square() - SquareType::zero(),
            self.dir(),
        )
    }
    fn as_absolute_face(&self) -> SquareWithOrthogonalDir {
        SquareWithOrthogonalDir::from_square_and_step(
            WorldSquare::zero() + self.as_relative_face().square(),
            self.dir(),
        )
    }
    pub fn face_is_on_same_line<OtherType: Into<Self>>(&self, other: OtherType) -> bool {
        let other_face: Self = other.into();
        let directions_are_parallel = self.dir.step().dot(other_face.dir.step()) != 0;
        if !directions_are_parallel {
            return false;
        }

        let pos_on_dir_axis = self.dir().pos_on_axis(self.as_relative_face().square());
        let stepped_pos_on_dir_axis = self
            .dir()
            .pos_on_axis(self.stepped().as_relative_face().square().into());
        let other_pos_on_dir_axis = self
            .dir()
            .pos_on_axis(other_face.as_relative_face().square().into());

        let same_direction = self.dir() == other_face.dir();
        if same_direction {
            other_pos_on_dir_axis == pos_on_dir_axis
        } else {
            other_pos_on_dir_axis == stepped_pos_on_dir_axis
        }
    }
    pub fn faces_overlap<OtherType: Into<Self> + std::marker::Copy>(
        &self,
        other_face: OtherType,
    ) -> bool {
        *self == other_face.into() || *self == other_face.into().stepped().turned_back()
    }
    // TODO: return AbsOrRelWorldLine
    pub fn line(&self) -> WorldLine {
        let abs_face = self.as_absolute_face();
        square_face_as_line(abs_face.square, abs_face.dir)
    }
}

impl<T: Debug + Copy> Debug for AbsOrRelSquareWithOrthogonalDir<T>
where
    Self: Display,
{
    fn fmt(&self, mut f: &mut Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(&(&self), &mut f)
    }
}

impl<T: Debug + Copy> Display for AbsOrRelSquareWithOrthogonalDir<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Pos: {:?}, Dir: {:?} {} ",
            self.square(),
            self.dir().step(),
            Glyph::extract_arrow_from_arrow_string(self.dir().step(), FACE_ARROWS)
        )
    }
}

// TODO: generalize for absolute squares too
impl RelativeSquareWithOrthogonalDir {
    pub fn face_center_point(&self) -> WorldMove {
        self.square().to_f32() + self.dir().step().to_f32() * 0.5
    }
    pub fn face_end_points(&self) -> [WorldMove; 2] {
        [self.left(), self.right()].map(|dir| self.face_center_point() + dir.step().to_f32() * 0.5)
    }
    pub fn face_end_points_in_ccw_order(&self) -> [WorldMove; 2] {
        let mut ps = self.face_end_points();
        if !two_in_ccw_order(ps[0], ps[1]) {
            ps.reverse();
        }
        ps
    }
    pub fn face_end_point_approx_touches_point(&self, point: WorldMove) -> bool {
        let tolerance = 1e-6;
        self.face_end_points()
            .into_iter()
            .any(|end_point| about_eq_2d(end_point, point, tolerance))
    }
}

impl TryFrom<SquareWithKingDir> for SquareWithOrthogonalDir {
    type Error = ();

    fn try_from(value: SquareWithKingDir) -> Result<Self, Self::Error> {
        if is_orthogonal(value.direction().into()) {
            Ok(SquareWithOrthogonalDir::from_square_and_step(
                value.square(),
                value.direction(),
            ))
        } else {
            Err(())
        }
    }
}

impl Add<StepWithQuarterRotations> for SquareWithOrthogonalDir {
    type Output = Self;

    fn add(self, rhs: StepWithQuarterRotations) -> Self::Output {
        SquareWithOrthogonalDir::from_square_and_turns(
            self.square + rhs.stepp,
            self.direction_in_quarter_turns() + rhs.rotation,
        )
    }
}

impl Sub<SquareWithOrthogonalDir> for SquareWithOrthogonalDir {
    type Output = StepWithQuarterRotations;

    fn sub(self, rhs: SquareWithOrthogonalDir) -> Self::Output {
        StepWithQuarterRotations::new(
            self.square - rhs.square,
            self.direction_in_quarter_turns() - rhs.direction_in_quarter_turns(),
        )
    }
}

impl<ConvertableToSquareType, SquareType, DirectionType>
    From<(ConvertableToSquareType, DirectionType)> for AbsOrRelSquareWithOrthogonalDir<SquareType>
where
    ConvertableToSquareType: Into<SquareType>,
    SquareType: AbsOrRelSquareTrait<SquareType>,
    DirectionType: Into<OrthogonalWorldStep>,
{
    fn from(value: (ConvertableToSquareType, DirectionType)) -> Self {
        Self::from_square_and_step(value.0.into(), value.1)
    }
}
impl<SquareType> From<AbsOrRelSquareWithOrthogonalDir<SquareType>>
    for (SquareType, OrthogonalWorldStep)
where
    SquareType: AbsOrRelSquareTrait<SquareType>,
{
    fn from(
        value: AbsOrRelSquareWithOrthogonalDir<SquareType>,
    ) -> (SquareType, OrthogonalWorldStep) {
        (value.square, value.direction())
    }
}

#[derive(Clone, Hash, Eq, PartialEq, Debug, Copy)]
pub struct KingWorldStep {
    step: WorldStep,
}

impl KingWorldStep {
    pub fn new(dir: WorldStep) -> Self {
        assert!(is_king_step(dir));
        KingWorldStep { step: dir }
    }
    pub fn step(&self) -> WorldStep {
        self.step
    }
}

impl QuarterTurnRotatable for KingWorldStep {
    fn rotated(&self, quarter_turns_anticlockwise: QuarterTurnsAnticlockwise) -> Self {
        self.step().rotated(quarter_turns_anticlockwise).into()
    }
}

impl From<WorldStep> for KingWorldStep {
    fn from(value: WorldStep) -> Self {
        KingWorldStep::new(value)
    }
}

impl From<OrthogonalWorldStep> for KingWorldStep {
    fn from(value: OrthogonalWorldStep) -> Self {
        KingWorldStep::new(value.step)
    }
}

impl From<KingWorldStep> for WorldStep {
    fn from(value: KingWorldStep) -> Self {
        value.step
    }
}

#[derive(Clone, Hash, Neg, Eq, PartialEq, Debug, Copy)]
pub struct OrthogonalWorldStep {
    step: WorldStep,
}

impl OrthogonalWorldStep {
    pub fn new(dir: WorldStep) -> Self {
        assert!(is_orthogonal_king_step(dir));
        OrthogonalWorldStep { step: dir }
    }
    pub fn step(&self) -> WorldStep {
        self.step
    }
    pub fn pos_on_axis(&self, pos: WorldStep) -> i32 {
        self.step().dot(pos)
    }
}

impl QuarterTurnRotatable for OrthogonalWorldStep {
    fn rotated(&self, quarter_turns_anticlockwise: QuarterTurnsAnticlockwise) -> Self {
        self.step().rotated(quarter_turns_anticlockwise).into()
    }
}

impl From<WorldStep> for OrthogonalWorldStep {
    fn from(value: WorldStep) -> Self {
        OrthogonalWorldStep::new(value)
    }
}

impl From<OrthogonalWorldStep> for WorldStep {
    fn from(value: OrthogonalWorldStep) -> Self {
        value.step
    }
}

impl From<KingWorldStep> for OrthogonalWorldStep {
    fn from(value: KingWorldStep) -> Self {
        OrthogonalWorldStep::new(value.step)
    }
}

#[derive(Clone, Hash, Eq, PartialEq, Debug, Copy, CopyGetters)]
#[get_copy = "pub"]
pub struct SquareWithKingDir {
    square: WorldSquare,
    direction: KingWorldStep,
}

impl SquareWithKingDir {
    pub fn new(square: WorldSquare, direction: KingWorldStep) -> Self {
        SquareWithKingDir { square, direction }
    }
    pub fn from_square_and_step(square: WorldSquare, direction: WorldStep) -> SquareWithKingDir {
        Self::new(square, direction.into())
    }
    pub fn tuple(&self) -> (WorldSquare, KingWorldStep) {
        (self.square, self.direction)
    }
    pub fn stepped(&self) -> SquareWithKingDir {
        SquareWithKingDir::from_square_and_step(
            self.square + self.direction.step,
            self.direction.into(),
        )
    }
}

impl From<SquareWithOrthogonalDir> for SquareWithKingDir {
    fn from(value: SquareWithOrthogonalDir) -> Self {
        SquareWithKingDir {
            square: value.square(),
            direction: value.direction().into(),
        }
    }
}

impl From<(WorldSquare, KingWorldStep)> for SquareWithKingDir {
    fn from(value: (WorldSquare, KingWorldStep)) -> Self {
        Self::new(value.0, value.1)
    }
}
impl From<SquareWithKingDir> for (WorldSquare, KingWorldStep) {
    fn from(value: SquareWithKingDir) -> (WorldSquare, KingWorldStep) {
        (value.square, value.direction)
    }
}

#[derive(Clone, Hash, Eq, PartialEq, Debug, Copy, CopyGetters)]
#[get_copy = "pub"]
pub struct TranslationAndRotationTransform {
    translation: WorldStep,
    quarter_rotations_counterclockwise: u32,
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

impl<SquareType> RigidlyTransformable for AbsOrRelSquareWithOrthogonalDir<SquareType>
where
    SquareType: Copy + RigidlyTransformable + AbsOrRelSquareTrait<SquareType>,
{
    fn apply_rigid_transform(&self, tf: RigidTransform) -> Self {
        Self::from_square_and_step(
            self.square().apply_rigid_transform(tf),
            self.dir().apply_rigid_transform(tf),
        )
    }
}

impl RigidlyTransformable for WorldSquare {
    fn apply_rigid_transform(&self, tf: RigidTransform) -> Self {
        revolve_square(*self, tf.start_pose.square(), tf.rotation()) + tf.translation()
    }
}
impl RigidlyTransformable for WorldStep {
    fn apply_rigid_transform(&self, tf: RigidTransform) -> Self {
        tf.rotation().rotate_vector(*self)
    }
}
impl RigidlyTransformable for OrthogonalWorldStep {
    fn apply_rigid_transform(&self, tf: RigidTransform) -> Self {
        tf.rotation().rotate_vector(self.step).into()
    }
}

impl RigidlyTransformable for AngleInterval {
    fn apply_rigid_transform(&self, tf: RigidTransform) -> Self {
        match self {
            AngleInterval::Empty | AngleInterval::FullCircle => *self,
            AngleInterval::PartialArc(partial_angle_interval) => {
                AngleInterval::PartialArc(partial_angle_interval.apply_rigid_transform(tf))
            }
        }
    }
}

impl RigidlyTransformable for PartialAngleInterval {
    fn apply_rigid_transform(&self, tf: RigidTransform) -> Self {
        self.rotated_quarter_turns(tf.rotation())
    }
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

pub fn quadrants_of_rel_square(
    can_be_rel_square: impl Into<WorldStep> + Copy,
) -> HashSet<Quadrant> {
    // A square on an axis is in two quadrants
    // The origin is in all 4
    (0..4)
        .map(|i| Quadrant(i))
        .filter(|&q| rel_square_is_in_quadrant_or_on_adjacent_axis(can_be_rel_square, q))
        .collect()
}
pub fn rel_square_is_in_quadrant_or_on_adjacent_axis(
    can_be_rel_square: impl Into<WorldStep>,
    quadrant: Quadrant,
) -> bool {
    let rel_square: WorldStep = can_be_rel_square.into();
    let dir = direction_of_quadrant(quadrant);

    todo!()
}
pub fn direction_of_quadrant(quadrant: Quadrant) -> WorldStep {
    STEP_UP_RIGHT.rotated(QuarterTurnsAnticlockwise::new(quadrant.0))
}

pub fn squares_sharing_face<SquareType: AbsOrRelSquareTrait<SquareType>>(
    face: AbsOrRelSquareWithOrthogonalDir<SquareType>,
) -> [SquareType; 2] {
    [face.square, face.stepped().square]
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
    fn test_world_pos_to_character_world_pos() {
        assert_eq!(
            Point2D::<f32, CharacterGridInWorldFrame>::new(0.5, 0.0),
            world_point_to_world_character_point(Point2D::<f32, SquareGridInWorldFrame>::new(
                0.0, 0.0,
            )),
            "zero is actually between two characters"
        );
        assert_eq!(
            Point2D::<f32, CharacterGridInWorldFrame>::new(2.5, 1.0),
            world_point_to_world_character_point(Point2D::<f32, SquareGridInWorldFrame>::new(
                1.0, 1.0,
            )),
            "diagonal a bit"
        );
    }

    #[test]
    fn test_local_square_point_to_local_character_point() {
        assert_eq!(
            local_square_point_to_local_character_point(point2(0.0, 0.0), 0),
            point2(0.5, 0.0)
        );
        assert_eq!(
            local_square_point_to_local_character_point(point2(0.0, 0.0), 1),
            point2(-0.5, 0.0)
        );
    }

    #[test]
    fn test_revolve_square() {
        assert_eq!(
            revolve_square(
                point2(3, 4),
                point2(5, 5),
                QuarterTurnsAnticlockwise::new(3),
            ),
            point2(4, 7)
        );
    }

    #[test]
    fn test_quarter_turns_from_vectors() {
        assert_eq!(
            QuarterTurnsAnticlockwise::from_start_and_end_directions(STEP_UP, STEP_UP),
            QuarterTurnsAnticlockwise::new(0)
        );
        assert_eq!(
            QuarterTurnsAnticlockwise::from_start_and_end_directions(STEP_UP, STEP_RIGHT),
            QuarterTurnsAnticlockwise::new(3)
        );
        assert_eq!(
            QuarterTurnsAnticlockwise::from_start_and_end_directions(STEP_LEFT, STEP_RIGHT),
            QuarterTurnsAnticlockwise::new(2)
        );
        assert_eq!(
            QuarterTurnsAnticlockwise::from_start_and_end_directions(
                STEP_DOWN_LEFT,
                STEP_DOWN_RIGHT,
            ),
            QuarterTurnsAnticlockwise::new(1)
        );
    }

    #[test]
    fn test_step_back_pose() {
        let pose = SquareWithOrthogonalDir::from_square_and_step(point2(4, 6), STEP_RIGHT);
        let back = SquareWithOrthogonalDir::from_square_and_step(point2(3, 6), STEP_RIGHT);
        assert_eq!(pose.stepped_back(), back);
    }

    #[test]
    fn test_step_or_turn_pose() {
        let p = SquareWithOrthogonalDir::from_square_and_step;
        let s = point2(5, 5);
        assert_eq!(p(s, STEP_RIGHT).stepped(), p(s + STEP_RIGHT, STEP_RIGHT));
        assert_eq!(p(s, STEP_UP).stepped(), p(s + STEP_UP, STEP_UP));
        assert_eq!(p(s, STEP_DOWN).strafed_left(), p(s + STEP_RIGHT, STEP_DOWN));
        assert_eq!(p(s, STEP_LEFT).strafed_right(), p(s + STEP_UP, STEP_LEFT));
        assert_eq!(p(s, STEP_LEFT).turned_left(), p(s, STEP_DOWN));
        assert_eq!(p(s, STEP_LEFT).turned_right(), p(s, STEP_UP));
        assert_eq!(p(s, STEP_LEFT).turned_back(), p(s, STEP_RIGHT));
    }

    #[test]
    fn test_line_point_reflection() {
        let line = Line::new(WorldPoint::new(1.0, 5.0), WorldPoint::new(2.4, 5.0));

        assert_about_eq!(
            line.reflect_point_over_line(point2(0.0, 3.0)).to_array(),
            WorldPoint::new(0.0, 7.0).to_array()
        );
        assert_ne!(
            line.reflect_point_over_line(point2(0.0, 3.0)).to_array(),
            WorldPoint::new(0.0, 8.0).to_array()
        );
    }

    #[test]
    fn test_half_plane_cover_unit_square() {
        let [exactly_cover, less_than_cover, more_than_cover]: [HalfPlane<_, _>; 3] =
            [0.0, 0.01, -0.01].map(|dx| {
                HalfPlane::from_line_and_point_on_half_plane(
                    Line::new(
                        WorldPoint::new(-0.5 + dx, 0.0),
                        point2(-0.5 + 2.0 * dx, 1.0),
                    ),
                    point2(1.5, 0.0),
                )
            });

        assert!(more_than_cover.fully_covers_unit_square());
        assert_false!(less_than_cover.fully_covers_unit_square());
        assert!(exactly_cover.fully_covers_unit_square());
    }

    #[test]
    fn test_same_side_of_line() {
        let line = Line::<_, WorldPoint>::new(point2(1.0, 1.0), point2(2.0, 1.0));
        let low = point2(0.0, 0.0);
        let low2 = point2(9.0, 0.3);
        let high = point2(0.0, 10.0);
        let high2 = point2(5.0, 10.0);
        let on = point2(0.0, 1.0);
        let on2 = point2(5.0, 1.0);

        assert!(line.same_side_of_line(low, low2));

        assert!(line.same_side_of_line(high, high2));
        assert!(line.same_side_of_line(high2, high));

        assert!(line.same_side_of_line(on, on2));
        assert!(line.same_side_of_line(on2, on));

        assert_false!(line.same_side_of_line(low, on2));
        assert_false!(line.same_side_of_line(high, on));
        assert_false!(line.same_side_of_line(low, high2));
    }

    #[test]
    fn test_halfplane_covers_expanded_unit_square() {
        let the_plane = HalfPlane::from_line_and_point_on_half_plane(
            Line::new(WorldPoint::new(1.0, 5.0), point2(1.0, 6.0)),
            point2(-5.0, 0.0),
        );
        assert!(the_plane.fully_covers_expanded_unit_square(0.0));
        assert!(the_plane.fully_covers_expanded_unit_square(0.49));
        assert_false!(the_plane.fully_covers_expanded_unit_square(0.51));
        assert_false!(the_plane.fully_covers_expanded_unit_square(100.0));
    }

    #[test]
    fn test_squares_on_board() {
        let size = BoardSize::new(5, 40);
        let squares = squares_on_board(size);
        assert!(squares.contains(&point2(4, 20)));
        assert_false!(squares.contains(&point2(14, 2)));
    }

    #[test]
    fn test_horizontal_line_intersection_with_square() {
        let input_line: Line<f32, SquareGridInWorldFrame> =
            Line::new(point2(0.5, 0.0), point2(-1.5, 0.0));
        let output_points = input_line.line_intersections_with_centered_unit_square();
        assert_eq!(output_points, vec![point2(0.5, 0.0), point2(-0.5, 0.0)]);
    }

    #[test]
    fn test_vertical_line_intersection_with_square() {
        let input_line: Line<f32, SquareGridInWorldFrame> =
            Line::new(point2(0.0, 0.5), point2(0.0, -1.5));
        let output_points = input_line.line_intersections_with_centered_unit_square();
        assert_eq!(output_points, vec![point2(0.0, 0.5), point2(0.0, -0.5)]);
    }

    #[test]
    fn test_depth_of_point_in_half_plane() {
        let horizontal = HalfPlane::from_line_and_point_on_half_plane(
            Line::new(WorldPoint::new(0.0, 0.0), point2(1.0, 0.0)),
            point2(0.0, 5.0),
        );

        assert_about_eq!(
            horizontal.depth_of_point_in_half_plane(point2(0.0, 12.0)),
            12.0
        );
        assert_about_eq!(
            horizontal
                .extended(3.0)
                .depth_of_point_in_half_plane(point2(2000.0, 10.0)),
            13.0
        );
        assert_about_eq!(
            horizontal.depth_of_point_in_half_plane(point2(0.0, -1.0)),
            -1.0
        );

        let diag = HalfPlane::from_line_and_point_on_half_plane(
            Line::new(WorldPoint::new(0.0, 0.0), point2(-1.0, 1.0)),
            point2(0.0, 5.0),
        );
        assert_about_eq!(
            diag.depth_of_point_in_half_plane(point2(1.0, 0.0)),
            1.0 / 2.0.sqrt()
        );
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
    fn test_ray_hit_face__simple() {
        let start_point = point2(5.0, 5.0);
        let degrees = 90;
        let range = 5.0;
        let face = (point2(5, 6), STEP_UP).into();

        let result = does_ray_hit_oriented_square_face(
            start_point,
            Angle::degrees(degrees as f32),
            range,
            face,
        );

        assert!(result);
    }

    #[test]
    fn test_ray_hit_face__face_must_face_ray() {
        let start_point = point2(5.0, 5.0);
        let degrees = 90;
        let range = 5.0;
        let face = (point2(5, 6), STEP_DOWN).into();

        let result = does_ray_hit_oriented_square_face(
            start_point,
            Angle::degrees(degrees as f32),
            range,
            face,
        );

        assert_false!(result);
    }
    #[test]
    fn test_ray_hit_face__miss() {
        let start_point = point2(5.0, 5.0);
        let degrees = 90;
        let range = 5.0;
        let face = (point2(6, 6), STEP_UP).into();

        let result = does_ray_hit_oriented_square_face(
            start_point,
            Angle::degrees(degrees as f32),
            range,
            face,
        );

        assert_false!(result);
    }
    #[test]
    fn test_ray_hit_face__under_ranged() {
        let start_point = point2(5.0, 5.0);
        let degrees = 90;
        let range = 1.49;
        let face = (point2(5, 6), STEP_UP).into();

        let result = does_ray_hit_oriented_square_face(
            start_point,
            Angle::degrees(degrees as f32),
            range,
            face,
        );

        assert_false!(result);
    }
    #[test]
    fn test_ray_hit_face__just_within_range() {
        let start_point = point2(5.0, 5.0);
        let degrees = 90;
        let range = 01.501;
        let face = (point2(5, 6), STEP_UP).into();

        let result = does_ray_hit_oriented_square_face(
            start_point,
            Angle::degrees(degrees as f32),
            range,
            face,
        );

        assert!(result);
    }
    #[test]
    fn test_ray_hit_face__just_out_of_closer_range() {
        let start_point = point2(5.0, 5.49);
        let degrees = 90;
        let range = 1.0;
        let face = (point2(5, 6), STEP_UP).into();

        let result = does_ray_hit_oriented_square_face(
            start_point,
            Angle::degrees(degrees as f32),
            range,
            face,
        );

        assert_false!(result);
    }
    #[test]
    fn test_ray_hit_face__just_within_closer_range() {
        let start_point = point2(5.0, 5.49);
        let degrees = 90;
        let range = 1.02;
        let face = (point2(5, 6), STEP_UP).into();

        let result = does_ray_hit_oriented_square_face(
            start_point,
            Angle::degrees(degrees as f32),
            range,
            face,
        );

        assert!(result);
    }
    #[test]
    fn test_ray_hit_face__just_out_of_really_close_range() {
        let start_point = point2(5.0, 6.49);
        let degrees = 90;
        let range = 0.001;
        let face = (point2(5, 6), STEP_UP).into();

        let result = does_ray_hit_oriented_square_face(
            start_point,
            Angle::degrees(degrees as f32),
            range,
            face,
        );

        assert_false!(result);
    }
    #[test]
    fn test_ray_hit_face__just_within_really_close_range() {
        assert!(does_ray_hit_oriented_square_face(
            point2(5.0, 6.49),
            Angle::degrees(90.0),
            0.02,
            (point2(5, 6), STEP_UP).into(),
        ));
    }
    #[test]
    fn test_ray_hit_face__angled_miss() {
        assert_false!(does_ray_hit_oriented_square_face(
            point2(5.0, 5.49),
            Angle::degrees(45.0),
            5.0,
            (point2(5, 6), STEP_UP).into(),
        ));
    }
    #[test]
    fn test_ray_hit_face__angled_hit() {
        assert!(does_ray_hit_oriented_square_face(
            point2(5.0, 5.49),
            Angle::degrees(45.0),
            5.0,
            (point2(5, 6), STEP_RIGHT).into(),
        ));
    }
    #[test]
    fn test_ray_hit_face__just_barely_touching_still_counts() {
        assert!(does_ray_hit_oriented_square_face(
            point2(5.5, 5.0),
            Angle::degrees(90.0),
            5.5,
            (point2(5, 10), STEP_UP).into(),
        ));
    }
    #[test]
    fn test_ray_hit_face__parallel_hit_does_not_count() {
        assert_false!(does_ray_hit_oriented_square_face(
            point2(5.0, 5.5),
            Angle::degrees(0.0),
            5.0,
            (point2(7, 5), STEP_UP).into(),
        ));
    }
    #[test]
    fn test_line_line_intersection__easy_orthogonal_hit() {
        assert_about_eq_2d(
            WorldLine::new(point2(0.0, 0.0), point2(0.0, 4.0))
                .intersection_point_with_other_line(&WorldLine::new(
                    point2(-1.0, 1.0),
                    point2(1.0, 1.0),
                ))
                .unwrap(),
            point2(0.0, 1.0),
        )
    }
    #[test]
    fn test_line_line_intersection__diagonal_intersection() {
        assert_about_eq_2d(
            WorldLine::new(point2(0.0, 0.0), point2(1.0, 1.0))
                .intersection_point_with_other_line(&WorldLine::new(
                    point2(1.0, 0.0),
                    point2(0.0, 1.0),
                ))
                .unwrap(),
            point2(0.5, 0.5),
        )
    }
    #[test]
    fn test_line_line_intersection__miss() {
        assert!(WorldLine::new(point2(0.0, 0.0), point2(1.0, 1.0))
            .intersection_point_with_other_line(&WorldLine::new(
                point2(100.0, 1000.0),
                point2(10.0, 10.0),
            ))
            .is_none())
    }
    #[test]
    fn test_line_line_intersection__endpoint_touch_mid_counts() {
        assert_about_eq_2d(
            WorldLine::new(point2(5.0, 5.0), point2(7.0, 5.0))
                .intersection_point_with_other_line(&WorldLine::new(
                    point2(5.5, 5.0),
                    point2(10.0, 10.0),
                ))
                .unwrap(),
            point2(5.5, 5.0),
        )
    }
    #[test]
    fn test_line_line_intersection__perpendicular_endpoints_touch() {
        assert_about_eq_2d(
            WorldLine::new(point2(5.0, 5.0), point2(10.0, 5.0))
                .intersection_point_with_other_line(&WorldLine::new(
                    point2(10.0, 5.0),
                    point2(10.0, 10.0),
                ))
                .unwrap(),
            point2(10.0, 5.0),
        )
    }
    #[test]
    fn test_line_line_intersection__parallel_endpoints_touch() {
        let line1 = WorldLine::new(point2(5.0, 5.0), point2(10.0, 5.0));
        let line2 = WorldLine::new(point2(10.0, 5.0), point2(20.0, 5.0));
        assert_about_eq_2d(
            line1.intersection_point_with_other_line(&line2).unwrap(),
            point2(10.0, 5.0),
        );
        assert_about_eq_2d(
            line1
                .reversed()
                .intersection_point_with_other_line(&line2)
                .unwrap(),
            point2(10.0, 5.0),
        );
        assert_about_eq_2d(
            line1
                .intersection_point_with_other_line(&line2.reversed())
                .unwrap(),
            point2(10.0, 5.0),
        );
        assert_about_eq_2d(
            line1
                .reversed()
                .intersection_point_with_other_line(&line2.reversed())
                .unwrap(),
            point2(10.0, 5.0),
        );
    }
    #[test]
    fn test_line_line_intersection__parallel_miss() {
        assert!(WorldLine::new(point2(5.0, 5.0), point2(10.0, 5.0))
            .intersection_point_with_other_line(&WorldLine::new(
                point2(11.0, 5.0),
                point2(20.0, 5.0),
            ))
            .is_none(),)
    }
    #[test]
    fn test_line_line_intersection__parallel_overlap_does_not_count() {
        assert!(WorldLine::new(point2(5.0, 5.0), point2(10.0, 5.0))
            .intersection_point_with_other_line(&WorldLine::new(
                point2(9.0, 5.0),
                point2(20.0, 5.0),
            ))
            .is_none(),)
    }
    #[test]
    fn test_line_line_intersection__parallel_full_overlap_does_not_count() {
        assert!(WorldLine::new(point2(5.0, 5.0), point2(10.0, 5.0))
            .intersection_point_with_other_line(&WorldLine::new(
                point2(0.0, 5.0),
                point2(20.0, 5.0),
            ))
            .is_none(),)
    }
    #[test]
    fn test_line_line_intersection__parallel_exact_overlap_does_not_count() {
        assert!(WorldLine::new(point2(5.0, 5.0), point2(10.0, 5.0))
            .intersection_point_with_other_line(&WorldLine::new(
                point2(5.0, 5.0),
                point2(10.0, 5.0),
            ))
            .is_none(),)
    }
    #[test]
    fn test_first_inside_square_face_hit_by_ray__simple_case() {
        let inside_faces = HashSet::from([
            (point2(5, 6), STEP_UP).into(),
            (point2(5, 7), STEP_DOWN).into(),
            (point2(5, 7), STEP_UP).into(),
        ]);
        let result = first_inside_square_face_hit_by_ray(
            point2(5.0, 5.0),
            Angle::degrees(90.0),
            20.0,
            &inside_faces,
        );
        assert_eq!(result.unwrap().0, (point2(5, 6), STEP_UP).into());
        assert_about_eq_2d(result.unwrap().1, point2(5.0, 6.5));
    }
    #[test]
    fn test_project_step_onto_axis() {
        assert_eq!(
            distance_of_step_along_axis(STEP_UP_LEFT * 8, STEP_RIGHT.into()),
            -8
        );
    }
    #[test]
    fn test_face_is_on_same_line() {
        let f = |a, b| SquareWithOrthogonalDir::from(a).face_is_on_same_line(b);
        // facing each other left-right
        assert!(f((point2(3, 5), STEP_RIGHT), (point2(4, 5), STEP_LEFT)));
        // facing each other left-right, with vertical offset
        assert!(f((point2(3, 5), STEP_RIGHT), (point2(4, 25), STEP_LEFT)));
        //facing each other left-right, too far apart
        assert_false!(f((point2(2, 5), STEP_RIGHT), (point2(4, 5), STEP_LEFT)));

        // facing each other up-down
        assert!(f((point2(3, 5), STEP_UP), (point2(3, 6), STEP_DOWN)));

        // Same face
        assert!(f((point2(3, 5), STEP_RIGHT), (point2(3, 5), STEP_RIGHT)));
        // Same face, vertical offset
        assert!(f((point2(3, 5), STEP_RIGHT), (point2(3, 45), STEP_RIGHT)));
    }
    #[test]
    fn test_faces_away_from_center_at_relative_square() {
        let step = vec2(3, 4);
        assert_eq!(
            faces_away_from_center_at_rel_square(step),
            HashSet::from([(step, STEP_UP).into(), (step, STEP_RIGHT).into()])
        );
        let step = vec2(0, -40);
        assert_eq!(
            faces_away_from_center_at_rel_square(step),
            HashSet::from([
                (step, STEP_LEFT).into(),
                (step, STEP_DOWN).into(),
                (step, STEP_RIGHT).into()
            ])
        );
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
    #[test]
    fn test_relative_points_in_ccw_order() {
        assert_true!(two_in_ccw_order(STEP_RIGHT.to_f32(), STEP_UP.to_f32()));
        assert_true!(two_in_ccw_order(STEP_UP.to_f32(), STEP_LEFT.to_f32()));
        // slight diff
        assert_true!(two_in_ccw_order(
            STEP_UP.to_f32(),
            STEP_UP.to_f32() + WorldMove::new(-0.001, 0.0)
        ));
        assert_true!(two_in_ccw_order(
            STEP_UP.to_f32(),
            STEP_DOWN.to_f32() + WorldMove::new(-0.001, 0.0)
        ));

        // across
        assert_false!(two_in_ccw_order(STEP_UP.to_f32(), STEP_DOWN.to_f32()));

        assert_false!(two_in_ccw_order(STEP_UP.to_f32(), STEP_RIGHT.to_f32()));
        assert_false!(two_in_ccw_order(STEP_ZERO.to_f32(), STEP_RIGHT.to_f32()));
    }
    #[test]
    #[ignore = "Not yet implemented"]
    fn test_quadrants_of_relative_square() {
        let point_quadrants: Vec<((i32, i32), Vec<i32>)> = vec![
            ((0, 0), vec![0, 1, 2, 3]),
            ((1, 0), vec![0, 3]),
            ((1, 1), vec![0]),
            ((0, 1), vec![0, 1]),
            ((0, -100), vec![3, 2]),
        ];
        point_quadrants.into_iter().for_each(|(p, v)| {
            assert_eq!(
                quadrants_of_rel_square(p),
                v.iter().map(|&q| Quadrant(q)).collect()
            );
        });
    }
    #[test]
    fn test_halfplane_overlap_within_unit_square() {
        //
        //      +----------------+
        //   o  |  o         o   |
        //   G  |  A         B   |
        //      |                |
        //      |       +        |   o
        //      |       O        |   C
        //      |                |
        //   F  |  E         D   |
        //   o  |  o         o   |
        //      +----------------+
        //
        let a = (-0.4, 0.4);
        let b = (0.4, 0.4);
        let c = (0.6, 0.0);
        let d = (0.4, -0.4);
        let e = (-0.4, -0.4);
        let f = (-0.6, -0.4);
        let g = (-0.6, 0.4);

        let up: LocalSquareHalfPlane =
            HalfPlane::from_line_and_point_on_half_plane((a, b), (0.0, 5.0));
        let up_right: LocalSquareHalfPlane =
            HalfPlane::from_line_and_point_on_half_plane((b, c), (1.0, 1.0));
        let down_right: LocalSquareHalfPlane =
            HalfPlane::from_line_and_point_on_half_plane((c, d), (1.0, -1.0));
        let down: LocalSquareHalfPlane =
            HalfPlane::from_line_and_point_on_half_plane((d, e), (0.0, -5.0));
        let left: LocalSquareHalfPlane =
            HalfPlane::from_line_and_point_on_half_plane((f, g), (-5.0, 0.0));

        let tolerance = 1e-5;

        let f = HalfPlane::overlaps_within_unit_square;
        let vars = vec![up, up_right, down_right, down, left];
        // in format of f(vars[row], vars[col]).  Should be symmetric anyway
        let correct_boolean_matrix = [
            [1, 1, 0, 0, 0],
            [1, 1, 0, 0, 0],
            [0, 0, 1, 1, 0],
            [0, 0, 1, 1, 0],
            [0, 0, 0, 0, 0],
        ];
        let actual_boolean_matrix: [[i32; 5]; 5] = from_fn(|row| {
            from_fn(|col| {
                if f(&vars[row], &vars[col], tolerance) {
                    1
                } else {
                    0
                }
            })
        });

        assert_eq!(actual_boolean_matrix, correct_boolean_matrix);
    }
}
