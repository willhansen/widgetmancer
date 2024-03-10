use std::{
    collections::{HashMap, HashSet},
    f32::consts::{PI, TAU},
    fmt::Display,
    marker::PhantomData,
    ops::{Add, Div, Mul, Neg, Sub},
};

use typenum::{Sum, Unsigned};

use derive_more::{AddAssign, Neg};
pub use euclid::Angle;
use itertools::Itertools;
use num::{One, Signed, Zero};
use ordered_float::OrderedFloat;
use portrait::derive_delegate;
use rand::{rngs::StdRng, Rng};
use static_assertions::{assert_impl_all, assert_not_impl_any};

// TODO: get rid of this section
use super::{
    bool_with_partial::*,
    // TODO: get rid of this line
    coordinate_frame_conversions::{
        SquareGridInWorldFrame, SquareSet, StepSet, WorldMove, WorldPoint, WorldSquare, WorldStep,
        DIAGONAL_STEPS,
    },
    general_utility::*,
    get_new_rng,
    int_cos,
    int_sin,
    int_to_T,
    lerp,
    random_angle,
    relative_interval_location::RelativeIntervalLocation,
    sign,
    trait_alias_macro::*,

    RigidTransform,
    RigidlyTransformable,
    ORTHOGONAL_STEPS,
    STEP_DOWN,
    STEP_LEFT,
    STEP_RIGHT,
    STEP_UP,
};

pub type IPoint = Point2D<i32, euclid::UnknownUnit>;
pub type FPoint = Point2D<f32, euclid::UnknownUnit>;
pub type IVector = Vector2D<i32, euclid::UnknownUnit>;
pub type FVector = Vector2D<f32, euclid::UnknownUnit>;

pub const DOWN_I: IVector = vec2(0, -1);
pub const UP_I: IVector = vec2(0, 1);
pub const LEFT_I: IVector = vec2(-1, 0);
pub const RIGHT_I: IVector = vec2(1, 0);

pub type FAngle = Angle<f32>;

// TODO: why does using newtypes on these cause rust-analyzer memory to skyrocket?
// TODO: replace these with versions that properly incorporate addition and subtraction relativity
// #[derive(Clone, Copy, Hash, Eq, PartialEq, Debug)]
// pub struct Point2D<DataType, UnitType>(euclid::Point2D<DataType, UnitType>);
// #[derive(Clone, Copy, Hash, Eq, PartialEq, Debug)]
// pub struct Vector2D<DataType, UnitType>(euclid::Vector2D<DataType, UnitType>);

// This is kind of hack to ignore relativity until it is re-implemented later
pub type Point2D<DataType, UnitType> = euclid::Vector2D<DataType, UnitType>;
pub type Vector2D<DataType, UnitType> = euclid::Vector2D<DataType, UnitType>;

pub mod default {
    pub type Point2D<T> = super::Point2D<T, euclid::UnknownUnit>;
    pub type Vector2D<T> = super::Vector2D<T, euclid::UnknownUnit>;
}

// TODO: is this the right place for these two functions?
/// Intended to be a drop-in replacement for the `euclid` equivalent
pub const fn point2<DataType, UnitType>(x: DataType, y: DataType) -> Point2D<DataType, UnitType>
where
    DataType: CoordinateDataTypeTrait,
{
    Point2D::new(x, y)
}
/// Intended to be a drop-in replacement for the `euclid` equivalent
pub const fn vec2<DataType, UnitType>(x: DataType, y: DataType) -> Vector2D<DataType, UnitType>
where
    DataType: CoordinateDataTypeTrait,
{
    Vector2D::new(x, y)
}

trait_alias_macro!(pub trait AbsOrRelPoint = Copy + PartialEq + Sub<Self, Output = WorldMove>);

// TODO: is this just a scalar?
trait_alias_macro!(pub trait CoordinateDataTypeTrait = Clone + Debug + PartialEq + num::Num + Copy + PartialOrd + Display + num::Zero + num::One + num::NumCast);

macro_rules! make_cast_function {
    ($name:ident, $data_type:ty, $coord_type:ty) => {
        fn $name(&self) -> $coord_type {
            <$coord_type>::new(
                num::NumCast::from(self.x()).unwrap(),
                num::NumCast::from(self.y()).unwrap(),
            )
        }
    };
}

pub trait Coordinate:
    Copy
    + PartialEq
    + Add<Self, Output = Self>
    + Sub<Self, Output = Self>
    + Mul<Self::DataType, Output = Self>
    + Div<Self::DataType, Output = Self>
    + euclid::num::Zero
    + Sized
    + Debug
    // + Display // TODO
    + From<(Self::DataType, Self::DataType)>
// + IntoIterator + FromIterator // TODO
where
    Self::DataType: CoordinateDataTypeTrait,
{
    type DataType;
    type UnitType;

    type Floating: FloatCoordinate<UnitType = Self::UnitType>;
    type OnGrid: IntCoordinate<UnitType = Self::UnitType>;

    fn x(&self) -> Self::DataType;
    fn y(&self) -> Self::DataType;
    fn new(x: Self::DataType, y: Self::DataType) -> Self;
    // TODO: delete when compiling
    // fn zero() -> Self {
    //     Self::new(Self::DataType::zero(), Self::DataType::zero())
    // }
    fn tuple(&self) -> (Self::DataType, Self::DataType) {
        (self.x(), self.y())
    }
    fn is_horizontal(&self) -> bool {
        self.x() != Self::DataType::zero() && self.y() == Self::DataType::zero()
    }
    fn to_string(&self) -> String {
        if self.is_absolute() {
            format!("(x: {}, y: {})", self.x(), self.y())
        }else {
            format!("(dx: {}, dy: {})", self.x(), self.y())
        }
    }
    // TODO: somehow shorten long parameter type
    fn from_any_relativity(
        other: impl Coordinate<DataType = Self::DataType, UnitType = Self::UnitType>,
    ) -> Self {
        Self::new(other.x(), other.y())
    }
    fn is_vertical(&self) -> bool {
        self.x() == Self::DataType::zero() && self.y() != Self::DataType::zero()
    }
    fn is_zero(&self) -> bool {
        *self == Self::zero()
    }
    fn square_length(&self) -> Self::DataType {
        self.x() * self.x() + self.y() * self.y()
    }
    // fn cast_data_type<T>(&self) -> Self<DataType=T> where T: num::NumCast{self.cast()}
    fn cast_unit<C, U>(&self) -> C where C: Coordinate<DataType = Self::DataType, UnitType = U>{
        C::new(self.x(), self.y())
    }
    // fn cast_relativity_level<C,R>(&self) -> C where C: Coordinate<DataType=Self::DataType, UnitType=Self::UnitType, RelativityLevel = R>{self.cast()}
    // fn cast<C,T,U>(&self) -> C where C: Coordinate<DataType=T, UnitType=U> { }
    fn cast_metadata<Other: Coordinate<DataType = Self::DataType>>(&self) -> Other {
        Other::new(self.x(), self.y())
    }

    make_cast_function!(to_f32, f32, Self::Floating);
    make_cast_function!(to_i32, i32, Self::OnGrid);

    fn king_length(&self) -> Self::DataType {
        // TODO: Why isn't there a `PartialOrd::max`?
        let a = self.x().abs();
        let b = self.y().abs();
        if a >= b {
            a
        } else {
            b
        }
    }
    fn dot(&self, other: impl Into<Self>) -> Self::DataType {
        let other = other.into();
        self.x() * other.x() + self.y() * other.y()
    }
    fn cross(&self, other: Self) -> Self::DataType {
        self.x() * other.y() - self.y() * other.x()
    }
    fn project_onto_vector(self, onto: impl Into<Self>) -> Self {
        let onto = onto.into();
        onto * (self.dot(onto) / onto.square_length())
    }
    fn is_orthogonal(&self) -> bool {
        self.x() == Self::DataType::zero() || self.y() == Self::DataType::zero()
    }
    fn is_diagonal(&self) -> bool {
        self.x() == self.y() || self.x() == self.y().neg()
    }
    fn is_orthodiagonal(&self) -> bool {
        self.is_orthogonal() || self.is_diagonal()
    }
    fn is_unit_length(&self) -> bool {
        self.square_length() == Self::DataType::one()
    }
}

impl<T, U> Coordinate for Vector2D<T, U>
where
    T: CoordinateDataTypeTrait,
{
    type DataType = T;
    type UnitType = U;

    type Floating = Vector2D<f32, U>;
    type OnGrid = Vector2D<i32, U>;

    fn x(&self) -> T {
        self.x
    }

    fn y(&self) -> T {
        self.y
    }

    fn new(x: T, y: T) -> Self {
        vec2(x, y)
    }
}

pub trait UnsignedCoordinate: Coordinate {}
impl<T> UnsignedCoordinate for T
where
    T: Coordinate,
    T::DataType: num::Unsigned,
{
}

pub trait SignedCoordinate: Coordinate + Neg<Output = Self> {}
impl<T> SignedCoordinate for T
where
    T: Coordinate + Neg<Output = Self>,
    T::DataType: num::Signed,
{
}

impl<T> From<(T, T)> for OrthogonalWorldStep
where
    (T, T): Into<WorldStep>,
{
    fn from(value: (T, T)) -> Self {
        OrthogonalWorldStep::new(value)
    }
}

// TODO: uncomment when newtyping Vector2D and Point2D
// macro_rules! impl_from_tuple {
//     ($type:ident) => {
//         impl<T, U> From<(T, T)> for $type<T, U>
//         where
//             $type<T,U>: Coordinate<DataType = T>,
//         {
//             fn from(value: (T, T)) -> Self {
//                 <$type<T,U>>::new(value.0, value.1)
//             }
//         }
//     }
// }
// impl_from_tuple!(Point2D);
// impl_from_tuple!(Vector2D);

// macro_rules! delegate_unary{
//     ($type:ident, $trait:ident, $func:ident) => {
//         impl<T,U> $trait for $type<T,U> {
//             type Output = Self;
//             fn $func(&self) -> Self {
//                 Self(self.0.$func())
//             }
//         }
//     }
// }
// delegate_unary!(Point2D, Neg, neg);
// delegate_unary!(Vector2D, Neg, neg);

// macro_rules! delegate_asymmetric_binary{
//     ($type:ident, $trait:ident, $func:ident) => {
//         impl<T,U> $trait<T> for $type<T,U> {
//             type Output = Self;
//             fn $func(&self, rhs: T) -> Self {
//                 Self($func(self.0, rhs))
//             }
//         }
//     }
// }
// delegate_asymmetric_binary!(Point2D, Mul, mul);
// delegate_asymmetric_binary!(Vector2D, Mul, mul);
// delegate_asymmetric_binary!(Point2D, Div, div);
// delegate_asymmetric_binary!(Vector2D, Div, div);

// TODO: delete commented code
// TODO: clean these up (with the trait alias macro?)
// TODO: convert to auto trait when stable
// pub trait AbsoluteCoordinate: Coordinate {}
// impl<COORD> AbsoluteCoordinate for COORD where
//     COORD: Coordinate<RelativityComplement = Self::RelativeVersionOfSelf>
// {
// }

// // TODO: convert to auto trait when stable
// pub trait RelativeCoordinate: Coordinate {}
// impl<COORD> RelativeCoordinate for COORD where
//     COORD: Coordinate<RelativityComplement = Self::AbsoluteVersionOfSelf>
// {
// }

pub trait IntCoordinate: SignedCoordinate<DataType = i32> + Hash + Eq {
    fn is_orthogonal_king_step(&self) -> bool {
        self.square_length() == 1
    }

    fn is_diagonal_king_step(&self) -> bool {
        self.square_length() == 2
    }
    fn is_king_step(&self) -> bool {
        self.is_orthogonal_king_step() || self.is_diagonal_king_step()
    }
}
// TODO: convert to auto trait when stable
// TODO: Same trait bounds are copy pasted from main trait declaration.  Factor them out somehow.
impl<T> IntCoordinate for T where T: SignedCoordinate<DataType = i32> + Hash + Eq {}

trait_alias_macro!(pub trait WorldIntCoordinate = IntCoordinate< UnitType = SquareGridInWorldFrame>);

pub trait FloatCoordinate: SignedCoordinate<DataType = f32> {
    // TODO: Add tolerance?
    fn on_centered_unit_square(&self) -> bool {
        // NOTE: 0.5 can be exactly represented by floating point numbers
        self.king_length() == 0.5
    }
    fn length(&self) -> Self::DataType {
        self.square_length().sqrt()
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
    fn lerp2d(&self, target: Self, t: f32) -> Self {
        Self::new(lerp(self.x(), target.x(), t), lerp(self.y(), target.y(), t))
    }
    // TODO: remember the reason for this existing (there IS a good reason)
    // related to `test_built_in_angle_from_x_axis_can_not_be_trusted`
    fn better_angle_from_x_axis(&self) -> Angle<f32> {
        Angle::radians(self.y().atan2(self.x()))
    }
    fn angle_to(&self, other: Self) -> Angle<f32> {
        self.better_angle_from_x_axis()
            .angle_to(other.better_angle_from_x_axis())
    }
}

// TODO: convert to auto trait when stable
impl<T> FloatCoordinate for T where T: SignedCoordinate<DataType = f32> {}

pub fn sign2d<U>(point: Point2D<f32, U>) -> Point2D<f32, U> {
    point2(sign(point.x()), sign(point.y()))
}

pub fn fraction_part<U>(point: Point2D<f32, U>) -> Point2D<f32, U> {
    (point - point.round()).to_point()
}

pub fn snap_angle_to_diagonal(angle: Angle<f32>) -> Angle<f32> {
    (0..4)
        .map(|i| standardize_angle(Angle::degrees(45.0 + 90.0 * i as f32)))
        .min_by_key(|&snap_angle| OrderedFloat(abs_angle_distance(snap_angle, angle).radians))
        .unwrap()
}

// TODO: make a coordinate method
pub fn get_8_octant_transforms_of<PointType: SignedCoordinate>(v: PointType) -> Vec<PointType> {
    let transpose = PointType::new(v.y(), v.x());
    vec![v, transpose]
        .into_iter()
        .map(|x| x.quadrant_rotations_going_ccw())
        .flatten()
        .collect()
}

impl<V> QuarterTurnRotatable for V
where
    V: SignedCoordinate,
{
    fn quarter_rotated_ccw(&self, quarter_turns_ccw: impl Into<QuarterTurnsCcw>) -> Self {
        // if self.is_absolute() {
        //     return *self;
        // }
        let quarter_turns: i32 = quarter_turns_ccw.into().quarter_turns;
        Self::new(
            self.x() * int_to_T(int_cos(quarter_turns))
                - self.y() * int_to_T(int_sin(quarter_turns)),
            self.x() * int_to_T(int_sin(quarter_turns))
                + self.y() * int_to_T(int_cos(quarter_turns)),
        )
    }
}

pub fn reversed<T: Copy>(v: Vec<T>) -> Vec<T> {
    let mut new_v = v.clone();
    new_v.reverse();
    new_v
}
#[deprecated(note = "Coordinate::king_length instead")]
pub fn king_step_distance<U>(step: Vector2D<i32, U>) -> u32 {
    step.x().abs().max(step.y().abs()) as u32
}
#[deprecated(note = "Coordinate::king_length instead")]
pub fn king_move_distance<U>(step: Vector2D<f32, U>) -> f32 {
    step.x().abs().max(step.y().abs())
}

pub fn round_to_king_step(step: WorldStep) -> WorldStep {
    if step.square_length() == 0 {
        return step;
    }
    let radians_from_plus_x = step.to_f32().better_angle_from_x_axis();
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

pub fn seeded_rand_radial_offset<P: FloatCoordinate>(rng: &mut StdRng, radius: f32) -> P {
    let mut v = P::new(10.0, 10.0);
    while v.square_length() > 1.0 {
        v = P::new(rng.gen_range(-1.0..=1.0), rng.gen_range(-1.0..=1.0));
    }
    v * radius
}

pub fn rand_radial_offset(radius: f32) -> default::Vector2D<f32> {
    seeded_rand_radial_offset(&mut get_new_rng(), radius)
}
pub fn random_unit_vector() -> FVector {
    let angle = random_angle();
    FVector::unit_vector_from_angle(angle)
}

pub fn standardize_angle(angle: Angle<f32>) -> Angle<f32> {
    let mut radians = angle.radians;
    if radians > -PI && radians <= PI {
        angle
    } else {
        radians = radians.rem_euclid(TAU);
        if radians > PI {
            radians -= TAU;
        }
        Angle::radians(radians)
    }
}

pub fn abs_angle_distance(a: Angle<f32>, b: Angle<f32>) -> Angle<f32> {
    Angle::radians(
        standardize_angle(a)
            .angle_to(standardize_angle(b))
            .radians
            .abs(),
    )
}

pub fn revolve_square(
    moving_square: WorldSquare,
    pivot_square: WorldSquare,
    rotation: QuarterTurnsCcw,
) -> WorldSquare {
    let rel_square = moving_square - pivot_square;
    pivot_square + rotation.rotate_vector(rel_square)
}
// TODO: convert to Coordinate::flip_y
pub fn flip_y<T, U>(v: Vector2D<T, U>) -> Vector2D<T, U>
where
    T: CoordinateDataTypeTrait,
{
    vec2(v.x, -v.y)
}

// TODO: convert to Coordinate::flip_x
pub fn flip_x<T, U>(v: Vector2D<T, U>) -> Vector2D<T, U>
where
    T: CoordinateDataTypeTrait,
{
    vec2(-v.x, v.y)
}
#[deprecated(note = "use Vector2D's to_array function instead")]
pub fn ith_projection_of_step(step: WorldStep, i: u32) -> WorldStep {
    match i {
        0 => WorldStep::new(step.x, 0),
        1 => WorldStep::new(0, step.y),
        _ => panic!("Too many dimensions: {}", i),
    }
}

pub fn distance_of_step_along_axis(step: WorldStep, axis: OrthogonalWorldStep) -> i32 {
    step.project_onto_vector(axis.step).dot(axis.step)
}
pub fn square_is_odd(square: WorldSquare) -> bool {
    (square.x + square.y) % 2 == 1
}
pub fn square_is_even(square: WorldSquare) -> bool {
    !square_is_odd(square)
}
pub fn about_eq_2d<P: AbsOrRelPoint>(p1: P, p2: P, tolerance: f32) -> bool {
    (p1 - p2).length().abs() < tolerance
}
pub fn check_about_eq_2d<P: AbsOrRelPoint + Debug>(p1: P, p2: P) -> OkOrMessage {
    let tolerance = 0.001; // TODO: make parameter

    if about_eq_2d(p1, p2, tolerance) {
        Ok(())
    } else {
        Err(format!(
            "\nPoints too far apart:\n\tp1: {:?}\n\tp2: {:?}\n",
            p1, p2
        ))
    }
}

pub fn assert_about_eq_2d<P: AbsOrRelPoint + Debug>(p1: P, p2: P) {
    check_about_eq_2d(p1, p2).unwrap();
}
pub fn sorted_left_to_right(faces: [OrthogonalWorldStep; 2]) -> [OrthogonalWorldStep; 2] {
    assert_ne!(faces[0], faces[1]);
    assert_ne!(faces[0], -faces[1]);
    if faces[0] == faces[1].quarter_rotated_ccw(QuarterTurnsCcw::new(1)) {
        faces
    } else {
        [faces[1], faces[0]]
    }
}

#[derive(Clone, Hash, Eq, PartialEq, Debug, Copy)]
pub struct KingWorldStep {
    step: WorldStep,
}

impl KingWorldStep {
    pub fn new(dir: WorldStep) -> Self {
        assert!(dir.is_king_step());
        KingWorldStep { step: dir }
    }
    pub fn step(&self) -> WorldStep {
        self.step
    }
}

// TODO: generate with macro
impl QuarterTurnRotatable for KingWorldStep {
    fn quarter_rotated_ccw(&self, quarter_turns_ccw: impl Into<QuarterTurnsCcw> + Copy) -> Self {
        self.step().quarter_rotated_ccw(quarter_turns_ccw).into()
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

#[derive(Clone, Hash, Neg, Eq, PartialEq, Debug, Copy, Default)]
pub struct OrthogonalUnitCoordinate<T: SignedCoordinate> {
    step: T,
}

pub type OrthogonalWorldStep = OrthogonalUnitCoordinate<WorldStep>;

impl<P: SignedCoordinate> OrthogonalUnitCoordinate<P> {
    pub fn new(dir: impl Into<P>) -> Self {
        let dir = dir.into();
        assert!(dir.is_unit_length());
        assert!(dir.is_orthogonal());
        OrthogonalUnitCoordinate { step: dir }
    }
    pub fn step(&self) -> P {
        self.step
    }
    pub fn pos_on_axis(&self, pos: P) -> P::DataType {
        self.step().dot(pos)
    }
}

// TODO: generate with macro
impl QuarterTurnRotatable for OrthogonalWorldStep {
    fn quarter_rotated_ccw(&self, quarter_turns_ccw: impl Into<QuarterTurnsCcw> + Copy) -> Self {
        self.step().quarter_rotated_ccw(quarter_turns_ccw).into()
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
pub fn adjacent_king_steps(dir: WorldStep) -> StepSet {
    assert!(dir.is_king_step());
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
// TODO: move RigidlyTransformable to its own file to prevent super:: imports
impl RigidlyTransformable for WorldSquare {
    fn apply_rigid_transform(&self, tf: RigidTransform) -> Self {
        revolve_square(*self, tf.start_pose.square(), tf.rotation()) + tf.translation()
    }
}
impl RigidlyTransformable for OrthogonalWorldStep {
    fn apply_rigid_transform(&self, tf: RigidTransform) -> Self {
        tf.rotation().rotate_vector(self.step).into()
    }
}

#[derive(Hash, Default, Debug, Copy, Clone, Eq, PartialEq, getset::CopyGetters, AddAssign)]
#[get_copy = "pub"]
pub struct QuarterTurnsCcw {
    quarter_turns: i32,
}

impl QuarterTurnsCcw {
    pub fn new(quarter_turns: i32) -> Self {
        QuarterTurnsCcw {
            quarter_turns: quarter_turns.rem_euclid(4),
        }
    }
    pub fn to_orthogonal_direction(&self) -> WorldStep {
        STEP_RIGHT.quarter_rotated_ccw(self.quarter_turns)
    }
    pub fn all_4() -> impl Iterator<Item = Self> + Clone {
        (0..4).map(|x| x.into())
    }
    pub fn from_vector(dir: WorldStep) -> Self {
        assert!(dir.is_orthogonal());
        QuarterTurnsCcw::new(if dir.x == 0 {
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

    pub fn from_start_and_end_directions<P: IntCoordinate>(start: P, end: P) -> Self {
        assert!(start.is_king_step());
        assert!(end.is_king_step());
        // needs to be quarter turn, no eighths
        assert_eq!(start.is_diagonal(), end.is_diagonal());

        let d_angle = start.to_f32().angle_to(end.to_f32());
        let quarter_turns = (d_angle.to_degrees() / 90.0).round() as i32;
        Self::new(quarter_turns)
    }

    pub fn rotate_angle(&self, angle: Angle<f32>) -> Angle<f32> {
        standardize_angle(Angle::<f32>::degrees(
            angle.to_degrees() + 90.0 * (self.quarter_turns() as f32),
        ))
    }
    pub fn rotate_vector<PointType: SignedCoordinate>(&self, v: PointType) -> PointType {
        v.quarter_rotated_ccw(self.quarter_turns)
    }
}

impl Neg for QuarterTurnsCcw {
    type Output = Self;

    fn neg(self) -> Self::Output {
        QuarterTurnsCcw::new(-self.quarter_turns)
    }
}

impl Add for QuarterTurnsCcw {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        Self::new(self.quarter_turns() + rhs.quarter_turns())
    }
}

impl Sub for QuarterTurnsCcw {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        Self::new(self.quarter_turns() - rhs.quarter_turns())
    }
}

impl From<i32> for QuarterTurnsCcw {
    fn from(value: i32) -> Self {
        Self::new(value)
    }
}

impl QuarterTurnRotatable for QuarterTurnsCcw {
    fn quarter_rotated_ccw(&self, quarter_turns_ccw: impl Into<QuarterTurnsCcw> + Copy) -> Self {
        *self + quarter_turns_ccw.into()
    }
}

#[portrait::make()]
pub trait QuarterTurnRotatable {
    // TODO: pass reference?
    fn quarter_rotated_ccw(&self, quarter_turns_ccw: impl Into<QuarterTurnsCcw> + Copy) -> Self;
    #[portrait(derive_delegate(reduce = |s,x|{x}))] // TODO: understand
    fn quadrant_rotations_going_ccw(&self) -> [Self; 4]
    where
        Self: Sized + Debug,
    {
        (0..4)
            .into_iter()
            .map(|i| self.quarter_rotated_ccw(i))
            .collect_vec()
            .try_into()
            .unwrap()
    }
}

// TODO: generate with macro
impl<T> QuarterTurnRotatable for Vec<T>
where
    T: QuarterTurnRotatable,
{
    fn quarter_rotated_ccw(&self, quarter_turns_ccw: impl Into<QuarterTurnsCcw> + Copy) -> Self {
        self.iter()
            .map(|t| t.quarter_rotated_ccw(quarter_turns_ccw))
            .collect()
    }
}

// TODO: generate with macro
impl<T> QuarterTurnRotatable for Option<T>
where
    T: QuarterTurnRotatable,
{
    fn quarter_rotated_ccw(&self, quarter_turns_ccw: impl Into<QuarterTurnsCcw> + Copy) -> Self {
        self.as_ref()
            .map(|x| x.quarter_rotated_ccw(quarter_turns_ccw))
    }
}

impl QuarterTurnRotatable for Angle<f32> {
    fn quarter_rotated_ccw(&self, quarter_turns_ccw: impl Into<QuarterTurnsCcw> + Copy) -> Self {
        standardize_angle(Angle::radians(
            self.radians + PI / 2.0 * quarter_turns_ccw.into().quarter_turns as f32,
        ))
    }
}
pub fn furthest_apart_points<P: FloatCoordinate>(points: Vec<P>) -> [P; 2] {
    assert!(points.len() >= 2);
    let furthest = points
        .iter()
        .combinations(2)
        .max_by_key(|two_points: &Vec<&P>| OrderedFloat((*two_points[0] - *two_points[1]).length()))
        .unwrap();
    let furthest_values: Vec<P> = furthest.into_iter().copied().collect();
    furthest_values.try_into().unwrap()
}

pub fn three_points_are_clockwise<P: SignedCoordinate>(a: P, b: P, c: P) -> bool {
    let ab = b - a;
    let ac = c - a;
    ab.cross(ac) < P::DataType::zero()
}

pub fn two_in_ccw_order(a: WorldMove, b: WorldMove) -> bool {
    a.cross(b) > 0.0
}

pub fn two_sorted_going_ccw(v: [WorldMove; 2]) -> [WorldMove; 2] {
    if two_in_ccw_order(v[0], v[1]) {
        v
    } else {
        [v[1], v[0]]
    }
}

pub fn opposite_angle(a: FAngle) -> FAngle {
    a + FAngle::degrees(180.0)
}

pub fn check_vectors_in_ccw_order(
    v: impl IntoIterator<Item = impl Into<WorldMove> + Copy>,
) -> OkOrMessage {
    v.into_iter()
        .map(|x| x.into())
        .tuple_windows()
        .map(|(a, b)| match two_in_ccw_order(a, b) {
            true => Ok(()),
            false => Err(format!(
                "These two points not in order: \na: {}\nb: {}",
                a.to_string(),
                b.to_string()
            )),
        })
        .collect()
}
pub fn on_line<P: FloatCoordinate>(a: P, b: P, c: P) -> bool {
    let ab = b - a;
    let ac = c - a;
    ab.cross(ac) == 0.0
}

pub fn on_line_in_this_order<P: FloatCoordinate>(a: P, b: P, c: P) -> bool {
    on_line(a, b, c) && (a - b).length() < (a - c).length()
}

pub fn point_is_in_centered_unit_square_with_tolerance<U>(
    point: impl Into<Point2D<f32, U>>,
    tolerance: f32,
) -> BoolWithPartial {
    assert!(tolerance >= 0.0);
    let vec = point.into().to_vector();
    BoolWithPartial::from_less_than_with_tolerance(king_move_distance(vec), 0.5, tolerance)
}

pub fn corner_points_of_centered_unit_square<P: FloatCoordinate>() -> Vec<P> {
    P::Relative::new(0.5, 0.5)
        .quadrant_rotations_going_ccw()
        .into_iter()
        .map(|v| P::zero() + v)
        .collect()
}

#[cfg(test)]
mod tests {

    use ntest::{assert_about_eq, assert_false, assert_true, timeout};
    use pretty_assertions::{assert_eq, assert_ne};

    use crate::utility::{STEP_DOWN_LEFT, STEP_DOWN_RIGHT, STEP_UP_LEFT, STEP_UP_RIGHT, STEP_ZERO};

    use super::*;
    #[test]
    fn test_round_to_kingstep() {
        assert_eq!(
            WorldStep::new(0, 0),
            round_to_king_step(WorldStep::new(0, 0)),
            "zero to zero"
        );
        assert_eq!(
            WorldStep::new(1, 0),
            round_to_king_step(WorldStep::new(5, 0)),
            "reduce length"
        );
        assert_eq!(
            WorldStep::new(0, -1),
            round_to_king_step(WorldStep::new(5, -300)),
            "snap to orthogonal"
        );
        assert_eq!(
            WorldStep::new(-1, 1),
            round_to_king_step(WorldStep::new(-30, 25)),
            "snap to diagonal"
        );
    }
    #[test]
    fn test_clockwise() {
        assert!(three_points_are_clockwise::<WorldPoint>(
            point2(0.0, 0.0),
            point2(0.0, 1.0),
            point2(1.0, 0.0),
        ));
        assert_false!(three_points_are_clockwise::<WorldPoint>(
            point2(0.0, 0.0),
            point2(0.0, 1.0),
            point2(-0.1, -10.0)
        ));
    }
    #[test]
    fn test_adjacent_king_steps() {
        assert_eq!(
            adjacent_king_steps(STEP_UP),
            vec![STEP_UP_RIGHT, STEP_UP_LEFT].into_iter().collect()
        );
        assert_eq!(
            adjacent_king_steps(STEP_RIGHT),
            vec![STEP_UP_RIGHT, STEP_DOWN_RIGHT].into_iter().collect()
        );
        assert_eq!(
            adjacent_king_steps(STEP_DOWN_LEFT),
            vec![STEP_DOWN, STEP_LEFT].into_iter().collect()
        );
    }

    #[test]
    fn test_rotate_zero_vector() {
        assert_eq!(
            WorldMove::new(0.0, 0.0).rotate_vect(Angle::radians(PI)),
            vec2(0.0, 0.0)
        );
    }
    #[test]
    fn test_angle_from_x_axis() {
        assert_about_eq!(
            Vector2D::new(0.5, 0.5)
                .better_angle_from_x_axis()
                .to_degrees(),
            45.0
        );
        assert_about_eq!(
            Vector2D::new(0.0, 0.5)
                .better_angle_from_x_axis()
                .to_degrees(),
            90.0
        );
        assert_about_eq!(
            Vector2D::new(0.0, -0.5)
                .better_angle_from_x_axis()
                .to_degrees(),
            -90.0
        );
        assert_about_eq!(
            Vector2D::new(1.0, 0.0)
                .better_angle_from_x_axis()
                .to_degrees(),
            0.0
        );
        assert_about_eq!(
            Vector2D::new(-1.0, 0.0)
                .better_angle_from_x_axis()
                .to_degrees(),
            180.0
        );
    }

    #[test]
    fn test_built_in_angle_from_x_axis_can_not_be_trusted() {
        assert!((Vector2D::new(0.5, 0.5).angle_from_x_axis().to_degrees() - 45.0).abs() > 0.01);
    }

    #[test]
    fn test_standardize_angle() {
        assert_about_eq!(
            standardize_angle(Angle::<f32>::degrees(75.0)).radians,
            standardize_angle(Angle::<f32>::degrees(75.0 - 360.0)).radians
        );
    }
    #[test]
    fn test_revolve_square() {
        assert_eq!(
            revolve_square(point2(3, 4), point2(5, 5), QuarterTurnsCcw::new(3),),
            point2(4, 7)
        );
    }

    #[test]
    fn test_quarter_turns_from_vectors() {
        assert_eq!(
            QuarterTurnsCcw::from_start_and_end_directions(STEP_UP, STEP_UP),
            QuarterTurnsCcw::new(0)
        );
        assert_eq!(
            QuarterTurnsCcw::from_start_and_end_directions(STEP_UP, STEP_RIGHT),
            QuarterTurnsCcw::new(3)
        );
        assert_eq!(
            QuarterTurnsCcw::from_start_and_end_directions(STEP_LEFT, STEP_RIGHT),
            QuarterTurnsCcw::new(2)
        );
        assert_eq!(
            QuarterTurnsCcw::from_start_and_end_directions(STEP_DOWN_LEFT, STEP_DOWN_RIGHT,),
            QuarterTurnsCcw::new(1)
        );
    }
    #[test]
    fn test_project_step_onto_axis() {
        assert_eq!(
            distance_of_step_along_axis(STEP_UP_LEFT * 8, STEP_RIGHT.into()),
            -8
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
    fn test_point_is_in_centered_unit_square__simple_true() {
        assert!(
            point_is_in_centered_unit_square_with_tolerance(WorldPoint::new(0.0, 0.0), 0.0)
                .is_true()
        )
    }
    #[test]
    fn test_point_is_in_centered_unit_square__simple_false() {
        assert!(
            point_is_in_centered_unit_square_with_tolerance(WorldPoint::new(5.0, 0.0), 0.0)
                .is_false()
        )
    }
    #[test]
    fn test_point_is_in_centered_unit_square__outside_square__inside_tolerance() {
        assert!(
            point_is_in_centered_unit_square_with_tolerance(WorldPoint::new(0.51, 0.0), 0.2)
                .is_partial()
        )
    }
    #[test]
    fn test_point_is_in_centered_unit_square__inside_square__inside_tolerance() {
        assert!(
            point_is_in_centered_unit_square_with_tolerance(WorldPoint::new(0.49, 0.0), 0.2)
                .is_partial()
        )
    }
    #[test]
    fn test_point_is_in_centered_unit_square__outside_square_diagonally__inside_tolerance() {
        assert!(
            point_is_in_centered_unit_square_with_tolerance(WorldPoint::new(0.51, 0.51), 0.2)
                .is_partial()
        )
    }
    #[test]
    fn test_point_is_in_centered_unit_square__inside_square_diagonally__inside_tolerance() {
        assert!(
            point_is_in_centered_unit_square_with_tolerance(WorldPoint::new(0.49, 0.49), 0.2)
                .is_partial()
        )
    }
    #[test]
    #[ignore = "Relativity is unimplemented for coordinates for the time being"]
    fn test_relative_and_absolute() {
        let point = WorldPoint::new(4.0, 5.0);
        let moov = WorldMove::new(4.0, 5.0);
        let square = WorldSquare::new(4, 5);
        let step = WorldStep::new(4, 5);

        // TODO: uncomment when relativity is re-implemented
        // assert_true!(point.is_absolute());
        // assert_false!(point.is_relative());
        // assert_true!(moov.is_relative());
        // assert_false!(moov.is_absolute());

        // assert_true!(square.is_absolute());
        // assert_false!(square.is_relative());
        // assert_true!(step.is_relative());
        // assert_false!(step.is_absolute());

        // assert_eq!(point.as_relative(), moov);
        // assert_eq!(point.as_absolute(), point);
        // assert_eq!(moov.as_absolute(), point);
        // assert_eq!(moov.as_relative(), moov);

        // assert_eq!(square.as_relative(), step);
        // assert_eq!(square.as_absolute(), square);
        // assert_eq!(step.as_absolute(), square);
        // assert_eq!(step.as_relative(), step);
    }
    #[test]
    fn test_king_length() {
        assert_eq!(WorldStep::new(2, 3).king_length(), 3);
        assert_eq!(WorldMove::new(2.0, -3.0).king_length(), 3.0);
        assert_eq!(WorldSquare::new(-2, 200).king_length(), 200);
        assert_eq!(WorldPoint::new(1.5, 3.3).king_length(), 3.3);
    }
}
