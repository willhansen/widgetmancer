use std::{
    collections::{HashMap, HashSet},
    f32::consts::{PI, TAU},
    fmt::Display,
    marker::PhantomData,
    ops::{Add, Div, Mul, Neg, Sub},
};

use typenum::{Add1, Unsigned};

use derive_more::{AddAssign, Neg};
pub use euclid::Angle;
use itertools::Itertools;
use num::{One, Signed, Zero};
use ordered_float::OrderedFloat;
use portrait::derive_delegate;
use rand::{rngs::StdRng, Rng};
use static_assertions::{assert_impl_all, assert_not_impl_any};

use crate::thing_with_relativity::{HasRelativity, ThingWithRelativity};

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

pub type Vector2DWithRelativity<DATA_TYPE, UNIT_TYPE, RELATIVITY_LEVEL> =
    ThingWithRelativity<euclid::Vector2D<DATA_TYPE, UNIT_TYPE>, RELATIVITY_LEVEL>;

/// Intended to be a drop-in replacement for the `euclid` equivalent
pub type Point2D<DATA_TYPE, UNIT_TYPE> = Vector2DWithRelativity<DATA_TYPE, UNIT_TYPE, typenum::U0>;
/// Intended to be a drop-in replacement for the `euclid` equivalent
pub type Vector2D<DATA_TYPE, UNIT_TYPE> = Vector2DWithRelativity<DATA_TYPE, UNIT_TYPE, typenum::U1>;

// TODO: is this the right place for these two functions?
/// Intended to be a drop-in replacement for the `euclid` equivalent
pub fn point2<DATA_TYPE, UNIT_TYPE>(x: DATA_TYPE, y: DATA_TYPE) -> Point2D<DATA_TYPE, UNIT_TYPE>
where
    DATA_TYPE: CoordinateDataTypeTrait,
{
    Point2D::new(x, y)
}
/// Intended to be a drop-in replacement for the `euclid` equivalent
pub fn vec2<DATA_TYPE, UNIT_TYPE>(x: DATA_TYPE, y: DATA_TYPE) -> Vector2D<DATA_TYPE, UNIT_TYPE>
where
    DATA_TYPE: CoordinateDataTypeTrait,
{
    Vector2D::new(x, y)
}

trait_alias_macro!(pub trait AbsOrRelPoint = Copy + PartialEq + Sub<Self, Output = WorldMove>);

// TODO: is this just a scalar?
trait_alias_macro!(pub trait CoordinateDataTypeTrait = Clone + Debug + PartialEq + Signed + Copy + PartialOrd + Display + num::Zero + num::One);

macro_rules! make_to_type_function {
    ($the_type:ty, $func_name:ident) => {
        fn $func_name<C>(&self) -> C
        where
            C: Coordinate<
                DataType = $the_type,
                UnitType = Self::UnitType,
                RelativityLevel = Self::RelativityLevel,
            >,
        {
            self.cast_data_type()
        }
    };
}

pub trait Coordinate:
    Copy
    + PartialEq
    // + Add<Self::RelativeVersionOfSelf, Output = Self>
    // + Sub<Self::RelativeVersionOfSelf, Output = Self>
    // + Sub<Self, Output = Self::RelativeVersionOfSelf>
    // + Add<Vector2D<Self::DataType, Self::UnitType>, Output = Self>
    // + Sub<Vector2D<Self::DataType, Self::UnitType>, Output = Self>
    // + Sub<Self, Output = Vector2D<Self::DataType, Self::UnitType>>
    + Mul<Self::DataType, Output = Self>
    + Div<Self::DataType, Output = Self>
    + euclid::num::Zero
    + Sized
    + Debug
    + Neg<Output = Self>
    + From<(Self::DataType, Self::DataType)> // TODO: find out why this one isn't working
    + HasRelativity
where
    Self::DataType: CoordinateDataTypeTrait,
{
    type DataType;
    type UnitType;
    // type RelativityLevel: typenum::Unsigned;// + std::ops::Add<typenum::B1>;

    // const IS_RELATIVE: bool;
    // type AbsoluteVersionOfSelf: Coordinate<DataType = Self::DataType, UnitType = Self::UnitType, RelativityLevel = typenum::U0>;
    // type RelativeVersionOfSelf;//: Coordinate<DataType = Self::DataType, UnitType = Self::UnitType, RelativityLevel = typenum::Add1<Self::RelativityLevel>>;
    // type RelativityComplement: Coordinate<DataType = Self::DataType, UnitType = Self::UnitType>;

    fn x(&self) -> Self::DataType;
    fn y(&self) -> Self::DataType;
    fn new(x: Self::DataType, y: Self::DataType) -> Self;
    // TODO: delete when compiling
    // fn zero() -> Self {
    //     Self::new(Self::DataType::zero(), Self::DataType::zero())
    // }
    fn to_point<C>(&self) -> 
     C where C: Coordinate<DataType=Self::DataType, UnitType=Self::UnitType, RelativityLevel = typenum::U0>
    {
        self.as_absolute()
    }
    fn is_horizontal(&self) -> bool {
        self.x() != Self::DataType::zero() && self.y() == Self::DataType::zero()
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
    fn cast_data_type<T>(&self) -> Self<DataType=T> where T: num::NumCast{self.cast()}
    fn cast_unit<C, U>(&self) -> C where C: Coordinate<DataType = Self::DataType, UnitType = U, RelativityLevel = Self::RelativityLevel>{self.cast()}
    fn cast_relativity_level<C,R>(&self) -> C where C: Coordinate<DataType=Self::DataType, UnitType=Self::UnitType, RelativityLevel = R>{self.cast()}
    fn cast<C,T,U,R>(&self) -> C where C: Coordinate<DataType=T, UnitType=U, RelativityLevel = R>;


    make_to_type_function!(f32, to_f32);
    make_to_type_function!(u32, to_u32);

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
    fn dot(self, other: impl Into<Self>) -> Self::DataType
    {
        let other = other.into();
        self.x() * other.x() + self.y() * other.y()
    }
    fn cross(&self, other: Self) -> Self::DataType {
        self.x() * other.y() - self.y() * other.x()
    }
    fn project_onto_vector(self, onto: impl Into<Self>) -> Self
    {
        let onto = onto.into();
        onto * (self.dot(onto) / onto.square_length())
    }
}

impl<T, U, RELATIVITY_LEVEL> Coordinate
    for ThingWithRelativity<euclid::Vector2D<T, U>, RELATIVITY_LEVEL>
where
    // TODO: trait alias
    // T: Copy + PartialEq + euclid::num::Zero + Signed + Debug + PartialOrd + Display,
    T: CoordinateDataTypeTrait,
    RELATIVITY_LEVEL: typenum::Unsigned + Add<typenum::B1> + Add<typenum::U1>,
    typenum::Add1<RELATIVITY_LEVEL>: typenum::Unsigned + Add<typenum::B1>,
    typenum::Sum<RELATIVITY_LEVEL, typenum::U1>: typenum::Unsigned,
    // Self: Add<Self::RelativeVersionOfSelf, Output = Self> + Sub<Self::RelativeVersionOfSelf, Output = Self>,
{
    type DataType = T;
    type UnitType = U;
    // type RelativityLevel = RELATIVITY_LEVEL;
    // type AbsoluteVersionOfSelf = ThingWithRelativity<Vector2D<T, U>, typenum::U0>;
    // type RelativeVersionOfSelf =
    //     ThingWithRelativity<euclid::Vector2D<T, U>, Add1<RELATIVITY_LEVEL>>;
    // type RelativityComplement = $relativity_complement<T, U>;

    fn x(&self) -> T {
        self.thing.x
    }

    fn y(&self) -> T {
        self.thing.y
    }

    fn new(x: T, y: T) -> Self {
        ThingWithRelativity::new_thing(euclid::Vector2D::new(x, y))
    }

    fn cast<C, NEW_DATA_TYPE, NEW_UNIT_TYPE, NEW_RELATIVITY_LEVEL>(&self) -> C
    where
        C: Coordinate<
            DataType = NEW_DATA_TYPE,
            UnitType = NEW_UNIT_TYPE,
            RelativityLevel = NEW_RELATIVITY_LEVEL,
        >,
    {
        todo!()
    }
}

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

pub trait FloatCoordinate: Coordinate<DataType = f32> {
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
}

// TODO: convert to auto trait when stable
impl<COORDINATE_TYPE> FloatCoordinate for COORDINATE_TYPE where
    COORDINATE_TYPE: Coordinate<DataType = f32>
{
}

// TODO: Move to tests?
assert_impl_all!(Point2D<f32, euclid::UnknownUnit>: FloatCoordinate);
assert_impl_all!(Vector2D<f32, euclid::UnknownUnit>: FloatCoordinate);
assert_not_impl_any!(Point2D<i32, euclid::UnknownUnit>: FloatCoordinate);
assert_not_impl_any!(Vector2D<i32, euclid::UnknownUnit>: FloatCoordinate);

trait_alias_macro!(pub trait GridCoordinate = Coordinate<DataType = i32> + Hash + Eq);
trait_alias_macro!(pub trait WorldGridCoordinate = GridCoordinate< UnitType = SquareGridInWorldFrame>);

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

pub fn get_8_octant_transforms_of<POINT_TYPE: Coordinate>(v: POINT_TYPE) -> Vec<POINT_TYPE> {
    let transpose = POINT_TYPE::new(v.y(), v.x());
    vec![v, transpose]
        .into_iter()
        .map(|x| x.quadrant_rotations_going_ccw())
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

impl<V> QuarterTurnRotatable for V
where
    V: Coordinate, // impl<T, U> QuarterTurnRotatable for Vector2D<T, U>
                   // where
                   //     T: Copy + PartialEq + Add<Output = T> + Sub<Output = T> + Mul<Output = T> + Zero + Signed, //+ Debug,
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

pub fn seeded_rand_radial_offset(rng: &mut StdRng, radius: f32) -> euclid::default::Vector2D<f32> {
    let mut v = vec2(10.0, 10.0);
    while v.square_length() > 1.0 {
        v.x = rng.gen_range(-1.0..=1.0);
        v.y = rng.gen_range(-1.0..=1.0);
    }
    v * radius
}

pub fn rand_radial_offset(radius: f32) -> euclid::default::Vector2D<f32> {
    seeded_rand_radial_offset(&mut get_new_rng(), radius)
}
pub fn random_unit_vector() -> FVector {
    let angle = random_angle();
    unit_vector_from_angle(angle)
}

pub fn unit_vector_from_angle(angle: Angle<f32>) -> FVector {
    vec2(angle.radians.cos(), angle.radians.sin())
}

pub fn rotate_vect<U>(vector: Vector2D<f32, U>, delta_angle: Angle<f32>) -> Vector2D<f32, U> {
    if vector.length() == 0.0 {
        return vector;
    }
    let start_angle = better_angle_from_x_axis(vector);
    let new_angle = start_angle + delta_angle;
    Vector2D::<f32, U>::from_angle_and_length(new_angle, vector.length())
}
pub fn lerp2d<U>(a: Point2D<f32, U>, b: Point2D<f32, U>, t: f32) -> Point2D<f32, U> {
    point2(lerp(a.x, b.x, t), lerp(a.y, b.y, t))
}
// TODO: remember the reason for this existing (there IS a good reason)
// related to `test_built_in_angle_from_x_axis_can_not_be_trusted`
pub fn better_angle_from_x_axis<U>(v: impl Into<Vector2D<f32, U>>) -> Angle<f32> {
    let v = v.into();
    Angle::radians(v.y.atan2(v.x))
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
pub fn flip_y<T, U>(v: Vector2D<T, U>) -> Vector2D<T, U>
where
    T: Signed,
{
    vec2(v.x, -v.y)
}

pub fn flip_x<T, U>(v: Vector2D<T, U>) -> Vector2D<T, U>
where
    T: Signed,
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
        assert!(is_king_step(dir));
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
pub struct OrthogonalWorldStep {
    step: WorldStep,
}

impl OrthogonalWorldStep {
    pub fn new(dir: impl Into<WorldStep>) -> Self {
        let dir = dir.into();
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

impl<T> From<(T, T)> for OrthogonalWorldStep
where
    (T, T): Into<WorldStep>,
{
    fn from(value: (T, T)) -> Self {
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
// TODO: move RigidlyTransformable to its own file to prevent super:: imports
impl RigidlyTransformable for WorldSquare {
    fn apply_rigid_transform(&self, tf: RigidTransform) -> Self {
        revolve_square(*self, tf.start_pose.square(), tf.rotation()) + tf.translation()
    }
}
// TODO: Double check this
impl RigidlyTransformable for WorldStep {
    fn apply_rigid_transform(&self, tf: RigidTransform) -> Self {
        self.to_point().apply_rigid_transform(tf).to_vector()
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
        assert!(is_orthogonal(dir));
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

    pub fn from_start_and_end_directions(
        start: impl Into<WorldStep>,
        end: impl Into<WorldStep>,
    ) -> Self {
        let start = start.into();
        let end = end.into();
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
    pub fn rotate_vector<POINT_TYPE: Coordinate>(&self, v: POINT_TYPE) -> POINT_TYPE {
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
pub fn furthest_apart_points<U>(points: Vec<Point2D<f32, U>>) -> [Point2D<f32, U>; 2] {
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
    a: impl Into<Point2D<f32, U>>,
    b: impl Into<Point2D<f32, U>>,
    c: impl Into<Point2D<f32, U>>,
) -> bool {
    let a = a.into();
    let b = b.into();
    let c = c.into();
    let ab = b - a;
    let ac = c - a;
    ab.cross(ac) < 0.0
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
pub fn on_line<POINT_TYPE: FloatCoordinate>(
    a: impl Into<POINT_TYPE>,
    b: impl Into<POINT_TYPE>,
    c: impl Into<POINT_TYPE>,
) -> bool {
    let a: POINT_TYPE = a.into();
    let b: POINT_TYPE = b.into();
    let c: POINT_TYPE = c.into();
    let ab: POINT_TYPE::RelativeVersionOfSelf = b - a;
    let ac: POINT_TYPE::RelativeVersionOfSelf = c - a;
    ab.cross(ac) == 0.0
}

pub fn on_line_in_this_order<P: FloatCoordinate>(
    a: impl Into<P>,
    b: impl Into<P>,
    c: impl Into<P>,
) -> bool {
    let a: P = a.into();
    let b: P = b.into();
    let c: P = c.into();
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

pub fn corner_points_of_centered_unit_square<U>() -> Vec<Point2D<f32, U>> {
    vec2::<f32, U>(0.5, 0.5)
        .quadrant_rotations_going_ccw()
        .into_iter()
        .map(Vector2D::to_point)
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
            rotate_vect(WorldMove::new(0.0, 0.0), Angle::radians(PI)),
            vec2(0.0, 0.0)
        );
    }
    #[test]
    fn test_angle_from_x_axis() {
        assert_about_eq!(
            better_angle_from_x_axis(euclid::default::Vector2D::new(0.5, 0.5)).to_degrees(),
            45.0
        );
        assert_about_eq!(
            better_angle_from_x_axis(euclid::default::Vector2D::new(0.0, 0.5)).to_degrees(),
            90.0
        );
        assert_about_eq!(
            better_angle_from_x_axis(euclid::default::Vector2D::new(0.0, -0.5)).to_degrees(),
            -90.0
        );
        assert_about_eq!(
            better_angle_from_x_axis(euclid::default::Vector2D::new(1.0, 0.0)).to_degrees(),
            0.0
        );
        assert_about_eq!(
            better_angle_from_x_axis(euclid::default::Vector2D::new(-1.0, 0.0)).to_degrees(),
            180.0
        );
    }

    #[test]
    fn test_built_in_angle_from_x_axis_can_not_be_trusted() {
        assert!(
            (euclid::default::Vector2D::new(0.5, 0.5)
                .angle_from_x_axis()
                .to_degrees()
                - 45.0)
                .abs()
                > 0.01
        );
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
    fn test_relative_and_absolute() {
        let point = WorldPoint::new(4.0, 5.0);
        let moov = WorldMove::new(4.0, 5.0);
        let square = WorldSquare::new(4, 5);
        let step = WorldStep::new(4, 5);

        assert_true!(point.is_absolute());
        assert_false!(point.is_relative());
        assert_true!(moov.is_relative());
        assert_false!(moov.is_absolute());

        assert_true!(square.is_absolute());
        assert_false!(square.is_relative());
        assert_true!(step.is_relative());
        assert_false!(step.is_absolute());

        assert_eq!(point.as_relative(), moov);
        assert_eq!(point.as_absolute(), point);
        assert_eq!(moov.as_absolute(), point);
        assert_eq!(moov.as_relative(), moov);

        assert_eq!(square.as_relative(), step);
        assert_eq!(square.as_absolute(), square);
        assert_eq!(step.as_absolute(), square);
        assert_eq!(step.as_relative(), step);
    }
    #[test]
    fn test_king_length() {
        assert_eq!(WorldStep::new(2, 3).king_length(), 3);
        assert_eq!(WorldMove::new(2.0, -3.0).king_length(), 3.0);
        assert_eq!(WorldSquare::new(-2, 200).king_length(), 200);
        assert_eq!(WorldPoint::new(1.5, 3.3).king_length(), 3.3);
    }
}
