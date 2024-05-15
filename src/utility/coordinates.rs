use map_macro::hash_set;
use std::{
    collections::{HashMap, HashSet},
    f32::consts::{PI, TAU},
    fmt::Display,
    marker::PhantomData,
    ops::{Add, Div, Mul, Neg, Sub},
};

use typenum::{Sum, Unsigned};

use derive_more;
pub use euclid::Angle;
use itertools::Itertools;
use num::{One, Signed, Zero};
use ordered_float::OrderedFloat;
use rand::{rngs::StdRng, Rng};
use static_assertions::{assert_impl_all, assert_not_impl_any};

use crate::utility::*;

pub type IPoint = Point2D<i32, euclid::UnknownUnit>;
pub type FPoint = Point2D<f32, euclid::UnknownUnit>;
pub type IVector = Vector2D<i32, euclid::UnknownUnit>;
pub type FVector = Vector2D<f32, euclid::UnknownUnit>;

pub const DOWN_I: IVector = vec2(0, -1);
pub const UP_I: IVector = vec2(0, 1);
pub const LEFT_I: IVector = vec2(-1, 0);
pub const RIGHT_I: IVector = vec2(1, 0);

pub type FAngle = Angle<f32>;

// TODO: why does using newtypes on these cause rust-analyzer memory to skyrocket? // TODO: replace these with versions that properly incorporate addition and subtraction relativity #[derive( Clone, Copy, Hash, Eq, PartialEq, Debug, derive_more::Add, derive_more::Sub, derive_more::Neg, )]
// pub struct Point2D<DataType, UnitType>(euclid::Point2D<DataType, UnitType>);
// #[derive(Clone, Copy, Hash, Eq, PartialEq, Debug)]
// pub struct Vector2D<DataType, UnitType>(euclid::Vector2D<DataType, UnitType>);

// This is kind of hack to ignore relativity until it is re-implemented later
pub type Point2D<DataType, UnitType> = euclid::Vector2D<DataType, UnitType>;

// #[derive(Clone, Copy, Hash, Eq, PartialEq, Debug)]
// pub struct Point2D<DataType, UnitType> {
//     x: DataType,
//     y: DataType,
//     _unit: std::marker::PhantomData<UnitType>,
// }
// TODO: fix relativity
pub type Vector2D<T, U> = Point2D<T, U>;

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

// TODO: is this just a scalar?
trait_alias_macro!(pub trait CoordinateDataTypeTrait = Clone + Debug + PartialEq + num::Num + Copy + PartialOrd + Display + num::Zero + num::One + num::NumCast);

macro_rules! make_coordinate_datatype_cast_function {
    ($name:ident, $data_type:ty, $coord_type:ty) => {
        fn $name(&self) -> $coord_type {
            <$coord_type>::new(
                num::NumCast::from(self.x()).unwrap(),
                num::NumCast::from(self.y()).unwrap(),
            )
        }
    };
}

pub trait CoordinateOps:
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
{
    type DataType: CoordinateDataTypeTrait;
    type UnitType;

    type Floating: FloatCoordinateOps<UnitType = Self::UnitType, Floating = Self::Floating, OnGrid = Self::OnGrid>;
    type OnGrid: IntCoordinateOps<UnitType = Self::UnitType, Floating = Self::Floating, OnGrid = Self::OnGrid>;

    fn x(&self) -> Self::DataType;
    fn y(&self) -> Self::DataType;
    fn nth_component(&self, i: usize) -> Self::DataType {
        match i {
            0 => self.x(),
            1 => self.y(),
            _ => panic!("bad index: {i}"),
        }
    }
    fn nth_basis_vector(i: usize) -> Self {
        match i {
            0 => Self::new(Self::DataType::one(), Self::DataType::zero()),
            1 => Self::new(Self::DataType::zero(), Self::DataType::one()),
            _ => panic!("bad index: {i}"),
        }
    }
    fn new(x: Self::DataType, y: Self::DataType) -> Self;
    // TODO: delete commented code
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
        format!("(x: {}, y: {})", self.x(), self.y())
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
    fn length(&self) -> f32 {
        self.to_f32().square_length().sqrt()
    }
    // fn cast_data_type<T>(&self) -> Self<DataType=T> where T: num::NumCast{self.cast()}
    // fn cast_relativity_level<C,R>(&self) -> C where C: Coordinate<DataType=Self::DataType, UnitType=Self::UnitType, RelativityLevel = R>{self.cast()}
    // fn cast<C,T,U>(&self) -> C where C: Coordinate<DataType=T, UnitType=U> { }
    fn cast_unit<Other: CoordinateOps<DataType = Self::DataType>>(&self) -> Other {
        Other::new(self.x(), self.y())
    }
    // euclid uses fast and imprecise trig for this by default for some reason
    fn better_angle_from_x_axis(&self) -> FAngle {
        let float_self = self.to_f32();
        Angle::radians(float_self.y().atan2(float_self.x()))
    }

    // TODO: generalize
    make_coordinate_datatype_cast_function!(to_f32, f32, Self::Floating);
    make_coordinate_datatype_cast_function!(to_i32, i32, Self::OnGrid);

    fn king_length(&self) -> Self::DataType {
        let a = abs(self.x());
        let b = abs(self.y());
        max_for_partial_ord(a,b)
    }
    fn dot(&self, other: impl Into<Self>) -> Self::DataType {
        let other = other.into();
        self.x() * other.x() + self.y() * other.y()
    }
    fn cross(&self, other: Self) -> Self::DataType {
        self.x() * other.y() - self.y() * other.x()
    }
    fn projected_onto(self, onto: impl Into<Self>) -> Self {
        let onto = onto.into();
        onto * (self.dot(onto) / onto.square_length())
    }
    fn points_sorted_along_axis(points: impl IntoIterator<Item = Self>, axis: FAngle) -> impl IntoIterator<Item = Self> {
        // TODO: panic if there's a nan.
        points.into_iter().sorted_by_key(|&point|OrderedFloat(point.position_on_axis(axis)))
    }
    fn position_on_axis(&self, angle: FAngle) -> f32 {
        let cos_factor = self.better_angle_from_x_axis().angle_to(angle).radians.cos();
        self.length() * cos_factor
    }
    fn is_orthogonal(&self) -> bool {
        self.x() == Self::DataType::zero() || self.y() == Self::DataType::zero()
    }
    fn is_diagonal(&self) -> bool {
        abs(self.x()) == abs(self.y())
    }
    fn is_orthodiagonal(&self) -> bool {
        self.is_orthogonal() || self.is_diagonal()
    }
    fn is_unit_length(&self) -> bool {
        self.square_length() == Self::DataType::one()
    }
}

impl<T, U> From<NormalizedOrthoAngle> for Vector2D<T, U>
where
    T: CoordinateDataTypeTrait + num::Signed,
{
    fn from(value: NormalizedOrthoAngle) -> Self {
        let (x, y) = value.xy();
        vec2(x, y)
    }
}
impl<T, U> From<OrthogonalDirection> for Vector2D<T, U>
where
    T: CoordinateDataTypeTrait + num::Signed,
{
    fn from(value: OrthogonalDirection) -> Self {
        value.angle().into()
    }
}

impl<T, U> CoordinateOps for Vector2D<T, U>
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

pub trait UnsignedCoordinate: CoordinateOps {}
impl<T> UnsignedCoordinate for T
where
    T: CoordinateOps,
    T::DataType: num::Unsigned,
{
}

pub trait SignedCoordinateOps:
    CoordinateOps<DataType = Self::_DataType>
    + Neg<Output = Self>
    // TODO: put on the SignedCoordinateConstructor trait instead
    + From<NormalizedOrthoAngle>
    + From<OrthogonalDirection>
    + From<(Self::_DataType, Self::_DataType)>
{
    type _DataType: num::Signed + Copy + PartialOrd + Debug;
    fn flip_x(&self) -> Self {
        Self::new(-self.x(), self.y())
    }
    fn flip_y(&self) -> Self {
        Self::new(self.x(), -self.y())
    }
    fn right() -> Self {
        Self::new(Self::DataType::one(), Self::DataType::zero())
    }
    fn left() -> Self {
        Self::new(-Self::DataType::one(), Self::DataType::zero())
    }
    fn up() -> Self {
        Self::new(Self::DataType::zero(), Self::DataType::one())
    }
    fn down() -> Self {
        Self::new(Self::DataType::zero(), -Self::DataType::one())
    }
    // TODO: allow non-orthogonal directions
    fn stepped(&self, dir: OrthogonalDirection) -> Self {
        self.moved(dir, Self::DataType::one())
    }
    fn moved(&self, dir: OrthogonalDirection, length: Self::DataType) -> Self {
        *self + dir.to_step::<Self>() * length
    }
    fn position_on_orthogonal_axis(&self, axis: impl Into<OrthogonalDirection>) -> Self::DataType {
        let axis_vector: Self = axis.into().to_step();
        self.dot(axis_vector)
    }
    fn orthogonal_angle(&self) -> Result<NormalizedOrthoAngle, String> {
        <NormalizedOrthoAngle as OrthoAngleOps>::try_from_coordinate(*self)
    }
}

impl<T> SignedCoordinateOps for T
where
    T: CoordinateOps + Neg<Output = Self> + From<NormalizedOrthoAngle> + From<OrthogonalDirection>,
    T::DataType: num::Signed,
{
    type _DataType = T::DataType;
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

pub trait IntCoordinateOps:
    SignedCoordinateOps<_DataType = i32, OnGrid = Self> + Hash + Eq
{
    fn is_orthogonal_king_step(&self) -> bool {
        self.square_length() == 1
    }

    fn is_diagonal_king_step(&self) -> bool {
        self.square_length() == 2
    }
    fn is_king_step(&self) -> bool {
        self.is_orthogonal_king_step() || self.is_diagonal_king_step()
    }
    fn is_even(&self) -> bool {
        (self.x() + self.y()) % 2 == 0
    }
    fn is_odd(&self) -> bool {
        !self.is_even()
    }
}
// TODO: convert to auto trait when stable
// TODO: Same trait bounds are copy pasted from main trait declaration.  Factor them out somehow.
impl<T> IntCoordinateOps for T where T: SignedCoordinateOps<_DataType = i32, OnGrid = T> + Hash + Eq {}

trait_alias_macro!(pub trait WorldIntCoordinate = IntCoordinateOps< UnitType = SquareGridInWorldFrame>);

trait_alias_macro!(pub trait SignedIntCoordinate = IntCoordinateOps + SignedCoordinateOps);

pub trait FloatCoordinateOps: SignedCoordinateOps<_DataType = f32, Floating = Self> {
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
impl<T> FloatCoordinateOps for T where T: SignedCoordinateOps<_DataType = f32, Floating = T> {}

pub fn sign2d<U>(point: Point2D<f32, U>) -> Point2D<f32, U> {
    point2(sign_f32(point.x()), sign_f32(point.y()))
}

pub fn fraction_part<U>(point: Point2D<f32, U>) -> Point2D<f32, U> {
    point - point.round()
}

pub fn snap_angle_to_diagonal(angle: Angle<f32>) -> Angle<f32> {
    (0..4)
        .map(|i| standardize_angle_with_zero_mid(Angle::degrees(45.0 + 90.0 * i as f32)))
        .min_by_key(|&snap_angle| OrderedFloat(abs_angle_distance(snap_angle, angle).radians))
        .unwrap()
}

// TODO: make a coordinate method
pub fn get_8_octant_transforms_of<PointType: SignedCoordinateOps>(v: PointType) -> Vec<PointType> {
    let transpose = PointType::new(v.y(), v.x());
    vec![v, transpose]
        .into_iter()
        .map(|x| x.quadrant_rotations_going_ccw())
        .flatten()
        .collect()
}

impl<V> QuarterTurnRotatable for V
where
    V: SignedCoordinateOps,
{
    fn quarter_rotated_ccw(&self, angle: impl Into<NormalizedOrthoAngle>) -> Self {
        // if self.is_absolute() {
        //     return *self;
        // }
        let angle = angle.into();
        Self::new(
            self.x() * angle.cos() - self.y() * angle.sin(),
            self.x() * angle.sin() + self.y() * angle.cos(),
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

pub fn seeded_rand_radial_offset<P: FloatCoordinateOps>(rng: &mut StdRng, radius: f32) -> P {
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

pub fn revolve_square(
    moving_square: WorldSquare,
    pivot_square: WorldSquare,
    rotation: NormalizedOrthoAngle,
) -> WorldSquare {
    let rel_square = moving_square - pivot_square;
    pivot_square + rotation.rotate_vector(rel_square)
}
#[deprecated(note = "use Vector2D's to_array function instead")]
pub fn ith_projection_of_step(step: WorldStep, i: u32) -> WorldStep {
    match i {
        0 => WorldStep::new(step.x, 0),
        1 => WorldStep::new(0, step.y),
        _ => panic!("Too many dimensions: {}", i),
    }
}

#[deprecated(note = "use SignedCoordinate::position_on_axis instead")]
pub fn distance_of_step_along_axis(step: WorldStep, axis: OrthogonalDirection) -> i32 {
    step.project_onto_vector(axis.to_step()).dot(axis.to_step())
}

pub fn assert_about_eq_2d<P: FloatCoordinateOps>(p1: P, p2: P) {
    p1.check_about_eq(p2).unwrap();
}
pub fn sorted_left_to_right(faces: [OrthogonalDirection; 2]) -> [OrthogonalDirection; 2] {
    assert_ne!(faces[0], faces[1]);
    assert_ne!(faces[0], -faces[1]);
    if faces[0] == faces[1].quarter_rotated_ccw(NormalizedOrthoAngle::new_from_quarter_turns(1)) {
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

impl From<OrthogonalDirection> for KingWorldStep {
    fn from(value: OrthogonalDirection) -> Self {
        KingWorldStep::new(value.to_step())
    }
}

// TODO: generate with macro
impl QuarterTurnRotatable for KingWorldStep {
    fn quarter_rotated_ccw(&self, quarter_turns_ccw: impl Into<NormalizedOrthoAngle>) -> Self {
        self.step().quarter_rotated_ccw(quarter_turns_ccw).into()
    }
}

impl From<WorldStep> for KingWorldStep {
    fn from(value: WorldStep) -> Self {
        KingWorldStep::new(value)
    }
}

impl From<KingWorldStep> for WorldStep {
    fn from(value: KingWorldStep) -> Self {
        value.step
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

impl QuarterTurnRotatable for Angle<f32> {
    fn quarter_rotated_ccw(&self, quarter_turns_ccw: impl Into<NormalizedOrthoAngle>) -> Self {
        standardize_angle_with_zero_mid(Angle::radians(
            self.radians + PI / 2.0 * quarter_turns_ccw.into().quarter_turns() as f32,
        ))
    }
}
pub fn furthest_apart_points<P: FloatCoordinateOps>(points: Vec<P>) -> [P; 2] {
    assert!(points.len() >= 2);
    let furthest = points
        .iter()
        .combinations(2)
        .max_by_key(|two_points: &Vec<&P>| OrderedFloat((*two_points[0] - *two_points[1]).length()))
        .unwrap();
    let furthest_values: Vec<P> = furthest.into_iter().copied().collect();
    furthest_values.try_into().unwrap()
}

pub fn three_points_are_clockwise<P>(a: P, b: P, c: P) -> bool
where
    P: SignedCoordinateOps,
    P::DataType: PartialOrd, // TODO: should be implied by SignedCoordinate
{
    let ab = b - a;
    let ac = c - a;
    ab.cross(ac) < P::DataType::zero()
}

pub fn two_points_are_ccw_with_origin<P: SignedCoordinateOps>(a: P, b: P) -> bool
where
    P::DataType: PartialOrd, // TODO: should be implied by SignedCoordinate
{
    a.cross(b) > P::DataType::zero()
}

pub fn two_sorted_going_ccw(v: [WorldMove; 2]) -> [WorldMove; 2] {
    if two_points_are_ccw_with_origin(v[0], v[1]) {
        v
    } else {
        [v[1], v[0]]
    }
}

pub fn opposite_angle(a: FAngle) -> FAngle {
    a + FAngle::degrees(180.0)
}

pub fn check_vectors_in_ccw_order(
    v: impl IntoIterator<Item = impl Into<WorldMove>>,
) -> OkOrMessage {
    v.into_iter()
        .map(|x| x.into())
        .tuple_windows()
        .map(|(a, b)| match two_points_are_ccw_with_origin(a, b) {
            true => Ok(()),
            false => Err(format!(
                "These two points not in order: \na: {}\nb: {}",
                a.to_string(),
                b.to_string()
            )),
        })
        .collect()
}
pub fn on_line<P: CoordinateOps>(a: P, b: P, c: P) -> bool {
    let ab = b - a;
    let ac = c - a;
    ab.cross(ac) == P::DataType::zero()
}

pub fn on_line_in_this_order<P: FloatCoordinateOps>(a: P, b: P, c: P) -> bool {
    on_line(a, b, c) && (a - b).length() < (a - c).length()
}

pub fn point_is_in_centered_unit_square_with_tolerance<U>(
    point: impl Into<Point2D<f32, U>>,
    tolerance: f32,
) -> BoolWithPartial {
    assert!(tolerance >= 0.0);
    let vec = point.into();
    BoolWithPartial::from_less_than_with_tolerance(king_move_distance(vec), 0.5, tolerance)
}

pub fn corner_points_of_centered_unit_square<P: FloatCoordinateOps>() -> [P; 4] {
    <P::OnGrid as euclid::num::Zero>::zero().square_corners()
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
            default::Vector2D::new(0.5, 0.5)
                .better_angle_from_x_axis()
                .to_degrees(),
            45.0
        );
        assert_about_eq!(
            default::Vector2D::new(0.0, 0.5)
                .better_angle_from_x_axis()
                .to_degrees(),
            90.0
        );
        assert_about_eq!(
            default::Vector2D::new(0.0, -0.5)
                .better_angle_from_x_axis()
                .to_degrees(),
            -90.0
        );
        assert_about_eq!(
            default::Vector2D::new(1.0, 0.0)
                .better_angle_from_x_axis()
                .to_degrees(),
            0.0
        );
        assert_about_eq!(
            default::Vector2D::new(-1.0, 0.0)
                .better_angle_from_x_axis()
                .to_degrees(),
            180.0
        );
    }

    #[test]
    fn test_built_in_angle_from_x_axis_can_not_be_trusted() {
        assert!(
            (default::Vector2D::new(0.5, 0.5)
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
            standardize_angle_with_zero_mid(Angle::<f32>::degrees(75.0)).radians,
            standardize_angle_with_zero_mid(Angle::<f32>::degrees(75.0 - 360.0)).radians
        );
    }
    #[test]
    fn test_revolve_square() {
        assert_eq!(
            revolve_square(
                point2(3, 4),
                point2(5, 5),
                NormalizedOrthoAngle::new_from_quarter_turns(3),
            ),
            point2(4, 7)
        );
    }

    #[test]
    fn test_quarter_turns_from_vectors() {
        assert_eq!(
            NormalizedOrthoAngle::from_start_and_end_directions(STEP_UP, STEP_UP),
            NormalizedOrthoAngle::new_from_quarter_turns(0)
        );
        assert_eq!(
            NormalizedOrthoAngle::from_start_and_end_directions(STEP_UP, STEP_RIGHT),
            NormalizedOrthoAngle::new_from_quarter_turns(3)
        );
        assert_eq!(
            NormalizedOrthoAngle::from_start_and_end_directions(STEP_LEFT, STEP_RIGHT),
            NormalizedOrthoAngle::new_from_quarter_turns(2)
        );
        assert_eq!(
            NormalizedOrthoAngle::from_start_and_end_directions(STEP_DOWN_LEFT, STEP_DOWN_RIGHT,),
            NormalizedOrthoAngle::new_from_quarter_turns(1)
        );
    }
    #[test]
    fn test_project_step_onto_axis() {
        assert_eq!(distance_of_step_along_axis(STEP_UP_LEFT * 8, RIGHT), -8);
    }
    #[test]
    fn test_relative_points_in_ccw_order() {
        assert_true!(two_points_are_ccw_with_origin(
            STEP_RIGHT.to_f32(),
            STEP_UP.to_f32()
        ));
        assert_true!(two_points_are_ccw_with_origin(
            STEP_UP.to_f32(),
            STEP_LEFT.to_f32()
        ));
        // slight diff
        assert_true!(two_points_are_ccw_with_origin(
            STEP_UP.to_f32(),
            STEP_UP.to_f32() + WorldMove::new(-0.001, 0.0)
        ));
        assert_true!(two_points_are_ccw_with_origin(
            STEP_UP.to_f32(),
            STEP_DOWN.to_f32() + WorldMove::new(-0.001, 0.0)
        ));

        // across
        assert_false!(two_points_are_ccw_with_origin(
            STEP_UP.to_f32(),
            STEP_DOWN.to_f32()
        ));

        assert_false!(two_points_are_ccw_with_origin(
            STEP_UP.to_f32(),
            STEP_RIGHT.to_f32()
        ));
        assert_false!(two_points_are_ccw_with_origin(
            STEP_ZERO.to_f32(),
            STEP_RIGHT.to_f32()
        ));
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
    #[test]
    fn test_moving_a_point_by_rotated_direction() {
        let p: default::IntPoint = point2(3, 5);
        let dir = DOWN;
        let out = p.moved(dir.left(), 1);
        assert_eq!(out, point2(4, 5));
    }
    #[test]
    fn test_on_a_square_face() {
        [
            (0.0, 0.0, false),             // origin
            (0.5, 0.0, true),              // a side
            (0.5, 0.5, true),              // a corner
            (0.5, 0.51, true),             // near corner
            (0.49, 0.0, false),            // near_side
            (-5.5, 4.5, true),             // some other side
            (-584736.5, 40000.3, true),    // big numbers
            (-584736.45, 40000.51, false), // big numbers
        ]
        .into_iter()
        .for_each(|(x, y, on_face)| {
            assert_eq!(
                default::Point2D::new(x, y).on_a_square_face(),
                on_face,
                "point: {x}, {y}"
            );
        })
    }
    #[test]
    fn test_on_same_square_face() {
        [
            (0.0, 0.0, 0.0, 0.0, false, "same, but not on face"),
            (0.5, 0.0, 0.5, 0.1, true, "same face"),
            (0.5, 0.5, 0.5, -0.5, true, "adjacent corners"),
            (-0.5, 0.5, 0.5, -0.5, false, "opposite corners"),
            (0.5, 0.5, -0.5, -0.5, false, "other opposite corners"),
            (
                15.7,
                12.5,
                16.3,
                12.5,
                true,
                "same face, further from origin",
            ),
            (15.7, 12.5, 15.3, 12.5, false, "adjacent faces"),
        ]
        .into_iter()
        .for_each(|(x1, y1, x2, y2, same_face, case)| {
            let p1 = default::Point2D::new(x1, y1);
            let p2 = point2(x2, y2);
            assert_eq!(
                p1.on_same_square_face(p2),
                same_face,
                "{case}: {p1:?}, {p2:?}"
            );
        })
    }
    #[test]
    fn test_touched_square_faces() {
        [
            ((0.0, 0.0), hash_set![]),
            ((0.5, 0.0), hash_set![(0, 0, RIGHT), (1, 0, LEFT)]),
            ((-8.5, 0.0), hash_set![(-9, 0, RIGHT), (-8, 0, LEFT)]),
            ((0.2, 0.5), hash_set![(0, 0, UP), (0, 1, DOWN)]),
            (
                (0.5, 0.5),
                hash_set![
                    (0, 0, RIGHT),
                    (1, 0, LEFT),
                    (0, 1, RIGHT),
                    (1, 1, LEFT),
                    (0, 0, UP),
                    (0, 1, DOWN),
                    (1, 0, UP),
                    (1, 1, DOWN),
                ],
            ),
            (
                (-0.5, 0.5),
                hash_set![
                    (-1, 0, RIGHT),
                    (0, 0, LEFT),
                    (-1, 1, RIGHT),
                    (0, 1, LEFT),
                    (-1, 0, UP),
                    (-1, 1, DOWN),
                    (0, 0, UP),
                    (0, 1, DOWN),
                ],
            ),
        ]
        .into_iter()
        .for_each(|((x, y), faces)| {
            let faces: HashSet<OrthogonalFacingIntPose<WorldSquare>> = map_into(faces).collect();
            assert_eq!(WorldPoint::new(x, y).touched_square_faces(), faces);
        })
    }
}
