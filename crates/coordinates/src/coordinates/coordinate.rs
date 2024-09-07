use std::ops::{Add, Div, Mul, Neg, Sub};
use misc_utilities::euclid_zero_one_traits::{FancyZero, FancyOne};
use misc_utilities::trait_alias;
use angles::*;
use num;
// use std::num;

// TODO: is this just a scalar?
// Not quite.  Keeping the door open to points and relativity means that subtraction and addition
// might not be exactly among the same types.
// Multiplication between things of different units might also complicate it.
trait_alias!(pub trait DataTypeReqs = Clone + std::fmt::Debug + PartialEq + Copy + PartialOrd + std::fmt::Display + FancyZero + FancyOne);// + num::NumCast);


// macro_rules! make_coordinate_datatype_cast_function {
//     ($name:ident, $data_type:ty, $coord_type:ty) => {
//         fn $name(&self) -> $coord_type {
//             <$coord_type>::new(
//                 num::NumCast::from(self.x()).unwrap(),
//                 num::NumCast::from(self.y()).unwrap(),
//             )
//         }
//     };
// }

pub trait Coordinate:
    Copy
    + PartialEq
    + Add<Self, Output = Self>
    + Sub<Self, Output = Self>
    // + Mul<OnGridDataType<Self>, Output = Self> // TODO: stricter trait bounds
    + Mul<Self::T, Output = Self>
    + Div<Self::T, Output = Self>
    + FancyZero
    + Sized
    + std::fmt::Debug
    + std::fmt::Display
    // + From<(Self::T, Self::T)>
// + IntoIterator + FromIterator // TODO
{
    type T: DataTypeReqs;
    // type UnitType;

    // type Floating: float_coordinate::Operations<UnitType = Self::UnitType, Floating = Self::Floating, OnGrid = Self::OnGrid>;
    // type OnGrid: int_coordinate::Operations<UnitType = Self::UnitType, Floating = Self::Floating, OnGrid = Self::OnGrid>;

    fn x(&self) -> Self::T;
    fn y(&self) -> Self::T;
    fn nth_component(&self, i: usize) -> Self::T {
        match i {
            0 => self.x(),
            1 => self.y(),
            _ => panic!("bad index: {i}"),
        }
    }
    fn nth_basis_vector(i: usize) -> Self {
        match i {
            0 => Self::new(Self::T::one(), Self::T::zero()),
            1 => Self::new(Self::T::zero(), Self::T::one()),
            _ => panic!("bad index: {i}"),
        }
    }
    fn new(x: Self::T, y: Self::T) -> Self;
    // TODO: delete commented code
    // fn zero() -> Self {
    //     Self::new(Self::DataType::zero(), Self::DataType::zero())
    // }
    fn tuple(&self) -> (Self::T, Self::T) {
        (self.x(), self.y())
    }
    fn is_horizontal(&self) -> bool {
        self.x() != Self::T::zero() && self.y() == Self::T::zero()
    }
    fn to_string(&self) -> String {
        format!("(x: {}, y: {})", self.x(), self.y())
    }
    fn is_vertical(&self) -> bool {
        self.x() == Self::T::zero() && self.y() != Self::T::zero()
    }
    fn is_zero(&self) -> bool {
        *self == Self::zero()
    }
    fn square_length(&self) -> Self::T {
        self.x() * self.x() + self.y() * self.y()
    }
    fn length(&self) -> f32 {
        self.to_f32().square_length().sqrt()
    }
    // fn cast_data_type<T>(&self) -> Self<DataType=T> where T: num::NumCast{self.cast()}
    // fn cast_relativity_level<C,R>(&self) -> C where C: Coordinates<DataType=Self::DataType, UnitType=Self::UnitType, RelativityLevel = R>{self.cast()}
    // fn cast<C,T,U>(&self) -> C where C: Coordinates<DataType=T, UnitType=U> { }
    // TODO: this should be a trait
    // fn cast_unit<Other: Operations<DataType = Self::T>>(&self) -> Other {
    //     Other::new(self.x(), self.y())
    // }
    // euclid uses fast and imprecise trig for this by default for some reason
    fn better_angle_from_x_axis(&self) -> FAngle {
        let float_self = self.to_f32();
        angular_units::Rad(float_self.y().atan2(float_self.x())).into()
    }

    // TODO: generalize
    // make_coordinate_datatype_cast_function!(to_f32, f32, Self::Floating);
    // make_coordinate_datatype_cast_function!(to_i32, i32, Self::OnGrid);

    fn king_length(&self) -> Self::T {
        let a = num::traits::abs(self.x());
        let b = num::traits::abs(self.y());
        a.max(b)
        // max_for_partial_ord(a,b)
    }
    fn dot(&self, other: impl Into<Self>) -> Self::T {
        let other = other.into();
        self.x() * other.x() + self.y() * other.y()
    }
    fn cross(&self, other: Self) -> Self::T {
        self.x() * other.y() - self.y() * other.x()
    }
    fn projected_onto(self, onto: impl Into<Self>) -> Self {
        let onto = onto.into();
        onto * (self.dot(onto) / onto.square_length())
    }
    fn points_sorted_along_axis(points: impl IntoIterator<Item = Self>, axis: impl angular_units::Angle) -> impl IntoIterator<Item = Self> {
        // TODO: panic if there's a nan.
        points.into_iter().sorted_by_key(|&point|OrderedFloat(point.position_on_axis(axis)))
    }
    fn position_on_axis(&self, angle: FAngle) -> f32 {
        let cos_factor = self.better_angle_from_x_axis().angle_to(angle).radians.cos();
        self.length() * cos_factor
    }
    fn is_orthogonal(&self) -> bool {
        self.x() == Self::T::zero() || self.y() == Self::T::zero()
    }
    fn is_diagonal(&self) -> bool {
        abs(self.x()) == abs(self.y())
    }
    fn is_orthodiagonal(&self) -> bool {
        self.is_orthogonal() || self.is_diagonal()
    }
    fn is_unit_length(&self) -> bool {
        self.square_length() == Self::T::one()
    }
}
impl<T, U> Operations for Vector2D<T, U>
where
    T: DataTypeReqs,
{
    type DataType = T;

    fn x(&self) -> T {
        self.x
    }

    fn y(&self) -> T {
        self.y
    }

    fn new(x: T, y: T) -> Self {
        coord2(x, y)
    }
}

pub struct Coord2<T: DataTypeReqs>([T;2]);

pub const fn coord2<T>(x: T, y: T) -> Coord2<T>
where
    T: DataTypeReqs,
{
    Coord2([x, y])
}

