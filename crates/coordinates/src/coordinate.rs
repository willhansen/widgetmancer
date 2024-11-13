use angles::FAngle;
use geo::coord;
use geo::Coord;
use itertools::Itertools;
use misc_utilities::*;
use ordered_float::OrderedFloat;
use crate::float_coordinate;
use num::Zero;
use num::One;
use num::ToPrimitive;
use std::ops::{Add, Div, Mul, Neg, Sub};
use crate::FCoord;
use crate::ICoord;
// use std::num;

// TODO: is this just a scalar?
trait_alias!(pub trait DataTypeReqs = geo::CoordNum + AbsThatWorksWithUnsigned + MinMaxThatWorkWithPartialOrd + std::fmt::Display);

pub type Coord2<T> = geo::Coord<T>;


pub fn coord<T: DataTypeReqs>(x: T, y: T) -> Coord2<T> {
    coord! {x: x, y: y}
}

pub fn coord2<T: DataTypeReqs>(x: T, y: T) -> Coord2<T> {
    coord(x, y)
}

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

pub trait Operations:
    Copy
    + PartialEq
    + Add<Self, Output = Self>
    + Sub<Self, Output = Self>
    // + Mul<OnGridDataType<Self>, Output = Self> // TODO: stricter trait bounds
    + Mul<Self::DataType, Output = Self>
    + Div<Self::DataType, Output = Self>
    + Zero
    // + One // TODO
    + Sized
    + std::fmt::Debug
    // + std::fmt::Display
    // + geo::CoordNum
    // + From<(Self::T, Self::T)>
// + IntoIterator + FromIterator // TODO
{
    type DataType: DataTypeReqs;
    // type UnitType;

    // type Floating: float_coordinate::Operations<UnitType = Self::UnitType, Floating = Self::Floating, OnGrid = Self::OnGrid>;
    // type OnGrid: int_coordinate::Operations<UnitType = Self::UnitType, Floating = Self::Floating, OnGrid = Self::OnGrid>;

    fn x(&self) -> Self::DataType;
    fn y(&self) -> Self::DataType;
    // TODO: Should be trait
    fn one() -> Self {
        Self::new(Self::DataType::one(), Self::DataType::one())
    }
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
    fn to_string(&self) -> String where <Self as Operations>::DataType: std::fmt::Display {
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
        self.square_length().to_f32().unwrap().sqrt()
    }
    fn to_f32(&self) -> Option<FCoord> {
        let x = self.x().to_f32();
        let y = self.y().to_f32();
        if x.is_none() || y.is_none() {
            Some(coord2(x.unwrap(), y.unwrap()))
        } else {
            None
        }
    }
    fn to_i32(&self) -> Option<ICoord> {
        let x = self.x().to_i32();
        let y = self.y().to_i32();
        if x.is_none() || y.is_none() {
            Some(coord2(x.unwrap(), y.unwrap()))
        } else {
            None
        }
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
        let float_self = self.to_f32().unwrap();
        FAngle::from_rad(float_self.y().atan2(float_self.x()))
    }

    // TODO: generalize
    // make_coordinate_datatype_cast_function!(to_f32, f32, Self::Floating);
    // make_coordinate_datatype_cast_function!(to_i32, i32, Self::OnGrid);

    fn king_length(&self) -> Self::DataType {
        let a = self.x().abs_that_works_with_unsigned();
        let b = self.y().abs_that_works_with_unsigned();
        a.max_that_works_with_partial_ord(b)
        // max_for_partial_ord(a,b)
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
        let cos_factor = self.better_angle_from_x_axis().dot(angle);
        self.length() * cos_factor
    }
    fn is_orthogonal(&self) -> bool {
        self.x() == Self::DataType::zero() || self.y() == Self::DataType::zero()
    }
    fn is_diagonal(&self) -> bool {
        self.x().abs_that_works_with_unsigned() == self.y().abs_that_works_with_unsigned()
    }
    fn is_orthodiagonal(&self) -> bool {
        self.is_orthogonal() || self.is_diagonal()
    }
    fn is_unit_length(&self) -> bool {
        self.square_length() == Self::DataType::one()
    }
}


impl<DataType> Operations for Coord<DataType>
where
    DataType: DataTypeReqs,
{
    type DataType = DataType;

    fn x(&self) -> Self::DataType {
        self.x
    }

    fn y(&self) -> Self::DataType {
        self.y
    }

    fn new(x: Self::DataType, y: Self::DataType) -> Self {
        coord2(x, y)
    }
}
pub fn on_line<P: Operations>(a: P, b: P, c: P) -> bool {
    let ab = b - a;
    let ac = c - a;
    ab.cross(ac) == P::DataType::zero()
}

pub fn two_points_are_ccw_with_origin<P: Operations>(a: P, b: P) -> BoolWithPartial
where
    P::DataType: PartialOrd, // TODO: should be implied by SignedCoordinate
{
    BoolWithPartial::greater_than( a.cross(b) , P::DataType::zero())
}

pub fn two_sorted_going_ccw<P: Operations>(v: [P; 2]) -> [P; 2] {
    if two_points_are_ccw_with_origin(v[0], v[1]).is_false() {
        v
    } else {
        [v[1], v[0]]
    }
}

pub fn check_vectors_in_ccw_order<T: DataTypeReqs>(
    v: impl IntoIterator<Item = Coord2<T>>,
) -> OkOrMessage {
    v.into_iter()
        .tuple_windows()
        .map(|(a, b)| match two_points_are_ccw_with_origin(a, b) {
            BoolWithPartial::True => Ok(()),
            _ => Err(format!(
                "These two points not in order: \na: {}\nb: {}",
                a.to_string(),
                b.to_string()
            )),
        })
        .collect()
}
pub fn on_line_in_this_order<P: Operations>(a: P, b: P, c: P) -> bool {
    on_line(a, b, c) && (a - b).length() < (a - c).length()
}

pub fn point_is_in_centered_unit_square_with_tolerance(
    point: impl Into<Coord2<f32>>,
    tolerance: f32,
) -> BoolWithPartial {
    assert!(tolerance >= 0.0);
    let vec = point.into();
    BoolWithPartial::less_than_with_tolerance(vec.king_length(), 0.5, tolerance)
}
