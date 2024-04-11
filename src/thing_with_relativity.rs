use paste::paste;
use std::marker::PhantomData;
use std::ops::{Add, Div, Mul, Sub};
use typenum::*;

use crate::utility::coordinates::{Coordinate, CoordinateDataTypeTrait};
use crate::utility::trait_alias_macro::trait_alias_macro;

trait_alias_macro!(trait RelativityLevelTrait = Unsigned + Add<U1>);

#[derive(Hash, Clone, Copy, Eq)]
pub struct ThingWithRelativity<THING_TYPE, RELATIVITY_LEVEL>
where
    RELATIVITY_LEVEL: RelativityLevelTrait,
{
    pub thing: THING_TYPE,
    pub _level_of_relativity: std::marker::PhantomData<RELATIVITY_LEVEL>,
}

impl<THING_TYPE, RELATIVITY_LEVEL> ThingWithRelativity<THING_TYPE, RELATIVITY_LEVEL>
where
    RELATIVITY_LEVEL: RelativityLevelTrait,
    // Sum<RELATIVITY_LEVEL, U1>: RelativityLevelTrait,
{
    pub fn new_thing(new_thing: THING_TYPE) -> Self {
        ThingWithRelativity {
            thing: new_thing,
            _level_of_relativity: PhantomData,
        }
    }
}

pub trait HasRelativity {
    type RelativityLevel: RelativityLevelTrait;

    fn is_absolute(&self) -> bool {
        Self::RelativityLevel::to_u32() == 0
    }

    fn is_relative(&self) -> bool {
        !self.is_absolute()
    }
    fn as_absolute<T>(&self) -> ThingWithRelativity<T, U0>;
    fn as_relative<T>(&self) -> ThingWithRelativity<T, Sum<Self::RelativityLevel, U1>>
    where
        Sum<Self::RelativityLevel, U1>: Unsigned + Add<U1>;
}

impl<THING_TYPE, RELATIVITY_LEVEL> HasRelativity
    for ThingWithRelativity<THING_TYPE, RELATIVITY_LEVEL>
where
    RELATIVITY_LEVEL: RelativityLevelTrait,
    // Sum<RELATIVITY_LEVEL, U1>: RelativityLevelTrait,
{
    type RelativityLevel = RELATIVITY_LEVEL;
    fn as_relative(&self) -> ThingWithRelativity<THING_TYPE, Sum<RELATIVITY_LEVEL, U1>>
    where
        Sum<Self::RelativityLevel, U1>: Unsigned + Add<U1>,
    {
        ThingWithRelativity::new_thing(self.thing)
    }
    fn as_absolute(&self) -> ThingWithRelativity<THING_TYPE, U0> {
        ThingWithRelativity::new_thing(self.thing)
    }
}

// TODO: uncomment this for general relativity levels once the other bugs are worked out
macro_rules! make_op_with_relativity {
    ($OpTrait:ident, $op_func:ident, $OpTraitWithRelativity:ident, $op_func_with_relativity:ident) => {
        trait $OpTraitWithRelativity<WrappedType, REL_DIFF> {
            type Output;
            fn $op_func_with_relativity(self, rhs: WrappedType) -> Self::Output;
        }

        impl<T, L_REL, R_REL, DIFF_REL> $OpTrait<ThingWithRelativity<T, R_REL>>
            for ThingWithRelativity<T, L_REL>
        where
            T: $OpTrait<T>,
            L_REL: Unsigned + Add<U1> + Add<DIFF_REL, Output = R_REL>,
            R_REL: Unsigned + Add<U1> + Sub<L_REL, Output = DIFF_REL>,
            DIFF_REL: Unsigned,
            Self: $OpTraitWithRelativity<T, DIFF_REL>,
        {
            type Output = ThingWithRelativity<T, Sum<L_REL, DIFF_REL>>;
            fn $op_func(self, rhs: ThingWithRelativity<T, R_REL>) -> Self::Output {
                self.$op_func_with_relativity(rhs.thing)
            }
        }
    };
}
macro_rules! impl_op_with_relativity {
    ($OpTrait:ident, $op_func:ident, $OpTraitWithRelativity:ident, $op_func_with_relativity:ident, $rhs_rel_diff:ty, $out_rel_diff:ty) => {
        impl<T, L_REL, R_REL> $OpTraitWithRelativity<T, $rhs_rel_diff>
            for ThingWithRelativity<T, L_REL>
        where
            L_REL: Unsigned + Add<$rhs_rel_diff, Output = R_REL> + Add<U1> + Add<U0>,
            // TODO: use meta variables for U0 and U1 here?
            Sum<L_REL, U1>: Unsigned + Add<U1>,
            Sum<L_REL, U0>: Unsigned + Add<U1>,
            R_REL: Unsigned,
            // ThingWithRelativity<T, Sum<L_REL, $out_rel_diff>>: Unsigned,
        {
            type Output = ThingWithRelativity<T, Sum<L_REL, $out_rel_diff>>;
            fn $op_func_with_relativity(self, rhs: T) -> Self::Output {
                Self::Output::new_thing(self.thing.$op_func(rhs))
            }
        }
    };
}
make_op_with_relativity!(Add, add, AddWithRelativity, add_with_relativity);
make_op_with_relativity!(Sub, sub, SubWithRelativity, sub_with_relativity);
impl_op_with_relativity!(Add, add, AddWithRelativity, add_with_relativity, U1, U0);
impl_op_with_relativity!(Sub, sub, SubWithRelativity, sub_with_relativity, U1, U0);
impl_op_with_relativity!(Sub, sub, SubWithRelativity, sub_with_relativity, U0, U1);

// macro_rules! impl_op_with_naive_relativity {
//     ($OpTrait:ident, $op_func:ident, $lhs_rel:ty, $rhs_rel:ty, $out_rel:ty) => {
//         impl<T> $OpTrait<ThingWithRelativity<T, $rhs_rel>> for ThingWithRelativity<T, $lhs_rel>
//         where
//             T: $OpTrait<T>,
//         {
//             type Output = ThingWithRelativity<T, $out_rel>;
//             fn $op_func(self, rhs: ThingWithRelativity<T, $rhs_rel>) -> Self::Output {
//                 self.thing.$op_func(rhs.thing)
//             }
//         }
//     };
// }

// // TODO: generalize
// impl_op_with_naive_relativity!(Add, add, U0, U1, U0);
// impl_op_with_naive_relativity!(Add, add, U1, U2, U1);
// impl_op_with_naive_relativity!(Add, add, U2, U3, U2);

// impl_op_with_naive_relativity!(Sub, sub, U0, U1, U0);
// impl_op_with_naive_relativity!(Sub, sub, U1, U2, U1);
// impl_op_with_naive_relativity!(Sub, sub, U2, U3, U2);

// impl_op_with_naive_relativity!(Sub, sub, U0, U0, U1);
// impl_op_with_naive_relativity!(Sub, sub, U1, U1, U2);
// impl_op_with_naive_relativity!(Sub, sub, U2, U2, U3);

impl<THING_TYPE, RELATIVITY_LEVEL> std::fmt::Debug
    for ThingWithRelativity<THING_TYPE, RELATIVITY_LEVEL>
where
    THING_TYPE: std::fmt::Debug,
    RELATIVITY_LEVEL: RelativityLevelTrait,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "ThingWithRelativity: \n\tThing: {:#?}\n\tLevel of relativity: {}",
            self.thing,
            RELATIVITY_LEVEL::to_u32()
        )
    }
}

impl<THING_TYPE, RELATIVITY_LEVEL> euclid::num::Zero
    for ThingWithRelativity<THING_TYPE, RELATIVITY_LEVEL>
where
    THING_TYPE: euclid::num::Zero,
    RELATIVITY_LEVEL: RelativityLevelTrait,
{
    fn zero() -> Self {
        ThingWithRelativity::new_thing(THING_TYPE::zero())
    }
}

impl<THING_TYPE, RELATIVITY_LEVEL> std::ops::Neg
    for ThingWithRelativity<THING_TYPE, RELATIVITY_LEVEL>
where
    THING_TYPE: std::ops::Neg,
    RELATIVITY_LEVEL: RelativityLevelTrait,
{
    type Output = ThingWithRelativity<<THING_TYPE as std::ops::Neg>::Output, RELATIVITY_LEVEL>;

    fn neg(self) -> Self::Output {
        ThingWithRelativity::new_thing(Self::neg(self.thing))
    }
}

macro_rules! impl_binary_op {
    ($OpTrait:ident, $OpFunc:ident) => {
        impl<THING_TYPE, RHS_TYPE, RELATIVITY_LEVEL> $OpTrait<RHS_TYPE>
            for ThingWithRelativity<THING_TYPE, RELATIVITY_LEVEL>
        where
            THING_TYPE: $OpTrait<RHS_TYPE>,
            RELATIVITY_LEVEL: RelativityLevelTrait,
        {
            type Output =
                ThingWithRelativity<<THING_TYPE as $OpTrait<RHS_TYPE>>::Output, RELATIVITY_LEVEL>;

            fn $OpFunc(self, rhs: RHS_TYPE) -> Self::Output {
                Self::Output::new(self.thing.$OpFunc(rhs))
            }
        }
    };
}

impl_binary_op!(Mul, mul);
impl_binary_op!(Div, div);

// TODO: Can derive? (is the phantomdata the problem?)
impl<THING_TYPE, RHS_THING_TYPE, RELATIVITY_LEVEL>
    std::cmp::PartialEq<ThingWithRelativity<RHS_THING_TYPE, RELATIVITY_LEVEL>>
    for ThingWithRelativity<THING_TYPE, RELATIVITY_LEVEL>
where
    THING_TYPE: std::cmp::PartialEq<RHS_THING_TYPE>,
    RELATIVITY_LEVEL: RelativityLevelTrait,
{
    fn eq(&self, other: &ThingWithRelativity<RHS_THING_TYPE, RELATIVITY_LEVEL>) -> bool {
        self.thing == other.thing
    }
}
impl<THING_TYPE, T, RELATIVITY_LEVEL> From<(T, T)>
    for ThingWithRelativity<THING_TYPE, RELATIVITY_LEVEL>
where
    THING_TYPE: From<(T, T)>,
    RELATIVITY_LEVEL: RelativityLevelTrait,
{
    fn from(value: (T, T)) -> Self {
        ThingWithRelativity::new_thing(THING_TYPE::from(value))
    }
}
// impl<T, U, RELATIVITY_LEVEL> Coordinate
//     for ThingWithRelativity<euclid::Vector2D<T, U>, RELATIVITY_LEVEL>
// where
//     // TODO: trait alias
//     // T: Copy + PartialEq + euclid::num::Zero + Signed + Debug + PartialOrd + Display,
//     T: CoordinateDataTypeTrait,
//     RELATIVITY_LEVEL: typenum::Unsigned + Add<typenum::U1>,
//     typenum::Sum<RELATIVITY_LEVEL, typenum::U1>: typenum::Unsigned + Add<typenum::U1>,
//     // Self: Add<Self::RelativeVersionOfSelf, Output = Self> + Sub<Self::RelativeVersionOfSelf, Output = Self>,
// {
//     type DataType = T;
//     type UnitType = U;
//     // type RelativityLevel = RELATIVITY_LEVEL;
//     // type AbsoluteVersionOfSelf = ThingWithRelativity<Vector2D<T, U>, typenum::U0>;
//     // type RelativeVersionOfSelf =
//     //     ThingWithRelativity<euclid::Vector2D<T, U>, Add1<RELATIVITY_LEVEL>>;
//     // type RelativityComplement = $relativity_complement<T, U>;

//     fn x(&self) -> T {
//         self.thing.x
//     }

//     fn y(&self) -> T {
//         self.thing.y
//     }

//     fn new(x: T, y: T) -> Self {
//         ThingWithRelativity::new_thing(euclid::Vector2D::new(x, y))
//     }

//     // fn cast<C, NEW_DATA_TYPE, NEW_UNIT_TYPE, NEW_RELATIVITY_LEVEL>(&self) -> C
//     // where
//     //     C: Coordinate<
//     //         DataType = NEW_DATA_TYPE,
//     //         UnitType = NEW_UNIT_TYPE,
//     //         RelativityLevel = NEW_RELATIVITY_LEVEL,
//     //     >,
//     // {
//     //     todo!()
//     // }
// }
