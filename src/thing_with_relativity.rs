use paste::paste;
use std::marker::PhantomData;
use std::ops::{Add, Div, Mul, Sub};
use typenum::*;

use crate::utility::trait_alias_macro::trait_alias_macro;

trait_alias_macro!(trait RelativityLevelTrait = Unsigned + Add<B1>);

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
{
    pub fn new_thing(new_thing: THING_TYPE) -> Self {
        ThingWithRelativity {
            thing: new_thing,
            _level_of_relativity: PhantomData,
        }
    }
}

pub trait HasRelativity {
    type RelativityLevel: Unsigned;
    type RelativeVersionOfSelf;
    fn relative_version_of_self(&self) -> Self::RelativeVersionOfSelf;
}

impl<T, RELATIVITY_LEVEL> HasRelativity for ThingWithRelativity<T, RELATIVITY_LEVEL>
where
    RELATIVITY_LEVEL: Unsigned + Add<B1>,
    Add1<RELATIVITY_LEVEL>: Unsigned + Add<B1>,
{
    type RelativityLevel = RELATIVITY_LEVEL;
    type RelativeVersionOfSelf = ThingWithRelativity<T, Add1<Self::RelativityLevel>>;
    fn relative_version_of_self(&self) -> Self::RelativeVersionOfSelf {
        Self::RelativeVersionOfSelf::new(self.thing)
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
            L_REL: Unsigned + Add<B1> + Add<DIFF_REL, Output = R_REL>,
            R_REL: Unsigned + Add<B1> + Sub<L_REL, Output = DIFF_REL>,
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
            L_REL: Unsigned + Add<$rhs_rel_diff, Output = R_REL> + Add<B1> + Add<U1> + Add<U0>,
            // TODO: use meta variables for U0 and U1 here?
            Sum<L_REL, U1>: Unsigned + Add<B1>,
            Sum<L_REL, U0>: Unsigned + Add<B1>,
            R_REL: Unsigned,
            // ThingWithRelativity<T, Sum<L_REL, $out_rel_diff>>: Unsigned,
        {
            type Output = ThingWithRelativity<T, Sum<L_REL, $out_rel_diff>>;
            fn $op_func_with_relativity(self, rhs: T) -> Self::Output {
                Self::Output::new(self.thing.$op_func(rhs))
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
        ThingWithRelativity::new(THING_TYPE::from(value))
    }
}
