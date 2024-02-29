use paste::paste;
use std::marker::PhantomData;
use std::ops::{Add, Sub};
use typenum::*;

#[derive(Hash, Clone, Copy, Eq)]
pub struct ThingWithRelativity<THING_TYPE, RELATIVITY_LEVEL>
where
    RELATIVITY_LEVEL: Unsigned,
{
    pub thing: THING_TYPE,
    pub _level_of_relativity: std::marker::PhantomData<RELATIVITY_LEVEL>,
}

impl<THING_TYPE, RELATIVITY_LEVEL> ThingWithRelativity<THING_TYPE, RELATIVITY_LEVEL>
where
    RELATIVITY_LEVEL: Unsigned,
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
}

impl<T, RELATIVITY_LEVEL> HasRelativity for ThingWithRelativity<T, RELATIVITY_LEVEL>
where
    RELATIVITY_LEVEL: typenum::Unsigned,
{
    type RelativityLevel = RELATIVITY_LEVEL;
}

// TODO: uncomment this for general relativity levels once the other bugs are worked out
macro_rules! make_op_with_relativity {
    ($OpTrait:ident, $op_func:ident, $OpTraitWithRelativity:ident, $op_func_with_relativity:ident) => {
        trait $OpTraitWithRelativity<Rhs, REL_DIFF>
        where
            REL_DIFF: Unsigned,
        {
            type Output;
            fn $op_func_with_relativity(self, rhs: Rhs) -> Self::Output;
        }

        impl<T, L_REL, R_REL, DIFF_REL> $OpTrait<ThingWithRelativity<T, R_REL>>
            for ThingWithRelativity<T, L_REL>
        where
            T: $OpTrait<T>,
            L_REL: Unsigned + Add<DIFF_REL, Output = R_REL>,
            R_REL: Unsigned + Sub<L_REL, Output = DIFF_REL>,
            DIFF_REL: Unsigned,
            Self: $OpTraitWithRelativity<T, DIFF_REL>,
        {
            type Output = ThingWithRelativity<T, Sum<L_REL, DIFF_REL>>;
            fn $op_func(self, rhs: ThingWithRelativity<T, R_REL>) -> Self::Output {
                self.$op_func_with_relativity(rhs)
            }
        }
    };
}
macro_rules! impl_op_with_relativity {
    ($OpTrait:ident, $op_func:ident, $OpTraitWithRelativity:ident, $op_func_with_relativity:ident, $rhs_rel_diff:ty, $out_rel_diff:ty) => {
        impl<T, L_REL, R_REL> $OpTraitWithRelativity<ThingWithRelativity<T, R_REL>, $rhs_rel_diff>
            for ThingWithRelativity<T, L_REL>
        where
            L_REL: typenum::Unsigned + Add<U0, Output = L_REL> + Add<$out_rel_diff,
            R_REL: typenum::Unsigned,
            ThingWithRelativity<T, Sum<L_REL, $out_rel_diff>>: typenum::Unsigned,
        {
            type Output = ThingWithRelativity<T, Sum<L_REL, $out_rel_diff>>;
            fn $op_func_with_relativity(self, rhs: ThingWithRelativity<T, R_REL>) -> Self::Output {
                self.thing.$op_func(rhs.thing())
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
    RELATIVITY_LEVEL: Unsigned,
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
    RELATIVITY_LEVEL: Unsigned,
{
    fn zero() -> Self {
        ThingWithRelativity::new_thing(THING_TYPE::zero())
    }
}

impl<THING_TYPE, RELATIVITY_LEVEL> std::ops::Neg
    for ThingWithRelativity<THING_TYPE, RELATIVITY_LEVEL>
where
    THING_TYPE: std::ops::Neg,
    RELATIVITY_LEVEL: Unsigned,
{
    type Output = ThingWithRelativity<<THING_TYPE as std::ops::Neg>::Output, RELATIVITY_LEVEL>;

    fn neg(self) -> Self::Output {
        ThingWithRelativity::new_thing(Self::neg(self.thing))
    }
}

impl<THING_TYPE, RHS_TYPE, RELATIVITY_LEVEL> std::ops::Mul<RHS_TYPE>
    for ThingWithRelativity<THING_TYPE, RELATIVITY_LEVEL>
where
    THING_TYPE: std::ops::Mul<RHS_TYPE>,
    RELATIVITY_LEVEL: Unsigned,
{
    type Output =
        ThingWithRelativity<<THING_TYPE as std::ops::Mul<RHS_TYPE>>::Output, RELATIVITY_LEVEL>;

    fn mul(self, rhs: RHS_TYPE) -> Self::Output {
        Self::Output::new(self.thing * rhs)
    }
}

// TODO: Can derive? (is the phantomdata the problem?)
impl<THING_TYPE, RHS_THING_TYPE, RELATIVITY_LEVEL>
    std::cmp::PartialEq<ThingWithRelativity<RHS_THING_TYPE, RELATIVITY_LEVEL>>
    for ThingWithRelativity<THING_TYPE, RELATIVITY_LEVEL>
where
    THING_TYPE: std::cmp::PartialEq<RHS_THING_TYPE>,
    RELATIVITY_LEVEL: Unsigned,
{
    fn eq(&self, other: &ThingWithRelativity<RHS_THING_TYPE, RELATIVITY_LEVEL>) -> bool {
        self.thing == other.thing
    }
}
impl<THING_TYPE, T, RELATIVITY_LEVEL> From<(T, T)>
    for ThingWithRelativity<THING_TYPE, RELATIVITY_LEVEL>
where
    THING_TYPE: From<(T, T)>,
    RELATIVITY_LEVEL: Unsigned,
{
    fn from(value: (T, T)) -> Self {
        ThingWithRelativity::new(THING_TYPE::from(value))
    }
}
