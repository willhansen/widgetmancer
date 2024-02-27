use paste::paste;
use std::marker::PhantomData;
use typenum::*;

#[derive(Hash, Copy, Clone, Eq)]
pub struct ThingWithRelativity<THING_TYPE, RELATIVITY_LEVEL = typenum::U0>
where
    RELATIVITY_LEVEL: typenum::Unsigned,
{
    pub thing: THING_TYPE,
    pub _level_of_relativity: std::marker::PhantomData<RELATIVITY_LEVEL>,
}

impl<THING_TYPE, RELATIVITY_LEVEL> ThingWithRelativity<THING_TYPE, RELATIVITY_LEVEL>
where
    RELATIVITY_LEVEL: typenum::Unsigned,
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

impl<T, RELATIVITY_LEVEL> HasRelativity for ThingWithRelativity<T, RELATIVITY_LEVEL> {
    type RelativityLevel = RELATIVITY_LEVEL;
}

use std::ops::{Add, Sub};
macro_rules! op_with_relativity {
    ($trait:ident, $func:ident, $rel_trait:ident, $rel_func:ident) => {
        trait $rel_trait<T, REL_DIFF>
        where
            REL_DIFF: Unsigned,
        {
            type Output;
            fn $rel_func(self, rhs: T) -> Self::Output;
        }
        impl<T, L_REL, R_REL, DIFF_REL> $trait<ThingWithRelativity<T, R_REL>>
            for ThingWithRelativity<T, L_REL>
        where
            T: $trait<T>,
            L_REL: Unsigned, // + Add<DIFF_REL, Output = R_REL>,
            R_REL: Unsigned + Sub<L_REL, Output = DIFF_REL>,
            DIFF_REL: Unsigned,
            Self: $rel_trait<T, DIFF_REL>,
        {
            type Output = ThingWithRelativity<T, Sum<L_REL, DIFF_REL>>;
            fn $func(self, rhs: ThingWithRelativity<T, R_REL>) -> Self::Output {
                self.$rel_func(rhs)
            }
        }
    };
}
op_with_relativity!(Add, add, AddWithRelativity, add_with_relativity);
op_with_relativity!(Sub, sub, SubWithRelativity, sub_with_relativity);

impl<T, L_REL, R_REL> AddWithRelativity<ThingWithRelativity<T, R_REL>, U1>
    for ThingWithRelativity<T, L_REL>
{
    type Output = Self;
    fn add_with_relativity(self, rhs: ThingWithRelativity<T, R_REL>) -> Self::Output {
        self.thing.add(rhs.thing())
    }
}
impl<T, L_REL, R_REL> SubWithRelativity<ThingWithRelativity<T, R_REL>, U1>
    for ThingWithRelativity<T, L_REL>
{
    type Output = Self;
    fn sub_with_relativity(self, rhs: ThingWithRelativity<T, R_REL>) -> Self::Output {
        self.thing.sub(rhs.thing())
    }
}

impl<T, L_REL, R_REL> SubWithRelativity<ThingWithRelativity<T, R_REL>, U0>
    for ThingWithRelativity<T, L_REL>
{
    type Output = ThingWithRelativity<T, Add1<L_REL>>;
    fn sub_with_relativity(self, rhs: ThingWithRelativity<T, R_REL>) -> Self::Output {
        self.thing.sub(rhs.thing())
    }
}

impl<THING_TYPE, RELATIVITY_LEVEL> std::fmt::Debug
    for ThingWithRelativity<THING_TYPE, RELATIVITY_LEVEL>
where
    THING_TYPE: std::fmt::Debug,
    RELATIVITY_LEVEL: typenum::Unsigned,
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
    RELATIVITY_LEVEL: typenum::Unsigned,
{
    fn zero() -> Self {
        ThingWithRelativity::new_thing(THING_TYPE::zero())
    }
}

impl<THING_TYPE, RELATIVITY_LEVEL> std::ops::Neg
    for ThingWithRelativity<THING_TYPE, RELATIVITY_LEVEL>
where
    THING_TYPE: std::ops::Neg,
    RELATIVITY_LEVEL: typenum::Unsigned,
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
    RELATIVITY_LEVEL: typenum::Unsigned,
{
    type Output =
        ThingWithRelativity<<THING_TYPE as std::ops::Mul<RHS_TYPE>>::Output, RELATIVITY_LEVEL>;

    fn mul(self, rhs: RHS_TYPE) -> Self::Output {
        ThingWithRelativity {
            thing: self.thing * rhs,
            _level_of_relativity: std::marker::PhantomData,
        }
    }
}

// TODO: Can derive? (is the phantomdata the problem?)
impl<THING_TYPE, RHS_THING_TYPE, RELATIVITY_LEVEL>
    std::cmp::PartialEq<ThingWithRelativity<RHS_THING_TYPE, RELATIVITY_LEVEL>>
    for ThingWithRelativity<THING_TYPE, RELATIVITY_LEVEL>
where
    THING_TYPE: std::cmp::PartialEq<RHS_THING_TYPE>,
    RELATIVITY_LEVEL: typenum::Unsigned,
{
    fn eq(&self, other: &ThingWithRelativity<RHS_THING_TYPE, RELATIVITY_LEVEL>) -> bool {
        self.thing == other.thing
    }
}
impl<THING_TYPE, T, RELATIVITY_LEVEL> From<(T, T)>
    for ThingWithRelativity<THING_TYPE, RELATIVITY_LEVEL>
where
    THING_TYPE: From<(T, T)>,
    RELATIVITY_LEVEL: typenum::Unsigned,
{
    fn from(value: (T, T)) -> Self {
        ThingWithRelativity {
            thing: THING_TYPE::from(value),
            _level_of_relativity: std::marker::PhantomData,
        }
    }
}
