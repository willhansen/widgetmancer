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

macro_rules! impl_binary_op_with_relative {
    ($trait:ident, $function:ident) => {
        // Add one to zero case
        impl<T: $trait<T>> $trait<ThingWithRelativity<T, U1>> for ThingWithRelativity<T, U0> {
            type Output = Self;
            fn $function(self, rhs: ThingWithRelativity<T, U1>) -> Self::Output {
                ThingWithRelativity::new_thing(self.thing.$function(rhs.thing))
            }
        }
        // Add one to non-zero even case
        impl<L, T> $trait<ThingWithRelativity<T, UInt<L, B1>>>
            for ThingWithRelativity<T, UInt<L, B0>>
        where
            L: Unsigned,
            T: $trait<T>,
        {
            type Output = Self;
            fn $function(self, rhs: ThingWithRelativity<T, UInt<L, B1>>) -> Self::Output {
                ThingWithRelativity::new_thing(self.thing.$function(rhs.thing))
            }
        }
        // Add one to odd case
        impl<L, T> $trait<ThingWithRelativity<T, UInt<Add1<L>, B0>>>
            for ThingWithRelativity<T, UInt<L, B1>>
        where
            L: Unsigned + Add<B1>,
            Add1<L>: Unsigned,
            T: $trait<T>,
        {
            type Output = Self;
            fn $function(self, rhs: ThingWithRelativity<T, UInt<Add1<L>, B0>>) -> Self::Output {
                ThingWithRelativity::new_thing(self.thing.$function(rhs.thing))
            }
        }
    };
}

use std::ops::{Add, Sub};
// TODO: how pass full path directly to macro?
impl_binary_op_with_relative!(Add, add);
impl_binary_op_with_relative!(Sub, sub);

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

impl<T, RELATIVITY_LEVEL> std::ops::Sub<ThingWithRelativity<T, RELATIVITY_LEVEL>>
    for ThingWithRelativity<T, RELATIVITY_LEVEL>
where
    T: std::ops::Sub<T>,
    RELATIVITY_LEVEL: typenum::Unsigned + std::ops::Add<typenum::B1>,
    typenum::Add1<RELATIVITY_LEVEL>: typenum::Unsigned,
{
    type Output = ThingWithRelativity<T, typenum::Add1<RELATIVITY_LEVEL>>;

    fn sub(self, rhs: ThingWithRelativity<T, RELATIVITY_LEVEL>) -> Self::Output {
        ThingWithRelativity::new_thing(Self::sub(self.thing, rhs.thing))
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
