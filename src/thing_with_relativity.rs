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

macro_rules! create_binop_with_relativity {
    ($op_trait:ident, $function:ident, $new_op_trait:ident, $new_function:ident) => {
        trait $new_op_trait<T, RELATIVITY_DIFFERENCE>
        where
            T: $op_trait<T>,
            RELATIVITY_DIFFERENCE: Unsigned,
        {
            type Output;
            fn $new_function(self, rhs: T) -> Self::Output;
        }
    };
}
use std::ops::{Add, Sub};
create_binop_with_relativity!(Add, add, AddButWithRelativity, add_but_with_relativity);
create_binop_with_relativity!(Sub, sub, SubButWithRelativity, sub_but_with_relativity);

macro_rules! impl_bin_op_with_relativity {
    ($op_trait:ident, $function:ident, $new_op_trait:ident, $new_function:ident, $rhs_rel_diff:ty, $out_rel_diff:ty) => {
        impl<T, LEFT_REL, RIGHT_REL> $new_op_trait<T, $rhs_rel_diff>
            for ThingWithRelativity<T, LEFT_REL>
        where
            T: $op_trait<T>,
            LEFT_REL: Unsigned + Add<$rhs_rel_diff, Output = RIGHT_REL>,
            RIGHT_REL: Unsigned + Sub<LEFT_REL, Output = $out_rel_diff>,
        {
            type Output = ThingWithRelativity<T, Sum<LEFT_REL, $out_rel_diff>>;
            fn $new_function(self, rhs: ThingWithRelativity<T, RIGHT_REL>) -> Self::Output {
                Self::Output::new(self.thing.$function(rhs.thing))
            }
        }
        // impl<T, LEFT_REL, RIGHT_REL> $op_trait<ThingWithRelativity<T, RIGHT_REL>>
        //     for ThingWithRelativity<T, LEFT_REL>
        // where
        //     T: $op_trait<T>,
        //     ThingWithRelativity<T, LEFT_REL>: $new_op_trait<T, $rhs_drel>,
        //     RIGHT_REL: Unsigned + Sub<LEFT_REL, Output = $out_drel>,
        // {
        //     type Output = ThingWithRelativity<T, Sum<LEFT_REL, $out_drel>>;
        //     fn $function(self, rhs: ThingWithRelativity<T, RIGHT_REL>) -> Self::Output {
        //         self.$new_function(rhs)
        //     }
        // }
    };
}

// TODO: how pass full path directly to macro?
// impl_bin_op_with_relativity!(
//     Add,
//     add,
//     AddButWithRelativity,
//     add_but_with_relativity,
//     U1,
//     U0
// );
// impl_bin_op_with_relativity!(
//     Sub,
//     sub,
//     SubButWithRelativity,
//     sub_but_with_relativity,
//     U0,
//     U1
// );
// impl_bin_op_with_relativity!(
//     Sub,
//     sub,
//     SubButWithRelativity,
//     sub_but_with_relativity,
//     U1,
//     U0
// );

impl<T, LEFT_REL, RIGHT_REL> Add<ThingWithRelativity<T, RIGHT_REL>>
    for ThingWithRelativity<T, LEFT_REL>
where
    T: Add<T>,
    ThingWithRelativity<T, LEFT_REL>: AddButWithRelativity<T, U1>,
    RIGHT_REL: Unsigned + Sub<LEFT_REL, Output = U0>,
{
    type Output = ThingWithRelativity<T, Sum<LEFT_REL, U0>>;
    fn add(self, rhs: ThingWithRelativity<T, RIGHT_REL>) -> Self::Output {
        self.add_but_with_relativity(rhs)
    }
}
impl<T, LEFT_REL, RIGHT_REL, DREL> Sub<ThingWithRelativity<T, RIGHT_REL>>
    for ThingWithRelativity<T, LEFT_REL>
where
    T: Sub<T>,
    ThingWithRelativity<T, LEFT_REL>: SubButWithRelativity<T, DREL>,
    RIGHT_REL: Unsigned + Sub<LEFT_REL, Output = DREL>,
{
    type Output = ThingWithRelativity<T, Sum<LEFT_REL, U0>>;
    fn sub(self, rhs: ThingWithRelativity<T, RIGHT_REL>) -> Self::Output {
        self.sub_but_with_relativity(rhs)
    }
}
impl<T, LEFT_REL, RIGHT_REL> Sub<ThingWithRelativity<T, RIGHT_REL>>
    for ThingWithRelativity<T, LEFT_REL>
where
    T: Sub<T>,
    ThingWithRelativity<T, LEFT_REL>: SubButWithRelativity<T, U0>,
    RIGHT_REL: Unsigned + Sub<LEFT_REL, Output = U1>,
{
    type Output = ThingWithRelativity<T, Sum<LEFT_REL, U1>>;
    fn sub(self, rhs: ThingWithRelativity<T, RIGHT_REL>) -> Self::Output {
        self.sub_but_with_relativity(rhs)
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
