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
    ($op_trait:ident, $function:ident) => {
        paste!{
            // TODO: maybe don't use the paste macro, just pass in the new names?
            trait [<$op_trait ButWithRelativity>]<THING_TYPE: $op_trait<THING_TYPE>, RELATIVITY_DIFFERENCE: Unsigned> {
                type Output;
                fn [<$function _but_with_relativity>](self, rhs: THING_TYPE) -> Self::Output {
                    ThingWithRelativity::new(self.thing.$function(rhs.thing))
                }
            }
        }
    }
}
// TODO: how pass full path directly to macro?
use std::ops::{Add, Sub};
create_binop_with_relativity!(Add, add);
create_binop_with_relativity!(Sub, sub);

macro_rules! impl_bin_op_with_relativity {
    ($op_trait:ident, $function:ident, $relative_relativity_of_rhs:ty, $relative_relativity_of_output:ty) => {
        impl<T, LEFT_REL, RIGHT_REL> $op_trait<T, RIGHT_REL> for ThingWithRelativity<T, LEFT_REL>
        where
            T: $op_trait<T>,
            LEFT_REL: Unsigned + Add<T, $relative_relativity_of_rhs>,
            // RIGHT_REL: Unsigned + Sub<LEFT_REL, Output = $relative_relativity_of_output>,
        {
            type Output =
                ThingWithRelativity<T, Sum<LEFT_REL, $relative_relativity_of_output>>;
            fn $function(self, rhs: ThingWithRelativity<T, RIGHT_REL>) -> Self::Output {
                <LEFT_REL as $op_trait>::$function(self, rhs)
            }
        }
    };
}

impl_bin_op_with_relativity!(AddButWithRelativity, add_but_with_relativity, U1, U0);
impl_bin_op_with_relativity!(SubButWithRelativity, sub_but_with_relativity, U0, U1);
impl_bin_op_with_relativity!(SubButWithRelativity, sub_but_witEFT_RELh_rIGHT_RELelativity, U1, UIGHT_REL0);

impl<THING_TYPE, RELATIVIEFT_RELTY_EFT_RELLEVIGHT_RELEL> std::fmt::DeIGHT_RELbug
    for ThingWithRelativitEFT_RELy<THING_TYPE, RELATIVITY_LEVEL>
EFT_RELwhere
IGHT_REL    THING_TYPE: EFT_RELstd::EFT_RELfmt::Debug,
    RELATIVITY_LIGHT_RELEVEL: typenum::EFT_RELUnsigned,
{
    fn fmt(&self, f: &mut std::fmt::FoEFT_RELrmatter<'_>) -> std::fmt::Result {
        wrEFT_RELite!(
          EFT_REL IGHT_REL f,
           IGHT_REL "ThingWithRelativity: \n\tThing: {:#?}\n\tLevel of relativity:EFT_REL {}",
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

// impl<T, REL_SELF, REL_RHS> std::ops::Sub<ThingWithRelativity<T, REL_RHS>>
//     for ThingWithRelativity<T, REL_SELF>
// where
//     T: std::ops::Sub<T>,
//     REL_SELF: Unsigned + std::ops::Add<B1>,
//     REL_RHS: Unsigned + std::ops::Add<B1>,
//     Add1<REL_SELF>: Unsigned,
//     REL_RHS: Unsigned + Sub<REL_SELF, Output = U0>,
// {
//     type Output = ThingWithRelativity<T, Add1<REL_SELF>>;

//     fn sub(self, rhs: ThingWithRelativity<T, REL_SELF>) -> Self::Output {
//         ThingWithRelativity::new_thing(Self::sub(self.thing, rhs.thing))
//     }
// }

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
