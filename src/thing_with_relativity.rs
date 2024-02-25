use std::marker::PhantomData;

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

// macro_rules! binary_op_delegate {
//     ($trait:ty, $trait_function:ident) => {
//         impl<THING_TYPE, RHS_THING_TYPE, RELATIVITY_LEVEL>
//             $trait<ThingWithRelativity<THING_TYPE, RELATIVITY_LEVEL>>
//             for ThingWithRelativity<THING_TYPE, RELATIVITY_LEVEL>
//         where
//             THING_TYPE: $trait<RHS_THING_TYPE>,
//             RELATIVITY_LEVEL: typenum::Unsigned,
//         {
//             type Output = ThingWithRelativity<
//                 <THING_TYPE as $trait<RHS_THING_TYPE>>::Output,
//                 RELATIVITY_LEVEL,
//             >;

//             fn $trait_function(
//                 self,
//                 rhs: ThingWithRelativity<RHS_THING_TYPE, RELATIVITY_LEVEL>,
//             ) -> Self::Output {
//                 ThingWithRelativity {
//                     thing: $trait_function(self.thing, rhs.thing),
//                     _level_of_relativity: std::marker::PhantomData,
//                 }
//             }
//         }
//     };
// }

// binary_op_delegate!(std::ops::Add, add);

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

impl<THING_TYPE, RHS_THING_TYPE, RELATIVITY_LEVEL>
    std::ops::Add<ThingWithRelativity<RHS_THING_TYPE, typenum::Add1<RELATIVITY_LEVEL>>>
    for ThingWithRelativity<THING_TYPE, RELATIVITY_LEVEL>
where
    THING_TYPE: std::ops::Add<RHS_THING_TYPE>,
    RELATIVITY_LEVEL: typenum::marker_traits::Unsigned + std::ops::Add<typenum::B1>,
    typenum::Add1<RELATIVITY_LEVEL>: typenum::Unsigned,
{
    type Output = ThingWithRelativity<
        <THING_TYPE as std::ops::Add<RHS_THING_TYPE>>::Output,
        RELATIVITY_LEVEL,
    >;

    fn add(self, rhs: ThingWithRelativity<RHS_THING_TYPE, RELATIVITY_LEVEL>) -> Self::Output {
        ThingWithRelativity::new_thing(std::ops::Add::add(self.thing, rhs.thing))
    }
}

impl<THING_TYPE, RHS_THING_TYPE, RELATIVITY_LEVEL>
    std::ops::Sub<ThingWithRelativity<RHS_THING_TYPE, RELATIVITY_LEVEL>>
    for ThingWithRelativity<THING_TYPE, RELATIVITY_LEVEL>
where
    THING_TYPE: std::ops::Sub<RHS_THING_TYPE>,
    RELATIVITY_LEVEL: typenum::Unsigned + std::ops::Add<typenum::B1>,
    typenum::Add1<RELATIVITY_LEVEL>: typenum::Unsigned,
{
    type Output = ThingWithRelativity<
        <THING_TYPE as std::ops::Sub<RHS_THING_TYPE>>::Output,
        typenum::Add1<RELATIVITY_LEVEL>,
    >;

    fn sub(self, rhs: ThingWithRelativity<RHS_THING_TYPE, RELATIVITY_LEVEL>) -> Self::Output {
        ThingWithRelativity::new_thing(Self::sub(self.thing, rhs.thing))
    }
}
impl<THING_TYPE, RHS_THING_TYPE, RELATIVITY_LEVEL>
    std::ops::Sub<ThingWithRelativity<RHS_THING_TYPE, typenum::Add1<RELATIVITY_LEVEL>>>
    for ThingWithRelativity<THING_TYPE, RELATIVITY_LEVEL>
where
    THING_TYPE: std::ops::Sub<RHS_THING_TYPE>,
    RELATIVITY_LEVEL: typenum::Unsigned + std::ops::Add<typenum::B1>,
    typenum::Add1<RELATIVITY_LEVEL>: typenum::Unsigned,
{
    type Output = ThingWithRelativity<
        <THING_TYPE as std::ops::Sub<RHS_THING_TYPE>>::Output,
        RELATIVITY_LEVEL,
    >;

    fn sub(self, rhs: ThingWithRelativity<RHS_THING_TYPE, RELATIVITY_LEVEL>) -> Self::Output {
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
// impl<THING_TYPE, RELATIVITY_LEVEL> std::marker::Copy
//     for ThingWithRelativity<THING_TYPE, RELATIVITY_LEVEL>
// where
//     THING_TYPE: std::marker::Copy,
//     RELATIVITY_LEVEL: typenum::Unsigned,
// {
// }

// impl<THING_TYPE, RELATIVITY_LEVEL> std::clone::Clone
//     for ThingWithRelativity<THING_TYPE, RELATIVITY_LEVEL>
// where
//     THING_TYPE: std::clone::Clone,
//     RELATIVITY_LEVEL: typenum::Unsigned,
// {
//     fn clone(&self) -> Self {
//         ThingWithRelativity {
//             thing: self.thing.clone(),
//             _level_of_relativity: std::marker::PhantomData,
//         }
//     }
// }

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

// impl<THING_TYPE, RELATIVITY_LEVEL> std::cmp::Eq
//     for ThingWithRelativity<THING_TYPE, RELATIVITY_LEVEL>
// where
//     THING_TYPE: std::cmp::Eq,
//     RELATIVITY_LEVEL: typenum::Unsigned,
// {
// }

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
