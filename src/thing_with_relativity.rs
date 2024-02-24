#[derive(Clone, Hash, Eq, PartialEq, Debug, Copy)]
pub struct ThingWithRelativity<THING_TYPE, RELATIVITY_LEVEL = typenum::U0>
where
    RELATIVITY_LEVEL: typenum::marker_traits::Unsigned,
{
    thing: THING_TYPE,
    _level_of_relativity: std::marker::PhantomData<RELATIVITY_LEVEL>,
}

impl<THING_TYPE, RHS_THING_TYPE, RELATIVITY_LEVEL>
    std::ops::Add<ThingWithRelativity<RHS_THING_TYPE, RELATIVITY_LEVEL>>
    for ThingWithRelativity<THING_TYPE, RELATIVITY_LEVEL>
where
    THING_TYPE: std::ops::Add<RHS_THING_TYPE>,
    RELATIVITY_LEVEL: typenum::marker_traits::Unsigned,
{
    type Output = ThingWithRelativity<
        <THING_TYPE as std::ops::Add<RHS_THING_TYPE>>::Output,
        RELATIVITY_LEVEL,
    >;

    fn add(self, rhs: ThingWithRelativity<RHS_THING_TYPE, RELATIVITY_LEVEL>) -> Self::Output {
        ThingWithRelativity {
            thing: self.thing + rhs.thing,
            _level_of_relativity: std::marker::PhantomData,
        }
    }
}

impl<THING_TYPE, RHS_THING_TYPE, RELATIVITY_LEVEL>
    std::ops::Sub<ThingWithRelativity<RHS_THING_TYPE, RELATIVITY_LEVEL>>
    for ThingWithRelativity<THING_TYPE, RELATIVITY_LEVEL>
where
    THING_TYPE: std::ops::Sub<RHS_THING_TYPE>,
    RELATIVITY_LEVEL: typenum::Unsigned + std::ops::Add<typenum::B1>,
    <RELATIVITY_LEVEL as std::ops::Add<typenum::B1>>::Output: typenum::Unsigned,
{
    type Output = ThingWithRelativity<
        <THING_TYPE as std::ops::Sub<RHS_THING_TYPE>>::Output,
        typenum::Add1<RELATIVITY_LEVEL>,
    >;

    fn sub(self, rhs: ThingWithRelativity<RHS_THING_TYPE, RELATIVITY_LEVEL>) -> Self::Output {
        ThingWithRelativity {
            thing: self.thing - rhs.thing,
            _level_of_relativity: std::marker::PhantomData,
        }
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
        ThingWithRelativity {
            thing: -self,
            _level_of_relativity: std::marker::PhantomData,
        }
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
