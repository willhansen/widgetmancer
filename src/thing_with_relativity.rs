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
    // THING_RELATIVITY_LEVEL: typenum::marker_traits::Unsigned,
{
    type Output = ThingWithRelativity<
        <THING_TYPE as std::ops::Add<RHS_THING_TYPE>>::Output,
        RELATIVITY_LEVEL,
    >;

    fn add(self, rhs: RHS_THING_TYPE) -> Self::Output {
        ThingWithRelativity {
            thing: self + rhs,
            _level_of_relativity: std::marker::PhantomData,
        }
    }
}

impl<THING_TYPE, RHS_THING_TYPE, RELATIVITY_LEVEL>
    std::ops::Sub<ThingWithRelativity<RHS_THING_TYPE, RELATIVITY_LEVEL>>
    for ThingWithRelativity<THING_TYPE, RELATIVITY_LEVEL>
where
    THING_TYPE: std::ops::Sub<RHS_THING_TYPE>,
    // THING_RELATIVITY_LEVEL: typenum::marker_traits::Unsigned,
{
    type Output = ThingWithRelativity<
        <THING_TYPE as std::ops::Sub<RHS_THING_TYPE>>::Output,
        typenum::Add1<RELATIVITY_LEVEL>,
    >;

    fn sub(self, rhs: RHS_THING_TYPE) -> Self::Output {
        ThingWithRelativity {
            thing: self - rhs,
            _level_of_relativity: std::marker::PhantomData,
        }
    }
}
