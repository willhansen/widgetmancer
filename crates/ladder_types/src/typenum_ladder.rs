use std::ops::Add;
use typenum::{U0, U1, U2, U3, B1, Add1, Unsigned};

#[derive(Eq, PartialEq, Debug)]
pub struct LadderRung<T: Unsigned>(std::marker::PhantomData<T>);

impl<T: Unsigned> LadderRung<T> {
    pub fn new() -> Self {
        Self(std::marker::PhantomData::<T>)
    }
}

pub type R0 = LadderRung<U0>;
pub type R1 = LadderRung<U1>;
pub type R2 = LadderRung<U2>;
pub type R3 = LadderRung<U3>;
// ...


impl<L> Add<LadderRung<Add1<L>>> for LadderRung<L>
    where
    L: Unsigned + Add<B1>,
    Add1<L>: Unsigned
{
    type Output = LadderRung<L>;

    fn add(self, _rhs: LadderRung<Add1<L>>) -> Self::Output {
        Self::Output::new()
    }
} 

