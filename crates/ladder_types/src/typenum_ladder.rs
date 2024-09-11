use std::ops::Add;
use std::ops::Sub;
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

impl<L> Sub<LadderRung<Add1<L>>> for LadderRung<L>
    where
    L: Unsigned + Add<B1>,
    Add1<L>: Unsigned
{
    type Output = LadderRung<L>;

    fn sub(self, _rhs: LadderRung<Add1<L>>) -> Self::Output {
        Self::Output::new()
    }
} 

// impl<L> Sub<LadderRung<L>> for LadderRung<L>
//     where
//     L: Unsigned + Add<B1>,
//     Add1<L>: Unsigned
// {
//     type Output = LadderRung<Add1<L>>;

//     fn sub(self, _rhs: LadderRung<L>) -> Self::Output {
//         Self::Output::new()
//     }
// } 

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_add() {
        assert_eq!(R0::new() + R1::new(), R0::new());
        assert_eq!(R1::new() + R2::new(), R1::new());
    }
    #[test]
    fn test_sub() {
        assert_eq!(R0::new() - R1::new(), R0::new());
        assert_eq!(R1::new() - R2::new(), R1::new());

        // assert_eq!(R0::new() - R0::new(), R1::new());
        // assert_eq!(R1::new() - R1::new(), R2::new());
    }
}

