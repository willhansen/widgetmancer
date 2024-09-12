use std::ops::{Add, Sub};
use std::marker::PhantomData;

pub trait LadderRung {
    fn new() -> Self;
}

#[derive(Eq, PartialEq, Debug)]
pub struct RelativeTo<T>(PhantomData<T>);


impl<T: LadderRung> LadderRung for RelativeTo<T> { 
    fn new() -> Self {
        Self(PhantomData::<T>)
    }
}
impl LadderRung for RelativeTo<()> { 
    fn new() -> Self {
        Self(PhantomData::<()>)
    }
}

pub type R0 = RelativeTo<()>;
pub type R1 = RelativeTo<R0>;
pub type R2 = RelativeTo<R1>;
pub type R3 = RelativeTo<R2>;
pub type R4 = RelativeTo<R3>;
// ...


impl<R> Add<RelativeTo<RelativeTo<R>>> for RelativeTo<R>
    where
    RelativeTo<R>: LadderRung
{
    type Output = RelativeTo<R>;

    fn add(self, _rhs: RelativeTo<RelativeTo<R>>) -> Self::Output {
        Self::Output::new()
    }
} 

impl<R> Sub<RelativeTo<RelativeTo<R>>> for RelativeTo<R>
    where
    RelativeTo<R>: LadderRung
{
    type Output = RelativeTo<R>;

    fn sub(self, _rhs: RelativeTo<RelativeTo<R>>) -> Self::Output {
        Self::Output::new()
    }
} 

impl<R> Sub<RelativeTo<R>> for RelativeTo<R>
    where
    RelativeTo<R>: LadderRung
{
    type Output = RelativeTo<RelativeTo<R>>;

    fn sub(self, _rhs: RelativeTo<R>) -> Self::Output {
        Self::Output::new()
    }
} 

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

        assert_eq!(R0::new() - R0::new(), R1::new());
        assert_eq!(R1::new() - R1::new(), R2::new());
    }
}
