use std::ops::Add;
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

