//! ```
//! use ladder_types::unary_wrapper_ladder::*;
//! assert_eq!(R0::new() + R1::new(), R0::new());
//! assert_eq!(R1::new() + R2::new(), R1::new());
//! assert_eq!(R2::new() + R3::new(), R2::new());
//! ```
//! ```compile_fail
//! use ladder_types::unary_wrapper_ladder::*;
//! R0::new() + R0::new();
//! ```
//! ```compile_fail
//! use ladder_types::unary_wrapper_ladder::*;
//! R0::new() + R2::new();
//! ```
//! ```compile_fail
//! use ladder_types::unary_wrapper_ladder::*;
//! R1::new() + R1::new();
//! ```
//! ```compile_fail
//! use ladder_types::unary_wrapper_ladder::*;
//! R1::new() + R0::new();
//! ```
//!
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

