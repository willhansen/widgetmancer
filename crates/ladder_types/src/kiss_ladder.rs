//! ```rust
//! use ladder_types::kiss_ladder::*
//! assert_eq!(R0::new() + R1::new(), R0::new());
//! assert_eq!(R1::new() + R2::new(), R1::new());
//! assert_eq!(R2::new() + R3::new(), R2::new());
//! ```
//! ```rust,compile_fail,E0515
//! use ladder_types::kiss_ladder::*
//! R0::new() + R0::new();
//! R0::new() + R2::new();
//! R1::new() + R1::new();
//! R1::new() + R0::new();
//! ```
//!
use std::ops::Add;

#[derive(Eq, PartialEq, Debug)]
pub struct Relative<const Level: u32>();

impl< const L: u32> Relative<L> {
    fn new() -> Self {
        Self()
    }
}

// impl<const Level: u32> Relative<Level> {
//     fn level() -> u32
// }

pub type R0 = Relative<0>;
pub type R1 = Relative<1>;
pub type R2 = Relative<2>;
pub type R3 = Relative<3>;


macro_rules! impl_add {
    ($Ra:ty, $Rb:ty, $Rc:ty) => {
        impl Add<$Rb> for $Ra {
            type Output = $Rc;

            fn add(self, _rhs: $Rb) -> Self::Output {
                <$Rc>::new()
            }
        }

    }
}

impl_add!(R0, R1, R0);
impl_add!(R1, R2, R1);
impl_add!(R2, R3, R2);


