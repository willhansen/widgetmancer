use std::ops::{Add, Sub};

#[derive(Eq, PartialEq, Debug)]
pub struct Relative<const LEVEL: u32>();

impl< const L: u32> Relative<L> {
    pub fn new() -> Self {
        Self()
    }
}

impl<const L: u32> Default for Relative<L> {
    fn default() -> Self {
        Self::new()
    }
}

// impl<const LEVEL: u32> Relative<LEVEL> {
//     fn level() -> u32
// }

pub type R0 = Relative<0>;
pub type R1 = Relative<1>;
pub type R2 = Relative<2>;
pub type R3 = Relative<3>;



macro_rules! impl_binary_op {
    ($Trait:ident, $Fn:ident, $Ra:ty, $Rb:ty, $Rc:ty) => {
        impl $Trait<$Rb> for $Ra {
            type Output = $Rc;

            fn $Fn(self, _rhs: $Rb) -> Self::Output {
                <$Rc>::new()
            }
        }
    }
}
macro_rules! impl_add {
    ($Ra:ty, $Rb:ty, $Rc:ty) => {
        impl_binary_op!(Add, add, $Ra, $Rb, $Rc);
    }
}
macro_rules! impl_sub {
    ($Ra:ty, $Rb:ty, $Rc:ty) => {
        impl_binary_op!(Sub, sub, $Ra, $Rb, $Rc);
    }
}

impl_add!(R0, R1, R0);
impl_add!(R1, R2, R1);
impl_add!(R2, R3, R2);

impl_sub!(R0, R1, R0);
impl_sub!(R1, R2, R1);
impl_sub!(R2, R3, R2);

impl_sub!(R0, R0, R1);
impl_sub!(R1, R1, R2);
impl_sub!(R2, R2, R3);


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
