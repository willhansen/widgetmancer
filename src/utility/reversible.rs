use crate::utility::*;

/// Reversing twice should always return the original
pub trait Reversible {
    fn reversed(&self) -> Self;
}

macro_rules! impl_reversible_for_newtype {
    ($type:ident$(<$T:ident$(: $traitparam:ident)?>)?) => {
        impl$(<$T$(: $traitparam)?>)? Reversible for $type$(<$T>)? {
            fn reversed(
                &self,
                ) -> Self {
                Self::new(self.0.reversed())
            }
        }
    };
}
pub(crate) use impl_reversible_for_newtype;
