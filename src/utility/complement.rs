pub trait Complement {
    type Output;
    fn complement(&self) -> Self::Output;
}

macro_rules! impl_complement_for_newtype {
    ($type:ident$(<$T:ident$(: $traitparam:ident)?>)?) => {
        impl$(<$T$(: $traitparam)?>)? Complement for $type$(<$T>)? {
            type Output = Self;
            fn complement(&self) -> Self::Output {
                Self(self.0.complement())
            }
        }
    };
}

pub(crate) use impl_complement_for_newtype;
