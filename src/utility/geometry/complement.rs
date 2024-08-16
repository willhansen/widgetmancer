pub trait Complement {
    type Output;
    fn complement(&self) -> Self::Output;
}

macro_rules! impl_complement_via_newtype {
    ($type:ident$(<$T:ident$(: $traitparam:ident)?>)?) => {
        impl$(<$T$(: $traitparam)?>)? Complement for $type$(<$T>)? {
            type Output = Self;
            fn complement(&self) -> Self::Output {
                Self(self.0.complement())
            }
        }
    };
}
pub(crate) use impl_complement_via_newtype;

macro_rules! impl_complement_via_refinement {
    ($type:ident<P: $PointReqs:ident>, refinement_base= $RefinementBase:ident<P>) => {
        // TODO: double check the prerequisite trait is implemented for the macro (at compile time)
        // const {assert_impl!($type<P>, Refinement<$RefinementBase<P>>);}
        impl<P: $PointReqs> Complement for $type<P> {
            type Output = Self;
            fn complement(&self) -> Self::Output {
                let base: $RefinementBase::<P> = (*self).into();
                base.complement().try_into().unwrap()
            }
        }
    };
}
pub(crate) use impl_complement_via_refinement;
