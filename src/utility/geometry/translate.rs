use crate::utility::*;

pub trait Translate<MoveType>: Add<MoveType, Output = Self> + Sub<MoveType, Output = Self> {}
impl<T, MoveType> Translate<MoveType> for T where
    T: Add<MoveType, Output = Self> + Sub<MoveType, Output = Self>
{
}

//
macro_rules! impl_via_newtype {
    ($type:ident<P: $traitparam:ident>) => {
        impl<P: $traitparam> Add<P> for $type<P> {
            type Output = Self;

            fn add(self, rhs: P) -> Self::Output {
                // TODO: relies on specific constructor: bad
                Self::new(self.0.add(rhs))
            }
        }
        impl<P: $traitparam> Sub<P> for $type<P> {
            type Output = Self;

            fn sub(self, rhs: P) -> Self::Output {
                Self::new(self.0.sub(rhs))
            }
        }
    };
}
pub(crate) use impl_via_newtype;

// TODO
macro_rules! impl_for_refined_type {
    ($TheStruct:ident<P: $CoordTrait:ident>, refinement_base= $RefinementBase:ident<P>) => {

        // TODO: statically assert the $RefinementBase implements translate

        impl<P> Add<P> for $TheStruct<P>
        where
            Self: Operations<P> + Translate<P>,
            P: $CoordTrait,
        {
            type Output = Self;

            fn add(self, rhs: P) -> Self::Output {
                let base: $RefinementBase<P> = self.try_into().unwrap();
                base.add(rhs).into()
            }
        }
        impl<P> Sub<P> for $TheStruct<P>
        where
            Self: Operations<P>,
            P: $CoordTrait,
        {
            type Output = Self;

            fn sub(self, rhs: P) -> Self::Output {
                let base: $RefinementBase<P> = self.try_into().unwrap();
                base.sub(rhs).into()
            }
        }
    };
}
pub(crate) use impl_for_refined_type;
