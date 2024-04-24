use crate::utility::*;

pub trait Translate<MoveType>: Add<MoveType, Output = Self> + Sub<MoveType, Output = Self> {}
impl<T, MoveType> Translate<MoveType> for T where
    T: Add<MoveType, Output = Self> + Sub<MoveType, Output = Self>
{
}

macro_rules! impl_translate_for_newtype {
    // TODO: $P doesn't really need to be a metavariable.  Can I make it just part of the regex match?
    ($type:ident<$P:ident: $traitparam:ident>) => {
        impl<$P: $traitparam> Add<$P> for $type<$P> {
            type Output = Self;

            fn add(self, rhs: $P) -> Self::Output {
                Self::new(self.0.add(rhs))
            }
        }
        impl<$P: $traitparam> Sub<$P> for $type<$P> {
            type Output = Self;

            fn sub(self, rhs: $P) -> Self::Output {
                Self::new(self.0.sub(rhs))
            }
        }
    };
}
pub(crate) use impl_translate_for_newtype;
