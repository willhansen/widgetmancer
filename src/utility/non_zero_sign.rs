use crate::utility::*;

pub enum NonZeroSign {
    Pos,
    Neg,
}
macro_rules! impl_try_from_for_non_zero_sign {
    ($($Type:ty), +) => {
        $(

            impl TryFrom<$Type> for NonZeroSign {
                type Error = String;

                fn try_from(value: $Type) -> Result<Self, Self::Error> {
                    if value == <$Type>::zero() {
                        Err("is zero".to_owned())
                    } else if value.is_positive() {
                        Ok(Self::Pos)
                    } else if value.is_negative() {
                        Ok(Self::Neg)
                    } else {
                        Err("not valid: {value:?}".to_owned())
                    }
                }
            }
        )+
    }
}

impl_try_from_for_non_zero_sign!(i32, f32);

impl<T: Neg<Output = T> + Debug> std::ops::Mul<T> for NonZeroSign {
    type Output = T;

    fn mul(self, rhs: T) -> Self::Output {
        use NonZeroSign::*;
        match self {
            Pos => rhs,
            Neg => -rhs,
        }
    }
}
