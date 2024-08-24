use crate::utility::*;

pub enum Sign {
    Pos,
    Zero,
    Neg,
}

impl<T: Signed + Debug> From<T> for Sign {
    fn from(value: T) -> Self {
        if value.is_zero() {
            Self::Zero
        } else if value.is_positive() {
            Self::Pos
        } else if value.is_negative() {
            Self::Neg
        } else {
            panic!("Probably NaN: {value:?}")
        }
    }
}

impl<T: Neg<Output = T> + Zero + Debug> std::ops::Mul<T> for Sign {
    type Output = T;

    fn mul(self, rhs: T) -> Self::Output {
        use Sign::*;
        match self {
            Pos => rhs,
            Zero => T::zero(),
            Neg => -rhs,
        }
    }
}
