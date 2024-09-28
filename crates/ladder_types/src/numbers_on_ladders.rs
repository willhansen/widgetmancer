use crate::kiss_ladder::{LadderRung, R0, R1, R2, R3, R4};
use derive_more;
use num;
use std::ops::{Add, Div, Mul, Sub};

#[derive(PartialEq, Eq, Debug, Copy, Clone)]
struct ValOnLadder<T: Copy, R: LadderRung>(T, R);

impl<T: Copy, R: LadderRung> ValOnLadder<T, R> {
    fn new(x: T) -> Self {
        ValOnLadder::<T, R>(x, R::default())
    }

    fn val(&self) -> T {
        self.0
    }
}

#[allow(unused_macros)]
macro_rules! relnum {
    ($val:expr, $R:ty) => {
        ValOnLadder::<_, $R>::new($val)
    };
}

macro_rules! impl_bin_op {
    ($trait:ident, $function:ident) => {
        impl<Ta, Ra, Tb, Rb, Tc, Rc> $trait<ValOnLadder<Tb, Rb>> for ValOnLadder<Ta, Ra>
        where
            Ta: Copy + $trait<Tb, Output = Tc>,
            Tb: Copy,
            Tc: Copy,
            Ra: LadderRung + $trait<Rb, Output = Rc>,
            Rb: LadderRung,
            Rc: LadderRung,
        {
            type Output = ValOnLadder<Tc, Rc>;

            fn $function(self, rhs: ValOnLadder<Tb, Rb>) -> Self::Output {
                ValOnLadder::<Tc, Rc>::new(self.val().$function(rhs.val()))
            }
        }
    };
}

impl_bin_op!(Add, add);
impl_bin_op!(Sub, sub);

// TODO: still not sure how to handle rung multiplication
// impl_bin_op!(Mul, mul);
// impl_bin_op!(Div, div);

macro_rules! impl_op_on_datatype {
    ($TRAIT:ident, $FUNCTION:ident) => {
        impl<T, R> $TRAIT<T> for ValOnLadder<T, R>
        where
            T: $TRAIT<T, Output = T> + std::marker::Copy,
            R: LadderRung,
        {
            type Output = Self;

            fn $FUNCTION(self, rhs: T) -> Self::Output {
                Self::new(self.val().$FUNCTION(rhs))
            }
        }
    };
}

impl_op_on_datatype!(Mul, mul);
impl_op_on_datatype!(Div, div);

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_add() {
        assert_eq!(relnum!(5, R0) + relnum!(3, R1), relnum!(8, R0));
    }

    #[test]
    fn test_sub() {
        assert_eq!(relnum!(5, R0) - relnum!(3, R1), relnum!(2, R0));
        assert_eq!(relnum!(5, R0) - relnum!(3, R0), relnum!(2, R1));
    }

    #[test]
    fn test_mul_and_div_and_div_and_div_and_div_and_div_and_div_and_div_and_div_and_div() {
        assert_eq!(relnum!(5, R0) * 3, relnum!(15, R0));
        assert_eq!(relnum!(5, R1) * 3, relnum!(15, R1));

        assert_eq!(relnum!(5.0, R0) * 3.0, relnum!(15.0, R0));
        assert_eq!(relnum!(5.0, R1) * 3.0, relnum!(15.0, R1));

        assert_eq!(relnum!(15, R0) / 3, relnum!(5, R0));
        assert_eq!(relnum!(15, R1) / 3, relnum!(5, R1));

        assert_eq!(relnum!(15.0, R0) / 3.0, relnum!(5.0, R0));
        assert_eq!(relnum!(15.0, R1) / 3.0, relnum!(5.0, R1));
    }
    #[test]
    fn test_multistage() {
        let a = relnum!(1, R0);
        let b = relnum!(5, R1);
        let c = relnum!(100, R2);

        assert_eq!((a + (b * 2 + c)).val(), 111);

        // TODO?
        // assert_eq!((a + b*2 + c).val(), 111);
    }

    #[test]
    fn test_multistage2() {
        let a1 = relnum!(1, R0);
        let a2 = relnum!(3, R0);
        let a3 = relnum!(10, R0);
        let b = relnum!(5, R1);

        assert_eq!((a3 + (a2 - a1) + b).val(), 17);

        // TODO?
        // assert_eq!((a3 + a2-a1 + b).val(), 17);
    }
}
