use std::ops::{Add, Sub};

use derive_more::Display;
use num::{Num, Zero};

#[derive(Clone, Hash, Eq, PartialEq, Debug, Display, Copy)]
pub enum BoolWithPartial {
    True,
    Partial,
    False,
}

impl BoolWithPartial {
    pub fn is_true(&self) -> bool {
        matches!(self, Self::True)
    }
    pub fn is_partial(&self) -> bool {
        matches!(self, Self::Partial)
    }
    pub fn is_false(&self) -> bool {
        matches!(self, Self::False)
    }
    pub fn is_at_least_partial(&self) -> bool {
        !self.is_false()
    }
    pub fn is_at_most_partial(&self) -> bool {
        !self.is_true()
    }
    fn to_number(&self) -> i32 {
        match self {
            BoolWithPartial::True => 1,
            BoolWithPartial::Partial => 0,
            BoolWithPartial::False => -1,
        }
    }
    fn from_number(number: i32) -> Self {
        match number.clamp(-1, 1) {
            1 => BoolWithPartial::True,
            0 => BoolWithPartial::Partial,
            -1 => BoolWithPartial::False,
            _ => panic!("invalid number: {}", number),
        }
    }
    pub fn and(&self, other: Self) -> Self {
        Self::from_number(self.to_number().min(other.to_number()))
    }
    pub fn or(&self, other: Self) -> Self {
        Self::from_number(self.to_number().max(other.to_number()))
    }
    pub fn not(&self) -> Self {
        Self::from_number(-self.to_number())
    }
    pub fn from_less_than<T: Num + PartialOrd + Copy>(smaller: T, bigger: T) -> Self {
        Self::from_less_than_with_tolerance(smaller, bigger, T::zero())
    }
    pub fn from_less_than_with_tolerance<T: Num + PartialOrd + Copy>(
        smaller: T,
        bigger: T,
        tolerance: T,
    ) -> Self {
        assert!(tolerance >= T::zero());
        if smaller > bigger + tolerance {
            BoolWithPartial::False
        } else if smaller < bigger - tolerance {
            BoolWithPartial::True
        } else {
            BoolWithPartial::Partial
        }
    }
    pub fn any(iter: impl IntoIterator<Item = Self>) -> Self {
        iter.into_iter()
            .map(|x| x.to_number())
            .max()
            .map(BoolWithPartial::from_number)
            .unwrap_or(BoolWithPartial::False)
    }
    pub fn all(iter: impl IntoIterator<Item = Self>) -> Self {
        iter.into_iter()
            .map(|x| x.to_number())
            .min()
            .map(BoolWithPartial::from_number)
            .unwrap_or(BoolWithPartial::True)
    }
}

impl From<bool> for BoolWithPartial {
    fn from(value: bool) -> Self {
        match value {
            true => BoolWithPartial::True,
            false => BoolWithPartial::False,
        }
    }
}
impl TryFrom<BoolWithPartial> for bool {
    type Error = ();

    fn try_from(value: BoolWithPartial) -> Result<Self, Self::Error> {
        match value {
            BoolWithPartial::True => Ok(true),
            BoolWithPartial::Partial => Err(()),
            BoolWithPartial::False => Ok(false),
        }
    }
}
#[cfg(test)]
mod tests {

    use super::*;
    #[test]
    fn test_bool_with_partial__and() {
        let t = BoolWithPartial::True;
        let p = BoolWithPartial::Partial;
        let f = BoolWithPartial::False;
        assert_eq!(t.and(t), t);
        assert_eq!(t.and(p), p);
        assert_eq!(t.and(f), f);
        assert_eq!(p.and(t), p);
        assert_eq!(p.and(p), p);
        assert_eq!(p.and(f), f);
        assert_eq!(f.and(t), f);
        assert_eq!(f.and(p), f);
        assert_eq!(f.and(f), f);
    }
    #[test]
    fn test_bool_with_partial__or() {
        let t = BoolWithPartial::True;
        let p = BoolWithPartial::Partial;
        let f = BoolWithPartial::False;
        assert_eq!(t.or(t), t);
        assert_eq!(t.or(p), t);
        assert_eq!(t.or(f), t);
        assert_eq!(p.or(t), t);
        assert_eq!(p.or(p), p);
        assert_eq!(p.or(f), p);
        assert_eq!(f.or(t), t);
        assert_eq!(f.or(p), p);
        assert_eq!(f.or(f), f);
    }
    #[test]
    fn test_bool_with_partial__not() {
        let t = BoolWithPartial::True;
        let p = BoolWithPartial::Partial;
        let f = BoolWithPartial::False;
        assert_eq!(t.not(), f);
        assert_eq!(p.not(), p);
        assert_eq!(f.not(), t);
    }
    #[test]
    fn test_bool_with_partial__from_less_than() {
        let t = BoolWithPartial::True;
        let p = BoolWithPartial::Partial;
        let f = BoolWithPartial::False;
        assert_eq!(BoolWithPartial::from_less_than(-1, 0), t);
        assert_eq!(BoolWithPartial::from_less_than(1.0, 0.0), f);
        assert_eq!(BoolWithPartial::from_less_than(0, 0), p);
    }
    #[test]
    fn test_bool_with_partial__from_less_than_with_tolerance() {
        let t = BoolWithPartial::True;
        let p = BoolWithPartial::Partial;
        let f = BoolWithPartial::False;

        // true, outside tolerance
        assert_eq!(
            BoolWithPartial::from_less_than_with_tolerance(-1.0, 0.0, 0.0),
            t
        );
        assert_eq!(BoolWithPartial::from_less_than_with_tolerance(5, 50, 10), t);
        // true, on tolerance border
        assert_eq!(BoolWithPartial::from_less_than_with_tolerance(0, 1, 1), p);
        // true, but within tolerance
        assert_eq!(
            BoolWithPartial::from_less_than_with_tolerance(5.0, 10.0, 7.0),
            p
        );
        assert_eq!(
            BoolWithPartial::from_less_than_with_tolerance(5.0, 6.0, 9.0),
            p
        );
        // exact
        assert_eq!(
            BoolWithPartial::from_less_than_with_tolerance(5.0, 5.0, 0.0),
            p
        );
        // false, but within tolerance
        assert_eq!(
            BoolWithPartial::from_less_than_with_tolerance(7.0, 5.0, 3.0),
            p
        );
        // false, on tolerance border
        assert_eq!(
            BoolWithPartial::from_less_than_with_tolerance(7.0, 5.0, 2.0),
            p
        );
        // false, outside tolerance
        assert_eq!(
            BoolWithPartial::from_less_than_with_tolerance(70.0, 5.0, 2.0),
            f
        );
    }
    #[test]
    fn test_bool_with_partial__any() {
        let t = BoolWithPartial::True;
        let p = BoolWithPartial::Partial;
        let f = BoolWithPartial::False;

        // has true
        assert_eq!(BoolWithPartial::any([t, t, f, p, t]), t);
        assert_eq!(BoolWithPartial::any(vec![t, t, f, p, t]), t);
        assert_eq!(BoolWithPartial::any(vec![t, t, f, p, t].into_iter()), t);
        // has partial
        assert_eq!(BoolWithPartial::any([f, p, f, p, p]), p);
        assert_eq!(BoolWithPartial::any(vec![f, p, f, p, p]), p);
        assert_eq!(BoolWithPartial::any(vec![f, p, f, p, p].into_iter()), p);
        // only has false
        assert_eq!(BoolWithPartial::any([f, f, f, f, f]), f);
        // empty
        assert_eq!(BoolWithPartial::any([]), f);
    }
    #[test]
    fn test_bool_with_partial__all() {
        let t = BoolWithPartial::True;
        let p = BoolWithPartial::Partial;
        let f = BoolWithPartial::False;

        // has false
        assert_eq!(BoolWithPartial::all([t, t, f, p, t]), f);
        assert_eq!(BoolWithPartial::all(vec![t, t, f, p, t]), f);
        assert_eq!(BoolWithPartial::all(vec![t, t, f, p, t].into_iter()), f);
        // has partial
        assert_eq!(BoolWithPartial::all([t, p, t, p, p]), p);
        assert_eq!(BoolWithPartial::all(vec![t, p, t, p, p]), p);
        assert_eq!(BoolWithPartial::all(vec![t, p, t, p, p].into_iter()), p);
        // only has true
        assert_eq!(BoolWithPartial::all([t, t, t, t, t]), t);
        // empty
        assert_eq!(BoolWithPartial::all([]), t);
    }
}
