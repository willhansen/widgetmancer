use std::cmp::Ord;
use try_partialord::TryMinMax;

// TODO: better name
pub trait MinMaxThatWorkWithPartialOrd {
    fn min_that_works_with_partial_ord(&self, other: Self) -> Self;
    fn max_that_works_with_partial_ord(&self, other: Self) -> Self;
}

macro_rules! impl_min_max_that_work_for_partial_ord_for {
    (ord: $Type:ty) => {
        impl MinMaxThatWorkWithPartialOrd for $Type {
            fn min_that_works_with_partial_ord(&self, other: Self) -> Self {
                (*self).min(other)
            }
            fn max_that_works_with_partial_ord(&self, other: Self) -> Self {
                (*self).max(other)
            }
        }
    };
    (partial_ord: $Type:ty) => {
        impl MinMaxThatWorkWithPartialOrd for $Type {
            fn min_that_works_with_partial_ord(&self, other: Self) -> Self {
                [*self, other].into_iter().try_min().unwrap().unwrap()
            }
            fn max_that_works_with_partial_ord(&self, other: Self) -> Self {
                [*self, other].into_iter().try_max().unwrap().unwrap()
            }
        }
    }
}

impl_min_max_that_work_for_partial_ord_for!(ord: i32);
impl_min_max_that_work_for_partial_ord_for!(ord: u32);
impl_min_max_that_work_for_partial_ord_for!(partial_ord: f32);

#[cfg(test)]
mod tests {
    use super::*;


    #[test]
    fn test_i32() {
        assert_eq!(5i32.min_that_works_with_partial_ord(5i32), 5i32);
        assert_eq!(5i32.max_that_works_with_partial_ord(5i32), 5i32);
        assert_eq!(5i32.min_that_works_with_partial_ord(7i32), 5i32);
        assert_eq!(5i32.max_that_works_with_partial_ord(7i32), 7i32);
        assert_eq!((-5i32).min_that_works_with_partial_ord(200i32), -5i32);
        assert_eq!((-5i32).max_that_works_with_partial_ord(200i32), 200i32);
    }
    #[test]
    fn test_u32() {
        assert_eq!(5u32.min_that_works_with_partial_ord(5u32), 5u32);
        assert_eq!(5u32.max_that_works_with_partial_ord(5u32), 5u32);
        assert_eq!(5u32.min_that_works_with_partial_ord(7u32), 5u32);
        assert_eq!(5u32.max_that_works_with_partial_ord(7u32), 7u32);
    }
    #[test]
    fn test_f32() {
        assert_eq!(5f32.min_that_works_with_partial_ord(5f32), 5f32);
        assert_eq!(5f32.max_that_works_with_partial_ord(5f32), 5f32);

        assert_eq!(5f32.min_that_works_with_partial_ord(7f32), 5f32);
        assert_eq!(5f32.max_that_works_with_partial_ord(7f32), 7f32);
        assert_eq!((-5f32).min_that_works_with_partial_ord(200f32), -5f32);
        assert_eq!((-5f32).max_that_works_with_partial_ord(200f32), 200f32);

        assert_eq!(f32::INFINITY.min_that_works_with_partial_ord(f32::INFINITY), f32::INFINITY);
        assert_eq!(f32::INFINITY.max_that_works_with_partial_ord(f32::INFINITY), f32::INFINITY);

        assert_eq!(f32::NEG_INFINITY.min_that_works_with_partial_ord(f32::INFINITY), f32::NEG_INFINITY);
        assert_eq!(f32::NEG_INFINITY.max_that_works_with_partial_ord(f32::INFINITY), f32::INFINITY);

        assert_eq!(f32::NEG_INFINITY.min_that_works_with_partial_ord(f32::NEG_INFINITY), f32::NEG_INFINITY);
        assert_eq!(f32::NEG_INFINITY.max_that_works_with_partial_ord(f32::NEG_INFINITY), f32::NEG_INFINITY);

        assert_eq!(f32::INFINITY.min_that_works_with_partial_ord(f32::NEG_INFINITY), f32::NEG_INFINITY);
        assert_eq!(f32::INFINITY.max_that_works_with_partial_ord(f32::NEG_INFINITY), f32::INFINITY);
    }
}
