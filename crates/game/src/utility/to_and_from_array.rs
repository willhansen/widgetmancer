use crate::utility::*;

pub trait ToAndFromArray<T, const N: usize> {
    fn array(&self) -> [T; N];
    fn from_array(arr: [T; N]) -> Self;
}
