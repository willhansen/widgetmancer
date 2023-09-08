pub use getset::CopyGetters;
pub use itertools::Itertools;
pub use ntest::assert_false;
pub use std::ops::{Add, Sub};
pub use std::{
    collections::{HashMap, HashSet},
    f32::consts::{PI, TAU},
    fmt::{Debug, Display, Formatter},
    hash::Hash,
    mem,
    ops::Neg,
};

pub use derive_more::Neg;

pub use num::Signed;
pub use ordered_float::OrderedFloat;

pub type SimpleResult = Result<(), ()>;

pub fn sign(x: f32) -> f32 {
    if x < 0.0 {
        -1.0
    } else if x > 0.0 {
        1.0
    } else {
        0.0
    }
}
pub fn int_to_T<T: Signed>(x: i32) -> T {
    match x {
        1 => T::one(),
        0 => T::zero(),
        -1 => -T::one(),
        _ => panic!(),
    }
}

pub fn int_cos(quarter_periods: i32) -> i32 {
    match quarter_periods.rem_euclid(4) {
        0 => 1,
        1 | 3 => 0,
        2 => -1,
        _ => panic!(),
    }
}

pub fn int_sin(quarter_periods: i32) -> i32 {
    match quarter_periods.rem_euclid(4) {
        0 | 2 => 0,
        1 => 1,
        3 => -1,
        _ => panic!(),
    }
}
pub fn lerp(a: f32, b: f32, t: f32) -> f32 {
    a * (1.0 - t) + b * t
}

pub fn derivative(f: fn(f32) -> f32, x: f32, dx: f32) -> f32 {
    if dx == 0.0 {
        panic!("approximate derivatives only!");
    }
    (f(x + dx / 2.0) - f(x - dx / 2.0)) / dx
}
pub fn map_sum<K, V>(a: HashMap<K, V>, b: HashMap<K, V>) -> HashMap<K, V>
where
    K: Eq + Hash,
    V: Default + std::ops::AddAssign,
{
    let mut sum = HashMap::new();
    for m in [a, b] {
        m.into_iter().for_each(|(k, v): (K, V)| {
            *sum.entry(k).or_default() += v;
        });
    }
    sum
}

pub fn map_neg<K, V>(m: HashMap<K, V>) -> HashMap<K, V>
where
    K: Eq + Hash,
    V: Neg<Output = V>,
{
    m.into_iter().map(|(k, v): (K, V)| (k, -v)).collect()
}

pub fn map_to_signed<K>(m: HashMap<K, u32>) -> HashMap<K, i32>
where
    K: Eq + Hash,
{
    m.into_iter()
        .map(|(k, v): (K, u32)| (k, v as i32))
        .collect()
}

pub fn map_to_float<K>(m: HashMap<K, i32>) -> HashMap<K, f32>
where
    K: Eq + Hash,
{
    m.into_iter()
        .map(|(k, v): (K, i32)| (k, v as f32))
        .collect()
}
pub fn set_of_keys<K, V>(hashmap: &HashMap<K, V>) -> HashSet<K>
where
    K: Clone + Hash + Eq,
{
    hashmap.keys().cloned().collect::<HashSet<K>>()
}

pub fn union<T: Clone + Hash + Eq>(a: &HashSet<T>, b: &HashSet<T>) -> HashSet<T> {
    a.union(b).cloned().collect()
}

pub fn intersection<T: Clone + Hash + Eq>(a: &HashSet<T>, b: &HashSet<T>) -> HashSet<T> {
    a.intersection(b).cloned().collect()
}
pub fn snap_to_nths(x: f32, denominator: u32) -> f32 {
    (x * denominator as f32).round() / denominator as f32
}
pub fn looping_clamp(a: f32, b: f32, x: f32) -> f32 {
    assert!(a < b);
    ((x - a).rem_euclid(b - a)) + a
}
pub fn rotated_to_have_split_at_max<T: Copy>(vec: &Vec<T>, f: impl Fn(T, T) -> f32) -> Vec<T> {
    let index_of_new_end: usize = vec
        .iter()
        .cloned()
        .circular_tuple_windows()
        .position_max_by_key(|pair: &(T, T)| OrderedFloat(f(pair.0, pair.1)))
        .unwrap()
        + 1;

    let mut the_clone = vec.clone();
    the_clone.rotate_left(index_of_new_end);
    the_clone
}

pub trait TupleClone {
    type TupleType;
    fn tuple_clone(&self) -> Self::TupleType;
}
impl<A: Clone, B: Clone> TupleClone for (&A, &B) {
    type TupleType = (A, B);

    fn tuple_clone(&self) -> Self::TupleType {
        {
            let x = *self;
            (x.0.clone(), x.1.clone())
        }
    }
}
// TODO: learn macros
impl<A: Clone, B: Clone, C: Clone> TupleClone for (&A, &B, &C) {
    type TupleType = (A, B, C);

    fn tuple_clone(&self) -> Self::TupleType {
        {
            let x = *self;
            (x.0.clone(), x.1.clone(), x.2.clone())
        }
    }
}
pub fn all_true<'a>(v: impl IntoIterator<Item = &'a bool>) -> bool {
    v.into_iter().all(|&x| x)
}
pub fn any_true<'a>(v: impl IntoIterator<Item = &'a bool>) -> bool {
    v.into_iter().any(|&x| x)
}

pub fn get_column<const ROWS: usize, const COLS: usize, T: Copy>(
    a: &[[T; COLS]; ROWS],
    col: usize,
) -> [T; ROWS] {
    a.map(|row| row[col])
}

#[derive(Clone, Hash, Eq, PartialEq, Debug, Copy)]
pub enum BoolWithPartial {
    True,
    Partial,
    False,
}

#[cfg(test)]
mod tests {
    use std::array::from_fn;

    use ntest::{assert_about_eq, assert_false, assert_true, timeout};
    use pretty_assertions::{assert_eq, assert_ne};

    use super::*;
    #[test]
    fn test_looping_clamp() {
        assert_about_eq!(looping_clamp(0.0, 5.0, 3.0), 3.0); // in range
        assert_about_eq!(looping_clamp(0.0, 5.0, 5.1), 0.1); // above
        assert_about_eq!(looping_clamp(0.0, 5.0, -0.1), 4.9); // below
        assert_about_eq!(looping_clamp(-1.0, 5.0, 11.1), -0.9); // multiple periods above
        assert_about_eq!(looping_clamp(-1.5, 1.5, 2.0), -1.0); // fraction above
    }

    #[test]
    fn test_vec_rotated_to_max() {
        // up and down
        assert_eq!(
            rotated_to_have_split_at_max(&vec![0, 1, 2, 3, 1, 1], |a, b| (a - b).abs() as f32),
            vec![1, 1, 0, 1, 2, 3]
        );

        // find max
        assert_eq!(
            rotated_to_have_split_at_max(&vec![0, 1, 2, 7, 6, 3], |a, b| (a - b).abs() as f32),
            vec![7, 6, 3, 0, 1, 2]
        );

        // no change
        assert_eq!(
            rotated_to_have_split_at_max(&vec![0, 1, 2, 3, 4, 5], |a, b| (a - b).abs() as f32),
            vec![0, 1, 2, 3, 4, 5]
        );
    }
    #[test]
    fn test_all_true() {
        assert!(all_true(&vec![true, true, true]));
        assert!(all_true(&[true, true, true]));
        assert_false!(all_true(&[true, false, true]));
        assert_false!(all_true(&vec![true, true, false]));
    }
    #[test]
    fn test_any_true() {
        assert!(any_true(&vec![true, true, true]));
        assert!(any_true(&[false, true, false]));
        assert_false!(any_true(&[false, false, false]));
        assert_false!(any_true(&vec![false, false, false]));
    }
    #[test]
    fn test_get_column() {
        assert_eq!(get_column(&[[1, 2, 3], [4, 5, 6],], 1), [2, 5]);
    }
}
