use crate::utility::*;

pub struct ClosedInterval<T>([T; 2]);

impl<T> ClosedInterval<T>
where
    T: num::Num + PartialOrd + PartialEq + Clone + Copy + num::Signed,
{
    pub fn new(low: T, high: T) -> Self {
        assert!(low <= high);
        ClosedInterval([low, high])
    }
    pub fn low(&self) -> T {
        self.0[0]
    }
    pub fn high(&self) -> T {
        self.0[1]
    }
    pub fn bounds(&self) -> [T; 2] {
        self.0
    }
    pub fn contains(&self, val: T) -> bool {
        val <= self.high() && val >= self.low()
    }
    pub fn depth_of(&self, val: T) -> T {
        let dist_to_closer_bound = *self
            .bounds()
            .map(|b| abs(b - val))
            .iter()
            .try_min()
            .unwrap()
            .unwrap();
        if self.contains(val) {
            dist_to_closer_bound
        } else {
            -dist_to_closer_bound
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use ntest::assert_about_eq;
    use pretty_assertions::{assert_eq, assert_ne};

    #[test]
    fn test_closed_interval__i32() {
        let i = ClosedInterval::new(-4, 5);
        assert!(i.contains(0));
        assert!(i.contains(-4));
        assert!(i.contains(5));
        assert_false!(i.contains(-5));
        assert_false!(i.contains(10));
    }
    #[test]
    fn test_depth__f32() {
        let i = ClosedInterval::new(10.0, 20.0);
        assert_about_eq!(i.depth_of(12.0), 2.0);
        assert_about_eq!(i.depth_of(17.0), 3.0);
        assert_about_eq!(i.depth_of(25.0), -5.0);
    }
}
