use crate::utility::*;

pub struct ClosedInterval<T>([T; 2]);

impl<T: PartialOrd + PartialEq + Clone + Copy> ClosedInterval<T> {
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
    pub fn contains(&self, val: T) -> bool {
        val <= self.high() && val >= self.low()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
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
}
