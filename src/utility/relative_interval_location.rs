use super::bool_with_partial::{self, *};

/// Intended for determining how much a halfplane overlaps a shape.
/// Needs to account for exact points at start and end of coverage
/// All the sections of a closed intervals
#[derive(Clone, Hash, Eq, PartialEq, Debug, Copy)]
pub enum RelativeIntervalLocation {
    After,
    End,
    Inside,
    Start,
    Before,
}
impl RelativeIntervalLocation {
    pub fn in_interval(&self) -> BoolWithPartial {
        match self {
            RelativeIntervalLocation::After => false.into(),
            RelativeIntervalLocation::End => BoolWithPartial::Partial,
            RelativeIntervalLocation::Inside => true.into(),
            RelativeIntervalLocation::Start => BoolWithPartial::Partial,
            RelativeIntervalLocation::Before => false.into(),
        }
    }
    pub fn in_closed_interval(&self) -> bool {
        self.in_interval().is_at_least_partial()
    }
    pub fn in_open_interval(&self) -> bool {
        self.in_interval().is_true()
    }
    pub fn is_before(&self) -> BoolWithPartial {
        match self {
            RelativeIntervalLocation::After => BoolWithPartial::False,
            RelativeIntervalLocation::End => BoolWithPartial::False,
            RelativeIntervalLocation::Inside => BoolWithPartial::False,
            RelativeIntervalLocation::Start => BoolWithPartial::Partial,
            RelativeIntervalLocation::Before => BoolWithPartial::True,
        }
    }
    pub fn is_after(&self) -> BoolWithPartial {
        match self {
            RelativeIntervalLocation::After => BoolWithPartial::True,
            RelativeIntervalLocation::End => BoolWithPartial::Partial,
            RelativeIntervalLocation::Inside => BoolWithPartial::False,
            RelativeIntervalLocation::Start => BoolWithPartial::False,
            RelativeIntervalLocation::Before => BoolWithPartial::False,
        }
    }
}
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_is_before_is_after() {
        assert_eq!(
            RelativeIntervalLocation::End.is_after(),
            BoolWithPartial::Partial
        )
    }
}
