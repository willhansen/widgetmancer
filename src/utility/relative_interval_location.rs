use super::bool_with_partial::*;

/// Intended for determining how much a halfplane overlaps a shape.
/// Needs to account for exact points at start and end of coverage
/// All the sections of a closed intervals
#[derive(Clone, Hash, Eq, PartialEq, Debug, Copy)]
pub enum RelativeIntervalLocation {
    After,
    End,
    During,
    Start,
    Before,
}
impl RelativeIntervalLocation {
    pub fn on_closed_interval(&self) -> bool {
        match self {
            RelativeIntervalLocation::After => false,
            RelativeIntervalLocation::End => true,
            RelativeIntervalLocation::During => true,
            RelativeIntervalLocation::Start => true,
            RelativeIntervalLocation::Before => false,
        }
    }
    pub fn on_open_interval(&self) -> bool {
        match self {
            RelativeIntervalLocation::After => false,
            RelativeIntervalLocation::End => false,
            RelativeIntervalLocation::During => true,
            RelativeIntervalLocation::Start => false,
            RelativeIntervalLocation::Before => false,
        }
    }
    pub fn is_before(&self) -> BoolWithPartial {
        match self {
            RelativeIntervalLocation::After => BoolWithPartial::False,
            RelativeIntervalLocation::End => BoolWithPartial::False,
            RelativeIntervalLocation::During => BoolWithPartial::False,
            RelativeIntervalLocation::Start => BoolWithPartial::Partial,
            RelativeIntervalLocation::Before => BoolWithPartial::True,
        }
    }
    pub fn is_after(&self) -> BoolWithPartial {
        match self {
            RelativeIntervalLocation::After => BoolWithPartial::True,
            RelativeIntervalLocation::End => BoolWithPartial::Partial,
            RelativeIntervalLocation::During => BoolWithPartial::False,
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
