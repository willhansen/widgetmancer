use super::bool_with_partial::*;

/// Intended for determining how much a halfplane overlaps a shape.
/// Needs to account for exact points at start and end of coverage
/// All the sections of a closed intervals
#[derive(Clone, Hash, Eq, PartialEq, Debug, Copy)]
pub enum IntervalLocation {
    After,
    End,
    During,
    Start,
    Before,
}
impl IntervalLocation {
    pub fn on_closed_interval(&self) -> bool {
        match self {
            IntervalLocation::After => false,
            IntervalLocation::End => true,
            IntervalLocation::During => true,
            IntervalLocation::Start => true,
            IntervalLocation::Before => false,
        }
    }
    pub fn on_open_interval(&self) -> bool {
        match self {
            IntervalLocation::After => false,
            IntervalLocation::End => false,
            IntervalLocation::During => true,
            IntervalLocation::Start => false,
            IntervalLocation::Before => false,
        }
    }
    pub fn is_before(&self) -> BoolWithPartial {
        match self {
            IntervalLocation::After => BoolWithPartial::False,
            IntervalLocation::End => BoolWithPartial::False,
            IntervalLocation::During => BoolWithPartial::False,
            IntervalLocation::Start => BoolWithPartial::Partial,
            IntervalLocation::Before => BoolWithPartial::True,
        }
    }
    pub fn is_after(&self) -> BoolWithPartial {
        match self {
            IntervalLocation::After => BoolWithPartial::True,
            IntervalLocation::End => BoolWithPartial::Partial,
            IntervalLocation::During => BoolWithPartial::False,
            IntervalLocation::Start => BoolWithPartial::False,
            IntervalLocation::Before => BoolWithPartial::False,
        }
    }
}
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_is_before_is_after() {
        assert_eq!(IntervalLocation::End.is_after(), BoolWithPartial::Partial)
    }
}
