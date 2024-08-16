use derive_more::Display;
use strum_macros::EnumIter;

use super::bool_with_partial::{self, *};

/// Intended for determining how much a half_plane overlaps a shape.
/// Needs to account for exact points at start and end of coverage
/// All the sections of a closed intervals
#[derive(Clone, Hash, Eq, PartialEq, Debug, Display, Copy, EnumIter)]
pub enum RelativeIntervalLocation {
    After,
    End,
    Inside,
    Start,
    Before,
}
impl RelativeIntervalLocation {
    pub const VERY_TRUE: Self = Self::After;
    pub const EXACTLY_TRUE: Self = Self::End;
    pub const PARTIAL: Self = Self::Inside;
    pub const EXACTLY_FALSE: Self = Self::Start;
    pub const VERY_FALSE: Self = Self::Before;

    pub const MORE_THAN_FULL: Self = Self::After;
    pub const EXACTLY_FULL: Self = Self::End;
    pub const PARTIALLY_FULL: Self = Self::Inside;
    pub const EXACTLY_EMPTY: Self = Self::Start;
    pub const LESS_THAN_EMPTY: Self = Self::Before;

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
    // TODO: how treat edge cases of Start and End here?
    pub fn is_before_grey_zone(&self) -> BoolWithPartial {
        match self {
            RelativeIntervalLocation::After => BoolWithPartial::False,
            RelativeIntervalLocation::End => BoolWithPartial::Partial,
            RelativeIntervalLocation::Inside => BoolWithPartial::Partial,
            RelativeIntervalLocation::Start => BoolWithPartial::Partial,
            RelativeIntervalLocation::Before => BoolWithPartial::True,
        }
    }
    pub fn is_after_grey_zone(&self) -> BoolWithPartial {
        match self {
            RelativeIntervalLocation::After => BoolWithPartial::True,
            RelativeIntervalLocation::End => BoolWithPartial::Partial,
            RelativeIntervalLocation::Inside => BoolWithPartial::Partial,
            RelativeIntervalLocation::Start => BoolWithPartial::Partial,
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
