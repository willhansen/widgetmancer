use crate::utility::*;
use derive_getters::Getters;
use derive_more::Constructor;

#[derive(Clone, Copy, Debug, Constructor, Getters)]
pub struct ThingAtCoord<T, P: CoordinateOps> {
    thing: T,
    coord: P,
}

impl<T, P: CoordinateOps> ThingAtCoord<T, P> {
    pub fn revolved(&self, angle: QuarterTurns) -> Self
    where
        T: QuarterTurnRotatable,
        P: QuarterTurnRotatable,
    {
        Self::new(
            self.thing.quarter_rotated_ccw(angle),
            self.coord.quarter_rotated_ccw(angle),
        )
    }
    pub fn rotated_in_place(&self, angle: QuarterTurns) -> Self
    where
        T: QuarterTurnRotatable,
    {
        Self::new(self.thing.quarter_rotated_ccw(angle), self.coord)
    }
}
