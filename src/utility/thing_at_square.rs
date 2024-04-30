use crate::utility::*;
use derive_getters::Getters;
use derive_more::Constructor;

trait_alias_macro!(pub trait PointReqsForThingAtSquare = IntCoordinateOps);
trait_alias_macro!(trait PointReqs = PointReqsForThingAtSquare);

#[derive(Clone, Copy, Debug, Constructor, Getters)]
pub struct ThingAtSquare<T, P: PointReqs> {
    thing: T,
    square: P,
}

impl<T, P: PointReqs> ThingAtSquare<T, P> {
    pub fn revolved(&self, angle: QuarterTurns) -> Self
    where
        T: QuarterTurnRotatable,
        P: QuarterTurnRotatable,
    {
        Self::new(
            self.thing.quarter_rotated_ccw(angle),
            self.square.quarter_rotated_ccw(angle),
        )
    }
    pub fn rotated_in_place(&self, angle: QuarterTurns) -> Self
    where
        T: QuarterTurnRotatable,
    {
        Self::new(self.thing.quarter_rotated_ccw(angle), self.square)
    }
}
