use crate::utility::*;
use derive_getters::Getters;
use derive_more::Constructor;

trait_alias_macro!(pub trait PointReqsForThingAtSquare = IntCoordinateOps);
trait_alias_macro!(trait PointReqs = PointReqsForThingAtSquare);

#[derive(Clone, Copy, Debug, Constructor, PartialEq, Eq)]
pub struct ThingRelToSquare<T, P: PointReqs> {
    thing: T,
    square: P,
}

impl<T, P: PointReqs> ThingRelToSquare<T, P> {
    pub fn thing(&self) -> &T {
        &self.thing
    }
    pub fn square(&self) -> P {
        self.square
    }
    pub fn to_local(&self) -> &T {
        self.thing()
    }
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

macro_rules! impl_abstraction_for_wrapped_thing {
    ($Unwrapped:ident<P: $PointReqs:ident>, abstraction_base= $Wrapped:ident<P>) => {
        impl<PointType: $PointReqs> Abstraction<$Wrapped<PointType>> for $Unwrapped<PointType> {}
        impl<PointType: $PointReqs> From<$Wrapped<PointType>> for $Unwrapped<PointType> {
            fn from(value: $Wrapped<PointType>) -> Self {
                *value.thing()
            }
        }
    };
}
pub(crate) use impl_abstraction_for_wrapped_thing;
