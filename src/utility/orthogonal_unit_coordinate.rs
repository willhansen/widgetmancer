use crate::utility::*;
use derive_more::Neg;

use super::QuarterTurnRotatable;

#[derive(Clone, Hash, Neg, Eq, PartialEq, Debug, Copy, Default)]
pub struct OrthogonalUnitCoordinate<T: SignedCoordinate>(T);

impl<P: SignedCoordinate> OrthogonalUnitCoordinate<P> {
    pub fn try_new(dir: impl Into<P>) -> Result<Self, &'static str> {
        let dir = dir.into();
        if !dir.is_orthogonal() {
            Err("not orthogonal")
        } else if !dir.is_unit_length() {
            Err("not unit length")
        } else {
            Ok(OrthogonalUnitCoordinate(dir))
        }
    }
    #[deprecated(note = "Use Coordinate trait functions directly instead")]
    pub fn step(&self) -> P {
        self.step
    }
    #[deprecated(note = "Use a Coordinate projection function instead")]
    pub fn pos_on_axis(&self, pos: P) -> P::DataType {
        self.step().dot(pos)
    }
}

impl<T> Coordinate for OrthogonalUnitCoordinate<T>
where
    T: Coordinate,
{
    type DataType = T::DataType;

    type UnitType = T::UnitType;

    type Floating = T::Floating;

    type OnGrid = T::OnGrid;

    fn x(&self) -> Self::DataType {
        self.0.x()
    }

    fn y(&self) -> Self::DataType {
        self.0.y();
    }

    fn new(x: Self::DataType, y: Self::DataType) -> Self {
        Self::try_new(T::new(x, y)).unwrap()
    }
}

impl<C: SignedCoordinate> From<C> for OrthogonalUnitCoordinate<C> {
    fn from(value: C) -> Self {
        Self::new(value.x(), value.y())
    }
}
impl<C: SignedCoordinate<UnitType = U>, U> From<QuarterTurnsCcw> for OrthogonalUnitCoordinate<C> {
    fn from(value: QuarterTurnsCcw) -> Self {
        // TODO: factor out this default to one place
        let default_direction_for_zero_turns = C::right();
        Self::new(default_direction_for_zero_turns.quarter_rotated_ccw(value.quarter_turns()))
    }
}
impl From<OrthogonalWorldStep> for WorldStep {
    fn from(value: OrthogonalWorldStep) -> Self {
        value.step()
    }
}
// TODO: move to KingWorldStep file
impl From<OrthogonalWorldStep> for KingWorldStep {
    fn from(value: OrthogonalWorldStep) -> Self {
        KingWorldStep::new(value.step())
    }
}
impl From<KingWorldStep> for OrthogonalWorldStep {
    fn from(value: KingWorldStep) -> Self {
        OrthogonalWorldStep::new(value.step)
    }
}
