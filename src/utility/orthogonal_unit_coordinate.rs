use crate::utility::*;
use derive_more::Neg;

use super::QuarterTurnRotatable;

#[derive(
    Clone,
    Hash,
    Neg,
    Eq,
    PartialEq,
    Debug,
    Copy,
    Default,
    derive_more::Add,
    derive_more::Sub,
    derive_more::Mul,
    derive_more::Div,
)]
pub struct OrthogonalUnitCoordinate<T: Coordinate>(T);

impl<P: Coordinate> OrthogonalUnitCoordinate<P> {
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
        self.0
    }
    #[deprecated(note = "Use a Coordinate projection function instead")]
    pub fn pos_on_axis(&self, pos: P) -> P::DataType {
        self.step().dot(pos)
    }
}

impl<T: Coordinate> euclid::num::Zero for OrthogonalUnitCoordinate<T> {
    fn zero() -> Self {
        Self(T::zero())
    }
}

impl<D, T: Coordinate<DataType = D>> From<(D, D)> for OrthogonalUnitCoordinate<T> {
    fn from(value: (D, D)) -> Self {
        Self(value.into())
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
        self.0.y()
    }

    fn new(x: Self::DataType, y: Self::DataType) -> Self {
        Self::try_new(T::new(x, y)).unwrap()
    }
}

// TODO: why implementation conflict?
// impl<C: SignedCoordinate> TryFrom<C> for OrthogonalUnitCoordinate<C> {
//     type Error = &'static str;

//     fn try_from(value: C) -> Result<Self, Self::Error> {
//         Self::try_new(value)
//     }
// }
impl<C: SignedCoordinate> From<C> for OrthogonalUnitCoordinate<C> {
    fn from(value: C) -> Self {
        Self::try_new(value).unwrap()
    }
}
impl<C: SignedCoordinate<UnitType = U>, U> From<QuarterTurnsCcw> for OrthogonalUnitCoordinate<C> {
    fn from(value: QuarterTurnsCcw) -> Self {
        // TODO: factor out this default to one place
        let default_direction_for_zero_turns = C::right();
        Self::from(default_direction_for_zero_turns.quarter_rotated_ccw(value.quarter_turns()))
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
impl TryFrom<KingWorldStep> for OrthogonalWorldStep {
    type Error = &'static str;

    fn try_from(value: KingWorldStep) -> Result<Self, Self::Error> {
        value.step().try_into()
    }
}
