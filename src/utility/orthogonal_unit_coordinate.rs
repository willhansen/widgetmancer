use crate::utility::*;
use derive_more::Neg;

#[derive(Clone, Hash, Neg, Eq, PartialEq, Debug, Copy)]
#[deprecated(note = "Use an orthogonal direction instead")]
pub struct OrthogonalUnitCoordinate<T: Coordinate>(T);

impl<P: Coordinate> OrthogonalUnitCoordinate<P> {
    fn new(p: impl Into<P>) -> Self {
        Self::try_new(p).unwrap()
    }

    pub fn try_new(dir: impl Into<P>) -> Result<Self, String> {
        let dir = dir.into();
        if !dir.is_orthogonal() {
            Err(format!("not orthogonal: {:?}", dir))
        } else if !dir.is_unit_length() {
            Err(format!("not unit length: {:?}", dir))
        } else {
            Ok(OrthogonalUnitCoordinate(dir))
        }
    }
    pub fn step(&self) -> P {
        self.0
    }
    #[deprecated(note = "Use a Coordinate projection function instead")]
    pub fn pos_on_axis(&self, pos: P) -> P::DataType {
        self.step().dot(pos)
    }
}

// impl<C: SignedCoordinate> TryFrom<C> for OrthogonalUnitCoordinate<C> {
//     type Error = String;

//     fn try_from(value: C) -> Result<Self, Self::Error> {
//         Self::try_new(value)
//     }
// }
impl<C: SignedCoordinate> From<C> for OrthogonalUnitCoordinate<C> {
    fn from(value: C) -> Self {
        Self::try_new(value).unwrap()
    }
}

impl<C: SignedCoordinate> QuarterTurnRotatable for OrthogonalUnitCoordinate<C> {
    fn quarter_rotated_ccw(&self, quarter_turns_ccw: impl Into<QuarterTurnsCcw> + Copy) -> Self {
        Self::new(self.0.quarter_rotated_ccw(quarter_turns_ccw))
    }
}

impl<C: SignedCoordinate<UnitType = U>, U> From<QuarterTurnsCcw> for OrthogonalUnitCoordinate<C> {
    fn from(value: QuarterTurnsCcw) -> Self {
        // TODO: factor out this default to one place
        let default_direction_for_zero_turns = C::right();
        Self::new(default_direction_for_zero_turns.quarter_rotated_ccw(value.quarter_turns()))
    }
}

impl TryFrom<KingWorldStep> for OrthogonalWorldStep {
    type Error = String;

    fn try_from(value: KingWorldStep) -> Result<Self, Self::Error> {
        Self::try_new(value.step())
    }
}
