use crate::{QuarterTurnsCcw, RigidTransform, RigidlyTransformable, SignedCoordinate, STEP_RIGHT};
use derive_more::Neg;

use super::QuarterTurnRotatable;

#[derive(Clone, Hash, Neg, Eq, PartialEq, Debug, Copy, Default)]
pub struct OrthogonalUnitCoordinate<T: SignedCoordinate> {
    step: T,
}

impl<P: SignedCoordinate> OrthogonalUnitCoordinate<P> {
    pub fn new(dir: impl Into<P>) -> Self {
        Self::try_new(dir).unwrap()
    }
    pub fn try_new(dir: impl Into<P>) -> Result<Self, &'static str> {
        let dir = dir.into();
        if !dir.is_orthogonal() {
            Err("not orthogonal")
        } else if !dir.is_unit_length() {
            Err("not unit length")
        } else {
            Ok(OrthogonalUnitCoordinate { step: dir })
        }
    }
    pub fn step(&self) -> P {
        self.step
    }
    pub fn pos_on_axis(&self, pos: P) -> P::DataType {
        self.step().dot(pos)
    }
}
impl<C: SignedCoordinate> RigidlyTransformable for OrthogonalUnitCoordinate<C> {
    fn apply_rigid_transform(&self, tf: RigidTransform) -> Self {
        tf.rotation().rotate_vector(self.step()).try_into().unwrap()
    }
}
impl<T, C: SignedCoordinate> From<(T, T)> for OrthogonalUnitCoordinate<C>
where
    (T, T): Into<C>,
{
    fn from(value: (T, T)) -> Self {
        Self::new(value)
    }
}

// TODO: generate with macro
impl<C: SignedCoordinate> QuarterTurnRotatable for OrthogonalUnitCoordinate<C> {
    fn quarter_rotated_ccw(&self, quarter_turns_ccw: impl Into<QuarterTurnsCcw> + Copy) -> Self {
        self.step().quarter_rotated_ccw(quarter_turns_ccw).into()
    }
}

impl<C: SignedCoordinate> From<C> for OrthogonalUnitCoordinate<C> {
    fn from(value: C) -> Self {
        Self::new(value)
    }
}
impl<C: SignedCoordinate<UnitType = U>, U> From<QuarterTurnsCcw> for OrthogonalUnitCoordinate<C> {
    fn from(value: QuarterTurnsCcw) -> Self {
        Self::new(C::right().quarter_rotated_ccw(value.quarter_turns()))
    }
}
