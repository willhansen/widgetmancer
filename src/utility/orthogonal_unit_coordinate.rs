use crate::SignedCoordinate;
use derive_more::Neg;

#[derive(Clone, Hash, Neg, Eq, PartialEq, Debug, Copy, Default)]
pub struct OrthogonalUnitCoordinate<T: SignedCoordinate> {
    step: T,
}

impl<P: SignedCoordinate> OrthogonalUnitCoordinate<P> {
    pub fn new(dir: impl Into<P>) -> Self {
        let dir = dir.into();
        assert!(dir.is_unit_length());
        assert!(dir.is_orthogonal());
        OrthogonalUnitCoordinate { step: dir }
    }
    pub fn step(&self) -> P {
        self.step
    }
    pub fn pos_on_axis(&self, pos: P) -> P::DataType {
        self.step().dot(pos)
    }
}
