#[derive(Copy, Clone, Eq, PartialEq)]
pub enum AngularDirection {
    CW,
    CCW,
}

impl std::ops::Neg for AngularDirection {
    type Output = Self;

    fn neg(self) -> Self::Output {
        use AngularDirection::*;
        match self {
            CW => CCW,
            CCW => CW,
        }
    }
}
