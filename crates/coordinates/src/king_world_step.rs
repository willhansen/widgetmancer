// use crate::utility::*;

#[derive(Clone, Hash, Eq, PartialEq, Debug, Copy)]
pub struct KingWorldStep {
    step: WorldStep,
}

impl KingWorldStep {
    pub fn new(dir: WorldStep) -> Self {
        assert!(dir.is_king_step());
        KingWorldStep { step: dir }
    }
    pub fn step(&self) -> WorldStep {
        self.step
    }
}

impl From<OrthogonalDirection> for KingWorldStep {
    fn from(value: OrthogonalDirection) -> Self {
        KingWorldStep::new(value.to_step())
    }
}

// TODO: generate with macro
impl QuarterTurnRotatable for KingWorldStep {
    fn quarter_rotated_ccw(&self, quarter_turns_ccw: impl Into<NormalizedOrthoAngle>) -> Self {
        self.step().quarter_rotated_ccw(quarter_turns_ccw).into()
    }
}

impl From<WorldStep> for KingWorldStep {
    fn from(value: WorldStep) -> Self {
        KingWorldStep::new(value)
    }
}

impl From<KingWorldStep> for WorldStep {
    fn from(value: KingWorldStep) -> Self {
        value.step
    }
}
