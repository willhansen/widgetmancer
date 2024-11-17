use crate::*;
use angles::*;

#[derive(Clone, Hash, Eq, PartialEq, Debug, Copy)]
pub struct KingDirection {
    step: ICoord,
}


impl KingDirection {
    pub fn new(dir: ICoord) -> Self {
        assert!(dir.is_king_step());
        KingDirection { step: dir }
    }
    pub fn step(&self) -> ICoord {
        self.step
    }
    pub fn snap_angle(angle: FAngle) -> Self {
        let eighth_steps_from_plus_x = (angle.radians() * 8.0 / std::f32::consts::TAU).round();
        let rounded_radians_from_plus_x = FAngle::from_radians(eighth_steps_from_plus_x * std::f32::consts::TAU / 8.0);

        let float_step = FCoord::from_angle_and_length(
            rounded_radians_from_plus_x,
            1.5,
        );
        // 1.5 length to allow truncating down to 1 i32 in the diagonal case
        // because 1.5/sqrt(2) > 1.0

        // truncate towards zero intentionally
        Self::new(float_step.to_i32().unwrap())

    }
}

impl From<OrthogonalDirection> for KingDirection {
    fn from(value: OrthogonalDirection) -> Self {
        KingDirection::new(value.to_step())
    }
}

// TODO: generate with macro
impl QuarterTurnRotatable for KingDirection {
    fn quarter_rotated_ccw(&self, quarter_turns_ccw: OrthoAngle) -> Self {
        self.step().quarter_rotated_ccw(quarter_turns_ccw).into()
    }
}

impl From<ICoord> for KingDirection {
    fn from(value: ICoord) -> Self {
        KingDirection::new(value)
    }
}

