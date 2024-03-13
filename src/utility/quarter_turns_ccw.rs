use std::ops::{Add, Div, Mul, Neg, Sub};

use euclid::Angle;

use crate::{
    standardize_angle, IntCoordinate, QuarterTurnRotatable, SignedCoordinate, WorldStep, STEP_RIGHT,
};

#[derive(
    Hash, Default, Debug, Copy, Clone, Eq, PartialEq, getset::CopyGetters, derive_more::AddAssign,
)]
#[get_copy = "pub"]
pub struct QuarterTurnsCcw {
    pub(crate) quarter_turns: i32,
}

impl QuarterTurnsCcw {
    pub fn new(quarter_turns: i32) -> Self {
        QuarterTurnsCcw {
            quarter_turns: quarter_turns.rem_euclid(4),
        }
    }
    #[deprecated(note = "use OrthogonalUnitCoordinate::From instead")]
    pub fn to_orthogonal_direction(&self) -> WorldStep {
        STEP_RIGHT.quarter_rotated_ccw(self.quarter_turns)
    }
    pub fn all_4() -> impl Iterator<Item = Self> + Clone {
        (0..4).map(|x| x.into())
    }
    pub fn from_vector(dir: WorldStep) -> Self {
        assert!(dir.is_orthogonal());
        QuarterTurnsCcw::new(if dir.x == 0 {
            if dir.y > 0 {
                1
            } else {
                3
            }
        } else {
            if dir.x > 0 {
                0
            } else {
                2
            }
        })
    }

    pub fn from_start_and_end_directions<P: IntCoordinate>(start: P, end: P) -> Self {
        assert!(start.is_king_step());
        assert!(end.is_king_step());
        // needs to be quarter turn, no eighths
        assert_eq!(start.is_diagonal(), end.is_diagonal());

        let d_angle = start.to_f32().angle_to(end.to_f32());
        let quarter_turns = (d_angle.to_degrees() / 90.0).round() as i32;
        Self::new(quarter_turns)
    }

    pub fn rotate_angle(&self, angle: Angle<f32>) -> Angle<f32> {
        standardize_angle(Angle::<f32>::degrees(
            angle.to_degrees() + 90.0 * (self.quarter_turns() as f32),
        ))
    }
    pub fn rotate_vector<PointType: SignedCoordinate>(&self, v: PointType) -> PointType {
        v.quarter_rotated_ccw(self.quarter_turns)
    }
}

impl Neg for QuarterTurnsCcw {
    type Output = Self;

    fn neg(self) -> Self::Output {
        QuarterTurnsCcw::new(-self.quarter_turns)
    }
}

impl Add for QuarterTurnsCcw {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        Self::new(self.quarter_turns() + rhs.quarter_turns())
    }
}

impl Sub for QuarterTurnsCcw {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        Self::new(self.quarter_turns() - rhs.quarter_turns())
    }
}

impl From<i32> for QuarterTurnsCcw {
    fn from(value: i32) -> Self {
        Self::new(value)
    }
}

impl QuarterTurnRotatable for QuarterTurnsCcw {
    fn quarter_rotated_ccw(&self, quarter_turns_ccw: impl Into<QuarterTurnsCcw> + Copy) -> Self {
        *self + quarter_turns_ccw.into()
    }
}
