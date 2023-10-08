use std::f32::consts::PI;

use euclid::Angle;

use super::FAngle;
use super::OrthogonalWorldStep;
use super::{QuarterTurnsCcw, STEP_DOWN, STEP_LEFT, STEP_RIGHT, STEP_UP};

#[derive(Clone, PartialEq, Debug, Copy)]
pub struct Octant(i32);

impl Octant {
    pub fn new(octant: i32) -> Self {
        Octant(octant.rem_euclid(8))
    }
    pub fn with_n_quarter_turns_anticlockwise(&self, quarter_turns: QuarterTurnsCcw) -> Self {
        Self::new(self.0 + quarter_turns.quarter_turns() * 2)
    }
    pub fn outward_and_across_directions(&self) -> (OrthogonalWorldStep, OrthogonalWorldStep) {
        // TODO: probably make this an actual equation
        let world_step = match self.0 {
            0 => (STEP_RIGHT, STEP_UP),
            1 => (STEP_UP, STEP_RIGHT),
            2 => (STEP_UP, STEP_LEFT),
            3 => (STEP_LEFT, STEP_UP),
            4 => (STEP_LEFT, STEP_DOWN),
            5 => (STEP_DOWN, STEP_LEFT),
            6 => (STEP_DOWN, STEP_RIGHT),
            7 => (STEP_RIGHT, STEP_DOWN),
            _ => panic!("bad octant: {}", self.0),
        };
        (world_step.0.into(), world_step.1.into())
    }
    pub fn number(&self) -> i32 {
        self.0
    }

    pub fn from_outward_and_across_directions(
        outward_direction: OrthogonalWorldStep,
        across_direction: OrthogonalWorldStep,
    ) -> Self {
        // TODO: probably make this an actual equation
        let step_pair = (outward_direction.step(), across_direction.step());
        let octant_number = if step_pair == (STEP_RIGHT, STEP_UP) {
            0
        } else if step_pair == (STEP_UP, STEP_RIGHT) {
            1
        } else if step_pair == (STEP_UP, STEP_LEFT) {
            2
        } else if step_pair == (STEP_LEFT, STEP_UP) {
            3
        } else if step_pair == (STEP_LEFT, STEP_DOWN) {
            4
        } else if step_pair == (STEP_DOWN, STEP_LEFT) {
            5
        } else if step_pair == (STEP_DOWN, STEP_RIGHT) {
            6
        } else if step_pair == (STEP_RIGHT, STEP_DOWN) {
            7
        } else {
            panic!(
                "bad octant: outward: {:?}, across: {:?}",
                outward_direction, across_direction
            )
        };
        Self::new(octant_number)
    }
    pub fn all_octants_going_ccw() -> impl Iterator<Item = Self> {
        (0..8).map(|i| Octant(i))
    }

    fn from_angle_raw(angle: FAngle) -> Self {
        let normalized = angle.to_degrees() / 45.0;
        // octant 0 is above angle 0, so we want the floor rather than truncation

        let octant = Octant::new(normalized.floor() as i32);
        octant
    }
    pub fn near_octant_boundary(angle: FAngle) -> bool {
        let octant_width = Angle::radians((2.0 * PI) / 8.0);
        let standardized_angle = Angle::radians(
            (angle + octant_width / 2.0)
                .radians
                .rem_euclid(octant_width.radians)
                - octant_width.radians / 2.0,
        );
        standardized_angle.radians.abs() < Self::default_angle_tolerance().radians
    }
    fn default_angle_tolerance() -> FAngle {
        // TODO: may be too small for f32
        const DEFAULT_TOLERANCE_RADIANS: f32 = 1e-6;
        Angle::radians(DEFAULT_TOLERANCE_RADIANS)
    }

    pub fn from_angle_with_tie_break_toward_cw(angle: FAngle) -> Self {
        Self::from_angle_with_tie_break(angle, false)
    }
    pub fn from_angle_with_tie_break_toward_ccw(angle: FAngle) -> Self {
        Self::from_angle_with_tie_break(angle, true)
    }
    fn from_angle_with_tie_break(angle: FAngle, break_tie_ccw: bool) -> Self {
        let on_octant_boundary = Self::near_octant_boundary(angle);
        let tie_break_sign = if break_tie_ccw { 1.0 } else { -1.0 };
        let unambiguous_angle = if on_octant_boundary {
            angle + Angle::degrees(20.0) * tie_break_sign
        } else {
            angle
        };
        Octant::from_angle_raw(unambiguous_angle)
    }

    pub fn next_cw(&self) -> Self {
        Self::new(self.number() - 1)
    }
    pub fn next_ccw(&self) -> Self {
        Self::new(self.number() + 1)
    }
}

#[cfg(test)]
mod tests {

    use itertools::Itertools;

    use super::*;

    #[test]
    fn test_angle_to_octant() {
        // in format of degrees, octant biased cw, octant biased ccw
        let deg_octcw_octccw: Vec<(f32, [i32; 2])> = (0..9)
            .flat_map(|i| {
                vec![
                    (45.0 * i as f32, [-1 + i, 0 + i]),
                    (45.0 * i as f32 - 5.0, [-1 + i, -1 + i]),
                    (45.0 * i as f32 + 5.0, [0 + i, 0 + i]),
                ]
            })
            .collect_vec();

        let fs: [(fn(Angle<f32>) -> Octant, &str); 2] = [
            (Octant::from_angle_with_tie_break_toward_cw, "cw"),
            (Octant::from_angle_with_tie_break_toward_ccw, "ccw"),
        ];

        deg_octcw_octccw
            .iter()
            .for_each(|(degrees, biased_octants)| {
                fs.iter().enumerate().for_each(|(fi, (f, bias))| {
                    assert_eq!(
                        f(Angle::degrees(*degrees)),
                        Octant::new(biased_octants[fi]),
                        "angle: {}, correct_octant: {}, bias_for_this_function: {}",
                        degrees,
                        biased_octants[fi],
                        bias
                    )
                })
            });
    }
    #[test]
    fn test_from_angle() {
        let test_angle = Angle::degrees(45.0);
        let da = Octant::default_angle_tolerance() / 2.0;
        let slightly_cw = test_angle - da;
        let very_cw = test_angle - da * 3.0;
        let slightly_ccw = test_angle + da;
        let very_ccw = test_angle + da * 3.0;

        let octant_cw = Octant::new(0);
        let octant_ccw = octant_cw.next_ccw();

        let f_cw = Octant::from_angle_with_tie_break_toward_cw;
        let f_ccw = Octant::from_angle_with_tie_break_toward_ccw;
        let f_raw = Octant::from_angle_raw;

        assert_eq!(f_cw(very_ccw), octant_ccw);
        assert_eq!(f_cw(slightly_ccw), octant_cw);
        assert_eq!(f_cw(slightly_cw), octant_cw);
        assert_eq!(f_cw(very_cw), octant_cw);

        assert_eq!(f_ccw(very_ccw), octant_ccw);
        assert_eq!(f_ccw(slightly_ccw), octant_ccw);
        assert_eq!(f_ccw(slightly_cw), octant_ccw);
        assert_eq!(f_ccw(very_cw), octant_cw);

        assert_eq!(f_raw(very_ccw), octant_ccw);
        assert_eq!(f_raw(slightly_ccw), octant_ccw);
        assert_eq!(f_raw(slightly_cw), octant_cw);
        assert_eq!(f_raw(very_cw), octant_cw);
    }
    #[test]
    fn test_from_angle_raw__negative_angle() {
        assert_eq!(
            Octant::from_angle_raw(FAngle::radians(-0.3)),
            Octant::new(-1)
        );
    }
    #[test]
    fn test_near_octant_boundary() {
        let f = Octant::near_octant_boundary;
        let da = Octant::default_angle_tolerance() / 2.0;
        let small_ccw = da;
        let big_ccw = da * 3.0;
        let small_cw = -da;
        let big_cw = da * -3.0;
        let a = Angle::degrees(45.0);
        for i in 0..10 {
            assert_eq!(f(a * i as f32 + small_ccw), true);
            assert_eq!(f(a * i as f32 + small_cw), true);
            assert_eq!(f(a * i as f32 + big_ccw), false);
            assert_eq!(f(a * i as f32 + big_cw), false);
        }
    }
}
