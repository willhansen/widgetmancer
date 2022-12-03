use std::ops::{Add, Sub};

use euclid::Angle;

#[derive(Debug, Copy, Clone, PartialEq)]
pub struct LeftClosedClockwiseAngleInterval {
    pub start_angle: Angle<f32>,
    pub end_angle: Angle<f32>,
}

impl LeftClosedClockwiseAngleInterval {
    fn new(start_angle: Angle<f32>, end_angle: Angle<f32>) -> Self {
        LeftClosedClockwiseAngleInterval {
            start_angle,
            end_angle,
        }
    }
    fn from_degrees(start_angle_deg: f32, end_angle_deg: f32) -> Self {
        Self::new(
            Angle::degrees(start_angle_deg),
            Angle::degrees(end_angle_deg),
        )
    }
    fn overlaps(&self, other: LeftClosedClockwiseAngleInterval) -> bool {
        self.contains_angle(other.start_angle)
            || (self.contains_angle(other.end_angle) && self.start_angle != other.end_angle)
            || other.contains_angle(self.start_angle)
            || (other.contains_angle(self.end_angle) && other.start_angle != self.end_angle)
    }
    fn contains_angle(&self, angle: Angle<f32>) -> bool {
        // special case for zero length interval
        // TODO: full circle arc instead?
        if self.start_angle == self.end_angle {
            return angle == self.start_angle;
        }

        let interval_is_less_than_half_circle =
            self.start_angle.angle_to(self.end_angle).radians < 0.0;
        //dbg!( "---------", self.start_angle, self.end_angle, angle, self.start_angle.angle_to(angle), self.end_angle.angle_to(angle) );
        if interval_is_less_than_half_circle {
            self.start_angle.angle_to(angle).radians <= 0.0
                && self.end_angle.angle_to(angle).radians > 0.0
        } else {
            self.start_angle.angle_to(angle).radians <= 0.0
                || self.end_angle.angle_to(angle).radians > 0.0
        }
    }
}

pub struct AngleIntervalSet {
    intervals: Vec<LeftClosedClockwiseAngleInterval>,
}

impl AngleIntervalSet {
    pub fn new() -> Self {
        AngleIntervalSet { intervals: vec![] }
    }

    pub fn add_interval(&mut self, interval: LeftClosedClockwiseAngleInterval) {
        todo!()
    }
    pub fn fully_contains_interval(&self, interval: LeftClosedClockwiseAngleInterval) -> bool {
        todo!()
    }
    pub fn overlaps_interval(&self, interval: LeftClosedClockwiseAngleInterval) -> bool {
        todo!()
    }
}

#[cfg(test)]
mod tests {
    use ntest::assert_false;
    use pretty_assertions::{assert_eq, assert_ne};

    use super::*;

    #[test]
    fn test_interval_overlap() {
        let interval_b = LeftClosedClockwiseAngleInterval::from_degrees(15.0, 5.0);
        let interval_a = LeftClosedClockwiseAngleInterval::from_degrees(10.0, 0.0);

        assert!(interval_a.overlaps(interval_a), "self overlap");
        assert!(interval_b.overlaps(interval_b), "other self overlap");
        assert!(interval_a.overlaps(interval_b), "basic overlap");
        assert!(interval_b.overlaps(interval_a), "commutative");
    }

    #[test]
    fn test_interval_overlap_edges() {
        let interval_a = LeftClosedClockwiseAngleInterval::from_degrees(10.0, 0.0);
        let interval_c = LeftClosedClockwiseAngleInterval::from_degrees(50.0, 10.0);
        assert!(
            !interval_a.overlaps(interval_c),
            "touching edges should not count as overlap"
        );
    }

    #[test]
    fn test_interval_contains_angle() {
        assert!(
            LeftClosedClockwiseAngleInterval::from_degrees(10.0, 0.0)
                .contains_angle(Angle::degrees(5.0)),
            "simple case"
        );
        assert!(
            !LeftClosedClockwiseAngleInterval::from_degrees(10.0, 0.0)
                .contains_angle(Angle::degrees(15.0)),
            "simple outside bounds case"
        );
        assert!(
            LeftClosedClockwiseAngleInterval::from_degrees(10.0, 0.0)
                .contains_angle(Angle::degrees(10.0)),
            "On left bound should be inside"
        );
        assert!(
            !LeftClosedClockwiseAngleInterval::from_degrees(10.0, 0.0)
                .contains_angle(Angle::degrees(0.0)),
            "On right bound should NOT be inside"
        );
        assert!(
            !LeftClosedClockwiseAngleInterval::from_degrees(0.0, 10.0)
                .contains_angle(Angle::degrees(5.0)),
            "outside a large arc"
        );
        assert!(
            LeftClosedClockwiseAngleInterval::from_degrees(0.0, 10.0)
                .contains_angle(Angle::degrees(180.0)),
            "inside a large arc"
        );
        assert!(
            !LeftClosedClockwiseAngleInterval::from_degrees(0.0, 0.0)
                .contains_angle(Angle::degrees(180.0)),
            "directly across from zero width interval"
        );
    }
}
