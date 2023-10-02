use std::borrow::Borrow;
use std::collections::{HashMap, HashSet};
use std::f32::consts::{PI, TAU};
use std::fmt::{Debug, Display, Formatter};
use std::ops::{Add, Neg, Sub};

use euclid::{default, vec2, Angle};
use getset::CopyGetters;
use itertools::Itertools;
use ntest::assert_false;
use num::traits::FloatConst;
use ordered_float::OrderedFloat;
use termion::cursor::Left;

use crate::fov_stuff::OctantFOVSquareSequenceIter;
use crate::utility::coordinate_frame_conversions::{WorldMove, WorldStep};
use crate::utility::round_robin_iterator::round_robin;
use crate::utility::{
    abs_angle_distance, better_angle_from_x_axis, partial_angle_interval,
    rotated_n_quarter_turns_counter_clockwise, standardize_angle, Octant, OrthogonalWorldStep,
    QuarterTurnsAnticlockwise, RelativeSquareWithOrthogonalDir, SquareWithOrthogonalDir,
    ORTHOGONAL_STEPS, STEP_DOWN_LEFT, STEP_DOWN_RIGHT, STEP_UP_LEFT, STEP_UP_RIGHT, STEP_ZERO,
};

use super::bool_with_partial::BoolWithPartial;
use super::partial_angle_interval::PartialAngleInterval;
use super::{FAngle, RigidTransform, RigidlyTransformable};

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum AngleInterval {
    Empty,
    FullCircle,
    PartialArc(PartialAngleInterval),
}

impl Add for PartialAngleInterval {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        todo!("remove");
        //self.combine_with_overlapping_or_touching_arc(rhs)
    }
}

impl Sub for PartialAngleInterval {
    type Output = Vec<Self>;

    fn sub(self, rhs: Self) -> Self::Output {
        todo!("remove");
        self.subtract(rhs)
    }
}

impl Neg for PartialAngleInterval {
    type Output = Self;

    fn neg(self) -> Self::Output {
        todo!("remove");
        self.complement()
    }
}

impl Display for AngleInterval {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            AngleInterval::Empty => write!(f, "Empty"),
            AngleInterval::FullCircle => write!(f, "FullCircle"),
            AngleInterval::PartialArc(arc) => std::fmt::Display::fmt(&arc, f),
        }
    }
}

impl AngleInterval {
    // TODO: replace with a dedicated type like AngleIntervalInOneOctant, or something like that
    fn touched_rel_squares_going_outwards_in_one_octant_with_placeholders(
        &self,
    ) -> impl Iterator<Item = Option<WorldStep>> {
        use AngleInterval::*;
        match self {
            PartialArc(self_partial) => {
                self_partial.touched_rel_squares_going_outwards_in_one_octant_with_placeholders()
            }
            _ => panic!("not in one octant: {}", self),
        }
    }
    pub fn cw(&self) -> FAngle {
        use AngleInterval::*;
        match self {
            PartialArc(self_partial) => self_partial.cw(),
            _ => panic!("No endpoints to get"),
        }
    }
    pub fn ccw(&self) -> FAngle {
        use AngleInterval::*;
        match self {
            PartialArc(self_partial) => self_partial.ccw(),
            _ => panic!("No endpoints to get"),
        }
    }
    pub fn complement(&self) -> Self {
        match self {
            AngleInterval::Empty => Self::FullCircle,
            AngleInterval::FullCircle => Self::Empty,
            AngleInterval::PartialArc(partial) => Self::PartialArc(
                PartialAngleInterval::from_angles(partial.anticlockwise_end, partial.clockwise_end),
            ),
        }
    }
    pub fn combine_if_touching_panic_if_overlapping(
        &self,
        other: impl Into<Self>,
        tolerance: FAngle,
    ) -> Option<Self> {
        use BoolWithPartial::*;
        let other = other.into();

        match self.overlaps_arc(other, tolerance) {
            True => panic!("overlap"),
            Partial => self.try_combine(other, tolerance), // TODO: potential misalignment
            False => None,
        }
    }

    pub fn try_combine(&self, other: impl Into<Self>, tolerance: FAngle) -> Option<Self> {
        use AngleInterval::*;
        match self {
            Empty => Some(other.into()),
            FullCircle => Some(FullCircle),
            PartialArc(self_partial) => match other.into() {
                AngleInterval::Empty => Some(*self),
                AngleInterval::FullCircle => Some(AngleInterval::FullCircle),
                AngleInterval::PartialArc(other_partial) => {
                    Self::try_combine_partial_arcs(*self_partial, other_partial, tolerance)
                }
            },
        }
    }
    fn try_combine_partial_arcs(
        a: PartialAngleInterval,
        b: PartialAngleInterval,
        tolerance: FAngle,
    ) -> Option<Self> {
        if a.overlaps_partial_arc(b, tolerance).is_false() {
            return None;
        }
        if a.would_combine_with_other_partial_arc_to_full_circle(b, tolerance)
            .is_at_least_partial()
        {
            return Some(AngleInterval::FullCircle);
        }

        Some(
            PartialAngleInterval {
                anticlockwise_end: if a.contains_angle(b.ccw(), tolerance).is_at_least_partial() {
                    a.ccw()
                } else {
                    b.ccw()
                },
                clockwise_end: if a.contains_angle(b.cw(), tolerance).is_at_least_partial() {
                    a.cw()
                } else {
                    b.cw()
                },
            }
            .into(),
        )
    }
    pub fn from_octant(octant: Octant) -> Self {
        AngleInterval::PartialArc(PartialAngleInterval::from_octant(octant))
    }
    pub fn from_center_and_width(center_angle: FAngle, width: FAngle) -> Self {
        Self::PartialArc(PartialAngleInterval::from_center_and_width(
            center_angle,
            width,
        ))
    }
    fn split_into_octants_in_ccw_order(&self) -> Vec<AngleInterval> {
        use AngleInterval::*;
        match self {
            Empty => vec![],
            FullCircle => Octant::all_octants_going_ccw()
                .map(Self::from_octant)
                .collect(),
            PartialArc(self_partial_arc) => self_partial_arc
                .split_into_octants_in_ccw_order()
                .into_iter()
                .map(|partial| PartialArc(partial))
                .collect(),
        }
    }

    pub fn touched_squares_going_outwards_and_ccw(&self) -> impl Iterator<Item = WorldStep> {
        let iters: Vec<_> = self
            .split_into_octants_in_ccw_order()
            .into_iter()
            .map(|arc| arc.touched_rel_squares_going_outwards_in_one_octant_with_placeholders())
            .collect_vec();
        round_robin(iters)
            .filter_map(|maybe_step| maybe_step)
            .unique()
    }

    pub fn from_degrees(cw: f32, ccw: f32) -> Self {
        AngleInterval::PartialArc(PartialAngleInterval::from_degrees(cw, ccw))
    }
    pub fn from_relative_square(relative_square: impl Into<WorldStep>) -> Self {
        let relative_square = relative_square.into();
        if relative_square == STEP_ZERO {
            AngleInterval::FullCircle
        } else {
            AngleInterval::PartialArc(PartialAngleInterval::from_relative_square(relative_square))
        }
    }
    pub fn contains_arc(&self, other: impl Into<Self>, tolerance: FAngle) -> BoolWithPartial {
        use AngleInterval::*;
        use BoolWithPartial::*;
        let other = other.into();

        match self {
            Empty => False,
            FullCircle => match other {
                Empty => False,
                FullCircle => Partial, // TODO: doucle check this
                PartialArc(_) => True,
            },
            PartialArc(self_partial_arc) => match other {
                Empty => False,
                FullCircle => False,
                PartialArc(other_partial_arc) => {
                    self_partial_arc.contains_partial_arc(other_partial_arc, tolerance)
                }
            },
        }
    }
    pub fn overlaps_arc(&self, other: impl Into<Self>, tolerance: FAngle) -> BoolWithPartial {
        use AngleInterval::*;
        use BoolWithPartial::*;
        match self {
            Empty => False,
            FullCircle => match other.into() {
                Empty => False,
                _ => True,
            },
            PartialArc(self_partial) => match other.into() {
                Empty => False,
                FullCircle => True,
                PartialArc(other_partial) => {
                    self_partial.overlaps_partial_arc(other_partial, tolerance)
                }
            },
        }
    }
    pub fn contains_angle(&self, angle: Angle<f32>, tolerance: Angle<f32>) -> BoolWithPartial {
        use AngleInterval::*;
        use BoolWithPartial::*;
        match self {
            Empty => False,
            FullCircle => True,
            PartialArc(partial_arc) => partial_arc.contains_angle(angle, tolerance),
        }
    }

    #[deprecated(note = "use overlaps_arc instead")]
    pub fn overlapping_but_not_exactly_touching(
        &self,
        can_be_partial: impl TryInto<PartialAngleInterval>,
    ) -> bool {
        match self {
            AngleInterval::Empty => false,
            AngleInterval::FullCircle => true,
            AngleInterval::PartialArc(self_arc) => {
                if let Ok(other_partial) = can_be_partial.try_into() {
                    self_arc.overlapping_but_not_exactly_touching(other_partial)
                } else {
                    false
                }
            }
        }
    }
}

impl From<PartialAngleInterval> for AngleInterval {
    fn from(value: PartialAngleInterval) -> Self {
        AngleInterval::PartialArc(value)
    }
}

impl TryFrom<AngleInterval> for PartialAngleInterval {
    type Error = ();

    fn try_from(value: AngleInterval) -> Result<Self, Self::Error> {
        match value {
            AngleInterval::Empty => Err(()),
            AngleInterval::FullCircle => Err(()),
            AngleInterval::PartialArc(arc) => Ok(arc),
        }
    }
}

impl Debug for PartialAngleInterval {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "\n\
            \tradians: {:?}\n\
            \tdegrees: {:?}",
            self.to_radians(),
            self.to_degrees(),
        )
    }
}
impl RigidlyTransformable for AngleInterval {
    fn apply_rigid_transform(&self, tf: RigidTransform) -> Self {
        match self {
            AngleInterval::Empty | AngleInterval::FullCircle => *self,
            AngleInterval::PartialArc(partial_angle_interval) => {
                AngleInterval::PartialArc(partial_angle_interval.apply_rigid_transform(tf))
            }
        }
    }
}

impl RigidlyTransformable for PartialAngleInterval {
    fn apply_rigid_transform(&self, tf: RigidTransform) -> Self {
        self.rotated_quarter_turns(tf.rotation())
    }
}

#[cfg(test)]
mod tests {
    use euclid::point2;
    use itertools::iproduct;
    use ntest::{assert_about_eq, assert_false, assert_true, timeout};
    use num::zero;
    use pretty_assertions::{assert_eq, assert_ne};
    use strum::IntoEnumIterator;

    use crate::{
        fov_stuff::{rasterized_field_of_view::TopDownifiedFieldOfViewInterface, FieldOfView},
        utility::{
            coordinates::opposite_angle, relative_interval_location::RelativeIntervalLocation,
            STEP_DOWN, STEP_RIGHT, STEP_UP,
        },
    };

    use super::*;
    fn default_angle_tolerance_for_tests() -> FAngle {
        FAngle::degrees(0.001)
    }

    #[test]
    fn test_interval_overlap() {
        let interval_b = AngleInterval::from_degrees(5.0, 15.0);
        let interval_a = AngleInterval::from_degrees(0.0, 10.0);

        assert!(
            interval_a.overlapping_but_not_exactly_touching(interval_a),
            "self overlap"
        );
        assert!(
            interval_b.overlapping_but_not_exactly_touching(interval_b),
            "other self overlap"
        );
        assert!(
            interval_a.overlapping_but_not_exactly_touching(interval_b),
            "basic overlap"
        );
        assert!(
            interval_b.overlapping_but_not_exactly_touching(interval_a),
            "commutative"
        );
    }

    #[test]
    fn test_square_going_outwards_and_ccw__one_quadrant() {
        let arc = AngleInterval::from_degrees(0.0, 90.0);
        let iter = arc.touched_squares_going_outwards_and_ccw();
        let expected: Vec<WorldStep> = vec![
            (0, 0),
            (1, 0),
            (0, 1),
            (1, 1),
            (2, 0),
            (0, 2),
            (2, 1),
            (1, 2),
            (2, 2),
        ]
        .into_iter()
        .map(|t| t.into())
        .collect_vec();
        assert_eq!(iter.take(expected.len()).collect_vec(), expected);
    }
    #[test]
    fn test_overlapping_but_not_exactly_touching() {
        assert!(AngleInterval::from_degrees(0.0, 20.0)
            .overlapping_but_not_exactly_touching(AngleInterval::from_degrees(-10.0, 10.0)));
        assert!(!AngleInterval::from_degrees(0.0, 20.0)
            .overlapping_but_not_exactly_touching(AngleInterval::from_degrees(-10.0, -1.0)));

        assert!(AngleInterval::from_degrees(0.0, 20.0)
            .overlapping_but_not_exactly_touching(PartialAngleInterval::from_degrees(-10.0, 10.0)));

        assert!(AngleInterval::FullCircle
            .overlapping_but_not_exactly_touching(AngleInterval::from_degrees(0.0, 30.0)));
        assert!(AngleInterval::FullCircle
            .overlapping_but_not_exactly_touching(AngleInterval::FullCircle));
        // assert!(AngleInterval::from_degrees(20.0, 30.0)
        //     .overlapping_but_not_exactly_touching(AngleInterval::FullCircle));
        // assert!(!AngleInterval::from_degrees(20.0, 30.0)
        //     .overlapping_but_not_exactly_touching(AngleInterval::Empty));
    }

    #[test]
    fn test_combine_two_partial_to_full_circle() {
        let a = PartialAngleInterval::from_degrees(0.0, 20.0);
        let b = PartialAngleInterval::from_degrees(20.0, 0.0);
        let c = PartialAngleInterval::from_degrees(30.0, 40.0);
        let t = FAngle::degrees(0.01);
        let combine = |p1, p2| AngleInterval::try_combine_partial_arcs(p1, p2, t);
        let would_combine = |p1: PartialAngleInterval, p2| {
            p1.would_combine_with_other_partial_arc_to_full_circle(p2, t)
        };
        assert_eq!(combine(a, b), Some(AngleInterval::FullCircle));
        assert_eq!(would_combine(a, b), BoolWithPartial::Partial);
        assert_eq!(combine(b, a), Some(AngleInterval::FullCircle));
        assert_eq!(would_combine(b, a), BoolWithPartial::Partial);
        assert_eq!(combine(a, c), None);
        assert_eq!(would_combine(a, c), BoolWithPartial::False);
        assert_eq!(combine(b, c), Some(b.into()));
        assert_eq!(would_combine(b, c), BoolWithPartial::False);
    }
    #[test]
    fn test_combine_empty_and_partial_is_partial() {
        let a = PartialAngleInterval::from_degrees(0.0, 20.0);
        let wrapped_a = AngleInterval::PartialArc(a.clone());
        let b = AngleInterval::Empty;
        let t = FAngle::degrees(0.1);

        assert_eq!(wrapped_a.try_combine(b, t), Some(wrapped_a));
        assert_eq!(b.try_combine(wrapped_a, t), Some(wrapped_a));
    }
    #[test]
    fn test_squares_going_outwards_and_ccw__full_circle() {
        let arc = AngleInterval::FullCircle;
        let iter = arc.touched_squares_going_outwards_and_ccw();
        let expected: Vec<WorldStep> = vec![
            (0, 0),
            (1, 0),
            (0, 1),
            (-1, 0),
            (0, -1),
            (1, 1),
            (-1, 1),
            (-1, -1),
            (1, -1),
            (2, 0),
            (0, 2),
            (-2, 0),
            (0, -2),
            (2, 1),
            (1, 2),
            (-1, 2),
            (-2, 1),
            (-2, -1),
            (-1, -2),
            (1, -2),
            (2, -1),
        ]
        .into_iter()
        .map(|t| t.into())
        .collect_vec();
        assert_eq!(iter.take(expected.len()).collect_vec(), expected);
    }
    #[test]
    fn test_angle_interval_union() {
        assert_eq!(
            AngleInterval::from_degrees(80.0, 100.0)
                .try_combine(
                    AngleInterval::from_degrees(40.0, 90.0),
                    default_angle_tolerance_for_tests()
                )
                .unwrap(),
            AngleInterval::from_degrees(40.0, 100.0),
            "from overlap"
        );

        assert_eq!(
            AngleInterval::from_degrees(80.0, 100.0)
                .try_combine(
                    AngleInterval::from_degrees(40.0, 80.0),
                    default_angle_tolerance_for_tests()
                )
                .unwrap(),
            AngleInterval::from_degrees(40.0, 100.0),
            "from exactly touching"
        );
    }
    #[test]
    fn test_square_going_outwards_and_ccw__very_small_angle() {
        let arc = AngleInterval::from_degrees(0.0, 0.01);
        let iter = arc.touched_squares_going_outwards_and_ccw();
        let expected: Vec<WorldStep> = vec![
            (0, 0),
            (1, 0),
            (2, 0),
            (3, 0),
            (4, 0),
            (5, 0),
            (6, 0),
            (7, 0),
            (8, 0),
        ]
        .into_iter()
        .map(|t| t.into())
        .collect_vec();
        assert_eq!(iter.take(expected.len()).collect_vec(), expected);
    }
    #[test]
    fn test_combine_touching_panic_overlapping__non_panic_case() {
        let tolerance = Angle::degrees(0.1);
        let a = AngleInterval::from_degrees(0.0, 10.0);
        let b = AngleInterval::from_degrees(10.0, 20.0);
        let b_bigger = AngleInterval::from_degrees(10.0 - tolerance.to_degrees() / 2.0, 20.0);

        let b_smaller = AngleInterval::from_degrees(10.0 + tolerance.to_degrees() / 2.0, 20.0);
        let b_way_smaller = AngleInterval::from_degrees(10.0 + tolerance.to_degrees() * 2.0, 20.0);

        let correct = AngleInterval::from_degrees(0.0, 20.0);
        assert_eq!(
            a.combine_if_touching_panic_if_overlapping(b, tolerance),
            Some(correct)
        );
        assert_eq!(
            a.combine_if_touching_panic_if_overlapping(b_bigger, tolerance),
            Some(correct)
        );
        assert_eq!(
            a.combine_if_touching_panic_if_overlapping(b_smaller, tolerance),
            Some(correct)
        );
        assert_eq!(
            a.combine_if_touching_panic_if_overlapping(b_way_smaller, tolerance),
            None
        );
    }
    #[test]
    #[should_panic]
    fn test_combine_touching_panic_overlapping__panic_case() {
        let tolerance = Angle::degrees(0.1);
        let a = AngleInterval::from_degrees(0.0, 10.0);

        let b_way_bigger = AngleInterval::from_degrees(10.0 - tolerance.to_degrees() * 2.0, 20.0);

        a.combine_if_touching_panic_if_overlapping(b_way_bigger, tolerance);
    }
}
