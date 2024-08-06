use crate::utility::*;

trait_alias!(pub trait PointReqs = float_coordinate::Operations);

pub trait Operations<P: PointReqs>: float_line::Operations<P> + line_segment::Operations<P> {
    fn length(&self) -> f32 {
        let [p1, p2] = self.endpoints_in_arbitrary_order();
        (p1 - p2).length()
    }
    fn seeded_random_point_on_line(&self, rng: &mut StdRng) -> P {
        let t = rng.gen_range(0.0..=1.0);
        let [p1, p2] = self.endpoints_in_arbitrary_order();
        p1.lerp2d(p2, t)
    }

    fn seeded_random_point_near_line(&self, rng: &mut StdRng, radius: f32) -> P {
        // TODO: make more uniform
        self.seeded_random_point_on_line(rng) + seeded_rand_radial_offset::<P>(rng, radius)
    }

    fn random_point_near_line(&self, radius: f32) -> P {
        self.seeded_random_point_near_line(&mut get_new_rng(), radius)
    }

    fn projected_onto_parallel_line_through_origin_with_positive_direction_hint(
        &self,
        positive_direction_hint: FAngle,
    ) -> ClosedInterval<f32> {
        let Some(positive_direction) = self
            .parallel_directions()
            .iter()
            .filter(|&&dir| fangle_dot(dir, positive_direction_hint) > 0.0)
            .next()
        else {
            panic!(
                "direction hint not useful.  hint: {:?}, parallel directions: {:?}",
                positive_direction_hint,
                self.parallel_directions()
            );
        };

        todo!();
    }
    fn projected_onto_parallel_line_through_origin(&self) -> ClosedInterval<f32> {
        let chosen_positive = self.parallel_directions()[0];
        self.projected_onto_parallel_line_through_origin_with_positive_direction_hint(
            chosen_positive,
        )
    }

    fn line_segment_intersection_point(&self, other: impl line_segment::Operations<P>) -> Option<P> {
        let [a1, a2] = self.endpoints_in_arbitrary_order();
        let [b1, b2] = other.endpoints_in_arbitrary_order();

        // TODO: This is cumbersome.  Should be more concise somehow
        let both_on_same_line = self.point_is_on_line(b1) && self.point_is_on_line(b2);

        if both_on_same_line {
            // Now dealing with one-dimensional intersection.
            // as we only care about a singular intersection point, too much intersection is None

            // TODO: redo this with intervals

            let [a1_1d, a2_1d, b1_1d, b2_1d] = [a1, a2, b1, b2]
                .map(|p| (p - a1).position_on_axis((a2 - a1).better_angle_from_x_axis()));

            let (b_min, b_max) = (
                min_for_partial_ord(b1_1d, b2_1d),
                max_for_partial_ord(b1_1d, b2_1d),
            );
            let (a_min, a_max) = (
                min_for_partial_ord(a1_1d, a2_1d),
                max_for_partial_ord(a1_1d, a2_1d),
            );

            let no_overlap = a_max < b_min || b_max < a_min;

            if no_overlap {
                return None;
            }

            let b1_in_a = b1_1d > a_min && b1_1d < a_max;
            let b2_in_a = b2_1d > a_min && b2_1d < a_max;
            let a1_in_b = a1_1d > b_min && a1_1d < b_max;
            let a2_in_b = a2_1d > b_min && a2_1d < b_max;
            let too_much_overlap = b1_in_a || b2_in_a || a1_in_b || a2_in_b;
            if too_much_overlap {
                return None;
            }

            // Only case left is exact matches.  note that both matching is bad.
            let identical_segments = a_min == b_min && a_max == b_max;
            if identical_segments {
                return None;
            }

            Some(if a1 == b1 || a1 == b2 {
                a1
            } else if a2 == b1 || a2 == b2 {
                a2
            } else {
                panic!(
                    "endpoints do not match: self: {:?}, other: {:?}",
                    &self, &other
                );
            })
        } else {
            let no_intersection = self.same_side_of_line(b1, b2) || other.same_side_of_line(a1, a2);
            if no_intersection {
                None
            } else {
                // intersection confirmed, now just find it.
                self.intersection_point_with_other_extended_line(other)
            }
        }
    }
}
impl<T, P: PointReqs> float_line_segment::Operations<P> for T where T: float_line::Operations<P> + line_segment::Operations<P> {}

pub trait directed_float_line_segment::Operations<P: PointReqs>:
    OperationsForDirectedLine<P> + float_line_segment::Operations<P>
{
    fn lerp(&self, t: f32) -> P {
        self.start().lerp2d(self.end(), t)
    }
}
impl<T, P: PointReqs> directed_float_line_segment::Operations<P> for T where
    T: OperationsForDirectedLine<P> + float_line_segment::Operations<P>
{
}
