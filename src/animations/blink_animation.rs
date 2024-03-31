use std::f32::consts::PI;
use std::time::{Duration, Instant};

use num::ToPrimitive;
use rand::SeedableRng;

use crate::animations::Animation;
use crate::glyph::glyph_constants::BLINK_EFFECT_COLOR;
use crate::glyph::hextant_blocks::{points_to_hextant_chars, snap_to_hextant_grid};
use crate::glyph::Glyph;
use crate::line_segment::FloatLineSegment;
use crate::utility::*;

#[derive(Clone, PartialEq, Debug, Copy)]
pub struct BlinkAnimation {
    start_square: WorldSquare,
    end_square: WorldSquare,
    start_time: Instant,
}

impl BlinkAnimation {
    pub fn new(start_square: WorldSquare, end_square: WorldSquare) -> BlinkAnimation {
        BlinkAnimation {
            start_square,
            end_square,
            start_time: Instant::now(),
        }
    }
}

impl Animation for BlinkAnimation {
    fn start_time(&self) -> Instant {
        self.start_time
    }

    fn duration(&self) -> Duration {
        Duration::from_secs_f32(1.0)
    }

    fn glyphs_at_time(&self, time: Instant) -> WorldCharacterSquareGlyphMap {
        // pretty arbitrary
        let hash = ((self.start_square.x() as f32 * PI + self.start_square.y() as f32) * 1000.0
            + self.end_square.x() as f32 * 4.23746287
            + self.end_square.y() as f32 * 87.4736)
            .abs()
            .floor()
            .to_u64()
            .unwrap();
        let mut rng = rand::rngs::StdRng::seed_from_u64(hash);

        // the tunable constants
        // let ostensible_blink_duration = 0.2;
        let settling_time = 0.8;

        let time_constant = settling_time / 5.0;

        let points_per_square_blinked = 1.0;
        let point_spread_radius = 0.5;

        // let motion_vector: WorldMove = self.end_square.to_f32() - self.start_square.to_f32();
        // let motion_direction = motion_vector.normalize();
        // let motion_distance = motion_vector.length();

        // let start_speed = motion_distance / ostensible_blink_duration;

        // let start_vel = motion_direction * start_speed;

        let end_point = self.end_square.to_f32();
        let start_point = self.start_square.to_f32();
        let end_point_mirrored_over_start_point = start_point - (end_point - start_point);
        let float_line_centered_on_start =
            TwoDifferentPoints::new(end_point_mirrored_over_start_point, end_point);

        let age = time.duration_since(self.start_time);
        // let total_seconds = self.duration().as_secs_f32();
        // let remaining_seconds = total_seconds - age.as_secs_f32();
        let spent_seconds = age.as_secs_f32();
        // let lifetime_fraction_remaining = remaining_seconds / total_seconds;
        // let lifetime_fraction_spent = spent_seconds / total_seconds;

        //let vel = start_vel * (-lifetime_fraction_spent * time_constant).exp();

        let blink_vector = self.end_square.to_f32() - self.start_square.to_f32();
        let displacement = blink_vector * (1.0 - (-spent_seconds / time_constant).exp());

        let distance_blinked = (start_point - end_point).length();
        let num_points = (points_per_square_blinked * distance_blinked) as u32;
        let base_points: Vec<WorldPoint> = (0..num_points * 2)
            .into_iter()
            .map(|_i| {
                float_line_centered_on_start
                    .seeded_random_point_near_line(&mut rng, point_spread_radius)
            })
            .map(snap_to_hextant_grid)
            .collect();

        let moved_points: Vec<WorldPoint> =
            base_points.into_iter().map(|p| p + displacement).collect();

        let blink_line = TwoDifferentPoints::new(start_point, end_point);
        let visible_points: Vec<WorldPoint> = moved_points
            .into_iter()
            .filter(|&point| blink_line.point_is_on_or_normal_to_line_segment(point))
            .collect();

        points_to_hextant_chars(visible_points)
            .into_iter()
            .map(|(square, c)| (square, Glyph::fg_only(c, BLINK_EFFECT_COLOR)))
            .collect()

        //line_drawing::Bresenham::new(self.start_square.to_tuple(), self.end_square.to_tuple())
        //.map(|(x, y)| point2(x, y))
        //.flat_map(world_square_to_both_world_character_squares)
        //.map(|char_square| (char_square, Glyph::fg_only(FULL_BLOCK, BLINK_EFFECT_COLOR)))
        //.collect()
    }
}
