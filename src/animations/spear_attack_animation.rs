use std::f32::consts::{PI, TAU};
use std::time::{Duration, Instant};

use euclid::{point2, vec2, Angle};
use num::ToPrimitive;
use rand::{Rng, SeedableRng};

use crate::animations::Animation;
use crate::glyph::glyph_constants::SPEAR_COLOR;
use crate::glyph::Glyph;
use crate::utility::coordinate_frame_conversions::{
    MoveList, PointList, WorldCharacterSquareGlyphMap, WorldMove, WorldPoint, WorldSquare,
    WorldStep,
};
use crate::utility::{
    better_angle_from_x_axis, is_king_step, is_orthodiagonal, rotate_vect, KingWorldStep,
    KING_STEPS,
};

#[derive(Clone, PartialEq, Debug, Copy)]
pub struct SpearAttackAnimation {
    start_square: WorldSquare,
    direction: KingWorldStep,
    range: u32,
    start_time: Instant,
}

impl SpearAttackAnimation {
    pub fn new(
        start_square: WorldSquare,
        direction: KingWorldStep,
        range: u32,
    ) -> SpearAttackAnimation {
        SpearAttackAnimation {
            start_square,
            direction,
            range,
            start_time: Instant::now(),
        }
    }

    fn points_in_an_arrow() -> MoveList {
        //  >
        let x_length = 1.0;
        let slope = 1.0;
        let spacing = 0.2;

        let mut points = vec![];
        let x_layers = (x_length / spacing).to_f32().unwrap().ceil() as i32;
        (0..x_layers).for_each(|ix| {
            let x = ix as f32 * spacing;
            let column_half_height = x * slope;
            let points_in_half_column =
                (column_half_height / spacing).to_f32().unwrap().ceil() as i32;
            (-points_in_half_column..=points_in_half_column)
                .for_each(|iy| points.push(vec2(-ix as f32 * spacing, iy as f32 * spacing)));
        });
        points
    }
}

impl Animation for SpearAttackAnimation {
    fn start_time(&self) -> Instant {
        self.start_time
    }
    fn duration(&self) -> Duration {
        Duration::from_millis(500)
    }

    fn glyphs_at_time(&self, time: Instant) -> WorldCharacterSquareGlyphMap {
        let mut points_to_draw: Vec<WorldPoint> = vec![];
        let num_particles = 50;
        let sweep_degrees = 10.0;
        let angle = better_angle_from_x_axis(self.direction.step().to_f32());
        let spear_length = self.range as f32 * self.fraction_remaining_at_time(time);
        for i in 0..num_particles {
            let relative_position = WorldMove::from_angle_and_length(
                angle,
                spear_length * i as f32 / num_particles as f32,
            );
            let particle_pos = self.start_square.to_f32() + relative_position;
            points_to_draw.push(particle_pos);
        }
        let rel_spear_tip = WorldMove::from_angle_and_length(angle, spear_length);
        let mut spearhead_points: PointList = SpearAttackAnimation::points_in_an_arrow()
            .into_iter()
            .map(|p: WorldMove| rotate_vect(p, better_angle_from_x_axis(rel_spear_tip)))
            .map(|p| self.start_square.to_f32() + p + rel_spear_tip)
            .collect();
        points_to_draw.append(&mut spearhead_points);
        Glyph::points_to_braille_glyphs(points_to_draw, SPEAR_COLOR)
    }
}

#[cfg(test)]
mod tests {
    use euclid::point2;

    use crate::utility::STEP_RIGHT;

    use super::*;

    #[test]
    fn test_spear_attack_does_any_drawing() {
        let square = point2(3, 3);
        let dir = STEP_RIGHT;
        let animation = SpearAttackAnimation::new(square, dir.into(), 3);
        let double_glyphs = animation.double_glyphs_at_duration(Duration::from_secs_f32(0.1));
        assert!(double_glyphs.contains_key(&(square + STEP_RIGHT)));
    }
}
