use ambassador::{delegatable_trait, Delegate};
use std::collections::HashMap;
use std::f32::consts::{E, PI, TAU};
use std::time;
use std::time::{Duration, Instant};

use dyn_clone::DynClone;
use euclid::{point2, vec2, Angle, Length};
use num::{clamp, ToPrimitive};
use rand::{Rng, SeedableRng};
use termion::color::Black;
use termion::style::Blink;

use crate::{is_orthogonal_king_step, lerp, round_to_king_step, Glyph, Graphics, RIGHT_I, UP_I};
use terminal_rendering::glyph_constants::*;
use utility::*;
use terminal_rendering::*;

// use utility::pub_mod_and_use
pub_mod_and_use!(
    blink_animation,
    burst_explosion_animation,
    circle_attack_animation,
    floaty_laser,
    piece_death_animation,
    radial_shockwave,
    recoiling_board,
    selector_animation,
    simple_laser,
    smite_from_above,
    spear_attack_animation,
    static_board
);

pub type AnimationList = Vec<AnimationEnum>;

#[delegatable_trait]
pub trait Animation: Clone {
    fn start_time(&self) -> Instant;
    fn duration(&self) -> Duration;
    fn glyphs_at_time(&self, time: Instant) -> WorldCharacterSquareGlyphMap;

    fn double_glyphs_at_time(&self, time: Instant) -> WorldSquareGlyphMap {
        pair_up_character_square_map(self.glyphs_at_time(time), Glyph::transparent_glyph())
    }

    fn glyphs_at_duration(&self, duration: Duration) -> WorldCharacterSquareGlyphMap {
        self.glyphs_at_time(self.start_time() + duration)
    }

    fn double_glyphs_at_duration(&self, duration: Duration) -> WorldSquareGlyphMap {
        pair_up_character_square_map(
            self.glyphs_at_duration(duration),
            Glyph::transparent_glyph(),
        )
    }

    fn finished_at_time(&self, time: Instant) -> bool {
        self.fraction_done_at_time(time) == 1.0
    }

    fn fraction_done_at_time(&self, time: Instant) -> f32 {
        clamp(
            time.duration_since(self.start_time()).as_secs_f32() / self.duration().as_secs_f32(),
            0.0,
            1.0,
        )
    }
    fn fraction_remaining_at_time(&self, time: Instant) -> f32 {
        1.0 - self.fraction_done_at_time(time)
    }

    fn age_at_time(&self, time: Instant) -> Duration {
        time.duration_since(self.start_time())
    }
}

#[derive(Delegate, Clone)]
#[delegate(Animation)]
pub enum AnimationEnum {
    Blink(BlinkAnimation),
    BurstExplosion(BurstExplosionAnimation),
    CircleAttack(CircleAttackAnimation),
    FloatyLaser(FloatyLaserAnimation),
    PieceDeath(PieceDeathAnimation),
    RadialShockwave(RadialShockwave),
    RecoilingBoard(RecoilingBoardAnimation),
    Selector(SelectorAnimation),
    SimpleLaser(SimpleLaserAnimation),
    Smite(SmiteAnimation),
    SpearAttack(SpearAttackAnimation),
}

pub const DOTS_IN_SELECTOR: u32 = 3;

#[cfg(test)]
mod tests {
    use pretty_assertions::{assert_eq, assert_ne};


    use crate::graphics::FloorColorEnum;
    use crate::{derivative, glyph_map_to_string, DOWN_I, LEFT_I};

    use super::*;

    #[test]
    fn test_recoil_distance_function_increasing_for_first_half() {
        let peak_time = RecoilingBoardAnimation::TIME_TO_PEAK.as_secs_f32();
        let mut prev_d = 0.0;
        let mut t = 0.0;
        loop {
            let d = RecoilingBoardAnimation::recoil_distance_in_squares_at_age(t).abs();
            if t >= peak_time {
                break;
            }
            if t != 0.0 {
                assert!(
                    d > prev_d,
                    "t_peak: {peak_time}\nt: {t}\nd: {d}\nprev_d: {prev_d}"
                );
            }
            prev_d = d;
            t += 0.125;
        }
    }

    #[test]
    fn test_recoil_animation_has_smooth_animation_at_start_of_recoil_left() {
        let board_length = 5;
        let animation = RecoilingBoardAnimation::new(
            BoardSize::new(board_length, board_length),
            LEFT_I.cast_unit(),
            FloorColorEnum::Function(Graphics::big_chess_pattern),
        );
        let start_time = animation.start_time();

        // TODO: binary search instead, if this is slow
        let steps = 1000;
        for i in 0..steps {
            let fraction_of_second = i as f32 / steps as f32;
            let age = Duration::from_secs_f32(fraction_of_second);
            let animation_time = start_time + age;
            let glyph_map = animation.glyphs_at_time(animation_time);
            let right_half_of_top_left_square =
                WorldCharacterSquare::new(1, board_length as i32 - 1);
            let test_glyph = glyph_map.get(&right_half_of_top_left_square).unwrap();
            let target_char = '▉'; // one left of solid
            let bad_char = '▊'; // two left of solid
            if test_glyph.character == target_char {
                // test pass
                //println!("good character detected");
                break;
            }
            if test_glyph.character == bad_char {
                assert!(false, "bad character found");
            }
        }
    }

    #[test]
    #[ignore = "More for visual debugging than an actual test"]
    fn test_draw_tiny_board_recoil() {
        let board_length = 3;
        let animation = RecoilingBoardAnimation::new(
            BoardSize::new(board_length, board_length),
            RIGHT_I.cast_unit(),
            FloorColorEnum::Function(Graphics::big_chess_pattern),
        );
        let start_time = animation.start_time();

        let steps = 110;
        for i in 0..steps {
            let seconds = 0.11 * i as f32;
            let age = Duration::from_secs_f32(seconds);
            let animation_time = start_time + age;
            let glyph_map = animation.glyphs_at_time(animation_time);
            println!(
                "v-- seconds: {}\n{}",
                age.as_secs_f32(),
                glyph_map_to_string(&glyph_map)
            );
        }
        assert!(false);
    }

    #[test]
    fn test_simple_laser_transparent_background() {
        let animation =
            SimpleLaserAnimation::new(WorldPoint::new(0.0, 0.0), WorldPoint::new(10.0, 0.0));
        let glyph_map = animation.glyphs_at_time(animation.start_time() + Duration::from_millis(1));
        assert!(glyph_map.values().all(|glyph| glyph.bg_transparent == true));
    }

    #[test]
    fn test_floaty_laser_transparent_background() {
        let animation =
            FloatyLaserAnimation::new(WorldPoint::new(0.0, 0.0), WorldPoint::new(10.0, 0.0));
        let glyph_map = animation.glyphs_at_time(animation.start_time() + Duration::from_millis(1));
        assert!(glyph_map.values().all(|glyph| glyph.bg_transparent == true));
    }

    #[test]
    fn test_recoil_function_start_at_zero() {
        assert_eq!(
            RecoilingBoardAnimation::recoil_distance_in_squares_at_age(0.0),
            0.0
        );
    }

    #[test]
    fn test_recoil_function_start_fast() {
        assert!(
            derivative(
                RecoilingBoardAnimation::recoil_distance_in_squares_at_age,
                0.0,
                0.0001,
            ) > 0.0
        );
    }

    #[test]
    fn test_recoil_function_hit_peak() {
        assert_eq!(
            RecoilingBoardAnimation::recoil_distance_in_squares_at_age(
                RecoilingBoardAnimation::TIME_TO_PEAK.as_secs_f32()
            ),
            RecoilingBoardAnimation::RECOIL_DISTANCE.0
        );
    }

    #[test]
    fn test_recoil_function_flat_peak() {
        let slope = derivative(
            RecoilingBoardAnimation::recoil_distance_in_squares_at_age,
            RecoilingBoardAnimation::TIME_TO_PEAK.as_secs_f32(),
            0.0001,
        );
        assert!(slope.abs() < 0.01, "slope: {slope}");
    }

    #[test]
    fn test_recoil_function_fully_relax() {
        let height = RecoilingBoardAnimation::recoil_distance_in_squares_at_age(
            RecoilingBoardAnimation::RECOIL_DURATION.as_secs_f32(),
        );
        assert!(height.abs() < 0.01, "height: {}", height);
    }

    #[test]
    fn test_recoil_function_relax_flat() {
        let slope = derivative(
            RecoilingBoardAnimation::recoil_distance_in_squares_at_age,
            RecoilingBoardAnimation::RECOIL_DURATION.as_secs_f32(),
            0.0001,
        );
        assert!(slope.abs() < 0.01, "slope: {}", slope);
    }
}
