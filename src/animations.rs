use std::collections::HashMap;
use std::f32::consts::{E, PI, TAU};
use std::time;
use std::time::{Duration, Instant};

use dyn_clone::DynClone;
use euclid::{vec2, Angle, Length};
use num::ToPrimitive;
use rand::{Rng, SeedableRng};
use termion::color::Black;

use crate::{
    is_diagonal_king_step, is_orthogonal_king_step, lerp, round_to_king_step,
    world_square_glyph_map_to_world_character_glyph_map, BoardSize, BufferCharacterSquare, Glyph,
    Graphics, WorldCharacterGlyphMap, WorldMove, WorldPoint, WorldSquare, WorldSquareGlyphMap,
    WorldStep, BLACK, EXPLOSION_COLOR, RED, RIGHT_I, SELECTOR_COLOR, UP_I,
};

pub type AnimationObject = Box<dyn Animation>;
pub type AnimationList = Vec<AnimationObject>;

pub trait Animation: DynClone {
    fn glyphs_at_time(&self, time: Instant) -> WorldCharacterGlyphMap;
    fn finished_at_time(&self, time: Instant) -> bool;
}
// This is kinda magic.  Not great, but if it works, it works.
dyn_clone::clone_trait_object!(Animation);

pub trait BoardAnimation: Animation {
    fn next_animation(&self) -> Box<dyn BoardAnimation>;
}
dyn_clone::clone_trait_object!(BoardAnimation);

#[derive(Clone, PartialEq, Debug, Copy)]
pub struct SimpleLaser {
    start: WorldPoint,
    end: WorldPoint,
    creation_time: Instant,
}

impl SimpleLaser {
    pub fn new(start: WorldPoint, end: WorldPoint) -> SimpleLaser {
        SimpleLaser {
            start,
            end,
            creation_time: Instant::now(),
        }
    }
}

impl Animation for SimpleLaser {
    fn glyphs_at_time(&self, _time: Instant) -> WorldCharacterGlyphMap {
        Glyph::get_glyphs_for_colored_braille_line(self.start, self.end, RED)
    }

    fn finished_at_time(&self, time: Instant) -> bool {
        time.duration_since(self.creation_time) > Duration::from_millis(500)
    }
}

#[derive(Clone, PartialEq, Debug, Copy)]
pub struct FloatyLaser {
    start: WorldPoint,
    end: WorldPoint,
    creation_time: Instant,
}

impl FloatyLaser {
    pub fn new(start: WorldPoint, end: WorldPoint) -> FloatyLaser {
        FloatyLaser {
            start,
            end,
            creation_time: Instant::now(),
        }
    }
}

impl Animation for FloatyLaser {
    fn glyphs_at_time(&self, time: Instant) -> WorldCharacterGlyphMap {
        let mut line_points: Vec<WorldPoint> =
            Glyph::world_points_for_braille_line(self.start, self.end);
        // pretty arbitrary
        let hash = ((self.start.x * PI + self.start.y)
            * 1000.0
            * (self.end.x * E + self.end.y * TAU * 5.0))
            .abs()
            .floor()
            .to_u64()
            .unwrap();
        let mut rng = rand::rngs::StdRng::seed_from_u64(hash);
        let vertical_drift_speed_blocks_per_s = 3.0;
        let random_drift_speed_blocks_per_s = 1.0;
        let age = time.duration_since(self.creation_time);
        for mut point in &mut line_points {
            let vertical_displacement: WorldMove =
                WorldMove::new(0.0, 1.0) * vertical_drift_speed_blocks_per_s * age.as_secs_f32();
            let random_angle = Angle::radians(rng.gen_range(0.0..TAU));
            let random_displacement = WorldMove::from_angle_and_length(
                random_angle,
                random_drift_speed_blocks_per_s * age.as_secs_f32(),
            );
            *point += vertical_displacement + random_displacement;
        }
        Glyph::points_to_braille_glyphs(line_points, RED)
        //Glyph::get_glyphs_for_colored_braille_line(self.start, self.end, RED)
    }

    fn finished_at_time(&self, time: Instant) -> bool {
        time.duration_since(self.creation_time) > Duration::from_millis(500)
    }
}

#[derive(Clone, PartialEq, Debug, Copy)]
pub struct Explosion {
    position: WorldPoint,
    creation_time: Instant,
}

impl Explosion {
    pub fn new(position: WorldPoint) -> Explosion {
        Explosion {
            position,
            creation_time: Instant::now(),
        }
    }
}

impl Animation for Explosion {
    fn glyphs_at_time(&self, time: Instant) -> WorldCharacterGlyphMap {
        // rather arbitrary
        let hash = ((self.position.x * PI + self.position.y) * 1000.0)
            .abs()
            .floor()
            .to_u64()
            .unwrap();
        let mut rng = rand::rngs::StdRng::seed_from_u64(hash);
        let mut points_to_draw: Vec<WorldPoint> = vec![];
        let num_particles = 20;
        let age = time.duration_since(self.creation_time);
        for _ in 0..num_particles {
            let radius: f32 = 10.0;
            let speed_in_squares_per_second = rng.gen_range(0.0..=(radius.powi(2))).sqrt();

            let distance_in_squares = speed_in_squares_per_second * age.as_secs_f32();
            let angle = Angle::radians(rng.gen_range(0.0..TAU));
            let relative_position = WorldMove::from_angle_and_length(angle, distance_in_squares);
            let particle_pos = self.position + relative_position;
            points_to_draw.push(particle_pos);
        }
        Glyph::points_to_braille_glyphs(points_to_draw, EXPLOSION_COLOR)
    }

    fn finished_at_time(&self, time: Instant) -> bool {
        time.duration_since(self.creation_time) > Duration::from_millis(500)
    }
}

pub const DOTS_IN_SELECTOR: u32 = 5;

#[derive(Clone, PartialEq, Debug, Copy)]
pub struct Selector {
    square: WorldSquare,
    creation_time: Instant,
}

impl Selector {
    pub fn new(square: WorldSquare) -> Selector {
        Selector {
            square,
            creation_time: Instant::now(),
        }
    }
}

impl Animation for Selector {
    fn glyphs_at_time(&self, time: Instant) -> WorldCharacterGlyphMap {
        let num_dots = DOTS_IN_SELECTOR;
        let radius_in_squares = f32::sqrt(2.0) / 2.0;

        let rotation_rate_rad_per_s = 3.0;
        let age = time.duration_since(self.creation_time);

        let base_angle = Angle::radians(rotation_rate_rad_per_s * age.as_secs_f32());
        let mut points = vec![];
        for i in 0..num_dots {
            let radians: f32 = (base_angle).radians + i as f32 / num_dots as f32 * TAU;
            let relative_point = WorldMove::new(
                radius_in_squares * radians.cos(),
                radius_in_squares * radians.sin(),
            );
            points.push(self.square.to_f32() + relative_point);
        }
        Glyph::points_to_braille_glyphs(points, SELECTOR_COLOR)
    }

    fn finished_at_time(&self, _time: Instant) -> bool {
        false
    }
}

#[derive(Clone, PartialEq, Debug, Copy)]
pub struct StaticBoard {
    board_size: BoardSize,
}

impl StaticBoard {
    pub fn new(board_size: BoardSize) -> StaticBoard {
        StaticBoard { board_size }
    }
}

impl Animation for StaticBoard {
    fn glyphs_at_time(&self, _time: Instant) -> WorldCharacterGlyphMap {
        let mut glyphs = WorldCharacterGlyphMap::new();
        for x in 0..self.board_size.width {
            for y in 0..self.board_size.height {
                let world_square = WorldSquare::new(x as i32, y as i32);
                let glyph = Glyph::new(' ', BLACK, Graphics::board_color_at_square(world_square));
                let left_character_square =
                    Glyph::world_square_to_left_world_character_square(world_square);
                let right_character_square = left_character_square + vec2(1, 0);
                glyphs.insert(left_character_square, glyph);
                glyphs.insert(right_character_square, glyph);
            }
        }
        glyphs
    }

    fn finished_at_time(&self, _time: Instant) -> bool {
        false
    }
}

impl BoardAnimation for StaticBoard {
    fn next_animation(&self) -> Box<dyn BoardAnimation> {
        Box::new(*self)
    }
}

#[derive(Clone, PartialEq, Debug, Copy)]
pub struct RecoilingBoard {
    board_size: BoardSize,
    orthogonal_shot_direction: WorldStep,
    creation_time: Instant,
}

const RECOIL_DURATION: Duration = Duration::from_secs_f32(0.2);
const RECOIL_DISTANCE: Length<f32, WorldSquare> = Length::new(1.0);

impl RecoilingBoard {
    pub fn new(board_size: BoardSize, shot_direction: WorldStep) -> RecoilingBoard {
        let mut orthogonalized_step = round_to_king_step(shot_direction);
        if is_diagonal_king_step(orthogonalized_step) {
            orthogonalized_step.y = 0;
        }

        RecoilingBoard {
            board_size,
            orthogonal_shot_direction: orthogonalized_step,
            creation_time: Instant::now(),
        }
    }

    fn recoil_distance_in_squares_at_age(age: f32) -> f32 {
        // shot in positive direction, so recoil position should start negative at a fixed velocity
        // linear negative triangle
        let peak_dist = RECOIL_DISTANCE.0;
        let time_to_peak = RECOIL_DURATION.as_secs_f32() / 2.0;

        if age < time_to_peak {
            let t = age / time_to_peak;
            lerp(0.0, peak_dist, t)
        } else if age < 2.0 * time_to_peak {
            let t = age / time_to_peak - 1.0;
            lerp(peak_dist, 0.0, t)
        } else {
            0.0
        }
    }

    pub fn creation_time(&self) -> Instant {
        self.creation_time
    }
}

impl Animation for RecoilingBoard {
    fn glyphs_at_time(&self, time: Instant) -> WorldCharacterGlyphMap {
        let age = time.duration_since(self.creation_time);

        let mut offset_distance_in_squares: f32 =
            RecoilingBoard::recoil_distance_in_squares_at_age(age.as_secs_f32());

        let mut glyph_map = WorldSquareGlyphMap::new();

        assert!(is_orthogonal_king_step(self.orthogonal_shot_direction));
        let offset_vector: WorldMove =
            self.orthogonal_shot_direction.to_f32() * offset_distance_in_squares;

        for x in 0..self.board_size.width {
            for y in 0..self.board_size.height {
                let world_square: WorldSquare = WorldSquare::new(x as i32, y as i32);
                let square_color = Graphics::board_color_at_square(world_square);
                let other_square_color =
                    Graphics::board_color_at_square(world_square + RIGHT_I.cast_unit());

                let glyphs = Glyph::offset_board_square_glyphs(
                    offset_vector,
                    square_color,
                    other_square_color,
                );
                glyph_map.insert(world_square, glyphs);
            }
        }
        world_square_glyph_map_to_world_character_glyph_map(glyph_map)
    }

    fn finished_at_time(&self, time: Instant) -> bool {
        time.duration_since(self.creation_time) > RECOIL_DURATION
    }
}

impl BoardAnimation for RecoilingBoard {
    fn next_animation(&self) -> Box<dyn BoardAnimation> {
        Box::new(StaticBoard::new(self.board_size))
    }
}

#[cfg(test)]
mod tests {
    use pretty_assertions::{assert_eq, assert_ne};

    use crate::{WorldCharacterSquare, DOWN_I, LEFT_I};

    use super::*;

    #[test]
    fn test_recoil_distance_function_increasing_for_first_half() {
        let half_time = RECOIL_DURATION.as_secs_f32() / 2.0;
        let mut prev_d = 0.0;
        let mut t = 0.0;
        loop {
            let d = RecoilingBoard::recoil_distance_in_squares_at_age(t);
            if t >= half_time {
                break;
            }
            if t != 0.0 {
                //dbg!(&d, &prev_d);
                assert!(d < prev_d);
            }
            prev_d = d;
            t += 0.125;
        }
    }

    #[test]
    fn test_recoil_animation_has_smooth_animation__at_start_of_recoil_left() {
        let board_length = 5;
        let animation = RecoilingBoard::new(
            BoardSize::new(board_length, board_length),
            LEFT_I.cast_unit(),
        );
        let start_time = animation.creation_time();

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
                println!("good character detected");
                break;
            }
            if test_glyph.character == bad_char {
                assert!(false, "bad character found");
            }
        }
    }

    #[test]
    #[ignore] // more for visual debugging than an actual test
    fn test_draw_tiny_board_recoil() {
        let board_length = 3;
        let animation = RecoilingBoard::new(
            BoardSize::new(board_length, board_length),
            RIGHT_I.cast_unit(),
        );
        let start_time = animation.creation_time();

        let steps = 110;
        for i in 0..steps {
            let seconds = 0.11 * i as f32;
            let age = Duration::from_secs_f32(seconds);
            let animation_time = start_time + age;
            let glyph_map = animation.glyphs_at_time(animation_time);
            println!(
                "v-- seconds: {}\n{}",
                age.as_secs_f32(),
                Graphics::glyph_map_to_string(glyph_map)
            );
        }
        assert!(false);
    }

    #[test]
    fn test_simple_laser_transparent_background() {
        let animation = SimpleLaser::new(WorldPoint::new(0.0, 0.0), WorldPoint::new(10.0, 0.0));
        let glyph_map =
            animation.glyphs_at_time(animation.creation_time + Duration::from_millis(1));
        assert!(glyph_map.values().all(|glyph| glyph.bg_alpha == 0));
    }

    #[test]
    fn test_floaty_laser_transparent_background() {
        let animation = FloatyLaser::new(WorldPoint::new(0.0, 0.0), WorldPoint::new(10.0, 0.0));
        let glyph_map =
            animation.glyphs_at_time(animation.creation_time + Duration::from_millis(1));
        assert!(glyph_map.values().all(|glyph| glyph.bg_alpha == 0));
    }
}
