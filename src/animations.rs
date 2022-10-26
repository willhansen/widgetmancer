use dyn_clone::DynClone;
use std::collections::HashMap;
use std::f32::consts::{E, PI, TAU};
use std::time;
use std::time::{Duration, Instant};

use euclid::{vec2, Angle};
use num::ToPrimitive;
use rand::{Rng, SeedableRng};
use termion::color::Black;

use crate::{
    BufferCharacterSquare, Glyph, Graphics, WorldGlyphMap, WorldMove, WorldPoint, WorldSquare,
    BLACK, EXPLOSION_COLOR, RED, RIGHT_I, SELECTOR_COLOR, UP_I,
};

pub type AnimationObject = Box<dyn Animation>;
pub type AnimationList = Vec<AnimationObject>;

pub trait Animation: DynClone {
    fn glyphs_at_time(&self, time: Instant) -> WorldGlyphMap;
    fn finished_at_time(&self, time: Instant) -> bool;
}
// This is kinda magic.  Not great, but if it works, it works.
dyn_clone::clone_trait_object!(Animation);

pub trait BoardAnimation: Animation {
    fn next_animation(&self) -> Box<dyn BoardAnimation>;
}

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
    fn glyphs_at_time(&self, _time: Instant) -> WorldGlyphMap {
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
    fn glyphs_at_time(&self, time: Instant) -> WorldGlyphMap {
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
    fn glyphs_at_time(&self, time: Instant) -> WorldGlyphMap {
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
    fn glyphs_at_time(&self, time: Instant) -> WorldGlyphMap {
        let num_dots = 5;
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
    width: u32,
    height: u32,
}

impl StaticBoard {
    pub fn new(width: u32, height: u32) -> StaticBoard {
        StaticBoard { width, height }
    }
}

impl Animation for StaticBoard {
    fn glyphs_at_time(&self, _time: Instant) -> WorldGlyphMap {
        let mut glyphs = WorldGlyphMap::new();
        for x in 0..self.width {
            for y in 0..self.height {
                let world_square = WorldSquare::new(x as i32, y as i32);
                let glyph = Glyph {
                    character: ' ',
                    fg_color: BLACK,
                    bg_color: Graphics::board_color_at_square(world_square),
                };
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
