use euclid::Angle;
use num::ToPrimitive;
use rand::{Rng, SeedableRng};
use std::collections::HashMap;
use std::f32::consts::{PI, TAU};
use std::time::Duration;

use crate::{
    BufferSquare, Glyph, WorldGlyphMap, WorldMove, WorldPoint, WorldSquare, EXPLOSION_COLOR, RED,
};

pub trait Animation {
    fn glyphs(&self) -> WorldGlyphMap;
    fn advance(&mut self, delta: Duration);
    fn finished(&self) -> bool;
}

#[derive(Clone, PartialEq, Debug, Copy)]
pub struct Laser {
    start: WorldPoint,
    end: WorldPoint,
    age: Duration,
}

impl Laser {
    pub fn new(start: WorldPoint, end: WorldPoint) -> Laser {
        Laser {
            start,
            end,
            age: Duration::from_millis(0),
        }
    }
}

impl Animation for Laser {
    fn glyphs(&self) -> WorldGlyphMap {
        Glyph::get_glyphs_for_colored_braille_line(self.start, self.end, RED)
    }

    fn advance(&mut self, delta: Duration) {
        self.age += delta;
    }

    fn finished(&self) -> bool {
        self.age > Duration::from_millis(500)
    }
}

#[derive(Clone, PartialEq, Debug, Copy)]
pub struct Explosion {
    position: WorldPoint,
    age: Duration,
}

impl Explosion {
    pub fn new(position: WorldPoint) -> Explosion {
        Explosion {
            position,
            age: Duration::from_millis(0),
        }
    }
}

impl Animation for Explosion {
    fn glyphs(&self) -> WorldGlyphMap {
        let hash = ((self.position.x * PI + self.position.y) * 1000.0)
            .abs()
            .floor()
            .to_u64()
            .unwrap();
        let mut rng = rand::rngs::StdRng::seed_from_u64(hash);
        let mut points_to_draw: Vec<WorldPoint> = vec![];
        let num_particles = 10;
        for _ in 0..num_particles {
            let speed_in_squares_per_second = 10.0 + rng.gen_range(-2.0..=2.0);
            let distance_in_squares = speed_in_squares_per_second * self.age.as_secs_f32();
            let angle = Angle::radians(rng.gen_range(0.0..TAU));
            let relative_position = WorldMove::from_angle_and_length(angle, distance_in_squares);
            let particle_pos = self.position + relative_position;
            points_to_draw.push(particle_pos);
        }
        Glyph::points_to_braille_glyphs(points_to_draw, EXPLOSION_COLOR)
    }

    fn advance(&mut self, delta: Duration) {
        self.age += delta;
    }

    fn finished(&self) -> bool {
        self.age > Duration::from_millis(200)
    }
}

#[derive(Clone, PartialEq, Debug, Copy)]
pub struct Selector {
    square: WorldSquare,
    age: Duration,
}

impl Selector {
    pub fn new(square: WorldSquare) -> Selector {
        Selector {
            square,
            age: Duration::from_millis(0),
        }
    }
}

impl Animation for Selector {
    fn glyphs(&self) -> WorldGlyphMap {
        todo!()
    }

    fn advance(&mut self, delta: Duration) {
        self.age += delta;
    }

    fn finished(&self) -> bool {
        false
    }
}
