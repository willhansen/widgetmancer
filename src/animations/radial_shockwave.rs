use crate::animations::static_board::StaticBoard;
use crate::animations::Animation;
use crate::glyph::floating_square;
use crate::glyph::Glyph;
use crate::graphics::{FloorColorEnum, Graphics};
use crate::utility::coordinate_frame_conversions::{
    world_square_glyph_map_to_world_character_glyph_map, BoardSize, WorldCharacterSquareGlyphMap,
    WorldMove, WorldSquare, WorldSquareGlyphMap, WorldStep,
};
use crate::utility::{
    is_diagonal_king_step, is_orthogonal_king_step, round_to_king_step, RIGHT_I, STEP_RIGHT,
};
use euclid::{vec2, Length};
use rgb::RGB8;
use std::f32::consts::PI;
use std::time::{Duration, Instant};

pub struct RadialShockwave {
    start_square: WorldSquare,
    start_time: Instant,
    floor_color_enum: FloorColorEnum,
}

impl RadialShockwave {
    pub fn new(start_square: WorldSquare, floor_color_enum: FloorColorEnum) -> RadialShockwave {
        RadialShockwave {
            start_square,
            start_time: Instant::now(),
            floor_color_enum,
        }
    }
    fn normalized_single_period_triangle_sine_wave(x: f32) -> f32 {
        let y = if x.abs() > 1.0 {
            0.0
        } else if x.abs() <= 0.5 {
            x
        } else if x > 0.5 {
            -x + 1.0
        } else {
            -x - 1.0
        };
        y * 2.0
    }
    fn normalized_single_period_sine_wave(x: f32) -> f32 {
        if x.abs() > 1.0 {
            0.0
        } else {
            (x * PI).sin()
        }
    }

    fn shockwave_height(measure_radius: f32, shockwave_radius: f32, wavelength: f32) -> f32 {
        let height = RadialShockwave::normalized_single_period_sine_wave(
            (measure_radius - shockwave_radius) / wavelength + 1.0,
        );
        assert!(height.abs() <= 1.0);
        height
    }
    const SHOCKWAVE_SPEED: f32 = 9.0;
    const WAVELENGTH: f32 = 5.0;
}

impl Animation for RadialShockwave {
    fn start_time(&self) -> Instant {
        self.start_time
    }
    fn duration(&self) -> Duration {
        Duration::from_secs_f32(30.0 / RadialShockwave::SHOCKWAVE_SPEED)
    }

    fn glyphs_at_time(&self, time: Instant) -> WorldCharacterSquareGlyphMap {
        let shockwave_speed = RadialShockwave::SHOCKWAVE_SPEED;
        let wavelength = RadialShockwave::WAVELENGTH;
        let shockwave_radius = self.age_at_time(time).as_secs_f32() * shockwave_speed;
        let rounded_outer_edge_radius = shockwave_radius.ceil() as i32;

        let mut glyph_map = WorldSquareGlyphMap::new();

        for dx in -rounded_outer_edge_radius..=rounded_outer_edge_radius {
            for dy in -rounded_outer_edge_radius..=rounded_outer_edge_radius {
                let rel_square = vec2(dx, dy);
                let square: WorldSquare = self.start_square + rel_square;
                let square_radius = rel_square.to_f32().length();

                let mut height =
                    RadialShockwave::shockwave_height(square_radius, shockwave_radius, wavelength);

                if height == 0.0 {
                    continue;
                }

                if square_radius > 1.0 {
                    height /= square_radius;
                }

                if (Graphics::square_is_white(square) && height > 0.0)
                    || (!Graphics::square_is_white(square) && height < 0.0)
                {
                    continue;
                }
                let glyphs = [-1.0, 1.0].map(|i| {
                    Glyph::new(
                        floating_square::character_for_square_with_1d_offset(
                            false,
                            (1.0 - height.abs()) * i,
                        ),
                        self.floor_color_enum.color_at(square + STEP_RIGHT),
                        self.floor_color_enum.color_at(square),
                    )
                });

                glyph_map.insert(square, glyphs);
            }
        }
        world_square_glyph_map_to_world_character_glyph_map(glyph_map)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::animations::recoiling_board::RecoilingBoardAnimation;
    use crate::glyph::glyph_constants::RED;
    use euclid::point2;
    use ntest::assert_about_eq;

    #[test]
    fn test_triangle_wave() {
        assert_about_eq!(
            RadialShockwave::normalized_single_period_triangle_sine_wave(0.0),
            0.0
        );
        assert_about_eq!(
            RadialShockwave::normalized_single_period_triangle_sine_wave(1.0),
            0.0
        );
        assert_about_eq!(
            RadialShockwave::normalized_single_period_triangle_sine_wave(-1.0),
            0.0
        );
        assert_about_eq!(
            RadialShockwave::normalized_single_period_triangle_sine_wave(1.5),
            0.0
        );
        assert_about_eq!(
            RadialShockwave::normalized_single_period_triangle_sine_wave(-1.5),
            0.0
        );

        assert_about_eq!(
            RadialShockwave::normalized_single_period_triangle_sine_wave(0.5),
            1.0
        );
        assert_about_eq!(
            RadialShockwave::normalized_single_period_triangle_sine_wave(-0.5),
            -1.0
        );
        assert_about_eq!(
            RadialShockwave::normalized_single_period_triangle_sine_wave(0.25),
            0.5
        );
        assert_about_eq!(
            RadialShockwave::normalized_single_period_triangle_sine_wave(-0.25),
            -0.5
        );
    }

    #[test]
    fn test_has_some_glyphs() {
        let anim = RadialShockwave::new(point2(5, 5), FloorColorEnum::Solid(RED));
        assert!(anim.glyphs_at_duration(Duration::from_secs_f32(0.0)).len() < 5);
        assert!(anim.glyphs_at_duration(Duration::from_secs_f32(2.0)).len() > 5);
    }

    #[test]
    fn test_shockwave_heights() {
        // past edge of shockwave is zero
        assert_about_eq!(RadialShockwave::shockwave_height(1.0, 0.0, 1.0), 0.0);
        assert_about_eq!(RadialShockwave::shockwave_height(0.5, 0.0, 1.0), 0.0);
        // at edge of shockwave is zero
        assert_about_eq!(RadialShockwave::shockwave_height(0.0, 0.0, 1.0), 0.0);
        assert_about_eq!(RadialShockwave::shockwave_height(1.0, 1.0, 1.0), 0.0);
        assert_about_eq!(RadialShockwave::shockwave_height(1.0, 1.0, 10.0), 0.0);
    }
}
