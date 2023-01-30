use crate::animations::static_board::StaticBoard;
use crate::animations::{Animation, BoardAnimation};
use crate::glyph::Glyph;
use crate::graphics::Graphics;
use crate::utility::coordinate_frame_conversions::{
    world_square_glyph_map_to_world_character_glyph_map, BoardSize, WorldCharacterSquareGlyphMap,
    WorldMove, WorldSquare, WorldSquareGlyphMap, WorldStep,
};
use crate::utility::{is_diagonal_king_step, is_orthogonal_king_step, round_to_king_step, RIGHT_I};
use euclid::Length;
use std::f32::consts::PI;
use std::time::{Duration, Instant};

#[derive(Clone, PartialEq, Debug, Copy)]
pub struct RecoilingBoard {
    board_size: BoardSize,
    orthogonal_shot_direction: WorldStep,
    start_time: Instant,
}

impl RecoilingBoard {
    pub(crate) const TIME_TO_PEAK: Duration = Duration::from_secs_f32(0.1);
    const RECOIL_RELAX_DURATION: Duration =
        Duration::from_secs_f32(RecoilingBoard::TIME_TO_PEAK.as_secs_f32() * 3.0);
    pub(crate) const RECOIL_DURATION: Duration = Duration::from_secs_f32(
        RecoilingBoard::TIME_TO_PEAK.as_secs_f32()
            + RecoilingBoard::RECOIL_RELAX_DURATION.as_secs_f32(),
    );
    pub(crate) const RECOIL_DISTANCE: Length<f32, WorldSquare> = Length::new(1.0);

    pub fn new(board_size: BoardSize, shot_direction: WorldStep) -> RecoilingBoard {
        let mut orthogonalized_step = round_to_king_step(shot_direction);
        if is_diagonal_king_step(orthogonalized_step) {
            orthogonalized_step.y = 0;
        }

        RecoilingBoard {
            board_size,
            orthogonal_shot_direction: orthogonalized_step,
            start_time: Instant::now(),
        }
    }
    fn recoil_start(age: f32, end_height: f32, end_time: f32) -> f32 {
        // f(0.0)=0
        // f'(0.0)>0
        // f(1.0)=1
        // f'(1.0)=0

        fn normalized_sin_rise(t: f32) -> f32 {
            (t * (PI / 2.0)).sin()
        }

        normalized_sin_rise(age / end_time) * end_height
    }
    fn recoil_end(age: f32, start_height: f32, start_time: f32, end_time: f32) -> f32 {
        // f(0.0)=1
        // f'(0.0)=0
        // f(1.0)=0
        // f'(1.0)=0

        fn normalized_cos_ease_in_and_out(t: f32) -> f32 {
            ((t * PI).cos() + 1.0) / 2.0
        }

        let duration = end_time - start_time;
        normalized_cos_ease_in_and_out(((age - start_time) / duration)) * start_height
    }

    pub(crate) fn recoil_distance_in_squares_at_age(age: f32) -> f32 {
        // shot in positive direction, so recoil position should start negative at a fixed velocity
        // linear negative triangle
        let fraction_done = age / RecoilingBoard::RECOIL_DURATION.as_secs_f32();
        if age < RecoilingBoard::TIME_TO_PEAK.as_secs_f32() {
            RecoilingBoard::recoil_start(
                age,
                RecoilingBoard::RECOIL_DISTANCE.0,
                RecoilingBoard::TIME_TO_PEAK.as_secs_f32(),
            )
        } else {
            RecoilingBoard::recoil_end(
                age,
                RecoilingBoard::RECOIL_DISTANCE.0,
                RecoilingBoard::TIME_TO_PEAK.as_secs_f32(),
                RecoilingBoard::RECOIL_DURATION.as_secs_f32(),
            )
        }
    }
}

impl Animation for RecoilingBoard {
    fn start_time(&self) -> Instant {
        self.start_time
    }

    fn glyphs_at_time(&self, time: Instant) -> WorldCharacterSquareGlyphMap {
        let age = time.duration_since(self.start_time);

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
        time.duration_since(self.start_time) > RecoilingBoard::RECOIL_DURATION
    }
}

impl BoardAnimation for RecoilingBoard {
    fn next_animation(&self) -> Box<dyn BoardAnimation> {
        Box::new(StaticBoard::new(self.board_size))
    }
}
