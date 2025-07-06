use std::f32::consts::PI;
use std::time::{Duration, Instant};

use euclid::Length;
use rgb::RGB8;
use crate::graphics::*;
use terminal_rendering::*;
use utility::*;

#[derive(Clone)]
pub struct RecoilingBoardAnimation {
    board_size: BoardSize,
    orthogonal_shot_direction: OrthogonalWorldStep,
    start_time: Instant,
    floor_color_enum: FloorColorEnum,
}

impl RecoilingBoardAnimation {
    pub(crate) const TIME_TO_PEAK: Duration = Duration::from_millis(100);
    const RECOIL_RELAX_DURATION: Duration = Self::TIME_TO_PEAK.saturating_mul(3);
    pub(crate) const RECOIL_DURATION: Duration =
        Self::TIME_TO_PEAK.saturating_add(Self::RECOIL_RELAX_DURATION);
    pub(crate) const RECOIL_DISTANCE: Length<f32, WorldSquare> = Length::new(1.0);

    pub fn new(
        board_size: BoardSize,
        shot_direction: WorldStep,
        floor_color_enum: FloorColorEnum,
    ) -> RecoilingBoardAnimation {
        let mut orthogonalized_step = round_to_king_step(shot_direction);
        if is_diagonal_king_step(orthogonalized_step) {
            orthogonalized_step.y = 0;
        }

        RecoilingBoardAnimation {
            board_size,
            orthogonal_shot_direction: orthogonalized_step.into(),
            start_time: Instant::now(),
            floor_color_enum,
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
        let fraction_done = age / RecoilingBoardAnimation::RECOIL_DURATION.as_secs_f32();
        if age < RecoilingBoardAnimation::TIME_TO_PEAK.as_secs_f32() {
            RecoilingBoardAnimation::recoil_start(
                age,
                RecoilingBoardAnimation::RECOIL_DISTANCE.0,
                RecoilingBoardAnimation::TIME_TO_PEAK.as_secs_f32(),
            )
        } else {
            RecoilingBoardAnimation::recoil_end(
                age,
                RecoilingBoardAnimation::RECOIL_DISTANCE.0,
                RecoilingBoardAnimation::TIME_TO_PEAK.as_secs_f32(),
                RecoilingBoardAnimation::RECOIL_DURATION.as_secs_f32(),
            )
        }
    }
}

impl Animation for RecoilingBoardAnimation {
    fn start_time(&self) -> Instant {
        self.start_time
    }
    fn duration(&self) -> Duration {
        RecoilingBoardAnimation::RECOIL_DURATION
    }

    fn glyphs_at_time(&self, time: Instant) -> WorldCharacterSquareGlyphMap {
        let age = time.duration_since(self.start_time);

        let mut offset_distance_in_squares: f32 =
            RecoilingBoardAnimation::recoil_distance_in_squares_at_age(age.as_secs_f32());

        let mut glyph_map = WorldSquareGlyphMap::new();

        let offset_vector: WorldMove =
            self.orthogonal_shot_direction.step().to_f32() * offset_distance_in_squares;

        for x in 0..self.board_size.width {
            for y in 0..self.board_size.height {
                let world_square: WorldSquare = WorldSquare::new(x as i32, y as i32);
                let square_color = self.floor_color_enum.color_at(world_square);
                let other_square_color = self
                    .floor_color_enum
                    .color_at(world_square + RIGHT_I.cast_unit());

                let glyphs = Glyph::orthogonally_offset_board_square_glyphs(
                    offset_vector,
                    square_color,
                    other_square_color,
                );
                glyph_map.insert(world_square, glyphs);
            }
        }
        world_square_glyph_map_to_world_character_glyph_map(glyph_map)
    }
}
