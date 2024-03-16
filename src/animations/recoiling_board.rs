use crate::animations::Animation;
use crate::glyph::Glyph;
use crate::graphics::FloorColorEnum;
use crate::size_2d::Size2D;
use crate::utility::*;
use crate::IntCoordinate;
use euclid::Length;
use std::f32::consts::PI;
use std::time::{Duration, Instant};

#[derive(Clone)]
pub struct RecoilingBoardAnimation {
    board_size: BoardSize,
    orthogonal_shot_direction: NormalizedOrthoAngle,
    start_time: Instant,
    floor_color_enum: FloorColorEnum,
}

impl RecoilingBoardAnimation {
    pub(crate) const TIME_TO_PEAK_S: f32 = 0.1;
    const RECOIL_RELAX_DURATION_S: f32 = RecoilingBoardAnimation::TIME_TO_PEAK_S * 3.0;
    pub(crate) const RECOIL_DURATION_S: f32 =
        RecoilingBoardAnimation::TIME_TO_PEAK_S + RecoilingBoardAnimation::RECOIL_RELAX_DURATION_S;

    pub(crate) const RECOIL_DISTANCE: Length<f32, WorldSquare> = Length::new(1.0);

    pub fn new(
        board_size: BoardSize,
        shot_direction: WorldStep,
        floor_color_enum: FloorColorEnum,
    ) -> RecoilingBoardAnimation {
        let mut orthogonalized_step = round_to_king_step(shot_direction);
        if orthogonalized_step.is_diagonal_king_step() {
            orthogonalized_step.y = 0;
        }

        RecoilingBoardAnimation {
            board_size,
            orthogonal_shot_direction: NormalizedOrthoAngle::from_orthogonal_vector(
                orthogonalized_step,
            ),
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
        normalized_cos_ease_in_and_out((age - start_time) / duration) * start_height
    }

    pub(crate) fn recoil_distance_in_squares_at_age(age: f32) -> f32 {
        // shot in positive direction, so recoil position should start negative at a fixed velocity
        // linear negative triangle
        let fraction_done = age / RecoilingBoardAnimation::RECOIL_DURATION_S;
        if age < RecoilingBoardAnimation::TIME_TO_PEAK_S {
            RecoilingBoardAnimation::recoil_start(
                age,
                RecoilingBoardAnimation::RECOIL_DISTANCE.0,
                RecoilingBoardAnimation::TIME_TO_PEAK_S,
            )
        } else {
            RecoilingBoardAnimation::recoil_end(
                age,
                RecoilingBoardAnimation::RECOIL_DISTANCE.0,
                RecoilingBoardAnimation::TIME_TO_PEAK_S,
                RecoilingBoardAnimation::RECOIL_DURATION_S,
            )
        }
    }
}

impl Animation for RecoilingBoardAnimation {
    fn start_time(&self) -> Instant {
        self.start_time
    }
    fn duration(&self) -> Duration {
        Duration::from_secs_f32(RecoilingBoardAnimation::RECOIL_DURATION_S)
    }

    fn glyphs_at_time(&self, time: Instant) -> WorldCharacterSquareGlyphMap {
        let age = time.duration_since(self.start_time);

        let mut offset_distance_in_squares: f32 =
            RecoilingBoardAnimation::recoil_distance_in_squares_at_age(age.as_secs_f32());

        let mut glyph_map = WorldSquareGlyphMap::new();

        let offset_vector: WorldMove =
            self.orthogonal_shot_direction.step::<WorldMove>() * offset_distance_in_squares;

        for x in 0..self.board_size.width() {
            for y in 0..self.board_size.height() {
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
