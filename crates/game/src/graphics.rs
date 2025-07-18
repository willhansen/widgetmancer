use std::any::Any;
use std::borrow::Borrow;
use std::cmp::min;
use std::collections::{HashMap, HashSet};
use std::f32::consts::{PI, TAU};
use std::fmt::Debug;
use std::io::Write;
use std::mem::swap;
use std::ptr::hash;
use std::time::{Duration, Instant};

use euclid::*;
use itertools::Itertools;
use line_drawing::Point;
use rand::{Rng, SeedableRng};
use rgb::RGB8;
use termion::color::Black;
use termion::input::MouseTerminal;
use termion::raw::RawTerminal;
use termion::terminal_size;

use glyph::glyph_constants::*;

use crate::fov_stuff::{FieldOfViewResult, PositionedSquareVisibilityInFov, SquareVisibility};
use crate::game::{
    DeathCube, FloatingEntityTrait, FloatingHunterDrone, CONVEYOR_BELT_VISUAL_PERIOD,
    HUNTER_DRONE_SIGHT_RANGE,
};
use crate::graphics::drawable::{
    ArrowDrawable, BrailleDrawable, ConveyorBeltDrawable, Drawable, DrawableEnum,
    OffsetSquareDrawable, PartialVisibilityDrawable, SolidColorDrawable, TextDrawable,
};
use crate::graphics::screen::{
    CharacterGridInScreenBufferFrame, Screen, ScreenBufferCharacterSquare, ScreenBufferStep,
};
pub use crate::num::ToPrimitive;
use crate::piece::{Piece, Upgrade};
use crate::{
    get_by_point, pair_up_character_square_map, point_to_string, DoubleGlyphFunctions, Game,
    IPoint, PieceType, RIGHT_I,
};
use terminal_rendering::*;
use utility::*;

pub(crate) mod drawable;

pub mod animations;
pub use animations::*;

pub mod game_colors {
    use super::*;

    pub const EXPLOSION_COLOR: RGB8 = BLACK; //RGB8::new(200, 200, 255);
    pub const SELECTOR_COLOR: RGB8 = RGB8::new(255, 64, 0);
    pub const ENEMY_PIECE_COLOR: RGB8 = WHITE;
    pub const DANGER_SQUARE_COLOR: RGB8 = GREY_RED;
    pub const PATH_COLOR: RGB8 = MAGENTA;
    pub const RED_PAWN_COLOR: RGB8 = RED;
    pub const BLINK_EFFECT_COLOR: RGB8 = COBALT_BLUE;
    pub const SPEAR_COLOR: RGB8 = MAROON;
    pub const HUNTER_DRONE_COLOR: RGB8 = BRICK_RED;
    pub const SIGHT_LINE_SEEKING_COLOR: RGB8 = GREEN;
}
use game_colors::*;

pub type FloorColorFunction = fn(WorldSquare) -> RGB8;

#[derive(Clone)]
pub enum FloorColorEnum {
    Function(FloorColorFunction),
    Solid(RGB8),
}

impl FloorColorEnum {
    pub fn color_at(&self, world_square: WorldSquare) -> RGB8 {
        match self {
            FloorColorEnum::Function(f) => f(world_square),
            FloorColorEnum::Solid(color) => *color,
        }
    }
}

pub struct Graphics {
    pub screen: Screen,
    draw_buffer: HashMap<WorldSquare, DrawableEnum>,
    active_animations: Vec<AnimationEnum>,
    board_animation: Option<AnimationEnum>,
    selectors: Vec<SelectorAnimation>,
    start_time: Instant,
    floor_color_enum: FloorColorEnum,
    pub tint_portals: bool,
    render_portals_with_line_of_sight: bool,
}

impl Graphics {
    pub fn new(terminal_width: u16, terminal_height: u16, start_time: Instant) -> Graphics {
        let mut g = Graphics {
            screen: Screen::new(terminal_width, terminal_height),
            draw_buffer: HashMap::default(),
            active_animations: vec![],
            board_animation: None,
            selectors: vec![],
            start_time,
            floor_color_enum: FloorColorEnum::Function(Graphics::big_chess_pattern),
            tint_portals: true,
            render_portals_with_line_of_sight: true,
        };
        g.screen.fill_screen_buffer(BLACK);
        g
    }

    pub fn start_time(&self) -> Instant {
        self.start_time
    }

    fn time_since_start(&self) -> Duration {
        Instant::now().duration_since(self.start_time)
    }

    fn count_braille_dots_in_square(&self, square: WorldSquare) -> u32 {
        return if self
            .screen
            .world_square_is_at_least_partially_on_screen(square)
        {
            count_braille_dots(
                self.screen
                    .get_screen_buffered_glyph(
                        self.screen
                            .world_square_to_left_screen_buffer_character_square(square),
                    )
                    .character,
            )
        } else {
            0
        };
    }

    pub fn clear_draw_buffer(&mut self) {
        self.draw_buffer.clear();
    }

    fn draw_braille_point(&mut self, pos: WorldPoint, color: RGB8) {
        self.draw_naive_braille_line(pos, pos, color);
    }

    fn draw_glyphs(&mut self, glyph_map: WorldCharacterSquareGlyphMap) {
        let world_square_glyph_map =
            pair_up_character_square_map(glyph_map, Glyph::transparent_glyph());
        self.draw_glyphs_at_squares(world_square_glyph_map);
    }

    fn draw_glyphs_at_squares(&mut self, glyph_map: WorldSquareGlyphMap) {
        for (world_square, glyph) in glyph_map {
            self.draw_glyphs_for_square_to_draw_buffer(world_square, glyph);
        }
    }
    fn draw_drawables_at_squares<T: Drawable>(&mut self, drawable_map: HashMap<WorldSquare, T>) {
        for (world_square, drawable) in drawable_map {
            self.draw_drawable_to_draw_buffer(world_square, &drawable);
        }
    }

    pub fn set_solid_floor_color(&mut self, new_color: RGB8) {
        self.floor_color_enum = FloorColorEnum::Solid(new_color)
    }
    pub fn floor_color_at_square(&self, square: WorldSquare) -> RGB8 {
        self.floor_color_enum.color_at(square)
    }

    fn draw_naive_braille_line(&mut self, start_pos: WorldPoint, end_pos: WorldPoint, color: RGB8) {
        let drawables = BrailleDrawable::line(start_pos, end_pos, color);
        self.draw_drawables_at_squares(drawables);
    }

    pub fn set_empty_board_animation(&mut self) {
        self.board_animation = None
    }

    #[deprecated(note = "use more descriptive")]
    pub fn square_is_white(square: WorldSquare) -> bool {
        (square.x + square.y) % 2 == 0
    }
    pub fn big_chess_pattern(square: WorldSquare) -> RGB8 {
        let y = square.y % 6 < 3;
        let x = square.x % 6 < 3;

        if x ^ y {
            BOARD_BLACK
        } else {
            BOARD_WHITE
        }
    }
    fn checkerboard_square_function(square: WorldSquare) -> RGB8 {
        if Graphics::square_is_white(square) {
            BOARD_WHITE
        } else {
            BOARD_BLACK
        }
    }
    #[deprecated(note = "use more descriptive")]
    pub fn off_board_color_at_square(square: WorldSquare) -> RGB8 {
        if !Graphics::square_is_white(square) {
            BOARD_WHITE
        } else {
            BOARD_BLACK
        }
    }
    pub fn get_drawable_for_square_from_draw_buffer(
        &self,
        world_square: WorldSquare,
    ) -> Option<&DrawableEnum> {
        self.draw_buffer.get(&world_square)
    }

    pub fn print_draw_buffer(&self, center: WorldSquare, radius: u32) {
        let l = 2 * radius + 1;
        for row in 0..l {
            let y = center.y + radius as i32 - row as i32;
            let row_string = (0..l)
                .into_iter()
                .map(|column| center.x - radius as i32 + column as i32)
                .map(|x| {
                    if let Some(drawable) = self.draw_buffer.get(&point2(x, y)) {
                        drawable.to_glyphs().to_string()
                    } else {
                        Box::new(TextDrawable::new("**", RED, BLACK, false))
                            .to_glyphs()
                            .to_clean_string()
                    }
                })
                .fold(String::new(), |acc, other| acc + &other)
                + &Glyph::reset_colors();
            println!("{}", row_string);
        }
    }
    pub fn display_headless(&mut self) {
        self.display(&mut None);
    }

    pub fn display(&mut self, optional_writer: &mut Option<Box<dyn Write>>) {
        if optional_writer.is_some() {
            self.screen.update_screen(optional_writer.as_mut().unwrap());
        }
        self.screen.current_screen_state = self.screen.screen_buffer.clone();
    }

    pub fn maybe_drawable_for_rel_square_of_fov(
        &self,
        fov: &FieldOfViewResult,
        rel_square: WorldStep,
    ) -> Option<DrawableEnum> {
        fov.drawable_at_relative_square(
            rel_square,
            Some(&self.draw_buffer),
            self.tint_portals,
            self.render_portals_with_line_of_sight,
        )
    }

    pub fn load_screen_buffer_from_fov(&mut self, field_of_view: FieldOfViewResult) {
        for screen_square in self.screen.all_screen_squares() {
            let world_square = self
                .screen
                .screen_buffer_square_to_world_square(screen_square);

            let relative_world_square = world_square - field_of_view.root_square();
            let maybe_unrotated =
                self.maybe_drawable_for_rel_square_of_fov(&field_of_view, relative_world_square);

            if let Some(unrotated) = maybe_unrotated {
                let rotated: DrawableEnum =
                    unrotated.rotated(-self.screen.rotation().quarter_turns());
                self.screen
                    .draw_glyphs_straight_to_screen_square(rotated.to_glyphs(), screen_square);
            }
        }
    }

    pub fn load_screen_buffer_from_absolute_positions_in_draw_buffer(&mut self) {
        // for character squares on screen
        for buffer_x in 0..self.screen.terminal_width() {
            for buffer_y in 0..self.screen.terminal_height() {
                let screen_buffer_character_square: Point2D<i32, CharacterGridInScreenBufferFrame> =
                    point2(buffer_x, buffer_y);
                if !self
                    .screen
                    .screen_buffer_character_square_is_left_glyph_of_screen_square(
                        screen_buffer_character_square,
                    )
                {
                    continue;
                }
                let world_square = self
                    .screen
                    .screen_buffer_character_square_to_world_square(screen_buffer_character_square);
                if let Some(drawable) = self.draw_buffer.get(&world_square) {
                    let screen_buffer_square = self
                        .screen
                        .screen_buffer_character_square_to_screen_buffer_square(
                            screen_buffer_character_square,
                        );
                    self.screen.draw_glyphs_straight_to_screen_square(
                        drawable.to_glyphs(),
                        screen_buffer_square,
                    );
                }
            }
        }

        // if associated world square is in draw buffer, put it on screen
    }

    pub fn draw_string_to_draw_buffer(&mut self, world_pos: WorldSquare, the_string: &str) {
        let glyphs = the_string
            .chars()
            .map(|c: char| Glyph::fg_only(c, RED))
            .collect_vec();
        let start_char_square = world_square_to_left_world_character_square(world_pos);
        let glyphs_to_draw: WorldCharacterSquareGlyphMap = glyphs
            .into_iter()
            .enumerate()
            .map(|(i, glyph): (usize, Glyph)| {
                (start_char_square + STEP_RIGHT.cast_unit() * i as i32, glyph)
            })
            .collect();

        self.draw_glyphs(glyphs_to_draw);
    }

    pub fn draw_player(&mut self, world_pos: WorldSquare, faced_direction: KingWorldStep) {
        let drawable = ArrowDrawable::new(faced_direction.into(), THICK_ARROWS, PLAYER_COLOR);
        self.draw_drawable_to_draw_buffer(world_pos, &drawable);
    }

    pub fn draw_above_square<T: Drawable + Debug>(
        &mut self,
        drawable: &T,
        world_square: WorldSquare,
    ) {
        let to_draw = if let Some(below) = self.draw_buffer.get(&world_square) {
            drawable.drawn_over(below)
        } else {
            drawable.clone().to_enum()
        };
        self.draw_buffer.insert(world_square, to_draw.to_enum());
    }
    #[deprecated(
        note = "Graphics should not know about glyphs, use draw_drawable_to_draw_buffer instead"
    )]
    pub fn draw_glyphs_for_square_to_draw_buffer(
        &mut self,
        world_square: WorldSquare,
        glyphs: DoubleGlyph,
    ) {
        self.draw_above_square(&TextDrawable::from_glyphs(glyphs), world_square);
    }

    pub fn draw_drawable_to_draw_buffer<T: Drawable + Debug>(
        &mut self,
        world_square: WorldSquare,
        drawable: &T,
    ) {
        self.draw_above_square(drawable, world_square);
    }

    pub fn draw_piece_with_color(
        &mut self,
        square: WorldSquare,
        piece_type: PieceType,
        color: RGB8,
    ) {
        let mut piece_glyphs = Piece::glyphs_for_type(piece_type);
        piece_glyphs
            .iter_mut()
            .for_each(|g: &mut Glyph| g.fg_color = color);

        self.draw_glyphs_for_square_to_draw_buffer(square, piece_glyphs);
    }

    pub fn draw_upgrade(&mut self, square: WorldSquare, upgrade: Upgrade) {
        self.draw_glyphs_for_square_to_draw_buffer(square, Self::glyphs_for_upgrade(upgrade));
    }
    fn glyphs_for_upgrade(upgrade: Upgrade) -> DoubleGlyph {
        [Glyph::fg_only('*', CYAN), Glyph::transparent_glyph()]
    }
    pub fn draw_arrow(&mut self, square: WorldSquare, dir: KingWorldStep) {
        self.draw_glyphs_for_square_to_draw_buffer(square, Glyph::glyphs_for_flying_arrow(dir))
    }

    pub fn draw_same_glyphs_at_squares(&mut self, glyphs: DoubleGlyph, square_set: &SquareSet) {
        square_set
            .into_iter()
            .for_each(|&square| self.draw_glyphs_for_square_to_draw_buffer(square, glyphs));
    }
    pub fn draw_move_marker_squares(
        &mut self,
        move_squares: SquareSet,
        capture_squares: SquareSet,
        move_squares_ignoring_pieces: SquareSet,
        capture_squares_ignoring_pieces: SquareSet,
    ) {
        let move_and_capture_squares: SquareSet = move_squares
            .intersection(&capture_squares)
            .copied()
            .collect();
        let move_or_capture_squares: SquareSet =
            move_squares.union(&capture_squares).copied().collect();
        let move_only_squares: SquareSet =
            move_squares.difference(&capture_squares).copied().collect();
        let capture_only_squares: SquareSet =
            capture_squares.difference(&move_squares).copied().collect();
        let move_and_capture_squares_ignoring_pieces: SquareSet = move_squares_ignoring_pieces
            .intersection(&capture_squares_ignoring_pieces)
            .copied()
            .collect();
        // TODO: conditional move only and conditional capture only, when it becomes applicable.
        let conditional_move_and_capture_squares: SquareSet =
            move_and_capture_squares_ignoring_pieces
                .difference(&move_or_capture_squares)
                .copied()
                .collect();

        self.draw_same_glyphs_at_squares(danger_square_glyphs(), &move_and_capture_squares);
        self.draw_same_glyphs_at_squares(
            tricky_danger_square_glyphs(),
            &conditional_move_and_capture_squares,
        );
        self.draw_same_glyphs_at_squares(move_only_square_glyphs(), &move_only_squares);
        self.draw_same_glyphs_at_squares(capture_only_square_glyphs(), &capture_only_squares);
    }

    pub fn draw_death_cube(&mut self, death_cube: DeathCube) {
        let color = self.technicolor_at_time(Instant::now());
        self.draw_floating_square(death_cube.position(), color);
    }
    pub fn draw_floating_hunter_drone(
        &mut self,
        drone: &FloatingHunterDrone,
        sight_line_segments: &Vec<WorldLine>,
    ) {
        for line in sight_line_segments {
            self.draw_naive_braille_line(line.p1, line.p2, SIGHT_LINE_SEEKING_COLOR);
        }
        self.draw_floating_square(drone.position(), HUNTER_DRONE_COLOR);
    }

    fn draw_floating_square(&mut self, pos: WorldPoint, color: RGB8) {
        let drawables = OffsetSquareDrawable::drawables_for_floating_square_at_point(pos, color);
        drawables
            .iter()
            .for_each(|(&square, drawable)| self.draw_drawable_to_draw_buffer(square, drawable));
    }

    pub fn technicolor_at_time(&self, time: Instant) -> RGB8 {
        let duration_from_start = time.duration_since(self.start_time);
        let period = Duration::from_secs_f32(1.0);
        let period_fraction =
            (duration_from_start.as_secs_f32() % period.as_secs_f32()) / period.as_secs_f32();
        hue_to_rgb(period_fraction * 360.0)
    }

    pub fn draw_blocks(&mut self, block_squares: &SquareSet) {
        self.draw_same_glyphs_at_squares(Glyph::block_glyphs(), block_squares);
    }
    pub fn draw_floor_push_arrows(
        &mut self,
        floor_push_arrows: &HashMap<WorldSquare, OrthogonalWorldStep>,
    ) {
        floor_push_arrows.iter().for_each(|(&square, &dir)| {
            self.draw_drawable_to_draw_buffer(
                square,
                &ArrowDrawable::new(dir.into(), "⯬⯮⯭⯯", DARK_GREY),
            )
        })
    }
    pub fn draw_conveyor_belts(
        &mut self,
        conveyor_belts: &HashMap<WorldSquare, OrthogonalWorldStep>,
        global_phase: f32,
    ) {
        conveyor_belts.iter().for_each(|(&square, &dir)| {
            let phase = global_phase + if square_is_odd(square) { 0.5 } else { 0.0 };
            self.draw_drawable_to_draw_buffer(square, &ConveyorBeltDrawable::new(dir, phase))
        })
    }

    pub fn add_simple_laser(&mut self, start: WorldPoint, end: WorldPoint) {
        self.active_animations
            .push(AnimationEnum::SimpleLaser(SimpleLaserAnimation::new(
                start, end,
            )));
    }
    pub fn add_floaty_laser(&mut self, start: WorldPoint, end: WorldPoint) {
        self.active_animations
            .push(AnimationEnum::FloatyLaser(FloatyLaserAnimation::new(
                start, end,
            )));
    }

    pub fn do_smite_animation(&mut self, square: WorldSquare) {
        self.active_animations
            .push(AnimationEnum::Smite(SmiteAnimation::new(square)));
    }

    pub fn start_burst_explosion(&mut self, point: WorldPoint) {
        self.active_animations
            .push(AnimationEnum::BurstExplosion(BurstExplosionAnimation::new(
                point,
            )));
    }
    pub fn start_circle_attack_animation(&mut self, square: WorldSquare, radius: f32) {
        self.active_animations
            .push(AnimationEnum::CircleAttack(CircleAttackAnimation::new(
                square.to_f32(),
                radius,
            )));
        self.board_animation = Some(AnimationEnum::RadialShockwave(RadialShockwave::new(
            square,
            self.floor_color_enum.clone(),
        )));
    }
    pub fn start_spear_attack_animation(
        &mut self,
        start_square: WorldSquare,
        direction: KingWorldStep,
        range: u32,
    ) {
        self.active_animations
            .push(AnimationEnum::SpearAttack(SpearAttackAnimation::new(
                start_square,
                direction,
                range,
            )));
    }

    pub fn start_piece_death_animation_at(&mut self, square: WorldSquare) {
        self.active_animations
            .push(AnimationEnum::PieceDeath(PieceDeathAnimation::new(square)));
    }

    pub fn do_blink_animation(&mut self, start_square: WorldSquare, end_square: WorldSquare) {
        self.active_animations
            .push(AnimationEnum::Blink(BlinkAnimation::new(
                start_square,
                end_square,
            )));
    }

    pub fn add_selector(&mut self, square: WorldSquare) {
        self.active_animations
            .push(AnimationEnum::Selector(SelectorAnimation::new(square)));
    }
    pub fn draw_paths(&mut self, paths: Vec<SquareList>) {
        let mut path_squares = HashSet::<WorldSquare>::new();
        paths.iter().flatten().for_each(|&square| {
            path_squares.insert(square);
        });
        self.draw_same_glyphs_at_squares(path_glyphs(), &path_squares);
    }

    pub fn start_recoil_animation(&mut self, board_size: BoardSize, shot_direction: WorldStep) {
        self.board_animation = Some(AnimationEnum::RecoilingBoard(RecoilingBoardAnimation::new(
            board_size,
            shot_direction,
            self.floor_color_enum.clone(),
        )));
    }

    pub fn draw_static_board(&mut self, board_size: BoardSize) {
        squares_on_board(board_size).into_iter().for_each(|square| {
            let color = self.floor_color_enum.color_at(square);
            let drawable = SolidColorDrawable::new(color);
            self.draw_above_square(&drawable, square)
        })
    }

    pub fn draw_board_animation(&mut self, time: Instant) {
        if let Some(board_animation) = &self.board_animation {
            self.draw_animation(&board_animation.clone(), time);
        }
    }

    fn draw_animation(&mut self, animation: &AnimationEnum, time: Instant) {
        let glyph_map = animation.glyphs_at_time(time);
        self.draw_glyphs(glyph_map);
    }

    fn draw_animations(&mut self, animations: AnimationList, time: Instant) {
        animations
            .into_iter()
            .for_each(|animation| self.draw_animation(&animation, time))
    }
    pub fn number_to_color(i: u32) -> RGB8 {
        let in_order = vec![GREY, RED, BLUE, GREEN, YELLOW, CYAN, MAGENTA];
        in_order[i as usize % in_order.len()]
    }

    pub fn draw_non_board_animations(&mut self, time: Instant) {
        let mut glyphs_to_draw = vec![];
        for animation in &self.active_animations {
            glyphs_to_draw.push(animation.glyphs_at_time(time));
        }
        for selector in &self.selectors {
            glyphs_to_draw.push(selector.glyphs_at_time(time))
        }

        for glyph_map in glyphs_to_draw {
            self.draw_glyphs(glyph_map);
        }
    }

    pub fn remove_finished_animations(&mut self, time: Instant) {
        if let Some(board_animation) = &mut self.board_animation {
            if board_animation.finished_at_time(time) {
                self.board_animation = None;
            }
        }
        self.active_animations
            .extract_if(.., |x| x.finished_at_time(time));
        self.selectors.extract_if(.., |x| x.finished_at_time(time));
    }

    pub fn count_buffered_braille_dots_in_rect(&self, rect: WorldSquareRect) -> u32 {
        let mut count: u32 = 0;
        for x in rect.min.x..=rect.max.x {
            for y in rect.min.y..=rect.max.y {
                let square = WorldSquare::new(x, y);
                let maybe_glyphs = self
                    .get_drawable_for_square_from_draw_buffer(square)
                    .map(|d: &DrawableEnum| d.to_glyphs());
                if let Some(glyphs) = maybe_glyphs {
                    for glyph in glyphs {
                        let character = glyph.character;
                        count += count_braille_dots(character);
                    }
                }
            }
        }
        count
    }

    pub fn select_squares(&mut self, squares: Vec<WorldSquare>) {
        self.selectors = squares
            .into_iter()
            .map(|square| SelectorAnimation::new(square))
            .collect();
    }
}

pub type WorldSquareDrawableMap = HashMap<WorldSquare, DrawableEnum>;

pub fn danger_square_glyphs() -> DoubleGlyph {
    MOVE_AND_CAPTURE_SQUARE_CHARS.map(|c| Glyph::fg_only(c, DANGER_SQUARE_COLOR))
}
pub fn tricky_danger_square_glyphs() -> DoubleGlyph {
    CONDITIONAL_MOVE_AND_CAPTURE_SQUARE_CHARS.map(|c| Glyph::fg_only(c, DANGER_SQUARE_COLOR))
}
pub fn move_only_square_glyphs() -> DoubleGlyph {
    MOVE_ONLY_SQUARE_CHARS.map(|c| Glyph::fg_only(c, DANGER_SQUARE_COLOR))
}
pub fn capture_only_square_glyphs() -> DoubleGlyph {
    CAPTURE_ONLY_SQUARE_CHARS.map(|c| Glyph::fg_only(c, DANGER_SQUARE_COLOR))
}
pub fn path_glyphs() -> DoubleGlyph {
    KING_PATH_GLYPHS.map(|c| Glyph::fg_only(c, PATH_COLOR))
}

#[cfg(test)]
mod tests {
    use pretty_assertions::{assert_eq, assert_ne};

    use crate::fov_stuff::portal_aware_field_of_view_from_square;
    use crate::piece::PieceType::TurningPawn;
    use crate::{LEFT_I, RIGHT_I};
    use utility::*;

    use super::*;

    fn set_up_graphics() -> Graphics {
        Graphics::new(40, 20, Instant::now())
    }

    fn set_up_graphics_with_nxn_world_squares(board_length: u16) -> Graphics {
        Graphics::new(board_length * 2, board_length, Instant::now())
    }

    #[test]
    fn test_draw_diagonal_braille_line() {
        let mut g = set_up_graphics();
        let line_start = WorldSquare::new(2, 2);
        let line_end = WorldSquare::new(7, 7);

        g.draw_naive_braille_line(line_start.to_f32(), line_end.to_f32(), RED);

        let test_square = WorldSquare::new(4, 4);

        let [glyph_left, glyph_right] = g
            .get_drawable_for_square_from_draw_buffer(test_square)
            .unwrap()
            .to_glyphs();
        //g.print_output_buffer();

        assert_eq!(glyph_left.fg_color, RED);
    }

    #[test]
    fn test_single_braille_point() {
        let mut g = set_up_graphics();
        let test_square = point2(5, 5);
        let diag = vec2(1, 1);
        let test_rectangle = WorldSquareRect::new(test_square - diag, test_square + diag);
        let test_point = test_square.to_f32();

        g.draw_braille_point(test_point, RED);
        assert_eq!(g.count_buffered_braille_dots_in_rect(test_rectangle), 1)
    }

    #[test]
    fn test_laser_has_transparent_background() {
        let mut g = set_up_graphics();
        g.load_screen_buffer_from_absolute_positions_in_draw_buffer();
        let glyph1 = g.screen.get_screen_glyphs_at_world_square(point2(5, 0))[0];
        g.add_simple_laser(point2(0.0, 0.0), point2(10.0, 0.0));
        g.draw_non_board_animations(Instant::now());
        g.load_screen_buffer_from_absolute_positions_in_draw_buffer();
        //g.print_output_buffer();
        let glyph2 = g.screen.get_screen_glyphs_at_world_square(point2(5, 0))[0];
        assert_eq!(glyph1.bg_color, glyph2.bg_color);
        assert_ne!(glyph1.fg_color, glyph2.fg_color);
        //g.print_output_buffer();
    }

    #[test]
    fn test_draw_on_far_right_square_in_odd_width_terminal() {
        let mut g = Graphics::new(41, 20, Instant::now());
        g.add_simple_laser(point2(0.0, 0.0), point2(50.0, 0.0));
        g.draw_non_board_animations(Instant::now());
    }

    #[test]
    fn test_overlapped_glyphs_change_background_color() {
        let mut g = set_up_graphics_with_nxn_world_squares(6);
        let test_square = WorldSquare::new(3, 4);
        let maybe_glyphs_at_start = g
            .get_drawable_for_square_from_draw_buffer(test_square)
            .map(|d| d.to_glyphs());
        assert!(maybe_glyphs_at_start.is_none());

        g.draw_piece_with_color(
            test_square,
            PieceType::OmniDirectionalPawn,
            ENEMY_PIECE_COLOR,
        );
        let line_color = GREEN;
        assert_ne!(line_color, ENEMY_PIECE_COLOR);
        g.draw_naive_braille_line(
            test_square.to_f32() + vec2(-1.0, 0.0),
            test_square.to_f32() + vec2(1.0, 0.0),
            line_color,
        );
        let glyphs_at_end = g
            .get_drawable_for_square_from_draw_buffer(test_square)
            .unwrap()
            .to_glyphs();

        assert_eq!(glyphs_at_end[0].bg_color, ENEMY_PIECE_COLOR);
        //assert_eq!(glyphs_at_end[1].bg_color, ENEMY_PIECE_COLOR);

        assert_eq!(glyphs_at_end[0].fg_color, line_color);
        assert_eq!(glyphs_at_end[1].fg_color, line_color);
    }

    #[test]
    fn test_draw_piece_on_board() {
        let mut g = set_up_graphics_with_nxn_world_squares(1);
        let the_square = WorldSquare::new(0, 0);
        g.set_empty_board_animation();
        g.draw_static_board(BoardSize::new(1, 1));
        //g.print_output_buffer();
        g.draw_piece_with_color(the_square, TurningPawn, WHITE);
        //g.print_output_buffer();
        let drawable = g
            .get_drawable_for_square_from_draw_buffer(the_square)
            .unwrap();
        let drawn_glyphs = drawable.to_glyphs();
        let correct_bg = g.floor_color_enum.color_at(the_square);
        g.print_draw_buffer(the_square, 0);
        assert_eq!(drawn_glyphs[0].character, '♟');
        assert_eq!(drawn_glyphs[0].fg_color, ENEMY_PIECE_COLOR);
        assert_eq!(drawn_glyphs[0].bg_color, correct_bg);
        assert_eq!(drawn_glyphs[0].bg_transparent, false);
        assert_eq!(drawn_glyphs[1].character, ' ');
        assert_eq!(drawn_glyphs[1].bg_color, correct_bg);
        assert_eq!(drawn_glyphs[1].bg_transparent, false);
    }

    #[test]
    fn test_draw_buffer_to_screen_through_field_of_view() {
        let mut g = set_up_graphics_with_nxn_world_squares(5);
        let world_square = WorldSquare::new(1, 2);
        let color = CYAN;
        g.draw_glyphs_for_square_to_draw_buffer(world_square, DoubleGlyph::fg_only("# ", color));
        g.draw_glyphs_for_square_to_draw_buffer(point2(0, 0), DoubleGlyph::fg_only("# ", GREEN));
        let fov = portal_aware_field_of_view_from_square(
            point2(0, 0),
            5,
            &Default::default(),
            &Default::default(),
        );
        g.screen
            .set_screen_center_by_world_square(fov.root_square());
        g.load_screen_buffer_from_fov(fov);
        let screen_buffer_square = g.screen.world_square_to_screen_buffer_square(world_square);
        // g.print_draw_buffer(point2(0, 0), 3);
        // g.screen.print_screen_buffer();
        assert_eq!(screen_buffer_square, point2(3, 0));
        assert_eq!(
            g.screen.get_glyphs_at_screen_square(screen_buffer_square)[0].fg_color,
            color
        );
    }
}
