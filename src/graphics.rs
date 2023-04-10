pub mod screen;

use std::any::Any;
use std::borrow::Borrow;
use std::cmp::min;
use std::collections::{HashMap, HashSet};
use std::f32::consts::{PI, TAU};
use std::io::Write;
use std::mem::swap;
use std::ptr::hash;
use std::time::{Duration, Instant};

use euclid::*;
use itertools::Itertools;
use line_drawing::Point;
use rand::{Rng, SeedableRng};
use rgb::RGB8;
use termion::input::MouseTerminal;
use termion::raw::RawTerminal;
use termion::terminal_size;

use glyph::glyph_constants::*;

use crate::animations::blink_animation::BlinkAnimation;
use crate::animations::burst_explosion_animation::BurstExplosionAnimation;
use crate::animations::circle_attack_animation::CircleAttackAnimation;
use crate::animations::floaty_laser::FloatyLaser;
use crate::animations::piece_death_animation::PieceDeathAnimation;
use crate::animations::radial_shockwave::RadialShockwave;
use crate::animations::recoiling_board::RecoilingBoard;
use crate::animations::selector_animation::SelectorAnimation;
use crate::animations::simple_laser::SimpleLaser;
use crate::animations::smite_from_above::SmiteFromAbove;
use crate::animations::spear_attack_animation::SpearAttackAnimation;
use crate::animations::static_board::StaticBoard;
use crate::animations::*;
use crate::fov_stuff::FovResult;
use crate::game::DeathCube;
use crate::glyph::braille::count_braille_dots;
use crate::glyph::floating_square::characters_for_full_square_at_point;
use crate::glyph::{DoubleGlyph, Glyph};
use crate::graphics::screen::Screen;
use crate::num::ToPrimitive;
use crate::piece::{Piece, Upgrade};
use crate::utility::coordinate_frame_conversions::*;
use crate::utility::{
    hue_to_rgb, is_world_character_square_left_square_of_world_square, STEP_RIGHT,
};
use crate::{
    get_by_point, glyph, pair_up_glyph_map, point_to_string, print_glyph_map, DoubleGlyphFunctions,
    Game, IPoint, PieceType, RIGHT_I,
};

pub struct Graphics {
    screen: Screen,
    draw_buffer: HashMap<WorldCharacterSquare, Glyph>,
    active_animations: Vec<Box<dyn Animation>>,
    selectors: Vec<SelectorAnimation>,
    board_animation: Option<Box<dyn BoardAnimation>>,
    start_time: Instant,
}

impl Graphics {
    pub(crate) fn new(terminal_width: u16, terminal_height: u16, start_time: Instant) -> Graphics {
        Graphics {
            screen: Screen {
                screen_buffer_origin: point2(0, terminal_height as i32 - 1),
                screen_buffer: vec![
                    vec![Glyph::from_char(' '); terminal_height as usize];
                    terminal_width as usize
                ],
                current_screen_state: vec![
                    vec![Glyph::from_char('x'); terminal_height as usize];
                    terminal_width as usize
                ],
                terminal_width,
                terminal_height,
            },
            draw_buffer: HashMap::default(),
            active_animations: vec![],
            selectors: vec![],
            board_animation: None,
            start_time,
        }
    }

    pub fn start_time(&self) -> Instant {
        self.start_time
    }

    fn count_braille_dots_in_square(&self, square: WorldSquare) -> u32 {
        return if self.screen.square_is_on_screen(square) {
            count_braille_dots(
                self.screen
                    .get_screen_buffered_glyph(
                        self.screen.world_square_to_left_buffer_square(square),
                    )
                    .character,
            )
        } else {
            0
        };
    }

    fn draw_braille_point(&mut self, pos: WorldPoint, color: RGB8) {
        self.draw_braille_line(pos, pos, color);
    }

    fn draw_glyphs(&mut self, glyph_map: WorldCharacterSquareGlyphMap) {
        let world_square_glyph_map = pair_up_glyph_map(glyph_map);
        self.draw_glyphs_at_squares(world_square_glyph_map);
    }

    fn draw_glyphs_at_squares(&mut self, glyph_map: WorldSquareGlyphMap) {
        for (world_square, glyph) in glyph_map {
            self.draw_glyphs_for_square_to_draw_buffer(world_square, glyph);
        }
    }

    pub fn draw_string_below_board(&mut self, string: String) {
        let left_of_screen_under_board =
            self.screen.world_square_to_left_screen_square(point2(0, 0)) + vec2(0, -1);
        self.screen
            .draw_string_to_screen(left_of_screen_under_board, &string);
    }

    fn draw_braille_line(&mut self, start_pos: WorldPoint, end_pos: WorldPoint, color: RGB8) {
        let line_glyphs = Glyph::get_glyphs_for_colored_braille_line(start_pos, end_pos, color);
        self.draw_glyphs(line_glyphs);
    }

    pub fn fill_output_buffer_with_black(&mut self) {
        self.fill_output_buffer_with_solid_color(BLACK);
    }
    pub fn fill_output_buffer_with_solid_color(&mut self, color: RGB8) {
        for x in 0..self.screen.terminal_width as usize {
            for y in 0..self.screen.terminal_height as usize {
                self.screen.screen_buffer[x][y] = Glyph::new(' ', WHITE, color);
            }
        }
    }

    pub fn set_empty_board_animation(&mut self, board_size: BoardSize) {
        self.board_animation = Some(Box::new(StaticBoard::new(board_size)));
    }

    pub fn square_is_white(square: WorldSquare) -> bool {
        (square.x + square.y) % 2 == 0
    }
    pub fn board_color_at_square(square: WorldSquare) -> RGB8 {
        if Graphics::square_is_white(square) {
            BOARD_WHITE
        } else {
            BOARD_BLACK
        }
    }
    pub fn off_board_color_at_square(square: WorldSquare) -> RGB8 {
        if !Graphics::square_is_white(square) {
            BOARD_WHITE
        } else {
            BOARD_BLACK
        }
    }
    pub fn get_glyphs_for_square_from_draw_buffer(&self, world_square: WorldSquare) -> DoubleGlyph {
        let world_character_squares = world_square_to_both_world_character_squares(world_square);
        world_character_squares.map(|char_square| {
            *self
                .draw_buffer
                .get(&char_square)
                .unwrap_or(&Glyph::default_background())
        })
    }

    pub fn print_draw_buffer(&self, center: WorldCharacterSquare, radius: u32) {
        let l = 2 * radius + 1;
        for row in 0..l {
            let y = center.y + radius as i32 - row as i32;
            let row_string = (0..l)
                .into_iter()
                .map(|column| center.x - radius as i32 + column as i32)
                .map(|x| {
                    self.draw_buffer
                        .get(&point2(x, y))
                        .unwrap_or(&Glyph::fg_only('*', RED))
                        .to_string()
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
            self.update_screen(optional_writer.as_mut().unwrap());
        }
        self.screen.current_screen_state = self.screen.screen_buffer.clone();
    }

    pub fn load_screen_buffer_from_fov(&mut self, field_of_view: FovResult) {
        for buffer_x in 0..self.screen.terminal_width() {
            for buffer_y in 0..self.screen.terminal_height() {
                let buffer_character_square: ScreenBufferCharacterSquare =
                    point2(buffer_x, buffer_y);

                let world_character_square = self
                    .screen
                    .buffer_square_to_world_character_square(buffer_character_square);
                let character_square_position_in_world_square =
                    if is_world_character_square_left_square_of_world_square(world_character_square)
                    {
                        0
                    } else {
                        1
                    };

                let world_square = world_character_square_to_world_square(world_character_square);

                let relative_world_square = world_square - field_of_view.root_square();
                let visibility = field_of_view.visibility_of_relative_square(relative_world_square);

                if visibility.is_visible() {
                    let absolute_world_square_seen: WorldSquare = field_of_view
                        .relative_to_absolute(relative_world_square)
                        .unwrap();
                    let absolute_world_character_square_seen: WorldCharacterSquare =
                        world_square_to_world_character_squares(
                            absolute_world_square_seen,
                            character_square_position_in_world_square,
                        );

                    let mut glyph: Glyph = *self
                        .draw_buffer
                        .get(&absolute_world_character_square_seen)
                        .unwrap_or(&Glyph::default_background());

                    if let Some(partial) = visibility.partial_visibility() {
                        let shadow_glyph =
                            partial.to_glyphs()[character_square_position_in_world_square];
                        glyph = shadow_glyph.drawn_over(glyph);
                    }
                    self.screen
                        .draw_glyph_straight_to_screen_buffer(buffer_character_square, glyph);
                }
            }
        }
    }

    pub fn load_screen_buffer_from_absolute_positions_in_draw_buffer(&mut self) {
        // for character squares on screen
        for buffer_x in 0..self.screen.terminal_width() {
            for buffer_y in 0..self.screen.terminal_height() {
                let buffer_square: Point2D<i32, CharacterGridInScreenBufferFrame> =
                    point2(buffer_x, buffer_y);
                let world_character_square = self
                    .screen
                    .buffer_square_to_world_character_square(buffer_square);
                if let Some(&glyph) = self.draw_buffer.get(&world_character_square) {
                    self.screen
                        .draw_glyph_straight_to_screen_buffer(buffer_square, glyph);
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

    pub fn draw_player(&mut self, world_pos: WorldSquare, faced_direction: WorldStep) {
        let mut player_glyphs = Glyph::get_glyphs_for_player(faced_direction);
        let square_color = Graphics::board_color_at_square(world_pos);
        player_glyphs[0].bg_color = square_color;
        player_glyphs[1].bg_color = square_color;
        self.draw_glyphs_for_square_to_draw_buffer(world_pos, player_glyphs);
    }

    pub fn draw_glyphs_for_square_to_draw_buffer(
        &mut self,
        world_square: WorldSquare,
        glyphs: DoubleGlyph,
    ) {
        let world_character_squares = world_square_to_both_world_character_squares(world_square);
        let background_glyphs = self.get_glyphs_for_square_from_draw_buffer(world_square);

        let combined_glyphs = glyphs.drawn_over(background_glyphs);

        (0..2).for_each(|i| {
            self.draw_buffer
                .insert(world_character_squares[i], combined_glyphs[i]);
        });
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
        self.draw_glyphs_for_square_to_draw_buffer(square, Glyph::glyphs_for_upgrade(upgrade));
    }
    pub fn draw_arrow(&mut self, square: WorldSquare, dir: WorldStep) {
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

        self.draw_same_glyphs_at_squares(Glyph::danger_square_glyphs(), &move_and_capture_squares);
        self.draw_same_glyphs_at_squares(
            Glyph::tricky_danger_square_glyphs(),
            &conditional_move_and_capture_squares,
        );
        self.draw_same_glyphs_at_squares(Glyph::move_only_square_glyphs(), &move_only_squares);
        self.draw_same_glyphs_at_squares(
            Glyph::capture_only_square_glyphs(),
            &capture_only_squares,
        );
    }

    pub fn draw_death_cube(&mut self, death_cube: DeathCube) {
        let character_grid_point = world_point_to_world_character_point(death_cube.position);
        let characters_map = characters_for_full_square_at_point(character_grid_point);
        let color = self.technicolor_at_time(Instant::now());
        let glyphs = characters_map
            .into_iter()
            .map(|(char_square, char)| (char_square, Glyph::fg_only(char, color)))
            .collect();
        self.draw_glyphs(glyphs);
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

    #[deprecated(note = "draw the fields of view directly instead")]
    pub fn draw_field_of_view_mask(&mut self, fov_mask: FovResult) {
        let glyph_mask_for_partially_visible_squares: WorldSquareGlyphMap =
            fov_mask.partially_visible_squares_as_glyph_mask();
        let squares_on_screen = self.screen.all_squares_on_screen();
        let squares_on_screen_but_out_of_sight = squares_on_screen
            .difference(
                &fov_mask
                    .at_least_partially_visible_relative_squares_main_view_only()
                    .iter()
                    .map(|step| fov_mask.root_square() + *step)
                    .collect(),
            )
            .copied()
            .collect();
        self.draw_glyphs_at_squares(glyph_mask_for_partially_visible_squares);
        self.draw_same_glyphs_at_squares(
            Glyph::out_of_sight_glyphs(),
            &squares_on_screen_but_out_of_sight,
        );
    }

    pub fn add_simple_laser(&mut self, start: WorldPoint, end: WorldPoint) {
        self.active_animations
            .push(Box::new(SimpleLaser::new(start, end)));
    }
    pub fn add_floaty_laser(&mut self, start: WorldPoint, end: WorldPoint) {
        self.active_animations
            .push(Box::new(FloatyLaser::new(start, end)));
    }

    pub fn do_smite_animation(&mut self, square: WorldSquare) {
        self.active_animations
            .push(Box::new(SmiteFromAbove::new(square)));
    }

    pub fn start_burst_explosion(&mut self, point: WorldPoint) {
        self.active_animations
            .push(Box::new(BurstExplosionAnimation::new(point)));
    }
    pub fn start_circle_attack_animation(&mut self, square: WorldSquare, radius: f32) {
        self.active_animations
            .push(Box::new(CircleAttackAnimation::new(
                square.to_f32(),
                radius,
            )));
        self.active_animations
            .push(Box::new(RadialShockwave::new(square)));
    }
    pub fn start_spear_attack_animation(
        &mut self,
        start_square: WorldSquare,
        direction: WorldStep,
        range: u32,
    ) {
        self.active_animations
            .push(Box::new(SpearAttackAnimation::new(
                start_square,
                direction,
                range,
            )));
    }

    pub fn start_piece_death_animation_at(&mut self, square: WorldSquare) {
        self.active_animations
            .push(Box::new(PieceDeathAnimation::new(square)));
    }

    pub fn do_blink_animation(&mut self, start_square: WorldSquare, end_square: WorldSquare) {
        self.active_animations
            .push(Box::new(BlinkAnimation::new(start_square, end_square)));
    }

    pub fn add_selector(&mut self, square: WorldSquare) {
        self.active_animations
            .push(Box::new(SelectorAnimation::new(square)));
    }
    pub fn draw_paths(&mut self, paths: Vec<SquareList>) {
        let mut path_squares = HashSet::<WorldSquare>::new();
        paths.iter().flatten().for_each(|&square| {
            path_squares.insert(square);
        });
        self.draw_same_glyphs_at_squares(Glyph::path_glyphs(), &path_squares);
    }

    pub fn start_recoil_animation(&mut self, board_size: BoardSize, shot_direction: WorldStep) {
        self.board_animation = Some(Box::new(RecoilingBoard::new(board_size, shot_direction)));
    }

    pub fn draw_board_animation(&mut self, time: Instant) {
        if let Some(board_animation) = &self.board_animation {
            self.draw_animation(board_animation.clone(), time);
        }
    }

    fn draw_animation(&mut self, animation: AnimationObject, time: Instant) {
        self.draw_animations(vec![animation], time)
    }

    fn draw_animations(&mut self, animations: AnimationList, time: Instant) {
        animations
            .into_iter()
            .map(|animation| animation.glyphs_at_time(time))
            .for_each(|glyph_map| self.draw_glyphs(glyph_map));
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
                self.board_animation = Some(board_animation.next_animation())
            }
        }
        self.active_animations
            .drain_filter(|x| x.finished_at_time(time));
        self.selectors.drain_filter(|x| x.finished_at_time(time));
    }

    pub fn update_screen(&mut self, writer: &mut Box<dyn Write>) {
        // Now update the graphics where applicable
        for buffer_x in 0..self.screen.terminal_width() {
            for buffer_y in 0..self.screen.terminal_height() {
                let buffer_pos: Point2D<i32, CharacterGridInScreenBufferFrame> =
                    point2(buffer_x, buffer_y);
                if self.screen.screen_buffer[buffer_pos.x as usize][buffer_pos.y as usize]
                    != self.screen.current_screen_state[buffer_pos.x as usize]
                        [buffer_pos.y as usize]
                {
                    let screen_pos: Point2D<i32, CharacterGridInScreenFrame> = self
                        .screen
                        .screen_buffer_square_to_screen_square(buffer_pos);
                    write!(
                        writer,
                        "{}",
                        termion::cursor::Goto(screen_pos.x as u16, screen_pos.y as u16)
                    )
                    .unwrap();
                    write!(
                        writer,
                        "{}",
                        self.screen.screen_buffer[buffer_pos.x as usize][buffer_pos.y as usize]
                            .to_string()
                    )
                    .unwrap();
                }
            }
        }
        //write!(
        //stdout,
        //"{}{}",
        //termion::cursor::Goto(1, 1),
        //self.fps() as i32
        //)
        //.unwrap();
        writer.flush().unwrap();
    }

    pub fn count_buffered_braille_dots_in_rect(&self, rect: WorldSquareRect) -> u32 {
        let mut count: u32 = 0;
        for x in rect.min.x..=rect.max.x {
            for y in rect.min.y..=rect.max.y {
                let square = WorldSquare::new(x, y);
                let glyphs = self.get_glyphs_for_square_from_draw_buffer(square);
                for glyph in glyphs {
                    let character = glyph.character;
                    count += count_braille_dots(character);
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

#[cfg(test)]
mod tests {
    use pretty_assertions::{assert_eq, assert_ne};

    use crate::fov_stuff::field_of_view_from_square;
    use crate::piece::PieceType::TurningPawn;
    use crate::utility::*;
    use crate::{LEFT_I, RIGHT_I};

    use super::*;

    fn set_up_graphics() -> Graphics {
        Graphics::new(40, 20, Instant::now())
    }

    fn set_up_graphics_with_nxn_world_squares(board_length: u16) -> Graphics {
        Graphics::new(board_length * 2, board_length, Instant::now())
    }

    #[test]
    fn test_one_world_square_is_two_characters() {
        let g = set_up_graphics();

        let world_pos = point2(5, 5); // arbitrary
        let screen_pos1 = g
            .screen
            .world_square_to_left_screen_square(world_pos + RIGHT_I.cast_unit());
        let screen_pos2 =
            g.screen.world_square_to_left_screen_square(world_pos) + RIGHT_I.cast_unit() * 2;
        assert_eq!(screen_pos1, screen_pos2);
    }

    #[test]
    fn test_draw_diagonal_braille_line() {
        let mut g = set_up_graphics();
        let line_start = WorldSquare::new(2, 2);
        let line_end = WorldSquare::new(7, 7);

        g.draw_braille_line(line_start.to_f32(), line_end.to_f32(), RED);

        let test_square = WorldSquare::new(4, 4);

        let [glyph_left, glyph_right] = g.get_glyphs_for_square_from_draw_buffer(test_square);
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
        g.fill_output_buffer_with_solid_color(BLUE);
        assert_eq!(
            g.screen
                .get_glyphs_for_square_from_screen_buffer(point2(5, 0))[0]
                .bg_color,
            BLUE
        );
        g.add_simple_laser(point2(0.0, 0.0), point2(10.0, 0.0));
        g.draw_non_board_animations(Instant::now());
        //g.print_output_buffer();
        assert_eq!(
            g.screen
                .get_glyphs_for_square_from_screen_buffer(point2(5, 0))[0]
                .bg_color,
            BLUE
        );
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
        let glyphs_at_start = g.get_glyphs_for_square_from_draw_buffer(test_square);
        assert_ne!(glyphs_at_start[0].bg_color, ENEMY_PIECE_COLOR);
        assert_ne!(glyphs_at_start[1].bg_color, ENEMY_PIECE_COLOR);

        g.draw_piece_with_color(
            test_square,
            PieceType::OmniDirectionalPawn,
            ENEMY_PIECE_COLOR,
        );
        let line_color = GREEN;
        assert_ne!(line_color, ENEMY_PIECE_COLOR);
        g.draw_braille_line(
            test_square.to_f32() + vec2(-1.0, 0.0),
            test_square.to_f32() + vec2(1.0, 0.0),
            line_color,
        );
        let glyphs_at_end = g.get_glyphs_for_square_from_draw_buffer(test_square);

        assert_eq!(glyphs_at_end[0].bg_color, ENEMY_PIECE_COLOR);
        //assert_eq!(glyphs_at_end[1].bg_color, ENEMY_PIECE_COLOR);

        assert_eq!(glyphs_at_end[0].fg_color, line_color);
        assert_eq!(glyphs_at_end[1].fg_color, line_color);
    }

    #[test]
    fn test_draw_piece_on_board() {
        let mut g = set_up_graphics_with_nxn_world_squares(1);
        let the_square = WorldSquare::new(0, 0);
        g.set_empty_board_animation(BoardSize::new(1, 1));
        g.draw_board_animation(Instant::now());
        //g.print_output_buffer();
        g.draw_piece_with_color(the_square, TurningPawn, WHITE);
        //g.print_output_buffer();
        let drawn_glyphs = g.get_glyphs_for_square_from_draw_buffer(the_square);
        assert_eq!(drawn_glyphs[0].character, '♟');
        assert_eq!(drawn_glyphs[0].fg_color, ENEMY_PIECE_COLOR);
        assert_eq!(drawn_glyphs[0].bg_color, BOARD_WHITE);
        assert_eq!(drawn_glyphs[0].bg_transparent, false);
        assert_eq!(drawn_glyphs[1].character, ' ');
        assert_eq!(drawn_glyphs[1].bg_color, BOARD_WHITE);
        assert_eq!(drawn_glyphs[1].bg_transparent, false);
    }

    #[test]
    fn test_field_of_view_mask_is_fully_transparent() {
        let mut g = set_up_graphics_with_nxn_world_squares(1);
        let the_square = WorldSquare::new(0, 0);
        g.draw_piece_with_color(the_square, TurningPawn, WHITE);

        let drawn_glyphs = g.get_glyphs_for_square_from_draw_buffer(the_square);
        assert_eq!(drawn_glyphs[0].character, '♟');

        let mut fov = FovResult::new_empty_fov_at(point2(0, 0));
        fov.add_fully_visible_square(the_square - fov.root_square());
        g.draw_field_of_view_mask(fov);

        let drawn_glyphs = g.get_glyphs_for_square_from_draw_buffer(the_square);
        assert_eq!(drawn_glyphs[0].character, '♟');
    }
    #[test]
    fn test_draw_buffer_to_screen_through_field_of_view() {
        let mut g = set_up_graphics_with_nxn_world_squares(5);
        let world_character_square = WorldCharacterSquare::new(3, 2);
        let color = CYAN;
        g.draw_buffer
            .insert(world_character_square, Glyph::fg_only('#', color));
        g.draw_buffer
            .insert(point2(0, 0), Glyph::fg_only('#', GREEN));
        let fov = field_of_view_from_square(point2(0, 0), 5, &Default::default());
        g.screen.center_screen_at_square(fov.root_square());
        g.load_screen_buffer_from_fov(fov);
        let screen_buffer_square = g
            .screen
            .world_character_square_to_buffer_square(world_character_square);
        //g.print_draw_buffer(point2(0, 0), 3);
        //g.print_screen_buffer();
        //dbg!( screen_buffer_square, world_character_square, g.get_screen_buffered_glyph(point2(8, 0)).fg_color );
        assert_eq!(screen_buffer_square, point2(8, 0));
        assert_eq!(
            g.screen
                .get_screen_buffered_glyph(screen_buffer_square)
                .fg_color,
            color
        );
    }
}
