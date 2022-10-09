use termion::color;
use crate::utility::sign;

use euclid::*;
use euclid::{point2, vec2};
use std::cmp::min;
use line_drawing::Point;

use crate::utility::*;

// "heighth", "reighth"
pub const EIGHTH_BLOCKS_FROM_LEFT: &[char] = &[' ', '▏', '▎', '▍', '▌', '▋', '▊', '▉', '█'];
pub const EIGHTH_BLOCKS_FROM_BOTTOM: &[char] = &[' ', '▁', '▂', '▃', '▄', '▅', '▆', '▇', '█'];

pub type BrailleArray = [[bool; 4]; 2];

pub fn quarter_block_by_offset(half_steps: IVector) -> char {
    match half_steps.to_tuple() {
        (1, -1) => '▗',
        (1, 0) => '▐',
        (1, 1) => '▝',
        (0, -1) => '▄',
        (0, 0) => '█',
        (0, 1) => '▀',
        (-1, -1) => '▖',
        (-1, 0) => '▌',
        (-1, 1) => '▘',
        _ => ' ',
    }
}

#[derive(Clone, PartialEq, Eq, Debug, Copy)]
pub enum ColorName {
    Red,
    Green,
    Blue,
    LightBlue,
    Cyan,
    LightCyan,
    Black,
    White,
    Reset,
}

#[derive(Clone, PartialEq, Eq, Debug, Copy)]
pub struct Glyph {
    pub character: char,
    pub fg_color: ColorName,
    pub bg_color: ColorName,
}

impl Glyph {
    pub fn to_string(&self) -> String {
        let mut output = self.character.to_string();
        //if self.fg_color != ColorName::White {
        output = format!(
            "{}{}{}",
            Glyph::fg_color_from_name(self.fg_color),
            output,
            Glyph::fg_color_from_name(ColorName::White),
        );
        //}
        //if self.bg_color != ColorName::Black {
        output = format!(
            "{}{}{}",
            Glyph::bg_color_from_name(self.bg_color),
            output,
            Glyph::bg_color_from_name(ColorName::Black),
        );
        //}
        return output;
    }

    pub fn from_char(character: char) -> Glyph {
        Glyph {
            character,
            fg_color: ColorName::White,
            bg_color: ColorName::Black,
        }
    }

    pub fn reset_colors() -> String {
        format!(
            "{}{}",
            Glyph::fg_color_from_name(ColorName::Reset),
            Glyph::bg_color_from_name(ColorName::Reset)
        )
    }

    pub fn fg_color_from_name(color_name: ColorName) -> String {
        match color_name {
            ColorName::Red => color::Fg(color::Red).to_string(),
            ColorName::Green => color::Fg(color::Green).to_string(),
            ColorName::Blue => color::Fg(color::Blue).to_string(),
            ColorName::LightBlue => color::Fg(color::LightBlue).to_string(),
            ColorName::Cyan => color::Fg(color::Cyan).to_string(),
            ColorName::LightCyan => color::Fg(color::LightCyan).to_string(),
            ColorName::White => color::Fg(color::White).to_string(),
            ColorName::Black => color::Fg(color::Black).to_string(),
            ColorName::Reset => color::Fg(color::Reset).to_string(),
        }
    }

    pub fn bg_color_from_name(color_name: ColorName) -> String {
        match color_name {
            ColorName::Red => color::Bg(color::Red).to_string(),
            ColorName::Green => color::Bg(color::Green).to_string(),
            ColorName::Blue => color::Bg(color::Blue).to_string(),
            ColorName::LightBlue => color::Bg(color::LightBlue).to_string(),
            ColorName::Cyan => color::Bg(color::Cyan).to_string(),
            ColorName::LightCyan => color::Bg(color::LightCyan).to_string(),
            ColorName::White => color::Bg(color::White).to_string(),
            ColorName::Black => color::Bg(color::Black).to_string(),
            ColorName::Reset => color::Bg(color::Reset).to_string(),
        }
    }

    #[allow(dead_code)]
    pub fn square_with_horizontal_offset(fraction_of_square_offset: f32) -> Glyph {
        Glyph::colored_square_with_horizontal_offset(fraction_of_square_offset, ColorName::White)
    }

    pub fn colored_square_with_horizontal_offset(
        fraction_of_square_offset: f32,
        color_name: ColorName,
    ) -> Glyph {
        let offset_in_eighths_rounded_towards_inf = (fraction_of_square_offset * 8.0).ceil() as i32;
        assert!(offset_in_eighths_rounded_towards_inf.abs() <= 8);
        return if offset_in_eighths_rounded_towards_inf <= 0 {
            Glyph {
                character: EIGHTH_BLOCKS_FROM_LEFT
                    [(8 + offset_in_eighths_rounded_towards_inf) as usize],
                fg_color: color_name,
                bg_color: ColorName::Black,
            }
        } else {
            Glyph {
                character: EIGHTH_BLOCKS_FROM_LEFT[offset_in_eighths_rounded_towards_inf as usize],
                fg_color: ColorName::Black,
                bg_color: color_name,
            }
        };
    }
    #[allow(dead_code)]
    pub fn square_with_vertical_offset(fraction_of_square_offset: f32) -> Glyph {
        return Glyph::colored_square_with_vertical_offset(
            fraction_of_square_offset,
            ColorName::White,
        );
    }
    pub fn colored_square_with_vertical_offset(
        fraction_of_square_offset: f32,
        color_name: ColorName,
    ) -> Glyph {
        let offset_in_eighths_rounded_towards_inf = (fraction_of_square_offset * 8.0).ceil() as i32;
        assert!(offset_in_eighths_rounded_towards_inf.abs() <= 8);
        return if offset_in_eighths_rounded_towards_inf <= 0 {
            Glyph {
                character: EIGHTH_BLOCKS_FROM_BOTTOM
                    [(8 + offset_in_eighths_rounded_towards_inf) as usize],
                fg_color: color_name,
                bg_color: ColorName::Black,
            }
        } else {
            Glyph {
                character: EIGHTH_BLOCKS_FROM_BOTTOM
                    [(offset_in_eighths_rounded_towards_inf) as usize],
                fg_color: ColorName::Black,
                bg_color: color_name,
            }
        };
    }
    pub fn colored_square_with_half_step_offset(
        offset: FVector,
        color_name: ColorName,
    ) -> Glyph {
        let step: IVector = (offset * 2.0).round().to_i32();
        Glyph {
            character: quarter_block_by_offset(step),
            fg_color: color_name,
            bg_color: ColorName::Black,
        }
    }

    #[allow(dead_code)]
    pub fn get_glyphs_for_floating_square(pos: FPoint) -> Vec<Vec<Option<Glyph>>> {
        Glyph::get_glyphs_for_colored_floating_square(pos, ColorName::White)
    }

    pub fn get_glyphs_for_colored_floating_square(
        pos: FPoint,
        color: ColorName,
    ) -> Vec<Vec<Option<Glyph>>> {
        let grid_offset = fraction_part(pos);
        let x_offset = grid_offset.x;
        let y_offset = grid_offset.y;
        if y_offset.abs() < x_offset.abs() && y_offset.abs() < 0.25 {
            Glyph::get_smooth_horizontal_glyphs_for_colored_floating_square(pos, color)
        } else if x_offset.abs() < 0.25 {
            Glyph::get_smooth_vertical_glyphs_for_colored_floating_square(pos, color)
        } else {
            Glyph::get_half_grid_glyphs_for_colored_floating_square(pos, color)
        }
    }
    #[allow(dead_code)]
    pub fn get_smooth_horizontal_glyphs_for_floating_square(
        pos: FPoint,
    ) -> Vec<Vec<Option<Glyph>>> {
        Glyph::get_smooth_horizontal_glyphs_for_colored_floating_square(pos, ColorName::White)
    }


    pub fn get_smooth_horizontal_glyphs_for_colored_floating_square(
        pos: FPoint,
        color: ColorName,
    ) -> Vec<Vec<Option<Glyph>>> {
        let width = 3;
        let mut output = vec![vec![None; width]; width];

        let c = width / 2 as usize;

        let grid_offset = fraction_part(pos);
        let x_offset = grid_offset.x;
        let offset_dir: IPoint = sign2d(grid_offset).to_i32();

        for i in 0..3 {
            let x = i as i32 - 1;
            if offset_dir.x == x || x == 0 {
                output[i][c] = Some(Glyph::colored_square_with_horizontal_offset(
                    x_offset - x as f32,
                    color,
                ));
            }
        }

        return output;
    }
    #[allow(dead_code)]
    pub fn get_smooth_vertical_glyphs_for_floating_square(
        pos: FPoint,
    ) -> Vec<Vec<Option<Glyph>>> {
        Glyph::get_smooth_vertical_glyphs_for_colored_floating_square(pos, ColorName::White)
    }
    pub fn get_smooth_vertical_glyphs_for_colored_floating_square(
        pos: FPoint,
        color: ColorName,
    ) -> Vec<Vec<Option<Glyph>>> {
        let width = 3;
        let mut output = vec![vec![None; width]; width];

        let c = width / 2 as usize;

        let grid_offset = fraction_part(pos);
        let y_offset = grid_offset.y;
        let offset_dir: IPoint = sign2d(grid_offset).to_i32();
        for j in 0..3 {
            let y = j as i32 - 1;
            if offset_dir.y == y || y == 0 {
                output[c][j] = Some(Glyph::colored_square_with_vertical_offset(
                    y_offset - y as f32,
                    color,
                ));
            }
        }
        return output;
    }

    pub fn get_half_grid_glyphs_for_floating_square(pos: default::Point2D<f32>) -> Vec<Vec<Option<Glyph>>> {
        Glyph::get_half_grid_glyphs_for_colored_floating_square(pos, ColorName::White)
    }

    pub fn get_half_grid_glyphs_for_colored_floating_square(
        pos: FPoint,
        color: ColorName,
    ) -> Vec<Vec<Option<Glyph>>> {
        let width = 3;
        let mut output = vec![vec![None; width]; width];
        let grid_offset = fraction_part(pos);
        let offset_dir = sign2d(grid_offset).to_i32();

        for i in 0..3 {
            for j in 0..3 {
                let x = i as i32 - 1;
                let y = j as i32 - 1;
                let square = point2(x, y);
                if (offset_dir.x == x || x == 0) && (offset_dir.y == y || y == 0) {
                    let glyph =
                        Glyph::colored_square_with_half_step_offset((grid_offset - square.to_f32()), color);
                    if glyph.character != ' ' {
                        output[i][j] = Some(glyph);
                    }
                }
            }
        }
        return output;
    }

    pub fn array_to_braille(input: BrailleArray) -> char {
        let mut dot_val: u32 = 0;
        for x in 0..2 {
            for y in 0..4 {
                if input[x][y] {
                    dot_val |= Glyph::braille_bit_for_pos(point2(x as i32, y as i32));
                }
            }
        }
        return char::from_u32('\u{2800}' as u32 | dot_val).unwrap();
    }

    pub fn braille_bit_for_pos(p: Point2D<i32, BrailleWorldSpace>) -> u32 {
        let braille_value_map = vec![vec![7, 3, 2, 1], vec![8, 6, 5, 4]];
        1 << (braille_value_map[p.x as usize][p.y as usize] - 1)
    }

    pub fn add_braille_dot(character: char, p: Point2D<i32, BrailleWorldSpace>) -> char {
        char::from_u32(character as u32 | Glyph::braille_bit_for_pos(p)).unwrap()
    }

    pub fn empty_braille() -> char {
        '\u{2800}'
    }

    pub fn is_braille(c: char) -> bool {
        let x = c as u32;
        // The unicode braille block
        x >= 0x2800 && x <= 0x28FF
    }

    pub fn count_braille_dots(character: char) -> i32 {
        if !Glyph::is_braille(character) {
            return 0;
        }
        let num_good_bits = 8;
        let mut sum = 0;
        let bits = character as u32;
        for i in 0..num_good_bits {
            sum += (bits >> i) & 1;
        }
        return sum as i32;
    }

    pub fn add_braille(c1: char, c2: char) -> char {
        char::from_u32(c1 as u32 | c2 as u32).unwrap()
    }

    pub fn get_glyphs_for_braille_line(
        start_pos: Point2D<f32, WorldSpace>,
        end_pos: Point2D<f32, WorldSpace>,
    ) -> Vec<Vec<Option<Glyph>>> {
        Glyph::get_glyphs_for_colored_braille_line(start_pos, end_pos, ColorName::White)
    }

    pub fn world_pos_to_braille_pos(pos: Point2D<f32, WorldSpace>) -> Point2D<f32, BrailleWorldSpace> {
        point2(pos.x * 2.0 + 0.5, pos.y * 4.0 + 1.5)
    }

    pub fn braille_pos_to_world_pos(pos: Point2D<f32, BrailleWorldSpace>) -> Point2D<f32, WorldSpace> {
        point2((pos.x - 0.5) / 2.0, (pos.y - 1.5) / 4.0)
    }

    pub fn braille_square_to_dot_in_character(pos: Point2D<i32, BrailleWorldSpace>) -> Point2D<i32, BrailleWorldSpace> {
        point2((pos.x % 2).abs(), (pos.y % 4).abs())
    }

    pub fn braille_grid_to_character_grid(braille_square: Point2D<i32, BrailleWorldSpace>) -> Point2D<i32, CharacterWorldSpace> {
        point2(
            ((braille_square.x as f32 - 0.5) / 2.0).round() as i32,
            ((braille_square.y as f32 - 1.5) / 4.0).round() as i32,
        )
    }

    pub fn get_glyphs_for_colored_braille_line(
        start_pos: Point2D<f32, WorldSpace>,
        end_pos: Point2D<f32, WorldSpace>,
        color: ColorName,
    ) -> Vec<Vec<Option<Glyph>>> {
        let start_grid_square = start_pos.round().to_i32();
        let end_grid_square = end_pos.round().to_i32();
        let start_braille_grid_square = Glyph::world_pos_to_braille_pos(start_pos).round().to_i32();
        let end_braille_grid_square = Glyph::world_pos_to_braille_pos(end_pos).round().to_i32();

        let grid_diagonal = end_grid_square - start_grid_square;
        let grid_width = grid_diagonal.x.abs() + 1;
        let grid_height = grid_diagonal.y.abs() + 1;

        let bottom_square_y = min(start_grid_square.y, end_grid_square.y);
        let left_square_x = min(start_grid_square.x, end_grid_square.x);
        let grid_origin_square = point2(left_square_x, bottom_square_y);

        let mut output_grid: Vec<Vec<Option<Glyph>>> =
            vec![vec![None; grid_height as usize]; grid_width as usize];

        for (x, y) in line_drawing::Bresenham::new(
            start_braille_grid_square.to_tuple(),
            end_braille_grid_square.to_tuple(),
        ) {
            let braille_pos = Point2D::<i32, BrailleWorldSpace>::new(x, y);
            let character_grid_square =
                Glyph::braille_grid_to_character_grid(braille_pos) - grid_origin_square;
            let glyph_in_grid = &mut output_grid[character_grid_square.x as usize]
                [character_grid_square.y as usize];
            if *glyph_in_grid == None {
                *glyph_in_grid = Some(Glyph {
                    character: Glyph::empty_braille(),
                    fg_color: color,
                    bg_color: ColorName::Black,
                });
            }
            let braille_character = &mut (*glyph_in_grid).as_mut().unwrap().character;
            *braille_character = Glyph::add_braille_dot(
                *braille_character,
                Glyph::braille_square_to_dot_in_character(braille_pos),
            );
        }
        return output_grid;
    }

    pub fn world_pos_to_braille_char(world_pos: Point2D<f32, WorldSpace>) -> char {
        let character = Glyph::empty_braille();
        Glyph::add_braille_dot(
            character,
            Glyph::braille_square_to_dot_in_character(
                Glyph::world_pos_to_braille_pos(world_pos
                ).round().to_i32()),
        )
    }

    pub fn world_pos_to_colored_braille_glyph(world_pos: Point2D<f32, WorldSpace>, color: ColorName) -> Glyph {
        Glyph {
            character: Glyph::world_pos_to_braille_char(world_pos),
            fg_color: color,
            bg_color: ColorName::Black,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use assert2::assert;

    #[test]
    fn test_colored_square_with_half_step_offsets() {
        assert_eq!(Glyph::colored_square_with_half_step_offset(vec2(0.0, 0.0), ColorName::Red).character, quarter_block_by_offset(vec2(0, 0)));
        assert_eq!(Glyph::colored_square_with_half_step_offset(vec2(0.0, 0.0), ColorName::Red).fg_color, ColorName::Red);
        assert_eq!(Glyph::colored_square_with_half_step_offset(vec2(0.0, 0.0), ColorName::Red).bg_color, ColorName::Black);
        assert_eq!(Glyph::colored_square_with_half_step_offset(vec2(0.1, 0.1), ColorName::Red).character, quarter_block_by_offset(vec2(0, 0)));
        assert_eq!(Glyph::colored_square_with_half_step_offset(vec2(0.24, 0.0), ColorName::Red).character, quarter_block_by_offset(vec2(0, 0)));
        assert_eq!(Glyph::colored_square_with_half_step_offset(vec2(0.25, 0.0), ColorName::Red).character, quarter_block_by_offset(vec2(1, 0)));
        assert_eq!(Glyph::colored_square_with_half_step_offset(vec2(0.26, 0.0), ColorName::Red).character, quarter_block_by_offset(vec2(1, 0)));
        assert_eq!(Glyph::colored_square_with_half_step_offset(vec2(-0.25, 0.0), ColorName::Red).character, quarter_block_by_offset(vec2(0, 0)));
        assert_eq!(Glyph::colored_square_with_half_step_offset(vec2(-0.26, 0.0), ColorName::Red).character, quarter_block_by_offset(vec2(-1, 0)));
        assert_eq!(Glyph::colored_square_with_half_step_offset(vec2(0.49, 0.0), ColorName::Red).character, quarter_block_by_offset(vec2(1, 0)));
        assert_eq!(Glyph::colored_square_with_half_step_offset(vec2(0.5, 0.0), ColorName::Red).character, quarter_block_by_offset(vec2(1, 0)));
        assert_eq!(Glyph::colored_square_with_half_step_offset(vec2(0.2, 0.4), ColorName::Red).character, quarter_block_by_offset(vec2(0, 1)));
        assert_eq!(Glyph::colored_square_with_half_step_offset(vec2(-0.499, 0.4), ColorName::Red).character, quarter_block_by_offset(vec2(-1, 1)));
        assert_eq!(Glyph::colored_square_with_half_step_offset(vec2(0.74, 0.0), ColorName::Red).character, quarter_block_by_offset(vec2(1, 0)));
        assert_eq!(Glyph::colored_square_with_half_step_offset(vec2(0.76, 0.0), ColorName::Red).character, quarter_block_by_offset(vec2(2, 0)));
        assert_eq!(Glyph::colored_square_with_half_step_offset(vec2(0.3, -0.6), ColorName::Red).character, quarter_block_by_offset(vec2(1, -1)));
    }

    #[test]
    fn test_half_grid_glyph_when_rounding_to_zero_for_both_axes() {
        let test_pos = point2(-0.24, 0.01);
        let glyphs = Glyph::get_half_grid_glyphs_for_floating_square(test_pos);
        assert!(glyphs[0][0] == None);
        assert!(glyphs[0][1] == None);
        assert!(glyphs[0][2] == None);
        assert!(glyphs[1][0] == None);
        assert_eq!(glyphs[1][1].clone().unwrap().character, quarter_block_by_offset(vec2(0, 0)));
        assert!(glyphs[1][2] == None);
        assert!(glyphs[2][0] == None);
        assert!(glyphs[2][1] == None);
        assert!(glyphs[2][2] == None);
    }

    #[test]
    fn test_half_grid_glyphs_when_rounding_to_zero_for_x_and_half_step_up_for_y() {
        let test_pos = point2(0.24, 0.26);
        let glyphs = Glyph::get_half_grid_glyphs_for_floating_square(test_pos);
        assert!(glyphs[0][0] == None);
        assert!(glyphs[0][1] == None);
        assert!(glyphs[0][2] == None);
        assert!(glyphs[1][0] == None);
        assert_eq!(glyphs[1][1].clone().unwrap().character, quarter_block_by_offset(vec2(0, 1)));
        assert_eq!(glyphs[1][2].clone().unwrap().character, quarter_block_by_offset(vec2(0, -1)));
        assert!(glyphs[2][0] == None);
        assert!(glyphs[2][1] == None);
        assert!(glyphs[2][2] == None);
    }

    #[test]
    fn test_half_grid_glyphs_when_rounding_to_zero_for_x_and_exactly_half_step_up_for_y() {
        let test_pos = point2(0.24, 0.25);

        let glyphs = Glyph::get_half_grid_glyphs_for_floating_square(test_pos);
        assert!(glyphs[0][0] == None);
        assert!(glyphs[0][1] == None);
        assert!(glyphs[0][2] == None);
        assert!(glyphs[1][0] == None);
        assert_eq!(glyphs[1][1].clone().unwrap().character, quarter_block_by_offset(vec2(0, 1)));
        assert_eq!(glyphs[1][2].clone().unwrap().character, quarter_block_by_offset(vec2(0, -1)));
        assert!(glyphs[2][0] == None);
        assert!(glyphs[2][1] == None);
        assert!(glyphs[2][2] == None);
    }

    #[test]
    fn test_half_grid_glyphs_when_rounding_to_zero_for_x_and_exactly_half_step_down_for_y() {
        let test_pos = point2(-0.2, -0.25);
        let glyphs = Glyph::get_half_grid_glyphs_for_floating_square(test_pos);
        assert!(glyphs[0][0] == None);
        assert!(glyphs[0][1] == None);
        assert!(glyphs[0][2] == None);
        assert!(glyphs[1][0] == None);
        assert_eq!(glyphs[1][1].clone().unwrap().character, quarter_block_by_offset(vec2(0, 0)));
        assert!(glyphs[1][2] == None);
        assert!(glyphs[2][0] == None);
        assert!(glyphs[2][1] == None);
        assert!(glyphs[2][2] == None);
    }

    #[test]
    fn test_half_grid_glyphs_when_rounding_to_zero_for_y_and_half_step_right_for_x() {
        let test_pos = point2(0.3, 0.1);
        let glyphs = Glyph::get_half_grid_glyphs_for_floating_square(test_pos);
        assert!(glyphs[0][0] == None);
        assert!(glyphs[0][1] == None);
        assert!(glyphs[0][2] == None);
        assert!(glyphs[1][0] == None);
        assert_eq!(glyphs[1][1].clone().unwrap().character, quarter_block_by_offset(vec2(1, 0)));
        assert!(glyphs[1][2] == None);
        assert!(glyphs[2][0] == None);
        assert_eq!(glyphs[2][1].clone().unwrap().character, quarter_block_by_offset(vec2(-1, 0)));
        assert!(glyphs[2][2] == None);
    }

    #[test]
    fn test_half_grid_glyphs_when_rounding_to_zero_for_y_and_half_step_left_for_x() {
        let test_pos = point2(-0.3, 0.2);
        let glyphs = Glyph::get_half_grid_glyphs_for_floating_square(test_pos);
        assert!(glyphs[0][0] == None);
        assert_eq!(glyphs[0][1].clone().unwrap().character, quarter_block_by_offset(vec2(1, 0)));
        assert!(glyphs[0][2] == None);
        assert!(glyphs[1][0] == None);
        assert_eq!(glyphs[1][1].clone().unwrap().character, quarter_block_by_offset(vec2(-1, 0)));
        assert!(glyphs[1][2] == None);
        assert!(glyphs[2][0] == None);
        assert!(glyphs[2][1] == None);
        assert!(glyphs[2][2] == None);
    }

    #[test]
    fn test_array_to_braille_char() {
        // 10
        // 00
        // 01
        // 01
        let input = [[false, false, false, true], [true, true, false, false]];
        assert_eq!(Glyph::array_to_braille(input), '⢡');

        // 00
        // 11
        // 01
        // 00
        let input = [[false, false, true, false], [false, true, true, false]];
        assert_eq!(Glyph::array_to_braille(input), '⠲');
    }

    #[test]
    fn test_get_empty_braille_character() {
        assert_eq!(Glyph::empty_braille(), '\u{2800}');
    }

    #[test]
    fn test_set_braille_dot() {
        let mut b = Glyph::empty_braille();
        b = Glyph::add_braille_dot(b, point2(0, 0));
        b = Glyph::add_braille_dot(b, point2(1, 1));
        assert_eq!(b, '⡠');
    }

    #[test]
    fn test_chars_for_horizontal_braille_line_without_rounding() {
        let start = point2(-0.25, -0.4);
        let end = point2(1.75, -0.4);

        // Expected braille:
        // 00 00 00
        // 00 00 00
        // 00 00 00
        // 11 11 10

        let line_glyphs = Glyph::get_glyphs_for_braille_line(start, end);
        assert_eq!(line_glyphs.len(), 3);
        assert_eq!(line_glyphs[0].len(), 1);
        assert_eq!(line_glyphs[1].len(), 1);
        assert_eq!(line_glyphs[2].len(), 1);

        assert_eq!(line_glyphs[0][0].clone().unwrap().character, '\u{28C0}');
        assert_eq!(line_glyphs[1][0].clone().unwrap().character, '\u{28C0}');
        assert_eq!(line_glyphs[2][0].clone().unwrap().character, '\u{2840}');
    }

    #[test]
    fn test_chars_for_horizontal_braille_line_with_offset_without_rounding() {
        let start = point2(-0.25, 0.4);
        let end = point2(1.75, 0.4);

        // Expected braille:
        // 11 11 10
        // 00 00 00
        // 00 00 00
        // 00 00 00

        let line_glyphs = Glyph::get_glyphs_for_braille_line(start, end);
        assert_eq!(line_glyphs.len(), 3);
        assert_eq!(line_glyphs[0].len(), 1);
        assert_eq!(line_glyphs[1].len(), 1);
        assert_eq!(line_glyphs[2].len(), 1);

        assert_eq!(line_glyphs[0][0].clone().unwrap().character, '\u{2809}');
        assert_eq!(line_glyphs[1][0].clone().unwrap().character, '\u{2809}');
        assert_eq!(line_glyphs[2][0].clone().unwrap().character, '\u{2801}');
    }

    #[test]
    fn test_chars_for_vertical_braille_line_without_rounding() {
        let start = point2(-0.25, -0.4);
        let end = point2(-0.25, 0.875);

        // Expected braille:
        // 00
        // 00
        // 10
        // 10

        // 10
        // 10
        // 10
        // 10

        let line_glyphs = Glyph::get_glyphs_for_braille_line(start, end);
        assert_eq!(line_glyphs.len(), 1);
        assert_eq!(line_glyphs[0].len(), 2);

        assert_eq!(line_glyphs[0][0].clone().unwrap().character, '\u{2847}');
        assert_eq!(line_glyphs[0][1].clone().unwrap().character, '\u{2844}');
    }

    #[test]
    fn test_braille_grid_to_character_grid() {
        assert_eq!(Glyph::braille_grid_to_character_grid(point2(0, 0)), point2(0, 0));
        assert_eq!(Glyph::braille_grid_to_character_grid(point2(1, 3)), point2(0, 0));
        assert_eq!(Glyph::braille_grid_to_character_grid(point2(-1, -1)), point2(-1, -1));
        assert_eq!(Glyph::braille_grid_to_character_grid(point2(2, 8)), point2(1, 2));
        assert_eq!(Glyph::braille_grid_to_character_grid(point2(21, 80)), point2(10, 20));
    }

    #[test]
    fn test_world_pos_to_braille_pos() {
        assert_eq!(Glyph::world_pos_to_braille_pos(point2(0.0, 0.0)), point2(0.5, 1.5));
        assert_eq!(Glyph::world_pos_to_braille_pos(point2(1.0, 0.0)), point2(2.5, 1.5));
        assert_eq!(Glyph::world_pos_to_braille_pos(point2(0.25, 0.375)), point2(1.0, 3.0));
    }

    #[test]
    fn test_braille_pos_to_world_pos() {
        assert_eq!(Glyph::braille_pos_to_world_pos(point2(0.5, 1.5)), point2(0.0, 0.0));
        assert_eq!(Glyph::braille_pos_to_world_pos(point2(2.5, 1.5)), point2(1.0, 0.0));
        assert_eq!(Glyph::braille_pos_to_world_pos(point2(1.0, 3.0)), point2(0.25, 0.375));
    }

    #[test]
    fn test_braille_square_to_dot_in_character() {
        assert_eq!(Glyph::braille_square_to_dot_in_character(point2(0, 0)), point2(0, 0));
        assert_eq!(Glyph::braille_square_to_dot_in_character(point2(1, 3)), point2(1, 3));
        assert_eq!(Glyph::braille_square_to_dot_in_character(point2(25, 4)), point2(1, 0));
        assert_eq!(Glyph::braille_square_to_dot_in_character(point2(-3, 4)), point2(1, 0));
    }

    #[test]
    fn test_combine_braille_character() {
        assert_eq!(Glyph::add_braille('\u{2800}', '\u{2820}'), '\u{2820}');
        assert_eq!(Glyph::add_braille('\u{2801}', '\u{28C0}'), '\u{28C1}');
    }

    #[test]
    fn test_world_point_to_braille_char() {
        assert_eq!(Glyph::world_pos_to_braille_char(point2(0.0, 0.0)), '\u{2810}');
        assert_eq!(Glyph::world_pos_to_braille_char(point2(-0.4, -0.4)), '\u{2840}');
        assert_eq!(Glyph::world_pos_to_braille_char(point2(0.2, 0.4)), '\u{2808}');
    }

    #[test]
    fn test_world_point_to_braille_char_is_always_braille() {
        for _ in 0..200 {
            //let random_point = p(rand_in_range(0.0, 30.0), rand_in_range(0.0, 30.0));
            let random_point = point2(23.2273, 2.05);

            assert!(Glyph::is_braille(Glyph::world_pos_to_braille_char(
                random_point
            )));
        }
    }

    #[test]
    fn test_world_point_to_braille_glyph() {
        let points = [point2(0.0, 0.0), point2(-0.4, -0.4), point2(0.2, 0.4)];
        for p1 in points {
            assert_eq!(Glyph::world_pos_to_colored_braille_glyph(p1, ColorName::Black).character, Glyph::world_pos_to_braille_char(p1));
        }
    }

    #[test]
    fn test_count_braille_dots() {
        assert_eq!(Glyph::count_braille_dots('\u{2800}'), 0);
        assert_eq!(Glyph::count_braille_dots('\u{2818}'), 2);
        assert_eq!(Glyph::count_braille_dots('\u{28C0}'), 2);
        assert_eq!(Glyph::count_braille_dots('\u{28FF}'), 8);
        assert_eq!(Glyph::count_braille_dots('A'), 0);
        assert_eq!(Glyph::count_braille_dots('#'), 0);
    }
}
