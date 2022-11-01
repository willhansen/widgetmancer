use std::cmp::min;
use std::collections::{HashMap, HashSet};

use ::num::clamp;
use euclid::*;
use euclid::{point2, vec2};
use line_drawing::Point;
use rgb::*;
use termion::color;

use crate::utility::sign;
use crate::utility::*;

pub const PLAYER_GREEN: RGB8 = RGB8::new(50, 200, 50);
pub const RED: RGB8 = RGB8::new(255, 0, 0);
pub const GREEN: RGB8 = RGB8::new(0, 255, 0);
pub const BLUE: RGB8 = RGB8::new(0, 0, 255);
pub const CYAN: RGB8 = RGB8::new(0, 255, 255);
pub const MAGENTA: RGB8 = RGB8::new(255, 0, 255);
pub const YELLOW: RGB8 = RGB8::new(255, 255, 0);
pub const WHITE: RGB8 = RGB8::new(200, 200, 150);
pub const BLACK: RGB8 = RGB8::new(0, 0, 0);
pub const BOARD_WHITE: RGB8 = RGB8::new(100, 100, 80);
pub const BOARD_BLACK: RGB8 = RGB8::new(50, 50, 70);
pub const EXPLOSION_COLOR: RGB8 = RGB8::new(200, 200, 255);
pub const SELECTOR_COLOR: RGB8 = RGB8::new(255, 64, 0);

pub const EIGHTH_BLOCKS_FROM_LEFT: &[char] = &[' ', '▏', '▎', '▍', '▌', '▋', '▊', '▉', '█'];
pub const EIGHTH_BLOCKS_FROM_BOTTOM: &[char] = &[' ', '▁', '▂', '▃', '▄', '▅', '▆', '▇', '█'];

pub const HORIZONTAL_HALF_BLOCK: char = EIGHTH_BLOCKS_FROM_LEFT[4];

pub const FULL_BLOCK: char = '█';
pub const SPACE: char = ' ';
pub const EMPTY_BRAILLE: char = '\u{2800}';

pub const KNOWN_FG_ONLY_CHARS: &[char] = &[FULL_BLOCK];
pub const KNOWN_BG_ONLY_CHARS: &[char] = &[SPACE, EMPTY_BRAILLE];

pub const DANGER_SQUARE_CHARS: &[char; 2] = &['▪', ' '];

pub type BrailleArray = [[bool; 4]; 2];
pub type TwoGlyphs = [Glyph; 2];

// Fun unicode for later
// ↈ ▴ ⚠ 🞁 🢑  🛆  𝅉  ⏹  ᙮ ⸼  ▪
// ☠⯃⯄
// ⨻🕱
//   ⃤  ⟁   ⃠   ꙰
//     ◌  𝨞
// ◌
//    𝩬   𝩫

//  ♜ 	♞ 	♝ 	♛ 	♚ 	♝ 	♞ 	♜
// 	♟︎ 	♟︎ 	♟︎ 	♟︎ 	♟︎ 	♟︎ 	♟︎ 	♟︎

// 	♙ 	♙ 	♙ 	♙ 	♙ 	♙ 	♙ 	♙
// 	♖ 	♘ 	♗ 	♕ 	♔ 	♗ 	♘ 	♖

//  🨀 	🨁 	🨂 	🨃 	🨄 	🨅 	🨆 	🨇 	🨈 	🨉 	🨊 	🨋 	🨌 	🨍 	🨎 	🨏
//	🨐 	🨑 	🨒 	🨓 	🨔 	🨕 	🨖 	🨗 	🨘 	🨙 	🨚 	🨛 	🨜 	🨝 	🨞 	🨟
//	🨠 	🨡 	🨢 	🨣 	🨤 	🨥 	🨦 	🨧 	🨨 	🨩 	🨪 	🨫 	🨬 	🨭 	🨮 	🨯
//	🨰 	🨱 	🨲 	🨳 	🨴 	🨵 	🨶 	🨷 	🨸 	🨹 	🨺 	🨻 	🨼 	🨽 	🨾 	🨿
//	🩀 	🩁 	🩂 	🩃 	🩄 	🩅 	🩆 	🩇 	🩈 	🩉 	🩊 	🩋 	🩌 	🩍 	🩎 	🩏
//	🩐 	🩑 	🩒 	🩓
//	🩠 	🩡 	🩢 	🩣 	🩤 	🩥 	🩦 	🩧 	🩨 	🩩 	🩪 	🩫 	🩬 	🩭

// All the braille unicode consecutively for easy reference
//⠁⠂⠃⠄⠅⠆⠇⠈⠉⠊⠋⠌⠍⠎⠏⠐⠑⠒⠓⠔⠕⠖⠗⠘⠙⠚⠛⠜⠝⠞⠟⠠⠡⠢⠣⠤⠥⠦⠧⠨⠩⠪⠫⠬⠭⠮⠯⠰⠱⠲⠳⠴⠵⠶⠷⠸⠹⠺⠻⠼⠽⠾⠿⡀⡁⡂⡃⡄⡅⡆⡇⡈⡉⡊⡋⡌⡍⡎⡏⡐⡑⡒⡓⡔⡕⡖⡗⡘⡙⡚⡛⡜⡝⡞⡟⡠⡡⡢⡣⡤⡥⡦⡧⡨⡩⡪⡫⡬⡭⡮⡯⡰⡱⡲⡳⡴⡵⡶⡷⡸⡹⡺⡻⡼⡽⡾⡿⢀⢁⢂⢃⢄⢅⢆⢇⢈⢉⢊⢋⢌⢍⢎⢏⢐⢑⢒⢓⢔⢕⢖⢗⢘⢙⢚⢛⢜⢝⢞⢟⢠⢡⢢⢣⢤⢥⢦⢧⢨⢩⢪⢫⢬⢭⢮⢯⢰⢱⢲⢳⢴⢵⢶⢷⢸⢹⢺⢻⢼⢽⢾⢿⣀⣁⣂⣃⣄⣅⣆⣇⣈⣉⣊⣋⣌⣍⣎⣏⣐⣑⣒⣓⣔⣕⣖⣗⣘⣙⣚⣛⣜⣝⣞⣟⣠⣡⣢⣣⣤⣥⣦⣧⣨⣩⣪⣫⣬⣭⣮⣯⣰⣱⣲⣳⣴⣵⣶⣷⣸⣹⣺⣻⣼⣽⣾⣿

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
pub struct Glyph {
    pub character: char,
    pub fg_color: RGB8,
    pub bg_color: RGB8,
    pub fg_alpha: u8,
    pub bg_alpha: u8,
}

impl Glyph {
    pub fn new(character: char, fg_color: RGB8, bg_color: RGB8) -> Glyph {
        Glyph {
            character,
            fg_color,
            bg_color,
            fg_alpha: 255,
            bg_alpha: 255,
        }
    }
    pub fn fg_only(character: char, fg_color: RGB8) -> Glyph {
        Glyph {
            character,
            fg_color,
            bg_color: BLACK,
            fg_alpha: 255,
            bg_alpha: 0,
        }
    }

    pub fn to_string(&self) -> String {
        let mut output = self.character.to_string();
        output = format!(
            "{}{}{}",
            color::Fg(color::Rgb(
                self.fg_color.r,
                self.fg_color.g,
                self.fg_color.b,
            )),
            output,
            color::Fg(color::Reset),
        );
        output = format!(
            "{}{}{}",
            color::Bg(color::Rgb(
                self.bg_color.r,
                self.bg_color.g,
                self.bg_color.b,
            )),
            output,
            color::Bg(color::Reset),
        );
        //}
        return output;
    }

    pub fn from_char(character: char) -> Glyph {
        Glyph::new(character, WHITE, BLACK)
    }

    pub fn with_char(&self, new_char: char) -> Glyph {
        let mut dup = self.clone();
        dup.character = new_char;
        dup
    }
    pub fn with_fg(&self, new_fg: RGB8) -> Glyph {
        let mut dup = self.clone();
        dup.fg_color = new_fg;
        dup
    }
    pub fn with_bg(&self, new_bg: RGB8) -> Glyph {
        let mut dup = self.clone();
        dup.bg_color = new_bg;
        dup
    }

    pub fn reset_colors() -> String {
        format!("{}{}", color::Fg(color::Reset), color::Bg(color::Reset),)
    }

    pub fn partial_block(vertical: bool, fraction: f32) -> char {
        let eighths = (fraction * 8.0).round() as usize;
        let clamped_eighths = clamp(eighths, 0, 8);
        if vertical {
            EIGHTH_BLOCKS_FROM_BOTTOM[clamped_eighths]
        } else {
            EIGHTH_BLOCKS_FROM_LEFT[clamped_eighths]
        }
    }

    pub fn colored_character_square_with_offset(
        is_vertical: bool,
        fraction_of_square_offset: f32,
        square_color: RGB8,
        background_color: RGB8,
    ) -> Glyph {
        if fraction_of_square_offset < 0.0 {
            let character = Glyph::partial_block(is_vertical, 1.0 + fraction_of_square_offset);
            Glyph::new(character, square_color, background_color)
        } else {
            let character = Glyph::partial_block(is_vertical, fraction_of_square_offset);
            Glyph::new(character, background_color, square_color)
        }
    }
    pub fn colored_square_with_half_step_offset(offset: FVector, color: RGB8) -> Glyph {
        let step: IVector = (offset * 2.0).round().to_i32();
        Glyph::new(quarter_block_by_offset(step), color, BLACK)
    }

    pub fn offset_board_square_glyphs(
        offset_vector: WorldMove,
        square_color: RGB8,
        background_color: RGB8,
    ) -> TwoGlyphs {
        assert!(is_orthogonal(offset_vector));
        let is_vertical = offset_vector.x == 0.0;
        // because sign
        let offset_magnitude = if is_vertical {
            offset_vector.y
        } else {
            offset_vector.x
        };
        let offset_magnitude_within_one_period_symmetric_about_zero =
            (offset_magnitude + 1.0).rem_euclid(2.0) - 1.0;

        if is_vertical {
            Glyph::vertical_square_offset_to_character_offsets(
                offset_magnitude_within_one_period_symmetric_about_zero,
            )
            .map(|character_offset| {
                Glyph::colored_character_square_with_offset(
                    true,
                    character_offset,
                    square_color,
                    background_color,
                )
            })
        } else {
            let character_offsets = Glyph::horizontal_square_offset_to_character_offsets(
                offset_magnitude_within_one_period_symmetric_about_zero,
            );
            character_offsets.map(|character_offset| {
                Glyph::colored_character_square_with_offset(
                    false,
                    character_offset,
                    square_color,
                    background_color,
                )
            })
        }
    }

    pub fn horizontal_square_offset_to_character_offsets(offset_fraction: f32) -> [f32; 2] {
        assert!(offset_fraction.abs() <= 1.0);

        let mut f_c = offset_fraction * 2.0;
        if f_c.abs() >= 1.0 {
            if f_c > 0.0 {
                f_c -= 1.0;
            } else {
                f_c += 1.0;
            }
        }

        if offset_fraction <= -0.5 {
            [f_c, -1.0]
        } else if offset_fraction < 0.0 {
            [0.0, f_c]
        } else if offset_fraction < 0.5 {
            [f_c, 0.0]
        } else {
            [1.0, f_c]
        }
    }
    pub fn vertical_square_offset_to_character_offsets(offset_fraction: f32) -> [f32; 2] {
        // outputs in range [-1.0, 1.0)
        assert!(offset_fraction >= -1.0 && offset_fraction <= 1.0);
        [offset_fraction, offset_fraction]
    }

    #[allow(dead_code)]
    pub fn get_glyphs_for_floating_square(pos: FPoint) -> Vec<Vec<Option<Glyph>>> {
        Glyph::get_glyphs_for_colored_floating_square(pos, WHITE)
    }

    pub fn get_glyphs_for_colored_floating_square(
        pos: FPoint,
        color: RGB8,
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
        Glyph::get_smooth_horizontal_glyphs_for_colored_floating_square(pos, WHITE)
    }

    pub fn get_smooth_horizontal_glyphs_for_colored_floating_square(
        pos: FPoint,
        color: RGB8,
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
                output[i][c] = Some(Glyph::colored_character_square_with_offset(
                    false,
                    x_offset - x as f32,
                    color,
                    BLACK,
                ));
            }
        }

        return output;
    }
    #[allow(dead_code)]
    pub fn get_smooth_vertical_glyphs_for_floating_square(pos: FPoint) -> Vec<Vec<Option<Glyph>>> {
        Glyph::get_smooth_vertical_glyphs_for_colored_floating_square(pos, WHITE)
    }
    pub fn get_smooth_vertical_glyphs_for_colored_floating_square(
        pos: FPoint,
        color: RGB8,
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
                output[c][j] = Some(Glyph::colored_character_square_with_offset(
                    true,
                    y_offset - y as f32,
                    color,
                    BLACK,
                ));
            }
        }
        return output;
    }

    pub fn get_half_grid_glyphs_for_floating_square(
        pos: default::Point2D<f32>,
    ) -> Vec<Vec<Option<Glyph>>> {
        Glyph::get_half_grid_glyphs_for_colored_floating_square(pos, WHITE)
    }

    pub fn get_half_grid_glyphs_for_colored_floating_square(
        pos: FPoint,
        color: RGB8,
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
                    let glyph = Glyph::colored_square_with_half_step_offset(
                        (grid_offset - square.to_f32()),
                        color,
                    );
                    if glyph.character != ' ' {
                        output[i][j] = Some(glyph);
                    }
                }
            }
        }
        return output;
    }

    pub fn braille_array_to_braille_char(input: BrailleArray) -> char {
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

    pub fn braille_bit_for_pos(p: Point2D<i32, BrailleGridInWorldFrame>) -> u32 {
        let braille_value_map = vec![vec![7, 3, 2, 1], vec![8, 6, 5, 4]];
        1 << (braille_value_map[p.x as usize][p.y as usize] - 1)
    }

    pub fn add_braille_dot(character: char, p: Point2D<i32, BrailleGridInWorldFrame>) -> char {
        char::from_u32(character as u32 | Glyph::braille_bit_for_pos(p)).unwrap()
    }

    pub fn is_braille(&self) -> bool {
        Glyph::char_is_braille(self.character)
    }

    pub fn char_is_braille(c: char) -> bool {
        let x = c as u32;
        // The unicode braille block
        x >= 0x2800 && x <= 0x28FF
    }

    pub fn count_braille_dots(character: char) -> u32 {
        if !Glyph::char_is_braille(character) {
            return 0;
        }
        let num_good_bits = 8;
        let mut sum = 0;
        let bits = character as u32;
        for i in 0..num_good_bits {
            sum += (bits >> i) & 1;
        }
        return sum as u32;
    }

    pub fn combine_braille_characters(c1: char, c2: char) -> char {
        assert!(Glyph::char_is_braille(c1));
        assert!(Glyph::char_is_braille(c2));
        char::from_u32(c1 as u32 | c2 as u32).unwrap()
    }

    pub fn get_glyphs_for_braille_line(
        start_pos: WorldPoint,
        end_pos: WorldPoint,
    ) -> WorldCharacterGlyphMap {
        Glyph::get_glyphs_for_colored_braille_line(start_pos, end_pos, WHITE)
    }

    pub fn get_glyphs_for_player(faced_direction: WorldStep) -> [Glyph; 2] {
        let mut arrow_step_map: HashMap<WorldStep, char> = HashMap::new();

        // ⭠⭢⭡⭣ ⭦⭧⭨⭩
        let arrows = "🢀🢂🢁🢃🢄🢅🢆🢇";
        let king_steps_in_arrow_order = vec![
            vec2(-1, 0),
            vec2(1, 0),
            vec2(0, 1),
            vec2(0, -1),
            vec2(-1, 1),
            vec2(1, 1),
            vec2(1, -1),
            vec2(-1, -1),
        ];
        for i in 0..king_steps_in_arrow_order.len() {
            let arrow_char = arrows.chars().nth(i).unwrap();
            arrow_step_map.insert(*king_steps_in_arrow_order.get(i).unwrap(), arrow_char);
        }

        let mut glyphs = [
            Glyph::from_char(*arrow_step_map.get(&faced_direction).unwrap_or(&'X')),
            Glyph::from_char(' '),
        ];
        glyphs[0].fg_color = PLAYER_GREEN;

        glyphs
    }

    pub fn world_character_point_to_braille_point(
        pos: Point2D<f32, CharacterGridInWorldFrame>,
    ) -> Point2D<f32, BrailleGridInWorldFrame> {
        point2(pos.x * 2.0 + 0.5, pos.y * 4.0 + 1.5)
    }

    pub fn braille_pos_to_character_world_pos(
        pos: Point2D<f32, BrailleGridInWorldFrame>,
    ) -> Point2D<f32, CharacterGridInWorldFrame> {
        point2((pos.x - 0.5) / 2.0, (pos.y - 1.5) / 4.0)
    }

    pub fn world_point_to_world_character_point(
        pos: Point2D<f32, SquareGridInWorldFrame>,
    ) -> Point2D<f32, CharacterGridInWorldFrame> {
        point2(pos.x * 2.0 + 0.5, pos.y)
    }

    pub fn world_character_point_to_world_point(
        pos: Point2D<f32, CharacterGridInWorldFrame>,
    ) -> Point2D<f32, SquareGridInWorldFrame> {
        point2((pos.x - 0.5) / 2.0, pos.y)
    }

    pub fn world_character_square_to_world_square(pos: WorldCharacterSquare) -> WorldSquare {
        Glyph::world_character_point_to_world_point(pos.to_f32())
            .round()
            .to_i32()
    }

    pub fn world_square_to_left_world_character_square(
        world_square: WorldSquare,
    ) -> WorldCharacterSquare {
        (Glyph::world_point_to_world_character_point(world_square.to_f32()) + vec2(-0.5, 0.0))
            .round()
            .to_i32()
    }

    pub fn world_point_to_world_braille_point(pos: WorldPoint) -> WorldBraillePoint {
        Glyph::world_character_point_to_braille_point(Glyph::world_point_to_world_character_point(
            pos,
        ))
    }
    pub fn world_braille_point_to_world_point(pos: WorldBraillePoint) -> WorldPoint {
        Glyph::world_character_point_to_world_point(
            Glyph::world_braille_point_to_world_character_point(pos),
        )
    }

    pub fn braille_square_to_dot_in_character(
        pos: Point2D<i32, BrailleGridInWorldFrame>,
    ) -> Point2D<i32, BrailleGridInWorldFrame> {
        point2((pos.x % 2).abs(), (pos.y % 4).abs())
    }

    pub fn world_braille_point_to_world_character_point(
        braille_point: WorldBraillePoint,
    ) -> WorldCharacterPoint {
        point2(
            (braille_point.x as f32 - 0.5) / 2.0,
            (braille_point.y as f32 - 1.5) / 4.0,
        )
    }

    pub fn world_braille_square_to_world_character_square(
        braille_square: WorldBrailleSquare,
    ) -> WorldCharacterSquare {
        Glyph::world_braille_point_to_world_character_point(braille_square.to_f32())
            .round()
            .to_i32()
    }

    pub fn world_points_for_braille_line(
        start_pos: WorldPoint,
        end_pos: WorldPoint,
    ) -> Vec<WorldPoint> {
        let braille_start_square = Glyph::world_point_to_world_braille_point(start_pos)
            .round()
            .to_i32();
        let braille_end_square = Glyph::world_point_to_world_braille_point(end_pos)
            .round()
            .to_i32();

        line_drawing::Bresenham::new(
            braille_start_square.to_tuple(),
            braille_end_square.to_tuple(),
        )
        .map(|(x, y)| WorldBraillePoint::new(x as f32, y as f32))
        .map(Glyph::world_braille_point_to_world_point)
        .collect()
    }

    pub fn get_glyphs_for_colored_braille_line(
        start_pos: WorldPoint,
        end_pos: WorldPoint,
        color: RGB8,
    ) -> WorldCharacterGlyphMap {
        let start_char_point = Glyph::world_point_to_world_character_point(start_pos);
        let end_char_point = Glyph::world_point_to_world_character_point(end_pos);

        let mut glyph_map = HashMap::<Point2D<i32, CharacterGridInWorldFrame>, Glyph>::new();

        let start_braille_grid_square =
            Glyph::world_character_point_to_braille_point(start_char_point)
                .round()
                .to_i32();
        let end_braille_grid_square = Glyph::world_character_point_to_braille_point(end_char_point)
            .round()
            .to_i32();

        for (x, y) in line_drawing::Bresenham::new(
            start_braille_grid_square.to_tuple(),
            end_braille_grid_square.to_tuple(),
        ) {
            let braille_pos = Point2D::<i32, BrailleGridInWorldFrame>::new(x, y);
            let character_grid_square =
                Glyph::world_braille_square_to_world_character_square(braille_pos);
            if !glyph_map.contains_key(&character_grid_square) {
                let mut new_glyph = Glyph::new(EMPTY_BRAILLE, color, BLACK);
                new_glyph.bg_alpha = 0;
                glyph_map.insert(character_grid_square, new_glyph);
            }
            let braille_character =
                &mut glyph_map.get_mut(&character_grid_square).unwrap().character;
            *braille_character = Glyph::add_braille_dot(
                *braille_character,
                Glyph::braille_square_to_dot_in_character(braille_pos),
            );
        }
        return glyph_map;
    }

    pub fn danger_square_glyphs() -> TwoGlyphs {
        [
            Glyph::fg_only(DANGER_SQUARE_CHARS[0], RED),
            Glyph::fg_only(' ', RED),
        ]
    }

    pub fn character_world_pos_to_braille_char(
        world_pos: Point2D<f32, CharacterGridInWorldFrame>,
    ) -> char {
        let character = EMPTY_BRAILLE;
        Glyph::add_braille_dot(
            character,
            Glyph::braille_square_to_dot_in_character(
                Glyph::world_character_point_to_braille_point(world_pos)
                    .round()
                    .to_i32(),
            ),
        )
    }

    pub fn character_world_pos_to_colored_braille_glyph(
        world_pos: Point2D<f32, CharacterGridInWorldFrame>,
        color: RGB8,
    ) -> Glyph {
        Glyph::new(
            Glyph::character_world_pos_to_braille_char(world_pos),
            color,
            BLACK,
        )
    }

    pub fn points_to_braille_glyphs(
        points: Vec<WorldPoint>,
        color: RGB8,
    ) -> WorldCharacterGlyphMap {
        // bin braille squares by world character squares
        let mut local_braille_squares_by_character_square =
            HashMap::<WorldCharacterSquare, HashSet<WorldBrailleSquare>>::new();

        for point in points {
            let char_point = Glyph::world_point_to_world_character_point(point);
            let char_square = char_point.round().to_i32();
            let braille_square = Glyph::world_character_point_to_braille_point(char_point)
                .round()
                .to_i32();
            let local_braille_square = Glyph::braille_square_to_dot_in_character(braille_square);

            if !local_braille_squares_by_character_square.contains_key(&char_square) {
                local_braille_squares_by_character_square
                    .insert(char_square, HashSet::<WorldBrailleSquare>::new());
            }
            local_braille_squares_by_character_square
                .get_mut(&char_square)
                .unwrap()
                .insert(local_braille_square);
        }

        let mut output_map = WorldCharacterGlyphMap::new();

        for (char_square, braille_square_set) in local_braille_squares_by_character_square {
            let braille_char: char = Glyph::local_braille_squares_to_braille_char(
                braille_square_set.into_iter().collect(),
            );
            let mut braille_glyph = Glyph::new(braille_char, color, BLACK);
            braille_glyph.bg_alpha = 0;
            output_map.insert(char_square, braille_glyph);
        }
        output_map
    }

    fn local_braille_squares_to_braille_array(squares: Vec<WorldBrailleSquare>) -> BrailleArray {
        let mut output_array: BrailleArray = [[false; 4]; 2];
        for square in squares {
            assert!(square.x >= 0 || square.x < 2);
            assert!(square.y >= 0 || square.y < 4);
            output_array[square.x as usize][square.y as usize] = true;
        }
        output_array
    }

    fn local_braille_squares_to_braille_char(squares: Vec<WorldBrailleSquare>) -> char {
        Glyph::braille_array_to_braille_char(Glyph::local_braille_squares_to_braille_array(squares))
    }

    pub fn looks_solid(&self, color: RGB8) -> bool {
        if let Some(solid_color) = self.get_solid_color() {
            color == solid_color
        } else {
            false
        }
    }

    pub fn is_solid(&self) -> bool {
        self.get_solid_color() != None
    }

    pub fn get_solid_color(&self) -> Option<RGB8> {
        if KNOWN_FG_ONLY_CHARS.contains(&self.character) {
            Some(self.fg_color)
        } else if KNOWN_BG_ONLY_CHARS.contains(&self.character) {
            Some(self.bg_color)
        } else {
            None
        }
    }
}

#[cfg(test)]
mod tests {
    use pretty_assertions::{assert_eq, assert_ne};

    use super::*;

    #[test]
    fn test_colored_square_with_half_step_offsets() {
        assert_eq!(
            Glyph::colored_square_with_half_step_offset(vec2(0.0, 0.0), RED).character,
            quarter_block_by_offset(vec2(0, 0))
        );
        assert_eq!(
            Glyph::colored_square_with_half_step_offset(vec2(0.0, 0.0), RED).fg_color,
            RED
        );
        assert_eq!(
            Glyph::colored_square_with_half_step_offset(vec2(0.0, 0.0), RED).bg_color,
            BLACK
        );
        assert_eq!(
            Glyph::colored_square_with_half_step_offset(vec2(0.1, 0.1), RED).character,
            quarter_block_by_offset(vec2(0, 0))
        );
        assert_eq!(
            Glyph::colored_square_with_half_step_offset(vec2(0.24, 0.0), RED).character,
            quarter_block_by_offset(vec2(0, 0))
        );
        assert_eq!(
            Glyph::colored_square_with_half_step_offset(vec2(0.25, 0.0), RED).character,
            quarter_block_by_offset(vec2(1, 0))
        );
        assert_eq!(
            Glyph::colored_square_with_half_step_offset(vec2(0.26, 0.0), RED).character,
            quarter_block_by_offset(vec2(1, 0))
        );
        assert_eq!(
            Glyph::colored_square_with_half_step_offset(vec2(-0.25, 0.0), RED).character,
            quarter_block_by_offset(vec2(0, 0))
        );
        assert_eq!(
            Glyph::colored_square_with_half_step_offset(vec2(-0.26, 0.0), RED).character,
            quarter_block_by_offset(vec2(-1, 0))
        );
        assert_eq!(
            Glyph::colored_square_with_half_step_offset(vec2(0.49, 0.0), RED).character,
            quarter_block_by_offset(vec2(1, 0))
        );
        assert_eq!(
            Glyph::colored_square_with_half_step_offset(vec2(0.5, 0.0), RED).character,
            quarter_block_by_offset(vec2(1, 0))
        );
        assert_eq!(
            Glyph::colored_square_with_half_step_offset(vec2(0.2, 0.4), RED).character,
            quarter_block_by_offset(vec2(0, 1))
        );
        assert_eq!(
            Glyph::colored_square_with_half_step_offset(vec2(-0.499, 0.4), RED).character,
            quarter_block_by_offset(vec2(-1, 1))
        );
        assert_eq!(
            Glyph::colored_square_with_half_step_offset(vec2(0.74, 0.0), RED).character,
            quarter_block_by_offset(vec2(1, 0))
        );
        assert_eq!(
            Glyph::colored_square_with_half_step_offset(vec2(0.76, 0.0), RED).character,
            quarter_block_by_offset(vec2(2, 0))
        );
        assert_eq!(
            Glyph::colored_square_with_half_step_offset(vec2(0.3, -0.6), RED).character,
            quarter_block_by_offset(vec2(1, -1))
        );
    }

    #[test]
    fn test_half_grid_glyph_when_rounding_to_zero_for_both_axes() {
        let test_pos = point2(-0.24, 0.01);
        let glyphs = Glyph::get_half_grid_glyphs_for_floating_square(test_pos);
        assert!(glyphs[0][0] == None);
        assert!(glyphs[0][1] == None);
        assert!(glyphs[0][2] == None);
        assert!(glyphs[1][0] == None);
        assert_eq!(
            glyphs[1][1].clone().unwrap().character,
            quarter_block_by_offset(vec2(0, 0))
        );
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
        assert_eq!(
            glyphs[1][1].clone().unwrap().character,
            quarter_block_by_offset(vec2(0, 1))
        );
        assert_eq!(
            glyphs[1][2].clone().unwrap().character,
            quarter_block_by_offset(vec2(0, -1))
        );
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
        assert_eq!(
            glyphs[1][1].clone().unwrap().character,
            quarter_block_by_offset(vec2(0, 1))
        );
        assert_eq!(
            glyphs[1][2].clone().unwrap().character,
            quarter_block_by_offset(vec2(0, -1))
        );
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
        assert_eq!(
            glyphs[1][1].clone().unwrap().character,
            quarter_block_by_offset(vec2(0, 0))
        );
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
        assert_eq!(
            glyphs[1][1].clone().unwrap().character,
            quarter_block_by_offset(vec2(1, 0))
        );
        assert!(glyphs[1][2] == None);
        assert!(glyphs[2][0] == None);
        assert_eq!(
            glyphs[2][1].clone().unwrap().character,
            quarter_block_by_offset(vec2(-1, 0))
        );
        assert!(glyphs[2][2] == None);
    }

    #[test]
    fn test_half_grid_glyphs_when_rounding_to_zero_for_y_and_half_step_left_for_x() {
        let test_pos = point2(-0.3, 0.2);
        let glyphs = Glyph::get_half_grid_glyphs_for_floating_square(test_pos);
        assert!(glyphs[0][0] == None);
        assert_eq!(
            glyphs[0][1].clone().unwrap().character,
            quarter_block_by_offset(vec2(1, 0))
        );
        assert!(glyphs[0][2] == None);
        assert!(glyphs[1][0] == None);
        assert_eq!(
            glyphs[1][1].clone().unwrap().character,
            quarter_block_by_offset(vec2(-1, 0))
        );
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
        assert_eq!(Glyph::braille_array_to_braille_char(input), '⢡');

        // 00
        // 11
        // 01
        // 00
        let input = [[false, false, true, false], [false, true, true, false]];
        assert_eq!(Glyph::braille_array_to_braille_char(input), '⠲');
    }

    #[test]
    fn test_set_braille_dot() {
        let mut b = EMPTY_BRAILLE;
        b = Glyph::add_braille_dot(b, point2(0, 0));
        b = Glyph::add_braille_dot(b, point2(1, 1));
        assert_eq!(b, '⡠');
    }

    #[test]
    fn test_chars_for_horizontal_braille_line_without_rounding() {
        let start: WorldCharacterPoint = point2(-0.25, -0.4);
        let end: WorldCharacterPoint = point2(1.75, -0.4);

        // Expected braille:
        // 00 00 00
        // 00 00 00
        // 00 00 00
        // 11 11 10

        let line_glyphs = Glyph::get_glyphs_for_braille_line(
            Glyph::world_character_point_to_world_point(start),
            Glyph::world_character_point_to_world_point(end),
        );
        assert_eq!(line_glyphs.len(), 3);

        assert_eq!(
            line_glyphs.get(&point2(0, 0)).unwrap().character,
            '\u{28C0}'
        );
        assert_eq!(
            line_glyphs.get(&point2(1, 0)).unwrap().character,
            '\u{28C0}'
        );
        assert_eq!(
            line_glyphs.get(&point2(2, 0)).unwrap().character,
            '\u{2840}'
        );
    }

    #[test]
    fn test_chars_for_horizontal_braille_line_with_offset_without_rounding() {
        let start = WorldCharacterPoint::new(-0.25, 0.4);
        let end = WorldCharacterPoint::new(1.75, 0.4);

        // Expected braille:
        // 11 11 10
        // 00 00 00
        // 00 00 00
        // 00 00 00

        let line_glyphs = Glyph::get_glyphs_for_braille_line(
            Glyph::world_character_point_to_world_point(start),
            Glyph::world_character_point_to_world_point(end),
        );
        assert_eq!(line_glyphs.len(), 3);

        assert_eq!(
            line_glyphs.get(&point2(0, 0)).unwrap().character,
            '\u{2809}'
        );
        assert_eq!(
            line_glyphs.get(&point2(1, 0)).unwrap().character,
            '\u{2809}'
        );
        assert_eq!(
            line_glyphs.get(&point2(2, 0)).unwrap().character,
            '\u{2801}'
        );
    }

    #[test]
    fn test_chars_for_vertical_braille_line_without_rounding() {
        let start = WorldCharacterPoint::new(-0.25, -0.4);
        let end = WorldCharacterPoint::new(-0.25, 0.875);

        // Expected braille:
        // 00
        // 00
        // 10
        // 10

        // 10
        // 10
        // 10
        // 10

        let line_glyphs = Glyph::get_glyphs_for_braille_line(
            Glyph::world_character_point_to_world_point(start),
            Glyph::world_character_point_to_world_point(end),
        );
        assert_eq!(line_glyphs.len(), 2);

        assert_eq!(
            line_glyphs.get(&point2(0, 0)).unwrap().character,
            '\u{2847}'
        );
        assert_eq!(
            line_glyphs.get(&point2(0, 1)).unwrap().character,
            '\u{2844}'
        );
    }

    #[test]
    fn test_braille_grid_to_character_grid() {
        assert_eq!(
            Glyph::world_braille_square_to_world_character_square(point2(0, 0)),
            point2(0, 0)
        );
        assert_eq!(
            Glyph::world_braille_square_to_world_character_square(point2(1, 3)),
            point2(0, 0)
        );
        assert_eq!(
            Glyph::world_braille_square_to_world_character_square(point2(-1, -1)),
            point2(-1, -1)
        );
        assert_eq!(
            Glyph::world_braille_square_to_world_character_square(point2(2, 8)),
            point2(1, 2)
        );
        assert_eq!(
            Glyph::world_braille_square_to_world_character_square(point2(21, 80)),
            point2(10, 20)
        );
    }

    #[test]
    fn test_world_pos_to_braille_pos() {
        assert_eq!(
            Glyph::world_character_point_to_braille_point(point2(0.0, 0.0)),
            point2(0.5, 1.5)
        );
        assert_eq!(
            Glyph::world_character_point_to_braille_point(point2(1.0, 0.0)),
            point2(2.5, 1.5)
        );
        assert_eq!(
            Glyph::world_character_point_to_braille_point(point2(0.25, 0.375)),
            point2(1.0, 3.0)
        );
    }

    #[test]
    fn test_braille_pos_to_world_pos() {
        assert_eq!(
            Glyph::braille_pos_to_character_world_pos(point2(0.5, 1.5)),
            point2(0.0, 0.0)
        );
        assert_eq!(
            Glyph::braille_pos_to_character_world_pos(point2(2.5, 1.5)),
            point2(1.0, 0.0)
        );
        assert_eq!(
            Glyph::braille_pos_to_character_world_pos(point2(1.0, 3.0)),
            point2(0.25, 0.375)
        );
    }

    #[test]
    fn test_braille_square_to_dot_in_character() {
        assert_eq!(
            Glyph::braille_square_to_dot_in_character(point2(0, 0)),
            point2(0, 0)
        );
        assert_eq!(
            Glyph::braille_square_to_dot_in_character(point2(1, 3)),
            point2(1, 3)
        );
        assert_eq!(
            Glyph::braille_square_to_dot_in_character(point2(25, 4)),
            point2(1, 0)
        );
        assert_eq!(
            Glyph::braille_square_to_dot_in_character(point2(-3, 4)),
            point2(1, 0)
        );
    }

    #[test]
    fn test_combine_braille_character() {
        assert_eq!(
            Glyph::combine_braille_characters('\u{2800}', '\u{2820}'),
            '\u{2820}'
        );
        assert_eq!(
            Glyph::combine_braille_characters('\u{2801}', '\u{28C0}'),
            '\u{28C1}'
        );
    }

    #[test]
    fn test_world_point_to_braille_char() {
        assert_eq!(
            Glyph::character_world_pos_to_braille_char(point2(0.0, 0.0)),
            '\u{2810}'
        );
        assert_eq!(
            Glyph::character_world_pos_to_braille_char(point2(-0.4, -0.4)),
            '\u{2840}'
        );
        assert_eq!(
            Glyph::character_world_pos_to_braille_char(point2(0.2, 0.4)),
            '\u{2808}'
        );
    }

    #[test]
    fn test_world_point_to_braille_char_is_always_braille() {
        for _ in 0..200 {
            //let random_point = p(rand_in_range(0.0, 30.0), rand_in_range(0.0, 30.0));
            let random_point = point2(23.2273, 2.05);

            assert!(Glyph::char_is_braille(
                Glyph::character_world_pos_to_braille_char(random_point)
            ));
        }
    }

    #[test]
    fn test_world_point_to_braille_glyph() {
        let points = [point2(0.0, 0.0), point2(-0.4, -0.4), point2(0.2, 0.4)];
        for p1 in points {
            assert_eq!(
                Glyph::character_world_pos_to_colored_braille_glyph(p1, BLACK).character,
                Glyph::character_world_pos_to_braille_char(p1)
            );
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

    #[test]
    fn test_world_pos_to_character_world_pos() {
        assert_eq!(
            Point2D::<f32, CharacterGridInWorldFrame>::new(0.5, 0.0),
            Glyph::world_point_to_world_character_point(
                Point2D::<f32, SquareGridInWorldFrame>::new(0.0, 0.0)
            ),
            "zero is actually between two characters"
        );
        assert_eq!(
            Point2D::<f32, CharacterGridInWorldFrame>::new(2.5, 1.0),
            Glyph::world_point_to_world_character_point(
                Point2D::<f32, SquareGridInWorldFrame>::new(1.0, 1.0)
            ),
            "diagonal a bit"
        );
    }

    //⠁⠂⠃⠄⠅⠆⠇⠈⠉⠊⠋⠌⠍⠎⠏⠐⠑⠒⠓⠔⠕⠖⠗⠘⠙⠚⠛⠜⠝⠞⠟⠠⠡⠢⠣⠤⠥⠦⠧⠨⠩⠪⠫⠬⠭⠮⠯⠰⠱⠲⠳⠴⠵⠶⠷⠸⠹⠺⠻⠼⠽⠾⠿⡀⡁⡂⡃⡄⡅⡆⡇⡈⡉⡊⡋⡌⡍⡎⡏⡐⡑⡒⡓⡔⡕⡖⡗⡘⡙⡚⡛⡜⡝⡞⡟⡠⡡⡢⡣⡤⡥⡦⡧⡨⡩⡪⡫⡬⡭⡮⡯⡰⡱⡲⡳⡴⡵⡶⡷⡸⡹⡺⡻⡼⡽⡾⡿⢀⢁⢂⢃⢄⢅⢆⢇⢈⢉⢊⢋⢌⢍⢎⢏⢐⢑⢒⢓⢔⢕⢖⢗⢘⢙⢚⢛⢜⢝⢞⢟⢠⢡⢢⢣⢤⢥⢦⢧⢨⢩⢪⢫⢬⢭⢮⢯⢰⢱⢲⢳⢴⢵⢶⢷⢸⢹⢺⢻⢼⢽⢾⢿⣀⣁⣂⣃⣄⣅⣆⣇⣈⣉⣊⣋⣌⣍⣎⣏⣐⣑⣒⣓⣔⣕⣖⣗⣘⣙⣚⣛⣜⣝⣞⣟⣠⣡⣢⣣⣤⣥⣦⣧⣨⣩⣪⣫⣬⣭⣮⣯⣰⣱⣲⣳⣴⣵⣶⣷⣸⣹⣺⣻⣼⣽⣾⣿

    #[test]
    fn test_points_to_braille_glyphs() {
        // ┌──┬──┐┌──┬──┐
        // │  │  ││  │  │
        // │  │o ││  │oo│
        // │ o│  ││  │  │
        // │  │  ││  │  │
        // └──┴──┘└──┴──┘

        // 00 00  00 00
        // 00 10  00 11
        // 01 00  00 00
        // 00 00  00 00

        let points = vec![
            WorldPoint::new(0.1, 0.1),
            WorldPoint::new(0.1, 0.1), // duplicate for funsies
            WorldPoint::new(-0.1, -0.1),
            WorldPoint::new(1.1, 0.1),
            WorldPoint::new(1.4, 0.1),
        ];

        let glyphs = Glyph::points_to_braille_glyphs(points, WHITE);

        assert_eq!(glyphs.len(), 3);
        assert_eq!(glyphs.get(&point2(0, 0)).unwrap().character, '⠠');
        assert_eq!(glyphs.get(&point2(1, 0)).unwrap().character, '⠂');
        assert_eq!(glyphs.get(&point2(3, 0)).unwrap().character, '⠒');
    }

    #[test]
    fn test_double_glyph_square_offset__up() {
        // offset up
        let glyphs = Glyph::offset_board_square_glyphs(vec2(0.0, 0.5), RED, BLACK);
        assert_eq!(glyphs[0].character, '▄');
        assert_eq!(glyphs[0].fg_color, BLACK);
        assert_eq!(glyphs[0].bg_color, RED);
        assert_eq!(glyphs[1].character, '▄');
        assert_eq!(glyphs[1].fg_color, BLACK);
        assert_eq!(glyphs[1].bg_color, RED);
    }

    #[test]
    fn test_double_glyph_square_offset__150_up() {
        // offset up
        let glyphs = Glyph::offset_board_square_glyphs(vec2(0.0, 1.5), RED, BLACK);
        assert_eq!(glyphs[0].character, '▄');
        assert_eq!(glyphs[0].fg_color, RED);
        assert_eq!(glyphs[0].bg_color, BLACK);
        assert_eq!(glyphs[1].character, '▄');
        assert_eq!(glyphs[1].fg_color, RED);
        assert_eq!(glyphs[1].bg_color, BLACK);
    }

    #[test]
    fn test_double_glyph_square_offset__50_down() {
        // offset down
        let glyphs = Glyph::offset_board_square_glyphs(vec2(0.0, -0.5), RED, BLACK);
        assert_eq!(glyphs[0].character, '▄');
        assert_eq!(glyphs[0].fg_color, RED);
        assert_eq!(glyphs[0].bg_color, BLACK);
        assert_eq!(glyphs[1].character, '▄');
        assert_eq!(glyphs[1].fg_color, RED);
        assert_eq!(glyphs[1].bg_color, BLACK);
    }

    //                      |<--halfway
    // ' ', '▏', '▎', '▍', '▌', '▋', '▊', '▉', '█'
    #[test]
    fn test_double_glyph_square_offset__1_left() {
        // offset left
        let glyphs = Glyph::offset_board_square_glyphs(vec2(-0.01, 0.0), RED, BLACK);
        assert!(glyphs[0].looks_solid(RED));
        assert!(glyphs[1].looks_solid(RED));
    }

    #[test]
    fn test_double_glyph_square_offset__14_left() {
        // offset left
        let glyphs = Glyph::offset_board_square_glyphs(vec2(-0.14, 0.0), RED, BLACK);
        assert!(
            glyphs[0].looks_solid(RED),
            "glyph: {}",
            &glyphs[0].to_string()
        );
        //assert_eq!(glyphs[1].character, '▏');
        assert_eq!(glyphs[1], Glyph::new('▊', RED, BLACK,));
    }

    #[test]
    fn test_double_glyph_square_offset__25_left() {
        // offset left
        let glyphs = Glyph::offset_board_square_glyphs(vec2(-0.25, 0.0), RED, BLACK);
        assert!(glyphs[0].looks_solid(RED));
        assert_eq!(glyphs[1].character, '▌');
        assert_eq!(glyphs[1].fg_color, RED);
        assert_eq!(glyphs[1].bg_color, BLACK);
    }

    #[test]
    fn test_double_glyph_square_offset__75_left() {
        // offset left
        let glyphs = Glyph::offset_board_square_glyphs(vec2(-0.75, 0.0), RED, BLACK);
        assert_eq!(glyphs[0].character, '▌');
        assert_eq!(glyphs[0].fg_color, RED);
        assert_eq!(glyphs[0].bg_color, BLACK);
        assert!(glyphs[1].looks_solid(BLACK));
    }

    #[test]
    fn test_double_glyph_square_offset__25_right() {
        // offset right
        let glyphs = Glyph::offset_board_square_glyphs(vec2(0.25, 0.0), RED, BLACK);
        assert_eq!(glyphs[0].character, '▌');
        assert_eq!(glyphs[0].fg_color, BLACK);
        assert_eq!(glyphs[0].bg_color, RED);
        assert!(glyphs[1].looks_solid(RED));
    }

    #[test]
    fn test_double_glyph_square_offset__50_right() {
        // offset right
        let glyphs = Glyph::offset_board_square_glyphs(vec2(0.50, 0.0), RED, BLACK);
        assert!(glyphs[0].looks_solid(BLACK));
        assert!(glyphs[1].looks_solid(RED));
    }

    #[test]
    fn test_double_glyph_square_offset__75_right() {
        // offset right
        let glyphs = Glyph::offset_board_square_glyphs(vec2(0.75, 0.0), RED, BLACK);
        assert_eq!(glyphs[0].character, '█');
        assert_eq!(glyphs[0].fg_color, BLACK);
        assert_eq!(glyphs[0].bg_color, RED);
        assert_eq!(glyphs[1].character, '▌');
        assert_eq!(glyphs[1].fg_color, BLACK);
        assert_eq!(glyphs[1].bg_color, RED);
    }

    #[test]
    fn test_double_glyph_square_offset__slightly_past_full_square_right() {
        // offset right
        let glyphs = Glyph::offset_board_square_glyphs(vec2(1.02, 0.0), RED, BLACK);
        assert!(
            glyphs[0].looks_solid(BLACK),
            "glyph: {}",
            &glyphs[0].to_string()
        );
        assert!(
            glyphs[1].looks_solid(BLACK),
            "glyph: {}",
            &glyphs[1].to_string()
        );
    }

    #[test]
    fn test_double_glyph_square_offset__partial_character_past_full_square_right() {
        // offset right
        let glyphs = Glyph::offset_board_square_glyphs(vec2(1.25, 0.0), RED, BLACK);
        assert_eq!(glyphs[0], Glyph::new(HORIZONTAL_HALF_BLOCK, RED, BLACK,));
        assert!(
            glyphs[1].looks_solid(BLACK),
            "glyph: {}",
            &glyphs[1].to_string()
        );
    }

    //                      |<--halfway
    // ' ', '▏', '▎', '▍', '▌', '▋', '▊', '▉', '█'
    #[test]
    fn test_character_square_horizontal_offset__base_case() {
        assert!(
            Glyph::colored_character_square_with_offset(false, 0.0, RED, BLACK).looks_solid(RED)
        );
    }

    #[test]
    fn test_character_square_horizontal_offset__round_to_zero() {
        assert!(
            Glyph::colored_character_square_with_offset(false, -0.001, RED, BLACK).looks_solid(RED)
        );

        assert!(
            Glyph::colored_character_square_with_offset(false, 0.001, RED, BLACK).looks_solid(RED)
        );
    }

    #[test]
    fn test_character_square_horizontal_offset__out_of_range() {
        assert_eq!(
            Glyph::colored_character_square_with_offset(false, -1.5, RED, BLACK),
            Glyph::new(' ', RED, BLACK,)
        );
        assert_eq!(
            Glyph::colored_character_square_with_offset(false, 1.5, RED, BLACK),
            Glyph::new('█', BLACK, RED,)
        );
    }

    #[test]
    fn test_character_square_horizontal_offset__halfway() {
        assert_eq!(
            Glyph::colored_character_square_with_offset(false, -0.5, RED, BLACK),
            Glyph::new('▌', RED, BLACK,)
        );
        assert_eq!(
            Glyph::colored_character_square_with_offset(false, 0.5, RED, BLACK),
            Glyph::new('▌', BLACK, RED,)
        );
    }

    #[test]
    fn test_character_square_horizontal_offset__match_opposite_ends() {
        assert!(
            Glyph::colored_character_square_with_offset(false, -1.0, RED, BLACK).looks_solid(BLACK)
        );
        assert!(
            Glyph::colored_character_square_with_offset(false, 1.0, RED, BLACK).looks_solid(BLACK)
        );
    }

    #[test]
    fn test_square_offset_to_character_offsets__zeros() {
        assert_eq!(
            Glyph::horizontal_square_offset_to_character_offsets(0.0),
            [0.0, 0.0]
        );
    }

    #[test]
    fn test_square_offset_to_character_offsets__1_4_left() {
        assert_eq!(
            Glyph::horizontal_square_offset_to_character_offsets(-0.25),
            [0.0, -0.5]
        );
    }

    #[test]
    fn test_square_offset_to_character_offsets__1_4_right() {
        assert_eq!(
            Glyph::horizontal_square_offset_to_character_offsets(0.25),
            [0.5, 0.0]
        );
    }

    #[test]
    fn test_square_offset_to_character_offsets__1_2_right() {
        assert_eq!(
            Glyph::horizontal_square_offset_to_character_offsets(0.5),
            [1.0, 0.0]
        );
    }

    #[test]
    fn test_square_offset_to_character_offsets__1_2_left() {
        assert_eq!(
            Glyph::horizontal_square_offset_to_character_offsets(-0.5),
            [0.0, -1.0]
        );
    }

    #[test]
    fn test_square_offset_to_character_offsets__1_left() {
        assert_eq!(
            Glyph::horizontal_square_offset_to_character_offsets(-1.0),
            [-1.0, -1.0]
        );
    }

    #[test]
    fn test_square_offset_to_character_offsets__1_right() {
        assert_eq!(
            Glyph::horizontal_square_offset_to_character_offsets(1.0),
            [1.0, 1.0]
        );
    }

    #[test]
    fn test_get_solid_color_if_there_is_one() {
        let glyph = Glyph::new(' ', BLUE, RED);
        assert_eq!(glyph.get_solid_color(), Some(RED));
        let glyph = Glyph::new(FULL_BLOCK, BLUE, RED);
        assert_eq!(glyph.get_solid_color(), Some(BLUE));
    }

    #[test]
    fn test_can_not_get_solid_color_if_there_is_not_one() {
        let glyph = Glyph::new('a', BLUE, RED);
        assert_eq!(glyph.get_solid_color(), None);
    }

    #[test]
    fn test_braille_line_has_transparent_background() {
        let glyph_map =
            Glyph::get_glyphs_for_colored_braille_line(point2(1.0, 1.0), point2(3.0, 30.0), RED);
        assert!(glyph_map.values().all(|glyph| glyph.bg_alpha == 0))
    }
}
