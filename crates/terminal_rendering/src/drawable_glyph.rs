use std::collections::HashMap;
use std::fmt::{Debug, Display, Formatter};
use std::hash::Hash;
use crate::DoubleChar;

use euclid::*;
use itertools::Itertools;
use rgb::*;
use termion::color;

use glyph_constants::*;
use utility::geometry2::{IPoint, IPointExt};
use utility::geometry2::{FPoint, FPointExt};
use utility::geometry2::PointExt;

use crate::floating_square::character_for_half_square_with_1d_offset;
use crate::screen::CharacterGridInWorldFrame;
use utility::coordinate_frame_conversions::*;
use utility::*;

pub use crate::angled_blocks::*;
pub use crate::braille::*;
pub use crate::floating_square::*;
pub use crate::glyph::*;
pub use crate::glyph_constants;
pub use crate::hextant_blocks::*;

// x, y
pub type DoubleDrawableGlyph = [DrawableGlyph; 2];

pub const KNOWN_FG_ONLY_CHARS: &[char] = &[FULL_BLOCK];
pub const KNOWN_BG_ONLY_CHARS: &[char] = &[SPACE, EMPTY_BRAILLE];

// Fun unicode for later
// ↈ ▴ ⚠ 🞁 🢑  🛆  𝅉  ⏹  ᙮ ⸼  ▪
// ☠⯃⯄
// ⨻🕱
//   ⃤  ⟁   ⃠   ꙰
//     ◌  𝨞
// ◌  ⚿ ⯐
//    𝩬   𝩫
// ⍾

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

pub type ORGB8 = Option<RGB8>;
pub type DrawableGlyphMap = HashMap<IPoint, DrawableGlyph>;
pub type CharMap = HashMap<IPoint, char>;

#[derive(Clone, PartialEq, Eq, Copy)]
pub struct DrawableGlyph {
    pub character: char,
    pub fg_color: ORGB8,
    pub bg_color: ORGB8,
}

impl Default for DrawableGlyph {
    fn default() -> Self {
        Self::new(' ', None, None)
    }
}

impl DrawableGlyph {
    pub fn new(character: char, fg_color: ORGB8, bg_color: ORGB8) -> DrawableGlyph {
        DrawableGlyph {
            character,
            fg_color,
            bg_color,
        }
    }
    pub fn new_colored(character: char, fg_color: RGB8, bg_color: RGB8) -> DrawableGlyph {
        DrawableGlyph {
            character,
            fg_color: Some(fg_color),
            bg_color: Some(bg_color),
        }
    }

    pub fn fg_only(character: char, fg_color: RGB8) -> DrawableGlyph {
        DrawableGlyph {
            character,
            fg_color: Some(fg_color),
            bg_color: None,
        }
    }
    fn tinted(&self, color: RGB8, strength: f32) -> Self {
        DrawableGlyph {
            fg_color: self.fg_color.map(|c| tint_color(c, color, strength)),
            bg_color: self.bg_color.map(|c| tint_color(c, color, strength)),
            ..*self
        }
    }

    pub fn fg_color_string(&self) -> String {
        if let Some(c) = self.fg_color {
            color::Fg(color::Rgb(c.r, c.g, c.b)).to_string()
        } else {
            color::Fg(color::Reset).to_string()
        }
    }
    pub fn bg_color_string(&self) -> String {
        if let Some(c) = self.bg_color {
            color::Bg(color::Rgb(c.r, c.g, c.b)).to_string()
        } else {
            color::Bg(color::Reset).to_string()
        }
    }
    pub fn color_string(&self) -> String {
        self.bg_color_string() + &self.fg_color_string()
    }
    pub fn fg_color_reset_string() -> String {
        color::Fg(color::Reset).to_string()
    }
    pub fn bg_color_reset_string() -> String {
        color::Bg(color::Reset).to_string()
    }
    pub fn color_reset_string() -> String {
        color::Bg(color::Reset).to_string() + &color::Fg(color::Reset).to_string()
    }

    pub fn render(&self) -> String {
        self.color_string() + &self.character.to_string() + &DrawableGlyph::color_reset_string()
    }
    pub fn to_string_after(&self, prev: Option<Self>) -> String {
        if let Some(prev) = prev {
            let mut output = String::new();
            if self.fg_color != prev.fg_color {
                output += &self.fg_color_string();
            }
            if self.bg_color != prev.bg_color {
                output += &self.bg_color_string();
            }
            output += &self.character.to_string();
            output
        } else {
            self.color_string() + &self.character.to_string()
        }
    }

    pub fn from_char(character: char) -> DrawableGlyph {
        DrawableGlyph::new(character, None, None)
    }

    pub fn with_char(&self, new_char: char) -> DrawableGlyph {
        let mut dup = self.clone();
        dup.character = new_char;
        dup
    }
    pub fn with_fg(&self, new_fg: ORGB8) -> DrawableGlyph {
        let mut dup = self.clone();
        dup.fg_color = new_fg;
        dup
    }
    pub fn with_bg(&self, new_bg: ORGB8) -> DrawableGlyph {
        let mut dup = self.clone();
        dup.bg_color = new_bg;
        dup
    }

    pub fn reset_colors() -> String {
        format!("{}{}", color::Fg(color::Reset), color::Bg(color::Reset),)
    }

    pub fn orthogonally_offset_board_square_glyphs(
        offset_vector: WorldMove,
        square_color: RGB8,
        background_color: RGB8,
    ) -> DoubleDrawableGlyph {
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
            DrawableGlyph::vertical_square_offset_to_character_offsets(
                offset_magnitude_within_one_period_symmetric_about_zero,
            )
            .map(|character_offset| {
                DrawableGlyph::new_colored(
                    character_for_half_square_with_1d_offset(true, character_offset),
                    square_color,
                    background_color,
                )
            })
        } else {
            let character_offsets = DrawableGlyph::horizontal_square_offset_to_character_offsets(
                offset_magnitude_within_one_period_symmetric_about_zero,
            );
            character_offsets.map(|character_offset| {
                DrawableGlyph::new_colored(
                    character_for_half_square_with_1d_offset(false, character_offset),
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

    pub fn is_braille(&self) -> bool {
        char_is_braille(self.character)
    }

    pub fn looks_solid_color(&self, color: RGB8) -> bool {
        if let Some(solid_color) = self.get_solid_color() {
            Some(color) == solid_color
        } else {
            false
        }
    }

    pub fn looks_solid(&self) -> bool {
        self.get_solid_color() != None
    }

    pub fn solid_fg(color: RGB8) -> DrawableGlyph {
        DrawableGlyph::fg_only(FULL_BLOCK, color)
    }
    pub fn solid_bg(color: RGB8) -> DrawableGlyph {
        DrawableGlyph {
            character: SPACE,
            fg_color: None,
            bg_color: Some(color),
        }
    }
    pub fn solid_color(color: RGB8) -> DrawableGlyph {
        Self::solid_bg(color)
    }
    pub fn solid_maybe_color(color: ORGB8) -> DrawableGlyph {
        DrawableGlyph {
            character: SPACE,
            fg_color: None,
            bg_color: color,
        }
    }

    pub fn get_solid_color(&self) -> Option<ORGB8> {
        if KNOWN_FG_ONLY_CHARS.contains(&self.character) {
            Some(self.fg_color)
        } else if KNOWN_BG_ONLY_CHARS.contains(&self.character) {
            Some(self.bg_color)
        } else if self.fg_color == self.bg_color {
            Some(self.fg_color)
        } else {
            None
        }
    }

    #[deprecated(note = "Use has_fg instead")]
    pub fn has_no_fg(&self) -> bool {
        DrawableGlyph::char_is_empty(self.character)
    }

    pub fn has_fg(&self) -> bool {
        !DrawableGlyph::char_is_empty(self.character)
    }

    pub fn char_is_empty(c: char) -> bool {
        KNOWN_BG_ONLY_CHARS.contains(&c)
    }

    pub fn is_fullwidth(&self) -> bool {
        DrawableGlyph::char_is_fullwidth(self.character)
    }
    pub fn char_is_fullwidth(c: char) -> bool {
        DrawableGlyph::char_is_chess(c)
            || THICK_ARROWS.contains(c)
            || c == MOVE_AND_CAPTURE_SQUARE_CHARS[0]
            || c == CONDITIONAL_MOVE_AND_CAPTURE_SQUARE_CHARS[0]
            || c == MOVE_ONLY_SQUARE_CHARS[0]
            || c == CAPTURE_ONLY_SQUARE_CHARS[0]
    }
    pub fn extract_arrow_from_arrow_string(dir: WorldStep, arrow_string: &str) -> char {
        let arrow_string_direction_order = [
            STEP_LEFT,
            STEP_RIGHT,
            STEP_UP,
            STEP_DOWN,
            STEP_UP_LEFT,
            STEP_UP_RIGHT,
            STEP_DOWN_RIGHT,
            STEP_DOWN_LEFT,
        ];

        let index_of_char = arrow_string_direction_order
            .iter()
            .position(|&arrow_dir| dir == arrow_dir)
            .unwrap();

        arrow_string.chars().nth(index_of_char).unwrap()
    }

    pub fn char_for_spear_shaft(dir: WorldStep) -> char {
        if dir.y == 0 {
            '─'
        } else if dir.x == 0 {
            '│'
        } else if dir.x == dir.y {
            '╱'
        } else {
            '╲'
        }
    }

    pub fn is_chess(&self) -> bool {
        DrawableGlyph::char_is_chess(self.character)
    }

    pub fn char_is_chess(c: char) -> bool {
        SOLID_CHESS_PIECES.contains(&c)
    }

    pub fn out_of_sight_glyphs() -> DoubleDrawableGlyph {
        [DrawableGlyph::new_colored(FULL_BLOCK, OUT_OF_SIGHT_COLOR, OUT_OF_SIGHT_COLOR); 2]
    }

    // ╳
    pub fn block_glyphs() -> DoubleDrawableGlyph {
        [DrawableGlyph::new_colored(FULL_BLOCK, BLOCK_FG, BLOCK_BG); 2]
    }

    pub fn char_map_to_fg_only_glyph_map(
        char_map: CharMap,
        color: RGB8,
    ) -> DrawableGlyphMap {
        char_map
            .iter()
            .map(|(&square, &character)| (square, DrawableGlyph::fg_only(character.clone(), color)))
            .collect()
    }

    pub fn get_glyphs_for_colored_braille_line(
        start_pos: WorldPoint,
        end_pos: WorldPoint,
        color: RGB8,
    ) -> DrawableGlyphMap {
        DrawableGlyph::char_map_to_fg_only_glyph_map(
            get_chars_for_braille_line(start_pos, end_pos).into_iter().map(|(p, c)| (p.into(), c)).collect(),
            color,
        )
    }

    pub fn points_to_braille_glyphs(points: Vec<FPoint>, color: RGB8) -> DrawableGlyphMap {
        DrawableGlyph::char_map_to_fg_only_glyph_map(points_to_braille_chars(points).into_iter().map(|(a,b)| (a.into(), b.into())).collect(), color)
    }

    pub fn character_world_pos_to_colored_braille_glyph(
        world_pos: Point2D<f32, CharacterGridInWorldFrame>,
        color: RGB8,
    ) -> DrawableGlyph {
        DrawableGlyph::new(character_world_pos_to_braille_char(world_pos), Some(color), None)
    }

    pub fn swap_fg_bg(&mut self) {
        let tmp = self.fg_color;
        self.fg_color = self.bg_color;
        self.fg_color = tmp;
    }

    pub fn colors_mut(&mut self) -> impl Iterator<Item = &mut ORGB8> {
        use std::iter::once;
        once(&mut self.fg_color).chain(once(&mut self.bg_color))
    }
}

impl Debug for DrawableGlyph {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            // "[{}|fg:{}|bg:{}|char:{}]",
            "[{}|{}|{}|{}]",
            self.render(),
            Self::from_char(self.character).render(),
            Self::solid_maybe_color(self.fg_color).render(),
            Self::solid_maybe_color(self.bg_color).render(),
        )
    }
}

impl Display for DrawableGlyph {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.render())
    }
}

pub trait DoubleDrawableGlyphExt {
    fn solid_color_if_backgroundified(&self) -> [ORGB8; 2];
    fn to_string(&self) -> String;
    fn to_clean_string(&self) -> String;
    fn chars(&self) -> DoubleChar;
    fn get_solid_color(&self) -> Option<ORGB8>;
    fn looks_solid(&self) -> bool;
    fn fg_only(character: &str, color: RGB8) -> DoubleDrawableGlyph;
    fn fg_colors(&self) -> [ORGB8; 2];
    fn solid_color(color: ORGB8) -> DoubleDrawableGlyph;
    fn tinted(&self, color: RGB8, strength: f32) -> DoubleDrawableGlyph;
    fn from_chars(value: DoubleChar) -> DoubleDrawableGlyph {
        value.map(DrawableGlyph::from_char)
    }
    fn from_str(value: &str) -> DoubleDrawableGlyph {
        assert!(value.len() == 2);
        value
            .chars()
            .map(DrawableGlyph::from_char)
            .collect_vec()
            .try_into()
            .unwrap()
    }
    fn colors_mut(&mut self) -> impl Iterator<Item = &mut ORGB8>;
}

impl DoubleDrawableGlyphExt for DoubleDrawableGlyph {
    fn solid_color_if_backgroundified(&self) -> [ORGB8; 2] {
        if self[0].is_fullwidth() {
            // fullwidth case
            [self[0].fg_color; 2]
        } else {
            // halfwidth case
            self.map(|glyph| {
                if let Some(solid_color) = glyph.get_solid_color() {
                    solid_color
                } else {
                    glyph.fg_color
                }
            })
        }
    }

    fn to_string(&self) -> String {
        self[0].to_string() + &self[1].to_string()
    }

    fn to_clean_string(&self) -> String {
        self[0].character.to_string() + &self[1].character.to_string()
    }
    fn chars(&self) -> DoubleChar {
        self.map(|g| g.character)
    }

    fn get_solid_color(&self) -> Option<ORGB8> {
        let left_solid_color_optional = self[0].get_solid_color();
        let right_solid_color_optional = self[1].get_solid_color();
        if left_solid_color_optional.is_some()
            && left_solid_color_optional == right_solid_color_optional
        {
            left_solid_color_optional
        } else {
            None
        }
    }

    fn looks_solid(&self) -> bool {
        self.get_solid_color().is_some()
    }

    fn fg_only(string: &str, color: RGB8) -> DoubleDrawableGlyph {
        assert_eq!(string.chars().count(), 2);
        [0, 1].map(|i| DrawableGlyph::fg_only(string.chars().nth(i).unwrap(), color))
    }

    fn fg_colors(&self) -> [ORGB8; 2] {
        [0, 1].map(|i| self[i].fg_color)
    }

    fn solid_color(color: ORGB8) -> DoubleDrawableGlyph {
        [DrawableGlyph::new(SPACE, None, color); 2]
    }

    fn tinted(&self, color: RGB8, strength: f32) -> DoubleDrawableGlyph {
        self.map(|g| g.tinted(color, strength))
    }

    fn colors_mut(&mut self) -> impl Iterator<Item = &mut ORGB8> {
        self.iter_mut().flat_map(|g| g.colors_mut())
    }
}

fn combine_characters(top_char: char, bottom_char: char) -> Option<char> {
    if DrawableGlyph::char_is_empty(top_char) {
        Some(bottom_char)
    } else if char_is_braille(top_char) && char_is_braille(bottom_char) {
        Some(combine_braille_characters(top_char, bottom_char))
    } else if char_is_hextant(top_char) && char_is_hextant(bottom_char) {
        Some(combine_hextant_characters(top_char, bottom_char))
    } else {
        None
    }
}

pub fn map_of_stringables_to_string<S>(stringable_map: &HashMap<[i32; 2], S>) -> String
where
    S: ToString,
{
    let top_row = stringable_map
        .keys()
        .map(|square| square.y())
        .max()
        .unwrap();
    let bottom_row = stringable_map
        .keys()
        .map(|square| square.y())
        .min()
        .unwrap();
    let left_column = stringable_map
        .keys()
        .map(|square| square.x())
        .min()
        .unwrap();
    let right_column = stringable_map
        .keys()
        .map(|square| square.x())
        .max()
        .unwrap();
    (bottom_row..=top_row)
        .map(|bottom_to_top_y| {
            let y = top_row + bottom_row - bottom_to_top_y;
            (left_column..=right_column)
                .map(|x| {
                    let square = [x, y];
                    let new_part = if let Some(glyph) = stringable_map.get(&square) {
                        glyph.to_string()
                    } else {
                        " ".to_string()
                    };

                    new_part
                })
                .join("")
        })
        .join("\n")
}
pub fn glyph_map_to_string(glyph_map: &DrawableGlyphMap) -> String {
    map_of_stringables_to_string(
        glyph_map
    )
}

// Order is anticlockwise [right, up, left, down]
pub fn chars_for_square_walls(walls: [bool; 4]) -> DoubleChar {
    let left = match walls {
        [_, true, true, true] => '𜷂',
        [_, true, true, false] => '🭽', //𜵊
        [_, true, false, true] => '𜶮',
        [_, true, false, false] => '▔',
        [_, false, true, true] => '🭼', //𜷀
        [_, false, true, false] => '▏',
        [_, false, false, true] => '▁',
        [_, false, false, false] => ' ',
    };
    let right = match walls {
        [true, true, _, true] => '𜷖',
        [true, true, _, false] => '🭾', //'𜶘'
        [true, false, _, true] => '🭿', //𜷕
        [true, false, _, false] => '▕',
        [false, true, _, true] => '𜶮',
        [false, true, _, false] => '▔',
        [false, false, _, true] => '▁',
        [false, false, _, false] => ' ',
    };
    [left, right]
}

#[cfg(test)]
mod tests {
    use ntest::assert_false;

    use super::*;

    #[test]
    fn test_double_glyph_square_offset_up() {
        // offset up
        let glyphs =
            DrawableGlyph::orthogonally_offset_board_square_glyphs(vec2(0.0, 0.5), RED, BLACK);
        assert_eq!(glyphs[0].character, UPPER_HALF_BLOCK);
        assert_eq!(glyphs[0].fg_color.unwrap(), RED);
        assert_eq!(glyphs[0].bg_color.unwrap(), BLACK);
        assert_eq!(glyphs[1].character, UPPER_HALF_BLOCK);
        assert_eq!(glyphs[1].fg_color.unwrap(), RED);
        assert_eq!(glyphs[1].bg_color.unwrap(), BLACK);
    }

    #[test]
    fn test_double_glyph_square_offset_150_up() {
        // offset up
        let glyphs =
            DrawableGlyph::orthogonally_offset_board_square_glyphs(vec2(0.0, 1.5), RED, BLACK);
        assert_eq!(glyphs[0].character, '▄');
        assert_eq!(glyphs[0].fg_color.unwrap(), RED);
        assert_eq!(glyphs[0].bg_color.unwrap(), BLACK);
        assert_eq!(glyphs[1].character, '▄');
        assert_eq!(glyphs[1].fg_color.unwrap(), RED);
        assert_eq!(glyphs[1].bg_color.unwrap(), BLACK);
    }

    #[test]
    fn test_double_glyph_square_offset_50_down() {
        // offset down
        let glyphs =
            DrawableGlyph::orthogonally_offset_board_square_glyphs(vec2(0.0, -0.5), RED, BLACK);
        assert_eq!(glyphs[0].character, '▄');
        assert_eq!(glyphs[0].fg_color.unwrap(), RED);
        assert_eq!(glyphs[0].bg_color.unwrap(), BLACK);
        assert_eq!(glyphs[1].character, '▄');
        assert_eq!(glyphs[1].fg_color.unwrap(), RED);
        assert_eq!(glyphs[1].bg_color.unwrap(), BLACK);
    }

    //                      |<--halfway
    // ' ', '▏', '▎', '▍', '▌', '▋', '▊', '▉', '█'
    #[test]
    fn test_double_glyph_square_offset_1_left() {
        // offset left
        let glyphs =
            DrawableGlyph::orthogonally_offset_board_square_glyphs(vec2(-0.01, 0.0), RED, BLACK);
        assert!(glyphs[0].looks_solid_color(RED));
        assert!(glyphs[1].looks_solid_color(RED));
    }

    #[test]
    fn test_double_glyph_square_offset_14_left() {
        // offset left
        let glyphs =
            DrawableGlyph::orthogonally_offset_board_square_glyphs(vec2(-0.14, 0.0), RED, BLACK);
        assert!(
            glyphs[0].looks_solid_color(RED),
            "glyph: {}",
            &glyphs[0].to_string()
        );
        //assert_eq!(glyphs[1].character, '▏');
        assert_eq!(glyphs[1], DrawableGlyph::new_colored('▊', RED, BLACK));
    }

    #[test]
    fn test_double_glyph_square_offset_25_left() {
        // offset left
        let glyphs =
            DrawableGlyph::orthogonally_offset_board_square_glyphs(vec2(-0.25, 0.0), RED, BLACK);
        assert!(glyphs[0].looks_solid_color(RED));
        assert_eq!(glyphs[1].character, '▌');
        assert_eq!(glyphs[1].fg_color.unwrap(), RED);
        assert_eq!(glyphs[1].bg_color.unwrap(), BLACK);
    }

    #[test]
    fn test_double_glyph_square_offset_75_left() {
        // offset left
        let glyphs =
            DrawableGlyph::orthogonally_offset_board_square_glyphs(vec2(-0.75, 0.0), RED, BLACK);
        assert_eq!(glyphs[0].character, '▌');
        assert_eq!(glyphs[0].fg_color.unwrap(), RED);
        assert_eq!(glyphs[0].bg_color.unwrap(), BLACK);
        assert!(glyphs[1].looks_solid_color(BLACK));
    }

    #[test]
    fn test_double_glyph_square_offset_25_right() {
        // offset right
        let glyphs =
            DrawableGlyph::orthogonally_offset_board_square_glyphs(vec2(0.25, 0.0), RED, BLACK);
        assert_eq!(glyphs[0].character, RIGHT_HALF_BLOCK);
        assert_eq!(glyphs[0].fg_color.unwrap(), RED);
        assert_eq!(glyphs[0].bg_color.unwrap(), BLACK);
        assert!(glyphs[1].looks_solid_color(RED));
    }

    #[test]
    fn test_double_glyph_square_offset_50_right() {
        // offset right
        let glyphs =
            DrawableGlyph::orthogonally_offset_board_square_glyphs(vec2(0.50, 0.0), RED, BLACK);
        assert!(glyphs[0].looks_solid_color(BLACK));
        assert!(glyphs[1].looks_solid_color(RED));
    }

    #[test]
    fn test_double_glyph_square_offset_75_right() {
        // offset right
        let glyphs =
            DrawableGlyph::orthogonally_offset_board_square_glyphs(vec2(0.75, 0.0), RED, BLACK);
        assert_eq!(glyphs[0].character, SPACE);
        assert_eq!(glyphs[0].fg_color.unwrap(), RED);
        assert_eq!(glyphs[0].bg_color.unwrap(), BLACK);
        assert_eq!(glyphs[1].character, RIGHT_HALF_BLOCK);
        assert_eq!(glyphs[1].fg_color.unwrap(), RED);
        assert_eq!(glyphs[1].bg_color.unwrap(), BLACK);
    }

    #[test]
    fn test_double_glyph_square_offset_slightly_past_full_square_right() {
        // offset right
        let glyphs =
            DrawableGlyph::orthogonally_offset_board_square_glyphs(vec2(1.02, 0.0), RED, BLACK);
        assert!(
            glyphs[0].looks_solid_color(BLACK),
            "glyph: {}",
            &glyphs[0].to_string()
        );
        assert!(
            glyphs[1].looks_solid_color(BLACK),
            "glyph: {}",
            &glyphs[1].to_string()
        );
    }

    #[test]
    fn test_double_glyph_square_offset_partial_character_past_full_square_right() {
        // offset right
        let glyphs =
            DrawableGlyph::orthogonally_offset_board_square_glyphs(vec2(1.25, 0.0), RED, BLACK);
        assert_eq!(glyphs[0], DrawableGlyph::new_colored(LEFT_HALF_BLOCK, RED, BLACK));
        assert!(
            glyphs[1].looks_solid_color(BLACK),
            "glyph: {}",
            &glyphs[1].to_string()
        );
    }

    #[test]
    fn test_square_offset_to_character_offsets_zeros() {
        assert_eq!(
            DrawableGlyph::horizontal_square_offset_to_character_offsets(0.0),
            [0.0, 0.0]
        );
    }

    #[test]
    fn test_square_offset_to_character_offsets_1_4_left() {
        assert_eq!(
            DrawableGlyph::horizontal_square_offset_to_character_offsets(-0.25),
            [0.0, -0.5]
        );
    }

    #[test]
    fn test_square_offset_to_character_offsets_1_4_right() {
        assert_eq!(
            DrawableGlyph::horizontal_square_offset_to_character_offsets(0.25),
            [0.5, 0.0]
        );
    }

    #[test]
    fn test_square_offset_to_character_offsets_1_2_right() {
        assert_eq!(
            DrawableGlyph::horizontal_square_offset_to_character_offsets(0.5),
            [1.0, 0.0]
        );
    }

    #[test]
    fn test_square_offset_to_character_offsets_1_2_left() {
        assert_eq!(
            DrawableGlyph::horizontal_square_offset_to_character_offsets(-0.5),
            [0.0, -1.0]
        );
    }

    #[test]
    fn test_square_offset_to_character_offsets_1_left() {
        assert_eq!(
            DrawableGlyph::horizontal_square_offset_to_character_offsets(-1.0),
            [-1.0, -1.0]
        );
    }

    #[test]
    fn test_square_offset_to_character_offsets_1_right() {
        assert_eq!(
            DrawableGlyph::horizontal_square_offset_to_character_offsets(1.0),
            [1.0, 1.0]
        );
    }

    #[test]
    fn test_get_solid_color_if_there_is_one() {
        let glyph = DrawableGlyph::new_colored(' ', BLUE, RED);
        assert_eq!(glyph.get_solid_color().unwrap(), Some(RED));
        let glyph = DrawableGlyph::new_colored(FULL_BLOCK, BLUE, RED);
        assert_eq!(glyph.get_solid_color().unwrap(), Some(BLUE));
    }

    #[test]
    fn test_can_not_get_solid_color_if_there_is_not_one() {
        let glyph = DrawableGlyph::new_colored('a', BLUE, RED);
        assert_eq!(glyph.get_solid_color(), None);
    }

    #[test]
    fn test_braille_line_has_default_background() {
        let glyph_map = DrawableGlyph::get_glyphs_for_colored_braille_line(
            point2(1.0, 1.0),
            point2(3.0, 30.0),
            RED,
        );
        assert!(glyph_map.values().all(|glyph| glyph.bg_color.is_none()))
    }

    #[test]
    fn test_character_width_detection() {
        assert!(DrawableGlyph::char_is_fullwidth('🢂'));
        assert_false!(DrawableGlyph::char_is_fullwidth('>'));
    }
}
