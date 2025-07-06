use std::cmp::min;
use std::collections::{HashMap, HashSet};
use std::fmt::{Debug, Formatter};
use std::hash::Hash;

use ::num::clamp;
use euclid::*;
use euclid::{point2, vec2};
use line_drawing::Point;
use ordered_float::OrderedFloat;
use rgb::*;
use termion::color;
use termion::color::Black;

use braille::*;
use glyph_constants::*;
use hextant_blocks::*;

use crate::glyph::floating_square::character_for_half_square_with_1d_offset;
use crate::screen::{
    is_world_character_square_left_square_of_world_square, world_character_square_to_world_square,
    CharacterGridInWorldFrame, WorldCharacterSquare, WorldCharacterSquareGlyphMap,
};
use utility::coordinate_frame_conversions::*;
use utility::sign;
use utility::*;

pub_mod_and_use!(angled_blocks, braille, floating_square, hextant_blocks);

pub mod glyph_constants;

// x, y
pub type DoubleChar = [char; 2];
pub type DoubleGlyph = [Glyph; 2];

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

#[derive(Clone, PartialEq, Eq, Copy)]
pub struct Glyph {
    pub character: char,
    pub fg_color: RGB8,
    pub bg_color: RGB8,
    pub bg_transparent: bool,
}

impl Glyph {
    pub fn new(character: char, fg_color: RGB8, bg_color: RGB8) -> Glyph {
        Glyph {
            character,
            fg_color,
            bg_color,
            bg_transparent: false,
        }
    }
    pub fn default_transparent() -> Glyph {
        Glyph::fg_only(' ', WHITE)
    }
    pub fn default_background() -> Glyph {
        Glyph::new(FULL_BLOCK, BLACK, BLACK)
    }

    pub fn fg_only(character: char, fg_color: RGB8) -> Glyph {
        Glyph {
            character,
            fg_color,
            bg_color: BLACK,
            bg_transparent: true,
        }
    }
    fn tinted(&self, color: RGB8, strength: f32) -> Self {
        Glyph {
            fg_color: tint_color(self.fg_color, color, strength),
            bg_color: tint_color(self.bg_color, color, strength),
            ..*self
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
    pub fn with_transparent_bg(&self, bg_transparent: bool) -> Glyph {
        let mut dup = self.clone();
        dup.bg_transparent = bg_transparent;
        dup
    }

    pub fn reset_colors() -> String {
        format!("{}{}", color::Fg(color::Reset), color::Bg(color::Reset),)
    }

    pub fn orthogonally_offset_board_square_glyphs(
        offset_vector: WorldMove,
        square_color: RGB8,
        background_color: RGB8,
    ) -> DoubleGlyph {
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
                Glyph::new(
                    character_for_half_square_with_1d_offset(true, character_offset),
                    square_color,
                    background_color,
                )
            })
        } else {
            let character_offsets = Glyph::horizontal_square_offset_to_character_offsets(
                offset_magnitude_within_one_period_symmetric_about_zero,
            );
            character_offsets.map(|character_offset| {
                Glyph::new(
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

    #[deprecated(note = "Use ArrowDrawable instead")]
    pub fn get_glyphs_for_player(faced_direction: KingWorldStep) -> DoubleGlyph {
        // ⭠⭢⭡⭣ ⭦⭧⭨⭩

        let mut glyphs = [
            Glyph::from_char(Glyph::extract_arrow_from_arrow_string(
                faced_direction.into(),
                THICK_ARROWS,
            ))
            .with_transparent_bg(true),
            Glyph::transparent_glyph(),
        ];
        glyphs[0].fg_color = PLAYER_COLOR;

        glyphs
    }

    pub fn looks_solid_color(&self, color: RGB8) -> bool {
        if let Some(solid_color) = self.get_solid_color() {
            color == solid_color
        } else {
            false
        }
    }

    pub fn looks_solid(&self) -> bool {
        self.get_solid_color() != None
    }

    pub fn solid_fg(color: RGB8) -> Glyph {
        Glyph::fg_only(FULL_BLOCK, color)
    }
    pub fn solid_bg(color: RGB8) -> Glyph {
        Glyph {
            character: SPACE,
            fg_color: MAGENTA,
            bg_color: color,
            bg_transparent: false,
        }
    }

    pub fn get_solid_color(&self) -> Option<RGB8> {
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
        Glyph::char_is_empty(self.character)
    }

    pub fn has_fg(&self) -> bool {
        !Glyph::char_is_empty(self.character)
    }

    pub fn char_is_empty(c: char) -> bool {
        KNOWN_BG_ONLY_CHARS.contains(&c)
    }
    pub fn is_fully_transparent(&self) -> bool {
        self.has_no_fg() && self.bg_transparent
    }
    pub fn is_bg_only(&self) -> bool {
        self.has_no_fg() && !self.bg_transparent
    }

    pub fn is_fullwidth(&self) -> bool {
        Glyph::char_is_fullwidth(self.character)
    }
    pub fn char_is_fullwidth(c: char) -> bool {
        Glyph::char_is_chess(c)
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
    pub fn char_for_flying_arrow(dir: KingWorldStep) -> char {
        //"🡐🡒🡑🡓🡔🡕🡖🡗")
        Glyph::extract_arrow_from_arrow_string(dir.into(), THIN_TRIANGLE_ARROWS)
    }

    pub fn glyphs_for_flying_arrow(dir: KingWorldStep) -> DoubleGlyph {
        [
            Glyph::fg_only(Glyph::char_for_flying_arrow(dir), RED),
            Glyph::transparent_glyph(),
        ]
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
        Glyph::char_is_chess(self.character)
    }

    pub fn char_is_chess(c: char) -> bool {
        SOLID_CHESS_PIECES.contains(&c)
    }

    pub fn transparent_glyph() -> Glyph {
        Glyph::fg_only(' ', BLACK)
    }
    pub fn transparent_square_glyphs() -> DoubleGlyph {
        [Glyph::fg_only(' ', BLACK); 2]
    }
    pub fn out_of_sight_glyphs() -> DoubleGlyph {
        [Glyph::new(FULL_BLOCK, OUT_OF_SIGHT_COLOR, OUT_OF_SIGHT_COLOR); 2]
    }

    // ╳
    pub fn block_glyphs() -> DoubleGlyph {
        [Glyph::new(FULL_BLOCK, BLOCK_FG, BLOCK_BG); 2]
    }

    pub fn drawn_over(&self, bottom: Glyph) -> Glyph {
        let top = *self;

        if top.has_fg() {
            let maybe_combined = combine_characters(top.character, bottom.character);
            if top.bg_transparent && maybe_combined.is_some() {
                let combined_character = maybe_combined.unwrap();
                Glyph::new(combined_character, top.fg_color, bottom.bg_color)
            } else {
                let bg = if !top.bg_transparent {
                    top.bg_color
                } else if bottom.has_fg() {
                    bottom.fg_color
                } else {
                    bottom.bg_color
                };

                Glyph::new(top.character, top.fg_color, bg)
            }
        } else if !top.bg_transparent {
            Glyph::solid_bg(top.bg_color)
        } else if bottom.has_fg() {
            bottom
        } else {
            Glyph::solid_bg(bottom.bg_color)
        }
    }

    pub fn char_map_to_fg_only_glyph_map<T: Hash + Eq + Copy>(
        char_map: HashMap<T, char>,
        color: RGB8,
    ) -> HashMap<T, Glyph> {
        char_map
            .iter()
            .map(|(&square, &character)| (square, Glyph::fg_only(character.clone(), color)))
            .collect()
    }

    pub fn get_glyphs_for_colored_braille_line(
        start_pos: WorldPoint,
        end_pos: WorldPoint,
        color: RGB8,
    ) -> WorldCharacterSquareGlyphMap {
        Glyph::char_map_to_fg_only_glyph_map(get_chars_for_braille_line(start_pos, end_pos), color)
    }

    pub fn points_to_braille_glyphs(
        points: Vec<WorldPoint>,
        color: RGB8,
    ) -> WorldCharacterSquareGlyphMap {
        Glyph::char_map_to_fg_only_glyph_map(points_to_braille_chars(points), color)
    }

    pub fn character_world_pos_to_colored_braille_glyph(
        world_pos: Point2D<f32, CharacterGridInWorldFrame>,
        color: RGB8,
    ) -> Glyph {
        Glyph::new(character_world_pos_to_braille_char(world_pos), color, BLACK)
    }
}

impl Debug for Glyph {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "char: {}\n\
             fg: {}\n\
             bg: {}\n\
             bg_transparent: {}",
            self.character,
            rgb_to_string(self.fg_color),
            rgb_to_string(self.bg_color),
            self.bg_transparent
        )
    }
}

pub trait DoubleGlyphFunctions {
    fn solid_color_if_backgroundified(&self) -> [RGB8; 2];
    fn drawn_over(&self, background_glyphs: DoubleGlyph) -> DoubleGlyph;
    fn to_string(&self) -> String;
    fn to_clean_string(&self) -> String;
    fn chars(&self) -> DoubleChar;
    fn get_solid_color(&self) -> Option<RGB8>;
    fn looks_solid(&self) -> bool;
    fn fg_only(character: &str, color: RGB8) -> DoubleGlyph;
    fn fg_colors(&self) -> [RGB8; 2];
    fn solid_color(color: RGB8) -> DoubleGlyph;
    fn tinted(&self, color: RGB8, strength: f32) -> DoubleGlyph;
}

impl DoubleGlyphFunctions for DoubleGlyph {
    fn solid_color_if_backgroundified(&self) -> [RGB8; 2] {
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

    fn drawn_over(&self, background_glyphs: DoubleGlyph) -> DoubleGlyph {
        let top_left = self[0];
        let top_right = self[1];
        let bottom_left = background_glyphs[0];
        let bottom_right = background_glyphs[1];

        let halfwidth_only_output = [0, 1].map(|i| {
            let top = self[i];
            let bottom = background_glyphs[i];
            top.drawn_over(bottom)
        });

        let output = if bottom_left.is_fullwidth() {
            if !top_left.is_fullwidth() && top_left.has_fg() && top_right.has_fg() {
                let mut tmp = halfwidth_only_output;
                tmp[1].bg_color = bottom_left.fg_color;
                tmp
            } else if top_left.is_fullwidth() && top_left.bg_transparent {
                let mut tmp = halfwidth_only_output;
                tmp[1].bg_color = bottom_left.fg_color;
                tmp
            } else {
                halfwidth_only_output
            }
        } else {
            halfwidth_only_output
        };

        return output;
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

    fn get_solid_color(&self) -> Option<RGB8> {
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

    fn fg_only(string: &str, color: RGB8) -> DoubleGlyph {
        assert_eq!(string.chars().count(), 2);
        [0, 1].map(|i| Glyph::fg_only(string.chars().nth(i).unwrap(), color))
    }

    fn fg_colors(&self) -> [RGB8; 2] {
        [0, 1].map(|i| self[i].fg_color)
    }

    fn solid_color(color: RGB8) -> DoubleGlyph {
        [Glyph::new(SPACE, color, color); 2]
    }

    fn tinted(&self, color: RGB8, strength: f32) -> DoubleGlyph {
        self.map(|g| g.tinted(color, strength))
    }
}

fn combine_characters(top_char: char, bottom_char: char) -> Option<char> {
    if Glyph::char_is_empty(top_char) {
        Some(bottom_char)
    } else if char_is_braille(top_char) && char_is_braille(bottom_char) {
        Some(combine_braille_characters(top_char, bottom_char))
    } else if char_is_hextant(top_char) && char_is_hextant(bottom_char) {
        Some(combine_hextant_characters(top_char, bottom_char))
    } else {
        None
    }
}
pub fn glyph_map_to_string(glyph_map: &WorldCharacterSquareGlyphMap) -> String {
    let top_row = glyph_map.keys().map(|square| square.y).max().unwrap();
    let bottom_row = glyph_map.keys().map(|square| square.y).min().unwrap();
    let left_column = glyph_map.keys().map(|square| square.x).min().unwrap();
    let right_column = glyph_map.keys().map(|square| square.x).max().unwrap();
    let mut string = String::new();
    for bottom_to_top_y in bottom_row..=top_row {
        let y = top_row + bottom_row - bottom_to_top_y;
        for x in left_column..=right_column {
            let square = WorldCharacterSquare::new(x, y);
            let new_part = if let Some(glyph) = glyph_map.get(&square) {
                glyph.to_string()
            } else {
                " ".to_string()
            };

            string += &new_part;
        }
        string += "\n";
    }
    string
}
#[deprecated(note = "worldcharactersquareglyphmap is bad")]
pub fn pair_up_character_square_map<T: Clone>(
    character_glyph_map: HashMap<WorldCharacterSquare, T>,
    default_filler: T,
) -> HashMap<WorldSquare, [T; 2]> {
    let mut output_map = HashMap::<WorldSquare, [T; 2]>::new();
    character_glyph_map
        .into_iter()
        .for_each(|(character_square, value)| {
            let world_square = world_character_square_to_world_square(character_square);
            let is_left_value =
                is_world_character_square_left_square_of_world_square(character_square);
            let position_index = if is_left_value { 0 } else { 1 };

            if output_map.contains_key(&world_square) {
                let mut existing_double_value = output_map.get_mut(&world_square).unwrap();
                existing_double_value[position_index] = value;
            } else {
                let mut new_double_value = [default_filler.clone(), default_filler.clone()];
                new_double_value[position_index] = value;
                output_map.insert(world_square, new_double_value);
            }
        });
    output_map
}

#[cfg(test)]
mod tests {
    use ntest::assert_false;

    use super::*;

    #[test]
    fn test_double_glyph_square_offset__up() {
        // offset up
        let glyphs = Glyph::orthogonally_offset_board_square_glyphs(vec2(0.0, 0.5), RED, BLACK);
        assert_eq!(glyphs[0].character, UPPER_HALF_BLOCK);
        assert_eq!(glyphs[0].fg_color, RED);
        assert_eq!(glyphs[0].bg_color, BLACK);
        assert_eq!(glyphs[1].character, UPPER_HALF_BLOCK);
        assert_eq!(glyphs[1].fg_color, RED);
        assert_eq!(glyphs[1].bg_color, BLACK);
    }

    #[test]
    fn test_double_glyph_square_offset__150_up() {
        // offset up
        let glyphs = Glyph::orthogonally_offset_board_square_glyphs(vec2(0.0, 1.5), RED, BLACK);
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
        let glyphs = Glyph::orthogonally_offset_board_square_glyphs(vec2(0.0, -0.5), RED, BLACK);
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
        let glyphs = Glyph::orthogonally_offset_board_square_glyphs(vec2(-0.01, 0.0), RED, BLACK);
        assert!(glyphs[0].looks_solid_color(RED));
        assert!(glyphs[1].looks_solid_color(RED));
    }

    #[test]
    fn test_double_glyph_square_offset__14_left() {
        // offset left
        let glyphs = Glyph::orthogonally_offset_board_square_glyphs(vec2(-0.14, 0.0), RED, BLACK);
        assert!(
            glyphs[0].looks_solid_color(RED),
            "glyph: {}",
            &glyphs[0].to_string()
        );
        //assert_eq!(glyphs[1].character, '▏');
        assert_eq!(glyphs[1], Glyph::new('▊', RED, BLACK));
    }

    #[test]
    fn test_double_glyph_square_offset__25_left() {
        // offset left
        let glyphs = Glyph::orthogonally_offset_board_square_glyphs(vec2(-0.25, 0.0), RED, BLACK);
        assert!(glyphs[0].looks_solid_color(RED));
        assert_eq!(glyphs[1].character, '▌');
        assert_eq!(glyphs[1].fg_color, RED);
        assert_eq!(glyphs[1].bg_color, BLACK);
    }

    #[test]
    fn test_double_glyph_square_offset__75_left() {
        // offset left
        let glyphs = Glyph::orthogonally_offset_board_square_glyphs(vec2(-0.75, 0.0), RED, BLACK);
        assert_eq!(glyphs[0].character, '▌');
        assert_eq!(glyphs[0].fg_color, RED);
        assert_eq!(glyphs[0].bg_color, BLACK);
        assert!(glyphs[1].looks_solid_color(BLACK));
    }

    #[test]
    fn test_double_glyph_square_offset__25_right() {
        // offset right
        let glyphs = Glyph::orthogonally_offset_board_square_glyphs(vec2(0.25, 0.0), RED, BLACK);
        assert_eq!(glyphs[0].character, RIGHT_HALF_BLOCK);
        assert_eq!(glyphs[0].fg_color, RED);
        assert_eq!(glyphs[0].bg_color, BLACK);
        assert!(glyphs[1].looks_solid_color(RED));
    }

    #[test]
    fn test_double_glyph_square_offset__50_right() {
        // offset right
        let glyphs = Glyph::orthogonally_offset_board_square_glyphs(vec2(0.50, 0.0), RED, BLACK);
        assert!(glyphs[0].looks_solid_color(BLACK));
        assert!(glyphs[1].looks_solid_color(RED));
    }

    #[test]
    fn test_double_glyph_square_offset__75_right() {
        // offset right
        let glyphs = Glyph::orthogonally_offset_board_square_glyphs(vec2(0.75, 0.0), RED, BLACK);
        assert_eq!(glyphs[0].character, SPACE);
        assert_eq!(glyphs[0].fg_color, RED);
        assert_eq!(glyphs[0].bg_color, BLACK);
        assert_eq!(glyphs[1].character, RIGHT_HALF_BLOCK);
        assert_eq!(glyphs[1].fg_color, RED);
        assert_eq!(glyphs[1].bg_color, BLACK);
    }

    #[test]
    fn test_double_glyph_square_offset__slightly_past_full_square_right() {
        // offset right
        let glyphs = Glyph::orthogonally_offset_board_square_glyphs(vec2(1.02, 0.0), RED, BLACK);
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
    fn test_double_glyph_square_offset__partial_character_past_full_square_right() {
        // offset right
        let glyphs = Glyph::orthogonally_offset_board_square_glyphs(vec2(1.25, 0.0), RED, BLACK);
        assert_eq!(glyphs[0], Glyph::new(LEFT_HALF_BLOCK, RED, BLACK));
        assert!(
            glyphs[1].looks_solid_color(BLACK),
            "glyph: {}",
            &glyphs[1].to_string()
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
        assert!(glyph_map.values().all(|glyph| glyph.bg_transparent == true))
    }

    #[test]
    fn test_basic_drawn_over_case() {
        let bottom_glyphs = [Glyph {
            character: 'b',
            fg_color: BLUE,
            bg_color: RED,
            bg_transparent: false,
        }; 2];
        let top_glyphs = [Glyph {
            character: 'a',
            fg_color: GREEN,
            bg_color: WHITE,
            bg_transparent: true,
        }; 2];
        let combo_glyphs = top_glyphs.drawn_over(bottom_glyphs);
        // not true in all cases
        assert_eq!(combo_glyphs[0], combo_glyphs[1]);
        assert_eq!(
            combo_glyphs[0],
            Glyph {
                character: 'a',
                fg_color: GREEN,
                bg_color: BLUE,
                bg_transparent: false,
            }
        );
    }

    #[test]
    fn test_hextant_drawn_over_hextant_combines() {
        let bottom_glyphs = [Glyph::fg_only('🬀', GREEN); 2];
        let top_glyphs = [Glyph::fg_only('🬑', RED); 2];
        let combo_glyphs = top_glyphs.drawn_over(bottom_glyphs);

        assert_eq!(combo_glyphs[0], combo_glyphs[1]); // not true in all cases
        assert_eq!(combo_glyphs[0].character, '🬒');
        assert_eq!(combo_glyphs[0].fg_color, RED);
    }

    #[test]
    fn test_space_drawn_over_hextant_does_nothing() {
        let the_char = '🬒';
        let bottom_glyphs = [Glyph::fg_only(the_char, RED); 2];
        let top_glyphs = [Glyph::fg_only(SPACE, BLUE); 2];
        let combo_glyphs = top_glyphs.drawn_over(bottom_glyphs);

        assert_eq!(combo_glyphs[0], combo_glyphs[1]); // not true in all cases
        assert_eq!(combo_glyphs[0], Glyph::fg_only(the_char, RED));
    }

    #[test]
    fn test_braille_drawn_over_braille_combines() {
        let bottom_glyphs = [Glyph::fg_only('⠎', BLUE); 2];
        let top_glyphs = [Glyph::fg_only('⠁', RED); 2];
        let combo_glyphs = top_glyphs.drawn_over(bottom_glyphs);

        assert_eq!(combo_glyphs[0], combo_glyphs[1]); // not true in all cases
        assert_eq!(combo_glyphs[0].character, '⠏');
        assert_eq!(combo_glyphs[0].fg_color, RED);
    }

    #[test]
    fn test_halfwidth_char_drawn_over_right_side_of_fullwidth_char() {
        let halfwidth_char = 'a';
        let fullwidth_char = '🢃';
        let top_fg_color = RED;
        let bottom_fg_color = BLUE;
        let bottom_bg_color = GREEN;

        let bottom_glyphs =
            [fullwidth_char, SPACE].map(|c| Glyph::new(c, bottom_fg_color, bottom_bg_color));
        let top_glyphs = [SPACE, halfwidth_char].map(|c| Glyph::fg_only(c, top_fg_color));
        let combo_glyphs = top_glyphs.drawn_over(bottom_glyphs);

        assert_eq!(combo_glyphs[0].character, fullwidth_char);
        assert_eq!(combo_glyphs[0].fg_color, bottom_fg_color);
        assert_eq!(combo_glyphs[0].bg_color, bottom_bg_color);

        assert_eq!(combo_glyphs[1].character, halfwidth_char);
        assert_eq!(combo_glyphs[1].fg_color, top_fg_color);
        assert_eq!(combo_glyphs[1].bg_color, bottom_bg_color);
    }

    #[test]
    fn test_fullwidth_char_drawn_over_two_halfwidth_chars() {
        let halfwidth_char = 'a';
        let fullwidth_char = '🢃';
        let top_color = RED;
        let bottom_color = BLUE;
        let bottom_glyphs = [halfwidth_char; 2].map(|c| Glyph::fg_only(c, bottom_color));
        let top_glyphs = [fullwidth_char, SPACE].map(|c| Glyph::fg_only(c, top_color));
        let combo_glyphs = top_glyphs.drawn_over(bottom_glyphs);

        assert_eq!(combo_glyphs[0].character, fullwidth_char);
        assert_eq!(combo_glyphs[0].fg_color, top_color);
        assert_eq!(combo_glyphs[0].bg_color, bottom_color);

        assert_eq!(combo_glyphs[1].character, halfwidth_char);
        assert_eq!(combo_glyphs[1].fg_color, bottom_color);
    }

    #[test]
    fn test_two_halfwidth_chars_drawn_over_fullwidth_char() {
        let halfwidth_char = 'a';
        let fullwidth_char = '🢃';
        let top_color = RED;
        let bottom_color = BLUE;
        let top_glyphs = [halfwidth_char; 2].map(|c| Glyph::fg_only(c, top_color));
        let bottom_glyphs = [fullwidth_char, SPACE].map(|c| Glyph::fg_only(c, bottom_color));
        let combo_glyphs = top_glyphs.drawn_over(bottom_glyphs);

        assert_eq!(combo_glyphs[0].character, halfwidth_char);
        assert_eq!(combo_glyphs[0].fg_color, top_color);
        assert_eq!(combo_glyphs[0].bg_color, bottom_color);

        assert_eq!(combo_glyphs[1].character, halfwidth_char);
        assert_eq!(combo_glyphs[1].fg_color, top_color);
        assert_eq!(combo_glyphs[1].bg_color, bottom_color);
    }

    #[test]
    fn test_fullwidth_char_drawn_over_fullwidth_char() {
        let fullwidth_char = '🢃';
        let top_color = RED;
        let bottom_color = BLUE;

        let top_glyphs = [fullwidth_char, SPACE].map(|c| Glyph::fg_only(c, top_color));
        let bottom_glyphs = [fullwidth_char, SPACE].map(|c| Glyph::fg_only(c, bottom_color));
        let combo_glyphs = top_glyphs.drawn_over(bottom_glyphs);

        assert_eq!(combo_glyphs[0].character, fullwidth_char);
        assert_eq!(combo_glyphs[0].fg_color, top_color);
        assert_eq!(combo_glyphs[0].bg_color, bottom_color);
        assert_eq!(combo_glyphs[1].character, SPACE);
        assert_eq!(combo_glyphs[1].bg_color, bottom_color);
    }

    #[test]
    fn test_character_width_detection() {
        assert!(Glyph::char_is_fullwidth('🢂'));
        assert_false!(Glyph::char_is_fullwidth('>'));
    }
    #[test]
    fn test_pair_up_glyph_map__positions() {
        let character_squares: Vec<WorldCharacterSquare> = vec![
            point2(0, 0),
            point2(1, 0),
            point2(1, 1),
            point2(2, 0),
            point2(2, 1),
        ];

        let mut character_glyph_map = WorldCharacterSquareGlyphMap::new();
        for square in character_squares {
            character_glyph_map.insert(square, Glyph::default_transparent());
        }
        let square_glyph_map =
            pair_up_character_square_map(character_glyph_map, Glyph::transparent_glyph());
        let correct_squares = vec![point2(0, 0), point2(1, 0), point2(0, 1), point2(1, 1)];
        assert_eq!(square_glyph_map.len(), correct_squares.len());
        for square in correct_squares {
            assert!(square_glyph_map.contains_key(&square));
        }
    }

    #[test]
    fn test_pair_up_glyph_map__glyphs() {
        let mut character_glyph_map = WorldCharacterSquareGlyphMap::new();
        let test_glyph = Glyph {
            character: ' ',
            fg_color: RGB8::new(0, 0, 0),
            bg_color: RGB8::new(100, 100, 150),
            bg_transparent: false,
        };
        character_glyph_map.insert(point2(0, 0), test_glyph);
        character_glyph_map.insert(point2(1, 0), test_glyph);
        let square_glyph_map =
            pair_up_character_square_map(character_glyph_map, Glyph::transparent_glyph());
        assert_eq!(square_glyph_map.len(), 1);
        assert_eq!(
            *square_glyph_map.get(&point2(0, 0)).unwrap(),
            [test_glyph; 2]
        );
    }
}
