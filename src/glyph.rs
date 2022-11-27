use std::cmp::min;
use std::collections::{HashMap, HashSet};

pub mod braille;
pub mod glyph_constants;
pub mod hextant_blocks;

use braille::*;
use glyph_constants::*;
use hextant_blocks::*;

use ::num::clamp;
use euclid::*;
use euclid::{point2, vec2};
use line_drawing::Point;
use ordered_float::OrderedFloat;
use rgb::*;
use termion::color;

use crate::utility::sign;
use crate::utility::*;

// x, y
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

pub fn quadrant_block_by_offset(half_steps: IVector) -> char {
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

    pub fn fg_only(character: char, fg_color: RGB8) -> Glyph {
        Glyph {
            character,
            fg_color,
            bg_color: BLACK,
            bg_transparent: true,
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
        Glyph::new(quadrant_block_by_offset(step), color, BLACK)
    }

    pub fn offset_board_square_glyphs(
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

    pub fn is_braille(&self) -> bool {
        char_is_braille(self.character)
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

    pub fn looks_solid_specifically(&self, color: RGB8) -> bool {
        if let Some(solid_color) = self.get_solid_color() {
            color == solid_color
        } else {
            false
        }
    }

    pub fn looks_solid(&self) -> bool {
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
    pub fn is_fullwidth(&self) -> bool {
        self.is_chess()
            || self.character == MOVE_AND_CAPTURE_SQUARE_CHARS[0]
            || self.character == CONDITIONAL_MOVE_AND_CAPTURE_SQUARE_CHARS[0]
            || self.character == MOVE_ONLY_SQUARE_CHARS[0]
            || self.character == CAPTURE_ONLY_SQUARE_CHARS[0]
    }

    pub fn is_chess(&self) -> bool {
        SOLID_CHESS_PIECES.contains(&self.character)
    }

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

    // ╳
    pub fn block_glyphs() -> DoubleGlyph {
        [Glyph::new(' ', BLOCK_FG, BLOCK_BG); 2]
    }

    pub fn drawn_over(&self, background_glyphs: DoubleGlyph, is_left_glyph: bool) -> Glyph {
        let position_index = if is_left_glyph { 0 } else { 1 };
        let mut output_glyph = self.clone();
        if self.bg_transparent == true {
            let glyph_directly_below = background_glyphs[position_index];
            if self.is_braille() && glyph_directly_below.is_braille() {
                output_glyph.character =
                    combine_braille_characters(self.character, glyph_directly_below.character);
                output_glyph.bg_color = glyph_directly_below.bg_color;
            } else {
                let bg_colors = background_glyphs.solid_color_if_backgroundified();
                output_glyph.bg_color = bg_colors[position_index];
            }
        }
        output_glyph.bg_transparent = false;
        output_glyph
    }

    pub fn line_and_inside_point_to_angled_block_character(
        line_point_A: LocalCharacterPoint,
        line_point_B: LocalCharacterPoint,
        inside_point: LocalCharacterPoint,
    ) -> char {
        // angle blocks have important edge points

        //    0  0.5
        //    |  |
        // o──o──o -- 0.5
        // │     │
        // o     o -- 1/6
        // │     │ -- 0
        // o     o
        // │     │
        // o──o──o

        assert_ne!(line_point_A, line_point_B);
        assert_ne!(inside_point, line_point_B);
        assert_ne!(line_point_A, inside_point);

        // snap grid origin is bottom left of character, because the center doesn't really line up with the snap grid
        struct AngleBlockSnapGridInLocalFrame;
        type SnapGridPoint = Point2D<i32, AngleBlockSnapGridInLocalFrame>;
        fn local_snap_grid_to_local_character_frame(
            grid_point: &SnapGridPoint,
        ) -> LocalCharacterPoint {
            point2(
                grid_point.x as f32 * 0.5 - 0.5,
                grid_point.y as f32 * 1.0 / 3.0 - 0.5,
            )
        }

        fn local_character_frame_to_local_snap_grid(
            local_character_point: &LocalCharacterPoint,
        ) -> SnapGridPoint {
            point2(
                (local_character_point.x / 0.5) as i32 + 1,
                (local_character_point.y + 0.5) as i32 * 3,
            )
        }

        let mut valid_snap_grid_points = Vec::<SnapGridPoint>::new();
        for x in 0..3 {
            for y in 0..4 {
                if x == 1 && [1, 2].contains(&y) {
                    continue;
                }
                valid_snap_grid_points.push(point2(x, y));
            }
        }

        let snap_points: Vec<LocalCharacterPoint> = valid_snap_grid_points
            .iter()
            .map(local_snap_grid_to_local_character_frame)
            .collect();

        let raw_intersection_points =
            line_intersections_with_centered_unit_square(line_point_A, line_point_B);
        assert!(raw_intersection_points.len() <= 2);

        let snapped_points: Vec<LocalCharacterPoint> = raw_intersection_points
            .iter()
            .map(|&intersection_point| {
                *snap_points
                    .iter()
                    .min_by_key(|&&snap_point| {
                        OrderedFloat((intersection_point - snap_point).length())
                    })
                    .unwrap()
            })
            .collect();

        if snapped_points.len() < 2 {
            if same_side_of_line(line_point_A, line_point_B, inside_point, point2(0.0, 0.0)) {
                FULL_BLOCK
            } else {
                SPACE
            }
        } else {
            todo!()
        }
    }

    pub fn get_glyphs_for_braille_line(
        start_pos: WorldPoint,
        end_pos: WorldPoint,
    ) -> WorldCharacterGlyphMap {
        Glyph::get_glyphs_for_colored_braille_line(start_pos, end_pos, WHITE)
    }

    pub fn get_glyphs_for_colored_braille_line(
        start_pos: WorldPoint,
        end_pos: WorldPoint,
        color: RGB8,
    ) -> WorldCharacterGlyphMap {
        let start_char_point = world_point_to_world_character_point(start_pos);
        let end_char_point = world_point_to_world_character_point(end_pos);

        let mut glyph_map = HashMap::<Point2D<i32, CharacterGridInWorldFrame>, Glyph>::new();

        let start_braille_grid_square = world_character_point_to_braille_point(start_char_point)
            .round()
            .to_i32();
        let end_braille_grid_square = world_character_point_to_braille_point(end_char_point)
            .round()
            .to_i32();

        for (x, y) in line_drawing::Bresenham::new(
            start_braille_grid_square.to_tuple(),
            end_braille_grid_square.to_tuple(),
        ) {
            let braille_pos = Point2D::<i32, BrailleGridInWorldFrame>::new(x, y);
            let character_grid_square = world_braille_square_to_world_character_square(braille_pos);
            if !glyph_map.contains_key(&character_grid_square) {
                let mut new_glyph = Glyph::new(EMPTY_BRAILLE, color, BLACK);
                new_glyph.bg_transparent = true;
                glyph_map.insert(character_grid_square, new_glyph);
            }
            let braille_character =
                &mut glyph_map.get_mut(&character_grid_square).unwrap().character;
            *braille_character = add_braille_dot(
                *braille_character,
                braille_square_to_dot_in_character(braille_pos),
            );
        }
        return glyph_map;
    }

    pub fn points_to_braille_glyphs(
        points: Vec<WorldPoint>,
        color: RGB8,
    ) -> WorldCharacterGlyphMap {
        // bin braille squares by world character squares
        let mut local_braille_squares_by_character_square =
            HashMap::<WorldCharacterSquare, HashSet<WorldBrailleSquare>>::new();

        for point in points {
            let char_point = world_point_to_world_character_point(point);
            let char_square = char_point.round().to_i32();
            let braille_square = world_character_point_to_braille_point(char_point)
                .round()
                .to_i32();
            let local_braille_square = braille_square_to_dot_in_character(braille_square);

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
            let braille_char: char =
                local_braille_squares_to_braille_char(braille_square_set.into_iter().collect());
            let mut braille_glyph = Glyph::new(braille_char, color, BLACK);
            braille_glyph.bg_transparent = true;
            output_map.insert(char_square, braille_glyph);
        }
        output_map
    }
    pub fn character_world_pos_to_colored_braille_glyph(
        world_pos: Point2D<f32, CharacterGridInWorldFrame>,
        color: RGB8,
    ) -> Glyph {
        Glyph::new(character_world_pos_to_braille_char(world_pos), color, BLACK)
    }
}

pub trait DoubleGlyphFunctions {
    fn solid_color_if_backgroundified(&self) -> [RGB8; 2];
    fn drawn_over(&self, background_glyphs: DoubleGlyph) -> DoubleGlyph;
    fn to_string(&self) -> String;
    fn looks_solid(&self) -> bool;
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
        let glyphs = [
            self[0].drawn_over(background_glyphs, true),
            self[1].drawn_over(background_glyphs, false),
        ];
        glyphs
    }
    fn to_string(&self) -> String {
        self[0].to_string() + &self[1].to_string()
    }

    fn looks_solid(&self) -> bool {
        let left_solid_color_optional = self[0].get_solid_color();
        let right_solid_color_optional = self[1].get_solid_color();
        left_solid_color_optional.is_some()
            && left_solid_color_optional == right_solid_color_optional
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
            quadrant_block_by_offset(vec2(0, 0))
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
            quadrant_block_by_offset(vec2(0, 0))
        );
        assert_eq!(
            Glyph::colored_square_with_half_step_offset(vec2(0.24, 0.0), RED).character,
            quadrant_block_by_offset(vec2(0, 0))
        );
        assert_eq!(
            Glyph::colored_square_with_half_step_offset(vec2(0.25, 0.0), RED).character,
            quadrant_block_by_offset(vec2(1, 0))
        );
        assert_eq!(
            Glyph::colored_square_with_half_step_offset(vec2(0.26, 0.0), RED).character,
            quadrant_block_by_offset(vec2(1, 0))
        );
        assert_eq!(
            Glyph::colored_square_with_half_step_offset(vec2(-0.25, 0.0), RED).character,
            quadrant_block_by_offset(vec2(0, 0))
        );
        assert_eq!(
            Glyph::colored_square_with_half_step_offset(vec2(-0.26, 0.0), RED).character,
            quadrant_block_by_offset(vec2(-1, 0))
        );
        assert_eq!(
            Glyph::colored_square_with_half_step_offset(vec2(0.49, 0.0), RED).character,
            quadrant_block_by_offset(vec2(1, 0))
        );
        assert_eq!(
            Glyph::colored_square_with_half_step_offset(vec2(0.5, 0.0), RED).character,
            quadrant_block_by_offset(vec2(1, 0))
        );
        assert_eq!(
            Glyph::colored_square_with_half_step_offset(vec2(0.2, 0.4), RED).character,
            quadrant_block_by_offset(vec2(0, 1))
        );
        assert_eq!(
            Glyph::colored_square_with_half_step_offset(vec2(-0.499, 0.4), RED).character,
            quadrant_block_by_offset(vec2(-1, 1))
        );
        assert_eq!(
            Glyph::colored_square_with_half_step_offset(vec2(0.74, 0.0), RED).character,
            quadrant_block_by_offset(vec2(1, 0))
        );
        assert_eq!(
            Glyph::colored_square_with_half_step_offset(vec2(0.76, 0.0), RED).character,
            quadrant_block_by_offset(vec2(2, 0))
        );
        assert_eq!(
            Glyph::colored_square_with_half_step_offset(vec2(0.3, -0.6), RED).character,
            quadrant_block_by_offset(vec2(1, -1))
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
            quadrant_block_by_offset(vec2(0, 0))
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
            quadrant_block_by_offset(vec2(0, 1))
        );
        assert_eq!(
            glyphs[1][2].clone().unwrap().character,
            quadrant_block_by_offset(vec2(0, -1))
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
            quadrant_block_by_offset(vec2(0, 1))
        );
        assert_eq!(
            glyphs[1][2].clone().unwrap().character,
            quadrant_block_by_offset(vec2(0, -1))
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
            quadrant_block_by_offset(vec2(0, 0))
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
            quadrant_block_by_offset(vec2(1, 0))
        );
        assert!(glyphs[1][2] == None);
        assert!(glyphs[2][0] == None);
        assert_eq!(
            glyphs[2][1].clone().unwrap().character,
            quadrant_block_by_offset(vec2(-1, 0))
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
            quadrant_block_by_offset(vec2(1, 0))
        );
        assert!(glyphs[0][2] == None);
        assert!(glyphs[1][0] == None);
        assert_eq!(
            glyphs[1][1].clone().unwrap().character,
            quadrant_block_by_offset(vec2(-1, 0))
        );
        assert!(glyphs[1][2] == None);
        assert!(glyphs[2][0] == None);
        assert!(glyphs[2][1] == None);
        assert!(glyphs[2][2] == None);
    }

    #[test]
    fn test_world_pos_to_character_world_pos() {
        assert_eq!(
            Point2D::<f32, CharacterGridInWorldFrame>::new(0.5, 0.0),
            world_point_to_world_character_point(Point2D::<f32, SquareGridInWorldFrame>::new(
                0.0, 0.0
            )),
            "zero is actually between two characters"
        );
        assert_eq!(
            Point2D::<f32, CharacterGridInWorldFrame>::new(2.5, 1.0),
            world_point_to_world_character_point(Point2D::<f32, SquareGridInWorldFrame>::new(
                1.0, 1.0
            )),
            "diagonal a bit"
        );
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
        assert!(glyphs[0].looks_solid_specifically(RED));
        assert!(glyphs[1].looks_solid_specifically(RED));
    }

    #[test]
    fn test_double_glyph_square_offset__14_left() {
        // offset left
        let glyphs = Glyph::offset_board_square_glyphs(vec2(-0.14, 0.0), RED, BLACK);
        assert!(
            glyphs[0].looks_solid_specifically(RED),
            "glyph: {}",
            &glyphs[0].to_string()
        );
        //assert_eq!(glyphs[1].character, '▏');
        assert_eq!(glyphs[1], Glyph::new('▊', RED, BLACK));
    }

    #[test]
    fn test_double_glyph_square_offset__25_left() {
        // offset left
        let glyphs = Glyph::offset_board_square_glyphs(vec2(-0.25, 0.0), RED, BLACK);
        assert!(glyphs[0].looks_solid_specifically(RED));
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
        assert!(glyphs[1].looks_solid_specifically(BLACK));
    }

    #[test]
    fn test_double_glyph_square_offset__25_right() {
        // offset right
        let glyphs = Glyph::offset_board_square_glyphs(vec2(0.25, 0.0), RED, BLACK);
        assert_eq!(glyphs[0].character, '▌');
        assert_eq!(glyphs[0].fg_color, BLACK);
        assert_eq!(glyphs[0].bg_color, RED);
        assert!(glyphs[1].looks_solid_specifically(RED));
    }

    #[test]
    fn test_double_glyph_square_offset__50_right() {
        // offset right
        let glyphs = Glyph::offset_board_square_glyphs(vec2(0.50, 0.0), RED, BLACK);
        assert!(glyphs[0].looks_solid_specifically(BLACK));
        assert!(glyphs[1].looks_solid_specifically(RED));
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
            glyphs[0].looks_solid_specifically(BLACK),
            "glyph: {}",
            &glyphs[0].to_string()
        );
        assert!(
            glyphs[1].looks_solid_specifically(BLACK),
            "glyph: {}",
            &glyphs[1].to_string()
        );
    }

    #[test]
    fn test_double_glyph_square_offset__partial_character_past_full_square_right() {
        // offset right
        let glyphs = Glyph::offset_board_square_glyphs(vec2(1.25, 0.0), RED, BLACK);
        assert_eq!(glyphs[0], Glyph::new(LEFT_HALF_BLOCK, RED, BLACK));
        assert!(
            glyphs[1].looks_solid_specifically(BLACK),
            "glyph: {}",
            &glyphs[1].to_string()
        );
    }

    //                      |<--halfway
    // ' ', '▏', '▎', '▍', '▌', '▋', '▊', '▉', '█'
    #[test]
    fn test_character_square_horizontal_offset__base_case() {
        assert!(
            Glyph::colored_character_square_with_offset(false, 0.0, RED, BLACK)
                .looks_solid_specifically(RED)
        );
    }

    #[test]
    fn test_character_square_horizontal_offset__round_to_zero() {
        assert!(
            Glyph::colored_character_square_with_offset(false, -0.001, RED, BLACK)
                .looks_solid_specifically(RED)
        );

        assert!(
            Glyph::colored_character_square_with_offset(false, 0.001, RED, BLACK)
                .looks_solid_specifically(RED)
        );
    }

    #[test]
    fn test_character_square_horizontal_offset__out_of_range() {
        assert_eq!(
            Glyph::colored_character_square_with_offset(false, -1.5, RED, BLACK),
            Glyph::new(' ', RED, BLACK)
        );
        assert_eq!(
            Glyph::colored_character_square_with_offset(false, 1.5, RED, BLACK),
            Glyph::new('█', BLACK, RED)
        );
    }

    #[test]
    fn test_character_square_horizontal_offset__halfway() {
        assert_eq!(
            Glyph::colored_character_square_with_offset(false, -0.5, RED, BLACK),
            Glyph::new('▌', RED, BLACK)
        );
        assert_eq!(
            Glyph::colored_character_square_with_offset(false, 0.5, RED, BLACK),
            Glyph::new('▌', BLACK, RED)
        );
    }

    #[test]
    fn test_character_square_horizontal_offset__match_opposite_ends() {
        assert!(
            Glyph::colored_character_square_with_offset(false, -1.0, RED, BLACK)
                .looks_solid_specifically(BLACK)
        );
        assert!(
            Glyph::colored_character_square_with_offset(false, 1.0, RED, BLACK)
                .looks_solid_specifically(BLACK)
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
    fn test_hextant_array_to_char() {
        assert_eq!(
            hextant_array_to_char([[false, false], [false, false], [false, false],]),
            SPACE
        );
        assert_eq!(
            hextant_array_to_char([[true, true], [true, true], [true, true],]),
            FULL_BLOCK
        );
        assert_eq!(
            hextant_array_to_char([[true, false], [true, false], [true, false],]),
            LEFT_HALF_BLOCK
        );
        assert_eq!(
            hextant_array_to_char([[false, true], [false, true], [false, true],]),
            RIGHT_HALF_BLOCK
        );
        assert_eq!(
            hextant_array_to_char([[true, false], [false, false], [false, false],]),
            '🬀'
        );
        assert_eq!(
            hextant_array_to_char([[false, true], [true, true], [false, false],]),
            '🬍'
        );
    }

    #[test]
    fn test_line_and_inside_point_to_angled_block_character() {
        let line_point_A: LocalCharacterPoint = point2(-0.5, -0.5);
        assert_eq!(
            Glyph::line_and_inside_point_to_angled_block_character(
                point2(-0.5, -0.5),
                point2(-0.5, 0.5),
                point2(0.0, 0.0),
            ),
            FULL_BLOCK,
            "on left edge, full block"
        );
        assert_eq!(
            Glyph::line_and_inside_point_to_angled_block_character(
                point2(-0.5, -0.5),
                point2(-0.5, 0.5),
                point2(-20.0, 0.0),
            ),
            SPACE,
            "on left edge, empty block"
        );
        assert_eq!(
            Glyph::line_and_inside_point_to_angled_block_character(
                point2(-0.5, -0.5),
                point2(-0.4, -0.4),
                point2(2.0, 0.0),
            ),
            '◢',
            "lower-right diagonal given short line"
        );
        assert_eq!(
            Glyph::line_and_inside_point_to_angled_block_character(
                point2(0.0, -0.5),
                point2(0.5, -0.15),
                point2(0.0, 0.0),
            ),
            '🭝',
            "Notch off bottom-right"
        );
    }

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
    fn test_world_point_to_braille_glyph() {
        let points = [point2(0.0, 0.0), point2(-0.4, -0.4), point2(0.2, 0.4)];
        for p1 in points {
            assert_eq!(
                Glyph::character_world_pos_to_colored_braille_glyph(p1, BLACK).character,
                character_world_pos_to_braille_char(p1)
            );
        }
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
            world_character_point_to_world_point(start),
            world_character_point_to_world_point(end),
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
            world_character_point_to_world_point(start),
            world_character_point_to_world_point(end),
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
            world_character_point_to_world_point(start),
            world_character_point_to_world_point(end),
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
}
