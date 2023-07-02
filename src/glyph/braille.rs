use crate::glyph::DoubleChar;
use crate::utility::coordinate_frame_conversions::*;
use crate::utility::*;
use euclid::{point2, Point2D};
use std::collections::{HashMap, HashSet};
use std::ops::BitXor;
use std::string::ToString;

pub struct BrailleGridInWorldFrame;
pub type WorldBrailleSquare = Point2D<i32, BrailleGridInWorldFrame>;
pub type WorldBraillePoint = Point2D<f32, BrailleGridInWorldFrame>;

pub struct BrailleGridInCharacterFrame;
pub type LocalCharacterBrailleSquare = Point2D<i32, BrailleGridInCharacterFrame>;
pub type LocalCharacterBraillePoint = Point2D<f32, BrailleGridInCharacterFrame>;

pub struct BrailleGridInLocalSquareFrame;
pub type LocalBrailleSquare = Point2D<i32, BrailleGridInLocalSquareFrame>;
pub type LocalBraillePoint = Point2D<f32, BrailleGridInLocalSquareFrame>;

pub const EMPTY_BRAILLE: char = '\u{2800}';
pub const FULL_BRAILLE: char = '⣿';

// All the braille unicode consecutively for easy reference
pub const ALL_BRAILLE_IN_ONE_STRING: &str = "\u{2800}⠁⠂⠃⠄⠅⠆⠇⠈⠉⠊⠋⠌⠍⠎⠏⠐⠑⠒⠓⠔⠕⠖⠗⠘⠙⠚⠛⠜⠝⠞⠟⠠⠡⠢⠣⠤⠥⠦⠧⠨⠩⠪⠫⠬⠭⠮⠯⠰⠱⠲⠳⠴⠵⠶⠷⠸⠹⠺⠻⠼⠽⠾⠿⡀⡁⡂⡃⡄⡅⡆⡇⡈⡉⡊⡋⡌⡍⡎⡏⡐⡑⡒⡓⡔⡕⡖⡗⡘⡙⡚⡛⡜⡝⡞⡟⡠⡡⡢⡣⡤⡥⡦⡧⡨⡩⡪⡫⡬⡭⡮⡯⡰⡱⡲⡳⡴⡵⡶⡷⡸⡹⡺⡻⡼⡽⡾⡿⢀⢁⢂⢃⢄⢅⢆⢇⢈⢉⢊⢋⢌⢍⢎⢏⢐⢑⢒⢓⢔⢕⢖⢗⢘⢙⢚⢛⢜⢝⢞⢟⢠⢡⢢⢣⢤⢥⢦⢧⢨⢩⢪⢫⢬⢭⢮⢯⢰⢱⢲⢳⢴⢵⢶⢷⢸⢹⢺⢻⢼⢽⢾⢿⣀⣁⣂⣃⣄⣅⣆⣇⣈⣉⣊⣋⣌⣍⣎⣏⣐⣑⣒⣓⣔⣕⣖⣗⣘⣙⣚⣛⣜⣝⣞⣟⣠⣡⣢⣣⣤⣥⣦⣧⣨⣩⣪⣫⣬⣭⣮⣯⣰⣱⣲⣳⣴⣵⣶⣷⣸⣹⣺⣻⣼⣽⣾⣿";

#[derive(Debug, Clone)]
pub struct BrailleArray {
    array: [[bool; 2]; 4],
}
#[derive(Debug, Clone)]
pub struct DoubleBrailleArray {
    array: [[bool; 4]; 4],
}

impl BrailleArray {
    const WIDTH: usize = 2;
    const HEIGHT: usize = 4;
    pub fn from_char(c: char) -> Self {
        let mut braille_array = BrailleArray::empty();
        let dot_val = (c as u32).bitxor(EMPTY_BRAILLE as u32);
        for x in 0..2 {
            for y in 0..4 {
                let just_the_bit = braille_bit_for_pos(point2(x as i32, y as i32));
                let there_is_a_dot_here = just_the_bit & dot_val != 0;
                if there_is_a_dot_here {
                    braille_array.set_xy(x, y, true);
                }
            }
        }
        braille_array
    }
    pub fn char(&self) -> char {
        let mut dot_val: u32 = 0;
        for x in 0..2 {
            for y in 0..4 {
                if self.get_xy(x, y) {
                    dot_val |= braille_bit_for_pos(point2(x as i32, y as i32));
                }
            }
        }
        return char::from_u32(EMPTY_BRAILLE as u32 | dot_val).unwrap();
    }
    pub fn from_array(array: [[bool; Self::WIDTH]; Self::HEIGHT]) -> Self {
        Self { array }
    }
    pub fn empty() -> Self {
        Self::from_array([[false; Self::WIDTH]; Self::HEIGHT])
    }
    pub fn get_xy(&self, x: usize, y: usize) -> bool {
        self.get_row_col(Self::y_to_row(y), x)
    }
    pub fn set_xy(&mut self, x: usize, y: usize, val: bool) {
        self.set_row_col(Self::y_to_row(y), x, val);
    }
    fn y_to_row(y: usize) -> usize {
        Self::HEIGHT - 1 - y
    }
    pub fn get_row_col(&self, row: usize, col: usize) -> bool {
        self.array[row][col]
    }
    pub fn set_row_col(&mut self, row: usize, col: usize, val: bool) {
        self.array[row][col] = val;
    }
}

impl DoubleBrailleArray {
    const WIDTH: usize = 4;
    const HEIGHT: usize = 4;
    pub fn from_chars(chars: DoubleChar) -> Self {
        Self::from_two_braille_arrays(chars.map(|c| BrailleArray::from_char(c)))
    }
    pub fn chars(&self) -> DoubleChar {
        self.to_two_braille_arrays()
            .map(|braille_array| braille_array.char())
    }
    pub fn from_two_braille_arrays(arrays: [BrailleArray; 2]) -> Self {
        let mut double_braille_array = Self::empty();
        for row in 0..Self::HEIGHT {
            for col in 0..Self::WIDTH {
                let sub_col = col % BrailleArray::WIDTH;
                let index = col / BrailleArray::WIDTH;
                double_braille_array.set_row_col(row, col, arrays[index].get_row_col(row, sub_col));
            }
        }
        double_braille_array
    }
    pub fn to_two_braille_arrays(&self) -> [BrailleArray; 2] {
        let mut arrays = [BrailleArray::empty(), BrailleArray::empty()];
        for row in 0..Self::HEIGHT {
            for col in 0..Self::WIDTH {
                let sub_col = col % BrailleArray::WIDTH;
                let index = col / BrailleArray::WIDTH;
                arrays[index].set_row_col(row, sub_col, self.get_row_col(row, col));
            }
        }
        arrays
    }
    pub fn rotated(&self, quarter_turns: QuarterTurnsAnticlockwise) -> Self {
        let rotation_function = match quarter_turns.quarter_turns() {
            0 => |x, y| (x, y),
            1 => |x, y| (Self::WIDTH - 1 - y, x),
            2 => |x, y| (Self::WIDTH - 1 - x, Self::HEIGHT - 1 - y),
            3 => |x, y| (y, Self::HEIGHT - 1 - x),
            _ => panic!("Malformed parameter"),
        };
        let mut the_clone = self.clone();
        for x in 0..Self::WIDTH {
            for y in 0..Self::HEIGHT {
                let (new_x, new_y) = rotation_function(x, y);
                the_clone.set_xy(new_x, new_y, self.get_xy(x, y));
            }
        }
        the_clone
    }
    pub fn from_array(array: [[bool; Self::WIDTH]; Self::HEIGHT]) -> Self {
        Self { array }
    }
    pub fn empty() -> Self {
        Self::from_array([[false; Self::WIDTH]; Self::HEIGHT])
    }
    pub fn get_xy(&self, x: usize, y: usize) -> bool {
        self.get_row_col(Self::y_to_row(y), x)
    }
    pub fn set_xy(&mut self, x: usize, y: usize, val: bool) {
        self.set_row_col(Self::y_to_row(y), x, val);
    }
    fn y_to_row(y: usize) -> usize {
        Self::HEIGHT - 1 - y
    }
    pub fn get_row_col(&self, row: usize, col: usize) -> bool {
        self.array[row][col]
    }
    pub fn set_row_col(&mut self, row: usize, col: usize, val: bool) {
        self.array[row][col] = val;
    }
}

pub fn braille_bit_for_pos(p: Point2D<i32, BrailleGridInWorldFrame>) -> u32 {
    let braille_value_map = vec![vec![7, 3, 2, 1], vec![8, 6, 5, 4]];
    1 << (braille_value_map[p.x as usize][p.y as usize] - 1)
}

pub fn add_braille_dot(character: char, p: Point2D<i32, BrailleGridInWorldFrame>) -> char {
    char::from_u32(character as u32 | braille_bit_for_pos(p)).unwrap()
}

pub fn char_is_braille(c: char) -> bool {
    let x = c as u32;
    // The unicode braille block
    // TODO: This includes empty braille.  Bad?
    x >= 0x2800 && x <= 0x28FF
}

pub fn count_braille_dots(character: char) -> u32 {
    if !char_is_braille(character) {
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
    assert!(char_is_braille(c1));
    assert!(char_is_braille(c2));
    char::from_u32(c1 as u32 | c2 as u32).unwrap()
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

pub fn world_point_to_world_braille_point(pos: WorldPoint) -> WorldBraillePoint {
    world_character_point_to_braille_point(world_point_to_world_character_point(pos))
}
pub fn world_braille_point_to_world_point(pos: WorldBraillePoint) -> WorldPoint {
    world_character_point_to_world_point(world_braille_point_to_world_character_point(pos))
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
    world_braille_point_to_world_character_point(braille_square.to_f32())
        .round()
        .to_i32()
}

pub fn world_points_for_braille_line(
    start_pos: WorldPoint,
    end_pos: WorldPoint,
) -> Vec<WorldPoint> {
    let braille_start_square = world_point_to_world_braille_point(start_pos)
        .round()
        .to_i32();
    let braille_end_square = world_point_to_world_braille_point(end_pos).round().to_i32();

    line_drawing::Bresenham::new(
        braille_start_square.to_tuple(),
        braille_end_square.to_tuple(),
    )
    .map(|(x, y)| WorldBraillePoint::new(x as f32, y as f32))
    .map(world_braille_point_to_world_point)
    .collect()
}

pub fn character_world_pos_to_braille_char(
    world_pos: Point2D<f32, CharacterGridInWorldFrame>,
) -> char {
    let character = EMPTY_BRAILLE;
    add_braille_dot(
        character,
        braille_square_to_dot_in_character(
            world_character_point_to_braille_point(world_pos)
                .round()
                .to_i32(),
        ),
    )
}

pub fn local_braille_squares_to_braille_array(squares: Vec<WorldBrailleSquare>) -> BrailleArray {
    let mut output_array: BrailleArray = BrailleArray::empty();
    for square in squares {
        assert!(square.x >= 0 || square.x < 2);
        assert!(square.y >= 0 || square.y < 4);
        output_array.set_xy(square.x as usize, square.y as usize, true);
    }
    output_array
}

pub fn local_braille_squares_to_braille_char(squares: Vec<WorldBrailleSquare>) -> char {
    local_braille_squares_to_braille_array(squares).char()
}

pub fn get_chars_for_braille_line(
    start_pos: WorldPoint,
    end_pos: WorldPoint,
) -> WorldCharacterSquareToCharMap {
    let start_char_point = world_point_to_world_character_point(start_pos);
    let end_char_point = world_point_to_world_character_point(end_pos);

    let mut char_map = WorldCharacterSquareToCharMap::new();

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
        if !char_map.contains_key(&character_grid_square) {
            char_map.insert(character_grid_square, EMPTY_BRAILLE);
        }
        let braille_character = char_map.get_mut(&character_grid_square).unwrap();
        *braille_character = add_braille_dot(
            *braille_character,
            braille_square_to_dot_in_character(braille_pos),
        );
    }
    return char_map;
}
pub fn get_braille_arrays_for_braille_line(
    start_pos: WorldPoint,
    end_pos: WorldPoint,
) -> HashMap<WorldSquare, DoubleBrailleArray> {
    let chars = get_chars_for_braille_line(start_pos, end_pos);
    let paired_chars = pair_up_character_square_map(chars);
    paired_chars
        .iter()
        .map(|(&world_square, &double_char)| {
            (world_square, DoubleBrailleArray::from_chars(double_char))
        })
        .collect()
}
pub fn points_to_braille_chars(points: Vec<WorldPoint>) -> WorldCharacterSquareToCharMap {
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

    let mut char_map = WorldCharacterSquareToCharMap::new();

    for (char_square, braille_square_set) in local_braille_squares_by_character_square {
        let braille_char: char =
            local_braille_squares_to_braille_char(braille_square_set.into_iter().collect());
        char_map.insert(char_square, braille_char);
    }
    char_map
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::{assert_eq, assert_ne};

    #[test]
    fn test_array_to_braille_char() {
        // 10
        // 00
        // 01
        // 01
        assert_eq!(
            BrailleArray::from_array([[true, false], [false, false], [false, true], [false, true]])
                .char(),
            '⢡'
        );

        // 00
        // 11
        // 01
        // 00
        assert_eq!(
            BrailleArray::from_array([[false, false], [true, true], [false, true], [false, false]])
                .char(),
            '⠲'
        );
    }

    #[test]
    fn test_set_braille_dot() {
        let mut b = EMPTY_BRAILLE;
        b = add_braille_dot(b, point2(0, 0));
        b = add_braille_dot(b, point2(1, 1));
        assert_eq!(b, '⡠');
    }

    #[test]
    fn test_braille_grid_to_character_grid() {
        assert_eq!(
            world_braille_square_to_world_character_square(point2(0, 0)),
            point2(0, 0)
        );
        assert_eq!(
            world_braille_square_to_world_character_square(point2(1, 3)),
            point2(0, 0)
        );
        assert_eq!(
            world_braille_square_to_world_character_square(point2(-1, -1)),
            point2(-1, -1)
        );
        assert_eq!(
            world_braille_square_to_world_character_square(point2(2, 8)),
            point2(1, 2)
        );
        assert_eq!(
            world_braille_square_to_world_character_square(point2(21, 80)),
            point2(10, 20)
        );
    }

    #[test]
    fn test_world_pos_to_braille_pos() {
        assert_eq!(
            world_character_point_to_braille_point(point2(0.0, 0.0)),
            point2(0.5, 1.5)
        );
        assert_eq!(
            world_character_point_to_braille_point(point2(1.0, 0.0)),
            point2(2.5, 1.5)
        );
        assert_eq!(
            world_character_point_to_braille_point(point2(0.25, 0.375)),
            point2(1.0, 3.0)
        );
    }

    #[test]
    fn test_braille_pos_to_world_pos() {
        assert_eq!(
            braille_pos_to_character_world_pos(point2(0.5, 1.5)),
            point2(0.0, 0.0)
        );
        assert_eq!(
            braille_pos_to_character_world_pos(point2(2.5, 1.5)),
            point2(1.0, 0.0)
        );
        assert_eq!(
            braille_pos_to_character_world_pos(point2(1.0, 3.0)),
            point2(0.25, 0.375)
        );
    }

    #[test]
    fn test_braille_square_to_dot_in_character() {
        assert_eq!(
            braille_square_to_dot_in_character(point2(0, 0)),
            point2(0, 0)
        );
        assert_eq!(
            braille_square_to_dot_in_character(point2(1, 3)),
            point2(1, 3)
        );
        assert_eq!(
            braille_square_to_dot_in_character(point2(25, 4)),
            point2(1, 0)
        );
        assert_eq!(
            braille_square_to_dot_in_character(point2(-3, 4)),
            point2(1, 0)
        );
    }

    #[test]
    fn test_combine_braille_character() {
        assert_eq!(
            combine_braille_characters('\u{2800}', '\u{2820}'),
            '\u{2820}'
        );
        assert_eq!(
            combine_braille_characters('\u{2801}', '\u{28C0}'),
            '\u{28C1}'
        );
    }

    #[test]
    fn test_world_point_to_braille_char() {
        assert_eq!(
            character_world_pos_to_braille_char(point2(0.0, 0.0)),
            '\u{2810}'
        );
        assert_eq!(
            character_world_pos_to_braille_char(point2(-0.4, -0.4)),
            '\u{2840}'
        );
        assert_eq!(
            character_world_pos_to_braille_char(point2(0.2, 0.4)),
            '\u{2808}'
        );
    }

    #[test]
    fn test_world_point_to_braille_char_is_always_braille() {
        for _ in 0..200 {
            //let random_point = p(rand_in_range(0.0, 30.0), rand_in_range(0.0, 30.0));
            let random_point = point2(23.2273, 2.05);

            assert!(char_is_braille(character_world_pos_to_braille_char(
                random_point
            )));
        }
    }

    #[test]
    fn test_count_braille_dots() {
        assert_eq!(count_braille_dots('\u{2800}'), 0);
        assert_eq!(count_braille_dots('\u{2818}'), 2);
        assert_eq!(count_braille_dots('\u{28C0}'), 2);
        assert_eq!(count_braille_dots('\u{28FF}'), 8);
        assert_eq!(count_braille_dots('A'), 0);
        assert_eq!(count_braille_dots('#'), 0);
    } //⠁⠂⠃⠄⠅⠆⠇⠈⠉⠊⠋⠌⠍⠎⠏⠐⠑⠒⠓⠔⠕⠖⠗⠘⠙⠚⠛⠜⠝⠞⠟⠠⠡⠢⠣⠤⠥⠦⠧⠨⠩⠪⠫⠬⠭⠮⠯⠰⠱⠲⠳⠴⠵⠶⠷⠸⠹⠺⠻⠼⠽⠾⠿⡀⡁⡂⡃⡄⡅⡆⡇⡈⡉⡊⡋⡌⡍⡎⡏⡐⡑⡒⡓⡔⡕⡖⡗⡘⡙⡚⡛⡜⡝⡞⡟⡠⡡⡢⡣⡤⡥⡦⡧⡨⡩⡪⡫⡬⡭⡮⡯⡰⡱⡲⡳⡴⡵⡶⡷⡸⡹⡺⡻⡼⡽⡾⡿⢀⢁⢂⢃⢄⢅⢆⢇⢈⢉⢊⢋⢌⢍⢎⢏⢐⢑⢒⢓⢔⢕⢖⢗⢘⢙⢚⢛⢜⢝⢞⢟⢠⢡⢢⢣⢤⢥⢦⢧⢨⢩⢪⢫⢬⢭⢮⢯⢰⢱⢲⢳⢴⢵⢶⢷⢸⢹⢺⢻⢼⢽⢾⢿⣀⣁⣂⣃⣄⣅⣆⣇⣈⣉⣊⣋⣌⣍⣎⣏⣐⣑⣒⣓⣔⣕⣖⣗⣘⣙⣚⣛⣜⣝⣞⣟⣠⣡⣢⣣⣤⣥⣦⣧⣨⣩⣪⣫⣬⣭⣮⣯⣰⣱⣲⣳⣴⣵⣶⣷⣸⣹⣺⣻⣼⣽⣾⣿

    #[test]
    fn test_chars_for_horizontal_braille_line_without_rounding() {
        let start: WorldCharacterPoint = point2(-0.25, -0.4);
        let end: WorldCharacterPoint = point2(1.75, -0.4);

        // Expected braille:
        // 00 00 00
        // 00 00 00
        // 00 00 00
        // 11 11 10

        let line_chars = get_chars_for_braille_line(
            world_character_point_to_world_point(start),
            world_character_point_to_world_point(end),
        );
        assert_eq!(line_chars.len(), 3);

        assert_eq!(line_chars.get(&point2(0, 0)).unwrap(), &'\u{28C0}');
        assert_eq!(line_chars.get(&point2(1, 0)).unwrap(), &'\u{28C0}');
        assert_eq!(line_chars.get(&point2(2, 0)).unwrap(), &'\u{2840}');
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

        let line_glyphs = get_chars_for_braille_line(
            world_character_point_to_world_point(start),
            world_character_point_to_world_point(end),
        );
        assert_eq!(line_glyphs.len(), 3);

        assert_eq!(line_glyphs.get(&point2(0, 0)).unwrap(), &'\u{2809}');
        assert_eq!(line_glyphs.get(&point2(1, 0)).unwrap(), &'\u{2809}');
        assert_eq!(line_glyphs.get(&point2(2, 0)).unwrap(), &'\u{2801}');
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

        let line_glyphs = get_chars_for_braille_line(
            world_character_point_to_world_point(start),
            world_character_point_to_world_point(end),
        );
        assert_eq!(line_glyphs.len(), 2);

        assert_eq!(line_glyphs.get(&point2(0, 0)).unwrap(), &'\u{2847}');
        assert_eq!(line_glyphs.get(&point2(0, 1)).unwrap(), &'\u{2844}');
    }

    #[test]
    fn test_points_to_braille_chars() {
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

        let chars = points_to_braille_chars(points);

        assert_eq!(chars.len(), 3);
        assert_eq!(chars.get(&point2(0, 0)).unwrap(), &'⠠');
        assert_eq!(chars.get(&point2(1, 0)).unwrap(), &'⠂');
        assert_eq!(chars.get(&point2(3, 0)).unwrap(), &'⠒');
    }
    #[test]
    fn test_the_big_braille_string() {
        ALL_BRAILLE_IN_ONE_STRING
            .chars()
            .for_each(|c| assert!(char_is_braille(c)));
    }
    #[test]
    fn test_braille_array_to_and_from_char() {
        ALL_BRAILLE_IN_ONE_STRING
            .chars()
            .for_each(|c| assert_eq!(BrailleArray::from_char(c).char(), c));
    }
    #[test]
    fn test_double_braille_array_rotation() {
        let mut array = DoubleBrailleArray::empty();
        array.set_xy(2, 0, true);
        array.set_xy(2, 1, true);
        // ....
        // ....
        // ..o.
        // ..o.
        let turns_xys = vec![
            (1, vec![(2, 2), (3, 2)]),
            (2, vec![(1, 3), (1, 2)]),
            (3, vec![(0, 1), (1, 1)]),
            (-1, vec![(0, 1), (1, 1)]),
        ];
        for t in turns_xys {
            for p in t.1 {
                assert_eq!(
                    array
                        .rotated(QuarterTurnsAnticlockwise::new(t.0))
                        .get_xy(p.0, p.1),
                    true
                );
            }
        }
    }
}
