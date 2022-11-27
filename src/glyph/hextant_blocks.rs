use super::glyph_constants::*;

pub const FIRST_HEXTANT: char = '🬀';
pub const LAST_HEXTANT: char = '🬻';

pub type HextantArray = [[bool; 2]; 3]; // row, column

// all hextant blocks
// U+1FB0x 🬀 🬁 🬂 🬃 🬄 🬅 🬆 🬇 🬈 🬉 🬊 🬋 🬌 🬍 🬎 🬏
// U+1FB1x 🬐 🬑 🬒 🬓 🬔 🬕 🬖 🬗 🬘 🬙 🬚 🬛 🬜 🬝 🬞 🬟
// U+1FB2x 🬠 🬡 🬢 🬣 🬤 🬥 🬦 🬧 🬨 🬩 🬪 🬫 🬬 🬭 🬮 🬯
// U+1FB3x 🬰 🬱 🬲 🬳 🬴 🬵 🬶 🬷 🬸 🬹 🬺 🬻
// 2^6 = 64 = 4* 16
// missing hextant blocks: empty, left half, right half, full

pub fn hextant_array_to_char(hextant_array: HextantArray) -> char {
    let as_binary = hextant_array_as_binary(hextant_array);
    let before_half_left = '🬓';
    let after_half_left = '🬔';
    let before_half_right = '🬧';
    let after_half_right = '🬨';

    match as_binary {
        const { hextant_character_as_binary(SPACE) } => SPACE,
        const { hextant_character_as_binary(LEFT_HALF_BLOCK) } => LEFT_HALF_BLOCK,
        const { hextant_character_as_binary(RIGHT_HALF_BLOCK) } => RIGHT_HALF_BLOCK,
        const { hextant_character_as_binary(FULL_BLOCK) } => FULL_BLOCK,
        _ => {
            let unadjusted_value = FIRST_HEXTANT as u32 + as_binary as u32;
            let offset = if unadjusted_value
                < hextant_character_to_value_it_damn_well_should_have(LEFT_HALF_BLOCK)
            {
                1
            } else if unadjusted_value
                < hextant_character_to_value_it_damn_well_should_have(RIGHT_HALF_BLOCK)
            {
                2
            } else {
                3
            };
            char::from_u32(unadjusted_value - offset).unwrap()
        }
    }
}

const fn char_is_hextant(character: char) -> bool {
    character == SPACE
        || character == LEFT_HALF_BLOCK
        || character == RIGHT_HALF_BLOCK
        || character == FULL_BLOCK
        || (FIRST_HEXTANT <= character && character <= LAST_HEXTANT)
}

fn hextant_array_as_binary(hextant_array: HextantArray) -> u8 {
    let mut out = 0;
    for row in 0..hextant_array.len() {
        for column in 0..hextant_array[row].len() {
            let position_value = 4u8.pow(row as u32) * 2u8.pow(column as u32);
            if hextant_array[row][column] {
                out += position_value;
            }
        }
    }
    out
}
const fn hextant_character_as_binary(hextant_character: char) -> u8 {
    assert!(char_is_hextant(hextant_character));
    let before_half_left = '🬓';
    let before_half_right = '🬧';
    match hextant_character {
        SPACE => 0,
        LEFT_HALF_BLOCK => 1 + 4 + 16,
        RIGHT_HALF_BLOCK => 2 + 8 + 32,
        FULL_BLOCK => 1 + 2 + 4 + 8 + 16 + 32,
        _ => {
            let raw_value = hextant_character as u32;
            let offset = if raw_value <= before_half_left as u32 {
                1
            } else if raw_value <= before_half_right as u32 {
                2
            } else {
                3
            };
            (raw_value - FIRST_HEXTANT as u32 + offset) as u8
        }
    }
}

fn hextant_character_to_value_it_damn_well_should_have(character: char) -> u32 {
    // If its empty, full, and horizontal halfblocks weren't already taken
    assert!(char_is_hextant(character));
    FIRST_HEXTANT as u32 + hextant_character_as_binary(character) as u32
}
