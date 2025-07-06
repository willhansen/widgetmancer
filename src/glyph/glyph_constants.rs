use rgb::*;

pub mod named_colors {
    use super::*;

    pub const EGYPTIAN_BLUE: RGB8 = RGB8::new(20, 52, 164);
    pub const PLAYER_COLOR: RGB8 = EGYPTIAN_BLUE;
    pub const RED: RGB8 = RGB8::new(255, 0, 0);
    pub const MAROON: RGB8 = RGB8::new(127, 0, 0);
    pub const DARKER_RED: RGB8 = RGB8::new(63, 0, 0);
    pub const BRICK_RED: RGB8 = RGB8::new(255, 87, 51);
    pub const GREEN: RGB8 = RGB8::new(0, 255, 0);
    pub const BLUE: RGB8 = RGB8::new(0, 0, 255);
    pub const COBALT_BLUE: RGB8 = RGB8::new(0, 71, 171);
    pub const LIGHTISH_BLUE: RGB8 = RGB8::new(0, 130, 170);
    pub const CYAN: RGB8 = RGB8::new(0, 255, 255);
    pub const DARK_CYAN: RGB8 = RGB8::new(0, 127, 127);
    pub const MAGENTA: RGB8 = RGB8::new(255, 0, 255);
    pub const YELLOW: RGB8 = RGB8::new(255, 255, 0);
    pub const WHITE: RGB8 = RGB8::new(255, 255, 255);
    pub const LIGHT_GREY: RGB8 = RGB8::new(191, 191, 191);
    pub const GREY: RGB8 = RGB8::new(127, 127, 127);
    pub const DARK_GREY: RGB8 = RGB8::new(63, 63, 63);
    pub const BLACK: RGB8 = RGB8::new(0, 0, 0);
    pub const BOARD_WHITE: RGB8 = LIGHT_GREY;
    pub const BOARD_BLACK: RGB8 = GREY;
    pub const GREY_RED: RGB8 = RGB8::new(127, 100, 100);
    pub const OUT_OF_SIGHT_COLOR: RGB8 = BLACK; //DARK_GREY;
}
pub use named_colors::*;

pub const BLOCK_BG: RGB8 = BLACK;
pub const BLOCK_FG: RGB8 = BLACK;

pub mod named_chars {
    use super::*;

    pub const LEFT_HALF_BLOCK: char = '▌';
    pub const RIGHT_HALF_BLOCK: char = '▐';
    pub const LOWER_HALF_BLOCK: char = '▄';
    pub const UPPER_HALF_BLOCK: char = '▀';

    pub const UPPER_ONE_THIRD_BLOCK: char = '🬂';
    pub const UPPER_TWO_THIRD_BLOCK: char = '🬎';
    pub const LOWER_ONE_THIRD_BLOCK: char = '🬭';
    pub const LOWER_TWO_THIRD_BLOCK: char = '🬹';

    pub const UPPER_RIGHT_HALF_BLOCK_TRIANGLE: char = '◥';
    pub const UPPER_LEFT_HALF_BLOCK_TRIANGLE: char = '◤';
    pub const LOWER_RIGHT_HALF_BLOCK_TRIANGLE: char = '◢';
    pub const LOWER_LEFT_HALF_BLOCK_TRIANGLE: char = '◣';

    pub const FULL_BLOCK: char = '█';
    pub const SPACE: char = ' ';
    pub const GOOMBA: char = '⍾';

    pub const LEFT_ONE_EIGHTH_BLOCK: char = '▏';
    pub const LEFT_ONE_QUARTER_BLOCK: char = '▎';
    pub const LEFT_THREE_EIGHTHS_BLOCK: char = '▍';
    pub const LEFT_FIVE_EIGHTHS_BLOCK: char = '▋';
    pub const LEFT_THREE_QUARTERS_BLOCK: char = '▊';
    pub const LEFT_SEVEN_EIGHTHS_BLOCK: char = '▉';

    pub const RIGHT_ONE_EIGHTH_BLOCK: char = '▕';
    pub const RIGHT_ONE_QUARTER_BLOCK: char = '🮇';
    pub const RIGHT_THREE_EIGHTHS_BLOCK: char = '🮈';
    pub const RIGHT_FIVE_EIGHTHS_BLOCK: char = '🮉';
    pub const RIGHT_THREE_QUARTERS_BLOCK: char = '🮊';
    pub const RIGHT_SEVEN_EIGHTHS_BLOCK: char = '🮋';

    pub const LOWER_ONE_EIGHTH_BLOCK: char = '▁';
    pub const LOWER_ONE_QUARTER_BLOCK: char = '▂';
    pub const LOWER_THREE_EIGHTHS_BLOCK: char = '▃';
    pub const LOWER_FIVE_EIGHTHS_BLOCK: char = '▅';
    pub const LOWER_THREE_QUARTERS_BLOCK: char = '▆';
    pub const LOWER_SEVEN_EIGHTHS_BLOCK: char = '▇';

    pub const UPPER_ONE_EIGHTH_BLOCK: char = '▔';
    pub const UPPER_ONE_QUARTER_BLOCK: char = '🮂';
    pub const UPPER_THREE_EIGHTHS_BLOCK: char = '🮃';
    pub const UPPER_FIVE_EIGHTHS_BLOCK: char = '🮄';
    pub const UPPER_THREE_QUARTERS_BLOCK: char = '🮅';
    pub const UPPER_SEVEN_EIGHTHS_BLOCK: char = '🮆';

    pub const EIGHTH_BLOCKS_FROM_LEFT: &[char] = &[
        SPACE,
        LEFT_ONE_EIGHTH_BLOCK,
        LEFT_ONE_QUARTER_BLOCK,
        LEFT_THREE_EIGHTHS_BLOCK,
        LEFT_HALF_BLOCK,
        LEFT_FIVE_EIGHTHS_BLOCK,
        LEFT_THREE_QUARTERS_BLOCK,
        LEFT_SEVEN_EIGHTHS_BLOCK,
        FULL_BLOCK,
    ];
    pub const EIGHTH_BLOCKS_FROM_RIGHT: &[char] = &[
        SPACE,
        RIGHT_ONE_EIGHTH_BLOCK,
        RIGHT_ONE_QUARTER_BLOCK,
        RIGHT_THREE_EIGHTHS_BLOCK,
        RIGHT_HALF_BLOCK,
        RIGHT_FIVE_EIGHTHS_BLOCK,
        RIGHT_THREE_QUARTERS_BLOCK,
        RIGHT_SEVEN_EIGHTHS_BLOCK,
        FULL_BLOCK,
    ];

    pub const EIGHTH_BLOCKS_FROM_BOTTOM: &[char] = &[
        SPACE,
        LOWER_ONE_EIGHTH_BLOCK,
        LOWER_ONE_QUARTER_BLOCK,
        LOWER_THREE_EIGHTHS_BLOCK,
        LOWER_HALF_BLOCK,
        LOWER_FIVE_EIGHTHS_BLOCK,
        LOWER_THREE_QUARTERS_BLOCK,
        LOWER_SEVEN_EIGHTHS_BLOCK,
        FULL_BLOCK,
    ];
    pub const EIGHTH_BLOCKS_FROM_TOP: &[char] = &[
        SPACE,
        UPPER_ONE_EIGHTH_BLOCK,
        UPPER_ONE_QUARTER_BLOCK,
        UPPER_THREE_EIGHTHS_BLOCK,
        UPPER_HALF_BLOCK,
        UPPER_FIVE_EIGHTHS_BLOCK,
        UPPER_THREE_QUARTERS_BLOCK,
        UPPER_SEVEN_EIGHTHS_BLOCK,
        FULL_BLOCK,
    ];

    pub const SOLID_CHESS_PIECES: &[char] = &['♟', '♛', '♚', '♝', '♞', '♜'];

    pub const THICK_ARROWS: &str = "🢀🢂🢁🢃🢄🢅🢆🢇";
    pub const THIN_TRIANGLE_ARROWS: &str = "⭠⭢⭡⭣⭦⭧⭨⭩";

    // tiny squid: ᵜ
    // big squid: ᴥ (or a jelly thing from zelda)
    // pac-ish man: ᗢᗣᗤᗧ
    // bullets: ᗜ (plus rotations)

    // ●○ ⚬⦁⚫⚪ ✕ ✕
    // ✕ ⨉ ⨯ 🞨 🞮 ×

    // best x yet: ᳵ
    const EX: char = 'ᳵ';
    //const EX: char ='⨯' ;

    pub const MOVE_ONLY_SQUARE_CHARS: &[char; 2] = &['○', ' '];
    pub const CAPTURE_ONLY_SQUARE_CHARS: &[char; 2] = &[EX, ' '];
    pub const MOVE_AND_CAPTURE_SQUARE_CHARS: &[char; 2] = &['●', ' '];
    pub const CONDITIONAL_MOVE_AND_CAPTURE_SQUARE_CHARS: &[char; 2] = &['◌', ' '];
    // ▴▵
    pub const KING_PATH_GLYPHS: &[char; 2] = &['▵', ' '];
}
pub use named_chars::*;
