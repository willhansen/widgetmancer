use rgb::*;

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
pub const EXPLOSION_COLOR: RGB8 = BLACK; //RGB8::new(200, 200, 255);
pub const SELECTOR_COLOR: RGB8 = RGB8::new(255, 64, 0);
pub const ENEMY_PIECE_COLOR: RGB8 = WHITE;
pub const GREY_RED: RGB8 = RGB8::new(127, 100, 100);
pub const DANGER_SQUARE_COLOR: RGB8 = GREY_RED;
pub const PATH_COLOR: RGB8 = MAGENTA;
pub const OUT_OF_SIGHT_COLOR: RGB8 = BLACK; //DARK_GREY;
pub const RED_PAWN_COLOR: RGB8 = RED;
pub const BLINK_EFFECT_COLOR: RGB8 = COBALT_BLUE;
pub const SPEAR_COLOR: RGB8 = MAROON;

pub const BLOCK_BG: RGB8 = BLACK;
pub const BLOCK_FG: RGB8 = BLACK;

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

pub const EIGHTH_BLOCKS_FROM_LEFT: &[char] = &[
    SPACE,
    '▏',
    '▎',
    '▍',
    LEFT_HALF_BLOCK,
    '▋',
    '▊',
    '▉',
    FULL_BLOCK,
];
pub const EIGHTH_BLOCKS_FROM_RIGHT: &[char] = &[
    SPACE,
    '▕',
    '🮇',
    '🮈',
    RIGHT_HALF_BLOCK,
    '🮉',
    '🮊',
    '🮋',
    FULL_BLOCK,
];

pub const EIGHTH_BLOCKS_FROM_BOTTOM: &[char] = &[
    SPACE,
    '▁',
    '▂',
    '▃',
    LOWER_HALF_BLOCK,
    '▅',
    '▆',
    '▇',
    FULL_BLOCK,
];
pub const EIGHTH_BLOCKS_FROM_TOP: &[char] = &[
    SPACE,
    '▔',
    '🮂',
    '🮃',
    UPPER_HALF_BLOCK,
    '🮄',
    '🮅',
    '🮆',
    FULL_BLOCK,
];

pub const SOLID_CHESS_PIECES: &[char] = &['♟', '♛', '♚', '♝', '♞', '♜'];

pub const THICK_ARROWS: &[char] = &['🢀', '🢂', '🢁', '🢃', '🢄', '🢅', '🢆', '🢇'];

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
