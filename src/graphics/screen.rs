use crate::glyph::Glyph;
use crate::utility::coordinate_frame_conversions::WorldCharacterSquare;

pub struct Screen {
    pub screen_buffer_origin: WorldCharacterSquare,
    pub screen_buffer: Vec<Vec<Glyph>>,
    // (x,y), left to right, top to bottom
    pub current_screen_state: Vec<Vec<Glyph>>,
    // (x,y), left to right, top to bottom
    pub terminal_width: u16,
    pub terminal_height: u16,
}
