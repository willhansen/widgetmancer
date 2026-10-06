//! Screen-space overlay layer for UI content.
//!
//! A `UiLayer` is a character-resolution glyph grid in the same frame as
//! [`Screen`]'s buffer (origin top-left, y down), but with no camera attached:
//! drawing into it never consults the screen's `rotation`. It is meant for
//! things that belong to the terminal rather than the world — frames, HUD text,
//! panels — so they stay put when the view turns.
//!
//! Cells start fully transparent, so `composite_onto` blends the layer over
//! whatever the world pass produced: transparent cells leave the world alone,
//! opaque cells replace it.

use euclid::point2;
use rgb::RGB8;

use crate::glyph::{DoubleGlyph, Glyph};
use crate::screen::{Screen, ScreenBufferCharacterSquare, ScreenBufferSquare};

pub struct UiLayer {
    width: u16,
    height: u16,
    // [x][y], matching `Screen::screen_buffer`.
    cells: Vec<Vec<Glyph>>,
}

impl UiLayer {
    pub fn new(width: u16, height: u16) -> Self {
        UiLayer {
            width,
            height,
            cells: vec![vec![Glyph::transparent_glyph(); height as usize]; width as usize],
        }
    }

    pub fn width(&self) -> u16 {
        self.width
    }

    pub fn height(&self) -> u16 {
        self.height
    }

    pub fn clear(&mut self) {
        for column in self.cells.iter_mut() {
            for cell in column.iter_mut() {
                *cell = Glyph::transparent_glyph();
            }
        }
    }

    fn is_on_screen(&self, pos: ScreenBufferCharacterSquare) -> bool {
        pos.x >= 0 && pos.x < self.width as i32 && pos.y >= 0 && pos.y < self.height as i32
    }

    /// Write one terminal cell. Off-screen positions are dropped.
    pub fn draw_glyph(&mut self, pos: ScreenBufferCharacterSquare, glyph: Glyph) {
        if !self.is_on_screen(pos) {
            return;
        }
        self.cells[pos.x as usize][pos.y as usize] = glyph;
    }

    /// Write a world-square's worth of glyphs (two adjacent cells) at a screen
    /// square. Mirrors `Screen::draw_glyphs_straight_to_screen_square`, including
    /// clipping the off-screen half.
    pub fn draw_double_glyph(&mut self, square: ScreenBufferSquare, glyphs: DoubleGlyph) {
        let left = point2(square.x * 2, square.y);
        self.draw_glyph(left, glyphs[0]);
        self.draw_glyph(left + euclid::vec2(1, 0), glyphs[1]);
    }

    /// Write `text` left-to-right starting at `pos`, one glyph per cell.
    pub fn draw_string(&mut self, pos: ScreenBufferCharacterSquare, text: &str, fg: RGB8, bg: RGB8) {
        for (i, character) in text.chars().enumerate() {
            self.draw_glyph(
                pos + euclid::vec2(i as i32, 0),
                Glyph::new(character, fg, bg),
            );
        }
    }

    /// Read one cell. Used by tests and by callers composing on top of the layer.
    pub fn glyph_at(&self, pos: ScreenBufferCharacterSquare) -> Glyph {
        if !self.is_on_screen(pos) {
            return Glyph::transparent_glyph();
        }
        self.cells[pos.x as usize][pos.y as usize]
    }

    /// Blend this layer over the screen buffer, cell by cell. Transparent cells
    /// (`Glyph::transparent_glyph`) leave the screen untouched.
    pub fn composite_onto(&self, screen: &mut Screen) {
        for x in 0..self.width as usize {
            for y in 0..self.height as usize {
                let top = self.cells[x][y];
                // Skip untouched cells entirely: `drawn_over` would otherwise
                // rebuild a bg-only cell through `solid_bg`, changing its fg.
                if top.is_fully_transparent() {
                    continue;
                }
                let bottom = screen.screen_buffer[x][y];
                screen.screen_buffer[x][y] = top.drawn_over(bottom);
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::glyph::glyph_constants::named_colors::{BLACK, DODGER_BLUE, RED, WHITE};
    use crate::glyph::glyph_constants::UPPER_HALF_BLOCK;

    #[test]
    fn transparent_layer_leaves_the_screen_untouched() {
        let mut screen = Screen::new(6, 3);
        let world_glyph = Glyph::new('a', WHITE, RED);
        screen.screen_buffer[2][1] = world_glyph;

        let ui = UiLayer::new(6, 3);
        ui.composite_onto(&mut screen);

        assert_eq!(screen.screen_buffer[2][1], world_glyph);
    }

    #[test]
    fn transparent_layer_does_not_rewrite_bg_only_cells() {
        // A space cell with a non-default fg (e.g. a shaded void cell) must be
        // preserved exactly, fg included.
        let mut screen = Screen::new(4, 2);
        let void = Glyph::new(' ', RGB8::new(17, 34, 51), BLACK);
        screen.screen_buffer[1][1] = void;

        let ui = UiLayer::new(4, 2);
        ui.composite_onto(&mut screen);

        assert_eq!(screen.screen_buffer[1][1], void);
    }

    #[test]
    fn opaque_layer_overwrites_the_screen() {
        let mut screen = Screen::new(6, 3);
        screen.screen_buffer[2][1] = Glyph::new('a', WHITE, RED);

        let mut ui = UiLayer::new(6, 3);
        let glyph = Glyph::new(UPPER_HALF_BLOCK, DODGER_BLUE, BLACK);
        ui.draw_glyph(point2(2, 1), glyph);
        ui.composite_onto(&mut screen);

        assert_eq!(screen.screen_buffer[2][1], glyph);
    }

    #[test]
    fn draw_double_glyph_spans_two_cells() {
        let mut ui = UiLayer::new(6, 3);
        let glyphs = [Glyph::new('L', WHITE, BLACK), Glyph::new('R', WHITE, BLACK)];
        ui.draw_double_glyph(point2(1, 1), glyphs);

        assert_eq!(ui.glyph_at(point2(2, 1)).character, 'L');
        assert_eq!(ui.glyph_at(point2(3, 1)).character, 'R');
    }

    #[test]
    fn drawing_clips_off_screen() {
        let mut ui = UiLayer::new(2, 2);
        ui.draw_glyph(point2(-1, 0), Glyph::from_char('x'));
        ui.draw_glyph(point2(0, 5), Glyph::from_char('x'));
        // On an odd-width screen the last square's right cell falls off the
        // edge; keep the visible left half.
        let mut odd = UiLayer::new(5, 2);
        odd.draw_double_glyph(point2(2, 0), [Glyph::from_char('L'), Glyph::from_char('R')]);

        assert_eq!(odd.glyph_at(point2(4, 0)).character, 'L');
    }

    #[test]
    fn clear_restores_transparency() {
        let mut ui = UiLayer::new(2, 2);
        ui.draw_glyph(point2(0, 0), Glyph::new('x', WHITE, BLACK));
        ui.clear();

        let cell = ui.glyph_at(point2(0, 0));
        assert!(cell.is_fully_transparent());
    }

    #[test]
    fn draw_string_fills_cells_left_to_right() {
        let mut ui = UiLayer::new(6, 2);
        ui.draw_string(point2(1, 0), "hi", WHITE, BLACK);

        assert_eq!(ui.glyph_at(point2(1, 0)).character, 'h');
        assert_eq!(ui.glyph_at(point2(2, 0)).character, 'i');
    }
}
