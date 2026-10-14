//! Shared terminal-UI widgets used by the in-game report overlay (`report.rs`)
//! and the standalone issue review tool (`bin/issue_review.rs`).
//!
//! These are deliberately free of any `Game`/gameplay coupling so the review
//! tool can reuse them without pulling in the `report` feature or the game
//! loop. The overlay keeps its own layout, tint constants and help line; only
//! the generic pieces live here.

use std::collections::HashMap;

use rgb::RGB8;
use terminal_rendering::glyph::Glyph;
use terminal_rendering::*;

/// One selected region: a click (1x1) or a mouse drag. Only the bounds are
/// kept — the per-square detail is cheap to re-derive when the issue is
/// investigated, so it is not duplicated into the filed issue.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Region {
    pub world_min: WorldSquare,
    pub world_max: WorldSquare,
    pub screen_min: ScreenBufferSquare,
    pub screen_max: ScreenBufferSquare,
}

impl Region {
    pub fn contains(&self, square: WorldSquare) -> bool {
        square.x >= self.world_min.x
            && square.x <= self.world_max.x
            && square.y >= self.world_min.y
            && square.y <= self.world_max.y
    }
}

/// A rectangle in terminal character cells (origin top-left, 0-indexed).
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Rect {
    pub x: u16,
    pub y: u16,
    pub width: u16,
    pub height: u16,
}

impl Rect {
    pub fn inner(&self) -> Rect {
        Rect {
            x: self.x + 1,
            y: self.y + 1,
            width: self.width.saturating_sub(2).max(1),
            height: self.height.saturating_sub(2).max(1),
        }
    }

    pub fn contains(&self, x: u16, y: u16) -> bool {
        x >= self.x && x < self.x + self.width && y >= self.y && y < self.y + self.height
    }
}

/// A small multi-line text buffer (the built-in editor).
#[derive(Debug)]
pub struct TextBuffer {
    lines: Vec<Vec<char>>,
    row: usize,
    col: usize,
}

impl Default for TextBuffer {
    fn default() -> Self {
        Self::new()
    }
}

impl TextBuffer {
    pub fn new() -> Self {
        TextBuffer {
            lines: vec![Vec::new()],
            row: 0,
            col: 0,
        }
    }

    pub fn insert_char(&mut self, character: char) {
        self.lines[self.row].insert(self.col, character);
        self.col += 1;
    }

    pub fn insert_newline(&mut self) {
        let rest = self.lines[self.row].split_off(self.col);
        self.lines.insert(self.row + 1, rest);
        self.row += 1;
        self.col = 0;
    }

    pub fn backspace(&mut self) {
        if self.col > 0 {
            self.lines[self.row].remove(self.col - 1);
            self.col -= 1;
        } else if self.row > 0 {
            let current = self.lines.remove(self.row);
            self.row -= 1;
            self.col = self.lines[self.row].len();
            self.lines[self.row].extend(current);
        }
    }

    pub fn delete(&mut self) {
        if self.col < self.lines[self.row].len() {
            self.lines[self.row].remove(self.col);
        } else if self.row + 1 < self.lines.len() {
            let next = self.lines.remove(self.row + 1);
            self.lines[self.row].extend(next);
        }
    }

    pub fn move_left(&mut self) {
        if self.col > 0 {
            self.col -= 1;
        } else if self.row > 0 {
            self.row -= 1;
            self.col = self.lines[self.row].len();
        }
    }

    pub fn move_right(&mut self) {
        if self.col < self.lines[self.row].len() {
            self.col += 1;
        } else if self.row + 1 < self.lines.len() {
            self.row += 1;
            self.col = 0;
        }
    }

    pub fn move_up(&mut self) {
        if self.row > 0 {
            self.row -= 1;
            self.col = self.col.min(self.lines[self.row].len());
        }
    }

    pub fn move_down(&mut self) {
        if self.row + 1 < self.lines.len() {
            self.row += 1;
            self.col = self.col.min(self.lines[self.row].len());
        }
    }

    pub fn home(&mut self) {
        self.col = 0;
    }

    pub fn end(&mut self) {
        self.col = self.lines[self.row].len();
    }

    pub fn text(&self) -> String {
        self.lines
            .iter()
            .map(|line| line.iter().collect::<String>())
            .collect::<Vec<_>>()
            .join("\n")
    }
}

/// A blank cell for parts of a region a translated frame does not cover.
pub fn blank_glyph() -> Glyph {
    Glyph::new(' ', Glyph::DEFAULT_FG_COLOR, Glyph::DEFAULT_BG_COLOR)
}

/// Blend `color` into a glyph's foreground and background. The character is
/// untouched, so a mark tints the underlying glyph instead of hiding it.
pub fn tint_glyph(glyph: Glyph, color: RGB8, strength: f32) -> Glyph {
    Glyph {
        character: glyph.character,
        fg_color: tint_color(glyph.fg_color, color, strength),
        bg_color: tint_color(glyph.bg_color, color, strength),
        bg_transparent: false,
    }
}

/// The glyph at a screen character cell, if in bounds (coordinates may be
/// negative, e.g. when a translated frame hangs off an edge).
pub fn glyph_at(buffer: &[Vec<Glyph>], cx: i32, cy: i32) -> Option<Glyph> {
    if cx < 0 || cy < 0 {
        return None;
    }
    buffer
        .get(cx as usize)
        .and_then(|col| col.get(cy as usize))
        .copied()
}

/// Every screen square in the inclusive world-square box `a`..`b`.
pub fn world_box_screen_squares(
    screen: &terminal_rendering::Screen,
    a: WorldSquare,
    b: WorldSquare,
) -> Vec<ScreenBufferSquare> {
    let (x0, x1) = (a.x.min(b.x), a.x.max(b.x));
    let (y0, y1) = (a.y.min(b.y), a.y.max(b.y));
    let mut squares = Vec::new();
    for y in y0..=y1 {
        for x in x0..=x1 {
            squares.push(screen.world_square_to_screen_buffer_square(WorldSquare::new(x, y)));
        }
    }
    squares
}

/// Editor pane colors, so the overlay and the review tool can theme it.
#[derive(Clone, Copy)]
pub struct EditorColors {
    pub fg: RGB8,
    pub bg: RGB8,
    pub border: RGB8,
    pub cursor_bg: RGB8,
}

/// Draw the built-in text editor into `rect`: border, title, scrolled lines
/// and an inverted cursor cell. `title` is shown on the top border.
pub fn draw_editor_pane(
    cells: &mut HashMap<(u16, u16), Glyph>,
    rect: Rect,
    buffer: &TextBuffer,
    title: &str,
    colors: EditorColors,
) {
    if rect.width < 4 || rect.height < 3 {
        return;
    }
    let inner = rect.inner();

    for row in 0..rect.height {
        for col in 0..rect.width {
            let (cx, cy) = (rect.x + col, rect.y + row);
            let character = if row == 0 || row == rect.height - 1 {
                if col == 0 || col == rect.width - 1 {
                    '+'
                } else {
                    '-'
                }
            } else if col == 0 || col == rect.width - 1 {
                '|'
            } else {
                ' '
            };
            let fg = if character == ' ' { colors.fg } else { colors.border };
            cells.insert((cx, cy), Glyph::new(character, fg, colors.bg));
        }
    }

    for (i, character) in title.chars().enumerate() {
        let cx = rect.x + 1 + i as u16;
        if cx >= rect.x + rect.width - 1 {
            break;
        }
        cells.insert((cx, rect.y), Glyph::new(character, colors.border, colors.bg));
    }

    let inner_height = inner.height as usize;
    let scroll = if buffer.row < inner_height {
        0
    } else {
        buffer.row + 1 - inner_height
    };

    for (index, line) in buffer
        .lines
        .iter()
        .enumerate()
        .skip(scroll)
        .take(inner_height)
    {
        let row = inner.y + (index - scroll) as u16;
        for (col, &character) in line.iter().enumerate().take(inner.width as usize) {
            let cx = inner.x + col as u16;
            cells.insert((cx, row), Glyph::new(character, colors.fg, colors.bg));
        }
    }

    let cursor_row = inner.y + (buffer.row - scroll) as u16;
    let cursor_col = inner.x + buffer.col.min(inner.width as usize - 1) as u16;
    let character = buffer
        .lines
        .get(buffer.row)
        .and_then(|line| line.get(buffer.col))
        .copied()
        .unwrap_or(' ');
    cells.insert(
        (cursor_col, cursor_row),
        Glyph::new(character, colors.bg, colors.cursor_bg),
    );
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn text_buffer_edits() {
        let mut buffer = TextBuffer::new();
        for character in "hello".chars() {
            buffer.insert_char(character);
        }
        assert_eq!(buffer.text(), "hello");
        buffer.insert_newline();
        buffer.insert_char('x');
        assert_eq!(buffer.text(), "hello\nx");
        buffer.backspace();
        assert_eq!(buffer.text(), "hello\n");
        buffer.backspace();
        assert_eq!(buffer.text(), "hello");
        buffer.home();
        buffer.delete();
        assert_eq!(buffer.text(), "ello");
    }

    #[test]
    fn rect_inner_and_contains() {
        let rect = Rect {
            x: 2,
            y: 3,
            width: 10,
            height: 5,
        };
        assert_eq!(rect.inner().x, 3);
        assert_eq!(rect.inner().width, 8);
        assert!(rect.contains(2, 3));
        assert!(!rect.contains(12, 3));
        assert!(!rect.contains(2, 8));
    }

    #[test]
    fn region_contains_its_box() {
        let region = Region {
            world_min: WorldSquare::new(1, 2),
            world_max: WorldSquare::new(3, 4),
            screen_min: ScreenBufferSquare::new(0, 0),
            screen_max: ScreenBufferSquare::new(1, 1),
        };
        assert!(region.contains(WorldSquare::new(2, 3)));
        assert!(!region.contains(WorldSquare::new(4, 3)));
    }
}
