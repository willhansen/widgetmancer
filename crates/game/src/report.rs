//! In-game issue-report overlay (issue 0021).
//!
//! `Ctrl+P` freezes the game and captures the last drawn frame as a static
//! image. While the overlay is open that image is blitted straight to the
//! terminal, repositioned so the player lands in the middle of the game region
//! (the editor takes the rest), and the reposition is recorded so mouse input
//! still maps back to the real frame cells / world squares. The live game is
//! never re-rendered while the overlay is open, so `snapshot/screen.txt` stays
//! the un-altered render and `verify-issues` is unaffected.
//!
//! The reporter flags one *region* per click/drag (a click is a 1x1 region);
//! each region is tinted magenta, numbered, and filed by its world/screen
//! bounds only. The per-square `explain` detail is cheap to re-derive during
//! investigation, so it is not duplicated into the issue. Drawing diffs the
//! desired overlay against the last one, so typing touches only a few cells —
//! no full-pane rewrite, no flicker.
//!
//! Gated behind the default-on `report` feature (which implies `debug-tools`
//! for `Game::explain_screen_cell`).

use std::collections::HashMap;
use std::io::Write;

use rgb::RGB8;
use termion::cursor::Goto;
use termion::event::{Event, Key, MouseButton, MouseEvent};

use terminal_rendering::glyph::Glyph;
use terminal_rendering::*;

use crate::game::snapshot::{create_issue_dir, write_snapshot_to, InputRecord};
use crate::game::Game;

/// Magenta tint blended into a flagged region, so the glyph shows through.
const FLAG_TINT: RGB8 = RGB8::new(255, 0, 255);
const FLAG_TINT_STRENGTH: f32 = 0.45;
/// Cyan tint for the live drag preview.
const DRAG_TINT: RGB8 = RGB8::new(0, 255, 255);
const DRAG_TINT_STRENGTH: f32 = 0.45;
/// Yellow tint for the keyboard marking cursor.
const CURSOR_TINT: RGB8 = RGB8::new(255, 255, 0);
const CURSOR_TINT_STRENGTH: f32 = 0.5;
/// Colors of the numbered region labels.
const LABEL_FG: RGB8 = RGB8::new(255, 255, 255);
const LABEL_BG: RGB8 = RGB8::new(160, 0, 160);
/// Background of the help/status line.
const HELP_BG: RGB8 = RGB8::new(20, 20, 40);
/// Foreground of the help/status line.
const HELP_FG: RGB8 = RGB8::new(220, 220, 235);
/// Editor pane colors.
const EDITOR_BG: RGB8 = RGB8::new(10, 12, 22);
const EDITOR_FG: RGB8 = RGB8::new(214, 218, 232);
const EDITOR_BORDER: RGB8 = RGB8::new(80, 80, 120);
const EDITOR_CURSOR_BG: RGB8 = RGB8::new(150, 150, 60);

/// One selected region: a click (1x1) or a mouse drag. Only the bounds are
/// kept — the per-square `explain` detail is cheap to re-derive when the issue
/// is investigated, so it is not duplicated into the filed issue.
#[derive(Clone, Debug)]
pub struct Region {
    pub world_min: WorldSquare,
    pub world_max: WorldSquare,
    pub screen_min: ScreenBufferSquare,
    pub screen_max: ScreenBufferSquare,
}

impl Region {
    fn contains(&self, square: WorldSquare) -> bool {
        square.x >= self.world_min.x
            && square.x <= self.world_max.x
            && square.y >= self.world_min.y
            && square.y <= self.world_max.y
    }
}

/// Which pane owns keyboard/mouse input.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum Focus {
    Marking,
    Editor,
}

/// What the driver should do after handling an event.
pub enum ReportAction {
    None,
    Cancel,
    File,
}

/// A rectangle in terminal character cells (origin top-left, 0-indexed).
#[derive(Clone, Copy, Debug)]
struct Rect {
    x: u16,
    y: u16,
    width: u16,
    height: u16,
}

impl Rect {
    fn inner(&self) -> Rect {
        Rect {
            x: self.x + 1,
            y: self.y + 1,
            width: self.width.saturating_sub(2).max(1),
            height: self.height.saturating_sub(2).max(1),
        }
    }

    fn contains(&self, x: u16, y: u16) -> bool {
        x >= self.x && x < self.x + self.width && y >= self.y && y < self.y + self.height
    }
}

/// A small multi-line text buffer for the description.
#[derive(Default, Debug)]
struct TextBuffer {
    lines: Vec<Vec<char>>,
    row: usize,
    col: usize,
}

impl TextBuffer {
    fn new() -> Self {
        TextBuffer {
            lines: vec![Vec::new()],
            row: 0,
            col: 0,
        }
    }

    fn insert_char(&mut self, character: char) {
        self.lines[self.row].insert(self.col, character);
        self.col += 1;
    }

    fn insert_newline(&mut self) {
        let rest = self.lines[self.row].split_off(self.col);
        self.lines.insert(self.row + 1, rest);
        self.row += 1;
        self.col = 0;
    }

    fn backspace(&mut self) {
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

    fn delete(&mut self) {
        if self.col < self.lines[self.row].len() {
            self.lines[self.row].remove(self.col);
        } else if self.row + 1 < self.lines.len() {
            let next = self.lines.remove(self.row + 1);
            self.lines[self.row].extend(next);
        }
    }

    fn move_left(&mut self) {
        if self.col > 0 {
            self.col -= 1;
        } else if self.row > 0 {
            self.row -= 1;
            self.col = self.lines[self.row].len();
        }
    }

    fn move_right(&mut self) {
        if self.col < self.lines[self.row].len() {
            self.col += 1;
        } else if self.row + 1 < self.lines.len() {
            self.row += 1;
            self.col = 0;
        }
    }

    fn move_up(&mut self) {
        if self.row > 0 {
            self.row -= 1;
            self.col = self.col.min(self.lines[self.row].len());
        }
    }

    fn move_down(&mut self) {
        if self.row + 1 < self.lines.len() {
            self.row += 1;
            self.col = self.col.min(self.lines[self.row].len());
        }
    }

    fn home(&mut self) {
        self.col = 0;
    }

    fn end(&mut self) {
        self.col = self.lines[self.row].len();
    }

    fn text(&self) -> String {
        self.lines
            .iter()
            .map(|line| line.iter().collect::<String>())
            .collect::<Vec<_>>()
            .join("\n")
    }
}

/// A live report overlay. Owns the flagged regions, the description buffer,
/// the frozen frame, and the terminal-direct drawing state.
pub struct ReportSession {
    regions: Vec<Region>,
    cursor: ScreenBufferSquare,
    focus: Focus,
    /// Mouse drag anchor/current (world square) while the left button is held.
    drag_anchor: Option<WorldSquare>,
    drag_current: Option<WorldSquare>,
    /// The frozen frame captured on open (`screen_buffer` clone, indexed `[x][y]`).
    frame: Vec<Vec<Glyph>>,
    /// Where the frozen frame is repositioned, in terminal character cells.
    offset_x: i32,
    offset_y: i32,
    terminal_width: u16,
    terminal_height: u16,
    editor_rect: Rect,
    buffer: TextBuffer,
    /// Overlay cells from the last draw, diffed against the next so an idle
    /// overlay emits nothing and a keystroke touches only a few cells.
    last: HashMap<(u16, u16), Glyph>,
    dirty: bool,
}

impl ReportSession {
    /// Open over `game`'s current (frozen) frame.
    pub fn open(game: &Game) -> Self {
        let screen = &game.graphics().screen;
        let terminal_width = screen.terminal_width;
        let terminal_height = screen.terminal_height;
        let editor_rect = editor_rect(terminal_width, terminal_height);
        let (offset_x, offset_y) = frame_offset(&editor_rect);
        let cursor = screen.world_square_to_screen_buffer_square(game.player_square());
        ReportSession {
            regions: Vec::new(),
            cursor,
            focus: Focus::Marking,
            drag_anchor: None,
            drag_current: None,
            frame: screen.screen_buffer.clone(),
            offset_x,
            offset_y,
            terminal_width,
            terminal_height,
            editor_rect,
            buffer: TextBuffer::new(),
            last: HashMap::new(),
            dirty: true,
        }
    }

    pub fn regions(&self) -> &[Region] {
        &self.regions
    }

    pub fn focus(&self) -> Focus {
        self.focus
    }

    /// The description the reporter typed.
    pub fn description(&self) -> String {
        self.buffer.text()
    }

    /// Handle one terminal event. `game` is frozen while the session is open,
    /// so it is only read (for summaries and screen mapping).
    pub fn handle_event(&mut self, game: &Game, event: Event) -> ReportAction {
        self.dirty = true;
        // Global: `Ctrl+P` always cancels; `Tab` always swaps focus.
        if let Event::Key(Key::Ctrl('p')) = event {
            return ReportAction::Cancel;
        }
        if let Event::Key(Key::Char('\t')) = event {
            self.focus = match self.focus {
                Focus::Marking => Focus::Editor,
                Focus::Editor => Focus::Marking,
            };
            return ReportAction::None;
        }
        match self.focus {
            Focus::Marking => self.handle_marking(game, event),
            Focus::Editor => self.handle_editor(event),
        }
    }

    fn handle_editor(&mut self, event: Event) -> ReportAction {
        match event {
            // Editing is a mode; `Esc` leaves it (like vim's insert mode)
            // instead of cancelling the whole issue.
            Event::Key(Key::Esc) => {
                self.focus = Focus::Marking;
                return ReportAction::None;
            }
            Event::Key(Key::Char('\n')) | Event::Key(Key::Char('\r')) => {
                self.buffer.insert_newline()
            }
            Event::Key(Key::Char(c)) if !c.is_control() => self.buffer.insert_char(c),
            Event::Key(Key::Backspace) => self.buffer.backspace(),
            Event::Key(Key::Delete) => self.buffer.delete(),
            Event::Key(Key::Left) => self.buffer.move_left(),
            Event::Key(Key::Right) => self.buffer.move_right(),
            Event::Key(Key::Up) => self.buffer.move_up(),
            Event::Key(Key::Down) => self.buffer.move_down(),
            Event::Key(Key::Home) => self.buffer.home(),
            Event::Key(Key::End) => self.buffer.end(),
            _ => {}
        }
        ReportAction::None
    }

    fn handle_marking(&mut self, game: &Game, event: Event) -> ReportAction {
        match event {
            // `Esc` never cancels: a vim user hits it out of habit. The
            // documented cancel is the same key that opened the overlay, `Ctrl+P`.
            Event::Key(Key::Esc) => ReportAction::None,
            Event::Key(key) => self.handle_marking_key(game, key),
            Event::Mouse(me) => self.handle_marking_mouse(game, me),
            _ => ReportAction::None,
        }
    }

    fn handle_marking_key(&mut self, game: &Game, key: Key) -> ReportAction {
        match key {
            Key::Up | Key::Char('k') => self.move_cursor(0, -1),
            Key::Down | Key::Char('j') => self.move_cursor(0, 1),
            Key::Left | Key::Char('h') => self.move_cursor(-1, 0),
            Key::Right | Key::Char('l') => self.move_cursor(1, 0),
            Key::Char(' ') => self.toggle_flag_at_cursor(game),
            Key::Char('c') => self.regions.clear(),
            Key::Char('\n') | Key::Char('\r') => return ReportAction::File,
            _ => {}
        }
        ReportAction::None
    }

    fn handle_marking_mouse(&mut self, game: &Game, me: MouseEvent) -> ReportAction {
        match me {
            MouseEvent::Press(MouseButton::Left, term_x, term_y) => {
                // A click inside the editor focuses it instead of marking.
                if !self.term_in_game_region(term_x, term_y) {
                    self.focus = Focus::Editor;
                    return ReportAction::None;
                }
                let world = self.term_to_world(game, term_x, term_y);
                self.cursor = self.term_to_screen_square(term_x, term_y);
                self.drag_anchor = Some(world);
                self.drag_current = Some(world);
            }
            MouseEvent::Hold(term_x, term_y) => {
                if self.drag_anchor.is_none() {
                    return ReportAction::None;
                }
                self.drag_current = Some(self.term_to_world(game, term_x, term_y));
            }
            MouseEvent::Release(term_x, term_y) => {
                let end = self.term_to_world(game, term_x, term_y);
                if let Some(start) = self.drag_anchor.take() {
                    if start == end {
                        self.toggle_square(game, start);
                    } else {
                        self.add_region(game, start, end);
                    }
                }
                self.drag_current = None;
            }
            _ => {}
        }
        ReportAction::None
    }

    fn move_cursor(&mut self, dx: i32, dy: i32) {
        let max_x = (self.terminal_width as i32 / 2 - 1).max(0);
        let max_y = self.terminal_height as i32 - 1;
        self.cursor = ScreenBufferSquare::new(
            (self.cursor.x + dx).clamp(0, max_x),
            (self.cursor.y + dy).clamp(0, max_y),
        );
    }

    /// A terminal character cell (0-indexed) from a frame screen square.
    fn screen_square_to_term(&self, square: ScreenBufferSquare) -> (i32, i32) {
        (square.x * 2 + self.offset_x, square.y + self.offset_y)
    }

    /// The frame screen square under a terminal cell, clamped to the frame.
    fn term_to_screen_square(&self, term_x: u16, term_y: u16) -> ScreenBufferSquare {
        let frame_x = term_x.saturating_sub(1) as i32 - self.offset_x;
        let frame_y = term_y.saturating_sub(1) as i32 - self.offset_y;
        let max_x = (self.terminal_width as i32 / 2 - 1).max(0);
        let max_y = (self.terminal_height as i32 - 1).max(0);
        ScreenBufferSquare::new(
            frame_x.div_euclid(2).clamp(0, max_x),
            frame_y.clamp(0, max_y),
        )
    }

    fn term_to_world(&self, game: &Game, term_x: u16, term_y: u16) -> WorldSquare {
        game.graphics()
            .screen
            .screen_buffer_square_to_world_square(self.term_to_screen_square(term_x, term_y))
    }

    fn term_in_game_region(&self, term_x: u16, term_y: u16) -> bool {
        !self
            .editor_rect
            .contains(term_x.saturating_sub(1), term_y.saturating_sub(1))
    }

    fn toggle_flag_at_cursor(&mut self, game: &Game) {
        let square = game
            .graphics()
            .screen
            .screen_buffer_square_to_world_square(self.cursor);
        self.toggle_square(game, square);
    }

    /// A click toggles the region containing `world_square`: remove it if it is
    /// already marked, otherwise add a 1x1 region.
    fn toggle_square(&mut self, game: &Game, world_square: WorldSquare) {
        if let Some(index) = self.regions.iter().position(|region| region.contains(world_square)) {
            self.regions.remove(index);
            return;
        }
        self.add_region(game, world_square, world_square);
    }

    /// Add one region spanning the inclusive world-square box `a`..`b`. Only
    /// the world/screen bounds are recorded (issue 0021, revised: per-square
    /// `explain` detail is not duplicated into the issue).
    fn add_region(&mut self, game: &Game, a: WorldSquare, b: WorldSquare) {
        let world_min = WorldSquare::new(a.x.min(b.x), a.y.min(b.y));
        let world_max = WorldSquare::new(a.x.max(b.x), a.y.max(b.y));
        let screen = &game.graphics().screen;
        let corner_a = screen.world_square_to_screen_buffer_square(world_min);
        let corner_b = screen.world_square_to_screen_buffer_square(world_max);
        let screen_min = ScreenBufferSquare::new(
            corner_a.x.min(corner_b.x),
            corner_a.y.min(corner_b.y),
        );
        let screen_max = ScreenBufferSquare::new(
            corner_a.x.max(corner_b.x),
            corner_a.y.max(corner_b.y),
        );
        self.regions.push(Region {
            world_min,
            world_max,
            screen_min,
            screen_max,
        });
    }

    /// The bottom row and width of the game region (where the help line goes).
    fn help_row_and_width(&self) -> (u16, u16) {
        if self.editor_rect.x > 0 {
            (self.terminal_height.saturating_sub(1), self.editor_rect.x)
        } else {
            (self.editor_rect.y.saturating_sub(1), self.terminal_width)
        }
    }

    /// Draw the overlay straight to the terminal. No-op when nothing changed.
    pub fn draw(&mut self, game: &Game, writer: &mut dyn Write) {
        if !self.dirty {
            return;
        }
        self.dirty = false;

        // The full desired frame (background + overlay). Because it covers every
        // terminal cell, cells the previous draw touched but the new one does
        // not are restored simply by diffing against `last`.
        let desired = self.build_overlay(game);

        for (&(cx, cy), &glyph) in desired.iter() {
            if self.last.get(&(cx, cy)) != Some(&glyph) {
                let _ = write!(writer, "{}{}", Goto(cx + 1, cy + 1), glyph);
            }
        }

        self.last = desired;
        let _ = writer.flush();
    }

    /// Build every terminal cell for the current overlay state.
    fn build_overlay(&self, game: &Game) -> HashMap<(u16, u16), Glyph> {
        let mut cells: HashMap<(u16, u16), Glyph> = HashMap::new();

        // Frozen frame, repositioned and clipped into the game region.
        for cy in 0..self.terminal_height {
            for cx in 0..self.terminal_width {
                if self.editor_rect.contains(cx, cy) {
                    continue;
                }
                cells.insert(
                    (cx, cy),
                    self.frame_glyph_at(cx, cy).unwrap_or_else(blank_glyph),
                );
            }
        }

        // Committed regions, tinted so the glyph shows through.
        for region in &self.regions {
            for sy in region.screen_min.y..=region.screen_max.y {
                for sx in region.screen_min.x..=region.screen_max.x {
                    self.tint_square(
                        &mut cells,
                        ScreenBufferSquare::new(sx, sy),
                        FLAG_TINT,
                        FLAG_TINT_STRENGTH,
                    );
                }
            }
        }

        // Live drag: tentatively tint the box and label it as the next region.
        if let (Some(anchor), Some(current)) = (self.drag_anchor, self.drag_current) {
            let screen = &game.graphics().screen;
            for screen_square in world_box_screen_squares(screen, anchor, current) {
                self.tint_square(
                    &mut cells,
                    screen_square,
                    DRAG_TINT,
                    DRAG_TINT_STRENGTH,
                );
            }
        }

        // Keyboard cursor (marking focus only).
        if self.focus == Focus::Marking {
            self.tint_square(&mut cells, self.cursor, CURSOR_TINT, CURSOR_TINT_STRENGTH);
        }

        // Numbered labels, one per region; the drag preview is numbered next.
        for (index, region) in self.regions.iter().enumerate() {
            self.draw_label(&mut cells, region.screen_min, index + 1);
        }
        if let (Some(anchor), Some(current)) = (self.drag_anchor, self.drag_current) {
            if anchor != current {
                let screen = &game.graphics().screen;
                let a = screen.world_square_to_screen_buffer_square(anchor);
                let b = screen.world_square_to_screen_buffer_square(current);
                let top_left = ScreenBufferSquare::new(a.x.min(b.x), a.y.min(b.y));
                self.draw_label(&mut cells, top_left, self.regions.len() + 1);
            }
        }

        self.help_cells(&mut cells);
        // Editor last, so it wins over anything that strayed into it.
        self.editor_cells(&mut cells);

        cells
    }

    /// The frozen-frame glyph at a terminal cell, if the frame covers it.
    fn frame_glyph_at(&self, cx: u16, cy: u16) -> Option<Glyph> {
        glyph_at(&self.frame, cx as i32 - self.offset_x, cy as i32 - self.offset_y)
    }

    /// Blend `color` into a square's two character cells, if present.
    fn tint_square(
        &self,
        cells: &mut HashMap<(u16, u16), Glyph>,
        square: ScreenBufferSquare,
        color: RGB8,
        strength: f32,
    ) {
        let (tx, ty) = self.screen_square_to_term(square);
        for cx in [tx, tx + 1] {
            if cx < 0 || ty < 0 {
                continue;
            }
            if let Some(glyph) = cells.get_mut(&(cx as u16, ty as u16)) {
                *glyph = tint_glyph(*glyph, color, strength);
            }
        }
    }

    /// Draw a region's 1-based number one row above its top-left corner,
    /// clamped to the top edge and clipped out of the editor.
    fn draw_label(
        &self,
        cells: &mut HashMap<(u16, u16), Glyph>,
        top_left: ScreenBufferSquare,
        index: usize,
    ) {
        let (tx, ty) = self.screen_square_to_term(top_left);
        let row = (ty - 1).max(0) as u16;
        let mut x = tx;
        for character in index.to_string().chars() {
            if x < 0 {
                x += 1;
                continue;
            }
            let cx = x as u16;
            if cx >= self.terminal_width || self.editor_rect.contains(cx, row) {
                break;
            }
            cells.insert((cx, row), Glyph::new(character, LABEL_FG, LABEL_BG));
            x += 1;
        }
    }

    fn help_cells(&self, cells: &mut HashMap<(u16, u16), Glyph>) {
        let (help_row, help_width) = self.help_row_and_width();
        let focus_label = match self.focus {
            Focus::Marking => "MARK",
            Focus::Editor => "EDIT",
        };
        let text = format!(
            " REPORT [{focus_label}]  tab: swap  move: hjkl/arrows  flag: space/click/drag  clear: c  file: enter  back: esc  cancel: ^P   [{} regions]",
            self.regions.len()
        );
        for (cx, character) in (0..help_width).zip(text.chars().chain(std::iter::repeat(' '))) {
            cells.insert((cx, help_row), Glyph::new(character, HELP_FG, HELP_BG));
        }
    }

    fn editor_cells(&self, cells: &mut HashMap<(u16, u16), Glyph>) {
        let rect = self.editor_rect;
        if rect.width < 4 || rect.height < 3 {
            return;
        }
        let inner = rect.inner();

        // Fill the pane, then draw its border.
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
                let fg = if character == ' ' { EDITOR_FG } else { EDITOR_BORDER };
                cells.insert((cx, cy), Glyph::new(character, fg, EDITOR_BG));
            }
        }

        // Title on the top border.
        let title = if self.focus == Focus::Editor {
            " description (typing) "
        } else {
            " description "
        };
        for (i, character) in title.chars().enumerate() {
            let cx = rect.x + 1 + i as u16;
            if cx >= rect.x + rect.width - 1 {
                break;
            }
            cells.insert((cx, rect.y), Glyph::new(character, EDITOR_BORDER, EDITOR_BG));
        }

        // Scroll so the cursor line stays visible.
        let inner_height = inner.height as usize;
        let scroll = if self.buffer.row < inner_height {
            0
        } else {
            self.buffer.row + 1 - inner_height
        };

        for (index, line) in self.buffer.lines.iter().enumerate().skip(scroll).take(inner_height) {
            let row = inner.y + (index - scroll) as u16;
            for (col, &character) in line.iter().enumerate().take(inner.width as usize) {
                let cx = inner.x + col as u16;
                cells.insert((cx, row), Glyph::new(character, EDITOR_FG, EDITOR_BG));
            }
        }

        // Cursor as an inverted cell.
        let cursor_row = inner.y + (self.buffer.row - scroll) as u16;
        let cursor_col = inner.x + self.buffer.col.min(inner.width as usize - 1) as u16;
        let character = self
            .buffer
            .lines
            .get(self.buffer.row)
            .and_then(|line| line.get(self.buffer.col))
            .copied()
            .unwrap_or(' ');
        cells.insert(
            (cursor_col, cursor_row),
            Glyph::new(character, EDITOR_BG, EDITOR_CURSOR_BG),
        );
    }
}

/// Shift (in terminal character cells) that moves the frozen frame's center —
/// where the player is — into the middle of the game region. Kept even in `x`
/// so the two-cells-per-square alignment is preserved.
fn frame_offset(editor: &Rect) -> (i32, i32) {
    if editor.x > 0 {
        // Editor docked on the right: pull the frame left.
        ((-(editor.width as i32 / 2)) & !1, 0)
    } else {
        // Editor docked at the bottom: pull the frame up.
        (0, -(editor.height as i32 / 2))
    }
}

/// Split the terminal into the game region and the description pane. Fixed by
/// the terminal aspect ratio: side-by-side on wide terminals, stacked on tall.
fn editor_rect(terminal_width: u16, terminal_height: u16) -> Rect {
    if terminal_width >= terminal_height {
        let width = (terminal_width / 3)
            .max(24)
            .min(terminal_width.saturating_sub(16).max(1));
        Rect {
            x: terminal_width.saturating_sub(width),
            y: 0,
            width,
            height: terminal_height,
        }
    } else {
        let height = (terminal_height / 3)
            .max(6)
            .min(terminal_height.saturating_sub(6).max(1));
        Rect {
            x: 0,
            y: terminal_height.saturating_sub(height),
            width: terminal_width,
            height,
        }
    }
}

/// A blank cell for the parts of the game region the frozen frame does not
/// cover (the frame is translated, not scaled, so translated edges fall off).
fn blank_glyph() -> Glyph {
    Glyph::new(' ', Glyph::DEFAULT_FG_COLOR, Glyph::DEFAULT_BG_COLOR)
}

/// Blend `color` into a glyph's foreground and background. The character is
/// untouched, so a mark tints the underlying glyph instead of hiding it.
fn tint_glyph(glyph: Glyph, color: RGB8, strength: f32) -> Glyph {
    Glyph {
        character: glyph.character,
        fg_color: tint_color(glyph.fg_color, color, strength),
        bg_color: tint_color(glyph.bg_color, color, strength),
        bg_transparent: false,
    }
}

/// Every screen square in the inclusive world-square box `a`..`b`.
fn world_box_screen_squares(
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

/// The glyph at a screen character cell, if in bounds (coordinates may be
/// negative, e.g. when a translated frame hangs off an edge).
fn glyph_at(buffer: &[Vec<Glyph>], cx: i32, cy: i32) -> Option<Glyph> {
    if cx < 0 || cy < 0 {
        return None;
    }
    buffer
        .get(cx as usize)
        .and_then(|col| col.get(cy as usize))
        .copied()
}

/// File the issue: create the numbered dir, write the snapshot (with the
/// un-altered screen), then wrap the typed description and the flagged regions
/// into `issue.md` and `flags.json`.
pub fn finish_issue(
    game: &Game,
    input_history: &[InputRecord],
    map_name: Option<&str>,
    session: &ReportSession,
) -> Result<std::path::PathBuf, String> {
    let dir = create_issue_dir()?;
    write_snapshot_to(&dir.join("snapshot"), game, input_history, map_name);

    let note = render_issue_markdown(
        issue_number_of(&dir),
        map_name.unwrap_or(crate::DEFAULT_MAP_NAME),
        &session.description(),
        session.regions(),
    );
    std::fs::write(dir.join("issue.md"), note)
        .map_err(|error| format!("could not write issue.md: {error}"))?;
    std::fs::write(dir.join("flags.json"), flags_json(session.regions()))
        .map_err(|error| format!("could not write flags.json: {error}"))?;
    eprintln!("Filed issue at {}", dir.display());
    Ok(dir)
}

fn issue_number_of(dir: &std::path::Path) -> u32 {
    dir.file_name()
        .and_then(|name| name.to_str())
        .and_then(|name| name.parse().ok())
        .unwrap_or(0)
}

/// Build the issue markdown: template header, the reporter's description, then
/// a compact list of the flagged regions (bounds only — the per-square detail
/// is re-derivable when the issue is investigated).
pub fn render_issue_markdown(
    number: u32,
    map: &str,
    description: &str,
    regions: &[Region],
) -> String {
    let mut out = format!(
        "# Issue {number:04}\n\nMap: {map}\nCaptured: {}\n\n## Description\n\n",
        crate::game::snapshot::today_utc()
    );
    let trimmed = description.trim_end();
    if trimmed.is_empty() {
        out.push_str("(no description)\n");
    } else {
        out.push_str(trimmed);
        out.push('\n');
    }

    out.push_str("\n## Flagged regions\n\n");
    if regions.is_empty() {
        out.push_str("(none)\n");
    } else {
        for (index, region) in regions.iter().enumerate() {
            out.push_str(&format!(
                "{}. world({}, {})..({}, {}) — screen({}, {})..({}, {})\n",
                index + 1,
                region.world_min.x,
                region.world_min.y,
                region.world_max.x,
                region.world_max.y,
                region.screen_min.x,
                region.screen_min.y,
                region.screen_max.x,
                region.screen_max.y,
            ));
        }
    }
    out
}

fn flags_json(regions: &[Region]) -> String {
    let entries: Vec<String> = regions
        .iter()
        .map(|region| {
            format!(
                "  {{\"world\": [[{}, {}], [{}, {}]], \"screen\": [[{}, {}], [{}, {}]]}}",
                region.world_min.x,
                region.world_min.y,
                region.world_max.x,
                region.world_max.y,
                region.screen_min.x,
                region.screen_min.y,
                region.screen_max.x,
                region.screen_max.y,
            )
        })
        .collect();
    format!("[\n{}\n]\n", entries.join(",\n"))
}

#[cfg(test)]
mod tests {
    use super::*;

    fn region(min: (i32, i32), max: (i32, i32)) -> Region {
        Region {
            world_min: WorldSquare::new(min.0, min.1),
            world_max: WorldSquare::new(max.0, max.1),
            screen_min: ScreenBufferSquare::new(min.0, min.1),
            screen_max: ScreenBufferSquare::new(max.0, max.1),
        }
    }

    #[test]
    fn markdown_lists_regions_without_explain_detail() {
        let note = render_issue_markdown(42, "cubes", "the tint is wrong", &[region((4, 8), (5, 9))]);
        assert!(note.starts_with("# Issue 0042\n\nMap: cubes\n"));
        assert!(note.contains("## Description\n\nthe tint is wrong\n"));
        assert!(note.contains("## Flagged regions"));
        assert!(note.contains("1. world(4, 8)..(5, 9) — screen(4, 8)..(5, 9)"));
        assert!(!note.contains("```"), "per-square explain blocks are not duplicated");
    }

    #[test]
    fn markdown_handles_empty_description_and_no_regions() {
        let note = render_issue_markdown(1, "cubes", "   \n", &[]);
        assert!(note.contains("(no description)"));
        assert!(note.contains("(none)"));
    }

    #[test]
    fn flags_json_records_region_bounds() {
        let json = flags_json(&[region((1, 2), (3, 4))]);
        assert!(json.contains("\"world\": [[1, 2], [3, 4]]"));
        assert!(json.contains("\"screen\": [[1, 2], [3, 4]]"));
        assert!(!json.contains("summary"));
    }

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
    fn session_flags_and_unflags_the_cursor_square() {
        let game = crate::utils_for_tests::set_up_game_with_player();
        let mut session = ReportSession::open(&game);
        assert_eq!(session.focus(), Focus::Marking);
        assert!(session.regions().is_empty());

        session.handle_event(&game, Event::Key(Key::Char(' ')));
        assert_eq!(session.regions().len(), 1, "space marks the cursor square");
        let marked = &session.regions()[0];
        assert_eq!(
            marked.world_min, marked.world_max,
            "a click is a 1x1 region"
        );

        session.handle_event(&game, Event::Key(Key::Char(' ')));
        assert!(session.regions().is_empty(), "space again unmarks it");
    }

    #[test]
    fn editor_focus_types_into_the_description() {
        let game = crate::utils_for_tests::set_up_game_with_player();
        let mut session = ReportSession::open(&game);
        session.handle_event(&game, Event::Key(Key::Char('\t')));
        assert_eq!(session.focus(), Focus::Editor);
        for character in "tint".chars() {
            session.handle_event(&game, Event::Key(Key::Char(character)));
        }
        assert_eq!(session.description(), "tint");
    }

    #[test]
    fn tab_swaps_focus_and_ctrl_p_cancels() {
        let game = crate::utils_for_tests::set_up_game_with_player();
        let mut session = ReportSession::open(&game);
        session.handle_event(&game, Event::Key(Key::Char('\t')));
        assert_eq!(session.focus(), Focus::Editor);
        session.handle_event(&game, Event::Key(Key::Char('\t')));
        assert_eq!(session.focus(), Focus::Marking);
        assert!(matches!(
            session.handle_event(&game, Event::Key(Key::Ctrl('p'))),
            ReportAction::Cancel
        ));
    }

    #[test]
    fn session_reports_cancel_and_file_actions() {
        let game = crate::utils_for_tests::set_up_game_with_player();
        let mut session = ReportSession::open(&game);
        assert!(matches!(
            session.handle_event(&game, Event::Key(Key::Ctrl('p'))),
            ReportAction::Cancel
        ));
        assert!(matches!(
            session.handle_event(&game, Event::Key(Key::Char('\n'))),
            ReportAction::File
        ));
    }

    #[test]
    fn escape_does_not_cancel_the_issue() {
        let game = crate::utils_for_tests::set_up_game_with_player();
        let mut session = ReportSession::open(&game);
        // In the editor, `Esc` leaves the mode (vim-like) without cancelling.
        session.handle_event(&game, Event::Key(Key::Char('\t')));
        assert_eq!(session.focus(), Focus::Editor);
        assert!(matches!(
            session.handle_event(&game, Event::Key(Key::Esc)),
            ReportAction::None
        ));
        assert_eq!(session.focus(), Focus::Marking, "Esc returns to marking");
        // In marking too, `Esc` is not a cancel (a vim user hits it by habit).
        assert!(matches!(
            session.handle_event(&game, Event::Key(Key::Esc)),
            ReportAction::None
        ));
    }

    #[test]
    fn session_draw_is_dirty_gated() {
        let game = crate::utils_for_tests::set_up_game_with_player();
        let mut session = ReportSession::open(&game);
        let mut first: Vec<u8> = Vec::new();
        session.draw(&game, &mut first);
        assert!(!first.is_empty(), "the opening draw writes glyphs");

        let mut idle: Vec<u8> = Vec::new();
        session.draw(&game, &mut idle);
        assert!(idle.is_empty(), "an idle overlay emits nothing");
    }

    #[test]
    fn frame_offset_centers_the_player_in_the_game_region() {
        // Wide terminal: editor on the right, frame pulled left by half its width.
        let (dx, dy) = frame_offset(&editor_rect(120, 40));
        assert_eq!((dx, dy), (-20, 0), "shift is even and half the editor width");

        // Tall terminal: editor at the bottom, frame pulled up.
        let (dx, dy) = frame_offset(&editor_rect(40, 120));
        assert_eq!((dx, dy), (0, -20), "shift is half the editor height");
    }

    #[test]
    fn tint_keeps_the_character_and_blends_colors() {
        let original = Glyph::new('X', RGB8::new(0, 0, 0), RGB8::new(0, 0, 0));
        let tinted = tint_glyph(original, FLAG_TINT, FLAG_TINT_STRENGTH);
        assert_eq!(tinted.character, 'X', "the underlying glyph still shows");
        assert!(tinted.fg_color.r > 0 && tinted.fg_color.b > 0, "blended toward magenta");
        assert!(!tinted.bg_transparent, "the tint is visible");
    }

    #[test]
    fn mouse_maps_back_through_the_frame_offset() {
        let game = crate::utils_for_tests::set_up_game_with_player();
        let session = ReportSession::open(&game);
        let player_square = game.player_square();
        let screen_square = game
            .graphics()
            .screen
            .world_square_to_screen_buffer_square(player_square);
        let (tx, ty) = session.screen_square_to_term(screen_square);
        // Terminal coordinates are 1-indexed.
        let world = session.term_to_world(&game, (tx + 1) as u16, (ty + 1) as u16);
        assert_eq!(world, player_square, "the offset round-trips mouse input");
    }

    #[test]
    fn drag_creates_one_region_with_world_bounds() {
        let game = crate::utils_for_tests::set_up_game_with_player();
        let mut session = ReportSession::open(&game);
        session.add_region(&game, WorldSquare::new(0, 0), WorldSquare::new(2, 1));
        assert_eq!(session.regions().len(), 1, "a drag is a single region");
        let region = &session.regions()[0];
        assert_eq!(region.world_min, WorldSquare::new(0, 0));
        assert_eq!(region.world_max, WorldSquare::new(2, 1));
    }

    #[test]
    fn diff_draw_only_writes_changed_cells() {
        let game = crate::utils_for_tests::set_up_game_with_player();
        let mut session = ReportSession::open(&game);
        let mut opening: Vec<u8> = Vec::new();
        session.draw(&game, &mut opening);

        // Marking the cursor square changes only its two cells; the diff must
        // emit far less than the opening full-screen draw.
        session.handle_event(&game, Event::Key(Key::Char(' ')));
        let mut marked: Vec<u8> = Vec::new();
        session.draw(&game, &mut marked);
        assert!(!marked.is_empty());
        assert!(
            marked.len() < opening.len(),
            "a mark diff ({}) should be smaller than the full draw ({})",
            marked.len(),
            opening.len()
        );
    }

    #[test]
    fn labels_sit_one_row_above_a_region() {
        let game = crate::utils_for_tests::set_up_game_with_player();
        let session = ReportSession::open(&game);
        let mut cells: HashMap<(u16, u16), Glyph> = HashMap::new();
        let top_left = ScreenBufferSquare::new(4, 4);
        session.draw_label(&mut cells, top_left, 3);
        let (tx, ty) = session.screen_square_to_term(top_left);
        let label = cells.get(&(tx as u16, (ty - 1) as u16));
        assert_eq!(label.map(|glyph| glyph.character), Some('3'));
    }
}
