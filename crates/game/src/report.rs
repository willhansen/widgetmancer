//! In-game issue-report overlay (issue 0021).
//!
//! `Ctrl+P` opens a frozen overlay over the last drawn frame. The reporter
//! flags squares (mouse click/drag, or a keyboard cursor) and types a
//! description in a small built-in text editor (`Tab` swaps focus). Filing
//! writes a numbered `issues/NNNN/` with the snapshot, the description, and a
//! per-flag `explain` summary.
//!
//! The overlay is drawn *directly* to the terminal (never into
//! `screen_buffer`), so `snapshot/screen.txt` stays the un-altered game render
//! and `snapshot_tool verify-issues` is unaffected. Cells the overlay touched
//! are restored from `screen_buffer` on the next draw, and the overlay only
//! repaints when its state changed (`dirty`), so an idle overlay emits nothing.
//!
//! Gated behind the default-on `report` feature (which implies `debug-tools`
//! for `Game::explain_screen_cell`).

use std::io::Write;

use rgb::RGB8;
use termion::cursor::Goto;
use termion::event::{Event, Key, MouseButton, MouseEvent};

use terminal_rendering::glyph::Glyph;
use terminal_rendering::*;

use crate::game::snapshot::{create_issue_dir, write_snapshot_to, InputRecord};
use crate::game::Game;

/// Highlight color painted under a flagged square.
const FLAG_BG: RGB8 = RGB8::new(96, 0, 96);
/// Highlight color painted under the keyboard cursor.
const CURSOR_BG: RGB8 = RGB8::new(120, 96, 0);
/// Background of the help/status line.
const HELP_BG: RGB8 = RGB8::new(20, 20, 40);
/// Foreground of the help/status line.
const HELP_FG: RGB8 = RGB8::new(220, 220, 235);
/// Editor pane colors.
const EDITOR_BG: RGB8 = RGB8::new(10, 12, 22);
const EDITOR_FG: RGB8 = RGB8::new(214, 218, 232);
const EDITOR_BORDER: RGB8 = RGB8::new(80, 80, 120);
const EDITOR_CURSOR_BG: RGB8 = RGB8::new(150, 150, 60);

/// One square flagged for the issue, with its auto-generated summary.
#[derive(Clone, Debug)]
pub struct FlaggedSquare {
    pub world_square: WorldSquare,
    pub screen_square: ScreenBufferSquare,
    /// Concise `explain`-style summary (draw-buffer detail stripped).
    pub summary: String,
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

/// A live report overlay. Owns the flagged squares, the description buffer,
/// and the terminal-direct drawing state.
pub struct ReportSession {
    flags: Vec<FlaggedSquare>,
    cursor: ScreenBufferSquare,
    focus: Focus,
    /// Mouse drag anchor (world square) while the left button is held.
    drag_anchor: Option<WorldSquare>,
    /// Screen character cells the last overlay draw wrote, restored next draw.
    drawn_cells: Vec<(u16, u16)>,
    terminal_width: u16,
    terminal_height: u16,
    editor_rect: Rect,
    buffer: TextBuffer,
    /// Repaint only when something changed, so an idle overlay emits nothing.
    dirty: bool,
}

impl ReportSession {
    /// Open over `game`'s current (frozen) frame.
    pub fn open(game: &Game) -> Self {
        let screen = &game.graphics().screen;
        let terminal_width = screen.terminal_width;
        let terminal_height = screen.terminal_height;
        let editor_rect = editor_rect(terminal_width, terminal_height);
        let cursor = screen.world_square_to_screen_buffer_square(game.player_square());
        ReportSession {
            flags: Vec::new(),
            cursor,
            focus: Focus::Marking,
            drag_anchor: None,
            drawn_cells: Vec::new(),
            terminal_width,
            terminal_height,
            editor_rect,
            buffer: TextBuffer::new(),
            dirty: true,
        }
    }

    pub fn flags(&self) -> &[FlaggedSquare] {
        &self.flags
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
            Event::Key(Key::Esc) => return ReportAction::Cancel,
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
            Event::Key(Key::Esc) => ReportAction::Cancel,
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
            Key::Char('c') => self.flags.clear(),
            Key::Char('\n') | Key::Char('\r') => return ReportAction::File,
            _ => {}
        }
        ReportAction::None
    }

    fn handle_marking_mouse(&mut self, game: &Game, me: MouseEvent) -> ReportAction {
        // A click inside the editor focuses it.
        if let MouseEvent::Press(MouseButton::Left, x, y) = me {
            if self.editor_rect.contains(x.saturating_sub(1), y.saturating_sub(1)) {
                self.focus = Focus::Editor;
                return ReportAction::None;
            }
        }
        match me {
            MouseEvent::Press(MouseButton::Left, term_x, term_y) => {
                self.drag_anchor = Some(self.term_to_world(game, term_x, term_y));
            }
            MouseEvent::Hold(term_x, term_y) => {
                if self.drag_anchor.is_none() {
                    return ReportAction::None;
                }
                self.cursor = self.term_to_screen_square(term_x, term_y);
            }
            MouseEvent::Release(term_x, term_y) => {
                let end = self.term_to_world(game, term_x, term_y);
                match self.drag_anchor.take() {
                    Some(start) if start == end => self.toggle_flag(game, start),
                    Some(start) => self.flag_box(game, start, end),
                    None => {}
                }
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

    fn term_to_screen_square(&self, term_x: u16, term_y: u16) -> ScreenBufferSquare {
        ScreenBufferSquare::new(
            (term_x.saturating_sub(1) as i32) / 2,
            term_y.saturating_sub(1) as i32,
        )
    }

    fn term_to_world(&self, game: &Game, term_x: u16, term_y: u16) -> WorldSquare {
        game.graphics()
            .screen
            .screen_buffer_square_to_world_square(self.term_to_screen_square(term_x, term_y))
    }

    fn toggle_flag_at_cursor(&mut self, game: &Game) {
        let square = game
            .graphics()
            .screen
            .screen_buffer_square_to_world_square(self.cursor);
        self.toggle_flag(game, square);
    }

    fn toggle_flag(&mut self, game: &Game, world_square: WorldSquare) {
        if let Some(index) = self
            .flags
            .iter()
            .position(|flag| flag.world_square == world_square)
        {
            self.flags.remove(index);
            return;
        }
        self.add_flag(game, world_square);
    }

    fn flag_box(&mut self, game: &Game, a: WorldSquare, b: WorldSquare) {
        let (x0, x1) = (a.x.min(b.x), a.x.max(b.x));
        let (y0, y1) = (a.y.min(b.y), a.y.max(b.y));
        for y in y0..=y1 {
            for x in x0..=x1 {
                let square = WorldSquare::new(x, y);
                if !self.flags.iter().any(|flag| flag.world_square == square) {
                    self.add_flag(game, square);
                }
            }
        }
    }

    fn add_flag(&mut self, game: &Game, world_square: WorldSquare) {
        let screen = &game.graphics().screen;
        let screen_square = screen.world_square_to_screen_buffer_square(world_square);
        let summary = concise_explain(game, screen_square);
        self.flags.push(FlaggedSquare {
            world_square,
            screen_square,
            summary,
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

        let screen = &game.graphics().screen;
        let buffer = &screen.screen_buffer;
        let width = self.terminal_width;

        // Restore anything the last overlay draw covered.
        for &(cx, cy) in &self.drawn_cells {
            if let Some(glyph) = glyph_at(buffer, cx, cy) {
                let _ = write!(writer, "{}{}", Goto(cx + 1, cy + 1), glyph);
            }
        }
        self.drawn_cells.clear();

        let squares: Vec<(ScreenBufferSquare, RGB8)> = self
            .flags
            .iter()
            .map(|flag| (flag.screen_square, FLAG_BG))
            .chain(std::iter::once((self.cursor, CURSOR_BG)))
            .collect();
        for (square, bg) in squares {
            self.drawn_cells
                .extend(paint_square(writer, buffer, square, bg, width));
        }

        let (help_row, help_width) = self.help_row_and_width();
        self.drawn_cells
            .extend(paint_help(writer, self.flags.len(), self.focus, help_width, help_row));

        self.draw_editor(writer);
        let _ = writer.flush();
    }

    fn draw_editor(&mut self, writer: &mut dyn Write) {
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
                let glyph = Glyph::new(character, fg, EDITOR_BG);
                let _ = write!(writer, "{}{}", Goto(cx + 1, cy + 1), glyph);
                self.drawn_cells.push((cx, cy));
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
            let glyph = Glyph::new(character, EDITOR_BORDER, EDITOR_BG);
            let _ = write!(writer, "{}{}", Goto(cx + 1, rect.y + 1), glyph);
            self.drawn_cells.push((cx, rect.y));
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
                let glyph = Glyph::new(character, EDITOR_FG, EDITOR_BG);
                let _ = write!(writer, "{}{}", Goto(cx + 1, row + 1), glyph);
                self.drawn_cells.push((cx, row));
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
        let cursor = Glyph::new(character, EDITOR_BG, EDITOR_CURSOR_BG);
        let _ = write!(writer, "{}{}", Goto(cursor_col + 1, cursor_row + 1), cursor);
        self.drawn_cells.push((cursor_col, cursor_row));
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

/// The glyph at a screen character cell, if in bounds.
fn glyph_at(buffer: &[Vec<Glyph>], cx: u16, cy: u16) -> Option<Glyph> {
    buffer.get(cx as usize).and_then(|col| col.get(cy as usize)).copied()
}

/// `explain_screen_cell` with the verbose draw-buffer lines dropped.
fn concise_explain(game: &Game, screen_square: ScreenBufferSquare) -> String {
    let text = game.explain_screen_cell(screen_square.x.max(0) as usize, screen_square.y.max(0) as usize);
    text.lines()
        .filter(|line| !line.contains("draw_buffer:"))
        .collect::<Vec<_>>()
        .join("\n")
}

/// Paint a flagged/cursor square's two character cells highlighted, returning
/// the cells touched so they can be restored next frame.
fn paint_square(
    writer: &mut dyn Write,
    buffer: &[Vec<Glyph>],
    square: ScreenBufferSquare,
    bg: RGB8,
    terminal_width: u16,
) -> Vec<(u16, u16)> {
    let mut drawn = Vec::new();
    if square.x < 0 || square.y < 0 || square.x * 2 + 1 >= terminal_width as i32 {
        return drawn;
    }
    for cx in [square.x * 2, square.x * 2 + 1] {
        let (cx, cy) = (cx as u16, square.y as u16);
        let Some(glyph) = glyph_at(buffer, cx, cy) else {
            continue;
        };
        let mut highlighted = glyph;
        highlighted.bg_color = bg;
        highlighted.bg_transparent = false;
        let _ = write!(writer, "{}{}", Goto(cx + 1, cy + 1), highlighted);
        drawn.push((cx, cy));
    }
    drawn
}

/// Paint the help/status line, returning the cells touched.
fn paint_help(
    writer: &mut dyn Write,
    flag_count: usize,
    focus: Focus,
    width: u16,
    row: u16,
) -> Vec<(u16, u16)> {
    let focus_label = match focus {
        Focus::Marking => "MARK",
        Focus::Editor => "EDIT",
    };
    let text = format!(
        " REPORT [{focus_label}]  tab: swap  move: hjkl/arrows  flag: space/click/drag  clear: c  file: enter  cancel: esc/^P   [{flag_count} flagged]"
    );
    let mut drawn = Vec::new();
    let mut chars = text.chars();
    for cx in 0..width {
        let character = chars.next().unwrap_or(' ');
        let glyph = Glyph::new(character, HELP_FG, HELP_BG);
        let _ = write!(writer, "{}{}", Goto(cx + 1, row + 1), glyph);
        drawn.push((cx, row));
    }
    drawn
}

/// File the issue: create the numbered dir, write the snapshot (with the
/// un-altered screen), then wrap the typed description and the flagged squares
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
        session.flags(),
    );
    std::fs::write(dir.join("issue.md"), note)
        .map_err(|error| format!("could not write issue.md: {error}"))?;
    std::fs::write(dir.join("flags.json"), flags_json(session.flags()))
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
/// one section per flagged square.
pub fn render_issue_markdown(
    number: u32,
    map: &str,
    description: &str,
    flags: &[FlaggedSquare],
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

    out.push_str("\n## Flagged squares\n\n");
    if flags.is_empty() {
        out.push_str("(none)\n");
    } else {
        for (index, flag) in flags.iter().enumerate() {
            out.push_str(&format!(
                "### {}. world({}, {}) — screen({}, {})\n\n```\n{}\n```\n\n",
                index + 1,
                flag.world_square.x,
                flag.world_square.y,
                flag.screen_square.x,
                flag.screen_square.y,
                flag.summary,
            ));
        }
    }
    out
}

fn flags_json(flags: &[FlaggedSquare]) -> String {
    let entries: Vec<String> = flags
        .iter()
        .map(|flag| {
            format!(
                "  {{\"world\": [{}, {}], \"screen\": [{}, {}], \"summary\": {}}}",
                flag.world_square.x,
                flag.world_square.y,
                flag.screen_square.x,
                flag.screen_square.y,
                json_string(&flag.summary),
            )
        })
        .collect();
    format!("[\n{}\n]\n", entries.join(",\n"))
}

fn json_string(value: &str) -> String {
    let mut out = String::from("\"");
    for character in value.chars() {
        match character {
            '"' => out.push_str("\\\""),
            '\\' => out.push_str("\\\\"),
            '\n' => out.push_str("\\n"),
            '\r' => out.push_str("\\r"),
            '\t' => out.push_str("\\t"),
            c => out.push(c),
        }
    }
    out.push('"');
    out
}

#[cfg(test)]
mod tests {
    use super::*;

    fn flag(x: i32, y: i32, summary: &str) -> FlaggedSquare {
        FlaggedSquare {
            world_square: WorldSquare::new(x, y),
            screen_square: ScreenBufferSquare::new(x, y),
            summary: summary.to_string(),
        }
    }

    #[test]
    fn markdown_wraps_description_and_flags() {
        let note = render_issue_markdown(42, "cubes", "the tint is wrong", &[flag(4, 8, "depth 1")]);
        assert!(note.starts_with("# Issue 0042\n\nMap: cubes\n"));
        assert!(note.contains("## Description\n\nthe tint is wrong\n"));
        assert!(note.contains("### 1. world(4, 8) — screen(4, 8)"));
        assert!(note.contains("depth 1"));
    }

    #[test]
    fn markdown_handles_empty_description_and_no_flags() {
        let note = render_issue_markdown(1, "cubes", "   \n", &[]);
        assert!(note.contains("(no description)"));
        assert!(note.contains("(none)"));
    }

    #[test]
    fn flags_json_escapes_summary() {
        let json = flags_json(&[flag(1, 2, "a \"b\"\nc")]);
        assert!(json.contains("\"world\": [1, 2]"));
        assert!(json.contains("\\\"b\\\""));
        assert!(json.contains("\\nc"));
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
        assert!(session.flags().is_empty());

        session.handle_event(&game, Event::Key(Key::Char(' ')));
        assert_eq!(session.flags().len(), 1, "space flags the cursor square");

        session.handle_event(&game, Event::Key(Key::Char(' ')));
        assert!(session.flags().is_empty(), "space again unflags it");
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
            session.handle_event(&game, Event::Key(Key::Esc)),
            ReportAction::Cancel
        ));
        assert!(matches!(
            session.handle_event(&game, Event::Key(Key::Char('\n'))),
            ReportAction::File
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
}
