//! Review a filed issue capture and leave a free-form note.
//!
//! Standalone and terminal-only: it never touches the game loop or the in-game
//! report overlay. It loads a capture, renders the **current working tree** as
//! the "after" picture, and shows a before / before-marked / after triple
//! (one shared crop over the flagged regions and the player) beside a note
//! editor. `Ctrl+S` appends the note to the issue's `issue.md` (if non-empty)
//! and moves on; it does not decide, move, or archive anything — later
//! processing reads the note and acts.
//!
//! With no target (or `--all`) every open issue is reviewed in turn:
//!
//!     ./issue-review --all
//!
//! (the repo-root wrapper runs this bin through cargo). See
//! `docs/ISSUE_REVIEW_PLAN.md`.

use std::collections::HashMap;
use std::io::{stdout, Write};
use std::path::{Path, PathBuf};

use rgb::RGB8;
use termion::cursor::Goto;
use termion::event::{Event, Key};
use termion::input::TermRead;
use termion::raw::IntoRawMode;
use termion::screen::{ToAlternateScreen, ToMainScreen};
use termion::{cursor, terminal_size};

use game::game::snapshot::{self, debug};
use game::review_ui::{self, blank_glyph, tint_glyph, EditorColors, Rect, Region, TextBuffer};
use terminal_rendering::glyph::Glyph;
use terminal_rendering::{ScreenBufferSquare, WorldSquare};

/// Magenta tint on flagged regions, matching the in-game overlay.
const FLAG_TINT: RGB8 = RGB8::new(255, 0, 255);
const FLAG_STRENGTH: f32 = 0.45;
/// Cyan tint marks the player, so the crop is orientable in every pane.
const PLAYER_TINT: RGB8 = RGB8::new(0, 255, 255);
const PLAYER_STRENGTH: f32 = 0.5;
const TITLE_BG: RGB8 = RGB8::new(20, 20, 40);
const TITLE_FG: RGB8 = RGB8::new(220, 220, 235);
const HELP_BG: RGB8 = RGB8::new(40, 20, 20);
const HELP_FG: RGB8 = RGB8::new(255, 220, 220);
/// Issue-document pane colors.
const DOC_BG: RGB8 = RGB8::new(16, 18, 26);
const DOC_FG: RGB8 = RGB8::new(210, 214, 226);
const DOC_BORDER: RGB8 = RGB8::new(90, 90, 130);
const EDITOR_COLORS: EditorColors = EditorColors {
    fg: RGB8::new(214, 218, 232),
    bg: RGB8::new(10, 12, 22),
    border: RGB8::new(80, 80, 120),
    cursor_bg: RGB8::new(150, 150, 60),
};

fn usage() -> std::process::ExitCode {
    eprintln!(
        "Usage: issue_review [<NNNN|path>] [--all] [--issues-dir DIR] [--margin N] [--no-crop]\n\n\
         Shows a capture's before / before-marked / after pictures and appends a\n\
         note to its issue.md. With no issue (or --all) every open issue is\n\
         reviewed in turn.\n\
         \x20 Ctrl+S  save the note (if any) and go to the next issue\n\
         \x20 Ctrl+P  cancel\n\n\
         Run via: cargo run -p game --features debug-tools --bin issue_review -- <args>"
    );
    std::process::ExitCode::from(2)
}

fn main() -> std::process::ExitCode {
    let args: Vec<String> = std::env::args().skip(1).collect();
    let mut target: Option<String> = None;
    let mut all = false;
    let mut issues_dir: Option<PathBuf> = None;
    let mut margin: u32 = 4;
    let mut crop = true;

    let mut i = 0;
    while i < args.len() {
        match args[i].as_str() {
            "--all" => all = true,
            "--no-crop" => crop = false,
            "--issues-dir" => {
                i += 1;
                match args.get(i) {
                    Some(value) => issues_dir = Some(PathBuf::from(value)),
                    None => return usage(),
                }
            }
            "--margin" => {
                i += 1;
                match args.get(i).and_then(|value| value.parse::<u32>().ok()) {
                    Some(value) => margin = value,
                    None => return usage(),
                }
            }
            other if other.starts_with("--") => return usage(),
            other => target = Some(other.to_string()),
        }
        i += 1;
    }

    match run(target.as_deref(), all, issues_dir, margin, crop) {
        Ok(outcomes) => {
            if outcomes.is_empty() {
                println!("cancelled; no notes");
            } else {
                for outcome in &outcomes {
                    println!("{outcome}");
                }
            }
            std::process::ExitCode::SUCCESS
        }
        Err(error) => {
            eprintln!("error: {error}");
            std::process::ExitCode::FAILURE
        }
    }
}

/// What happened to one issue's note.
enum NoteOutcome {
    Appended { number: String, path: PathBuf, chars: usize },
    Skipped { number: String },
}

impl std::fmt::Display for NoteOutcome {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            NoteOutcome::Appended { number, path, chars } => {
                write!(f, "issue {number}: appended a {chars}-char note to {}", path.display())
            }
            NoteOutcome::Skipped { number } => write!(f, "issue {number}: no note"),
        }
    }
}

/// One issue loaded for review; built once per issue in the queue.
struct IssueView {
    number: String,
    issue_dir: PathBuf,
    panes: [Vec<Vec<Glyph>>; 3],
    pane_width: u16,
    pane_height: u16,
    /// The issue's `issue.md`, shown (re-flowed) beside the pictures.
    document: String,
}

fn run(
    target: Option<&str>,
    all: bool,
    issues_dir: Option<PathBuf>,
    margin: u32,
    crop: bool,
) -> Result<Vec<NoteOutcome>, String> {
    let base = issues_dir.unwrap_or_else(snapshot::issues_dir);
    let issue_dirs = if all || target.is_none() {
        discover_open_issues(&base)?
    } else {
        vec![resolve_issue_dir(target.unwrap(), &base)?]
    };
    if issue_dirs.is_empty() {
        return Err("no open issues to review".to_string());
    }

    let mut views = Vec::new();
    for issue_dir in &issue_dirs {
        views.push(load_view(issue_dir, margin, crop)?);
    }

    let mut app = ReviewApp::new(views);
    run_tui(&mut app)
}

// --- issue / flags ---

fn resolve_issue_dir(target: &str, base: &Path) -> Result<PathBuf, String> {
    let path = if target.chars().all(|c| c.is_ascii_digit()) {
        base.join(format!("{:04}", target.parse::<u32>().map_err(|e| e.to_string())?))
    } else {
        let candidate = PathBuf::from(target);
        if candidate.is_absolute() {
            candidate
        } else {
            base.join(target)
        }
    };
    let path = path
        .canonicalize()
        .map_err(|error| format!("no capture at {}: {error}", path.display()))?;
    if !path.join("snapshot").join("game_state.json").exists() {
        return Err(format!(
            "no capture at {} (expected snapshot/game_state.json)",
            path.display()
        ));
    }
    let solved = base
        .canonicalize()
        .map(|base| base.join("solved"))
        .unwrap_or_else(|_| base.join("solved"));
    if path.starts_with(&solved) {
        return Err(format!("{} is archived under issues/solved/", path.display()));
    }
    Ok(path)
}

/// Every open capture under `base`, numerically sorted. `solved/` and
/// non-capture entries are skipped.
fn discover_open_issues(base: &Path) -> Result<Vec<PathBuf>, String> {
    let base = base
        .canonicalize()
        .map_err(|error| format!("could not locate {}: {error}", base.display()))?;
    let mut dirs = Vec::new();
    for entry in std::fs::read_dir(&base).map_err(|error| error.to_string())? {
        let path = entry.map_err(|error| error.to_string())?.path();
        if !path.is_dir() {
            continue;
        }
        if path.file_name().and_then(|name| name.to_str()) == Some("solved") {
            continue;
        }
        if path.join("snapshot").join("game_state.json").exists() {
            dirs.push(path);
        }
    }
    dirs.sort();
    Ok(dirs)
}

/// Read `flags.json`, accepting both schemas: the newer `world`/`screen` pair
/// and the older `region`/`screen_region` (+ optional per-square `squares`).
/// A missing file (e.g. `issues/0020`) means "no flagged regions".
fn load_flags(path: &Path) -> Result<Vec<Region>, String> {
    if !path.exists() {
        return Ok(Vec::new());
    }
    let text = std::fs::read_to_string(path)
        .map_err(|error| format!("could not read {}: {error}", path.display()))?;
    let value: serde_json::Value = serde_json::from_str(&text)
        .map_err(|error| format!("could not parse {}: {error}", path.display()))?;
    let entries = value
        .as_array()
        .ok_or_else(|| format!("{}: expected a JSON array", path.display()))?;

    let mut regions = Vec::new();
    for entry in entries {
        let screen = entry.get("screen").or_else(|| entry.get("screen_region"));
        let world = entry.get("world").or_else(|| entry.get("region"));
        let (Some(screen), Some(world)) = (screen, world) else {
            continue;
        };
        let screen_min = point(screen, 0)?;
        let screen_max = point(screen, 1)?;
        let world_min = point(world, 0)?;
        let world_max = point(world, 1)?;
        regions.push(Region {
            world_min: WorldSquare::new(world_min.0, world_min.1),
            world_max: WorldSquare::new(world_max.0, world_max.1),
            screen_min: ScreenBufferSquare::new(screen_min.0, screen_min.1),
            screen_max: ScreenBufferSquare::new(screen_max.0, screen_max.1),
        });
    }
    Ok(regions)
}

fn point(value: &serde_json::Value, index: usize) -> Result<(i32, i32), String> {
    let pair = value
        .get(index)
        .and_then(|v| v.as_array())
        .ok_or_else(|| "flag bounds must be a pair of [x, y]".to_string())?;
    let x = pair.first().and_then(|v| v.as_i64()).ok_or("missing x")? as i32;
    let y = pair.get(1).and_then(|v| v.as_i64()).ok_or("missing y")? as i32;
    Ok((x, y))
}

/// Turn parsed `screen.txt` rows (`[y][x]` characters + colors) into a `[x][y]`
/// glyph buffer, the indexing `Screen::screen_buffer` uses.
fn cells_to_buffer(rows: &[Vec<debug::Cell>]) -> Vec<Vec<Glyph>> {
    let height = rows.len();
    let width = rows.first().map_or(0, Vec::len);
    let mut buffer = vec![vec![blank_glyph(); height]; width];
    for (y, row) in rows.iter().enumerate() {
        for (x, cell) in row.iter().enumerate() {
            buffer[x][y] = Glyph::new(cell.character, cell.fg, cell.bg);
        }
    }
    buffer
}

/// Load one capture: read its flags, render the current tree as "after", crop
/// the flagged regions ∪ player once for all three panes.
fn load_view(issue_dir: &Path, margin: u32, crop: bool) -> Result<IssueView, String> {
    let snapshot_dir = issue_dir.join("snapshot");
    let number = issue_dir
        .file_name()
        .and_then(|name| name.to_str())
        .map(str::to_owned)
        .unwrap_or_default();

    let regions = load_flags(&issue_dir.join("flags.json"))?;

    let before_rows = debug::parse_screen_text_file(&snapshot_dir.join("screen.txt"))?;
    let before = cells_to_buffer(&before_rows);
    let (after, player) = debug::render_snapshot_headless_buffer(&snapshot_dir)?;
    if before.len() != after.len() || before[0].len() != after[0].len() {
        return Err(format!(
            "{}: capture size {}x{} does not match the render {}x{}",
            issue_dir.display(),
            before.len(),
            before[0].len(),
            after.len(),
            after[0].len(),
        ));
    }
    let (width, height) = (before.len(), before[0].len());
    let crop = if crop {
        Crop::around(&regions, player, width, height, margin)
    } else {
        Crop {
            x0: 0,
            x1: width,
            y0: 0,
            y1: height,
        }
    };

    let (panes, pane_width, pane_height) = build_panes(&before, &after, &crop, &regions, player);
    let document = std::fs::read_to_string(issue_dir.join("issue.md")).unwrap_or_default();
    Ok(IssueView {
        number,
        issue_dir: issue_dir.to_path_buf(),
        panes,
        pane_width,
        pane_height,
        document,
    })
}

// --- crop & panes ---

/// An exclusive character-cell rectangle, shared by all three panes.
#[derive(Clone, Copy)]
struct Crop {
    x0: usize,
    x1: usize,
    y0: usize,
    y1: usize,
}

impl Crop {
    /// The flagged regions plus the player, padded by `margin` squares and
    /// clamped to the capture. `x0` is snapped even so two-column squares stay
    /// aligned.
    fn around(
        regions: &[Region],
        player: ScreenBufferSquare,
        width: usize,
        height: usize,
        margin: u32,
    ) -> Crop {
        let (mut min_x, mut min_y) = (player.x, player.y);
        let (mut max_x, mut max_y) = (player.x, player.y);
        for region in regions {
            min_x = min_x.min(region.screen_min.x);
            min_y = min_y.min(region.screen_min.y);
            max_x = max_x.max(region.screen_max.x);
            max_y = max_y.max(region.screen_max.y);
        }
        let margin = margin as i32;
        let x0 = ((2 * min_x - 2 * margin).max(0) as usize) & !1;
        let y0 = (min_y - margin).max(0) as usize;
        let x1 = (2 * (max_x + 1) + 2 * margin).max(0) as usize;
        let y1 = (max_y + 1 + margin).max(0) as usize;
        Crop {
            x0: x0.min(width),
            x1: x1.min(width),
            y0: y0.min(height),
            y1: y1.min(height),
        }
    }

    fn width(&self) -> usize {
        self.x1.saturating_sub(self.x0)
    }

    fn height(&self) -> usize {
        self.y1.saturating_sub(self.y0)
    }
}

/// True if the character cell `(cx, cy)` lies in any flagged region.
fn in_region(regions: &[Region], cx: usize, cy: usize) -> bool {
    regions.iter().any(|region| {
        let x0 = (2 * region.screen_min.x).max(0) as usize;
        let x1 = (2 * region.screen_max.x + 1).max(0) as usize;
        let y0 = region.screen_min.y.max(0) as usize;
        let y1 = region.screen_max.y.max(0) as usize;
        cx >= x0 && cx <= x1 && cy >= y0 && cy <= y1
    })
}

fn is_player(player: ScreenBufferSquare, cx: usize, cy: usize) -> bool {
    let x0 = (2 * player.x).max(0) as usize;
    cx >= x0 && cx <= x0 + 1 && cy == player.y.max(0) as usize
}

/// Build the three cropped panes: before, before-marked (regions tinted), and
/// after (regions tinted, so the change is locatable). The player is marked in
/// all three.
fn build_panes(
    before: &[Vec<Glyph>],
    after: &[Vec<Glyph>],
    crop: &Crop,
    regions: &[Region],
    player: ScreenBufferSquare,
) -> ([Vec<Vec<Glyph>>; 3], u16, u16) {
    let width = crop.width();
    let height = crop.height();
    let build = |source: &[Vec<Glyph>], with_regions: bool| {
        let mut pane = vec![vec![blank_glyph(); height]; width];
        for cx in crop.x0..crop.x1 {
            for cy in crop.y0..crop.y1 {
                let mut glyph = source[cx][cy];
                if with_regions && in_region(regions, cx, cy) {
                    glyph = tint_glyph(glyph, FLAG_TINT, FLAG_STRENGTH);
                }
                if is_player(player, cx, cy) {
                    glyph = tint_glyph(glyph, PLAYER_TINT, PLAYER_STRENGTH);
                }
                pane[cx - crop.x0][cy - crop.y0] = glyph;
            }
        }
        pane
    };
    let panes = [
        build(before, false),
        build(before, true),
        build(after, true),
    ];
    (panes, width as u16, height as u16)
}

// --- note ---

/// Append the note to `issue.md` unless it is empty or the file is absent.
/// Returns the outcome; the file is left untouched when skipped.
fn save_note(issue_dir: &Path, number: &str, text: &str) -> Result<NoteOutcome, String> {
    let path = issue_dir.join("issue.md");
    let trimmed = text.trim_end();
    if trimmed.trim().is_empty() || !path.exists() {
        return Ok(NoteOutcome::Skipped {
            number: number.to_owned(),
        });
    }
    let mut contents = std::fs::read_to_string(&path)
        .map_err(|error| format!("could not read {}: {error}", path.display()))?;
    if !contents.ends_with('\n') {
        contents.push('\n');
    }
    contents.push_str(&format!(
        "\n## Appended note ({})\n\n{trimmed}\n",
        snapshot::today_utc()
    ));
    std::fs::write(&path, contents)
        .map_err(|error| format!("could not write {}: {error}", path.display()))?;
    Ok(NoteOutcome::Appended {
        number: number.to_owned(),
        path,
        chars: trimmed.chars().count(),
    })
}

// --- TUI ---

/// How the panes are shown; there is no hotkey to change it.
#[derive(Clone, Copy, PartialEq, Eq)]
enum Layout {
    SideBySide,
    Stacked,
}

enum Action {
    None,
    Redraw,
    Save,
    Cancel,
}

struct ReviewApp {
    views: Vec<IssueView>,
    index: usize,
    editor: TextBuffer,
    /// Scroll offset (lines) of the re-flowed issue document.
    document_scroll: usize,
    term_width: u16,
    term_height: u16,
    last: HashMap<(u16, u16), Glyph>,
    dirty: bool,
}

impl ReviewApp {
    fn new(views: Vec<IssueView>) -> Self {
        let (term_width, term_height) = terminal_size().unwrap_or((120, 40));
        ReviewApp {
            views,
            index: 0,
            editor: TextBuffer::new(),
            document_scroll: 0,
            term_width,
            term_height,
            last: HashMap::new(),
            dirty: true,
        }
    }

    fn current(&self) -> &IssueView {
        &self.views[self.index]
    }

    /// Move to the next issue; `false` when the queue is exhausted.
    fn advance(&mut self) -> bool {
        self.index += 1;
        self.editor = TextBuffer::new();
        self.document_scroll = 0;
        self.last.clear();
        self.dirty = true;
        self.index < self.views.len()
    }

    fn editor_rect(&self) -> Rect {
        let height = (self.term_height / 3).clamp(4, 12);
        Rect {
            x: 0,
            y: self.term_height.saturating_sub(1 + height),
            width: self.term_width,
            height,
        }
    }

    /// The issue-document column on the right, or `None` when the terminal is
    /// too narrow (the pictures then get the full width).
    fn document_rect(&self, editor_top: u16) -> Option<Rect> {
        if self.term_width < 80 || self.current().document.is_empty() {
            return None;
        }
        let width = (self.term_width / 3).clamp(24, 60);
        let height = editor_top.saturating_sub(1);
        if height < 3 || self.term_width - width < 40 {
            return None;
        }
        Some(Rect {
            x: self.term_width - width,
            y: 1,
            width,
            height,
        })
    }

    /// Side-by-side when the cropped width fits the left region, else stacked.
    fn layout(&self, left_width: u16) -> Layout {
        let pw = self.current().pane_width as usize;
        if 3 * pw + 2 <= left_width as usize {
            Layout::SideBySide
        } else {
            Layout::Stacked
        }
    }

    fn pane_origins(&self, left_width: u16) -> Vec<(u16, u16)> {
        let (pw, ph) = (self.current().pane_width, self.current().pane_height);
        match self.layout(left_width) {
            Layout::SideBySide => (0..3).map(|i| (i as u16 * (pw + 1), 2)).collect(),
            Layout::Stacked => (0..3)
                .map(|i| (0u16, 2 + i as u16 * (ph + 1)))
                .collect(),
        }
    }

    fn scroll_document(&mut self, pages: i32) {
        let page = (self.term_height as i32 / 2).max(1);
        let next = self.document_scroll as i32 + pages * page;
        self.document_scroll = next.max(0) as usize;
    }

    fn handle_event(&mut self, event: Event) -> Action {
        // Any event may change what is drawn; the caller redraws on `Redraw`,
        // and `advance` sets `dirty` itself.
        self.dirty = true;
        match event {
            Event::Key(Key::Ctrl('p')) => Action::Cancel,
            Event::Key(Key::Ctrl('s')) => Action::Save,
            Event::Key(Key::PageUp) => {
                self.scroll_document(-1);
                Action::Redraw
            }
            Event::Key(Key::PageDown) => {
                self.scroll_document(1);
                Action::Redraw
            }
            other => {
                if review_ui::edit_buffer(&mut self.editor, &other) {
                    Action::Redraw
                } else {
                    Action::None
                }
            }
        }
    }

    fn draw(&mut self, out: &mut impl Write) -> Result<(), String> {
        if !self.dirty {
            return Ok(());
        }
        self.dirty = false;

        let mut cells: HashMap<(u16, u16), Glyph> = HashMap::new();
        for x in 0..self.term_width {
            for y in 0..self.term_height {
                cells.insert((x, y), blank_glyph());
            }
        }

        self.put_line(&mut cells, 0, &self.title(), TITLE_FG, TITLE_BG);

        let editor_rect = self.editor_rect();
        let document_rect = self.document_rect(editor_rect.y);
        let left_width = document_rect.map_or(self.term_width, |rect| rect.x);

        let labels = ["before", "before-marked", "after"];
        for (index, origin) in self.pane_origins(left_width).iter().enumerate() {
            self.put_pane_label(&mut cells, labels[index], *origin, left_width, editor_rect.y);
            self.put_pane(&mut cells, index, *origin, left_width, editor_rect.y);
        }

        if let Some(rect) = document_rect {
            self.draw_document(&mut cells, rect);
        }

        review_ui::draw_editor_pane(
            &mut cells,
            editor_rect,
            &self.editor,
            " note (^S save & next, ^P cancel) ",
            EDITOR_COLORS,
        );
        self.put_line(
            &mut cells,
            self.term_height - 1,
            " magenta: flags   cyan: player   pgup/pgdn: scroll doc   ^S: save note & next   ^P: cancel",
            HELP_FG,
            HELP_BG,
        );

        for (&cell, &glyph) in &cells {
            if self.last.get(&cell) != Some(&glyph) {
                let _ = write!(out, "{}{}", Goto(cell.0 + 1, cell.1 + 1), glyph);
            }
        }
        self.last = cells;
        out.flush().map_err(|error| error.to_string())
    }

    /// Draw the re-flowed issue document in a bordered, scrollable pane.
    fn draw_document(&self, cells: &mut HashMap<(u16, u16), Glyph>, rect: Rect) {
        if rect.width < 4 || rect.height < 3 {
            return;
        }
        let inner = rect.inner();
        // Border.
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
                let fg = if character == ' ' { DOC_FG } else { DOC_BORDER };
                cells.insert((cx, cy), Glyph::new(character, fg, DOC_BG));
            }
        }
        for (i, character) in " issue.md ".chars().enumerate() {
            let cx = rect.x + 1 + i as u16;
            if cx >= rect.x + rect.width - 1 {
                break;
            }
            cells.insert((cx, rect.y), Glyph::new(character, DOC_BORDER, DOC_BG));
        }

        let lines = wrap_text(&self.current().document, inner.width as usize);
        let max_scroll = lines.len().saturating_sub(inner.height as usize);
        let scroll = self.document_scroll.min(max_scroll);
        for (row, line) in lines.iter().skip(scroll).take(inner.height as usize).enumerate() {
            let cy = inner.y + row as u16;
            for (col, character) in line.chars().enumerate().take(inner.width as usize) {
                cells.insert((inner.x + col as u16, cy), Glyph::new(character, DOC_FG, DOC_BG));
            }
        }
    }

    fn title(&self) -> String {
        let position = if self.views.len() > 1 {
            format!(" ({}/{})", self.index + 1, self.views.len())
        } else {
            String::new()
        };
        format!(
            "Issue {}{position} — before | before-marked | after (after = current working tree)",
            self.current().number
        )
    }

    fn put_line(&self, cells: &mut HashMap<(u16, u16), Glyph>, row: u16, text: &str, fg: RGB8, bg: RGB8) {
        if row >= self.term_height {
            return;
        }
        for (x, character) in (0..self.term_width).zip(text.chars().chain(std::iter::repeat(' '))) {
            cells.insert((x, row), Glyph::new(character, fg, bg));
        }
    }

    fn put_pane_label(
        &self,
        cells: &mut HashMap<(u16, u16), Glyph>,
        label: &str,
        origin: (u16, u16),
        right: u16,
        bottom: u16,
    ) {
        let row = origin.1.saturating_sub(1);
        if row >= bottom {
            return;
        }
        for (i, character) in label.chars().enumerate() {
            let x = origin.0 + i as u16;
            if x >= right || row >= self.term_height {
                break;
            }
            cells.insert((x, row), Glyph::new(character, TITLE_FG, TITLE_BG));
        }
    }

    fn put_pane(
        &self,
        cells: &mut HashMap<(u16, u16), Glyph>,
        index: usize,
        origin: (u16, u16),
        right: u16,
        bottom: u16,
    ) {
        let view = self.current();
        let pane = &view.panes[index];
        for x in 0..view.pane_width {
            for y in 0..view.pane_height {
                let (cx, cy) = (origin.0 + x, origin.1 + y);
                if cx >= right || cy >= bottom {
                    continue;
                }
                cells.insert((cx, cy), pane[x as usize][y as usize]);
            }
        }
    }
}

/// Greedy word-wrap `text` to `width` characters, preserving blank lines.
/// Words longer than `width` are hard-broken.
fn wrap_text(text: &str, width: usize) -> Vec<String> {
    if width == 0 {
        return Vec::new();
    }
    let mut out = Vec::new();
    for raw in text.lines() {
        if raw.trim().is_empty() {
            out.push(String::new());
            continue;
        }
        let mut line = String::new();
        for word in raw.split_whitespace() {
            let mut word = word.to_string();
            while word.chars().count() > width {
                if !line.is_empty() {
                    out.push(std::mem::take(&mut line));
                }
                let head: String = word.chars().take(width).collect();
                out.push(head);
                word = word.chars().skip(width).collect();
            }
            if line.is_empty() {
                line = word;
            } else if line.chars().count() + 1 + word.chars().count() <= width {
                line.push(' ');
                line.push_str(&word);
            } else {
                out.push(std::mem::take(&mut line));
                line = word;
            }
        }
        out.push(line);
    }
    out
}

fn run_tui(app: &mut ReviewApp) -> Result<Vec<NoteOutcome>, String> {
    let mut out = stdout().into_raw_mode().map_err(|error| error.to_string())?;
    let _ = write!(out, "{}{}", ToAlternateScreen, cursor::Hide);

    app.dirty = true;
    let result = tui_loop(app, &mut out);

    let _ = write!(out, "{}", cursor::Show);
    let _ = write!(out, "{}", ToMainScreen);
    let _ = out.flush();

    result
}

fn tui_loop(app: &mut ReviewApp, out: &mut impl Write) -> Result<Vec<NoteOutcome>, String> {
    let mut outcomes = Vec::new();
    app.draw(out)?;
    let stdin = std::io::stdin();
    for event in stdin.events() {
        let event = event.map_err(|error| error.to_string())?;
        match app.handle_event(event) {
            Action::None => {}
            Action::Redraw => app.draw(out)?,
            Action::Save => {
                let issue_dir = app.current().issue_dir.clone();
                let number = app.current().number.clone();
                outcomes.push(save_note(&issue_dir, &number, &app.editor.text())?);
                if !app.advance() {
                    return Ok(outcomes);
                }
                app.draw(out)?;
            }
            Action::Cancel => return Ok(outcomes),
        }
    }
    Ok(outcomes)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn region(screen_min: (i32, i32), screen_max: (i32, i32)) -> Region {
        Region {
            world_min: WorldSquare::new(screen_min.0, screen_min.1),
            world_max: WorldSquare::new(screen_max.0, screen_max.1),
            screen_min: ScreenBufferSquare::new(screen_min.0, screen_min.1),
            screen_max: ScreenBufferSquare::new(screen_max.0, screen_max.1),
        }
    }

    fn temp_dir(tag: &str) -> PathBuf {
        let dir = std::env::temp_dir().join(format!(
            "ir_{tag}_{}",
            std::time::SystemTime::now()
                .duration_since(std::time::UNIX_EPOCH)
                .unwrap()
                .as_nanos()
        ));
        std::fs::create_dir_all(&dir).unwrap();
        dir
    }

    #[test]
    fn flags_parse_both_schemas() {
        let new = r#"[{"world": [[1,2],[3,4]], "screen": [[5,6],[7,8]]}]"#;
        let old = r#"[{"region": [[1,2],[3,4]], "screen_region": [[5,6],[7,8]],
                        "squares": [{"world":[1,2],"screen":[5,6],"summary":"x"}]}]"#;
        for text in [new, old] {
            let path = temp_dir("flags").join("flags.json");
            std::fs::write(&path, text).unwrap();
            let regions = load_flags(&path).unwrap();
            std::fs::remove_dir_all(path.parent().unwrap()).ok();
            assert_eq!(regions.len(), 1);
            assert_eq!(regions[0].screen_min, ScreenBufferSquare::new(5, 6));
            assert_eq!(regions[0].screen_max, ScreenBufferSquare::new(7, 8));
            assert_eq!(regions[0].world_min, WorldSquare::new(1, 2));
        }
    }

    #[test]
    fn missing_flags_file_is_empty() {
        let missing = std::env::temp_dir().join("ir_does_not_exist_flags.json");
        std::fs::remove_file(&missing).ok();
        assert!(load_flags(&missing).unwrap().is_empty());
    }

    #[test]
    fn crop_contains_flags_and_player_with_even_left_edge() {
        let regions = [region((46, 46), (48, 49))];
        let player = ScreenBufferSquare::new(10, 5);
        let crop = Crop::around(&regions, player, 100, 50, 4);
        assert!(crop.x0 <= 2 * 10, "player's left cell inside the crop");
        assert!(crop.x1 >= 2 * 48 + 1, "flagged region inside the crop");
        assert!(crop.y0 <= 5 && crop.y1 >= 50);
        assert_eq!(crop.x0 % 2, 0, "left edge is square-aligned");
    }

    #[test]
    fn crop_clamps_to_the_capture() {
        let regions = [region((0, 0), (1, 1))];
        let crop = Crop::around(&regions, ScreenBufferSquare::new(0, 0), 20, 10, 100);
        assert_eq!((crop.x0, crop.y0), (0, 0));
        assert_eq!((crop.x1, crop.y1), (20, 10));
    }

    #[test]
    fn panels_share_one_crop_size() {
        let (width, height) = (8, 6);
        let before: Vec<Vec<Glyph>> = vec![vec![blank_glyph(); height]; width];
        let after = before.clone();
        let crop = Crop { x0: 2, x1: 6, y0: 1, y1: 5 };
        let (panes, pw, ph) =
            build_panes(&before, &after, &crop, &[], ScreenBufferSquare::new(1, 1));
        assert_eq!((pw as usize, ph as usize), (4, 4));
        assert_eq!(panes[0].len(), panes[1].len());
        assert_eq!(panes[1].len(), panes[2].len());
        assert_eq!(panes[0][0].len(), panes[2][0].len());
    }

    #[test]
    fn flagged_cells_are_tinted_in_the_marked_pane_only() {
        let (width, height) = (6, 6);
        let before = vec![vec![blank_glyph(); height]; width];
        let after = before.clone();
        let crop = Crop { x0: 0, x1: width, y0: 0, y1: height };
        let regions = [region((1, 1), (1, 1))];
        let (panes, _, _) =
            build_panes(&before, &after, &crop, &regions, ScreenBufferSquare::new(9, 9));
        assert_eq!(panes[0][2][1], blank_glyph(), "before pane is unmarked");
        assert_ne!(panes[1][2][1], blank_glyph(), "marked pane tints the flag");
    }

    #[test]
    fn note_appends_with_a_dated_header_and_preserves_the_original() {
        let dir = temp_dir("note");
        std::fs::write(dir.join("issue.md"), "# Issue 0042\n\noriginal body\n").unwrap();
        let outcome = save_note(&dir, "0042", "  still wrong on the east side  ").unwrap();
        let text = std::fs::read_to_string(dir.join("issue.md")).unwrap();
        std::fs::remove_dir_all(&dir).ok();
        assert!(text.contains("original body"));
        assert!(text.contains("## Appended note ("));
        assert!(text.contains("still wrong on the east side"));
        assert!(matches!(outcome, NoteOutcome::Appended { .. }));
    }

    #[test]
    fn empty_note_is_skipped_and_leaves_the_file_untouched() {
        let dir = temp_dir("skip");
        let path = dir.join("issue.md");
        std::fs::write(&path, "# Issue 0042\n\noriginal body\n").unwrap();
        let before = std::fs::read_to_string(&path).unwrap();
        let outcome = save_note(&dir, "0042", "   \n  ").unwrap();
        let after = std::fs::read_to_string(&path).unwrap();
        std::fs::remove_dir_all(&dir).ok();
        assert_eq!(before, after);
        assert!(matches!(outcome, NoteOutcome::Skipped { .. }));
    }

    #[test]
    fn note_is_skipped_when_issue_md_is_absent() {
        let dir = temp_dir("absent");
        let outcome = save_note(&dir, "0042", "a real note").unwrap();
        std::fs::remove_dir_all(&dir).ok();
        assert!(matches!(outcome, NoteOutcome::Skipped { .. }));
    }

    #[test]
    fn wrap_text_reflows_words_and_hard_breaks_long_ones() {
        assert_eq!(
            wrap_text("the quick brown fox jumps", 10),
            vec!["the quick", "brown fox", "jumps"]
        );
        assert_eq!(
            wrap_text("supercalifragilistic", 6),
            vec!["superc", "alifra", "gilist", "ic"]
        );
        assert_eq!(wrap_text("a\n\nb", 10), vec!["a", "", "b"]);
        assert!(wrap_text("anything", 0).is_empty());
    }
}
