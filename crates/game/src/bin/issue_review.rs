//! Interactive approval gate for moving a live capture (`issues/NNNN`) to the
//! `issues/solved/` archive.
//!
//! It is deliberately independent of the game loop and the in-game report
//! overlay: it loads the saved snapshot, renders the *current* working tree as
//! the "after" picture, and shows a before / before-marked / after triple in a
//! terminal UI with a shared crop (flagged regions + the player). The reviewer
//! answers `approve? y/n`:
//!
//! * `y` — renames the original capture to `screen.pre-fix.txt`, blesses the
//!   fresh render as `screen.txt`, and `git mv`s `issues/NNNN` to
//!   `issues/solved/NNNN`. If the render is identical to the pre-fix capture it
//!   still moves, but warns that the fixture is vacuous.
//! * `n` — opens a built-in editor; the typed text is appended as a
//!   `## Clarification` section of `issue.md` and the issue stays open.
//!
//! Run through cargo so the binary matches the sources:
//!
//!     cargo run -p game --features debug-tools --bin issue_review -- 0027

use std::collections::HashMap;
use std::io::{stdout, Write};
use std::path::{Path, PathBuf};
use std::process::{Command, ExitCode};

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
const EDITOR_COLORS: EditorColors = EditorColors {
    fg: RGB8::new(214, 218, 232),
    bg: RGB8::new(10, 12, 22),
    border: RGB8::new(80, 80, 120),
    cursor_bg: RGB8::new(150, 150, 60),
};

fn usage() -> ExitCode {
    eprintln!(
        "Usage: issue_review <NNNN|issues/NNNN> [--flags FILE] [--margin N] [--no-crop]\n\
         \x20      issue_review --all   [--margin N] [--no-crop]\n\
         \x20      issue_review --ready [--margin N] [--no-crop]\n\n\
         Shows a capture's before / before-marked / after pictures in a terminal UI\n\
         and asks whether to move the issue to issues/solved/.\n\
         \x20 --all    review every open issue, advancing after each decision\n\
         \x20 --ready  review only issues whose render differs from the capture\n\n\
         Run via: cargo run -p game --features debug-tools --bin issue_review -- <args>"
    );
    ExitCode::from(2)
}

fn main() -> ExitCode {
    let args: Vec<String> = std::env::args().skip(1).collect();
    let mut target: Option<String> = None;
    let mut flags_path: Option<PathBuf> = None;
    let mut margin: u32 = 4;
    let mut crop = true;
    let mut all = false;
    let mut ready = false;

    let mut i = 0;
    while i < args.len() {
        match args[i].as_str() {
            "--all" => all = true,
            "--ready" => ready = true,
            "--no-crop" => crop = false,
            "--flags" => {
                i += 1;
                match args.get(i) {
                    Some(value) => flags_path = Some(PathBuf::from(value)),
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

    if !all && !ready && target.is_none() {
        return usage();
    }

    match run(target.as_deref(), all, ready, flags_path, margin, crop) {
        Ok(decisions) => {
            if decisions.is_empty() {
                println!("cancelled; nothing changed");
            } else {
                for decision in &decisions {
                    println!("{decision}");
                }
            }
            ExitCode::SUCCESS
        }
        Err(error) => {
            eprintln!("error: {error}");
            ExitCode::FAILURE
        }
    }
}

/// One reviewer decision, reported after the terminal is restored.
enum Decision {
    Moved {
        number: String,
        destination: PathBuf,
        changed: usize,
    },
    Clarified {
        number: String,
        path: PathBuf,
    },
    Skipped {
        number: String,
    },
    Failed {
        number: String,
        error: String,
    },
}

impl std::fmt::Display for Decision {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Decision::Moved {
                number,
                destination,
                changed,
            } => {
                write!(f, "issue {number}: approved — moved to {}", destination.display())?;
                if *changed == 0 {
                    write!(
                        f,
                        "\nwarning: issue {number}: the render is unchanged from the capture \
                         (vacuous fixture — the fix may not be in the working tree)"
                    )
                } else {
                    write!(f, " ({changed} cells differ from the capture)")
                }
            }
            Decision::Clarified { number, path } => write!(
                f,
                "issue {number}: rejected — appended a clarification to {}",
                path.display()
            ),
            Decision::Skipped { number } => write!(f, "issue {number}: skipped"),
            Decision::Failed { number, error } => {
                write!(f, "issue {number}: FAILED — {error}")
            }
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
    /// Cells that differ between the capture and the current render. `0` means
    /// the working tree still reproduces the capture (no candidate fix).
    changed: usize,
}

fn run(
    target: Option<&str>,
    all: bool,
    ready: bool,
    flags_path: Option<PathBuf>,
    margin: u32,
    crop: bool,
) -> Result<Vec<Decision>, String> {
    let issue_dirs: Vec<PathBuf> = if all || ready {
        if flags_path.is_some() {
            return Err("--flags applies to a single issue; not with --all/--ready".to_string());
        }
        discover_open_issues()?
    } else {
        vec![resolve_issue_dir(target.expect("target checked in main"))?]
    };

    let mut views = Vec::new();
    for issue_dir in &issue_dirs {
        views.push(load_view(issue_dir, flags_path.as_deref(), margin, crop)?);
    }
    if ready {
        views.retain(|view| view.changed > 0);
    }
    if views.is_empty() {
        return Err(if ready {
            "no issues are ready for review (the render still matches every capture)".to_string()
        } else {
            "no open issues to review".to_string()
        });
    }

    let mut app = ReviewApp::new(views);
    run_tui(&mut app)
}

/// Every open capture under `issues/`, numerically sorted. `issues/solved/` and
/// non-capture entries are skipped.
fn discover_open_issues() -> Result<Vec<PathBuf>, String> {
    let issues = snapshot::issues_dir()
        .canonicalize()
        .map_err(|error| format!("could not locate issues/: {error}"))?;
    let mut dirs = Vec::new();
    for entry in std::fs::read_dir(&issues).map_err(|error| error.to_string())? {
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

/// Load one capture: read its flags, render the current tree as "after", crop
/// the flagged regions ∪ player once for all three panes, and count how many
/// cells differ from the capture.
fn load_view(
    issue_dir: &Path,
    flags_override: Option<&Path>,
    margin: u32,
    crop: bool,
) -> Result<IssueView, String> {
    let snapshot_dir = issue_dir.join("snapshot");
    let number = issue_dir
        .file_name()
        .and_then(|name| name.to_str())
        .map(str::to_owned)
        .unwrap_or_default();

    let flags_path = flags_override
        .map(PathBuf::from)
        .unwrap_or_else(|| issue_dir.join("flags.json"));
    let regions = load_flags(&flags_path)?;

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

    let changed = count_changes(&before_rows, &after);
    let (panes, pane_width, pane_height) = build_panes(&before, &after, &crop, &regions, player);
    Ok(IssueView {
        number,
        issue_dir: issue_dir.to_path_buf(),
        panes,
        pane_width,
        pane_height,
        changed,
    })
}

// --- issue / flags ---

fn resolve_issue_dir(target: &str) -> Result<PathBuf, String> {
    let issues = snapshot::issues_dir();
    let path = if target.chars().all(|c| c.is_ascii_digit()) {
        issues.join(format!("{:04}", target.parse::<u32>().map_err(|e| e.to_string())?))
    } else {
        let candidate = PathBuf::from(target);
        if candidate.is_absolute() {
            candidate
        } else {
            issues.join(target)
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
    let solved = issues
        .canonicalize()
        .map(|issues| issues.join("solved"))
        .unwrap_or_else(|_| issues.join("solved"));
    if path.starts_with(&solved) {
        return Err(format!("{} is already archived under issues/solved/", path.display()));
    }
    Ok(path)
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

/// Cells whose character or colors differ between the capture and the render.
/// `0` means the working tree still reproduces the capture.
fn count_changes(before: &[Vec<debug::Cell>], after: &[Vec<Glyph>]) -> usize {
    let mut changed = 0;
    for (y, row) in before.iter().enumerate() {
        for (x, cell) in row.iter().enumerate() {
            let glyph = after[x][y];
            if cell.character != glyph.character
                || cell.fg != glyph.fg_color
                || cell.bg != glyph.bg_color
            {
                changed += 1;
            }
        }
    }
    changed
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

// --- TUI ---

/// How the three panes are shown for the current terminal size.
#[derive(Clone, Copy, PartialEq, Eq)]
enum Layout {
    SideBySide,
    Stacked,
    Cycle,
}

enum Action {
    None,
    Redraw,
    Approve,
    SaveClarification,
    Skip,
    Cancel,
}

struct ReviewApp {
    views: Vec<IssueView>,
    index: usize,
    labels: [&'static str; 3],
    cycle: usize,
    editing: bool,
    editor: TextBuffer,
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
            labels: ["before", "before-marked", "after"],
            cycle: 0,
            editing: false,
            editor: TextBuffer::new(),
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
        self.editing = false;
        self.editor = TextBuffer::new();
        self.cycle = 0;
        self.last.clear();
        self.dirty = true;
        self.index < self.views.len()
    }

    fn layout(&self) -> Layout {
        let w = self.term_width as usize;
        let h = self.term_height as usize;
        let (pw, ph) = (
            self.current().pane_width as usize,
            self.current().pane_height as usize,
        );
        if 3 * pw + 2 <= w && ph + 3 <= h {
            Layout::SideBySide
        } else if ph * 3 + 4 <= h {
            Layout::Stacked
        } else {
            Layout::Cycle
        }
    }

    /// Terminal rects for each pane (x, y, visible), plus the editor/help rows.
    fn pane_rects(&self) -> (Vec<(u16, u16)>, u16) {
        let (pw, ph) = (self.current().pane_width, self.current().pane_height);
        match self.layout() {
            Layout::SideBySide => {
                let origins = (0..3).map(|i| (i as u16 * (pw + 1), 2)).collect();
                (origins, 2 + ph)
            }
            Layout::Stacked => {
                let origins = (0..3).map(|i| (0u16, 2 + i as u16 * (ph + 1))).collect();
                (origins, 2 + 3 * (ph + 1))
            }
            Layout::Cycle => (vec![(0, 2)], 2 + ph),
        }
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

    fn handle_event(&mut self, event: Event) -> Action {
        if self.editing {
            return self.handle_editor(event);
        }
        match event {
            Event::Key(Key::Char('y')) => Action::Approve,
            Event::Key(Key::Char('n')) => {
                self.editing = true;
                Action::Redraw
            }
            Event::Key(Key::Char('s')) => Action::Skip,
            Event::Key(Key::Char('q')) | Event::Key(Key::Esc) => Action::Cancel,
            Event::Key(Key::Char('[')) | Event::Key(Key::Char(']')) | Event::Key(Key::Char('\t'))
                if self.layout() == Layout::Cycle =>
            {
                self.cycle = (self.cycle + 1) % 3;
                Action::Redraw
            }
            _ => Action::None,
        }
    }

    fn handle_editor(&mut self, event: Event) -> Action {
        match event {
            Event::Key(Key::Ctrl('s')) => Action::SaveClarification,
            Event::Key(Key::Esc) => {
                self.editing = false;
                Action::Redraw
            }
            Event::Key(Key::Char('\n')) | Event::Key(Key::Char('\r')) => {
                self.editor.insert_newline();
                Action::Redraw
            }
            Event::Key(Key::Char(c)) if !c.is_control() => {
                self.editor.insert_char(c);
                Action::Redraw
            }
            Event::Key(Key::Backspace) => {
                self.editor.backspace();
                Action::Redraw
            }
            Event::Key(Key::Delete) => {
                self.editor.delete();
                Action::Redraw
            }
            Event::Key(Key::Left) => {
                self.editor.move_left();
                Action::Redraw
            }
            Event::Key(Key::Right) => {
                self.editor.move_right();
                Action::Redraw
            }
            Event::Key(Key::Up) => {
                self.editor.move_up();
                Action::Redraw
            }
            Event::Key(Key::Down) => {
                self.editor.move_down();
                Action::Redraw
            }
            Event::Key(Key::Home) => {
                self.editor.home();
                Action::Redraw
            }
            Event::Key(Key::End) => {
                self.editor.end();
                Action::Redraw
            }
            _ => Action::None,
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
        let (origins, _) = self.pane_rects();
        let layout = self.layout();
        let editor_rect = if self.editing {
            Some(self.editor_rect())
        } else {
            None
        };

        let visible: Vec<usize> = match layout {
            Layout::Cycle => vec![self.cycle],
            _ => vec![0, 1, 2],
        };
        for (slot, index) in origins.iter().zip(visible.iter()) {
            let label = self.labels[*index];
            self.put_pane_label(&mut cells, &label, *slot);
            self.put_pane(&mut cells, *index, *slot, editor_rect);
        }

        if let Some(rect) = editor_rect {
            review_ui::draw_editor_pane(
                &mut cells,
                rect,
                &self.editor,
                " clarification (^S save, esc back) ",
                EDITOR_COLORS,
            );
            self.put_line(
                &mut cells,
                self.term_height - 1,
                " type your clarification; ^S to save it to issue.md",
                HELP_FG,
                HELP_BG,
            );
        } else {
            self.put_line(
                &mut cells,
                self.term_height - 1,
                &self.help(layout),
                HELP_FG,
                HELP_BG,
            );
        }

        for (&cell, &glyph) in &cells {
            if self.last.get(&cell) != Some(&glyph) {
                let _ = write!(out, "{}{}", Goto(cell.0 + 1, cell.1 + 1), glyph);
            }
        }
        self.last = cells;
        out.flush().map_err(|error| error.to_string())
    }

    fn title(&self) -> String {
        let position = if self.views.len() > 1 {
            format!(" ({}/{})", self.index + 1, self.views.len())
        } else {
            String::new()
        };
        let readiness = if self.current().changed == 0 {
            "  [unchanged from capture]"
        } else {
            ""
        };
        format!(
            "Issue {}{position} — approve move to issues/solved/?{readiness}   \
             y: approve   n: no / clarify   s: skip   q: quit",
            self.current().number
        )
    }

    fn help(&self, layout: Layout) -> String {
        let mode = match layout {
            Layout::SideBySide => "before | before-marked | after",
            Layout::Stacked => "before / before-marked / after (stacked)",
            Layout::Cycle => "one pane — [ ] / tab to cycle",
        };
        format!(" {mode}    magenta: flagged regions    cyan: player")
    }

    fn put_line(&self, cells: &mut HashMap<(u16, u16), Glyph>, row: u16, text: &str, fg: RGB8, bg: RGB8) {
        if row >= self.term_height {
            return;
        }
        for (x, character) in (0..self.term_width).zip(text.chars().chain(std::iter::repeat(' '))) {
            cells.insert((x, row), Glyph::new(character, fg, bg));
        }
    }

    fn put_pane_label(&self, cells: &mut HashMap<(u16, u16), Glyph>, label: &str, origin: (u16, u16)) {
        let row = origin.1.saturating_sub(1);
        for (i, character) in label.chars().enumerate() {
            let x = origin.0 + i as u16;
            if x >= self.term_width || row >= self.term_height {
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
        editor_rect: Option<Rect>,
    ) {
        let view = self.current();
        let pane = &view.panes[index];
        for x in 0..view.pane_width {
            for y in 0..view.pane_height {
                let (cx, cy) = (origin.0 + x, origin.1 + y);
                if cx >= self.term_width || cy >= self.term_height - 1 {
                    continue;
                }
                if let Some(rect) = editor_rect {
                    if rect.contains(cx, cy) {
                        continue;
                    }
                }
                cells.insert((cx, cy), pane[x as usize][y as usize]);
            }
        }
    }
}

fn run_tui(app: &mut ReviewApp) -> Result<Vec<Decision>, String> {
    let mut out = stdout().into_raw_mode().map_err(|error| error.to_string())?;
    let _ = write!(out, "{}{}", ToAlternateScreen, cursor::Hide);

    app.dirty = true;
    let result = tui_loop(app, &mut out);

    let _ = write!(out, "{}", cursor::Show);
    let _ = write!(out, "{}", ToMainScreen);
    let _ = out.flush();

    result
}

fn tui_loop(app: &mut ReviewApp, out: &mut impl Write) -> Result<Vec<Decision>, String> {
    let mut decisions = Vec::new();
    app.draw(out)?;
    let stdin = std::io::stdin();
    for event in stdin.events() {
        let event = event.map_err(|error| error.to_string())?;
        match app.handle_event(event) {
            Action::None => {}
            Action::Redraw => app.draw(out)?,
            Action::Approve => {
                let issue_dir = app.current().issue_dir.clone();
                let number = app.current().number.clone();
                decisions.push(match move_to_solved(&issue_dir, &number) {
                    Ok(decision) => decision,
                    Err(error) => Decision::Failed { number, error },
                });
                if !app.advance() {
                    return Ok(decisions);
                }
                app.draw(out)?;
            }
            Action::SaveClarification => {
                let issue_dir = app.current().issue_dir.clone();
                let number = app.current().number.clone();
                decisions.push(match append_clarification(&issue_dir, &app.editor.text()) {
                    Ok(path) => Decision::Clarified { number, path },
                    Err(error) => Decision::Failed { number, error },
                });
                if !app.advance() {
                    return Ok(decisions);
                }
                app.draw(out)?;
            }
            Action::Skip => {
                decisions.push(Decision::Skipped {
                    number: app.current().number.clone(),
                });
                if !app.advance() {
                    return Ok(decisions);
                }
                app.draw(out)?;
            }
            Action::Cancel => return Ok(decisions),
        }
    }
    Ok(decisions)
}

// --- move / clarify ---

/// Archive the issue: preserve the original capture as `screen.pre-fix.txt`,
/// bless the current render into `screen.txt`, and `git mv` the directory to
/// `issues/solved/`. A render identical to the pre-fix capture is a warning,
/// not a blocker (the reviewer may still accept the picture).
fn move_to_solved(issue_dir: &Path, number: &str) -> Result<Decision, String> {
    let issues = snapshot::issues_dir();
    let repo_root = issues
        .parent()
        .ok_or_else(|| "could not locate the repository root".to_string())?
        .canonicalize()
        .map_err(|error| format!("could not resolve the repository root: {error}"))?;
    let (destination, changed) = archive_issue(&repo_root, issue_dir)?;
    Ok(Decision::Moved {
        number: number.to_owned(),
        destination,
        changed,
    })
}

/// The move itself, against an explicit repository root so it is testable.
/// The issue must be tracked (as live captures are): `git mv` refuses an
/// untracked path.
fn archive_issue(repo_root: &Path, issue_dir: &Path) -> Result<(PathBuf, usize), String> {
    archive_issue_with_env(repo_root, issue_dir, &[])
}

/// [`archive_issue`], but clearing the listed environment variables from the
/// `git` child. Production inherits the ambient repo (the sandbox exports
/// `GIT_DIR`/`GIT_WORK_TREE`); tests pass those names to bind `git` to their
/// own temporary repository.
fn archive_issue_with_env(
    repo_root: &Path,
    issue_dir: &Path,
    clear_env: &[&str],
) -> Result<(PathBuf, usize), String> {
    let snapshot = issue_dir.join("snapshot");
    let screen_txt = snapshot.join("screen.txt");
    let pre_fix = snapshot.join("screen.pre-fix.txt");
    if !pre_fix.exists() {
        std::fs::copy(&screen_txt, &pre_fix)
            .map_err(|error| format!("could not preserve {}: {error}", pre_fix.display()))?;
    }
    debug::bless_snapshot(&snapshot)?;

    let changed = {
        let pre = debug::parse_screen_text_file(&pre_fix)?;
        let fixed = debug::parse_screen_text_file(&screen_txt)?;
        debug::diff_cells(&pre, &fixed).len()
    };

    let from = issue_dir
        .strip_prefix(repo_root)
        .map_err(|_| format!("{} is not under {}", issue_dir.display(), repo_root.display()))?;
    let name = issue_dir
        .file_name()
        .ok_or_else(|| "issue directory has no name".to_string())?;
    let to = Path::new("issues").join("solved").join(name);
    // `git mv` won't create the destination's parent directory.
    std::fs::create_dir_all(repo_root.join("issues").join("solved"))
        .map_err(|error| format!("could not create issues/solved: {error}"))?;
    let mut command = Command::new("git");
    command
        .arg("mv")
        .arg(from)
        .arg(&to)
        .current_dir(repo_root);
    for key in clear_env {
        command.env_remove(key);
    }
    let output = command
        .output()
        .map_err(|error| format!("could not run git: {error}"))?;
    if !output.status.success() {
        return Err(format!(
            "git mv failed: {}",
            String::from_utf8_lossy(&output.stderr).trim()
        ));
    }
    Ok((repo_root.join(&to), changed))
}

/// Append a dated `## Clarification` section to the issue note, leaving the
/// original text untouched.
fn append_clarification(issue_dir: &Path, text: &str) -> Result<PathBuf, String> {
    let path = issue_dir.join("issue.md");
    let mut contents = std::fs::read_to_string(&path)
        .map_err(|error| format!("could not read {}: {error}", path.display()))?;
    if !contents.ends_with('\n') {
        contents.push('\n');
    }
    contents.push_str(&format!(
        "\n## Clarification ({})\n\n{}\n",
        snapshot::today_utc(),
        text.trim_end()
    ));
    std::fs::write(&path, contents)
        .map_err(|error| format!("could not write {}: {error}", path.display()))?;
    Ok(path)
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

    #[test]
    fn flags_parse_both_schemas() {
        let new = r#"[{"world": [[1,2],[3,4]], "screen": [[5,6],[7,8]]}]"#;
        let old = r#"[{"region": [[1,2],[3,4]], "screen_region": [[5,6],[7,8]],
                        "squares": [{"world":[1,2],"screen":[5,6],"summary":"x"}]}]"#;
        for text in [new, old] {
            let dir = std::env::temp_dir().join(format!(
                "ir_flags_{}.json",
                std::time::SystemTime::now()
                    .duration_since(std::time::UNIX_EPOCH)
                    .unwrap()
                    .as_nanos()
            ));
            std::fs::write(&dir, text).unwrap();
            let regions = load_flags(&dir).unwrap();
            std::fs::remove_file(&dir).ok();
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
        let crop = Crop::around(
            &regions,
            ScreenBufferSquare::new(0, 0),
            20,
            10,
            100,
        );
        assert_eq!((crop.x0, crop.y0), (0, 0));
        assert_eq!((crop.x1, crop.y1), (20, 10));
    }

    #[test]
    fn panels_share_one_crop_size() {
        let width = 8;
        let height = 6;
        let before: Vec<Vec<Glyph>> =
            vec![vec![blank_glyph(); height]; width];
        let after = before.clone();
        let crop = Crop {
            x0: 2,
            x1: 6,
            y0: 1,
            y1: 5,
        };
        let (panes, pw, ph) = build_panes(&before, &after, &crop, &[], ScreenBufferSquare::new(1, 1));
        assert_eq!((pw as usize, ph as usize), (4, 4));
        assert_eq!(panes[0].len(), panes[1].len());
        assert_eq!(panes[1].len(), panes[2].len());
        assert_eq!(panes[0][0].len(), panes[2][0].len());
    }

    #[test]
    fn flagged_cells_are_tinted_in_the_marked_pane_only() {
        let width = 6;
        let height = 6;
        let before = vec![vec![blank_glyph(); height]; width];
        let after = before.clone();
        let crop = Crop {
            x0: 0,
            x1: width,
            y0: 0,
            y1: height,
        };
        let regions = [region((1, 1), (1, 1))];
        let (panes, _, _) =
            build_panes(&before, &after, &crop, &regions, ScreenBufferSquare::new(9, 9));
        assert_eq!(panes[0][2][1], blank_glyph(), "before pane is unmarked");
        assert_ne!(panes[1][2][1], blank_glyph(), "marked pane tints the flag");
    }

    #[test]
    fn clarification_appends_without_losing_the_original() {
        let dir = std::env::temp_dir().join(format!(
            "ir_clarify_{}",
            std::time::SystemTime::now()
                .duration_since(std::time::UNIX_EPOCH)
                .unwrap()
                .as_nanos()
        ));
        std::fs::create_dir_all(&dir).unwrap();
        std::fs::write(dir.join("issue.md"), "# Issue 0042\n\noriginal body\n").unwrap();
        let path = append_clarification(&dir, "  still wrong on the east side  ").unwrap();
        let text = std::fs::read_to_string(&path).unwrap();
        std::fs::remove_dir_all(&dir).ok();
        assert!(text.contains("original body"));
        assert!(text.contains("## Clarification ("));
        assert!(text.contains("still wrong on the east side"));
    }

    #[test]
    fn count_changes_detects_differing_cells() {
        let before = vec![vec![
            debug::Cell {
                character: 'a',
                fg: RGB8::new(0, 0, 0),
                bg: RGB8::new(0, 0, 0),
            },
            debug::Cell {
                character: 'b',
                fg: RGB8::new(0, 0, 0),
                bg: RGB8::new(0, 0, 0),
            },
        ]];
        let mut after = cells_to_buffer(&before);
        assert_eq!(count_changes(&before, &after), 0);
        after[0][0] = Glyph::new('a', RGB8::new(255, 0, 0), RGB8::new(0, 0, 0));
        assert_eq!(count_changes(&before, &after), 1);
    }

    fn test_git(dir: &Path, args: &[&str]) {
        let status = Command::new("git")
            .args(args)
            // The sandbox exports GIT_DIR/GIT_WORK_TREE; don't let the test
            // operate on the host repo.
            .env_remove("GIT_DIR")
            .env_remove("GIT_WORK_TREE")
            .current_dir(dir)
            .status()
            .expect("run git");
        assert!(status.success(), "git {args:?} failed");
    }

    fn copy_dir(from: &Path, to: &Path) {
        std::fs::create_dir_all(to).unwrap();
        for entry in std::fs::read_dir(from).unwrap() {
            let entry = entry.unwrap();
            let source = entry.path();
            let destination = to.join(entry.file_name());
            if source.is_dir() {
                copy_dir(&source, &destination);
            } else {
                std::fs::copy(&source, &destination).unwrap();
            }
        }
    }

    #[test]
    fn archive_moves_the_issue_and_preserves_the_pre_fix_capture() {
        let base = std::env::temp_dir().join(format!(
            "ir_archive_{}",
            std::time::SystemTime::now()
                .duration_since(std::time::UNIX_EPOCH)
                .unwrap()
                .as_nanos()
        ));
        let issue = base.join("issues/0027");
        let source = Path::new(env!("CARGO_MANIFEST_DIR")).join("../../issues/0027/snapshot");
        copy_dir(&source, &issue.join("snapshot"));

        test_git(&base, &["init", "-q"]);
        test_git(&base, &["add", "-A"]);
        test_git(
            &base,
            &[
                "-c",
                "user.email=test@example.com",
                "-c",
                "user.name=test",
                "commit",
                "-qm",
                "seed",
            ],
        );

        let (destination, _changed) =
            archive_issue_with_env(&base, &issue, &["GIT_DIR", "GIT_WORK_TREE"]).unwrap();

        assert!(!issue.exists(), "the original directory is moved, not copied");
        let snapshot = destination.join("snapshot");
        assert!(snapshot.join("screen.txt").exists());
        assert!(
            snapshot.join("screen.pre-fix.txt").exists(),
            "the original capture is preserved as the pre-fix render"
        );

        std::fs::remove_dir_all(&base).ok();
    }
}
