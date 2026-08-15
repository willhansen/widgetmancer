//! Visual debug tool for floating square rendering.
//!
//! Renders the same character picks the game uses
//! (`characters_for_full_square_with_2d_offset`, one world square = 2 terminal
//! columns) onto a checkerboard background with a dot marking each square
//! center, so sub-square offsets are easy to eyeball.
//!
//! Modes:
//!   pos X Y   draw one square at world point (X, Y), plus a text dump of the
//!             3x3 half-grid char table from `get_chars_for_floating_square`
//!   sweep     grid of mini views for x offsets 0..=0.5 x y offsets 0..=0.5
//!   animate   square orbiting the origin on the alternate screen (q quits)
//!
//! Run via scripts/debug-floating-squares.sh or:
//!   cargo run -p terminal_rendering --bin floating_square_debug -- pos 1.3 0.7

use std::io::{stdin, stdout, IsTerminal, Write};
use std::sync::mpsc::channel;
use std::thread;
use std::time::Duration;

use rgb::RGB8;
use termion::event::{Event, Key, MouseEvent};
use termion::input::{MouseTerminal, TermRead};
use termion::raw::IntoRawMode;
use termion::screen::IntoAlternateScreen;

use terminal_rendering::glyph_constants::named_colors::*;
use terminal_rendering::glyph_constants::SPACE;
use terminal_rendering::*;

const SQUARE_COLOR: RGB8 = RGB8::new(255, 165, 0); // orange
const CENTER_DOT_COLOR: RGB8 = RGB8::new(90, 90, 110);
const BG_DARK: RGB8 = RGB8::new(16, 16, 24);
const BG_LIGHT: RGB8 = RGB8::new(30, 30, 44);
const ORIGIN_COLOR: RGB8 = RGB8::new(0, 180, 180);

/// Same checkerboard parity rule as Graphics::square_is_light.
fn bg_color_for(square: WorldSquare) -> RGB8 {
    if (square.x + square.y).rem_euclid(2) == 0 {
        BG_LIGHT
    } else {
        BG_DARK
    }
}

/// Blank frame covering `radius` squares around `origin_square`, with a dot at
/// each square center so sub-square offsets are visible against the grid.
fn grid_frame(radius: i32, origin_square: WorldSquare) -> Frame {
    let squares_wide = (2 * radius + 1) as usize;
    let mut frame = Frame::blank(squares_wide * 2, squares_wide);
    for dx in -radius..=radius {
        for dy in -radius..=radius {
            let square = euclid::point2(origin_square.x + dx, origin_square.y + dy);
            let bg = bg_color_for(square);
            let marker = if square == origin_square {
                ('+', ORIGIN_COLOR)
            } else {
                ('·', CENTER_DOT_COLOR)
            };
            let [row, wide_col] = frame_row_col(radius, origin_square, square);
            frame.set_by_double_wide_grid(
                row,
                wide_col,
                [
                    DrawableGlyph::new_colored(marker.0, marker.1, bg),
                    DrawableGlyph::new_colored(SPACE, BLACK, bg),
                ],
            );
        }
    }
    frame
}

fn frame_row_col(radius: i32, origin_square: WorldSquare, square: WorldSquare) -> [usize; 2] {
    let [row, col] = [
        radius - (square.y - origin_square.y),
        radius + (square.x - origin_square.x),
    ];
    [row as usize, col as usize]
}

/// Mirrors OffsetSquareDrawable::drawables_for_floating_square_at_point:
/// only the 3x3 neighborhood of the rounded center square can be non-empty.
fn draw_floating_square(frame: &mut Frame, radius: i32, origin_square: WorldSquare, pos: WorldPoint) {
    let center = world_point_to_world_square(pos);
    for dx in -1..=1i32 {
        for dy in -1..=1i32 {
            let square = euclid::point2(center.x + dx, center.y + dy);
            let [row, wide_col] = frame_row_col(radius, origin_square, square);
            if row >= frame.height() || wide_col * 2 + 1 >= frame.width() {
                continue;
            }
            let offset: WorldMove = pos - square.to_f32();
            let chars = characters_for_full_square_with_2d_offset(offset);
            if chars != [SPACE; 2] {
                let bg = bg_color_for(square);
                frame.set_by_double_wide_grid(
                    row,
                    wide_col,
                    chars.map(|c| DrawableGlyph::new_colored(c, SQUARE_COLOR, bg)),
                );
            }
        }
    }
}

/// Which branch of get_chars_for_floating_square a position takes.
fn smoothing_branch(pos: WorldPoint) -> &'static str {
    let offset = fraction_part(pos);
    let (x, y) = (offset.x.abs(), offset.y.abs());
    if y < x && y < 0.25 {
        "smooth horizontal"
    } else if x < 0.25 {
        "smooth vertical"
    } else {
        "half grid (quadrant blocks)"
    }
}

fn dump_char_grid(pos: WorldPoint) {
    // get_chars_for_floating_square takes the unitless FPoint alias.
    let grid = get_chars_for_floating_square(euclid::point2(pos.x, pos.y));
    println!("get_chars_for_floating_square 3x3 (i = x+1, j = y+1):");
    for j in (0..3).rev() {
        let row: String = (0..3)
            .map(|i| grid[i][j].unwrap_or('·'))
            .collect();
        println!("  {row}");
    }
}

fn show_position(pos: WorldPoint) {
    let frac = fraction_part(pos);
    println!(
        "pos=({:.3}, {:.3})  frac=({:+.3}, {:+.3})  branch={}",
        pos.x,
        pos.y,
        frac.x,
        frac.y,
        smoothing_branch(pos)
    );
    let mut frame = grid_frame(3, world_point_to_world_square(pos));
    draw_floating_square(&mut frame, 3, world_point_to_world_square(pos), pos);
    println!("{frame}{}", Glyph::reset_colors());
    dump_char_grid(pos);
}

fn show_sweep() {
    // One 3x3-square cell (6 columns x 3 rows) per offset, +1 col/row gap.
    let x_offsets = [0.0f32, 0.1, 0.2, 0.3, 0.4, 0.5];
    let y_offsets = [0.0f32, 0.25, 0.5];
    // cell = 3 squares wide (6 char columns) x 3 rows, plus a 2-column gap
    let (cell_w, cell_h, gap) = (8usize, 3usize, 2usize);
    let mut big = Frame::blank(x_offsets.len() * cell_w, y_offsets.len() * (cell_h + gap));
    for (yi, &y_off) in y_offsets.iter().enumerate() {
        for (xi, &x_off) in x_offsets.iter().enumerate() {
            let pos: WorldPoint = euclid::point2(x_off, y_off);
            let mut cell = grid_frame(1, euclid::point2(0, 0));
            draw_floating_square(&mut cell, 1, euclid::point2(0, 0), pos);
            let row = yi * (cell_h + gap);
            let col = xi * cell_w;
            big.blit(&cell, [row as i32, col as i32]);
        }
    }
    let header: String = x_offsets
        .iter()
        .map(|x| format!("x={x:<+.2} "))
        .collect();
    println!("         {header}");
    let display_string = big.string_for_regular_display();
    for (yi, &y_off) in y_offsets.iter().enumerate() {
        let lines: Vec<&str> = display_string
            .lines()
            .skip(yi * (cell_h + gap))
            .take(cell_h)
            .collect();
        for line in lines {
            println!("y={y_off:<+.2}  {line}");
        }
        println!();
    }
}

const ORBIT_RADIUS: f32 = 2.5;
/// Half-width of the animation grid in squares; fixed so mouse cells can be
/// mapped back to world geometry.
const ANIMATE_GRID_RADIUS: i32 = 4;

/// Angle from the grid origin to the (1-based) terminal cell under the mouse.
/// atan2 on the pixel offset makes angular resolution grow with distance, so
/// dragging out past the grid edge allows very slight adjustments.
fn mouse_cell_angle(col: u16, row: u16) -> f32 {
    let r = ANIMATE_GRID_RADIUS as f32;
    // The origin square spans cols 2r+1..=2r+2 (center 2r+1.5), row r+1.
    let dx_cells = col as f32 - (2.0 * r + 1.5);
    let dy_cells = row as f32 - (r + 1.0);
    // Convert to world units: 2 columns per square vs 1 row per square,
    // and world +y is up while terminal rows increase downward.
    (-dy_cells).atan2(dx_cells * 0.5)
}

/// `raw_mode`: true when writing to a termion raw-mode terminal. Raw mode
/// disables ONLCR, so bare '\n' would stair-step the frame.
const FRAME_DT: Duration = Duration::from_millis(33);

fn render_animation_frame(out: &mut impl Write, theta: f32, time: Duration, raw_mode: bool) {
    let pos: WorldPoint =
        euclid::point2(theta.cos() * ORBIT_RADIUS, theta.sin() * ORBIT_RADIUS);
    let origin = euclid::point2(0, 0);
    let mut frame = grid_frame(ANIMATE_GRID_RADIUS, origin);
    draw_floating_square(&mut frame, ANIMATE_GRID_RADIUS, origin, pos);
    let frame_text = if raw_mode {
        frame.string_for_regular_display().replace('\n', "\r\n")
    } else {
        frame.to_string()
    };
    write!(
        out,
        "{frame_text}{}pos=({:6.3}, {:6.3})  branch={:<26}  t={:.1}s",
        Glyph::reset_colors(),
        pos.x,
        pos.y,
        smoothing_branch(pos),
        time.as_secs_f32(),
    )
    .unwrap();
    // erase leftovers when the status text shrinks between frames.
    // UntilNewline clears from the cursor to end of line; CurrentLine would
    // wipe the whole line, including the status just written.
    write!(out, "{}", termion::clear::UntilNewline).unwrap();
    out.flush().unwrap();
}

/// `frame_count`: None = run until quit (interactive) or a short default
/// (piped output). In interactive mode the square orbits until q/Esc.
fn run_animation(frame_count: Option<u32>) {
    let mut theta = 0.0f32;
    let interactive = stdout().is_terminal();

    if !interactive {
        // Piped output (e.g. `animate 5 | less -R`): no raw mode available.
        // These frames render instantly, so simulate the animation clock.
        let mut t = Duration::ZERO;
        for _ in 0..frame_count.unwrap_or(8) {
            render_animation_frame(&mut stdout(), theta, t, false);
            println!("\n");
            theta = (theta + 0.02).rem_euclid(std::f32::consts::TAU);
            t += FRAME_DT;
        }
        return;
    }

    let (tx, rx) = channel();
    thread::spawn(move || {
        for event in stdin().events() {
            if tx.send(event.unwrap()).is_err() {
                break;
            }
        }
    });

    let mut screen = MouseTerminal::from(stdout().into_raw_mode().unwrap())
        .into_alternate_screen()
        .unwrap();

    let mut paused = false;
    let mut frames = 0u32;
    let mut dirty = true;
    // Animation clock (freezes while paused), so a paused frame is fully
    // static and needs no redraws.
    let mut anim_time = Duration::ZERO;
    loop {
        while let Ok(event) = rx.try_recv() {
            match event {
                Event::Key(Key::Char('q')) | Event::Key(Key::Esc) => return,
                Event::Key(Key::Char(' ')) => {
                    paused = !paused;
                    dirty = true;
                }
                // Click both snaps the angle and pauses so the adjustment
                // sticks instead of the orbit moving on.
                Event::Mouse(MouseEvent::Press(_, x, y)) => {
                    theta = mouse_cell_angle(x, y);
                    paused = true;
                    dirty = true;
                }
                Event::Mouse(MouseEvent::Hold(x, y)) => {
                    theta = mouse_cell_angle(x, y);
                    dirty = true;
                }
                _ => {}
            }
        }

        if !paused {
            theta = (theta + 0.02).rem_euclid(std::f32::consts::TAU);
            anim_time += FRAME_DT;
            dirty = true;
        }

        // Redraw only on state change: repainting the full screen every
        // 33ms would wipe any in-progress text selection.
        if dirty {
            write!(screen, "{}", termion::cursor::Goto(1, 1)).unwrap();
            render_animation_frame(&mut screen, theta, anim_time, true);
            write!(screen, "q=quit space=pause click/drag=angle").unwrap();
            screen.flush().unwrap();
            dirty = false;
            frames += 1;
            if frame_count.is_some_and(|n| frames >= n) {
                return;
            }
        }
        thread::sleep(FRAME_DT);
    }
}

fn usage() {
    eprintln!(
        "usage: floating_square_debug <mode>\n\
         modes:\n  \
           pos X Y   single square at world point (X, Y)\n  \
           sweep     offset table for x in 0..=0.5, y in {{0, 0.25, 0.5}}\n  \
           animate   orbiting square; q quits, space pauses, click snaps
                     and pauses, drag scrubs the angle (finer far away).
                     Optional
                     frame count runs a fixed number of frames, which is
                     also the mode used when stdout is not a terminal.\n\
         default: pos 0.3 0.7"
    );
}

fn main() {
    let args: Vec<String> = std::env::args().skip(1).collect();
    match args.first().map(String::as_str) {
        None => show_position(euclid::point2(0.3, 0.7)),
        Some("pos") => {
            let x = args.get(1).and_then(|s| s.parse().ok());
            let y = args.get(2).and_then(|s| s.parse().ok());
            match (x, y) {
                (Some(x), Some(y)) => show_position(euclid::point2(x, y)),
                _ => {
                    usage();
                    std::process::exit(2);
                }
            }
        }
        Some("sweep") => show_sweep(),
        Some("animate") => {
            let frames = args.get(1).and_then(|s| s.parse().ok());
            run_animation(frames)
        }
        Some("--help" | "-h") => usage(),
        Some(other) => {
            eprintln!("unknown mode: {other}");
            usage();
            std::process::exit(2);
        }
    }
}
