use rgb::RGB8;
use rust_roguelike::glyph::DoubleGlyphFunctions;
use rust_roguelike::graphics::Graphics;
use rust_roguelike::set_up_input_thread;
use rust_roguelike::{glyph::DoubleGlyph, utility::coordinate_frame_conversions::WorldSquare};
use std::io::{stdin, stdout, Write};
use std::thread;
use std::time::{Duration, Instant};
use termion::screen::{IntoAlternateScreen, ToAlternateScreen};
use termion::{
    event::{Event, Key},
    input::{MouseTerminal, TermRead},
    raw::IntoRawMode,
};

struct GameState {
    width: u16,
    height: u16,

    mouse_pos: Option<WorldSquare>,
    last_frame: Option<Frame>,
}
impl GameState {
    pub fn render(&self) -> Frame {
        todo!();
    }
}

struct Frame {
    grid: Vec<Vec<DoubleGlyph>>,
}

impl Frame {
    pub fn diff(&self, old_frame: Frame) -> String {
        let mut output = String::new();

        let rows = self.grid.len();
        let cols = self.grid[0].len();
        let mut prev_written_row_col: Option<[usize; 2]> = None;
        for row in 0..rows {
            for col in 0..cols {
                let new_glyphs = self.grid[row][col].to_string();
                let old_glyphs = old_frame.grid[row][col].to_string();
                let this_square_is_same = new_glyphs == old_glyphs;
                if this_square_is_same {
                    continue;
                }

                let just_next_horizontally = prev_written_row_col
                    .is_some_and(|[prev_row, prev_col]| row == prev_row && prev_col == col - 1);

                let should_do_linewrap = prev_written_row_col.is_some_and(|[prev_row, prev_col]| {
                    prev_row == row - 1 && col == 0 && prev_col == cols - 1
                });
                let directly_below = prev_written_row_col
                    .is_some_and(|[prev_row, prev_col]| prev_row == row - 1 && col == prev_col);

                if just_next_horizontally {
                    // Do nothing
                } else if should_do_linewrap {
                    output += "\n\r";
                } else if directly_below {
                    output += "\n";
                }

                output += &new_glyphs.to_string();

                prev_written_row_col = Some([row, col]);
            }
        }
            output
    }
}

fn main() {
    let (width, height) = termion::terminal_size().unwrap();

    let mut game_state = GameState {
        width,
        height,
        mouse_pos: None,
        last_frame: None,
    };
    let mut graphics = Graphics::new(width, height, Instant::now());

    let mut writable =
        termion::cursor::HideCursor::from(MouseTerminal::from(stdout().into_raw_mode().unwrap()))
            .into_alternate_screen()
            .unwrap();

    let event_receiver = set_up_input_thread();

    loop {
        while let Ok(event) = event_receiver.try_recv() {
            match event {
                Event::Key(key) => match key {
                    Key::Char('q') => std::process::exit(0),
                },
                Event::Mouse(mouse_event) => todo!(),
                Event::Unsupported(items) => todo!(),
            }
        }
        let frame = game_state.render();
        draw_frame(frame, &mut writable);
        thread::sleep(Duration::from_millis(21));
    }
}

fn board_color(square: WorldSquare) -> Option<RGB8> {
    todo!();
}
