//#![allow(non_snake_case)]
#![feature(is_sorted)]
#![feature(drain_filter)]
#![allow(warnings)]

#[macro_use]
extern crate approx;
extern crate line_drawing;
extern crate num;
extern crate std;
extern crate termion;

use std::char;
use std::cmp::{max, min};
use std::collections::hash_map::Entry;
use std::collections::{HashMap, VecDeque};
use std::fmt::Debug;
use std::io::{stdin, stdout, Write};
use std::sync::mpsc::{channel, Receiver, Sender};
use std::thread;
use std::time::{Duration, Instant};

use enum_as_inner::EnumAsInner;
use euclid::default::Point2D;
use ntest::timeout;
use num::Integer;
use strum::IntoEnumIterator;
use strum_macros::EnumIter;
use termion::event::{Event, Key, MouseButton, MouseEvent};
use termion::input::{MouseTerminal, TermRead};
use termion::raw::{IntoRawMode, RawTerminal};

use glyph::*;
use utility::*;

use crate::game::Game;
use crate::graphics::Graphics;
use crate::inputmap::InputMap;
use crate::piece::{Piece, PieceType};

pub mod game;
pub mod glyph;
mod graphics;
mod inputmap;
pub mod piece;
pub mod utility;

fn set_up_panic_hook() {
    std::panic::set_hook(Box::new(move |panic_info| {
        write!(stdout(), "{}", termion::screen::ToMainScreen);
        write!(stdout(), "{:?}", panic_info);
    }));
}

fn set_up_input_thread() -> Receiver<Event> {
    let (tx, rx) = channel();
    thread::spawn(move || {
        for c in stdin().events() {
            let evt = c.unwrap();
            tx.send(evt).unwrap();
        }
    });
    return rx;
}

pub fn do_everything() {
    //let (width, height) = termion::terminal_size().unwrap();
    let (width, height) = (80, 40);
    let mut game = Game::new(width, height);
    let mut input_map = InputMap::new(width, height);
    //let mut game = init_platformer_test_world(width, height);

    let mut terminal = termion::screen::AlternateScreen::from(termion::cursor::HideCursor::from(
        MouseTerminal::from(stdout().into_raw_mode().unwrap()),
    ));

    set_up_panic_hook();

    // Separate thread for reading input
    let event_receiver = set_up_input_thread();

    let mut wrapped_terminal: &mut Option<Box<dyn Write>> = &mut Some(Box::new(terminal));

    let pawn_pos = game.player_position() + LEFT_I.cast_unit() * 3;
    game.place_piece(Piece::pawn(), pawn_pos)
        .expect("Failed to place pawn");

    let mut prev_start_time = Instant::now();
    while game.running {
        //let start_time = Instant::now();
        //let prev_tick_duration_ms = start_time.duration_since(prev_start_time).as_millis();
        //let prev_tick_duration_s: f32 = prev_tick_duration_ms as f32 / 1000.0;
        //prev_start_time = start_time;

        while let Ok(event) = event_receiver.try_recv() {
            input_map.handle_event(&mut game, event);
            game.move_all_pieces();
        }
        let delta = Duration::from_millis(100);
        game.draw(&mut wrapped_terminal, delta);
        thread::sleep(delta);
    }
}
