//#![allow(non_snake_case)]
#![allow(warnings)]
#![allow(non_snake_case)]

extern crate approx;
extern crate core;
extern crate line_drawing;
extern crate num;
extern crate shrinkwraprs;
extern crate std;
extern crate termion;

use std::io::{stdin, stdout, Write};
use std::sync::mpsc::{channel, Receiver, Sender};
use std::thread;
use std::time::{Duration, Instant};

use enum_as_inner::EnumAsInner;
use euclid::default::Point2D;
use euclid::point2;
use rand::SeedableRng;
use termion::event::{Event, Key, MouseButton, MouseEvent};
use termion::input::{MouseTerminal, TermRead};
use termion::raw::{IntoRawMode, RawTerminal};
use termion::screen::{IntoAlternateScreen, ToAlternateScreen};

use terminal_rendering::glyph::*;
use utility::utility::*;

use crate::game::Game;
use crate::graphics::Graphics;
use crate::inputmap::InputMap;
use crate::piece::{Piece, PieceType};

pub mod animations;
mod fov_stuff;
pub mod game;
pub mod graphics;
mod inputmap;
pub mod piece;
pub mod portal_geometry;
pub mod utils_for_tests;


fn set_up_panic_hook() {
    std::panic::set_hook(Box::new(move |panic_info| {
        stdout().flush().expect("flush stdout");
        write!(stdout(), "{}", termion::screen::ToMainScreen).expect("switch to main screen");
        write!(stdout(), "{:?}", panic_info).expect("display panic info");
    }));
}

pub fn set_up_input_thread() -> Receiver<Event> {
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
    let (width, height) = termion::terminal_size().unwrap();
    //let (width, height) = (40, 20);
    let mut game = Game::new(width, height, Instant::now());
    game.place_player(point2(width as i32 / 4, height as i32 / 2));
    let mut input_map = InputMap::new(width, height);
    //let mut game = init_platformer_test_world(width, height);

    let mut writable =
        termion::cursor::HideCursor::from(MouseTerminal::from(stdout().into_raw_mode().unwrap()))
            .into_alternate_screen()
            .unwrap();

    set_up_panic_hook();

    // Separate thread for reading input
    let event_receiver = set_up_input_thread();

    let mut wrapped_terminal: &mut Option<Box<dyn Write>> = &mut Some(Box::new(writable));

    //let pawn_pos = game.player_position() + LEFT_I.cast_unit() * 3; game.place_piece(Piece::pawn(), pawn_pos) .expect("Failed to place pawn");

    let mut rng = rand::rngs::StdRng::seed_from_u64(5);
    //game.set_up_test_map();
    //game.set_up_labyrinth_hunt();
    //game.set_up_labyrinth_kings();
    //game.set_up_labyrinth(&mut rng);
    // game.set_up_columns();
    // game.set_up_simple_portal_map();
    game.set_up_demo_map();
    // game.set_up_portal_across_wall_map(2, 0);
    // game.set_up_simple_freestanding_portal();
    // game.place_dense_horizontal_portals(
    //     game.player_square() + STEP_RIGHT * 10 + STEP_DOWN_RIGHT * 5,
    //     3,
    //     6,
    // );
    // game.set_up_vs_mini_factions();
    // game.set_up_vs_red_pawns();
    //game.set_up_upgrades_galore();
    //game.set_up_homogeneous_army(PieceType::OmniDirectionalSoldier);
    // game.set_up_vs_weak_with_pillars_and_turret_and_upgrades();
    //game.set_up_vs_arrows();

    let mut prev_tick_start_time = Instant::now();
    while game.running() {
        let tick_start_time = Instant::now();
        let delta = tick_start_time - prev_tick_start_time;
        prev_tick_start_time = tick_start_time;
        //let prev_tick_duration_ms = start_time.duration_since(prev_start_time).as_millis();
        //let prev_tick_duration_s: f32 = prev_tick_duration_ms as f32 / 1000.0;

        while let Ok(event) = event_receiver.try_recv() {
            input_map.handle_event(&mut game, event);

            game.tick_game_logic();
        }
        game.tick_realtime_effects(delta);
        game.draw(&mut wrapped_terminal, Instant::now());
        thread::sleep(Duration::from_millis(21));
    }
}
