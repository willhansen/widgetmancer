//#![allow(non_snake_case)]
#![allow(non_snake_case)]

extern crate approx;
extern crate core;
extern crate line_drawing;
extern crate num;
extern crate shrinkwraprs;
extern crate std;
extern crate termion;

use std::io::{stdin, stdout, Write};
use std::path::PathBuf;
use std::sync::mpsc::{channel, Receiver, Sender};
use std::thread;
use std::time::{Duration, Instant};

use euclid::point2;
use rand::SeedableRng;
use termion::event::Event;
use termion::input::{MouseTerminal, TermRead};
use termion::raw::IntoRawMode;
use termion::screen::IntoAlternateScreen;

use terminal_rendering::glyph::*;
use utility::*;

use crate::game::Game;
use crate::game::snapshot::{load_snapshot_game, write_snapshot, InputRecord, SNAPSHOT_KEY};
use crate::inputmap::InputMap;
use crate::piece::PieceType;

pub mod fov_stuff;
pub mod game;
pub mod graphics;
mod inputmap;
pub mod logical_time;
pub mod piece;
pub mod portal_geometry;
pub mod utils_for_tests;

pub use logical_time::LogicalTime;

fn set_up_panic_hook() {
    std::panic::set_hook(Box::new(move |panic_info| {
        stdout().flush().expect("flush stdout");
        write!(stdout(), "{}", termion::screen::ToMainScreen).expect("switch to main screen");
        write!(stdout(), "{:?}", panic_info).expect("display panic info");
    }));
}

pub fn set_up_input_thread_given_sender(sender: Sender<(Instant, Event)>) {
    thread::spawn(move || {
        for c in stdin().events() {
            let evt = c.unwrap();
            sender.send((Instant::now(), evt)).unwrap();
        }
    });
}

pub fn set_up_input_thread() -> Receiver<(Instant, Event)> {
    let (tx, rx) = channel();
    set_up_input_thread_given_sender(tx);
    return rx;
}

pub fn set_up_map_by_name(game: &mut Game, map_name: Option<&str>) {
    match map_name {
        None | Some("demo") => game.set_up_demo_map(),
        Some("racetrack") => game.set_up_portal_cube_racetrack_map(),
        Some("hallways") => game.set_up_portal_pair_hallways_map(),
        Some("numbered-boxes") => game.set_up_numbered_boxes_map(),
        Some(unknown) => {
            panic!("Unknown map '{unknown}'. Known maps: demo, racetrack, hallways, numbered-boxes.")
        }
    }
}

/// FOV performance toggles (see docs/PERFORMANCE.md). The cache is on by
/// default; `radius` optionally overrides the player's sight radius.
/// The game binary exposes `--no-fov-cache` / `--fov-radius <n>`.
#[derive(Clone, Copy, Debug)]
pub struct FovToggles {
    /// Cache the player FOV per player square.
    pub cache: bool,
    /// Override the player's sight radius in squares.
    pub radius: Option<u32>,
}

impl Default for FovToggles {
    fn default() -> Self {
        FovToggles {
            cache: true,
            radius: None,
        }
    }
}

pub fn do_everything(
    map_name: Option<String>,
    load_path: Option<PathBuf>,
    fov_toggles: FovToggles,
) {
    let (width, height) = termion::terminal_size().unwrap();
    //let (width, height) = (40, 20);
    // The racetrack map's exhibits span ~30x19 squares around the player,
    // and the board is half the terminal width in squares, so that map
    // needs at least a 96x26-character terminal. The numbered-boxes map is a
    // fixed 20x20 board, needing at least 40x20 characters.
    let (width, height) = match map_name.as_deref() {
        Some("racetrack") => (width.max(96), height.max(26)),
        Some("numbered-boxes") => (width.max(40), height.max(20)),
        _ => (width, height),
    };

    let mut game = match &load_path {
        Some(dir) => match load_snapshot_game(dir) {
            Ok(game) => game,
            Err(error) => {
                eprintln!("Could not load snapshot: {error}");
                return;
            }
        },
        None => {
            let mut game = Game::new(width, height, LogicalTime::ZERO);
            game.place_player(point2(width as i32 / 4, height as i32 / 2));
            set_up_map_by_name(&mut game, map_name.as_deref());
            game
        }
    };
    game.set_fov_cache_enabled(fov_toggles.cache);
    if let Some(radius) = fov_toggles.radius {
        game.set_player_sight_radius(radius);
    }
    let mut input_map = InputMap::new(width, height);
    //let mut game = init_platformer_test_world(width, height);

    let writable =
        termion::cursor::HideCursor::from(MouseTerminal::from(stdout().into_raw_mode().unwrap()))
            .into_alternate_screen()
            .unwrap();

    set_up_panic_hook();

    // Separate thread for reading input
    let event_receiver = set_up_input_thread();

    let mut wrapped_terminal: &mut Option<Box<dyn Write>> = &mut Some(Box::new(writable));

    //let pawn_pos = game.player_position() + LEFT_I.cast_unit() * 3; game.place_piece(Piece::pawn(), pawn_pos) .expect("Failed to place pawn");

    let _rng = rand::rngs::StdRng::seed_from_u64(5);
    //game.set_up_test_map();
    //game.set_up_labyrinth_hunt();
    //game.set_up_labyrinth_kings();
    //game.set_up_labyrinth(&mut rng);
    // game.set_up_columns();
    // game.set_up_simple_portal_map();
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

    // The one place real time enters the simulation. Everything downstream
    // consumes `LogicalTime` derived from this epoch (roadmap W.A).
    let epoch = Instant::now();
    let mut prev_logical_time = LogicalTime::ZERO;
    let mut input_history: Vec<InputRecord> = Vec::new();
    let mut snapshot_requested = false;
    while game.running() {
        let logical_now = LogicalTime::from_duration(epoch.elapsed());
        let delta = logical_now.saturating_duration_since(prev_logical_time);
        prev_logical_time = logical_now;
        // Stamp animations spawned while handling this iteration's input.
        game.borrow_graphics_mut().set_current_time(logical_now);

        while let Ok((event_time, event)) = event_receiver.try_recv() {
            input_history.push(InputRecord {
                millis_from_start: event_time.duration_since(epoch).as_millis(),
                event: event.clone(),
            });
            if event == Event::Key(SNAPSHOT_KEY) {
                snapshot_requested = true;
            }

            input_map.handle_event(&mut game, event);

            game.tick_game_logic();
        }
        game.tick_realtime_effects(delta);
        game.draw(&mut wrapped_terminal, logical_now);

        // Dump after drawing so the snapshot captures the frame the player saw.
        if snapshot_requested {
            snapshot_requested = false;
            write_snapshot(&game, &input_history, map_name.as_deref());
        }

        thread::sleep(Duration::from_millis(21));
    }
}
