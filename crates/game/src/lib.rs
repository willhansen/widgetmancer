//#![allow(non_snake_case)]
#![allow(non_snake_case)]

extern crate approx;
extern crate core;
extern crate line_drawing;
extern crate num;
extern crate shrinkwraprs;
extern crate std;
extern crate termion;

use std::io::{stdin, stdout, Read, Write};
use std::os::unix::io::{AsRawFd, RawFd};
use std::path::PathBuf;
use std::process::Command;
use std::sync::mpsc::{channel, Receiver, Sender};
use std::sync::{Arc, Condvar, Mutex};
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
use crate::game::snapshot::{
    create_new_issue, load_snapshot_game, write_snapshot_to, InputRecord, SNAPSHOT_KEY,
};
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

/// The complete glyph vocabulary, for font-fallback tooling (debug-tools only).
#[cfg(feature = "debug-tools")]
pub mod glyph_vocabulary;

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

/// Shared on/off switch between the driver loop and the input reader. The
/// reader parks while `paused`, which is how the terminal can be handed to an
/// external editor without the two splitting the user's keystrokes.
#[derive(Default)]
struct PauseInner {
    paused: bool,
    parked: bool,
}

/// Main-thread handle used to park and release the input reader.
pub struct InputPause {
    inner: Arc<(Mutex<PauseInner>, Condvar)>,
}

impl InputPause {
    fn new() -> Self {
        InputPause {
            inner: Arc::new((Mutex::new(PauseInner::default()), Condvar::new())),
        }
    }

    /// Request a pause and block until the reader confirms it is parked, so no
    /// input is consumed until [`InputPause::resume`].
    pub fn pause(&self) {
        let (lock, cv) = &*self.inner;
        let mut inner = lock.lock().unwrap();
        inner.paused = true;
        while !inner.parked {
            inner = cv.wait(inner).unwrap();
        }
    }

    /// Release the reader to consume input again.
    pub fn resume(&self) {
        let (lock, cv) = &*self.inner;
        let mut inner = lock.lock().unwrap();
        inner.paused = false;
        cv.notify_all();
    }
}

/// A `Read` over stdin that parks while paused instead of blocking forever in
/// `read`. The short `poll` timeout bounds how long [`InputPause::pause`] waits
/// for the reader to notice the request.
struct PausableInput {
    inner: Arc<(Mutex<PauseInner>, Condvar)>,
    fd: RawFd,
}

impl Read for PausableInput {
    fn read(&mut self, buf: &mut [u8]) -> std::io::Result<usize> {
        let (lock, cv) = &*self.inner;
        loop {
            {
                let mut inner = lock.lock().unwrap();
                if inner.paused {
                    inner.parked = true;
                    cv.notify_all();
                    while inner.paused {
                        inner = cv.wait(inner).unwrap();
                    }
                    inner.parked = false;
                    continue;
                }
            }

            let mut poll_fd = libc::pollfd {
                fd: self.fd,
                events: libc::POLLIN,
                revents: 0,
            };
            let ready = unsafe { libc::poll(&mut poll_fd, 1, 50) };
            if ready < 0 {
                let error = std::io::Error::last_os_error();
                if error.kind() == std::io::ErrorKind::Interrupted {
                    continue;
                }
                return Err(error);
            }
            if ready == 0 {
                continue;
            }

            // Re-check just before consuming: a byte that arrived together with
            // the pause request is left in the tty buffer for the editor.
            if lock.lock().unwrap().paused {
                continue;
            }
            // Read straight from the fd, not through the buffered `stdin()`.
            // `poll` watches the kernel buffer, but `Stdin`'s `BufReader` can
            // hold bytes already drained from it; termion asks for the tail of a
            // multi-byte key on a second read, so a buffered read here would
            // make `poll` report "not ready" while the needed byte sits in the
            // `BufReader`, delaying every escape sequence by a keypress.
            let n = unsafe { libc::read(self.fd, buf.as_mut_ptr().cast(), buf.len()) };
            if n < 0 {
                let error = std::io::Error::last_os_error();
                if error.kind() == std::io::ErrorKind::Interrupted {
                    continue;
                }
                return Err(error);
            }
            return Ok(n as usize);
        }
    }
}

pub fn set_up_input_thread() -> (Receiver<(Instant, Event)>, InputPause) {
    let (tx, rx) = channel();
    let pause = InputPause::new();
    let reader = PausableInput {
        inner: pause.inner.clone(),
        fd: stdin().as_raw_fd(),
    };
    thread::spawn(move || {
        for c in reader.events() {
            let evt = c.unwrap();
            tx.send((Instant::now(), evt)).unwrap();
        }
    });
    (rx, pause)
}

/// Hand the terminal to the user's editor for `note_path`, then recreate the
/// game terminal and release the input reader. Blocks until the editor exits.
fn edit_issue_note(
    note_path: &std::path::Path,
    terminal: &mut Option<Box<dyn Write>>,
    input_pause: &InputPause,
) {
    input_pause.pause();
    // Dropping the raw-mode/alternate-screen writer restores the terminal.
    *terminal = None;
    let _ = stdout().flush();

    let editor = std::env::var("VISUAL")
        .ok()
        .filter(|value| !value.trim().is_empty())
        .or_else(|| {
            std::env::var("EDITOR")
                .ok()
                .filter(|value| !value.trim().is_empty())
        })
        .unwrap_or_else(|| "vi".to_string());
    let mut parts = editor.split_whitespace();
    let program = parts.next().unwrap_or("vi");
    match Command::new(program).args(parts).arg(note_path).status() {
        Ok(status) if !status.success() => eprintln!(
            "Editor exited with {status}; note left at {}",
            note_path.display()
        ),
        Ok(_) => {}
        Err(error) => eprintln!(
            "Could not launch editor '{editor}': {error}; note left at {}",
            note_path.display()
        ),
    }

    // Recreate the game terminal exactly as at startup.
    let restored =
        termion::cursor::HideCursor::from(MouseTerminal::from(stdout().into_raw_mode().unwrap()))
            .into_alternate_screen()
            .unwrap();
    *terminal = Some(Box::new(restored));
    let _ = stdout().flush();
    input_pause.resume();
}

pub fn set_up_map_by_name(game: &mut Game, map_name: Option<&str>) {
    // A `maps/<name>.json` recipe takes precedence; maps own their board size
    // and are independent of the terminal.
    if let Some(name) = map_name {
        if let Some(map) = game::map_file::load_map_file(name) {
            game.apply_map_file(&map);
            return;
        }
    }
    match map_name {
        None | Some("demo") => game.set_up_demo_map(),
        Some("racetrack") => game.set_up_portal_cube_racetrack_map(),
        Some("hallways") => game.set_up_portal_pair_hallways_map(),
        Some("numbered-boxes") => game.set_up_numbered_boxes_map(),
        Some(unknown) => {
            panic!("Unknown map '{unknown}'. Known maps: demo, racetrack, hallways, numbered-boxes, space-cubes.")
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
            // Maps set their own board size and player start; this provisional
            // placement only matters for map-less use.
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

    // Separate thread for reading input; it can be parked while an external
    // editor owns the terminal.
    let (event_receiver, input_pause) = set_up_input_thread();

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
    let mut epoch = Instant::now();
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

        // Dump after drawing so the snapshot captures the frame the player saw,
        // then file it as a new numbered issue with a typed description.
        if snapshot_requested {
            snapshot_requested = false;
            match create_new_issue(map_name.as_deref()) {
                Ok(issue_dir) => {
                    write_snapshot_to(
                        &issue_dir.join("snapshot"),
                        &game,
                        &input_history,
                        map_name.as_deref(),
                    );
                    let editor_start = Instant::now();
                    edit_issue_note(
                        &issue_dir.join("issue.md"),
                        &mut *wrapped_terminal,
                        &input_pause,
                    );
                    // Treat the suspended wall time as paused, not as one giant
                    // tick, so the world clock and animations stay continuous.
                    epoch += editor_start.elapsed();
                    prev_logical_time = LogicalTime::from_duration(epoch.elapsed());
                    game.borrow_graphics_mut().screen.force_redraw();
                }
                Err(error) => eprintln!("{error}"),
            }
        }

        thread::sleep(Duration::from_millis(21));
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use termion::input::TermRead;

    #[test]
    fn escape_sequence_tails_are_delivered_without_a_followup_key() {
        // A pipe stands in for the tty. Termion reads a multi-byte key like Up
        // in two steps (2 bytes, then the trailing byte); all three must come
        // back without another key. Regression for the buffered-`stdin` read
        // that parked the trailing byte in `BufReader` while `poll` watched the
        // (now empty) kernel buffer, delaying every arrow by one keypress.
        let mut fds = [0; 2];
        assert_eq!(unsafe { libc::pipe(fds.as_mut_ptr()) }, 0, "pipe");
        let (read_fd, write_fd) = (fds[0], fds[1]);

        let written = unsafe { libc::write(write_fd, b"\x1b[A".as_ptr().cast(), 3) };
        assert_eq!(written, 3, "write full Up sequence");

        let reader = PausableInput {
            inner: Arc::new((Mutex::new(PauseInner::default()), Condvar::new())),
            fd: read_fd,
        };
        let (tx, rx) = channel();
        thread::spawn(move || {
            let event = reader.events().next().expect("an event");
            let _ = tx.send(format!("{event:?}"));
        });

        let received = rx
            .recv_timeout(Duration::from_secs(2))
            .expect("trailing byte must not wait for a follow-up keypress");
        assert_eq!(received, "Ok(Key(Up))");

        unsafe {
            libc::close(read_fd);
            libc::close(write_fd);
        }
    }
}
