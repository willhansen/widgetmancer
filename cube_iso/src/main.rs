//! `cube_iso` — an interactive visual prototype of four cubes floating in
//! space, rendered with projection P (see `project.rs`).
//!
//! Interactive: arrow keys / WASD to move, `g` cycles the physics scheme,
//! `r` respawns, `q` quits. Headless: `cube_iso --dump out.txt` renders a
//! single frame and writes it, so visuals can be inspected without a TTY.

mod physics;
mod project;
mod render;
mod world;

use physics::{Intent, PhysicsMode, Player};
use render::render_frame;
use std::io::{stdout, Write};
use std::sync::mpsc::{channel, Receiver, TryRecvError};
use std::thread;
use std::time::{Duration, Instant};
use terminal_rendering::Frame;
use termion::cursor::HideCursor;
use termion::event::{Event, Key};
use termion::input::TermRead;
use termion::raw::IntoRawMode;
use termion::screen::IntoAlternateScreen;
use world::World;

const DEFAULT_DUMP_WIDTH: usize = 120;
const DEFAULT_DUMP_HEIGHT: usize = 48;
const FRAME_MS: u64 = 16;

struct Args {
    dump: Option<String>,
    width: Option<usize>,
    height: Option<usize>,
    mode: PhysicsMode,
    scenario: String,
    help: bool,
}

fn parse_args() -> Args {
    let mut args = Args {
        dump: None,
        width: None,
        height: None,
        mode: PhysicsMode::GridMoveGated,
        scenario: "top".to_string(),
        help: false,
    };
    let mut it = std::env::args().skip(1);
    while let Some(arg) = it.next() {
        match arg.as_str() {
            "--dump" => args.dump = it.next(),
            "--width" => args.width = it.next().and_then(|s| s.parse().ok()),
            "--height" => args.height = it.next().and_then(|s| s.parse().ok()),
            "--scenario" => args.scenario = it.next().unwrap_or_else(|| "top".to_string()),
            "--mode" => {
                args.mode = match it.next().as_deref() {
                    Some("smooth") => PhysicsMode::SmoothRealtime,
                    Some("grid") => PhysicsMode::GridRealtimeGravity,
                    Some("gated") => PhysicsMode::GridMoveGated,
                    _ => PhysicsMode::GridMoveGated,
                }
            }
            "-h" | "--help" => args.help = true,
            other => eprintln!("ignoring unknown argument {other:?}"),
        }
    }
    args
}

fn main() {
    let args = parse_args();
    if args.help {
        println!(
            "cube_iso — pseudo-isometric floating-cube prototype\n\n\
             USAGE:\n  cube_iso [--dump FILE] [--width N] [--height N] [--mode smooth|grid|gated] [--scenario top|stairs|fall|east|west]\n\n\
             CONTROLS:\n  arrows / wasd  move\n  g              cycle physics scheme\n  r              respawn\n  q / esc        quit"
        );
        return;
    }
    if let Some(path) = args.dump.clone() {
        dump_frame(&path, &args);
        return;
    }
    if let Err(error) = run_interactive(args.mode, &args.scenario) {
        eprintln!("cube_iso: {error}");
    }
}

fn build_scene(mode: PhysicsMode, scenario: &str) -> (World, Player) {
    let world = World::four_cubes();
    let mut player = Player::new(mode);
    apply_scenario(&world, &mut player, scenario);
    (world, player)
}

/// Jump the player to a named starting situation, mainly so a single headless
/// dump can show the side platforms without recording a whole play session.
fn apply_scenario(world: &World, player: &mut Player, scenario: &str) {
    let intents: Vec<Intent> = match scenario {
        "top" => vec![],
        "stairs" => vec![Intent::South; 7], // off the south edge, down the staircase
        "fall" => vec![Intent::South; 12],  // overshoot the staircase into open space
        "east" => vec![Intent::East; 8],    // off the east face, onto the east ledges
        "west" => vec![Intent::West; 8],    // off the west face, onto the west ledges
        other => {
            eprintln!("unknown scenario {other:?}; using \"top\"");
            vec![]
        }
    };
    for intent in intents {
        player.apply_intent(world, intent);
    }
}

fn dump_frame(path: &str, args: &Args) {
    let width = args.width.unwrap_or(DEFAULT_DUMP_WIDTH);
    let height = args.height.unwrap_or(DEFAULT_DUMP_HEIGHT);
    let (world, player) = build_scene(args.mode, &args.scenario);
    let frame = render_frame(&world, &player, 0.0, width, height);
    std::fs::write(path, frame.string_for_regular_display()).expect("write dump");
    // Also echo a plain (uncolored) view so the shape is readable in a log.
    println!("{}", frame.uncolored_regular_string());
    eprintln!("wrote {path} ({width}x{height})");
}

fn spawn_input() -> Receiver<Event> {
    let (tx, rx) = channel();
    thread::spawn(move || {
        for event in std::io::stdin().events() {
            match event {
                Ok(event) => {
                    if tx.send(event).is_err() {
                        break;
                    }
                }
                Err(_) => break,
            }
        }
    });
    rx
}

fn install_panic_hook() {
    let default_hook = std::panic::take_hook();
    std::panic::set_hook(Box::new(move |info| {
        let mut out = stdout();
        let _ = write!(out, "{}", termion::screen::ToMainScreen);
        let _ = out.flush();
        default_hook(info);
    }));
}

fn run_interactive(initial_mode: PhysicsMode, scenario: &str) -> std::io::Result<()> {
    install_panic_hook();
    let (width, height) = termion::terminal_size()?;
    let (width, height) = (width as usize, height as usize);

    let mut out = HideCursor::from(stdout().into_raw_mode()?).into_alternate_screen()?;
    let events = spawn_input();

    let (world, mut player) = build_scene(initial_mode, scenario);
    let mut previous: Option<Frame> = None;
    let started = Instant::now();
    let mut last = Instant::now();

    loop {
        let mut quit = false;

        loop {
            match events.try_recv() {
                Ok(Event::Key(key)) => {
                    if handle_key(&mut player, &world, key) {
                        quit = true;
                    }
                }
                Ok(_) => {}
                Err(TryRecvError::Empty) => break,
                Err(TryRecvError::Disconnected) => {
                    quit = true;
                    break;
                }
            }
        }
        if quit {
            break;
        }

        let now = Instant::now();
        let dt = (now - last).as_secs_f32().min(0.1);
        last = now;
        player.tick(&world, dt);

        let mut frame = render_frame(
            &world,
            &player,
            started.elapsed().as_secs_f32(),
            width,
            height,
        );
        draw_hud(&mut frame, &player);

        write!(out, "{}", frame.string_for_raw_display_over(&previous))?;
        out.flush()?;
        previous = Some(frame);

        thread::sleep(Duration::from_millis(FRAME_MS));
    }

    Ok(())
}

/// Returns `true` if the key requests a quit.
fn handle_key(player: &mut Player, world: &World, key: Key) -> bool {
    match key {
        Key::Esc | Key::Char('q') => return true,
        Key::Up | Key::Char('w') | Key::Char('k') => player.apply_intent(world, Intent::North),
        Key::Down | Key::Char('s') | Key::Char('j') => player.apply_intent(world, Intent::South),
        Key::Left | Key::Char('a') | Key::Char('h') => player.apply_intent(world, Intent::West),
        Key::Right | Key::Char('d') | Key::Char('l') => player.apply_intent(world, Intent::East),
        Key::Char('g') => {
            let next = match player.mode {
                PhysicsMode::GridMoveGated => PhysicsMode::SmoothRealtime,
                PhysicsMode::SmoothRealtime => PhysicsMode::GridRealtimeGravity,
                PhysicsMode::GridRealtimeGravity => PhysicsMode::GridMoveGated,
            };
            player.set_mode(next);
        }
        Key::Char('r') => player.respawn(),
        _ => {}
    }
    false
}

fn draw_hud(frame: &mut Frame, player: &Player) {
    let status = format!(
        "cube_iso  [{}]   pos ({:.1}, {:.1}, {:.1})",
        player.mode.label(),
        player.x,
        player.y,
        player.z
    );
    frame.draw_text(status, [0, 0]);
    frame.draw_text(
        "arrows/wasd move   g scheme   r respawn   q quit".to_string(),
        [1, 0],
    );
}
