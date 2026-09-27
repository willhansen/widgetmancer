//! Headless racetrack profiling harness. Reproduces the real game loop
//! (`tick_realtime_effects` + a full headless draw per frame) so a profiler
//! can attribute the per-frame cost, and can time the player FOV alone.
//! Not a test; run under uftrace/gprof.
//!
//! Prototype toggles (docs/PERFORMANCE.md):
//!   --cache            prototype A: per-player-square FOV cache
//!   --budget=<squares> prototype B: cumulative portal-distance budget
//!
//! Usage:
//!   cargo run --release -p game --example profile_racetrack -- [flags] [map] [frames]
//!   cargo run --release -p game --example profile_racetrack -- fov [flags] [map] [iters]

use std::time::{Duration, Instant};

use euclid::point2;

use game::game::Game;
use game::set_up_map_by_name;
use game::LogicalTime;
use utility::coordinate_frame_conversions::WorldSquare;

struct Toggles {
    cache: bool,
    budget: Option<f32>,
}

fn parse_args() -> (String, Vec<String>, Toggles) {
    let mut toggles = Toggles {
        cache: false,
        budget: None,
    };
    let mut positional = Vec::new();
    for arg in std::env::args().skip(1) {
        if arg == "--cache" {
            toggles.cache = true;
        } else if let Some(value) = arg.strip_prefix("--budget=") {
            let parsed: f32 = value.parse().expect("--budget=<squares>");
            toggles.budget = (parsed > 0.0).then_some(parsed);
        } else {
            positional.push(arg);
        }
    }
    let mode = positional
        .first()
        .cloned()
        .unwrap_or_else(|| "frames".to_string());
    (mode, positional, toggles)
}

fn main() {
    let (mode, positional, toggles) = parse_args();

    if mode == "vis" {
        let map = positional
            .get(1)
            .cloned()
            .unwrap_or_else(|| "racetrack".to_string());
        let game = game_for(&map, &toggles);
        let p = game.player_square();
        let (mut full, mut partial, mut invisible) = (0u32, 0u32, 0u32);
        for dx in (-20..=20).step_by(4) {
            for dy in (-12..=12).step_by(4) {
                let square = WorldSquare::new(p.x + dx, p.y + dy);
                if game.square_is_not_visible_to_player(square) {
                    invisible += 1;
                } else if game.square_is_fully_visible_to_player(square) {
                    full += 1;
                } else {
                    partial += 1;
                }
            }
        }
        eprintln!(
            "{map}{}: full={full} partial={partial} invisible={invisible}",
            toggles_label(&toggles)
        );
        return;
    }

    if mode == "fov" {
        let map = positional
            .get(1)
            .cloned()
            .unwrap_or_else(|| "racetrack".to_string());
        let iters: usize = positional
            .get(2)
            .and_then(|s| s.parse().ok())
            .unwrap_or(200);
        let game = game_for(&map, &toggles);
        // A single square near the player; each call recomputes the whole
        // portal-recursive player FOV from scratch.
        let probe = WorldSquare::new(game.player_square().x + 1, game.player_square().y);
        let start = Instant::now();
        let mut hits = 0u32;
        for _ in 0..iters {
            if game.square_is_fully_visible_to_player(probe) {
                hits += 1;
            }
        }
        let elapsed = start.elapsed();
        eprintln!(
            "{map}{}: {iters} FOV computations in {elapsed:?} ({:?}/fov, {hits} visible)",
            toggles_label(&toggles),
            elapsed / iters as u32
        );
        return;
    }

    let map = mode;
    let frames: usize = positional
        .get(1)
        .and_then(|s| s.parse().ok())
        .unwrap_or(2000);
    let mut game = game_for(&map, &toggles);

    let frame = Duration::from_millis(21);
    let start = Instant::now();
    for _ in 0..frames {
        game.tick_realtime_effects(frame);
        game.draw_headless_now();
    }
    let elapsed = start.elapsed();
    eprintln!(
        "{map}{}: {frames} frames in {elapsed:?} ({:?}/frame)",
        toggles_label(&toggles),
        elapsed / frames as u32
    );
}

fn toggles_label(toggles: &Toggles) -> String {
    match (toggles.cache, toggles.budget) {
        (false, None) => String::new(),
        (cache, budget) => {
            let mut parts = Vec::new();
            if cache {
                parts.push("cache".to_string());
            }
            if let Some(b) = budget {
                parts.push(format!("budget={b}"));
            }
            format!(" [{}]", parts.join(","))
        }
    }
}

fn game_for(map: &str, toggles: &Toggles) -> Game {
    let mut game = Game::new(96, 26, LogicalTime::ZERO);
    game.place_player(point2(24, 13));
    set_up_map_by_name(&mut game, Some(map));
    game.set_fov_cache_enabled(toggles.cache);
    game.set_fov_cumulative_radius(toggles.budget);
    game
}
