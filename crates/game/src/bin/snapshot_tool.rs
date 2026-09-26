//! Headless snapshot rendering and diffing. Requires the `debug-tools`
//! feature:
//!
//!     cargo run -p game --features debug-tools --bin snapshot_tool -- diff snapshot/
//!
//! Subcommands:
//!   render <dir>          print the headless render of <dir>/game_state.json
//!   diff   <dir> [ref]    compare render against ref (default <dir>/screen.txt)
//!   bless  <dir>          overwrite <dir>/screen.txt with the render
//!   cells  <dir>          print the parsed reference grid as plain characters
//!   fov-trace <dir>       print the player's portal-recursion FOV trace
//!   fov-trace-json <dir>  the same trace as JSON
//!   explain <dir> X Y     explain how screen cell (X,Y) got its glyph
//!   invariants <dir>      FOV visibility-consistency violations
//!   minimize <dir> X Y [out]
//!       --review[=<path>]   write a screen-by-screen review transcript
//!       --review-explain    include the full explain block per step
//!       --review-plain      strip ANSI colors from the transcript
//!       --crop-margin <n>   virtual-screen crop margin in squares (default 4)
//!       --no-screen-crop    disable the virtual-screen crop step

use std::env;
use std::path::{Path, PathBuf};
use std::process::ExitCode;

use game::game::snapshot::debug;

fn usage() -> ExitCode {
    eprintln!(
        "Usage: snapshot_tool <render|diff|bless|cells> <dir> [ref]\n\
         \n\
         Requires the `debug-tools` feature:\n\
         cargo run -p game --features debug-tools --bin snapshot_tool -- <args>"
    );
    ExitCode::from(2)
}

fn main() -> ExitCode {
    let args: Vec<String> = env::args().skip(1).collect();
    let (Some(command), Some(dir)) = (args.first(), args.get(1)) else {
        return usage();
    };
    let dir = PathBuf::from(dir);

    match command.as_str() {
        "render" => match debug::render_snapshot_headless(&dir) {
            Ok(text) => {
                print!("{text}");
                ExitCode::SUCCESS
            }
            Err(error) => {
                eprintln!("error: {error}");
                ExitCode::FAILURE
            }
        },
        "diff" => {
            let ref_path = args
                .get(2)
                .map(PathBuf::from)
                .unwrap_or_else(|| dir.join("screen.txt"));
            match diff_against(&dir, &ref_path) {
                Ok(code) => code,
                Err(error) => {
                    eprintln!("error: {error}");
                    ExitCode::FAILURE
                }
            }
        }
        "bless" => match debug::bless_snapshot(&dir) {
            Ok(path) => {
                eprintln!("blessed {}", path.display());
                ExitCode::SUCCESS
            }
            Err(error) => {
                eprintln!("error: {error}");
                ExitCode::FAILURE
            }
        },
        "cells" => match debug::parse_screen_text_file(&dir.join("screen.txt")) {
            Ok(grid) => {
                for row in &grid {
                    println!("{}", row.iter().map(|c| c.character).collect::<String>());
                }
                ExitCode::SUCCESS
            }
            Err(error) => {
                eprintln!("error: {error}");
                ExitCode::FAILURE
            }
        },
        "fov-trace" => match debug::fov_trace_report(&dir) {
            Ok(report) => {
                print!("{report}");
                ExitCode::SUCCESS
            }
            Err(error) => {
                eprintln!("error: {error}");
                ExitCode::FAILURE
            }
        },
        "fov-trace-json" => match debug::fov_trace_json(&dir) {
            Ok(json) => {
                println!("{json}");
                ExitCode::SUCCESS
            }
            Err(error) => {
                eprintln!("error: {error}");
                ExitCode::FAILURE
            }
        },
        "minimize" => {
            let mut positionals: Vec<&String> = Vec::new();
            let mut options = debug::MinimizeOptions::default();
            let mut review: Option<debug::MinimizeReview> = None;
            let mut i = 2; // args[0] = "minimize", args[1] = dir
            while i < args.len() {
                let arg = &args[i];
                if let Some(path) = arg.strip_prefix("--review=") {
                    review = Some(debug::MinimizeReview {
                        path: Some(PathBuf::from(path)),
                        ..Default::default()
                    });
                } else if arg == "--review" {
                    review = Some(debug::MinimizeReview::default());
                } else if arg == "--review-explain" {
                    review.get_or_insert_with(Default::default).explain = true;
                } else if arg == "--review-plain" {
                    review.get_or_insert_with(Default::default).colors = false;
                } else if arg == "--no-screen-crop" {
                    options.screen_crop = false;
                } else if arg == "--crop-margin" {
                    i += 1;
                    match args.get(i).and_then(|value| value.parse::<u32>().ok()) {
                        Some(margin) => options.crop_margin = margin,
                        None => return usage(),
                    }
                } else if let Some(margin) = arg.strip_prefix("--crop-margin=") {
                    match margin.parse::<u32>() {
                        Ok(margin) => options.crop_margin = margin,
                        Err(_) => return usage(),
                    }
                } else {
                    positionals.push(arg);
                }
                i += 1;
            }

            let (Some(x), Some(y)) = (positionals.first(), positionals.get(1)) else {
                return usage();
            };
            let out_path = positionals
                .get(2)
                .map(PathBuf::from)
                .unwrap_or_else(|| dir.join("minimized.json"));
            match (x.parse::<usize>(), y.parse::<usize>()) {
                (Ok(x), Ok(y)) => {
                    match debug::minimize_snapshot_review(&dir, x, y, &out_path, options, review) {
                        Ok(report) => {
                            println!("{report}");
                            ExitCode::SUCCESS
                        }
                        Err(error) => {
                            eprintln!("error: {error}");
                            ExitCode::FAILURE
                        }
                    }
                }
                _ => usage(),
            }
        }
        "invariants" => match debug::fov_invariants_report(&dir) {
            Ok(report) => {
                print!("{report}");
                ExitCode::SUCCESS
            }
            Err(error) => {
                eprintln!("error: {error}");
                ExitCode::FAILURE
            }
        },
        "explain" => {
            let (Some(x), Some(y)) = (args.get(2), args.get(3)) else {
                return usage();
            };
            match (x.parse::<usize>(), y.parse::<usize>()) {
                (Ok(x), Ok(y)) => match debug::explain_cell(&dir, x, y) {
                    Ok(report) => {
                        print!("{report}");
                        ExitCode::SUCCESS
                    }
                    Err(error) => {
                        eprintln!("error: {error}");
                        ExitCode::FAILURE
                    }
                },
                _ => usage(),
            }
        }
        _ => usage(),
    }
}

fn diff_against(dir: &Path, ref_path: &Path) -> Result<ExitCode, String> {
    let rendered = debug::render_snapshot_headless(dir)?;
    let expected = debug::parse_screen_text_file(ref_path)?;
    let actual = debug::parse_screen_text(&rendered)?;
    if expected == actual {
        println!("OK: {} matches the render", ref_path.display());
        return Ok(ExitCode::SUCCESS);
    }
    let report = debug::render_diff_report(&expected, &actual);
    println!("MISMATCH: {}\n{report}", ref_path.display());
    Ok(ExitCode::FAILURE)
}
