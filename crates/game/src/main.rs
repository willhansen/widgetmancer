use std::env;
use std::path::PathBuf;

use game::{do_everything, FovToggles};

fn usage() {
    eprintln!(
        "Usage: game [--map <demo|racetrack|hallways|numbered-boxes|space-cubes>] [--load <snapshot-dir>]\n\
         \x20      [--no-fov-cache] [--fov-radius <squares>]\n\
         \n\
         FOV performance options (see docs/PERFORMANCE.md):\n\
         \x20 --no-fov-cache         recompute the player FOV every draw\n\
         \x20 --fov-radius <n>       override the player's sight radius (default 16)"
    );
}

fn main() {
    let args: Vec<String> = env::args().skip(1).collect();
    let mut map_name = None;
    let mut load_path = None;
    let mut fov_toggles = FovToggles::default();

    let mut iter = args.iter();
    while let Some(arg) = iter.next() {
        if let Some(name) = arg.strip_prefix("--map=") {
            map_name = Some(name.to_string());
        } else if arg == "--map" {
            match iter.next() {
                Some(name) => map_name = Some(name.clone()),
                None => {
                    usage();
                    return;
                }
            }
        } else if let Some(path) = arg.strip_prefix("--load=") {
            load_path = Some(PathBuf::from(path));
        } else if arg == "--load" {
            match iter.next() {
                Some(path) => load_path = Some(PathBuf::from(path)),
                None => {
                    usage();
                    return;
                }
            }
        } else if arg == "--fov-cache" {
            fov_toggles.cache = true;
        } else if arg == "--no-fov-cache" {
            fov_toggles.cache = false;
        } else if let Some(value) = arg.strip_prefix("--fov-radius=") {
            match value.parse::<u32>() {
                Ok(n) => fov_toggles.radius = Some(n),
                Err(_) => {
                    usage();
                    return;
                }
            }
        } else if arg == "--fov-radius" {
            match iter.next().and_then(|value| value.parse::<u32>().ok()) {
                Some(n) => fov_toggles.radius = Some(n),
                None => {
                    usage();
                    return;
                }
            }
        } else {
            usage();
            return;
        }
    }

    do_everything(map_name, load_path, fov_toggles);
}
