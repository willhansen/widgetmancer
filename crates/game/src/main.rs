use std::env;
use std::path::PathBuf;

use game::do_everything;

fn usage() {
    eprintln!(
        "Usage: game [--map <demo|racetrack|hallways>] [--load <snapshot-dir>]"
    );
}

fn main() {
    let args: Vec<String> = env::args().skip(1).collect();
    let mut map_name = None;
    let mut load_path = None;

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
        } else {
            usage();
            return;
        }
    }

    do_everything(map_name, load_path);
}