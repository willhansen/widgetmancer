use std::env;

use game::do_everything;

fn main() {
    let args: Vec<String> = env::args().skip(1).collect();
    let map_name = match args.as_slice() {
        [] => None,
        [a] if a.starts_with("--map=") => Some(a.trim_start_matches("--map=").to_string()),
        [flag, name] if flag == "--map" => Some(name.clone()),
        _ => {
            eprintln!("Usage: game [--map <demo|racetrack>]");
            return;
        }
    };
    do_everything(map_name);
}