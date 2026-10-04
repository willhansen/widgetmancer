//! Print glyphs as `U+XXXX <char>` lines, one per line.
//!
//! With no arguments: every glyph the game can render, gathered from the real
//! render sources. With arguments: the given characters (literal runs, or
//! `U+XXXX` / `0xXXXX` code points). Feeds `scripts/glyph-fonts.sh`, which asks
//! the local font stack which font draws each glyph. Requires `debug-tools`.

use game::glyph_vocabulary::game_glyph_vocabulary;

fn emit(c: char) {
    println!("U+{:04X} {}", c as u32, c);
}

fn main() {
    let args: Vec<String> = std::env::args().skip(1).collect();
    if args.is_empty() {
        for c in game_glyph_vocabulary() {
            emit(c);
        }
        return;
    }
    for arg in &args {
        if let Some(hex) = arg.strip_prefix("U+").or_else(|| arg.strip_prefix("0x")) {
            match u32::from_str_radix(hex, 16)
                .ok()
                .and_then(char::from_u32)
            {
                Some(c) => emit(c),
                None => eprintln!("not a scalar value: {arg}"),
            }
        } else {
            for c in arg.chars() {
                emit(c);
            }
        }
    }
}
