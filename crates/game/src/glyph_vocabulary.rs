//! The complete set of characters the game can render, gathered from the real
//! glyph sources rather than a hand-kept list: block families, braille, angled
//! blocks, arrows, move markers, chess pieces, widget digits, starfield and
//! debug overlays. Feeds the `glyph_vocabulary` debug binary and
//! `scripts/glyph-fonts.sh`, which ask the local font stack which font draws
//! each glyph. Debug tooling only (behind the `debug-tools` feature).

use std::collections::BTreeSet;

use strum::IntoEnumIterator;

use terminal_rendering::glyph_constants::named_chars::*;

use crate::game::Widget;
use crate::piece::{Piece, PieceType};

/// Every character the game can draw, sorted by codepoint.
pub fn game_glyph_vocabulary() -> Vec<char> {
    let mut set: BTreeSet<char> = BTreeSet::new();

    // Floating squares / conveyor belts / board blocks: the block families.
    set.extend(terminal_rendering::renderable_block_glyphs());

    // Sight lines, particles, explosions: the whole braille block.
    set.extend((0x2800..=0x28FF).filter_map(char::from_u32));

    // Angled/triangle half-blocks (attacks, lasers), plus their complements.
    for &c in terminal_rendering::angled_block_char_to_snap_points_map().keys() {
        set.insert(c);
        set.insert(terminal_rendering::angle_block_char_complement(c));
    }

    // Arrows (player, projectiles) and move/capture markers.
    set.extend(THICK_ARROWS.chars());
    set.extend(THIN_TRIANGLE_ARROWS.chars());
    for markers in [
        MOVE_ONLY_SQUARE_CHARS,
        CAPTURE_ONLY_SQUARE_CHARS,
        MOVE_AND_CAPTURE_SQUARE_CHARS,
        CONDITIONAL_MOVE_AND_CAPTURE_SQUARE_CHARS,
        KING_PATH_GLYPHS,
    ] {
        set.extend(markers.iter().copied());
    }

    // Pieces and the upgrade marker. Arrow is drawn from THICK_ARROWS (already
    // above), not from a piece glyph, so `chars_for_type` panics for it.
    set.extend(SOLID_CHESS_PIECES.iter().copied());
    for piece_type in PieceType::iter() {
        if piece_type == PieceType::Arrow {
            continue;
        }
        set.extend(Piece::chars_for_type(piece_type));
    }
    set.insert('*');

    // Floor push arrows and the numbered widgets.
    set.extend("⯬⯮⯭⯯".chars());
    for val in 0..=20 {
        set.insert(Widget::new(val).character());
    }

    // Starfield and debug-overlay markers.
    set.extend(['*', '+', '.', '✛', '⌜']);

    set.remove(&' ');
    set.into_iter().collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn vocabulary_covers_the_main_glyph_sources() {
        let vocab = game_glyph_vocabulary();
        assert!(vocab.len() > 400, "suspiciously small vocab: {}", vocab.len());
        // One representative from each source: widget digit, block element,
        // hextant, braille, arrow, chess piece, marker, floor arrow.
        for expected in ['❶', '⓫', '█', '🮇', '⣿', '🢀', '♟', '○', '⯬', '.'] {
            assert!(vocab.contains(&expected), "missing {expected:?}");
        }
        assert!(!vocab.contains(&' '), "space is not a drawn glyph");
    }
}
