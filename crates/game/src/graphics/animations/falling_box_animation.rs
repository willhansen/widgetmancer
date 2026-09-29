use crate::graphics::*;
use std::collections::HashMap;
use std::time::Duration;
use crate::LogicalTime;

/// The boxes shrink through these filled-circle glyphs, largest first. All are
/// `Emoji_Presentation=No` (U+2B24, U+25CF, U+2022, U+00B7): a codepoint whose
/// presentation is Yes (e.g. U+25FE `◾`) is handed to the color-emoji font and
/// renders as a gray emoji square, which is why this must stay text-only.
/// `test_falling_box_glyphs_are_text_presentation` pins that.
pub const FALLING_BOX_GLYPHS: [char; 4] = ['⬤', '●', '•', '·'];

/// A numbered box shoved off the board edge: at the off-board square it would
/// have landed on, a filled circle steps down through
/// [`FALLING_BOX_GLYPHS`] and fades toward black, so it reads as dropping away
/// into the void. Off-board (not the edge square) so the box behind it can
/// slide into the edge square without overlap.
#[derive(Clone, PartialEq, Debug, Copy)]
pub struct FallingBoxAnimation {
    square: WorldSquare,
    start_time: LogicalTime,
}

impl FallingBoxAnimation {
    pub fn new(square: WorldSquare) -> FallingBoxAnimation {
        FallingBoxAnimation {
            square,
            start_time: LogicalTime::ZERO,
        }
    }
}

impl Animation for FallingBoxAnimation {
    fn start_time(&self) -> LogicalTime {
        self.start_time
    }
    fn set_start_time(&mut self, time: LogicalTime) {
        self.start_time = time;
    }
    fn duration(&self) -> Duration {
        Duration::from_millis(600)
    }

    fn double_glyphs_at_time(&self, time: LogicalTime) -> HashMap<WorldSquare, DoubleGlyph> {
        let done = self.fraction_done_at_time(time);
        let step = ((done * FALLING_BOX_GLYPHS.len() as f32) as usize)
            .min(FALLING_BOX_GLYPHS.len() - 1);
        let color = lerp_rgb8(FALLING_BOX_COLOR, BLACK, done);
        let glyphs: DoubleGlyph = [
            Glyph::fg_only(FALLING_BOX_GLYPHS[step], color),
            Glyph::transparent_glyph(),
        ];
        HashMap::from([(self.square, glyphs)])
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_falling_box_glyphs_are_text_presentation() {
        for c in FALLING_BOX_GLYPHS {
            assert!(
                !char_is_emoji_presentation(c),
                "{c:?} defaults to emoji presentation and would render as a \
                 color-emoji square"
            );
        }
    }

    #[test]
    fn test_falling_box_shrinks_through_the_glyph_sequence() {
        let animation = FallingBoxAnimation::new(WorldSquare::new(3, 4));
        let start = animation.start_time();
        let glyph_at = |millis: u64| {
            animation.double_glyphs_at_time(start + Duration::from_millis(millis))
                [&animation.square][0]
                .character
        };
        assert_eq!(glyph_at(0), FALLING_BOX_GLYPHS[0]);
        assert_eq!(glyph_at(200), FALLING_BOX_GLYPHS[1]);
        assert_eq!(glyph_at(350), FALLING_BOX_GLYPHS[2]);
        assert_eq!(glyph_at(500), FALLING_BOX_GLYPHS[3]);
    }
}
