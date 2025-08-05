use std::iter::once;

use rgb::{RGB8, RGBA8};

use crate::Glyph;

#[derive(Clone, Copy, Hash, Debug, PartialEq)]
pub struct GlyphWithTransparency {
    character: char,
    primary_color: RGBA8,
    secondary_color: RGBA8,
    // For when you and unicode disagree on the foreground and background parts of a character
    // Primarily used when converting to a non-transparent Glyph
    fg_is_primary: bool,
}

impl GlyphWithTransparency {
    pub fn from_glyph(glyph: Glyph) -> Self {
        Self {
            character: glyph.character,
            primary_color: glyph.fg_color.with_alpha(255),
            secondary_color: glyph.bg_color.with_alpha(255),
            fg_is_primary: true,
        }
    }
    pub fn from_char(c: char) -> Self {
        Self::from_glyph(Glyph::from_char(c))
    }
    pub fn with_primary_only(&self) -> Self {
        let mut x = self.clone();
        x.secondary_color.a = 0;
        x
    }
    pub fn with_primary_rgb(&self, color: RGB8) -> Self {
        let mut x = self.clone();
        *x.primary_color.rgb_mut() = color;
        x
    }
    pub fn with_secondary_rgb(&self, color: RGB8) -> Self {
        let mut x = self.clone();
        *x.secondary_color.rgb_mut() = color;
        x
    }
    pub fn with_rgbs(&self, primary_rgb: RGB8, secondary_rgb: RGB8) -> Self {
        self.with_primary_rgb(primary_rgb).with_secondary_rgb(secondary_rgb)
    }
    pub fn over(&self, other: Self) -> Self {
        let mut out = self.clone();
        out.primary_color = color_combine(self.primary_color, other.primary_color);
        out.secondary_color = color_combine(self.secondary_color, other.primary_color);
        out
    }
}
// ref: https://en.wikipedia.org/wiki/Alpha_compositing
pub fn color_combine(above: RGBA8, below: RGBA8) -> RGBA8 {
    let a = above;
    let b = below;
    let a_a = a.a as f32 / 255.0;
    let b_a = b.a as f32 / 255.0;
    let c_a = a_a + (1.0 - a_a) * b_a;
    RGBA8::from_iter(a.rgb()
        .iter()
        .zip(b.rgb().iter())
        .map(|(a, b)| (a as f32 * a_a + b as f32 * (1.0 - a_a) * b_a) / c_a)
        .chain(once(c_a * 255.0))
        .map(|f| f as u8))
        
}
#[cfg(test)]
mod tests {
    use ntest::assert_false;

    use crate::glyph_constants::named_chars::*;
    use crate::glyph_constants::named_colors::*;

    use super::*;

    #[test]
    fn test_alpha_composite() {
        let a = RGBA8::from(RED).with_alpha(0);
        let b = BLUE.into();
        assert_eq!(color_combine(a, b), b);


    }

    #[test]
    fn test_glyph_to_aglyph() {
        let glyph = Glyph::from_char('a');
        let b = GlyphWithTransparency::from_glyph(glyph);
    }
    #[test]
    fn test_letter_over_letter() {
        let a = GlyphWithTransparency::from_char('a').with_primary_only().with_primary_rgb(BLUE);
        let b = GlyphWithTransparency::from_char('b').with_primary_rgb(GREEN);
        let c = GlyphWithTransparency::from_char('a').with_rgbs(BLUE, GREEN);
        dbg!(&a, &b, &c);
        assert_eq!(a.over(b), c);
    }
    #[test]
    fn test_partially_occluded_letter() {
        let mut a = GlyphWithTransparency::from_char(LOWER_HALF_BLOCK).with_primary_only().with_rgbs(GREEN, PURPLE);
        let mut b = GlyphWithTransparency::from_char('b').with_rgbs(RED, BLUE);

        let c = GlyphWithTransparency {
            character: LOWER_HALF_BLOCK,
            primary_color: GREEN.into(),
            secondary_color: RED.into(),
            fg_is_primary: true,
        };
        assert_eq!(a.over(b), c);
    }
    #[test]
    fn test_multiply_partially_occluded_letter() {
        let mut a = GlyphWithTransparency::from_char(LOWER_HALF_BLOCK);
        a.secondary_color.a = 0;
        *a.primary_color.rgb_mut() = RED;
        let mut b = GlyphWithTransparency::from_char(RIGHT_HALF_BLOCK);
        b.secondary_color.a = 0;
        *b.primary_color.rgb_mut() = BLUE;
        let mut c = GlyphWithTransparency::from_char('c').with_rgbs(PURPLE, GREEN);
        assert_eq!(a.over(b.over(c)), GlyphWithTransparency::from_char(LOWER_HALF_BLOCK).with_rgbs(RED, BLUE));
    }
}
