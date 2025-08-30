use std::iter::once;

use rgb::{RGB8, RGBA8};
use utility::array_zip;

use crate::glyph_constants::{named_colors::*, SPACE};
use crate::DrawableGlyph;

#[derive(Clone, Copy, Hash, Debug, PartialEq)]
pub struct GlyphWithTransparency {
    pub character: char,
    pub primary_color: RGBA8,
    pub secondary_color: RGBA8,
    // For when you and unicode disagree on the foreground and background parts of a character
    // Primarily used when converting to a non-transparent Glyph
    pub fg_is_primary: bool,
}

impl GlyphWithTransparency {
    pub fn new(character: char, fg: RGBA8, bg: RGBA8) -> Self {
        Self {
            character,
            primary_color: fg,
            secondary_color: bg,
            fg_is_primary: true,
        }
    }
    pub fn transparent() -> Self {
        GlyphWithTransparency {
            character: SPACE,
            primary_color: RGBA8::from(BLACK).with_alpha(0),
            secondary_color: RGBA8::from(BLACK).with_alpha(0),
            fg_is_primary: false,
        }
    }
    pub fn fg_only(c: char, fg: RGBA8) -> Self {
        Self::new(c, fg, BLACK.with_alpha(0))
    }

    pub fn from_char(c: char) -> Self {
        Self::from_drawable_with_default_as_white_on_transparent(DrawableGlyph::from_char(c))
    }
    pub fn with_primary_only(&self) -> Self {
        self.with_transparent_secondary()
    }
    pub fn with_transparent_secondary(&self) -> Self {
        let mut x = self.clone();
        x.secondary_color = x.secondary_color.with_alpha(0);
        x
    }
    pub fn with_primary_rgb(&self, color: RGB8) -> Self {
        let mut x = self.clone();
        *x.primary_color.rgb_mut() = color;
        x
    }
    pub fn with_primary_color(&self, color: RGBA8) -> Self {
        let mut x = self.clone();
        x.primary_color = color;
        x
    }
    pub fn with_secondary_color(&self, color: RGBA8) -> Self {
        let mut x = self.clone();
        x.secondary_color = color;
        x
    }
    pub fn with_secondary_rgb(&self, color: RGB8) -> Self {
        let mut x = self.clone();
        *x.secondary_color.rgb_mut() = color;
        x
    }
    pub fn fg_color(&self) -> RGBA8 {
        if self.fg_is_primary {
            self.primary_color
        } else {
            self.secondary_color
        }
    }
    pub fn bg_color(&self) -> RGBA8 {
        if self.fg_is_primary {
            self.secondary_color
        } else {
            self.primary_color
        }
    }
    pub fn fg_color_mut(&mut self) -> &mut RGBA8 {
        if self.fg_is_primary {
            &mut self.primary_color
        } else {
            &mut self.secondary_color
        }
    }
    pub fn bg_color_mut(&mut self) -> &mut RGBA8 {
        if self.fg_is_primary {
            &mut self.secondary_color
        } else {
            &mut self.primary_color
        }
    }
    pub fn with_rgbs(&self, primary_rgb: RGB8, secondary_rgb: RGB8) -> Self {
        self.with_primary_rgb(primary_rgb)
            .with_secondary_rgb(secondary_rgb)
    }
    pub fn with_colors(&self, primary_color: RGBA8, secondary_color: RGBA8) -> Self {
        self.with_primary_color(primary_color)
            .with_secondary_color(secondary_color)
    }
    pub fn over(&self, other: Self) -> Self {
        let mut out = self.clone();
        out.primary_color = color_combine(self.primary_color, other.primary_color);
        out.secondary_color = color_combine(self.secondary_color, other.primary_color);
        out
    }
    pub fn over_solid_bg(&self, c: RGB8) -> DrawableGlyph {
        let mut bg = Self::from_char(' ').with_primary_rgb(c);
        bg.fg_is_primary = false;
        let result = self.over(bg);
        DrawableGlyph::new_colored(
            self.character,
            result.fg_color().rgb(),
            result.bg_color().rgb(),
        )
    }
    pub fn seen_through_window(&self, window: char) -> Self {
        Self {
            character: window,
            primary_color: self.primary_color,
            secondary_color: BLACK.with_alpha(0),
            fg_is_primary: true,
        }
    }
    pub fn to_string(&self) -> String {
        self.over_solid_bg(BLACK).to_string()
    }
    pub fn with_bg_as_primary(&self) -> Self {
        let mut x = self.clone();
        x.fg_is_primary = false;
        x
    }
    pub fn solid_color(color: RGB8) -> Self {
        Self::from_char(' ')
            .with_primary_rgb(color)
            .with_bg_as_primary()
    }
    pub fn to_drawable_with_transparent_as_default(&self) -> DrawableGlyph {
        let fg = self.fg_color();
        let bg = self.bg_color();
        DrawableGlyph::new(
            self.character,
            if fg.a == 0 { None } else { Some(fg.rgb()) },
            if bg.a == 0 { None } else { Some(bg.rgb()) },
        )
    }
    pub fn from_drawable_with_default_as_transparent(drawable: DrawableGlyph) -> Self {
        Self::from_drawable_with_default_as(drawable, BLACK.with_alpha(0), BLACK.with_alpha(0))
    }
    pub fn from_drawable_with_default_as_white_on_transparent(drawable: DrawableGlyph) -> Self {
        Self::from_drawable_with_default_as(drawable, WHITE.with_alpha(255), BLACK.with_alpha(0))
    }
    fn from_drawable_with_default_as(
        drawable: DrawableGlyph,
        default_fg_color: RGBA8,
        default_bg_color: RGBA8,
    ) -> Self {
        Self::new(
            drawable.character,
            match drawable.fg_color {
                None => default_fg_color,
                Some(c) => c.into(),
            },
            match drawable.bg_color {
                None => default_bg_color,
                Some(c) => c.into(),
            },
        )
    }
}

pub type DoubleGlyphWithTransparency = [GlyphWithTransparency; 2];
pub trait DoubleGlyphWithTransparencyExt {
    fn solid_color(color: RGB8) -> Self;
    fn with_primary_only(&self) -> Self;
    fn with_primary_rgb(&self, color: RGB8) -> Self;
    fn with_secondary_rgb(&self, color: RGB8) -> Self;
    fn over(&self, other: Self) -> Self;
}
impl DoubleGlyphWithTransparencyExt for DoubleGlyphWithTransparency {
    fn solid_color(color: RGB8) -> Self {
        [GlyphWithTransparency::solid_color(color); 2]
    }
    fn with_primary_only(&self) -> Self {
        self.map(|g| g.with_primary_only())
    }
    fn with_primary_rgb(&self, color: RGB8) -> Self {
        self.map(|g| g.with_primary_rgb(color))
    }
    fn with_secondary_rgb(&self, color: RGB8) -> Self {
        self.map(|g| g.with_secondary_rgb(color))
    }
    fn over(&self, other: Self) -> Self {
        array_zip(*self, other).map(|(a, b)| a.over(b))
    }
}
// ref: https://en.wikipedia.org/wiki/Alpha_compositing
pub fn color_combine(above: RGBA8, below: RGBA8) -> RGBA8 {
    let a = above;
    let b = below;
    let a_a = a.a as f32 / 255.0;
    let b_a = b.a as f32 / 255.0;
    let c_a = a_a + (1.0 - a_a) * b_a;
    RGBA8::from_iter(
        a.rgb()
            .iter()
            .zip(b.rgb().iter())
            .map(|(a, b)| (a as f32 * a_a + b as f32 * (1.0 - a_a) * b_a) / c_a)
            .chain(once(c_a * 255.0))
            .map(|f| f as u8),
    )
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
    fn test_letter_over_letter() {
        let a = GlyphWithTransparency::from_char('a')
            .with_primary_only()
            .with_primary_rgb(BLUE);
        let b = GlyphWithTransparency::from_char('b').with_primary_rgb(GREEN);
        let c = GlyphWithTransparency::from_char('a').with_colors(BLUE.into(), GREEN.into());
        dbg!(&a, &b, &c);
        assert_eq!(a.over(b), c);
    }
    #[test]
    fn test_partially_occluded_letter() {
        let mut a = GlyphWithTransparency::from_char(LOWER_HALF_BLOCK)
            .with_primary_only()
            .with_rgbs(GREEN, PURPLE);
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
    fn test_stacked_partially_occluded_letter() {
        let mut a = GlyphWithTransparency::from_char(LOWER_HALF_BLOCK);
        a.secondary_color.a = 0;
        *a.primary_color.rgb_mut() = RED;
        let mut b = GlyphWithTransparency::from_char(RIGHT_HALF_BLOCK);
        b.secondary_color.a = 0;
        *b.primary_color.rgb_mut() = BLUE;
        let mut c = GlyphWithTransparency::from_char('c').with_colors(PURPLE.into(), GREEN.into());
        assert_eq!(
            a.over(b.over(c)),
            GlyphWithTransparency::from_char(LOWER_HALF_BLOCK).with_colors(RED.into(), BLUE.into())
        );
    }
}
