use derive_more::From;
use dyn_clone::DynClone;
use euclid::Angle;
use getset::CopyGetters;
use itertools::Itertools;
use rgb::RGB8;

use crate::fov_stuff::{LocalSquareHalfPlane, SquareVisibility};
use crate::glyph::angled_blocks::half_plane_to_angled_block_character;
use crate::glyph::braille::{BrailleArray, DoubleBrailleArray};
use crate::glyph::glyph_constants::{OUT_OF_SIGHT_COLOR, RED};
use crate::glyph::{DoubleGlyph, DoubleGlyphFunctions, Glyph};
use crate::utility::coordinate_frame_conversions::local_square_half_plane_to_local_character_half_plane;
use crate::utility::QuarterTurnsAnticlockwise;

pub trait Drawable: Clone {
    fn rotated(&self, quarter_rotations_anticlockwise: i32) -> Self;
    fn to_glyphs(&self) -> DoubleGlyph;
    fn drawn_over<T: Drawable>(&self, other: &T) -> Self;
    fn color_if_backgroundified(&self) -> RGB8;
    fn to_enum(&self) -> DrawableEnum;
}

#[derive(Debug, Clone, From)]
pub enum DrawableEnum {
    Text(TextDrawable),
    Shadow(ShadowDrawable),
    Braille(BrailleDrawable),
}

// TODO: Somehow reduce code duplication here
impl Drawable for DrawableEnum {
    fn rotated(&self, quarter_rotations_anticlockwise: i32) -> Self {
        match self {
            DrawableEnum::Text(v) => v.rotated(quarter_rotations_anticlockwise).into(),
            DrawableEnum::Shadow(v) => v.rotated(quarter_rotations_anticlockwise).into(),
            DrawableEnum::Braille(v) => v.rotated(quarter_rotations_anticlockwise).into(),
        }
    }

    fn to_glyphs(&self) -> DoubleGlyph {
        match self {
            DrawableEnum::Text(v) => v.to_glyphs(),
            DrawableEnum::Shadow(v) => v.to_glyphs(),
            DrawableEnum::Braille(v) => v.to_glyphs(),
        }
    }

    fn drawn_over<T: Drawable>(&self, other: &T) -> Self {
        match self {
            DrawableEnum::Text(v) => v.drawn_over(other).into(),
            DrawableEnum::Shadow(v) => v.drawn_over(other).into(),
            DrawableEnum::Braille(v) => v.drawn_over(other).into(),
        }
    }

    fn color_if_backgroundified(&self) -> RGB8 {
        match self {
            DrawableEnum::Text(v) => v.color_if_backgroundified(),
            DrawableEnum::Shadow(v) => v.color_if_backgroundified(),
            DrawableEnum::Braille(v) => v.color_if_backgroundified(),
        }
    }

    fn to_enum(&self) -> DrawableEnum {
        self.clone()
    }
}

#[derive(Debug, Clone, CopyGetters)]
pub struct TextDrawable {
    glyphs: DoubleGlyph,
}

impl TextDrawable {
    pub fn new(string: &str, fg_color: RGB8, bg_color: RGB8, bg_transparent: bool) -> Self {
        let chars = string.chars().collect_vec();
        TextDrawable {
            glyphs: [0, 1].map(|i| {
                Glyph::new(chars[i], fg_color, bg_color).with_transparent_bg(bg_transparent)
            }),
        }
    }

    pub fn from_glyphs(glyphs: DoubleGlyph) -> Self {
        TextDrawable { glyphs }
    }
}

impl Drawable for TextDrawable {
    fn rotated(&self, quarter_rotations_anticlockwise: i32) -> Self {
        // lmao no
        self.clone()
    }

    fn to_glyphs(&self) -> DoubleGlyph {
        self.glyphs
    }

    fn drawn_over<T: Drawable>(&self, other: &T) -> Self {
        let glyphs = self.to_glyphs().drawn_over(other.to_glyphs());
        Self::from_glyphs(glyphs)
    }

    fn color_if_backgroundified(&self) -> RGB8 {
        self.glyphs.solid_color_if_backgroundified()[0]
    }

    fn to_enum(&self) -> DrawableEnum {
        self.clone().into()
    }
}

#[derive(Debug, Clone, CopyGetters)]
pub struct ShadowDrawable {
    // TODO: more shadows
    the_shadow: LocalSquareHalfPlane,
    bg_color: RGB8,
}

impl ShadowDrawable {
    pub fn from_square_visibility(square_viz: SquareVisibility) -> Self {
        assert!(!square_viz.is_fully_visible());
        ShadowDrawable {
            the_shadow: square_viz.visible_portion().unwrap().complement(),
            bg_color: RED, // TODO: no default color
        }
    }
}

impl Drawable for ShadowDrawable {
    fn rotated(&self, quarter_rotations_anticlockwise: i32) -> Self {
        let mut the_clone = self.clone();
        the_clone.the_shadow = self.the_shadow.rotated(quarter_rotations_anticlockwise);
        the_clone
    }

    fn to_glyphs(&self) -> DoubleGlyph {
        let character_shadows = [0, 1]
            .map(|i| local_square_half_plane_to_local_character_half_plane(self.the_shadow, i));

        let bias_direction = self.the_shadow.direction_toward_plane();

        let glyphs = character_shadows
            .iter()
            .map(|shadow| {
                let angle_char = half_plane_to_angled_block_character(*shadow, bias_direction);
                Glyph::new(angle_char, OUT_OF_SIGHT_COLOR, self.bg_color)
            })
            .collect::<Vec<Glyph>>()
            .try_into()
            .unwrap();
        glyphs
    }

    fn drawn_over<T: Drawable>(&self, other: &T) -> Self {
        let mut the_clone = self.clone();
        the_clone.bg_color = other.color_if_backgroundified();
        the_clone
    }

    fn color_if_backgroundified(&self) -> RGB8 {
        self.bg_color
    }

    fn to_enum(&self) -> DrawableEnum {
        self.clone().into()
    }
}

#[derive(Debug, Clone, CopyGetters)]
pub struct BrailleDrawable {
    dot_array: DoubleBrailleArray,
    dot_color: RGB8,
    bg_color: RGB8,
}

impl Drawable for BrailleDrawable {
    fn rotated(&self, quarter_rotations_anticlockwise: i32) -> Self {
        todo!()
    }

    fn to_glyphs(&self) -> DoubleGlyph {
        todo!()
    }

    fn drawn_over<T: Drawable>(&self, other: &T) -> Self {
        todo!()
    }

    fn color_if_backgroundified(&self) -> RGB8 {
        todo!()
    }

    fn to_enum(&self) -> DrawableEnum {
        todo!()
    }
}

pub struct HextantDrawable {
    // todo
}

pub struct ArrowDrawable {
    // todo
}

#[cfg(test)]
mod tests {
    use pretty_assertions::{assert_eq, assert_ne};

    use crate::glyph::glyph_constants::GREEN;

    use super::*;

    #[test]
    fn test_shadow_over_text() {
        let shadow =
            ShadowDrawable::from_square_visibility(SquareVisibility::bottom_half_visible());
        let text = TextDrawable::new("a ", RED, GREEN, false);

        let stacked = shadow.drawn_over(&text);

        assert_eq!(
            stacked.to_glyphs().to_clean_string(),
            shadow.to_glyphs().to_clean_string()
        );
        assert_ne!(
            stacked.to_glyphs()[0].fg_color,
            stacked.to_glyphs()[0].bg_color
        );
    }

    #[test]
    fn test_text_background() {
        let text = TextDrawable::new("a ", RED, GREEN, false);
        assert_eq!(text.color_if_backgroundified(), RED);
    }
}
