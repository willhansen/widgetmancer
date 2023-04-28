use dyn_clone::DynClone;
use euclid::Angle;
use getset::CopyGetters;
use itertools::Itertools;
use rgb::RGB8;

use crate::fov_stuff::{LocalSquareHalfPlane, SquareVisibility};
use crate::glyph::angled_blocks::half_plane_to_angled_block_character;
use crate::glyph::glyph_constants::{OUT_OF_SIGHT_COLOR, RED};
use crate::glyph::{DoubleGlyph, DoubleGlyphFunctions, Glyph};
use crate::utility::coordinate_frame_conversions::local_square_half_plane_to_local_character_half_plane;
use crate::utility::QuarterTurnsAnticlockwise;

// This is kinda magic.  Not great, but if it works, it works.
dyn_clone::clone_trait_object!(Drawable);

pub trait Drawable: DynClone {
    fn rotate(&mut self, quarter_rotations_anticlockwise: i32);
    fn to_glyphs(&self) -> DoubleGlyph;
    fn draw_over(&mut self, other: &Box<dyn Drawable>);
    fn as_drawable_object(&self) -> Box<dyn Drawable>;
    fn color_if_backgroundified(&self) -> RGB8;
}

#[derive(Debug, Clone, PartialEq, CopyGetters)]
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
    fn rotate(&mut self, quarter_rotations_anticlockwise: i32) {
        // lmao no
    }

    fn to_glyphs(&self) -> DoubleGlyph {
        self.glyphs
    }

    fn draw_over(&mut self, other: &Box<dyn Drawable>) {
        self.glyphs = self.to_glyphs().drawn_over(other.to_glyphs())
    }

    fn as_drawable_object(&self) -> Box<dyn Drawable> {
        let trait_box: Box<dyn Drawable> = Box::new((*self).clone());
        trait_box
    }

    fn color_if_backgroundified(&self) -> RGB8 {
        self.glyphs.solid_color_if_backgroundified()[0]
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
    fn rotate(&mut self, quarter_rotations_anticlockwise: i32) {
        self.the_shadow = self.the_shadow.rotated(quarter_rotations_anticlockwise)
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

    fn draw_over(&mut self, other: &Box<dyn Drawable>) {
        self.bg_color = other.color_if_backgroundified();
    }

    fn as_drawable_object(&self) -> Box<dyn Drawable> {
        let trait_box: Box<dyn Drawable> = Box::new((*self).clone());
        trait_box
    }

    fn color_if_backgroundified(&self) -> RGB8 {
        self.bg_color
    }
}

pub struct BrailleDrawable {
    // todo
}

pub struct HextantDrawable {
    // todo
}

pub struct ArrowDrawable {
    // todo
}
#[cfg(test)]
mod tests {
    use crate::glyph::glyph_constants::GREEN;
    use pretty_assertions::{assert_eq, assert_ne};

    use super::*;

    #[test]
    fn test_shadow_over_text() {
        let shadow =
            ShadowDrawable::from_square_visibility(SquareVisibility::bottom_half_visible());
        let text = TextDrawable::new("a ", RED, GREEN, false);

        let mut stacked = shadow.clone();
        stacked.draw_over(&text.as_drawable_object());

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
