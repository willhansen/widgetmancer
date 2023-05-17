use ambassador::{delegatable_trait, Delegate};
use derive_more::From;
use dyn_clone::DynClone;
use euclid::Angle;
use getset::CopyGetters;
use itertools::Itertools;
use rgb::RGB8;

use crate::fov_stuff::{LocalSquareHalfPlane, SquareVisibility};
use crate::glyph::angled_blocks::half_plane_to_angled_block_character;
use crate::glyph::braille::{BrailleArray, DoubleBrailleArray};
use crate::glyph::glyph_constants::{GREEN, OUT_OF_SIGHT_COLOR, RED};
use crate::glyph::{DoubleGlyph, DoubleGlyphFunctions, Glyph};
use crate::utility::coordinate_frame_conversions::local_square_half_plane_to_local_character_half_plane;
use crate::utility::QuarterTurnsAnticlockwise;

#[delegatable_trait]
pub trait Drawable: Clone {
    fn rotated(&self, quarter_rotations_anticlockwise: i32) -> DrawableEnum;
    fn to_glyphs(&self) -> DoubleGlyph;
    fn drawn_over<T: Drawable>(&self, other: &T) -> DrawableEnum;
    fn color_if_backgroundified(&self) -> RGB8;
    fn to_enum(&self) -> DrawableEnum;
}

#[derive(Debug, Clone, From, Delegate)]
#[delegate(Drawable)]
pub enum DrawableEnum {
    Text(TextDrawable),
    PartialVisibility(PartialVisibilityDrawable),
    SolidColor(SolidColorDrawable),
    Braille(BrailleDrawable),
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
    fn rotated(&self, quarter_rotations_anticlockwise: i32) -> DrawableEnum {
        // lmao no
        self.clone().into()
    }

    fn to_glyphs(&self) -> DoubleGlyph {
        self.glyphs
    }

    fn drawn_over<T: Drawable>(&self, other: &T) -> DrawableEnum {
        let glyphs = self.to_glyphs().drawn_over(other.to_glyphs());
        Self::from_glyphs(glyphs).into()
    }

    fn color_if_backgroundified(&self) -> RGB8 {
        self.glyphs.solid_color_if_backgroundified()[0]
    }

    fn to_enum(&self) -> DrawableEnum {
        self.clone().into()
    }
}

#[derive(Debug, Clone, CopyGetters)]
pub struct PartialVisibilityDrawable {
    // TODO: more shadows
    visibility: SquareVisibility,
    fg_color: RGB8,
    bg_color: RGB8,
}

impl PartialVisibilityDrawable {
    #[deprecated(
        note = "use from_partially_visible_drawable instead.  Shadows should be conceptualized as lack of visibility"
    )]
    pub fn from_square_visibility(square_viz: SquareVisibility) -> Self {
        assert!(!square_viz.is_fully_visible());
        PartialVisibilityDrawable {
            visibility: square_viz,
            fg_color: GREEN,              // TODO: no default color
            bg_color: OUT_OF_SIGHT_COLOR, // TODO: no default color
        }
    }
    pub fn from_partially_visible_drawable<T: Drawable>(
        original_drawable: &T,
        square_viz: SquareVisibility,
    ) -> Self {
        assert!(!square_viz.is_fully_visible());
        PartialVisibilityDrawable {
            visibility: square_viz,
            fg_color: original_drawable.color_if_backgroundified(),
            bg_color: OUT_OF_SIGHT_COLOR,
        }
    }
}

impl Drawable for PartialVisibilityDrawable {
    fn rotated(&self, quarter_rotations_anticlockwise: i32) -> DrawableEnum {
        let mut the_clone = self.clone();
        the_clone.visibility = self.visibility.rotated(quarter_rotations_anticlockwise);
        the_clone.into()
    }

    fn to_glyphs(&self) -> DoubleGlyph {
        let character_visible_portions = [0, 1].map(|i| {
            local_square_half_plane_to_local_character_half_plane(
                self.visibility.visible_portion().unwrap(),
                i,
            )
        });

        let bias_direction = self
            .visibility
            .visible_portion()
            .unwrap()
            .direction_away_from_plane();

        let glyphs = character_visible_portions
            .iter()
            .map(|vis_portion| {
                let angle_char = half_plane_to_angled_block_character(*vis_portion, bias_direction);
                Glyph::new(angle_char, self.fg_color, self.bg_color)
            })
            .collect::<Vec<Glyph>>()
            .try_into()
            .unwrap();
        glyphs
    }

    fn drawn_over<T: Drawable>(&self, other: &T) -> DrawableEnum {
        let mut the_clone = self.clone();
        the_clone.bg_color = other.color_if_backgroundified();
        the_clone.into()
    }

    fn color_if_backgroundified(&self) -> RGB8 {
        self.fg_color
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
    fn rotated(&self, quarter_rotations_anticlockwise: i32) -> DrawableEnum {
        todo!()
    }

    fn to_glyphs(&self) -> DoubleGlyph {
        todo!()
    }

    fn drawn_over<T: Drawable>(&self, other: &T) -> DrawableEnum {
        todo!()
    }

    fn color_if_backgroundified(&self) -> RGB8 {
        todo!()
    }

    fn to_enum(&self) -> DrawableEnum {
        todo!()
    }
}

#[derive(Debug, Clone, Copy, CopyGetters)]
pub struct SolidColorDrawable {
    color: RGB8,
}

impl SolidColorDrawable {
    pub fn new(color: RGB8) -> Self {
        SolidColorDrawable { color }
    }
}

impl Drawable for SolidColorDrawable {
    fn rotated(&self, quarter_rotations_anticlockwise: i32) -> DrawableEnum {
        self.clone().into()
    }

    fn to_glyphs(&self) -> DoubleGlyph {
        DoubleGlyph::solid_color(self.color)
    }

    fn drawn_over<T: Drawable>(&self, other: &T) -> DrawableEnum {
        self.clone().into()
    }

    fn color_if_backgroundified(&self) -> RGB8 {
        self.color
    }

    fn to_enum(&self) -> DrawableEnum {
        self.clone().into()
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
        let shadow = PartialVisibilityDrawable::from_square_visibility(
            SquareVisibility::bottom_half_visible(),
        );
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
