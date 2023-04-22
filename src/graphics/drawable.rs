use dyn_clone::DynClone;
use getset::CopyGetters;
use itertools::Itertools;
use rgb::RGB8;

use crate::fov_stuff::SquareShadow;
use crate::glyph::{DoubleGlyph, DoubleGlyphFunctions, Glyph};
use crate::utility::QuarterTurnsAnticlockwise;

// This is kinda magic.  Not great, but if it works, it works.
dyn_clone::clone_trait_object!(Drawable);

pub trait Drawable: DynClone {
    fn rotate(&mut self, quarter_rotations_anticlockwise: i32);
    fn to_glyphs(&self) -> DoubleGlyph;
    fn draw_over(&mut self, other: &Box<dyn Drawable>);
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
}

#[derive(Debug, Clone, CopyGetters)]
pub struct ShadowDrawable {
    shadows: Vec<SquareShadow>,
    bg_color: RGB8,
}

impl Drawable for ShadowDrawable {
    fn rotate(&mut self, quarter_rotations_anticlockwise: i32) {
        self.shadows = self
            .shadows
            .iter()
            .map(|shadow: &SquareShadow| shadow.rotated(quarter_rotations_anticlockwise))
            .collect_vec();
    }

    fn to_glyphs(&self) -> DoubleGlyph {
        todo!()
    }

    fn draw_over(&mut self, other: &Box<dyn Drawable>) {
        self.bg_color = other.to_glyphs().solid_color_if_backgroundified()[0];
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
