use crate::glyph::DoubleGlyph;
use crate::utility::QuarterTurnsAnticlockwise;

pub trait Drawable {
    fn rotated(&self, rotation: QuarterTurnsAnticlockwise) -> Self;
    fn glyphs(&self) -> DoubleGlyph;
    fn drawn_over(&self, other: Self) -> Self;
}

pub struct TextDrawable {
    // todo
}
pub struct ShadowDrawable {
    // todo
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
