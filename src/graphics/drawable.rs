use std::fmt::Debug;

use ambassador::{delegatable_trait, delegate_to_methods, Delegate};
use derive_more::Constructor;
use derive_more::From;
use dyn_clone::DynClone;
use euclid::Angle;
use getset::CopyGetters;
use itertools::Itertools;
use rgb::RGB8;

use crate::fov_stuff::{LocalSquareHalfPlane, SquareVisibility};
use crate::glyph::angled_blocks::half_plane_to_angled_block_character;
use crate::glyph::braille::{BrailleArray, DoubleBrailleArray};
use crate::glyph::glyph_constants::{BLACK, GREEN, OUT_OF_SIGHT_COLOR, RED};
use crate::glyph::{DoubleGlyph, DoubleGlyphFunctions, Glyph};
use crate::utility::coordinate_frame_conversions::{
    local_square_half_plane_to_local_character_half_plane, WorldStep,
};
use crate::utility::{
    rotate_vect, rotated_n_quarter_turns_counter_clockwise, tint_color, KingDirection,
    QuarterTurnsAnticlockwise,
};

#[delegatable_trait]
pub trait Drawable: Clone + Debug {
    fn rotated(&self, quarter_rotations_anticlockwise: i32) -> Self;
    fn to_glyphs(&self) -> DoubleGlyph;
    fn drawn_over<T: Drawable>(&self, other: &T) -> Self;
    fn color_if_backgroundified(&self) -> RGB8;
    fn to_enum(&self) -> DrawableEnum;
    fn tinted(&self, color: RGB8, strength: f32) -> Self;
}

#[derive(Debug, Clone, From, Delegate)]
#[delegate(Drawable)]
pub enum DrawableEnum {
    Text(TextDrawable),
    PartialVisibility(PartialVisibilityDrawable),
    SolidColor(SolidColorDrawable),
    Braille(BrailleDrawable),
    Arrow(ArrowDrawable),
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

    fn tinted(&self, color: RGB8, mut strength: f32) -> DrawableEnum {
        Self::from_glyphs(self.glyphs.tinted(color, strength)).into()
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

    fn tinted(&self, color: RGB8, strength: f32) -> DrawableEnum {
        Self {
            fg_color: tint_color(self.fg_color, color, strength),
            bg_color: tint_color(self.bg_color, color, strength),
            ..self.clone()
        }
        .into()
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

    fn tinted(&self, tint: RGB8, strength: f32) -> DrawableEnum {
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

    fn tinted(&self, color: RGB8, strength: f32) -> DrawableEnum {
        SolidColorDrawable {
            color: tint_color(self.color, color, strength),
        }
        .into()
    }
}

pub struct HextantDrawable {
    // todo
}

#[derive(Debug, Clone, CopyGetters)]
pub struct ArrowDrawable {
    direction: KingDirection,
    arrow_string: String,
    text_drawable: TextDrawable,
}

impl ArrowDrawable {
    pub fn new(direction: KingDirection, arrow_string: &str, color: RGB8) -> Self {
        let arrow_char = Glyph::extract_arrow_from_arrow_string(direction.into(), arrow_string);
        let text_drawable = TextDrawable::new(&(arrow_char.to_string() + " "), color, BLACK, true);
        ArrowDrawable {
            direction,
            arrow_string: arrow_string.to_string(),
            text_drawable,
        }
    }

    fn with_updated_text_drawable(&self) -> Self {
        let mut the_clone = self.clone();
        the_clone.text_drawable.glyphs[0].character =
            Glyph::extract_arrow_from_arrow_string(self.direction.into(), &self.arrow_string);
        the_clone
    }
}

impl Drawable for ArrowDrawable {
    fn rotated(&self, quarter_rotations_anticlockwise: i32) -> DrawableEnum {
        ArrowDrawable {
            direction: rotated_n_quarter_turns_counter_clockwise(
                self.direction.into(),
                quarter_rotations_anticlockwise,
            )
            .into(),
            ..self.clone()
        }
        .with_updated_text_drawable()
        .into()
    }

    fn to_glyphs(&self) -> DoubleGlyph {
        self.text_drawable.to_glyphs()
    }

    fn drawn_over<T: Drawable>(&self, other: &T) -> DrawableEnum {
        match self.text_drawable.drawn_over(other) {
            DrawableEnum::Text(text_drawable) => ArrowDrawable {
                text_drawable,
                ..self.clone()
            }
            .into(),
            _ => panic!("should have still been text!"),
        }
    }

    fn color_if_backgroundified(&self) -> RGB8 {
        self.text_drawable.color_if_backgroundified()
    }

    fn to_enum(&self) -> DrawableEnum {
        self.clone().into()
    }

    fn tinted(&self, color: RGB8, strength: f32) -> DrawableEnum {
        ArrowDrawable {
            text_drawable: match self.text_drawable.tinted(color, strength) {
                DrawableEnum::Text(text_drawable) => text_drawable,
                _ => panic!("should have still been text!"),
            },
            ..self.clone()
        }
        .into()
    }
}

#[cfg(test)]
mod tests {
    use euclid::point2;
    use pretty_assertions::{assert_eq, assert_ne};

    use crate::glyph::glyph_constants::{BLACK, BLUE, GREEN, SPACE, THICK_ARROWS};
    use crate::utility::{Line, STEP_RIGHT};

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

    #[test]
    fn test_text_drawn_over_solid_to_glyphs_with_empty_space() {
        let solid_drawable = SolidColorDrawable::new(GREEN);
        let text_drawable = TextDrawable::new("♟ ", RED, BLUE, true);
        let combo = text_drawable.drawn_over(&solid_drawable);
        let glyphs = combo.to_glyphs();
        assert_eq!(glyphs[1].character, SPACE);
    }

    #[test]
    fn test_top_half_visible_glyphs() {
        let base = SolidColorDrawable::new(RED).to_enum();
        let visibility = SquareVisibility::new_partially_visible(
            LocalSquareHalfPlane::from_line_and_point_on_half_plane(
                Line::new(point2(0.0, 0.0), point2(-1.0, 0.0)),
                point2(0.0, 25.0),
            ),
        );
        dbg!(visibility);
        let top_half =
            PartialVisibilityDrawable::from_partially_visible_drawable(&base, visibility);
        assert_eq!(top_half.to_glyphs().to_clean_string(), "🬎🬎");
    }
    #[test]
    fn test_arrow_drawable() {
        let d = ArrowDrawable::new(STEP_RIGHT.into(), THICK_ARROWS, BLUE);
    }
}
