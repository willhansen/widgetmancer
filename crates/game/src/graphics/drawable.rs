use std::collections::HashMap;
use std::fmt::Debug;

use ambassador::{delegatable_trait, delegate_to_methods, Delegate};
use derive_more::Constructor;
use derive_more::From;
use dyn_clone::DynClone;
use euclid::{vec2, Angle};
use getset::CopyGetters;
use itertools::Itertools;
use rgb::RGB8;

use crate::fov_stuff::{LocalSquareHalfPlane, SquareVisibility};
use glyph_constants::named_colors::*;
use terminal_rendering::*;
use utility::*;

#[delegatable_trait]
pub trait Drawable: Clone + Debug {
    fn rotated(&self, quarter_rotations_anticlockwise: i32) -> DrawableEnum;
    fn to_glyphs(&self) -> DoubleGlyph;
    fn drawn_over<T: Drawable>(&self, other: &T) -> DrawableEnum;
    fn color_if_backgroundified(&self) -> RGB8;
    fn to_enum(&self) -> DrawableEnum;
    fn tinted(&self, color: RGB8, strength: f32) -> DrawableEnum;
}

#[derive(Debug, Clone, From, Delegate)]
#[delegate(Drawable)]
pub enum DrawableEnum {
    Text(TextDrawable),
    PartialVisibility(PartialVisibilityDrawable),
    SolidColor(SolidColorDrawable),
    Braille(BrailleDrawable),
    Arrow(ArrowDrawable),
    ConveyorBelt(ConveyorBeltDrawable),
    OffsetSquare(OffsetSquareDrawable),
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
    fn rotated(&self, _quarter_rotations_anticlockwise: i32) -> DrawableEnum {
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
        let character_visible_portions = self.visibility.split_into_character_visibilities();

        let bias_direction = self
            .visibility
            .visible_portion()
            .unwrap()
            .direction_away_from_plane();

        let glyphs = character_visible_portions
            .iter()
            .map(|vis_portion| {
                if let Some(vis_portion) = vis_portion {
                    dbg!(&vis_portion);
                    let angle_char =
                        half_plane_to_angled_block_character(*vis_portion, bias_direction);
                    dbg!(&angle_char);
                    Glyph::new(angle_char, self.fg_color, self.bg_color)
                } else {
                    Glyph::solid_fg(self.fg_color)
                }
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
    braille_array: DoubleBrailleArray,
    dot_color: RGB8,
    bg_color: RGB8,
}

impl BrailleDrawable {
    pub fn from_chars(chars: DoubleChar, dot_color: RGB8) -> Self {
        BrailleDrawable {
            braille_array: DoubleBrailleArray::from_chars(chars),
            dot_color,
            bg_color: BLACK,
        }
    }
    pub fn from_braille_array(array: DoubleBrailleArray, dot_color: RGB8) -> Self {
        BrailleDrawable {
            braille_array: array,
            dot_color,
            bg_color: BLACK,
        }
    }
    pub fn line(start: WorldPoint, end: WorldPoint, color: RGB8) -> HashMap<WorldSquare, Self> {
        let arrays = get_braille_arrays_for_braille_line(start, end);
        arrays
            .into_iter()
            .map(|(square, array)| (square, Self::from_braille_array(array, color)))
            .collect()
    }
}

impl Drawable for BrailleDrawable {
    fn rotated(&self, quarter_rotations_anticlockwise: i32) -> DrawableEnum {
        let r = self.braille_array.rotated(QuarterTurnsAnticlockwise::new(
            quarter_rotations_anticlockwise,
        ));
        Self {
            braille_array: r,
            ..self.clone()
        }
        .into()
    }

    fn to_glyphs(&self) -> DoubleGlyph {
        self.braille_array
            .to_two_braille_arrays()
            .map(|b| Glyph::new(b.char(), self.dot_color, self.bg_color))
    }

    fn drawn_over<T: Drawable>(&self, other: &T) -> DrawableEnum {
        let mut the_clone = self.clone();
        the_clone.bg_color = other.color_if_backgroundified();
        the_clone.into()
    }

    fn color_if_backgroundified(&self) -> RGB8 {
        self.dot_color
    }

    fn to_enum(&self) -> DrawableEnum {
        self.clone().into()
    }

    fn tinted(&self, tint: RGB8, strength: f32) -> DrawableEnum {
        Self {
            dot_color: tint_color(self.dot_color, tint, strength),
            bg_color: tint_color(self.bg_color, tint, strength),
            ..self.clone()
        }
        .into()
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
    fn rotated(&self, _quarter_rotations_anticlockwise: i32) -> DrawableEnum {
        self.clone().into()
    }

    fn to_glyphs(&self) -> DoubleGlyph {
        DoubleGlyph::solid_color(self.color)
    }

    fn drawn_over<T: Drawable>(&self, _other: &T) -> DrawableEnum {
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

#[derive(Debug, Clone, CopyGetters)]
pub struct ArrowDrawable {
    direction: KingWorldStep,
    arrow_string: String,
    text_drawable: TextDrawable,
}

impl ArrowDrawable {
    pub fn new(direction: KingWorldStep, arrow_string: &str, color: RGB8) -> Self {
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

#[derive(Debug, Clone, CopyGetters)]
pub struct ConveyorBeltDrawable {
    direction: OrthogonalWorldStep,
    normalized_phase_offset: f32,
    colors: [RGB8; 2],
}

impl ConveyorBeltDrawable {
    pub fn new(direction: OrthogonalWorldStep, phase_offset: f32) -> Self {
        ConveyorBeltDrawable {
            direction,
            normalized_phase_offset: phase_offset,
            colors: [WHITE, BLACK],
        }
    }
}

impl Drawable for ConveyorBeltDrawable {
    fn rotated(&self, quarter_rotations_anticlockwise: i32) -> DrawableEnum {
        ConveyorBeltDrawable {
            direction: rotated_n_quarter_turns_counter_clockwise(
                self.direction.into(),
                quarter_rotations_anticlockwise,
            )
            .into(),
            ..self.clone()
        }
        .to_enum()
    }

    fn to_glyphs(&self) -> DoubleGlyph {
        let chars = characters_for_full_square_with_looping_1d_offset(
            self.direction,
            self.normalized_phase_offset * 2.0,
        );
        chars.map(|c| Glyph::new(c, self.colors[0], self.colors[1]))
    }

    fn drawn_over<T: Drawable>(&self, other: &T) -> DrawableEnum {
        self.to_enum()
    }

    fn color_if_backgroundified(&self) -> RGB8 {
        if (self.normalized_phase_offset % 1.0).abs() <= 0.5 {
            self.colors[0]
        } else {
            self.colors[1]
        }
    }

    fn to_enum(&self) -> DrawableEnum {
        self.clone().into()
    }

    fn tinted(&self, color: RGB8, strength: f32) -> DrawableEnum {
        ConveyorBeltDrawable {
            colors: self.colors.map(|c| tint_color(c, color, strength)),
            ..self.clone()
        }
        .into()
    }
}

#[derive(Debug, Clone, CopyGetters)]
pub struct OffsetSquareDrawable {
    offset: WorldMove,
    colors: [RGB8; 2],
}

impl OffsetSquareDrawable {
    fn fg_color(&self) -> RGB8 {
        self.colors[0]
    }
    fn bg_color(&self) -> RGB8 {
        self.colors[1]
    }
    fn set_bg_color(&mut self, new_bg_color: RGB8) {
        self.colors[1] = new_bg_color;
    }
    pub fn drawables_for_floating_square_at_point(
        point: WorldPoint,
        color: RGB8,
    ) -> HashMap<WorldSquare, OffsetSquareDrawable> {
        let mut output = HashMap::<WorldSquare, OffsetSquareDrawable>::new();
        let center_square = world_point_to_world_square(point);

        (-1..=1).for_each(|dx| {
            (-1..=1).for_each(|dy| {
                let step = vec2(dx, dy);
                let square = center_square + step;
                let center_of_square = square.to_f32();
                let floating_square_offset_from_square_center = point - center_of_square;
                let drawable = OffsetSquareDrawable {
                    offset: floating_square_offset_from_square_center,
                    colors: [color, BLACK],
                };
                let glyphs_round_to_empty_square =
                    drawable.to_glyphs().chars() == [glyph_constants::SPACE; 2];
                if !glyphs_round_to_empty_square {
                    output.insert(square, drawable);
                }
            });
        });
        output
    }
}

impl Drawable for OffsetSquareDrawable {
    fn rotated(&self, quarter_rotations_anticlockwise: i32) -> DrawableEnum {
        OffsetSquareDrawable {
            offset: rotated_n_quarter_turns_counter_clockwise(
                self.offset,
                quarter_rotations_anticlockwise,
            ),
            ..self.clone()
        }
        .into()
    }

    fn to_glyphs(&self) -> DoubleGlyph {
        characters_for_full_square_with_2d_offset(self.offset)
            .map(|c| Glyph::new(c, self.fg_color(), self.bg_color()))
    }

    fn drawn_over<T: Drawable>(&self, other: &T) -> DrawableEnum {
        let mut the_clone = self.clone();
        the_clone.set_bg_color(other.color_if_backgroundified());
        the_clone.into()
    }

    fn color_if_backgroundified(&self) -> RGB8 {
        self.fg_color()
    }

    fn to_enum(&self) -> DrawableEnum {
        self.clone().into()
    }

    fn tinted(&self, color: RGB8, strength: f32) -> DrawableEnum {
        Self {
            colors: self.colors.map(|c| tint_color(c, color, strength)),
            ..self.clone()
        }
        .into()
    }
}

#[cfg(test)]
mod tests {
    use euclid::point2;
    use pretty_assertions::{assert_eq, assert_ne};
    use terminal_rendering::braille::EMPTY_BRAILLE;

    use terminal_rendering::glyph::glyph_constants::{BLACK, BLUE, GREEN, SPACE, THICK_ARROWS};
    use utility::{Line, STEP_DOWN, STEP_RIGHT, STEP_UP};

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
    fn test_arrow_drawable_rotation() {
        let d = ArrowDrawable::new(STEP_RIGHT.into(), THICK_ARROWS, BLUE);
        let character = d.rotated(1).to_glyphs()[0].character;
        assert_eq!(
            character,
            Glyph::extract_arrow_from_arrow_string(STEP_UP.into(), THICK_ARROWS)
        );
    }
    #[test]
    fn test_conveyor_belt_drawable_half_down() {
        let drawable = ConveyorBeltDrawable::new(STEP_DOWN.into(), 0.25);
        assert_eq!(drawable.to_glyphs().to_clean_string(), "▄▄")
    }
    #[test]
    fn test_offset_square_drawn_over_solid() {
        let top = OffsetSquareDrawable {
            offset: vec2(0.5, 0.0),
            colors: [RED, BLUE],
        };
        let bottom = SolidColorDrawable::new(GREEN);
        let combo = top.drawn_over(&bottom);
        assert_eq!(combo.to_glyphs()[0].get_solid_color(), Some(GREEN));
        assert_eq!(combo.to_glyphs()[1].get_solid_color(), Some(RED));
    }
    // All the braille unicode consecutively for easy reference
    //⠁⠂⠃⠄⠅⠆⠇⠈⠉⠊⠋⠌⠍⠎⠏⠐⠑⠒⠓⠔⠕⠖⠗⠘⠙⠚⠛⠜⠝⠞⠟⠠⠡⠢⠣⠤⠥⠦⠧⠨⠩⠪⠫⠬⠭⠮⠯⠰⠱⠲⠳⠴⠵⠶⠷⠸⠹⠺⠻⠼⠽⠾⠿⡀⡁⡂⡃⡄⡅⡆⡇⡈⡉⡊⡋⡌⡍⡎⡏⡐⡑⡒⡓⡔⡕⡖⡗⡘⡙⡚⡛⡜⡝⡞⡟⡠⡡⡢⡣⡤⡥⡦⡧⡨⡩⡪⡫⡬⡭⡮⡯⡰⡱⡲⡳⡴⡵⡶⡷⡸⡹⡺⡻⡼⡽⡾⡿⢀⢁⢂⢃⢄⢅⢆⢇⢈⢉⢊⢋⢌⢍⢎⢏⢐⢑⢒⢓⢔⢕⢖⢗⢘⢙⢚⢛⢜⢝⢞⢟⢠⢡⢢⢣⢤⢥⢦⢧⢨⢩⢪⢫⢬⢭⢮⢯⢰⢱⢲⢳⢴⢵⢶⢷⢸⢹⢺⢻⢼⢽⢾⢿⣀⣁⣂⣃⣄⣅⣆⣇⣈⣉⣊⣋⣌⣍⣎⣏⣐⣑⣒⣓⣔⣕⣖⣗⣘⣙⣚⣛⣜⣝⣞⣟⣠⣡⣢⣣⣤⣥⣦⣧⣨⣩⣪⣫⣬⣭⣮⣯⣰⣱⣲⣳⣴⣵⣶⣷⣸⣹⣺⣻⣼⣽⣾⣿
    #[test]
    fn test_braille_drawable_from_chars() {
        let drawable = BrailleDrawable::from_chars(['⣲', SPACE], RED);
        assert_eq!(drawable.braille_array.chars()[1], SPACE);
        drawable.braille_array.print();
    }
    #[test]
    fn test_braille_drawable_rotation() {
        let drawable = BrailleDrawable::from_chars(['⣲', SPACE], RED);
        drawable.braille_array.print();
        let f = |i| drawable.rotated(i).to_glyphs().chars();
        assert_eq!(f(-1), ['⠓', '⠃']);
        assert_eq!(f(1), ['⢠', '⢤']);
        assert_eq!(f(2), [' ', '⠯']);
        assert_eq!(f(3), ['⠓', '⠃']);
    }
}
