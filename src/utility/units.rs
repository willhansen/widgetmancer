use crate::graphics::drawable::{Drawable, DrawableEnum};
use crate::piece::NStep;
use crate::utility::*;

use self::size_2d::Size2D;

// empty enums for euclid typing
#[derive(Clone, PartialEq, Debug, Copy, Eq, Hash)]
pub struct SquareGridInWorldFrame;

#[deprecated(note = "Obsolete since screen rotation")]
#[derive(Clone, PartialEq, Debug, Copy, Eq, Hash)]
pub struct CharacterGridInWorldFrame;

#[derive(Clone, PartialEq, Debug, Copy, Eq, Hash)]
pub struct CharacterGridInLocalCharacterFrame;

#[derive(Clone, PartialEq, Debug, Copy, Eq, Hash)]
pub struct SquareGridInLocalSquareFrame;

pub type WorldCoordinate<DataType> = Vector2D<DataType, SquareGridInWorldFrame>;

pub type WorldSquare = WorldCoordinate<i32>;
pub type WorldPoint = WorldCoordinate<f32>;
pub type WorldSquareRect = TwoDifferentWorldSquares;
pub type BoardSize = Vector2D<u32, SquareGridInWorldFrame>;

pub type WorldStep = WorldCoordinate<i32>;
pub type WorldMove = WorldCoordinate<f32>;

pub type SquareList = Vec<WorldSquare>;
pub type StepList = Vec<WorldStep>;
pub type NStepList = Vec<NStep>;
pub type PointList = Vec<WorldPoint>;
pub type MoveList = Vec<WorldMove>;

pub type SquareSet = HashSet<WorldSquare>;
pub type StepSet = HashSet<WorldStep>;
pub type NStepSet = HashSet<NStep>;

pub type WorldSquareWithOrthogonalDir = OrthogonalFacingIntPose<WorldSquare>;
pub type Face = WorldSquareWithOrthogonalDir;
pub type RelativeSquareWithOrthogonalDir = OrthogonalFacingIntPose<WorldStep>;
pub type RelativeFace = RelativeSquareWithOrthogonalDir;
pub type OrthogonalWorldStep = OrthogonalUnitCoordinate<WorldStep>;

#[deprecated(note = "Obselete since screen rotation")]
pub type WorldCharacterSquare = Point2D<i32, CharacterGridInWorldFrame>;
#[deprecated(note = "Obselete since screen rotation")]
pub type WorldCharacterPoint = Point2D<f32, CharacterGridInWorldFrame>;
#[deprecated(note = "Obselete since screen rotation")]
pub type WorldCharacterStep = Vector2D<i32, CharacterGridInWorldFrame>;
#[deprecated(note = "Obselete since screen rotation")]
pub type WorldCharacterMove = Vector2D<f32, CharacterGridInWorldFrame>;

pub type LocalCharacterSquare = Point2D<i32, CharacterGridInLocalCharacterFrame>;
pub type LocalCharacterPoint = Point2D<f32, CharacterGridInLocalCharacterFrame>;

pub type LocalSquare = Point2D<i32, SquareGridInLocalSquareFrame>;
pub type LocalSquarePoint = Point2D<f32, SquareGridInLocalSquareFrame>;

#[deprecated(note = "World does not know about glyphs")]
pub type WorldSquareGlyphMap = HashMap<WorldSquare, DoubleGlyph>;
#[deprecated(note = "World does not know about characters")]
pub type WorldCharacterSquareGlyphMap = HashMap<WorldCharacterSquare, Glyph>;
pub type WorldSquareDrawableMap = HashMap<WorldSquare, DrawableEnum>;

#[deprecated(note = "World does not know about characters")]
pub type WorldCharacterSquareToCharMap = HashMap<WorldCharacterSquare, char>;

pub const STEP_ZERO: WorldStep = vec2(0, 0);
pub const STEP_UP: WorldStep = vec2(0, 1);
pub const STEP_DOWN: WorldStep = vec2(0, -1);
pub const STEP_RIGHT: WorldStep = vec2(1, 0);
pub const STEP_LEFT: WorldStep = vec2(-1, 0);

pub const STEP_UP_RIGHT: WorldStep = vec2(1, 1);
pub const STEP_UP_LEFT: WorldStep = vec2(-1, 1);
pub const STEP_DOWN_LEFT: WorldStep = vec2(-1, -1);
pub const STEP_DOWN_RIGHT: WorldStep = vec2(1, -1);

pub const ORTHOGONAL_STEPS: [WorldStep; 4] = [STEP_UP, STEP_DOWN, STEP_RIGHT, STEP_LEFT];
pub const DIAGONAL_STEPS: [WorldStep; 4] =
    [STEP_UP_RIGHT, STEP_UP_LEFT, STEP_DOWN_RIGHT, STEP_DOWN_LEFT];
pub const KING_STEPS: [WorldStep; 8] = [
    STEP_UP,
    STEP_DOWN,
    STEP_RIGHT,
    STEP_LEFT,
    STEP_UP_RIGHT,
    STEP_UP_LEFT,
    STEP_DOWN_RIGHT,
    STEP_DOWN_LEFT,
];

pub fn ORIGIN_POSE() -> WorldSquareWithOrthogonalDir {
    (0, 0, STEP_UP).into()
}

pub fn world_square_glyph_map_to_world_character_glyph_map(
    world_square_glyph_map: WorldSquareGlyphMap,
) -> WorldCharacterSquareGlyphMap {
    let mut world_character_glyph_map = WorldCharacterSquareGlyphMap::new();
    world_square_glyph_map
        .into_iter()
        .for_each(|(world_square, two_glyphs)| {
            let left_char_square = world_square_to_left_world_character_square(world_square);
            let right_char_square = left_char_square + RIGHT_I.cast_unit();
            world_character_glyph_map.insert(left_char_square, two_glyphs[0]);
            world_character_glyph_map.insert(right_char_square, two_glyphs[1]);
        });
    world_character_glyph_map
}

#[deprecated(note = "Invalidated by screen rotation")]
pub fn world_square_to_left_world_character_square(
    world_square: WorldSquare,
) -> WorldCharacterSquare {
    (world_point_to_world_character_point(world_square.to_f32()) + vec2(-0.5, 0.0))
        .round()
        .to_i32()
}

pub fn world_square_to_both_world_character_squares(
    world_square: WorldSquare,
) -> [WorldCharacterSquare; 2] {
    let left_char_square = world_square_to_left_world_character_square(world_square);
    [left_char_square, left_char_square + STEP_RIGHT.cast_unit()]
}

pub fn world_square_to_world_character_square(
    world_square: WorldSquare,
    index: usize,
) -> WorldCharacterSquare {
    world_square_to_both_world_character_squares(world_square)[index]
}

pub fn world_point_to_local_character_point(
    world_point: WorldPoint,
    origin_character_square: WorldCharacterSquare,
) -> LocalCharacterPoint {
    (world_point_to_world_character_point(world_point) - origin_character_square.to_f32())
        .cast_unit()
}

pub fn world_point_to_local_square_point(
    world_point: WorldPoint,
    origin_square: WorldSquare,
) -> LocalSquarePoint {
    (world_point - origin_square.to_f32()).cast_unit()
}

pub fn local_square_point_to_world_point(
    local_square_point: LocalSquarePoint,
    square: WorldSquare,
) -> WorldPoint {
    local_square_point.cast_unit() + square.to_f32()
}

pub fn local_square_point_to_local_character_point(
    local_square_point: LocalSquarePoint,
    character_index_in_square: usize,
) -> LocalCharacterPoint {
    assert!([0, 1].contains(&character_index_in_square));
    let ref_square = point2(0, 0);
    let world_point = local_square_point_to_world_point(local_square_point, ref_square);
    let ref_character_square = world_square_to_left_world_character_square(ref_square)
        + if character_index_in_square == 0 {
            vec2(0, 0)
        } else {
            STEP_RIGHT.cast_unit()
        };
    world_point_to_local_character_point(world_point, ref_character_square)
}

// TODO: make this more general
pub fn world_half_plane_to_local_square_half_plane(
    world_half_plane: HalfPlane<TwoDifferentWorldPoints>,
    ref_square: WorldSquare,
) -> HalfPlane<TwoDifferentLocalSquarePoints> {
    world_half_plane.with_transformed_points(|p| world_point_to_local_square_point(p, ref_square))
}

pub fn local_square_half_plane_to_local_character_half_plane(
    square_half_plane: LocalSquareHalfPlane,
    character_index_in_square: usize,
) -> LocalCharacterHalfPlane {
    square_half_plane.with_transformed_points(|p| {
        local_square_point_to_local_character_point(p, character_index_in_square)
    })
}

pub fn world_half_plane_to_local_character_half_plane(
    world_half_plane: HalfPlane<TwoDifferentWorldPoints>,
    ref_char_square: WorldCharacterSquare,
) -> HalfPlane<TwoDifferentLocalCharacterPoints> {
    world_half_plane
        .with_transformed_points(|p| world_point_to_local_character_point(p, ref_char_square))
}

pub fn world_point_to_world_character_point(
    pos: Point2D<f32, SquareGridInWorldFrame>,
) -> Point2D<f32, CharacterGridInWorldFrame> {
    point2(pos.x * 2.0 + 0.5, pos.y)
}

pub fn world_character_point_to_world_point(
    pos: Point2D<f32, CharacterGridInWorldFrame>,
) -> Point2D<f32, SquareGridInWorldFrame> {
    point2((pos.x - 0.5) / 2.0, pos.y)
}

#[deprecated(note = "Invalidated by screen rotation")]
pub fn world_character_square_to_world_square(pos: WorldCharacterSquare) -> WorldSquare {
    world_point_to_world_square(world_character_point_to_world_point(pos.to_f32()))
}

pub fn world_point_to_world_square(point: WorldPoint) -> WorldSquare {
    point.round().to_i32()
}

#[deprecated(note = "Invalidated by screen rotation")]
pub fn world_character_point_to_world_character_square(
    point: WorldCharacterPoint,
) -> WorldCharacterSquare {
    point.round().to_i32()
}

#[deprecated(note = "Invalidated by screen rotation")]
pub fn is_world_character_square_left_square_of_world_square(
    character_square: WorldCharacterSquare,
) -> bool {
    world_square_to_left_world_character_square(world_character_square_to_world_square(
        character_square,
    )) == character_square
}

#[cfg(test)]
mod tests {

    use ntest::{assert_about_eq, assert_false, assert_true, timeout};
    use pretty_assertions::{assert_eq, assert_ne};

    use super::*;
    #[test]
    fn test_world_pos_to_character_world_pos() {
        assert_eq!(
            Point2D::<f32, CharacterGridInWorldFrame>::new(0.5, 0.0),
            world_point_to_world_character_point(Point2D::<f32, SquareGridInWorldFrame>::new(
                0.0, 0.0,
            )),
            "zero is actually between two characters"
        );
        assert_eq!(
            Point2D::<f32, CharacterGridInWorldFrame>::new(2.5, 1.0),
            world_point_to_world_character_point(Point2D::<f32, SquareGridInWorldFrame>::new(
                1.0, 1.0,
            )),
            "diagonal a bit"
        );
    }

    #[test]
    fn test_local_square_point_to_local_character_point() {
        assert_eq!(
            local_square_point_to_local_character_point(point2(0.0, 0.0), 0),
            point2(0.5, 0.0)
        );
        assert_eq!(
            local_square_point_to_local_character_point(point2(0.0, 0.0), 1),
            point2(-0.5, 0.0)
        );
    }
}
