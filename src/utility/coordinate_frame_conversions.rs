use crate::utility::*;

// empty enums for euclid typing
#[derive(Copy, Clone)]
pub struct SquareGridInWorldFrame;

#[derive(Copy, Clone)]
pub struct CharacterGridInWorldFrame;

#[derive(Copy, Clone)]
pub struct CharacterGridInBufferFrame;

#[derive(Copy, Clone)]
pub struct CharacterGridInScreenFrame;

#[derive(Copy, Clone)]
pub struct CharacterGridInLocalCharacterFrame;

#[derive(Copy, Clone)]
pub struct SquareGridInLocalSquareFrame;

pub type WorldSquare = Point2D<i32, SquareGridInWorldFrame>;
pub type WorldPoint = Point2D<f32, SquareGridInWorldFrame>;
pub type WorldSquareRect = Box2D<i32, SquareGridInWorldFrame>;
pub type BoardSize = Size2D<u32, SquareGridInWorldFrame>;

pub type WorldStep = Vector2D<i32, SquareGridInWorldFrame>;
pub type WorldMove = Vector2D<f32, SquareGridInWorldFrame>;

pub type SquareList = Vec<WorldSquare>;
pub type StepList = Vec<WorldStep>;

pub type SquareSet = HashSet<WorldSquare>;

pub type WorldCharacterSquare = Point2D<i32, CharacterGridInWorldFrame>;
pub type WorldCharacterPoint = Point2D<f32, CharacterGridInWorldFrame>;

pub type LocalCharacterSquare = Point2D<i32, CharacterGridInLocalCharacterFrame>;
pub type LocalCharacterPoint = Point2D<f32, CharacterGridInLocalCharacterFrame>;

pub type LocalSquare = Point2D<i32, SquareGridInLocalSquareFrame>;
pub type LocalSquarePoint = Point2D<f32, SquareGridInLocalSquareFrame>;

pub type BufferCharacterSquare = Point2D<i32, CharacterGridInBufferFrame>;
pub type BufferCharacterPoint = Point2D<f32, CharacterGridInBufferFrame>;

pub type ScreenCharacterSquare = Point2D<i32, CharacterGridInScreenFrame>;
pub type ScreenCharacterPoint = Point2D<f32, CharacterGridInScreenFrame>;

pub type WorldSquareGlyphMap = HashMap<WorldSquare, DoubleGlyph>;
pub type WorldCharacterSquareToGlyphMap = HashMap<WorldCharacterSquare, Glyph>;

pub type WorldCharacterSquareToCharMap = HashMap<WorldCharacterSquare, char>;

pub type BufferGlyphMap = HashMap<BufferCharacterSquare, Glyph>;
pub fn world_square_glyph_map_to_world_character_glyph_map(
    world_square_glyph_map: WorldSquareGlyphMap,
) -> WorldCharacterSquareToGlyphMap {
    let mut world_character_glyph_map = WorldCharacterSquareToGlyphMap::new();
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

pub fn world_square_to_left_world_character_square(
    world_square: WorldSquare,
) -> WorldCharacterSquare {
    (world_point_to_world_character_point(world_square.to_f32()) + vec2(-0.5, 0.0))
        .round()
        .to_i32()
}

pub fn world_point_to_local_character_point(
    world_point: WorldPoint,
    origin_character_square: WorldCharacterSquare,
) -> LocalCharacterPoint {
    (world_point_to_world_character_point(world_point) - origin_character_square.to_f32())
        .to_point()
        .cast_unit()
}

pub fn world_point_to_local_square_point(
    world_point: WorldPoint,
    origin_square: WorldSquare,
) -> LocalSquarePoint {
    (world_point - origin_square.to_f32())
        .to_point()
        .cast_unit()
}

pub fn local_square_point_to_world_point(
    local_square_point: LocalSquarePoint,
    square: WorldSquare,
) -> WorldPoint {
    (local_square_point.cast_unit() + square.to_f32().to_vector())
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
pub fn world_half_plane_to_local_character_half_plane(
    world_half_plane: HalfPlane<SquareGridInWorldFrame>,
    ref_char_square: WorldCharacterSquare,
) -> HalfPlane<CharacterGridInLocalCharacterFrame> {
    HalfPlane::new(
        Line {
            p1: world_point_to_local_character_point(
                world_half_plane.dividing_line.p1,
                ref_char_square,
            ),
            p2: world_point_to_local_character_point(
                world_half_plane.dividing_line.p2,
                ref_char_square,
            ),
        },
        world_point_to_local_character_point(world_half_plane.point_on_half_plane, ref_char_square),
    )
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

pub fn world_character_square_to_world_square(pos: WorldCharacterSquare) -> WorldSquare {
    world_character_point_to_world_point(pos.to_f32())
        .round()
        .to_i32()
}