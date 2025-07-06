// use crate::graphics::drawable::{Drawable, DrawableEnum};
// use crate::piece::NStep;
use crate::*;

// empty enums for euclid typing
#[derive(Clone, PartialEq, Debug, Copy)]
pub struct SquareGridInWorldFrame;


#[derive(Clone, PartialEq, Debug, Copy)]
pub struct SquareGridInLocalSquareFrame;

pub type WorldSquare = Point2D<i32, SquareGridInWorldFrame>;
pub type WorldPoint = Point2D<f32, SquareGridInWorldFrame>;
pub type WorldSquareRect = Box2D<i32, SquareGridInWorldFrame>;
pub type BoardSize = Size2D<u32, SquareGridInWorldFrame>;

pub type WorldStep = Vector2D<i32, SquareGridInWorldFrame>;
pub type WorldMove = Vector2D<f32, SquareGridInWorldFrame>;

pub type SquareList = Vec<WorldSquare>;
pub type StepList = Vec<WorldStep>;
pub type NStepList = Vec<NStep>;
pub type PointList = Vec<WorldPoint>;
pub type MoveList = Vec<WorldMove>;

pub type SquareSet = HashSet<WorldSquare>;
pub type StepSet = HashSet<WorldStep>;
pub type NStepSet = HashSet<NStep>;



pub type LocalSquare = Point2D<i32, SquareGridInLocalSquareFrame>;
pub type LocalSquarePoint = Point2D<f32, SquareGridInLocalSquareFrame>;


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


// TODO: make this more general
pub fn world_half_plane_to_local_square_half_plane(
    world_half_plane: HalfPlane<f32, SquareGridInWorldFrame>,
    ref_square: WorldSquare,
) -> HalfPlane<f32, SquareGridInLocalSquareFrame> {
    world_half_plane.with_transformed_points(|p| world_point_to_local_square_point(p, ref_square))
}





pub fn world_point_to_world_square(point: WorldPoint) -> WorldSquare {
    point.round().to_i32()
}

