use euclid::default::{Point2D, Vector2D};
use euclid::*;

// empty enums for euclid typing
pub enum WorldSpace {}
pub enum BrailleWorldSpace {}
pub enum CharacterWorldSpace {}
pub enum ScreenBufferCharacterSpace {}
pub enum ScreenCharacterSpace {}

pub type IPoint = Point2D<i32>;
pub type FPoint = Point2D<f32>;
pub type IVector = Vector2D<i32>;
pub type FVector = Vector2D<f32>;

pub const DOWN_I: IVector = vec2(0, -1);
pub const UP_I: IVector = vec2(0, 1);
pub const LEFT_I: IVector = vec2(-1, 0);
pub const RIGHT_I: IVector = vec2(1, 0);


pub fn sign(x: f32) -> f32 {
    if x < 0.0 {
        -1.0
    } else if x > 0.0{
        1.0
    } else {
        0.0
    }
}

pub fn sign2d(point: FPoint) -> FPoint {
    point2(sign(point.x), sign(point.y))
}

pub fn fraction_part(point: Point2D<f32>) -> Point2D<f32> {
    (point - point.round()).to_point()
}

pub fn get_by_point<T>(grid: &Vec<Vec<T>>, p: IPoint) -> &T {
    &grid[p.x as usize][p.y as usize]
}
