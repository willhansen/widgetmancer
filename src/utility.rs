use euclid::default::{Point2D, Vector2D};
use euclid::point2 as p;

pub const DOWN_I: Point2D<i32> = p(0, -1);
pub const UP_I: Point2D<i32> = p(0, 1);
pub const LEFT_I: Point2D<i32> = p(-1, 0);
pub const RIGHT_I: Point2D<i32> = p(1, 0);

pub fn sign(x: f32) -> f32 {
    if x < 0.0 {
        -1.0
    } else if x > 0.0{
        1.0
    } else {
        0.0
    }
}

pub fn sign2d(point: Point2D<f32>) -> Point2D<f32> {
    p (sign(point.x), sign(point.y))
}

pub fn fraction_part(point: Point2D<f32>) -> Point2D<f32> {
    (point - point.round()).to_point()
}
