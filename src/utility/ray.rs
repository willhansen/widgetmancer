use crate::utility::*;

pub struct Ray<U> {
    pub point: Point2D<f32, U>,
    pub angle: Angle<f32>,
}
