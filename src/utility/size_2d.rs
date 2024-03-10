use crate::Coordinate;

pub trait Size2D: Coordinate {
    // TODO: make an `UnsignedCoordinate` trait

    fn width(&self) -> Self::DataType {
        self.x()
    }
    fn height(&self) -> Self::DataType {
        self.y()
    }
}

impl<T> Size2D for T where T: Coordinate {}
