use crate::*;

pub trait Operations: std::ops::Neg<Output = Self> + Clone {
    fn x<T: num::Signed>(&self) -> T;
    fn y<T: num::Signed>(&self) -> T;
    fn to_unit_vector(&self) -> FCoord {
        FCoord::new(self.x(), self.y())
    }
    fn xy_array<T: num::Signed>(&self) -> [T; 2] {
        [self.x(), self.y()]
    }
    fn xy_tuple<T: num::Signed>(&self) -> (T, T) {
        (self.x(), self.y())
    }
    fn reversed(&self) -> Self
    where
        Self: std::marker::Sized,
    {
        -(*self).clone()
    }
}
