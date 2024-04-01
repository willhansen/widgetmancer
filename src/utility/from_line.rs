use crate::utility::*;

pub trait FromLine<P: Coordinate> {
    fn new_from_two_points_on_line(p1: P, p2: P) -> Self
    where
        Self: Sized,
    {
        Self::try_new_from_two_points_on_line(p1, p2).unwrap()
    }
    fn easy_new_from_two_points_on_line(p1: impl Into<P>, p2: impl Into<P>) -> Self
    where
        Self: Sized,
    {
        Self::new_from_two_points_on_line(p1.into(), p2.into())
    }
    fn try_new_from_two_points_on_line(p1: P, p2: P) -> Result<Self, String>
    where
        Self: Sized,
    {
        let line = TwoDifferentPoints::<P>::new(p1, p2);
        todo!()
        // Self::try_new_from_line(line)
    }
    fn new_from_line(line: impl Line<PointType = P>) -> Self
    where
        Self: Sized,
    {
        Self::try_new_from_line(line).unwrap()
    }
    fn try_new_from_line(line: impl Line<PointType = P>) -> Result<Self, String>
    where
        Self: Sized;
}
