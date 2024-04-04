use crate::utility::*;

pub trait LineConstructors<P: SignedCoordinate>: TryFromTwoPoints<P> {
    fn new_from_two_unordered_points_on_line(p1: P, p2: P) -> Self
    where
        Self: Sized,
    {
        Self::try_new_from_two_points_on_line(p1, p2).unwrap()
    }
    fn try_from_array_of_two_points_on_line(p: [P; 2]) -> Result<Self, String>
    where
        Self: Sized,
    {
        Self::try_new_from_two_points_on_line(p[0], p[1])
    }
    fn easy_new_from_two_points_on_line(p1: impl Into<P>, p2: impl Into<P>) -> Self
    where
        Self: Sized,
    {
        Self::new_from_two_unordered_points_on_line(p1.into(), p2.into())
    }
    fn try_new_from_two_points_on_line(p1: P, p2: P) -> Result<Self, String>
    where
        Self: Sized,
    {
        let line = TwoDifferentPoints::<P>::new(p1, p2);
        Self::try_new_from_line(line)
    }
    fn new_from_line(line: impl LineOps<PointType = P>) -> Self
    where
        Self: Sized,
    {
        Self::try_new_from_line(line).unwrap()
    }
    fn try_new_from_line(line: impl LineOps<PointType = P>) -> Result<Self, String>
    where
        Self: Sized;
}
