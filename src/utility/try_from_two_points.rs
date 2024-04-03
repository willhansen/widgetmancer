use crate::utility::*;
pub trait TryFromTwoPoints<P: Coordinate>: Sized {
    fn try_from_two_exact_points(p1: P, p2: P) -> Result<Self, String>;
    fn from_two_exact_points(p1: P, p2: P) -> Self {
        Self::try_from_two_exact_points(p1, p2).unwrap()
    }
    fn from_array_of_two_exact_points(p: [P; 2]) -> Self {
        Self::try_from_array_of_two_exact_points(p).unwrap()
    }
    fn try_from_array_of_two_exact_points(p: [P; 2]) -> Result<Self, String> {
        Self::try_from_two_exact_points(p[1], p[2])
    }
    fn easy_from_two_exact_points(p1: impl Into<P>, p2: impl Into<P>) -> Self {
        Self::from_two_exact_points(p1.into(), p2.into())
    }
    /// NOTE: This is a very similar use case as TryFromDirectedLine::from_two_ordered_points_on_line.
    /// There is a difference between "allow snapping along one axis" and "define a line, and then use the line" though.  in the second case, the hinting power of giving specific points is intentionally discarded.
    fn try_from_two_points_allowing_snap_along_line(p1: P, p2: P) -> Result<Self, String> {
        Self::try_from_two_exact_points(p1, p2)
    }
    fn from_two_points_allowing_snap_along_line(p1: P, p2: P) -> Self {
        Self::try_from_two_points_allowing_snap_along_line(p1, p2).unwrap()
    }
}
