use crate::utility::*;
pub trait TryFromTwoPoints<P: Coordinate>: Sized {
    fn try_from_two_exact_points(p1: P, p2: P) -> Result<Self, String>;
    // TODO: is this equivalent to TryFromDirectedLine?
    fn try_from_two_points_allowing_snap_along_line(p1: P, p2: P) -> Result<Self, String> {
        Self::try_from_two_exact_points(p1, p2)
    }
}
