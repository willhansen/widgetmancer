use crate::utility::*;

pub trait LineConstructors: LineOps + TryFromTwoPoints<Self::PointType> + Sized {
    fn new_from_two_unordered_points_on_line(p1: Self::PointType, p2: Self::PointType) -> Self {
        Self::try_new_from_two_points_on_line(p1, p2).unwrap()
    }
    fn try_from_array_of_two_points_on_line(p: [Self::PointType; 2]) -> Result<Self, String> {
        Self::try_new_from_two_points_on_line(p[0], p[1])
    }
    fn easy_from_two_points_on_line(
        p1: impl Into<Self::PointType>,
        p2: impl Into<Self::PointType>,
    ) -> Self {
        Self::new_from_two_unordered_points_on_line(p1.into(), p2.into())
    }
    fn try_new_from_two_points_on_line(
        p1: Self::PointType,
        p2: Self::PointType,
    ) -> Result<Self, String> {
        Self::try_from_two_points_allowing_snap_along_line(p1, p2)
    }
    fn new_from_line(line: impl LineOps<PointType = Self::PointType>) -> Self {
        Self::try_new_from_line(line).unwrap()
    }
    fn try_new_from_line(line: impl LineOps<PointType = Self::PointType>) -> Result<Self, String> {
        let p = line.two_different_arbitrary_points_on_line();
        Self::try_from_array_of_two_points_on_line(p)
    }
}

impl<L> LineConstructors for L where L: LineOps + TryFromTwoPoints<L::PointType> {}
