use crate::utility::*;

/// angle is less than 180 degrees only
pub struct ConeFromPoint<PointType: Coordinate> {
    center: PointType,
    // angle_interval: NarrowAngleInterval,
}

#[derive(Clone, Copy, Debug)]
pub struct ConeEdgeIntersectionsWithCenteredUnitSquare {
    cw_edge: Option<HalfPlaneCuttingLocalSquare>,
    ccw_edge: Option<HalfPlaneCuttingLocalSquare>,
}

impl ConeEdgeIntersectionsWithCenteredUnitSquare {
    pub fn from_one_half_plane_cutting_origin_square(hp: HalfPlaneCuttingLocalSquare) -> Self {
        Self {
            cw_edge: Some(hp),
            ccw_edge: None,
        }
    }
}

impl ToAndFromArray<Option<HalfPlaneCuttingLocalSquare>, 2>
    for ConeEdgeIntersectionsWithCenteredUnitSquare
{
    fn array(&self) -> [Option<HalfPlaneCuttingLocalSquare>; 2] {
        [self.cw_edge, self.ccw_edge]
    }
    fn from_array(arr: [Option<HalfPlaneCuttingLocalSquare>; 2]) -> Self {
        let out = Self {
            cw_edge: arr[0],
            ccw_edge: arr[1],
        };
        assert!(!(out.cw_edge.is_none() && out.ccw_edge.is_none()));
        out
    }
}
impl<P: Coordinate> ConeFromPoint<P> {}

pub trait ConeOps {}

impl<P: Coordinate> ConeOps for ConeFromPoint<P> {}

impl QuarterTurnRotatable for ConeEdgeIntersectionsWithCenteredUnitSquare {
    fn quarter_rotated_ccw(&self, quarter_turns_ccw: impl Into<NormalizedOrthoAngle>) -> Self {
        todo!()
    }
}
