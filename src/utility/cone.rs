use crate::utility::*;
use derive_getters::Getters;
use derive_more::Constructor;

pub type ConeFromSquare = ThingAtCoord<Cone, WorldSquare>;

// TODO: There's got to be a better way to get convenient names for general fields
impl ConeFromSquare {
    pub fn square(&self) -> WorldSquare {
        *self.coord()
    }
    pub fn cone(&self) -> Cone {
        *self.thing()
    }
}

// TODO: maybe generalize to ConvexUnboundedPolygon? (just a collection of half planes, where a point is inside if it is on all the half planes)
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

pub trait ConeOps {}

impl ConeOps for ConeFromSquare {}

impl QuarterTurnRotatable for ConeEdgeIntersectionsWithCenteredUnitSquare {
    fn quarter_rotated_ccw(&self, quarter_turns_ccw: impl Into<NormalizedOrthoAngle>) -> Self {
        todo!()
    }
}
