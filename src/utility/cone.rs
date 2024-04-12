use crate::utility::*;

pub struct Cone([HalfPlane; 2]);

pub struct ConeSquareIntersection([Option<HalfPlaneCuttingLocalSquare>; 2]);

impl Cone {}

pub trait ConeOps {}

impl ConeOps for Cone {}
