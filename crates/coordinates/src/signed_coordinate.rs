// use crate::utility::*;
use angles::*;
use crate::coordinate;
use std::ops::Neg;
use crate::*;

pub trait Operations:
    coordinate::Operations<DataType = Self::_DataType>
    + Neg<Output = Self>
    // TODO: put on the SignedCoordinateConstructor trait instead
    // + From<NormalizedOrthoAngle>
    // + From<OrthogonalDirection>
    // + From<(Self::_DataType, Self::_DataType)>
    + QuarterTurnRotatable
{
    type _DataType: num::Signed + Copy + PartialOrd + Debug + Display;
    fn flip_x(&self) -> Self {
        Self::new(-self.x(), self.y())
    }
    fn flip_y(&self) -> Self {
        Self::new(self.x(), -self.y())
    }
    fn right() -> Self {
        Self::new(Self::DataType::one(), Self::DataType::zero())
    }
    fn left() -> Self {
        Self::new(-Self::DataType::one(), Self::DataType::zero())
    }
    fn up() -> Self {
        Self::new(Self::DataType::zero(), Self::DataType::one())
    }
    fn down() -> Self {
        Self::new(Self::DataType::zero(), -Self::DataType::one())
    }
    // TODO: allow non-orthogonal directions
    fn stepped(&self, dir: OrthogonalDirection) -> Self {
        self.moved(dir, Self::DataType::one())
    }
    fn from_orthogonal_direction(dir: OrthogonalDirection) -> Self {
        dir.xy().into()
    }
    fn moved(&self, dir: OrthogonalDirection, length: Self::_DataType) -> Self {
        *self + Self::from(dir) * length
    }
    fn position_on_orthogonal_axis(&self, axis: OrthogonalDirection) -> Self::_DataType {
        let axis_vector: Self = axis.into();
        self.dot(axis_vector)
    }
    fn orthogonal_angle(&self) -> Result<NormalizedOrthoAngle, String> {
        if !self.is_orthogonal() {
            return Err(format!("Not orthogonal: {}", self.to_string()));
        }
        Ok(NormalizedOrthoAngle::from_quarter_turns_ccw(
            if self.x() == Self::_DataType::zero() {
                if self.y() > Self::_DataType::zero() {
                    1
                } else {
                    3
                }
            } else if self.x() > Self::_DataType::zero() {
                0
            } else {
                2
            },
        ))
    }
}

impl<T> Operations for T
where
    T: coordinate::Operations +
    Neg<Output = Self>, 
    // + From<NormalizedOrthoAngle> 
    // + From<OrthogonalDirection> 
    // + From<(<T as coordinate::Operations>::DataType, <T as coordinate::Operations>::DataType)>,
    T::DataType: num::Signed,
{
    type _DataType = T::DataType;
}


// TODO: Incorporate this into the quarter QuarterTurnRotatable trait?
pub fn get_8_octant_transforms_of<PointType: coordinate::Operations>(v: PointType) -> Vec<PointType> {
    let transpose = PointType::new(v.y(), v.x());
    vec![v, transpose]
        .into_iter()
        .map(|x| x.quadrant_rotations_going_ccw())
        .flatten()
        .collect()
}

// TODO: why is this not just the negative operator?
pub fn reversed<T: Copy>(v: Vec<T>) -> Vec<T> {
    let mut new_v = v.clone();
    new_v.reverse();
    new_v
}

pub fn three_points_are_clockwise<P>(a: P, b: P, c: P) -> bool
where
    P: signed_coordinate::Operations,
    P::DataType: PartialOrd, // TODO: should be implied by SignedCoordinate
{
    let ab = b - a;
    let ac = c - a;
    ab.cross(ac) < P::DataType::zero()
}
