// use crate::utility::*;
use angles::*;
use crate::coordinate;

pub trait Operations:
    coordinate::Operations<DataType = Self::_DataType>
    + Neg<Output = Self>
    // TODO: put on the SignedCoordinateConstructor trait instead
    + From<NormalizedOrthoAngle>
    + From<OrthogonalDirection>
    + From<(Self::_DataType, Self::_DataType)>
{
    type _DataType: num::Signed + Copy + PartialOrd + Debug;
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
    fn moved(&self, dir: OrthogonalDirection, length: Self::DataType) -> Self {
        *self + dir.to_step::<Self>() * length
    }
    fn position_on_orthogonal_axis(&self, axis: impl Into<OrthogonalDirection>) -> Self::DataType {
        let axis_vector: Self = axis.into().to_step();
        self.dot(axis_vector)
    }
    fn orthogonal_angle(&self) -> Result<NormalizedOrthoAngle, String> {
        <NormalizedOrthoAngle as ortho_angle::Operations>::try_from_coordinate(*self)
    }
}

impl<T> Operations for T
where
    T: coordinate::Operations + Neg<Output = Self> + From<NormalizedOrthoAngle> + From<OrthogonalDirection>,
    T::DataType: num::Signed,
{
    type _DataType = T::DataType;
}

impl<V> QuarterTurnRotatable for V
where
    V: Operations,
{
    fn quarter_rotated_ccw(&self, angle: impl Into<NormalizedOrthoAngle>) -> Self {
        // if self.is_absolute() {
        //     return *self;
        // }
        let angle = angle.into();
        Self::new(
            self.x() * angle.cos() - self.y() * angle.sin(),
            self.x() * angle.sin() + self.y() * angle.cos(),
        )
    }
}

// TODO: Incorporate this into the quarter QuarterTurnRotatable trait?
pub fn get_8_octant_transforms_of<PointType: CoordinateTrait>(v: PointType) -> Vec<PointType> {
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
