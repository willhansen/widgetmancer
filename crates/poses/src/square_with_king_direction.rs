
use misc_utilities::*;
use coordinates::*;
use crate::*;

#[derive(Clone, Hash, Eq, PartialEq, Debug, Copy)]
pub struct SquareWithKingDir {
    square: ICoord,
    direction: KingDirection,
}

impl SquareWithKingDir {

    pub fn square(&self) -> ICoord {
        self.square
    }
    pub fn direction(&self) -> KingDirection {
        self.direction
    }
    pub fn new(square: ICoord, direction: KingDirection) -> Self {
        SquareWithKingDir { square, direction }
    }
    pub fn from_square_and_step(square: ICoord, direction: KingDirection) -> SquareWithKingDir {
        Self::new(square, direction.into())
    }
    pub fn tuple(&self) -> (ICoord, KingDirection) {
        (self.square, self.direction)
    }
    pub fn stepped(&self) -> SquareWithKingDir {
        SquareWithKingDir::from_square_and_step(
            self.square + self.direction.step(),
            self.direction.into(),
        )
    }
}

impl From<SquareWithOrthogonalDirection> for SquareWithKingDir {
    fn from(value: SquareWithOrthogonalDirection) -> Self {
        SquareWithKingDir {
            square: value.square(),
            direction: value.direction().into(),
        }
    }
}

impl From<(ICoord, KingDirection)> for SquareWithKingDir {
    fn from(value: (ICoord, KingDirection)) -> Self {
        Self::new(value.0, value.1)
    }
}
impl From<SquareWithKingDir> for (ICoord, KingDirection) {
    fn from(value: SquareWithKingDir) -> (ICoord, KingDirection) {
        (value.square, value.direction)
    }
}
