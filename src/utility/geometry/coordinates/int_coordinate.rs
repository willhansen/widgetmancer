use crate::utility::*;

pub trait int_coordinate::Operations:
    signed_coordinate::Operations<_DataType = i32, OnGrid = Self> + Hash + Eq
{
    fn is_orthogonal_king_step(&self) -> bool {
        self.square_length() == 1
    }

    fn is_diagonal_king_step(&self) -> bool {
        self.square_length() == 2
    }
    fn is_king_step(&self) -> bool {
        self.is_orthogonal_king_step() || self.is_diagonal_king_step()
    }
    fn is_even(&self) -> bool {
        (self.x() + self.y()) % 2 == 0
    }
    fn is_odd(&self) -> bool {
        !self.is_even()
    }
}
// TODO: convert to auto trait when stable
// TODO: Same trait bounds are copy pasted from main trait declaration.  Factor them out somehow.
impl<T> int_coordinate::Operations for T where T: signed_coordinate::Operations<_DataType = i32, OnGrid = T> + Hash + Eq {}

trait_alias!(pub trait WorldIntCoordinate = int_coordinate::Operations< UnitType = SquareGridInWorldFrame>);

trait_alias!(pub trait SignedIntCoordinate = int_coordinate::Operations + signed_coordinate::Operations);
