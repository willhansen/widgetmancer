// use crate::utility::*;
use crate::signed_coordinate;
use crate::king_direction::KingDirection;
use core::hash::Hash;
use geo::Coord;
use angles::*;
// use crate::coordinate::*;
use crate::*;
use std::collections::{HashMap, HashSet};

// use crate::*;

pub type ICoord = Coord<i32>;

pub type GridSet = HashSet<ICoord>;

pub trait ICoordConsts {
    const UP: ICoord = coord(0, 1);
    const DOWN: ICoord = coord(0, -1);
    const RIGHT: ICoord = coord(1, 0);
    const LEFT: ICoord = coord(-1, 0);
    const ORTHOGONAL_STEPS: [ICoord; 4] = [Self::RIGHT, Self::UP, Self::LEFT, Self::DOWN];
}
impl ICoordConsts for ICoord {}


pub trait Operations:
    signed_coordinate::Operations<_DataType = i32> + Hash + Eq
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
impl<T> Operations for T where T: signed_coordinate::Operations<_DataType = i32> + Hash + Eq {}

// // TODO: There's got to be an easier way to dodge the Orphan Rule
// pub trait FromKingDirectionable {}
// impl FromKingDirectionable for ICoord {}
// impl<T> From<KingDirection> for T where T: FromKingDirectionable {
//     fn from(value: KingDirection) -> Self {
//         value.step()
//     }
// }

#[deprecated(note = "coordinates::king_length instead")]
pub fn king_step_distance(step: ICoord) -> u32 {
    step.x().abs().max(step.y().abs()) as u32
}

pub fn round_to_king_step(step: ICoord) -> ICoord {
    if step.square_length() == 0 {
        return step;
    }
    let radians_from_plus_x = step.to_f32().better_angle_from_x_axis();
    let eighth_steps_from_plus_x = (radians_from_plus_x.radians * 8.0 / f32::consts::TAU).round();
    let rounded_radians_from_plus_x = FAngle::radians(eighth_steps_from_plus_x * f32::consts::TAU / 8.0);

    let float_step = crate::FCoord::from_angle_and_length(
        rounded_radians_from_plus_x,
        1.5,
    );
    // 1.5 length to allow truncating down to 1 i32 in the diagonal case
    // because 1.5/sqrt(2) > 1.0

    // truncate towards zero intentionally
    float_step.to_i32()
}

pub fn revolve_square(
    moving_square: ICoord,
    pivot_square: ICoord,
    rotation: NormalizedOrthoAngle,
) -> ICoord {
    let rel_square = moving_square - pivot_square;
    pivot_square + rotation.rotate_vector(rel_square)
}

#[deprecated(note = "use Vector2D's to_array function instead")]
pub fn ith_projection_of_step(step: ICoord, i: u32) -> ICoord {
    match i {
        0 => ICoord::new(step.x, 0),
        1 => ICoord::new(0, step.y),
        _ => panic!("Too many dimensions: {}", i),
    }
}
#[deprecated(note = "use SignedCoordinate::position_on_axis instead")]
pub fn distance_of_step_along_axis(step: ICoord, axis: OrthogonalDirection) -> i32 {
    step.project_onto_vector(axis.to_step()).dot(axis.to_step())
}
pub fn cross_correlate_squares_with_steps(
    squares: GridSet,
    steps: GridSet,
) -> HashMap<ICoord, u32> {
    let mut step_count_map = HashMap::<ICoord, u32>::new();
    squares.iter().for_each(|&square| {
        steps
            .iter()
            .map(|&diagonal_step| square + diagonal_step)
            .for_each(|step_square| *step_count_map.entry(step_square).or_default() += 1)
    });
    step_count_map
}
pub fn adjacent_king_steps(dir: ICoord) -> GridSet {
    assert!(dir.is_king_step());
    if ICoord::ORTHOGONAL_STEPS.contains(&dir) {
        if dir.x != 0 {
            HashSet::from([dir + ICoord::UP, dir + ICoord::DOWN])
        } else {
            HashSet::from([dir + ICoord::LEFT, dir + ICoord::RIGHT])
        }
    } else {
        let no_x = coord(0, dir.y);
        let no_y = coord(dir.x, 0);
        HashSet::from([no_x, no_y])
    }
}
