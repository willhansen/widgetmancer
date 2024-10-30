// use crate::utility::*;
use crate::signed_coordinate;
use crate::king_direction::KingDirection;
use core::hash::Hash;
use geo::Coord;
use crate::coordinate::*;

// use crate::*;

pub type ICoord = Coord<i32>;
pub struct IntCoordinate(ICoord);

impl IntCoordinate {
    pub const DOWN: Self = Self(coord(0, -1));
    pub const UP: Self = Self(coord(0, 1));
    pub const LEFT: Self = Self(coord(-1, 0));
    pub const RIGHT: Self = Self(coord(1, 0));

}


pub trait Operations:
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
impl<T> Operations for T where T: signed_coordinate::Operations<_DataType = i32, OnGrid = T> + Hash + Eq {}

impl From<KingDirection> for IntCoordinate {
    fn from(value: KingDirection) -> Self {
        value.step
    }
}

#[deprecated(note = "coordinates::king_length instead")]
pub fn king_step_distance(step: ICoord) -> u32 {
    step.x().abs().max(step.y().abs()) as u32
}

pub fn round_to_king_step(step: ICoord) -> ICoord {
    if step.square_length() == 0 {
        return step;
    }
    let radians_from_plus_x = step.to_f32().better_angle_from_x_axis();
    let eighth_steps_from_plus_x = (radians_from_plus_x.radians * 8.0 / TAU).round();
    let rounded_radians_from_plus_x = FAngle::radians(eighth_steps_from_plus_x * TAU / 8.0);

    let float_step = Vector2D::<f32, SquareGridInWorldFrame>::from_angle_and_length(
        rounded_radians_from_plus_x,
        1.5,
    );
    // 1.5 length to allow truncating down to 1 i32 in the diagonal case
    // because 1.5/sqrt(2) > 1.0

    // truncate towards zero intentionally
    float_step.to_i32()
}

pub fn revolve_square(
    moving_square: WorldSquare,
    pivot_square: WorldSquare,
    rotation: NormalizedOrthoAngle,
) -> WorldSquare {
    let rel_square = moving_square - pivot_square;
    pivot_square + rotation.rotate_vector(rel_square)
}

#[deprecated(note = "use Vector2D's to_array function instead")]
pub fn ith_projection_of_step(step: WorldStep, i: u32) -> WorldStep {
    match i {
        0 => WorldStep::new(step.x, 0),
        1 => WorldStep::new(0, step.y),
        _ => panic!("Too many dimensions: {}", i),
    }
}
#[deprecated(note = "use SignedCoordinate::position_on_axis instead")]
pub fn distance_of_step_along_axis(step: WorldStep, axis: OrthogonalDirection) -> i32 {
    step.project_onto_vector(axis.to_step()).dot(axis.to_step())
}
pub fn cross_correlate_squares_with_steps(
    squares: SquareSet,
    steps: HashSet<WorldStep>,
) -> HashMap<WorldSquare, u32> {
    let mut step_count_map = HashMap::<WorldSquare, u32>::new();
    squares.iter().for_each(|&square| {
        steps
            .iter()
            .map(|&diagonal_step| square + diagonal_step)
            .for_each(|step_square| *step_count_map.entry(step_square).or_default() += 1)
    });
    step_count_map
}
pub fn adjacent_king_steps(dir: WorldStep) -> StepSet {
    assert!(dir.is_king_step());
    if ORTHOGONAL_STEPS.contains(&dir) {
        if dir.x != 0 {
            HashSet::from([dir + STEP_UP, dir + STEP_DOWN])
        } else {
            HashSet::from([dir + STEP_LEFT, dir + STEP_RIGHT])
        }
    } else {
        let no_x = coord2(0, dir.y);
        let no_y = coord2(dir.x, 0);
        HashSet::from([no_x, no_y])
    }
}
