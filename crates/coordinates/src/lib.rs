// automod::dir!(pub "src/coordinates");
mod coordinates;
use crate::coordinates::coordinate::*;
use angles::normalized_ortho_angle::NormalizedOrthoAngle;
use angles::ortho_angle::{OrthoAngleOperations, OrthoAngle};
use angles::float_angle::FAngle;
use geo::Coord;
use geo::coord;

// pub_use!(
//     // float_coordinate,
//     // int_coordinate,
//     king_world_step,
//     // signed_coordinate,
//     // unsigned_coordinate,
//     coordinate,
// );

// use map_macro::hash_set;
use std::{
    collections::{HashMap, HashSet},
    f32::consts::{PI, TAU},
    // fmt::Display,
    // marker::PhantomData,
    // ops::{Add, Div, Mul, Neg, Sub},
};

// use typenum::{Sum, Unsigned};

// use derive_more;
// use itertools::Itertools;
// use num::{One, Signed, Zero};
// use ordered_float::OrderedFloat;
// use rand::{rngs::StdRng, Rng};
// use static_assertions::{assert_impl_all, assert_not_impl_any};

fn coord<T: geo::GeoNum> (x: T, y: T) -> geo::Coord<T> {
    coord!{x: x, y: y}
}

pub type ICoord = Coord<i32>;
pub type FCoord = Coord<f32>;

pub const DOWN_I: ICoord = coord(0, -1);
pub const UP_I: ICoord = coord(0, 1);
pub const LEFT_I: ICoord = coord(-1, 0);
pub const RIGHT_I: ICoord = coord(1, 0);


// TODO: why does using newtypes on these cause rust-analyzer memory to skyrocket? // TODO: replace these with versions that properly incorporate addition and subtraction relativity #[derive( Clone, Copy, Hash, Eq, PartialEq, Debug, derive_more::Add, derive_more::Sub, derive_more::Neg, )]
// pub struct Point2D<DataType, UnitType>(euclid::Point2D<DataType, UnitType>);
// #[derive(Clone, Copy, Hash, Eq, PartialEq, Debug)]
// pub struct Vector2D<DataType, UnitType>(euclid::Vector2D<DataType, UnitType>);



// TODO: there's got to be a better way to extract associated types
// pub type DataTypeOf<P> = <P as coordinate::Operations>::DataType;




// macro_rules! make_coordinate_datatype_cast_function {
//     ($name:ident, $data_type:ty, $coord_type:ty) => {
//         fn $name(&self) -> $coord_type {
//             <$coord_type>::new(
//                 num::NumCast::from(self.x()).unwrap(),
//                 num::NumCast::from(self.y()).unwrap(),
//             )
//         }
//     };
// }


// impl<O, T> From<O> for Coord<T>
// where
//     O: OrthoAngleOperations,
//     T: DataTypeReqs + num::Signed,
// {
//     fn from(value: O) -> Self {
//         let (x, y) = value.xy();
//         coord(x, y)
//     }
// }

// impl<T> From<OrthogonalDirection> for Coord<T>
// where
//     T: coordinate::DataTypeReqs + num::Signed,
// {
//     fn from(value: OrthogonalDirection) -> Self {
//         value.angle().into()
//     }
// }



// TODO: uncomment when newtyping Vector2D and Point2D
// macro_rules! impl_from_tuple {
//     ($type:ident) => {
//         impl<T, U> From<(T, T)> for $type<T, U>
//         where
//             $type<T,U>: Coordinates<DataType = T>,
//         {
//             fn from(value: (T, T)) -> Self {
//                 <$type<T,U>>::new(value.0, value.1)
//             }
//         }
//     }
// }
// impl_from_tuple!(Point2D);
// impl_from_tuple!(Vector2D);

// macro_rules! delegate_unary{
//     ($type:ident, $trait:ident, $func:ident) => {
//         impl<T,U> $trait for $type<T,U> {
//             type Output = Self;
//             fn $func(&self) -> Self {
//                 Self(self.0.$func())
//             }
//         }
//     }
// }
// delegate_unary!(Point2D, Neg, neg);
// delegate_unary!(Vector2D, Neg, neg);

// macro_rules! delegate_asymmetric_binary{
//     ($type:ident, $trait:ident, $func:ident) => {
//         impl<T,U> $trait<T> for $type<T,U> {
//             type Output = Self;
//             fn $func(&self, rhs: T) -> Self {
//                 Self($func(self.0, rhs))
//             }
//         }
//     }
// }
// delegate_asymmetric_binary!(Point2D, Mul, mul);
// delegate_asymmetric_binary!(Vector2D, Mul, mul);
// delegate_asymmetric_binary!(Point2D, Div, div);
// delegate_asymmetric_binary!(Vector2D, Div, div);

// TODO: delete commented code
// TODO: clean these up (with the trait alias macro?)
// TODO: convert to auto trait when stable
// pub trait AbsoluteCoordinate: coordinates {}
// impl<COORD> AbsoluteCoordinate for COORD where
//     COORD: Coordinates<RelativityComplement = Self::RelativeVersionOfSelf>
// {
// }

// // TODO: convert to auto trait when stable
// pub trait RelativeCoordinate: coordinates {}
// impl<COORD> RelativeCoordinate for COORD where
//     COORD: Coordinates<RelativityComplement = Self::AbsoluteVersionOfSelf>
// {
// }



// pub fn sign2d<U>(point: Point2D<f32, U>) -> Point2D<f32, U> {
//     point2(sign_f32(point.x()), sign_f32(point.y()))
// }

// pub fn fraction_part<U>(point: Point2D<f32, U>) -> Point2D<f32, U> {
//     point - point.round()
// }


// TODO: make a coordinates method
pub fn get_8_octant_transforms_of<PointType: CoordNum>(v: PointType) -> Vec<PointType> {
    let transpose = PointType::new(v.y(), v.x());
    vec![v, transpose]
        .into_iter()
        .map(|x| x.quadrant_rotations_going_ccw())
        .flatten()
        .collect()
}


pub fn reversed<T: Copy>(v: Vec<T>) -> Vec<T> {
    let mut new_v = v.clone();
    new_v.reverse();
    new_v
}
#[deprecated(note = "coordinates::king_length instead")]
pub fn king_step_distance(step: ICoord) -> u32 {
    step.x().abs().max(step.y().abs()) as u32
}
#[deprecated(note = "coordinates::king_length instead")]
pub fn king_move_distance(step: FCoord) -> f32 {
    step.x().abs().max(step.y().abs())
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

pub fn seeded_rand_radial_offset<P: float_coordinate::Operations>(rng: &mut StdRng, radius: f32) -> P {
    let mut v = P::new(10.0, 10.0);
    while v.square_length() > 1.0 {
        v = P::new(rng.gen_range(-1.0..=1.0), rng.gen_range(-1.0..=1.0));
    }
    v * radius
}

pub fn rand_radial_offset(radius: f32) -> default::Vector2D<f32> {
    seeded_rand_radial_offset(&mut get_new_rng(), radius)
}
pub fn random_unit_vector() -> FCoord {
    let angle = random_angle();
    FCoord::unit_vector_from_angle(angle)
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

pub fn assert_about_eq_2d<P: float_coordinate::Operations>(p1: P, p2: P) {
    p1.check_about_eq(p2).unwrap();
}
pub fn sorted_left_to_right(faces: [OrthogonalDirection; 2]) -> [OrthogonalDirection; 2] {
    assert_ne!(faces[0], faces[1]);
    assert_ne!(faces[0], -faces[1]);
    if faces[0] == faces[1].quarter_rotated_ccw(NormalizedOrthoAngle::new_from_quarter_turns(1)) {
        faces
    } else {
        [faces[1], faces[0]]
    }
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
// TODO: move RigidlyTransformable to its own file to prevent super:: imports
impl RigidlyTransformable for WorldSquare {
    fn apply_rigid_transform(&self, tf: RigidTransform) -> Self {
        revolve_square(*self, tf.start_pose.square(), tf.rotation()) + tf.translation()
    }
}

impl QuarterTurnRotatable for Angle<f32> {
    fn quarter_rotated_ccw(&self, quarter_turns_ccw: impl Into<NormalizedOrthoAngle>) -> Self {
        standardize_angle_with_zero_mid(Angle::radians(
            self.radians + PI / 2.0 * quarter_turns_ccw.into().quarter_turns() as f32,
        ))
    }
}
pub fn furthest_apart_points<P: float_coordinate::Operations>(points: Vec<P>) -> [P; 2] {
    assert!(points.len() >= 2);
    let furthest = points
        .iter()
        .combinations(2)
        .max_by_key(|two_points: &Vec<&P>| OrderedFloat((*two_points[0] - *two_points[1]).length()))
        .unwrap();
    let furthest_values: Vec<P> = furthest.into_iter().copied().collect();
    furthest_values.try_into().unwrap()
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

pub fn two_points_are_ccw_with_origin<P: signed_coordinate::Operations>(a: P, b: P) -> bool
where
    P::DataType: PartialOrd, // TODO: should be implied by SignedCoordinate
{
    a.cross(b) > P::DataType::zero()
}

pub fn two_sorted_going_ccw(v: [WorldMove; 2]) -> [WorldMove; 2] {
    if two_points_are_ccw_with_origin(v[0], v[1]) {
        v
    } else {
        [v[1], v[0]]
    }
}

pub fn opposite_angle(a: FAngle) -> FAngle {
    a + FAngle::degrees(180.0)
}

pub fn check_vectors_in_ccw_order(
    v: impl IntoIterator<Item = impl Into<WorldMove>>,
) -> OkOrMessage {
    v.into_iter()
        .map(|x| x.into())
        .tuple_windows()
        .map(|(a, b)| match two_points_are_ccw_with_origin(a, b) {
            true => Ok(()),
            false => Err(format!(
                "These two points not in order: \na: {}\nb: {}",
                a.to_string(),
                b.to_string()
            )),
        })
        .collect()
}
pub fn on_line<P: coordinate::Operations>(a: P, b: P, c: P) -> bool {
    let ab = b - a;
    let ac = c - a;
    ab.cross(ac) == P::DataType::zero()
}

pub fn on_line_in_this_order<P: float_coordinate::Operations>(a: P, b: P, c: P) -> bool {
    on_line(a, b, c) && (a - b).length() < (a - c).length()
}

pub fn point_is_in_centered_unit_square_with_tolerance<U>(
    point: impl Into<Point2D<f32, U>>,
    tolerance: f32,
) -> BoolWithPartial {
    assert!(tolerance >= 0.0);
    let vec = point.into();
    BoolWithPartial::from_less_than_with_tolerance(king_move_distance(vec), 0.5, tolerance)
}

pub fn corner_points_of_centered_unit_square<P: float_coordinate::Operations>() -> [P; 4] {
    <P::OnGrid as FancyZero>::zero().square_corners()
}

#[cfg(test)]
mod tests {

    use ntest::{assert_about_eq, assert_false, assert_true, timeout};
    use pretty_assertions::{assert_eq, assert_ne};

    use crate::utility::{STEP_DOWN_LEFT, STEP_DOWN_RIGHT, STEP_UP_LEFT, STEP_UP_RIGHT, STEP_ZERO};

    use super::*;
    #[test]
    fn test_round_to_kingstep() {
        assert_eq!(
            WorldStep::new(0, 0),
            round_to_king_step(WorldStep::new(0, 0)),
            "zero to zero"
        );
        assert_eq!(
            WorldStep::new(1, 0),
            round_to_king_step(WorldStep::new(5, 0)),
            "reduce length"
        );
        assert_eq!(
            WorldStep::new(0, -1),
            round_to_king_step(WorldStep::new(5, -300)),
            "snap to orthogonal"
        );
        assert_eq!(
            WorldStep::new(-1, 1),
            round_to_king_step(WorldStep::new(-30, 25)),
            "snap to diagonal"
        );
    }
    #[test]
    fn test_clockwise() {
        assert!(three_points_are_clockwise::<WorldPoint>(
            point2(0.0, 0.0),
            point2(0.0, 1.0),
            point2(1.0, 0.0),
        ));
        assert_false!(three_points_are_clockwise::<WorldPoint>(
            point2(0.0, 0.0),
            point2(0.0, 1.0),
            point2(-0.1, -10.0)
        ));
    }
    #[test]
    fn test_adjacent_king_steps() {
        assert_eq!(
            adjacent_king_steps(STEP_UP),
            vec![STEP_UP_RIGHT, STEP_UP_LEFT].into_iter().collect()
        );
        assert_eq!(
            adjacent_king_steps(STEP_RIGHT),
            vec![STEP_UP_RIGHT, STEP_DOWN_RIGHT].into_iter().collect()
        );
        assert_eq!(
            adjacent_king_steps(STEP_DOWN_LEFT),
            vec![STEP_DOWN, STEP_LEFT].into_iter().collect()
        );
    }

    #[test]
    fn test_rotate_zero_vector() {
        assert_eq!(
            WorldMove::new(0.0, 0.0).rotate_vect(Angle::radians(PI)),
            coord2(0.0, 0.0)
        );
    }
    #[test]
    fn test_angle_from_x_axis() {
        assert_about_eq!(
            default::Vector2D::new(0.5, 0.5)
                .better_angle_from_x_axis()
                .to_degrees(),
            45.0
        );
        assert_about_eq!(
            default::Vector2D::new(0.0, 0.5)
                .better_angle_from_x_axis()
                .to_degrees(),
            90.0
        );
        assert_about_eq!(
            default::Vector2D::new(0.0, -0.5)
                .better_angle_from_x_axis()
                .to_degrees(),
            -90.0
        );
        assert_about_eq!(
            default::Vector2D::new(1.0, 0.0)
                .better_angle_from_x_axis()
                .to_degrees(),
            0.0
        );
        assert_about_eq!(
            default::Vector2D::new(-1.0, 0.0)
                .better_angle_from_x_axis()
                .to_degrees(),
            180.0
        );
    }

    #[test]
    fn test_built_in_angle_from_x_axis_can_not_be_trusted() {
        assert!(
            (default::Vector2D::new(0.5, 0.5)
                .angle_from_x_axis()
                .to_degrees()
                - 45.0)
                .abs()
                > 0.01
        );
    }

    #[test]
    fn test_standardize_angle() {
        assert_about_eq!(
            standardize_angle_with_zero_mid(Angle::<f32>::degrees(75.0)).radians,
            standardize_angle_with_zero_mid(Angle::<f32>::degrees(75.0 - 360.0)).radians
        );
    }
    #[test]
    fn test_revolve_square() {
        assert_eq!(
            revolve_square(
                point2(3, 4),
                point2(5, 5),
                NormalizedOrthoAngle::new_from_quarter_turns(3),
            ),
            point2(4, 7)
        );
    }

    #[test]
    fn test_quarter_turns_from_vectors() {
        assert_eq!(
            NormalizedOrthoAngle::from_start_and_end_directions(STEP_UP, STEP_UP),
            NormalizedOrthoAngle::new_from_quarter_turns(0)
        );
        assert_eq!(
            NormalizedOrthoAngle::from_start_and_end_directions(STEP_UP, STEP_RIGHT),
            NormalizedOrthoAngle::new_from_quarter_turns(3)
        );
        assert_eq!(
            NormalizedOrthoAngle::from_start_and_end_directions(STEP_LEFT, STEP_RIGHT),
            NormalizedOrthoAngle::new_from_quarter_turns(2)
        );
        assert_eq!(
            NormalizedOrthoAngle::from_start_and_end_directions(STEP_DOWN_LEFT, STEP_DOWN_RIGHT,),
            NormalizedOrthoAngle::new_from_quarter_turns(1)
        );
    }
    #[test]
    fn test_project_step_onto_axis() {
        assert_eq!(distance_of_step_along_axis(STEP_UP_LEFT * 8, RIGHT), -8);
    }
    #[test]
    fn test_relative_points_in_ccw_order() {
        assert_true!(two_points_are_ccw_with_origin(
            STEP_RIGHT.to_f32(),
            STEP_UP.to_f32()
        ));
        assert_true!(two_points_are_ccw_with_origin(
            STEP_UP.to_f32(),
            STEP_LEFT.to_f32()
        ));
        // slight diff
        assert_true!(two_points_are_ccw_with_origin(
            STEP_UP.to_f32(),
            STEP_UP.to_f32() + WorldMove::new(-0.001, 0.0)
        ));
        assert_true!(two_points_are_ccw_with_origin(
            STEP_UP.to_f32(),
            STEP_DOWN.to_f32() + WorldMove::new(-0.001, 0.0)
        ));

        // across
        assert_false!(two_points_are_ccw_with_origin(
            STEP_UP.to_f32(),
            STEP_DOWN.to_f32()
        ));

        assert_false!(two_points_are_ccw_with_origin(
            STEP_UP.to_f32(),
            STEP_RIGHT.to_f32()
        ));
        assert_false!(two_points_are_ccw_with_origin(
            STEP_ZERO.to_f32(),
            STEP_RIGHT.to_f32()
        ));
    }
    #[test]
    fn test_point_is_in_centered_unit_square__simple_true() {
        assert!(
            point_is_in_centered_unit_square_with_tolerance(WorldPoint::new(0.0, 0.0), 0.0)
                .is_true()
        )
    }
    #[test]
    fn test_point_is_in_centered_unit_square__simple_false() {
        assert!(
            point_is_in_centered_unit_square_with_tolerance(WorldPoint::new(5.0, 0.0), 0.0)
                .is_false()
        )
    }
    #[test]
    fn test_point_is_in_centered_unit_square__outside_square__inside_tolerance() {
        assert!(
            point_is_in_centered_unit_square_with_tolerance(WorldPoint::new(0.51, 0.0), 0.2)
                .is_partial()
        )
    }
    #[test]
    fn test_point_is_in_centered_unit_square__inside_square__inside_tolerance() {
        assert!(
            point_is_in_centered_unit_square_with_tolerance(WorldPoint::new(0.49, 0.0), 0.2)
                .is_partial()
        )
    }
    #[test]
    fn test_point_is_in_centered_unit_square__outside_square_diagonally__inside_tolerance() {
        assert!(
            point_is_in_centered_unit_square_with_tolerance(WorldPoint::new(0.51, 0.51), 0.2)
                .is_partial()
        )
    }
    #[test]
    fn test_point_is_in_centered_unit_square__inside_square_diagonally__inside_tolerance() {
        assert!(
            point_is_in_centered_unit_square_with_tolerance(WorldPoint::new(0.49, 0.49), 0.2)
                .is_partial()
        )
    }
    #[test]
    #[ignore = "Relativity is unimplemented for coordinates for the time being"]
    fn test_relative_and_absolute() {
        let point = WorldPoint::new(4.0, 5.0);
        let moov = WorldMove::new(4.0, 5.0);
        let square = WorldSquare::new(4, 5);
        let step = WorldStep::new(4, 5);

        // TODO: uncomment when relativity is re-implemented
        // assert_true!(point.is_absolute());
        // assert_false!(point.is_relative());
        // assert_true!(moov.is_relative());
        // assert_false!(moov.is_absolute());

        // assert_true!(square.is_absolute());
        // assert_false!(square.is_relative());
        // assert_true!(step.is_relative());
        // assert_false!(step.is_absolute());

        // assert_eq!(point.as_relative(), moov);
        // assert_eq!(point.as_absolute(), point);
        // assert_eq!(moov.as_absolute(), point);
        // assert_eq!(moov.as_relative(), moov);

        // assert_eq!(square.as_relative(), step);
        // assert_eq!(square.as_absolute(), square);
        // assert_eq!(step.as_absolute(), square);
        // assert_eq!(step.as_relative(), step);
    }
    #[test]
    fn test_king_length() {
        assert_eq!(WorldStep::new(2, 3).king_length(), 3);
        assert_eq!(WorldMove::new(2.0, -3.0).king_length(), 3.0);
        assert_eq!(WorldSquare::new(-2, 200).king_length(), 200);
        assert_eq!(WorldPoint::new(1.5, 3.3).king_length(), 3.3);
    }
    #[test]
    fn test_moving_a_point_by_rotated_direction() {
        let p: default::IntPoint = point2(3, 5);
        let dir = DOWN;
        let out = p.moved(dir.left(), 1);
        assert_eq!(out, point2(4, 5));
    }
    #[test]
    fn test_on_a_square_face() {
        [
            (0.0, 0.0, false),             // origin
            (0.5, 0.0, true),              // a side
            (0.5, 0.5, true),              // a corner
            (0.5, 0.51, true),             // near corner
            (0.49, 0.0, false),            // near_side
            (-5.5, 4.5, true),             // some other side
            (-584736.5, 40000.3, true),    // big numbers
            (-584736.45, 40000.51, false), // big numbers
        ]
        .into_iter()
        .for_each(|(x, y, on_face)| {
            assert_eq!(
                default::Point2D::new(x, y).on_a_square_face(),
                on_face,
                "point: {x}, {y}"
            );
        })
    }
    #[test]
    fn test_on_same_square_face() {
        [
            (0.0, 0.0, 0.0, 0.0, false, "same, but not on face"),
            (0.5, 0.0, 0.5, 0.1, true, "same face"),
            (0.5, 0.5, 0.5, -0.5, true, "adjacent corners"),
            (-0.5, 0.5, 0.5, -0.5, false, "opposite corners"),
            (0.5, 0.5, -0.5, -0.5, false, "other opposite corners"),
            (
                15.7,
                12.5,
                16.3,
                12.5,
                true,
                "same face, further from origin",
            ),
            (15.7, 12.5, 15.3, 12.5, false, "adjacent faces"),
        ]
        .into_iter()
        .for_each(|(x1, y1, x2, y2, same_face, case)| {
            let p1 = default::Point2D::new(x1, y1);
            let p2 = point2(x2, y2);
            assert_eq!(
                p1.on_same_square_face(p2),
                same_face,
                "{case}: {p1:?}, {p2:?}"
            );
        })
    }
    #[test]
    fn test_touched_square_faces() {
        [
            ((0.0, 0.0), hash_set![]),
            ((0.5, 0.0), hash_set![(0, 0, RIGHT), (1, 0, LEFT)]),
            ((-8.5, 0.0), hash_set![(-9, 0, RIGHT), (-8, 0, LEFT)]),
            ((0.2, 0.5), hash_set![(0, 0, UP), (0, 1, DOWN)]),
            (
                (0.5, 0.5),
                hash_set![
                    (0, 0, RIGHT),
                    (1, 0, LEFT),
                    (0, 1, RIGHT),
                    (1, 1, LEFT),
                    (0, 0, UP),
                    (0, 1, DOWN),
                    (1, 0, UP),
                    (1, 1, DOWN),
                ],
            ),
            (
                (-0.5, 0.5),
                hash_set![
                    (-1, 0, RIGHT),
                    (0, 0, LEFT),
                    (-1, 1, RIGHT),
                    (0, 1, LEFT),
                    (-1, 0, UP),
                    (-1, 1, DOWN),
                    (0, 0, UP),
                    (0, 1, DOWN),
                ],
            ),
        ]
        .into_iter()
        .for_each(|((x, y), faces)| {
            let faces: HashSet<OrthogonalFacingIntPose<WorldSquare>> = map_into(faces).collect();
            assert_eq!(WorldPoint::new(x, y).touched_square_faces(), faces);
        })
    }
}
