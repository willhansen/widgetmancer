// automod::dir!("src");
// mod coordinates;
mod coordinate;
pub use coordinate::{Operations as CoordinateOperations, coord};

mod int_coordinate;
pub use int_coordinate::{ICoord, Operations as IntCoordinateOperations};
mod float_coordinate;
pub use float_coordinate::{FCoord, Operations as FloatCoordinateOperations};
mod signed_coordinate;
mod signed_int_coordinate;
mod unsigned_coordinate;

mod orthogonal_direction;
pub use orthogonal_direction::OrthogonalDirection;

mod king_direction;
pub use king_direction::KingDirection;

// TODO: move everything below this comment out of this file

use geo::Coord;

pub use std::fmt::{Debug, Display};
pub use num::{Zero, One};




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
    mod point_in_cus {
        use super::*;
        #[test]
        fn simple_true() {
            assert!(
            point_is_in_centered_unit_square_with_tolerance(WorldPoint::new(0.0, 0.0), 0.0)
                .is_true()
        )
        }
        #[test]
        fn simple_false() {
            assert!(
            point_is_in_centered_unit_square_with_tolerance(WorldPoint::new(5.0, 0.0), 0.0)
                .is_false()
            )
        }

        mod inside_tolerance {
            use super::*;
            #[test]
            fn outside_square() {
                assert!(
                point_is_in_centered_unit_square_with_tolerance(WorldPoint::new(0.51, 0.0), 0.2)
                    .is_partial()
            )
            }
            #[test]
            fn inside_square() {
                assert!(
                point_is_in_centered_unit_square_with_tolerance(WorldPoint::new(0.49, 0.0), 0.2)
                    .is_partial()
            )
            }
            #[test]
            fn outside_square_diagonally() {
                assert!(
                point_is_in_centered_unit_square_with_tolerance(WorldPoint::new(0.51, 0.51), 0.2)
                    .is_partial()
            )
            }
            #[test]
            fn inside_square_diagonally() {
                assert!(
                point_is_in_centered_unit_square_with_tolerance(WorldPoint::new(0.49, 0.49), 0.2)
                    .is_partial()
            )
            }
        }


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
}
