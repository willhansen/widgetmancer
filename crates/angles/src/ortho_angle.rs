// use crate::utility::*;
use crate::normalized_ortho_angle::NormalizedOrthoAngle;
use crate::quarter_turn_rotatable::QuarterTurnRotatable;
use num;

// AKA QuarterTurnsCcw
#[derive(Hash, Default, Debug, Copy, Clone, Eq, PartialEq, derive_more::AddAssign, derive_more::Add, derive_more::Sub)]
pub struct OrthoAngle(i32);

impl Operations for OrthoAngle
where
    Self: Sized,
{
    fn from_quarter_turns_ccw(quarter_turns: i32) -> Self {
        OrthoAngle(quarter_turns)
    }
    fn quarter_turns_ccw(&self) -> i32 {
        self.0
    }
}
impl QuarterTurnRotatable for OrthoAngle {
    fn quarter_rotated_ccw(
        &self,
        quarter_turns_ccw: OrthoAngle
    ) -> Self {
        self + quarter_turns_ccw
    }
}

pub trait Operations:
    Sized
    // + std::ops::Sub<NormalizedOrthoAngle, Output = Self>
    // + std::ops::Add<NormalizedOrthoAngle, Output = Self>
    + std::ops::Sub
    + std::ops::Add
    + QuarterTurnRotatable
    + Copy
    + Into<NormalizedOrthoAngle>
{
    fn from_quarter_turns_ccw(quarter_turns: i32) -> Self;
    fn quarter_turns_ccw(&self) -> i32;
    fn normalized(&self) -> NormalizedOrthoAngle {
        NormalizedOrthoAngle::from_quarter_turns_ccw(self.quarter_turns_ccw())
    }
    fn cos<T: num::Signed>(&self) -> T {
        match self.normalized().quarter_turns_ccw() {
            0 => T::one(),
            1 | 3 => T::zero(),
            2 => -T::one(),
            x => panic!("Invalid angle: {}", x),
        }
    }
    fn sin<T: num::Signed>(&self) -> T {
        match self.normalized().quarter_turns_ccw() {
            0 | 2 => T::zero(),
            1 => T::one(),
            3 => -T::one(),
            x => panic!("Invalid angle: {}", x),
        }
    }
    fn xy<T: num::Signed>(&self) -> [T;2]  {
        [self.cos(), self.sin()]
    }
    // fn dir(&self) -> OrthogonalDirection {
    //     OrthogonalDirection::from_angle(*self)
    // }

    // #[deprecated(note = "use to_step instead")]
    // fn to_orthogonal_direction(&self) -> WorldStep {
    //     self.to_step()
    // }
    
    // fn try_from_coordinate<T: coordinate::Operations>(dir: T) -> Result<Self, String> {
    //     if !dir.is_orthogonal() {
    //         return Err(format!("Not orthogonal: {}", dir.to_string()));
    //     }
    //     Ok(Self::new_from_quarter_turns(
    //         if dir.x() == T::DataType::zero() {
    //             if dir.y() > T::DataType::zero() {
    //                 1
    //             } else {
    //                 3
    //             }
    //         } else {
    //             if dir.x() > T::DataType::zero() {
    //                 0
    //             } else {
    //                 2
    //             }
    //         },
    //     ))
    // }

    // fn from_coordinate<T: coordinate::Operations>(dir: T) -> Self {
    //     Self::try_from_coordinate(dir).unwrap()
    // }
    // fn quarter_turns_from_x_axis<P: int_coordinate::Operations>(end: P) -> Self {
    //     Self::from_start_and_end_directions(P::right(), end)
    // }

    // fn from_start_and_end_directions<P: int_coordinate::Operations>(start: P, end: P) -> Self {
    //     assert!(start.is_king_step());
    //     assert!(end.is_king_step());
    //     // needs to be quarter turn, no eighths
    //     assert_eq!(start.is_diagonal(), end.is_diagonal());

    //     let d_angle = start.to_f32().angle_to(end.to_f32());
    //     let quarter_turns = (d_angle.to_degrees() / 90.0).round() as i32;
    //     Self::from_quarter_turns(quarter_turns)
    // }

    // fn rotate_angle(&self, angle: FAngle) -> FAngle {
    //     standardize_angle_with_zero_mid(euclid::Angle::<f32>::degrees(
    //         angle.to_degrees() + 90.0 * (self.quarter_turns() as f32),
    //     ))
    // }
    // fn to_float_angle(&self) -> FAngle {
    //     self.rotate_angle(FAngle::degrees(0.0))
    // }
    fn rad(&self) -> f32 {
         std::f32::consts::FRAC_PI_2 * self.quarter_turns_ccw() as f32
    }
    fn deg(&self) -> f32 {
        (self.quarter_turns_ccw() * 90) as f32
    }

    // fn rotate_vector<PointType: signed_coordinate::Operations>(&self, v: PointType) -> PointType {
    //     v.quarter_rotated_ccw(*self)
    // }
}

macro_rules! impl_ops_for_OrthoAngles {
    ($Type:ty) => {
        impl<T: Operations> std::ops::Add<T> for $Type {
            type Output = Self;

            fn add(self, rhs: T) -> Self::Output {
                Self::new_from_quarter_turns(self.quarter_turns() + rhs.quarter_turns())
            }
        }

        impl<T: Operations> std::ops::Sub<T> for $Type {
            type Output = Self;

            fn sub(self, rhs: T) -> Self::Output {
                Self::new_from_quarter_turns(self.quarter_turns() - rhs.quarter_turns())
            }
        }
        impl std::ops::Neg for $Type {
            type Output = Self;

            fn neg(self) -> Self::Output {
                Self::new_from_quarter_turns(-self.quarter_turns())
            }
        }

        impl From<i32> for $Type {
            fn from(value: i32) -> Self {
                Self::new_from_quarter_turns(value)
            }
        }
        impl QuarterTurnRotatable for $Type {
            fn quarter_rotated_ccw(
                &self,
                quarter_turns_ccw: impl Into<NormalizedOrthoAngle>,
            ) -> Self {
                *self + quarter_turns_ccw.into()
            }
        }

        impl Display for $Type {
            fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
                // TODO: tidy
                write!(
                    f,
                    "(x:{}, y:{}) {} {} ",
                    self.xy::<i32>().0,
                    self.xy::<i32>().1,
                    self.dir_name(),
                    Glyph::extract_arrow_from_arrow_string(self.to_step(), FACE_ARROWS)
                )
            }
        }
        impl Into<FAngle> for $Type {
            fn into(self) -> FAngle {
                self.to_float_angle()
            }
        }
    };
}

// impl_ops_for_OrthoAngles!(NormalizedOrthoAngle);
// impl_ops_for_OrthoAngles!(OrthoAngle);

impl From<NormalizedOrthoAngle> for OrthoAngle {
    fn from(value: NormalizedOrthoAngle) -> Self {
        value.quarter_turns_ccw().into()
    }
}

// impl<T: Coordinate> TryFrom<T> for OrthoAngle {
//     type Error = ();
//     fn try_from(value: T) -> Result<T, Self::Error> {
//         Ok(Self::from_orthogonal_vector(value))
//     }
// }
