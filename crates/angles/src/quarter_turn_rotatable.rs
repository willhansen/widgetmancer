// use crate::utility::*;
use crate::ortho_angle::OrthoAngle;
use itertools::Itertools;

pub trait QuarterTurnRotatable: Sized {
    // TODO: pass reference?
    fn quarter_rotated_ccw(&self, quarter_turns_ccw: OrthoAngle) -> Self;
    fn ez_quarter_rotated_ccw(&self, quarter_turns_ccw: impl Into<OrthoAngle>) -> Self {
        self.quarter_rotated_ccw(quarter_turns_ccw.into())
    }
    fn quadrant_rotations_going_ccw(&self) -> [Self; 4]
    where
        Self: Sized + std::fmt::Debug,
    {
        (0..4)
            .into_iter()
            .map(|i| self.ez_quarter_rotated_ccw(i))
            .collect_vec()
            .try_into()
            .unwrap()
    }
    fn turned_left(&self) -> Self
    where
        Self: Sized,
    {
        self.ez_quarter_rotated_ccw(1)
    }
    fn turned_right(&self) -> Self
    where
        Self: Sized,
    {
        self.ez_quarter_rotated_ccw(-1)
    }
    fn turned_back(&self) -> Self
    where
        Self: Sized,
    {
        self.ez_quarter_rotated_ccw(2)
    }
}

// TODO: generalize to any trait?
// macro_rules! impl_quarter_turn_rotatable_via_newtype {
//     // TODO: is the dummy $T metavariable required for the optional parts with only it?
//     ($type:ident$(<$T:ident$(: $traitparam:ident)?>)?) => {
//         impl$(<$T$(: $traitparam)?>)? QuarterTurnRotatable for $type$(<$T>)? {
//             fn quarter_rotated_ccw(
//                 &self,
//                 quarter_turns_ccw: impl Into<NormalizedOrthoAngle>,
//             ) -> Self {
//                 Self::new(self.0.quarter_rotated_ccw(quarter_turns_ccw))
//             }
//         }
//     };
// }
// pub(crate) use impl_quarter_turn_rotatable_via_newtype;

// TODO: Can use blanket implementation over IntoIterator and FromIterator instead?
impl<T> QuarterTurnRotatable for Vec<T>
where
    T: QuarterTurnRotatable,
{
    fn quarter_rotated_ccw(&self, quarter_turns_ccw: OrthoAngle) -> Self {
        self.iter()
            .map(|t| t.quarter_rotated_ccw(quarter_turns_ccw))
            .collect()
    }
}

// TODO: generate with macro ("for_mappable"?)
impl<T> QuarterTurnRotatable for Option<T>
where
    T: QuarterTurnRotatable,
{
    fn quarter_rotated_ccw(&self, quarter_turns_ccw: OrthoAngle) -> Self {
        self.as_ref()
            .map(|x| x.quarter_rotated_ccw(quarter_turns_ccw))
    }
}
