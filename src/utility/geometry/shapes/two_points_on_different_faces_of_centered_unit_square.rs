use crate::utility::*;

trait_alias!(pub trait PointReqs = two_different_points::PointReqs + float_coordinate::Operations);

// TODO: Make this just a special case for TwoDifferentPointsOnGridSquare, where the grid square is (0,0)?
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Shape<P: PointReqs>(TwoDifferentPoints<P>);

pub trait Operations<P: PointReqs>:
    Constructors<P>
    + directed_line_cutting_centered_unit_square::Operations<P>
    + two_different_points::Operations<P>
{
}
pub trait Constructors<P: PointReqs>: two_different_points::Constructors<P> {}

impls_for_two_different_points!(
    Shape<P: float_coordinate::Operations>
);
impl_translate_for_two_points_with_restriction!(
    Shape<P: float_coordinate::Operations>
);

impl<P: PointReqs> two_different_points::Operations<P> for Shape<P> {
    fn point_by_index(&self, pi: usize) -> P {
        self.0.point_by_index(pi)
    }
}
impl<P: float_coordinate::Operations> Display for Shape<P> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("TwoPointsOnDifferentFacesOfCenteredUnitSquare")
            .field("points", &self.0)
            .field(
                "point angles",
                &self.points().map(|p| {
                    let a = p.better_angle_from_x_axis();
                    format!(
                        "radians: {}\t degrees: {}\t turns:   {}",
                        a.radians,
                        a.to_degrees(),
                        a.radians / TAU
                    )
                }),
            )
            .finish()
    }
}

impl<P: float_coordinate::Operations> Shape<P> {
    fn points_are_valid(p1: P, p2: P) -> bool {
        p1.on_centered_unit_square() && p2.on_centered_unit_square() && !p1.on_same_square_face(p2)
    }
}

impl<P: float_coordinate::Operations> two_different_points::Constructors<P> for Shape<P> {
    fn try_from_two_exact_points(p1: P, p2: P) -> Result<Self, String> {
        // TODO: Add a tolerance to this check, or maybe snap to square along angle from origin
        if Self::points_are_valid(p1, p2) {
            Ok(Self(TwoDifferentPoints::try_from_two_exact_points(p1, p2)?))
        } else {
            Err(format!(
                "At least one point not on centered unit square: {:?}, {:?}",
                p1, p2
            ))
        }
    }
    fn try_from_two_points_allowing_snap_along_line(p1: P, p2: P) -> Result<Self, String> {
        Self::try_new_from_two_ordered_points_on_line(p1, p2)
    }
}
