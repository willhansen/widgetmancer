use crate::utility::*;
use euclid::num::Zero;

// pub trait HalfPlaneTrait<LineType>
// where
//     LineType: DirectedLineTrait,
// {
//     fn new_from_directed_line<P>(line: P) -> Self
//     where
//         P: DirectedLineTrait;

//     fn new_from_line_and_point_on_half_plane(
//         dividing_line: impl Into<LineType>,
//         point_on_half_plane: impl Into<LineType::PointType>,
//     ) -> Self;
// }

// TODO: allow non-floating-point-based half planes
#[derive(PartialEq, Clone, Copy)]
pub struct HalfPlane<LineType = TwoDifferentFloatPoints>
where
    LineType: DirectedFloatLine,
    Self: FromDirectedLine<LineType::PointType>,
{
    // Internal convention is that the half plane is clockwise of the vector from p1 to p2 of the dividing line
    pub dividing_line: LineType,
}

impl<LineType> HalfPlane<LineType>
where
    LineType: DirectedFloatLine,
    Self: FromDirectedLine<LineType::PointType>,
{
    pub fn new_from_directed_line<P>(line: P) -> Self
    where
        P: DirectedLineLike<PointType = LineType::PointType>,
    {
        Self {
            dividing_line: LineType::from_other_directed_line(line),
        }
    }
    pub fn new_from_line_and_point_on_half_plane(
        dividing_line: impl Into<LineType>,
        point_on_half_plane: impl Into<LineType::PointType>,
    ) -> Self {
        // let dividing_line = LineType::from_other_line(dividing_line);
        let dividing_line = dividing_line.into();
        let point_on_half_plane = point_on_half_plane.into();
        let [p1, p2] = dividing_line.two_points_on_line_in_order();
        HalfPlane {
            dividing_line: if three_points_are_clockwise(p1, p2, point_on_half_plane) {
                dividing_line
            } else {
                dividing_line.reversed()
            },
        }
    }
    pub fn new_with_inside_down(y: f32) -> Self {
        Self::new_from_point_on_border_and_vector_pointing_inside((0.0, y), (0.0, -1.0))
    }
    pub fn new_from_border_line_with_origin_outside(
        can_be_dividing_line: impl Into<LineType>,
    ) -> Self {
        let line: LineType = can_be_dividing_line.into();
        assert_false!(line.point_is_on_line((0.0, 0.0)));
        Self::new_from_line_and_point_on_half_plane(line, line.reflect_point_over_line((0.0, 0.0)))
    }
    pub fn new_from_normal_vector_going_from_origin_to_inside_edge_of_border(
        vector_to_outside: impl Into<LineType::PointType>,
    ) -> Self {
        let vector_to_outside = vector_to_outside.into();
        Self::new_from_point_on_border_and_vector_pointing_inside(
            LineType::PointType::zero() + vector_to_outside,
            -vector_to_outside,
        )
    }
    pub fn new_from_border_line_with_origin_inside(
        can_be_dividing_line: impl Into<LineType>,
    ) -> Self {
        let line: LineType = can_be_dividing_line.into();
        assert_false!(line.point_is_on_line((0.0, 0.0)));
        Self::new_from_line_and_point_on_half_plane(line, (0.0, 0.0))
    }
    pub fn new_from_point_on_border_and_vector_pointing_inside(
        point_on_border: impl Into<LineType::PointType>,
        normal_direction_into_plane: impl Into<LineType::PointType>,
    ) -> Self {
        let p = point_on_border.into();
        let v = normal_direction_into_plane.into();
        assert_ne!(v.square_length(), 0.0);
        let direction_along_edge_with_inside_on_right = v.turned_left();

        let border_line = LineType::new_from_two_points_on_line(p, p + v.quarter_rotated_ccw(1));
        let point_on_half_plane = p + v;
        Self::new_from_line_and_point_on_half_plane(border_line, point_on_half_plane)
    }

    pub fn complement(&self) -> Self {
        HalfPlane::new_from_line_and_point_on_half_plane(
            self.dividing_line,
            self.point_off_half_plane(),
        )
    }
    pub fn dividing_line(&self) -> LineType {
        self.dividing_line
    }

    pub fn point_on_half_plane(&self) -> LineType::PointType {
        self.dividing_line.arbitrary_point_right_of_line()
    }

    pub fn point_off_half_plane(&self) -> LineType::PointType {
        self.dividing_line
            .reflect_point_over_line(self.point_on_half_plane())
    }

    pub fn about_equal(&self, other: Self, tolerance: f32) -> bool {
        self.dividing_line
            .approx_on_same_line(other.dividing_line, tolerance)
            && self
                .dividing_line
                .same_side_of_line(self.point_on_half_plane(), other.point_on_half_plane())
    }

    pub fn about_complementary(&self, other: Self, tolerance: f32) -> bool {
        self.about_equal(other.complement(), tolerance)
    }

    pub fn covers_point(&self, point: impl Into<LineType::PointType> + Copy) -> BoolWithPartial {
        self.covers_point_with_tolerance(point, 0.0)
    }
    pub fn covers_point_with_tolerance(
        &self,
        point: impl Into<LineType::PointType> + Copy,
        tolerance: f32,
    ) -> BoolWithPartial {
        assert!(tolerance >= 0.0);
        let depth = self.depth_of_point_in_half_plane(point);
        BoolWithPartial::from_less_than_with_tolerance(0.0, depth, tolerance)
    }
    pub fn at_least_partially_covers_point(
        &self,
        point: impl Into<LineType::PointType> + Copy,
    ) -> bool {
        self.covers_point(point).is_at_least_partial()
    }
    pub fn covers_origin(&self) -> BoolWithPartial {
        self.covers_point((0.0, 0.0))
    }
    pub fn fully_covers_centered_unit_square(&self) -> BoolWithPartial {
        self.fully_covers_centered_unit_square_with_tolerance(0.0)
    }
    pub fn fully_covers_centered_unit_square_with_tolerance(
        &self,
        tolerance: f32,
    ) -> BoolWithPartial {
        self.covers_all_of_these_points_with_tolerance(
            corner_points_of_centered_unit_square(),
            tolerance,
        )
    }
    pub fn retracted(&self, retracted_distance: f32) -> Self {
        self.extended(-retracted_distance)
    }
    pub fn extended(&self, extended_distance: f32) -> Self {
        let direction = self.direction_away_from_plane();
        let move_vector = LineType::PointType::from_angle_and_length(direction, extended_distance);

        let line = self.dividing_line();
        let point = self.point_on_half_plane();

        let shifted_point = point + move_vector;
        let [p1, p2] = line.two_points_on_line_in_order();
        let shifted_line =
            LineType::new_from_two_points_on_line(p1 + move_vector, p2 + move_vector);

        Self::new_from_line_and_point_on_half_plane(shifted_line, shifted_point)
    }
    pub fn direction_away_from_plane(&self) -> Angle<f32> {
        standardize_angle(self.dividing_line.direction() + Angle::degrees(90.0))
    }
    pub fn direction_toward_plane(&self) -> Angle<f32> {
        standardize_angle(-self.direction_away_from_plane())
    }

    pub fn at_least_partially_covers_unit_square(&self) -> bool {
        !self
            .complement()
            .fully_covers_centered_unit_square()
            .is_true()
    }
    pub fn partially_covers_centered_unit_square_with_tolerance(
        &self,
        tolerance: f32,
    ) -> BoolWithPartial {
        self.fully_covers_centered_unit_square_with_tolerance(tolerance)
            .or(self
                .complement()
                .fully_covers_centered_unit_square_with_tolerance(tolerance))
            .not()
    }

    //Fn(LineType::PointType) -> Point2D<f32, V>,
    //fun: Box<dyn Fn<LineType::PointType, Output = Point2D<f32, V>>>,
    pub fn with_transformed_points<F, V>(
        &self,
        point_transform_function: F,
    ) -> HalfPlane<TwoDifferentPoints<Point2D<f32, V>>>
    where
        V: Copy + Debug,
        F: Fn(LineType::PointType) -> Point2D<f32, V>,
    {
        let [p1, p2] = self.dividing_line.two_points_on_line_in_order();
        HalfPlane::new_from_line_and_point_on_half_plane(
            TwoDifferentPoints::new_from_two_points_on_line(
                point_transform_function(p1),
                point_transform_function(p2),
            ),
            point_transform_function(self.point_on_half_plane()),
        )
    }
    pub fn top_half_plane() -> Self {
        Self::new_from_line_and_point_on_half_plane(
            LineType::new_from_two_points_on_line((1.0, 0.0), (-1.0, 0.0)),
            LineType::PointType::new(0.0, 1.0),
        )
    }
    pub fn depth_of_point_in_half_plane(
        &self,
        point: impl Into<LineType::PointType> + Copy,
    ) -> f32 {
        let dist = self.dividing_line().normal_distance_to_point(point);
        let is_on_half_plane = self
            .dividing_line
            .same_side_of_line(self.point_on_half_plane(), point);
        if is_on_half_plane {
            dist
        } else {
            -dist
        }
    }
    pub fn distance_of_point_from_half_plane(&self, point: LineType::PointType) -> f32 {
        -self.depth_of_point_in_half_plane(point)
    }
    // TODO: change output type to guarantee value in normalized range ( [0.0,1.0] )
    pub fn very_approximate_fraction_coverage_of_centered_unit_square(&self) -> f32 {
        let dist = self.dividing_line.distance_from_origin();
        let corner_dist = 2.0.sqrt() * 0.5;
        let side_dist = 0.5;

        // TODO: Do the actual math instead of lerp
        let fraction_on_origin_side_of_line = lerp(0.5, 1.0, dist / corner_dist).clamp(0.5, 1.0);
        if self.covers_origin().is_true() {
            fraction_on_origin_side_of_line
        } else {
            1.0 - fraction_on_origin_side_of_line
        }
    }
    pub fn coverage_of_centered_unit_square_with_tolerance(
        &self,
        tolerance: f32,
    ) -> RelativeIntervalLocation {
        assert!(tolerance >= 0.0);
        let fully_covers = self.fully_covers_centered_unit_square_with_tolerance(tolerance);
        if fully_covers.is_true() {
            return RelativeIntervalLocation::After;
        }
        if fully_covers.is_partial() {
            return RelativeIntervalLocation::End;
        }

        let partially_covers = self.partially_covers_centered_unit_square_with_tolerance(tolerance);
        if partially_covers.is_true() {
            RelativeIntervalLocation::Inside
        } else if partially_covers.is_partial() {
            RelativeIntervalLocation::Start
        } else {
            RelativeIntervalLocation::Before
        }
    }

    pub fn overlaps_other_inside_centered_unit_square_with_tolerance(
        &self,
        other: &Self,
        tolerance: f32,
    ) -> BoolWithPartial {
        assert!(tolerance >= 0.0);
        // square corners inside a halfplane and that halfplane's intersections with the square form a convex polygon, which the other plane can test against.

        // Does the border intersection point actually matter?  Not if we have the intersections with the square
        // Does the shape of the tolerance matter?  expanded square vs expanded points
        //     - As long as tolerance refers to circles, and expanded square refers to squares, it can stay visible, at least

        let other_border_cut_points = other
            .dividing_line
            .unordered_line_intersections_with_centered_unit_square_with_tolerance(tolerance);

        let other_covered_square_corner_points =
            other.at_least_partially_covered_corner_points_of_centered_unit_square(tolerance);

        let polygon_corners_for_overlap_of_other_and_square = other_border_cut_points
            .into_iter()
            .chain(other_covered_square_corner_points.into_iter())
            .collect();

        self.covers_any_of_these_points_with_tolerance(
            polygon_corners_for_overlap_of_other_and_square,
            tolerance,
        )
    }
    fn at_least_partially_covered_corner_points_of_centered_unit_square(
        &self,
        tolerance: f32,
    ) -> Vec<LineType::PointType> {
        corner_points_of_centered_unit_square()
            .into_iter()
            .filter(|&p| self.covers_point(p).is_at_least_partial())
            .collect()
    }
    fn covers_any_of_these_points_with_tolerance(
        &self,
        points: Vec<LineType::PointType>,
        tolerance: f32,
    ) -> BoolWithPartial {
        assert!(tolerance >= 0.0);
        BoolWithPartial::any(
            points
                .into_iter()
                .map(|p| self.covers_point_with_tolerance(p, tolerance)),
        )
    }
    fn covers_all_of_these_points_with_tolerance(
        &self,
        points: Vec<LineType::PointType>,
        tolerance: f32,
    ) -> BoolWithPartial {
        assert!(tolerance >= 0.0);
        BoolWithPartial::all(
            points
                .into_iter()
                .map(|p| self.covers_point_with_tolerance(p, tolerance)),
        )
    }
}

impl<P, L> FromDirectedLine<P> for HalfPlane<L>
where
    L: DirectedFloatLine<_PointType = P>,
    P: FloatCoordinate,
{
    fn try_new_from_directed_line(line: impl DirectedLine<PointType = P>) -> Result<Self, String>
    where
        Self: Sized,
    {
        todo!()
    }
}

impl<L: DirectedFloatLine> Debug for HalfPlane<L> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("HalfPlane")
            .field("dividing_line", &self.dividing_line)
            .field(
                "inside_direction",
                &self.dividing_line.direction().turned_right(),
            )
            .finish()
    }
}

impl<P: FloatCoordinate> TryFrom<HalfPlane<TwoDifferentPoints<P>>>
    for HalfPlane<TwoDifferentPointsOnCenteredUnitSquare<P>>
{
    type Error = ();

    fn try_from(value: HalfPlane<TwoDifferentPoints<P>>) -> Result<Self, Self::Error> {
        let points: Result<TwoDifferentPointsOnCenteredUnitSquare<P>, _> =
            value.dividing_line.try_into();
        match points {
            Ok(x) => Ok(Self::new_from_directed_line(x)),
            _ => Err(()),
        }
    }
}
impl<P: FloatCoordinate> From<HalfPlane<TwoDifferentPointsOnCenteredUnitSquare<P>>>
    for HalfPlane<TwoDifferentPoints<P>>
{
    fn from(value: HalfPlane<TwoDifferentPointsOnCenteredUnitSquare<P>>) -> Self {
        Self::new_from_directed_line(value.dividing_line)
    }
}

impl<LineType: DirectedFloatLine> QuarterTurnRotatable for HalfPlane<LineType> {
    fn quarter_rotated_ccw(&self, quarter_turns_ccw: impl Into<NormalizedOrthoAngle>) -> Self {
        let quarter_turns_ccw = quarter_turns_ccw.into();
        let line = self.dividing_line();
        let point = self.point_on_half_plane();
        let new_point = point.quarter_rotated_ccw(quarter_turns_ccw);
        let new_line = line.quarter_rotated_ccw(quarter_turns_ccw);
        Self::new_from_line_and_point_on_half_plane(new_line, new_point)
    }

    // #[cfg(all())]
    // fn quadrant_rotations_going_ccw(&self) -> [Self; 4]
    // where
    //     Self: Sized + Debug,
    // {
    //     (0..4)
    //         .into_iter()
    //         .map(|i| self.quarter_rotated_ccw(i))
    //         .collect_vec()
    //         .try_into()
    //         .unwrap()
    // }
}

pub type HalfPlaneCuttingSquare<SquareType: IntCoordinate> =
    HalfPlane<TwoDifferentPointsOnGridSquare<SquareType::Floating>>;

pub trait HalfPlaneCuttingSquareTrait<LineType: DirectedFloatLine> {
    // type PointType: FloatCoordinate;
    fn which_square(&self) -> <LineType::PointType as Coordinate>::OnGrid;
    fn to_local(&self) -> TwoDifferentPointsOnCenteredUnitSquare<LineType::PointType>;
    // TODO: change output to normalized float
    fn fraction_of_square_covered(&self) -> f32 {
        // TODO: tidy this up when halfplane is a trait
        HalfPlane::<LineType>::new_from_directed_line(self.to_local())
            .very_approximate_fraction_coverage_of_centered_unit_square()
    }
}

impl<L> HalfPlaneCuttingSquareTrait<L> for HalfPlane<L>
where
    L: TwoPointsOnASquareTrait<L::PointType> + DirectedFloatLine,
{
    fn which_square(&self) -> <L::PointType as Coordinate>::OnGrid {
        self.dividing_line.which_square()
    }

    fn to_local(&self) -> TwoDifferentPointsOnCenteredUnitSquare<L::PointType> {
        self.dividing_line.points_relative_to_the_square()
    }
}

#[cfg(test)]
mod tests {

    use std::array::from_fn;

    use ntest::{assert_about_eq, assert_false, assert_true, timeout};
    use pretty_assertions::{assert_eq, assert_ne};

    use crate::utility::units::SquareGridInWorldFrame;

    use super::*;
    #[test]
    fn test_half_plane_complementary_check__different_lines() {
        let good_line: TwoDifferentPoints<WorldPoint> =
            TwoDifferentPoints::new_from_two_points_on_line((0.0, 0.0), (1.0, 1.0));
        let bad_line: TwoDifferentPoints<WorldPoint> =
            TwoDifferentPoints::new_from_two_points_on_line((0.1, 0.0), (1.0, 1.0));
        let upper_point = WorldPoint::new(0.0, 1.0);
        let right_point = WorldPoint::new(1.0, 0.0);

        let good_half_plane_upwards =
            WorldHalfPlane::new_from_line_and_point_on_half_plane(good_line, upper_point);
        let good_half_plane_downwards =
            WorldHalfPlane::new_from_line_and_point_on_half_plane(good_line, right_point);
        let bad_half_plane =
            WorldHalfPlane::new_from_line_and_point_on_half_plane(bad_line, right_point);

        assert!(good_half_plane_upwards.about_complementary(good_half_plane_downwards, 1e-6));
        assert!(good_half_plane_downwards.about_complementary(good_half_plane_upwards, 1e-6));
        assert_false!(good_half_plane_upwards.about_complementary(good_half_plane_upwards, 1e-6));
        assert_false!(good_half_plane_upwards.about_complementary(bad_half_plane, 1e-6));
        assert_false!(good_half_plane_downwards.about_complementary(bad_half_plane, 1e-6));
    }

    #[test]
    fn test_half_plane_complementary_check__equivalent_lines() {
        let line: TwoDifferentPoints<WorldPoint> =
            TwoDifferentPoints::new_from_two_points_on_line((0.0, 0.0), (1.0, 1.0));
        let line2: TwoDifferentPoints<WorldPoint> =
            TwoDifferentPoints::new_from_two_points_on_line((2.0, 2.0), (5.0, 5.0));
        let p1 = WorldPoint::new(0.0, 1.0);
        let p2 = WorldPoint::new(1.0, 0.0);

        let half_plane_1 = WorldHalfPlane::new_from_line_and_point_on_half_plane(line, p1);
        let half_plane_2 = WorldHalfPlane::new_from_line_and_point_on_half_plane(line2, p2);

        assert!(half_plane_1.about_complementary(half_plane_2, 1e-6));
    }

    #[test]
    fn test_halfplane_fully_covers_centered_unit_square_with_tolerance() {
        let f = |x, tolerance| {
            let the_plane = WorldHalfPlane::new_from_point_on_border_and_vector_pointing_inside(
                (x, 0.0),
                (-1.0, 0.0),
            );

            the_plane.fully_covers_centered_unit_square_with_tolerance(tolerance)
        };
        // half cover
        assert!(f(0.0, 0.0).is_false());
        // almost full cover, outside tolerance
        assert!(f(0.49, 0.0).is_false());
        // exact cover, on tolerance
        assert!(f(0.5, 0.0).is_partial());
        // more than full cover, outside tolerance
        assert!(f(0.51, 0.0).is_true());
        // almost full cover, inside tolerance
        assert!(f(0.49, 0.2).is_partial());
        // exact full cover, inside tolerance
        assert!(f(0.5, 0.2).is_partial());
        // more than full cover, inside tolerance
        assert!(f(0.51, 0.2).is_partial());
        // almost full cover, on tolerance
        assert!(f(0.49, 0.1).is_partial());
        // more than full cover, on tolerance
        assert!(f(0.51, 0.1).is_partial());
    }
    #[test]
    fn test_depth_of_point_in_half_plane() {
        let horizontal = WorldHalfPlane::new_from_line_and_point_on_half_plane(
            ((0.0, 0.0), (1.0, 0.0)),
            (0.0, 5.0),
        );

        assert_about_eq!(
            horizontal.depth_of_point_in_half_plane(point2(0.0, 12.0)),
            12.0
        );
        assert_about_eq!(
            horizontal
                .extended(3.0)
                .depth_of_point_in_half_plane(point2(2000.0, 10.0)),
            13.0
        );
        assert_about_eq!(
            horizontal.depth_of_point_in_half_plane(point2(0.0, -1.0)),
            -1.0
        );

        let diag = WorldHalfPlane::new_from_line_and_point_on_half_plane(
            ((0.0, 0.0), (-1.0, 1.0)),
            (0.0, 5.0),
        );
        assert_about_eq!(
            diag.depth_of_point_in_half_plane(point2(1.0, 0.0)),
            1.0 / 2.0_f32.sqrt()
        );
    }
    // TODO: This test isn't readable.
    #[test]
    fn test_halfplane_overlap_within_unit_square() {
        //
        //      +----------------+
        //   o  |  o         o   |
        //   G  |  A         B   |
        //      |                |
        //      |       +        |   o
        //      |       O        |   C
        //      |                |
        //   F  |  E         D   |
        //   o  |  o         o   |
        //      +----------------+
        //
        let a = (-0.4, 0.4);
        let b = (0.4, 0.4);
        let c = (0.6, 0.0);
        let d = (0.4, -0.4);
        let e = (-0.4, -0.4);
        let f = (-0.6, -0.4);
        let g = (-0.6, 0.4);

        let up: LocalSquareHalfPlane =
            LocalSquareHalfPlane::new_from_line_and_point_on_half_plane((a, b), (0.0, 5.0));
        let up_right =
            LocalSquareHalfPlane::new_from_line_and_point_on_half_plane((b, c), (1.0, 1.0)).into();
        let down_right =
            LocalSquareHalfPlane::new_from_line_and_point_on_half_plane((c, d), (1.0, -1.0)).into();
        let down =
            LocalSquareHalfPlane::new_from_line_and_point_on_half_plane((d, e), (0.0, -5.0)).into();
        let left =
            LocalSquareHalfPlane::new_from_line_and_point_on_half_plane((f, g), (-5.0, 0.0)).into();

        let tolerance = 1e-5;

        let f = HalfPlane::overlaps_other_inside_centered_unit_square_with_tolerance;
        let vars = vec![up, up_right, down_right, down, left];
        // in format of f(vars[row], vars[col]).  Should be symmetric anyway
        let correct_boolean_matrix = [
            [1, 1, 0, 0, 0],
            [1, 1, 0, 0, 0],
            [0, 0, 1, 1, 0],
            [0, 0, 1, 1, 0],
            [0, 0, 0, 0, 0],
        ];
        let actual_boolean_matrix: [[i32; 5]; 5] = from_fn(|row| {
            from_fn(|col| {
                if f(&vars[row].into(), &vars[col], tolerance)
                    .try_into()
                    .unwrap()
                {
                    1
                } else {
                    0
                }
            })
        });

        assert_eq!(actual_boolean_matrix, correct_boolean_matrix);
    }
    #[test]
    fn test_halfplane_overlap_within_unit_square__true__horizontal_up() {
        let a = LocalSquareHalfPlane::new_from_border_line_with_origin_outside(
            TwoDifferentPoints::new_horizontal(0.3),
        );
        let b = LocalSquareHalfPlane::new_from_border_line_with_origin_outside(
            TwoDifferentPoints::new_horizontal(0.4),
        );
        assert!(a
            .overlaps_other_inside_centered_unit_square_with_tolerance(&b, 1e-5)
            .is_true())
    }
    #[test]
    fn test_halfplane_overlap_within_unit_square__true__vertical_left() {
        let a: LocalSquareHalfPlane = HalfPlane::new_from_border_line_with_origin_outside(
            TwoDifferentPoints::new_vertical(-0.3),
        );
        let b: LocalSquareHalfPlane = HalfPlane::new_from_border_line_with_origin_outside(
            TwoDifferentPoints::new_vertical(-0.4),
        );
        assert!(a
            .overlaps_other_inside_centered_unit_square_with_tolerance(&b, 1e-5)
            .is_true())
    }
    #[test]
    fn test_halfplane_overlap_within_unit_square__false__horizontal__both_partially_covering_square(
    ) {
        let a: LocalSquareHalfPlane = HalfPlane::new_from_border_line_with_origin_outside(
            TwoDifferentPoints::new_horizontal(0.3),
        );
        let b: LocalSquareHalfPlane = HalfPlane::new_from_border_line_with_origin_outside(
            TwoDifferentPoints::new_horizontal(-0.4),
        );
        assert!(a
            .overlaps_other_inside_centered_unit_square_with_tolerance(&b, 1e-5)
            .is_false())
    }
    #[test]
    fn test_halfplane_overlap_within_unit_square__true__both_fully_cover() {
        let a: LocalSquareHalfPlane = HalfPlane::new_from_border_line_with_origin_inside(
            TwoDifferentPoints::new_horizontal(5.0),
        );
        let b: LocalSquareHalfPlane = HalfPlane::new_from_border_line_with_origin_inside(
            TwoDifferentPoints::new_horizontal(-0.7),
        );
        assert!(a
            .overlaps_other_inside_centered_unit_square_with_tolerance(&b, 1e-5)
            .is_true())
    }
    #[test]
    fn test_halfplane_overlap_within_unit_square__true__both_fully_cover__identical() {
        let a: LocalSquareHalfPlane = HalfPlane::new_from_border_line_with_origin_inside(
            TwoDifferentPoints::new_horizontal(5.0),
        );
        let b: LocalSquareHalfPlane = a.clone();
        assert!(a
            .overlaps_other_inside_centered_unit_square_with_tolerance(&b, 1e-5)
            .is_true())
    }
    #[test]
    fn test_halfplane_overlap_within_unit_square__true__one_full_cover_one_partial_cover() {
        let a: LocalSquareHalfPlane = HalfPlane::new_from_border_line_with_origin_inside(
            TwoDifferentPoints::new_horizontal(0.3),
        );
        let b: LocalSquareHalfPlane = HalfPlane::new_from_border_line_with_origin_inside(
            TwoDifferentPoints::new_horizontal(5.0),
        );
        assert!(a
            .overlaps_other_inside_centered_unit_square_with_tolerance(&b, 1e-5)
            .is_true())
    }
    #[test]
    fn test_halfplane_overlap_within_unit_square__true__orthogonal() {
        let a: LocalSquareHalfPlane = HalfPlane::new_from_border_line_with_origin_outside(
            TwoDifferentPoints::new_vertical(0.3),
        );
        let b: LocalSquareHalfPlane = HalfPlane::new_from_border_line_with_origin_outside(
            TwoDifferentPoints::new_horizontal(0.1),
        );
        assert!(a
            .overlaps_other_inside_centered_unit_square_with_tolerance(&b, 1e-5)
            .is_true())
    }
    #[test]
    fn test_halfplane_overlap_within_unit_square__false__orthogonal() {
        let a: LocalSquareHalfPlane = HalfPlane::new_from_border_line_with_origin_outside(
            TwoDifferentPoints::new_vertical(0.6),
        );
        let b: LocalSquareHalfPlane = HalfPlane::new_from_border_line_with_origin_outside(
            TwoDifferentPoints::new_horizontal(0.1),
        );
        assert!(a
            .overlaps_other_inside_centered_unit_square_with_tolerance(&b, 1e-5)
            .is_false())
    }
    #[test]
    fn test_halfplane_overlap_within_unit_square__false__orthogonal__one_fully_covers() {
        let a: LocalSquareHalfPlane = HalfPlane::new_from_border_line_with_origin_outside(
            TwoDifferentPoints::new_vertical(0.6),
        );
        let b: LocalSquareHalfPlane = HalfPlane::new_from_border_line_with_origin_inside(
            TwoDifferentPoints::new_horizontal(5.0),
        );
        assert!(a
            .overlaps_other_inside_centered_unit_square_with_tolerance(&b, 1e-5)
            .is_false())
    }
    #[test]
    fn test_halfplane_overlap_within_unit_square__false__orthogonal__neither_fully_covers() {
        let a: LocalSquareHalfPlane = HalfPlane::new_from_border_line_with_origin_outside(
            TwoDifferentPoints::new_vertical(0.6),
        );
        let b: LocalSquareHalfPlane = HalfPlane::new_from_border_line_with_origin_outside(
            TwoDifferentPoints::new_horizontal(5.0),
        );
        assert!(a
            .overlaps_other_inside_centered_unit_square_with_tolerance(&b, 1e-5)
            .is_false())
    }
    #[test]
    fn test_halfplane_overlap_within_unit_square__true__diagonal_outside_square_but_inside_tolerance(
    ) {
        let tolerance = 0.01;
        let a: LocalSquareHalfPlane = HalfPlane::new_from_border_line_with_origin_outside(
            TwoDifferentPoints::new_vertical(0.5 + tolerance / 2.0),
        );
        let b: LocalSquareHalfPlane = HalfPlane::new_from_border_line_with_origin_outside(
            TwoDifferentPoints::new_horizontal(0.5 + tolerance / 2.0),
        );
        assert!(a
            .overlaps_other_inside_centered_unit_square_with_tolerance(&b, tolerance)
            .is_partial())
    }
    #[test]
    fn test_halfplane_overlap_within_unit_square__true__angled() {
        let a: LocalSquareHalfPlane = HalfPlane::new_from_border_line_with_origin_outside(
            TwoDifferentPoints::new_horizontal(-0.2),
        );
        let b: LocalSquareHalfPlane = HalfPlane::new_from_border_line_with_origin_outside(
            TwoDifferentPoints::new_from_two_points_on_line((0.0, 0.5), (0.5, -0.3)),
        );
        assert!(a
            .overlaps_other_inside_centered_unit_square_with_tolerance(&b, 1e-5)
            .is_true())
    }
    #[test]
    fn test_halfplane_overlap_within_unit_square__false__angled() {
        let a: LocalSquareHalfPlane = HalfPlane::new_from_border_line_with_origin_outside(
            TwoDifferentPoints::new_horizontal(-0.2),
        );
        let b: LocalSquareHalfPlane = HalfPlane::new_from_border_line_with_origin_outside(
            TwoDifferentPoints::new_from_two_points_on_line((0.0, 0.5), (0.5, -0.1)),
        );
        assert!(a
            .overlaps_other_inside_centered_unit_square_with_tolerance(&b, 1e-5)
            .is_false())
    }
    #[test]
    fn test_halfplane_overlap_within_unit_square__true__identical_on_edge() {
        let a: LocalSquareHalfPlane = HalfPlane::new_from_border_line_with_origin_outside(
            TwoDifferentPoints::new_horizontal(0.5),
        );
        let b: LocalSquareHalfPlane = a.clone();
        assert!(a
            .overlaps_other_inside_centered_unit_square_with_tolerance(&b, 0.0)
            .is_partial())
    }
    #[test]
    fn test_halfplane_overlap_within_unit_square__true__identical_on_corner() {
        let a: LocalSquareHalfPlane = HalfPlane::new_from_border_line_with_origin_outside(
            TwoDifferentPoints::new_from_two_points_on_line((0.5, 0.5), (0.0, 1.0)),
        );
        let b: LocalSquareHalfPlane = a.clone();
        assert!(a
            .overlaps_other_inside_centered_unit_square_with_tolerance(&b, 0.0)
            .is_partial())
    }
    #[test]
    fn test_halfplane_overlap_within_unit_square__false__opposing_parallel_half_planes_exactly_touching_square(
    ) {
        let a: LocalSquareHalfPlane = HalfPlane::new_from_border_line_with_origin_outside(
            TwoDifferentPoints::new_horizontal(0.5),
        );
        let b: LocalSquareHalfPlane = a.complement();
        assert!(a
            .overlaps_other_inside_centered_unit_square_with_tolerance(&b, 0.0)
            .is_partial())
    }
    #[test]
    fn test_halfplane_overlap_within_unit_square__false__opposing_parallel_half_planes_exactly_touching_each_other_within_square(
    ) {
        let a: LocalSquareHalfPlane = HalfPlane::new_from_border_line_with_origin_outside(
            TwoDifferentPoints::new_horizontal(0.2),
        );
        let b: LocalSquareHalfPlane = a.complement();
        assert!(a
            .overlaps_other_inside_centered_unit_square_with_tolerance(&b, 0.0)
            .is_partial())
    }
    #[test]
    fn test_halfplane_overlap_within_unit_square__opposing_parallel_half_planes_exactly_touching_each_other_is_consistent(
    ) {
        let a: LocalSquareHalfPlane = HalfPlane::new_from_border_line_with_origin_outside(
            TwoDifferentPoints::new_horizontal(0.2),
        );
        let b: LocalSquareHalfPlane = HalfPlane::new_from_border_line_with_origin_outside(
            TwoDifferentPoints::new_horizontal(0.5),
        );
        assert_eq!(
            a.overlaps_other_inside_centered_unit_square_with_tolerance(&a.complement(), 0.0),
            b.overlaps_other_inside_centered_unit_square_with_tolerance(&b.complement(), 0.0,)
        )
    }
    #[test]
    fn test_halfplane_overlap_within_unit_square__partial__opposing_parallel_half_planes_extended_but_within_tolerance(
    ) {
        let tolerance = 0.01;
        let a: LocalSquareHalfPlane = HalfPlane::new_from_border_line_with_origin_outside(
            TwoDifferentPoints::new_horizontal(0.5),
        );
        let b: LocalSquareHalfPlane = a.complement().extended(tolerance / 2.0);
        assert!(a
            .overlaps_other_inside_centered_unit_square_with_tolerance(&b, tolerance)
            .is_partial())
    }
    #[test]
    fn test_halfplane_overlap_within_unit_square__true__opposing_parallel_half_planes_retracted_but_within_tolerance(
    ) {
        let tolerance = 0.01;
        let a: LocalSquareHalfPlane = HalfPlane::new_from_border_line_with_origin_outside(
            TwoDifferentPoints::new_horizontal(0.5),
        );
        let b: LocalSquareHalfPlane = a.complement().retracted(tolerance / 2.0);
        assert!(a
            .overlaps_other_inside_centered_unit_square_with_tolerance(&b, tolerance)
            .is_partial())
    }
    #[test]
    fn test_halfplane_overlap_inside_unit_square__partial__exact__one_fully_covers__one_just_touches_corner(
    ) {
        let just_fully_covering: HalfPlane = HalfPlane::new_from_border_line_with_origin_inside(
            TwoDifferentPoints::new_horizontal(0.5),
        );
        let just_touching_corner: HalfPlane = HalfPlane::new_from_border_line_with_origin_outside(
            TwoDifferentPoints::new_from_two_points_on_line((0.0, 1.0), (0.5, 0.5)),
        );
        assert!(just_fully_covering
            .overlaps_other_inside_centered_unit_square_with_tolerance(&just_touching_corner, 0.01)
            .is_partial());
    }
    #[test]
    fn test_halfplane_overlap_unit_square() {
        let f = |top_y: f32, tolerance: f32| {
            HalfPlane::<TwoDifferentWorldPoints>::new_with_inside_down(top_y)
                .coverage_of_centered_unit_square_with_tolerance(tolerance)
        };
        use RelativeIntervalLocation::*;

        // more than fully covered
        assert_eq!(f(5.5, 0.01), After);
        // past far edge, but within threshold
        assert_eq!(f(0.51, 0.1), End);
        // on far edge exactly
        assert_eq!(f(0.5, 0.0), End);
        // before far edge, but within threshold
        assert_eq!(f(0.49, 0.1), End);
        // in the middle somewhere
        assert_eq!(f(0.2, 0.1), Inside);
        // past near edge, but within tolerance
        assert_eq!(f(-0.49, 0.1), Start);
        // on near edge exactly
        assert_eq!(f(-0.5, 0.0), Start);
        // before near edge, but within tolerance
        assert_eq!(f(-0.51, 0.1), Start);
        // not even close
        assert_eq!(f(-50.0, 0.1), Before);
    }
    #[test]
    fn test_halfplane_at_least_partially_covers_point() {
        let hp: LocalSquareHalfPlane = HalfPlane::new_with_inside_down(0.0);
        assert!(hp.at_least_partially_covers_point((0.0, 0.0)));
        assert!(hp.at_least_partially_covers_point((0.0, -0.1)));
        assert_false!(hp.at_least_partially_covers_point((0.0, 0.1)));
    }
    #[ignore = "Just very approximate for now"]
    #[test]
    fn test_fraction_coverage_of_square() {
        assert_about_eq!(
            WorldHalfPlane::new_with_inside_down(-0.4)
                .very_approximate_fraction_coverage_of_centered_unit_square(),
            0.1
        );
    }
}
