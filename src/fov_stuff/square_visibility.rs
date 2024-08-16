use crate::fov_stuff::*;
use crate::glyph::angled_blocks::angle_block_char_complement;
use crate::glyph::glyph_constants::{GREY, RED};
use crate::glyph::DoubleGlyphFunctions;
use crate::graphics::drawable::{
    Drawable, DrawableEnum, PartialVisibilityDrawable, SolidColorDrawable,
};
use crate::utility::*;
use derive_more::Constructor;
use euclid::Angle;
use itertools::Itertools;
use std::collections::HashMap;
use std::fmt::{Debug, Formatter};

// TODO: merge with generalization of BooleanWithPartial (BoolWithQuantifiedPartial?), (via newtype?)
#[derive(PartialEq, Clone, Copy)]
pub enum SquareVisibility<T: PartialSquareVisibilityOps> {
    FullyVisible,
    PartiallyVisible(T),
    NotVisible,
}
pub type SquareVisibilityFromOneHalfPlane =
    SquareVisibility<PartialSquareVisibilityByOneVisibleHalfPlane>;
pub type SquareVisibilityFromFovCones = SquareVisibility<PartialSquareVisibilityFromUnboundPolygon>;

pub type DefaultPartialSquareVisibilityType = PartialSquareVisibilityByOneVisibleHalfPlane;
pub type DefaultSquareVisibilityType = SquareVisibility<DefaultPartialSquareVisibilityType>;

impl<T: PartialSquareVisibilityOps> SquareVisibility<T> {
    pub fn visible_portion(&self) -> Option<T> {
        match self {
            Self::PartiallyVisible(v) => Some(v.clone()),
            _ => None,
        }
    }

    pub fn where_border_touches_unit_square(&self) -> Vec<LocalSquarePoint> {
        match self {
            SquareVisibility::PartiallyVisible(v) => v.where_border_touches_unit_square(),
            _ => vec![],
        }
    }


    pub fn is_fully_visible(&self) -> bool {
        matches!(self, Self::FullyVisible)
    }
    pub fn is_not_visible(&self) -> bool {
        matches!(self, Self::NotVisible)
    }
    pub fn is_nearly_or_fully_visible(&self, tolerance: f32) -> bool {
        self.is_fully_visible() || self.is_nearly_fully_visible(tolerance)
    }
    pub fn is_nearly_fully_visible(&self, tolerance_length: f32) -> bool {
        self.visible_portion()
            .is_some_and(|v| {
                v.is_within_distance_of_covering_centered_unit_square(tolerance_length)
            })
    }

    pub fn is_at_least_partially_visible(&self) -> bool {
        !self.is_not_visible()
    }
    pub fn is_only_partially_visible(&self) -> bool {
        matches!(self, Self::PartiallyVisible(_))
    }
    pub fn new_fully_visible() -> Self {
        Self::FullyVisible
    }
    pub fn new_not_visible() -> Self {
        Self::NotVisible
    }
    pub fn from_partial_viz_type(vis: T) -> Self {
        Self::PartiallyVisible(vis)
    }


    
    pub fn new_from_visible_half_plane(visible_portion: LocalSquareHalfPlane) -> Self where Self: Sized {
        assert!(visible_portion.at_least_partially_covers_unit_square());
        if visible_portion
            .fully_covers_centered_unit_square()
            .is_at_least_partial()
        {
            Self::new_fully_visible()
        } else if visible_portion
            .complement()
            .fully_covers_centered_unit_square()
            .is_at_least_partial()
        {
            Self::new_not_visible()
        } else {

            // NOTE: possibility of overlap check misalignment with full coverage check above, leading to panic on unwrap.
            Self::new_partially_visible_from_visible_half_plane(HalfPlaneCuttingLocalSquare::try_from(visible_portion).unwrap())
        }
    }    

    pub fn new_top_half_visible() -> Self{
        Self::from_partial_viz_type(T::half_visible(Angle::degrees(270.0)))
    }

    pub fn new_bottom_half_visible() -> Self{
        Self::from_partial_viz_type(T::half_visible(Angle::degrees(90.0)))
    }

    
    pub fn new_orthogonal_half_visible(which_half_visible: impl Into<OrthogonalDirection>) -> Self {
        Self::from_partial_viz_type(T::new_orthogonal_half_visible(which_half_visible))
    }

    pub fn new_partially_visible_from_visible_half_plane(visible_portion: HalfPlaneCuttingLocalSquare) -> Self {
        Self::from_partial_viz_type(T::new_from_visible_half_plane(visible_portion))
    }   

    pub fn combined_increasing_visibility(&self, other: &Self) -> Self {
        match self {
            SquareVisibility::FullyVisible => Self::FullyVisible,
            SquareVisibility::PartiallyVisible(v1) => match other {
                SquareVisibility::FullyVisible => self.clone(),
                SquareVisibility::PartiallyVisible(v2) => {
                    v1.combined_increasing_visibility(v2)


                    
                }
                SquareVisibility::NotVisible => self.clone(),
            },
            SquareVisibility::NotVisible => other.clone(),
        }
    }
}

impl<T: PartialSquareVisibilityOps> Complement for SquareVisibility<T> {
    type Output = Self;
    fn complement(&self) -> Self::Output {
        use SquareVisibility::*;
        match self {
            FullyVisible => Self::NotVisible,
            PartiallyVisible(v) => Self::PartiallyVisible(v.complement()),
            NotVisible => Self::FullyVisible,
        }
    }

    
}


// TODO: merge with SquareVisibilityOperations? rename?
pub trait ViewRoundable {
    fn rounded_towards_full_visibility(&self, tolerance_length: f32) -> Self;
}

// TODO: should this be a trait? (yes, because the half_plane square visibility is going to be swapped out, with these functions being common between the two)
// Might make more sense to have this not be a trait, and call methods of PartialSquareVisibilityOps
// TODO: remove.  put variation in the partialvisibility trait
pub trait SquareVisibilityOperations  {
    type PartialVizType: PartialSquareVisibilityOps;
    // visibility checks
    fn is_nearly_fully_visible(&self, tolerance_length: f32) -> bool;
    fn point_is_visible(&self, point: LocalSquarePoint) -> bool; // should return bool with partial for being on edge?

    fn from_relative_square_and_view_arc(
        view_arc: impl Into<AngleInterval>,
        rel_square: impl Into<WorldStep>,
    ) -> DefaultSquareVisibilityType;



    // other
    fn overlaps(&self, other: &Self, tolerance: f32) -> bool;
    fn as_string(&self) -> String;
    fn high_res_string(&self, output_diameter: u32) -> String;

    // TODO: add tolerance to these two?
    fn about_equal(&self, other: Self) -> bool;
    fn about_complementary(&self, other: Self) -> bool;

    fn is_visually_complementary_to(&self, other: Self) -> bool;
}

// TODO: merge with SquareVisibilityOperations
// pub trait SquareVisibilityFunctions: QuarterTurnRotatable {
//     fn is_fully_visible(&self) -> bool;
//     fn from_single_visible_arc(rel_square: WorldStep, visible_arc: AngleInterval) -> Self;
//     fn rounded_toward_full_visibility(&self, tolerance: f32) -> Self;
// }

impl<T: PartialSquareVisibilityOps> ViewRoundable for SquareVisibility<T> where Self: SquareVisibilityOperations {
    fn rounded_towards_full_visibility(&self, tolerance_length: f32) -> Self {
        if self.is_nearly_or_fully_visible(tolerance_length) {
            Self::new_fully_visible()
        } else {
            self.clone()
        }
    }
}
impl SquareVisibilityOperations for SquareVisibilityFromFovCones {
    type PartialVizType = PartialSquareVisibilityFromUnboundPolygon;

    fn is_nearly_fully_visible(&self, tolerance_length: f32) -> bool {
        todo!()
    }

    fn point_is_visible(&self, point: LocalSquarePoint) -> bool {
        todo!()
    }

    fn from_relative_square_and_view_arc(
        view_arc: impl Into<AngleInterval>,
        rel_square: impl Into<WorldStep>,
    ) -> DefaultSquareVisibilityType {
        todo!()
    }

    fn overlaps(&self, other: &Self, tolerance: f32) -> bool {
        todo!()
    }

    fn as_string(&self) -> String {
        todo!()
    }

    fn high_res_string(&self, output_diameter: u32) -> String {
        todo!()
    }

    fn about_equal(&self, other: Self) -> bool {
        todo!()
    }

    fn about_complementary(&self, other: Self) -> bool {
        todo!()
    }

    fn is_visually_complementary_to(&self, other: Self) -> bool {
        todo!()
    }
    }
impl SquareVisibilityOperations for SquareVisibilityFromOneHalfPlane {

    type PartialVizType = PartialSquareVisibilityByOneVisibleHalfPlane;

    fn from_relative_square_and_view_arc(
        view_arc: impl Into<AngleInterval>,
        rel_square: impl Into<WorldStep>,
    ) -> Self {
        let rel_square: WorldStep = rel_square.into(); // TODO: tired of writing this out a bunch
        if rel_square == STEP_ZERO {
            return Self::NotVisible;
        }
        let partial_view_arc = match view_arc.into() {
            AngleInterval::Empty => return Self::NotVisible,
            AngleInterval::FullCircle => return Self::FullyVisible,
            AngleInterval::PartialArc(partial) => partial,
        };
        let view_arc_edges: [AngularEdge; 2] = partial_view_arc.edges();
        if rel_square == point2(-1,-1) {
            dbg!(&view_arc_edges);
        }
        let intersections: Vec<HalfPlaneCuttingWorldSquare> = view_arc_edges
            .iter()
            .filter_map(|edge| edge.intersection_with_relative_square(rel_square))
            .collect();

        let substantial_intersections: Vec<HalfPlaneCuttingWorldSquare> = intersections
            .into_iter()
            .filter(|x| x.dividing_line().depth_in_square(rel_square) > 0.001) // TODO: standardize tolerance
            .collect();

        match substantial_intersections.len() {
            0 => {
                let square_is_in_arc = partial_view_arc
                    .contains_angle_inclusive(rel_square.better_angle_from_x_axis());
                if square_is_in_arc {
                    Self::FullyVisible
                } else {
                    Self::NotVisible
                }
            }
            1 => Self::new_partially_visible_from_visible_half_plane(half_plane_cutting_world_square_to_half_plane_local_square(
                substantial_intersections[0],
            )),
            2 => {
                let is_wraparound_case = fangle_dot(
                    partial_view_arc.center_angle(),
                    rel_square.better_angle_from_x_axis(),
                ) > 0.0;
                // high coverage percentage means more visible
                // wraparound case means there's a small angle segment not visible
                let key_fn =
                    |i: &&HalfPlaneCuttingWorldSquare| OrderedFloat(i.fraction_of_square_covered());
                let selected = if is_wraparound_case {
                    substantial_intersections.iter().max_by_key(key_fn)
                } else {
                    substantial_intersections.iter().min_by_key(key_fn)
                }
                .unwrap();
                dbg!(&selected);

                Self::new_partially_visible_from_visible_half_plane(half_plane_cutting_world_square_to_half_plane_local_square(
                    *selected,
                ))
            }
            n => panic!("invalid number of intersections: {}", n),
        }

        // TODO: delete below this line in this function

        // let angle_tolerance = FAngle::degrees(0.01); // TODO: double check this, maybe standardize
        // let length_tolerance = rel_square.to_f32().length() * angle_tolerance.radians;

        // let square_arc = PartialAngleInterval::from_relative_square(rel_square);
        // if partial_view_arc
        //     .contains_partial_arc(square_arc, angle_tolerance)
        //     .is_at_least_partial()
        // {
        //     Self::FullyVisible
        // } else if partial_view_arc
        //     .overlaps_partial_arc(square_arc, angle_tolerance)
        //     .is_at_least_partial()
        // {
        //     // TODO: double check tolerance choice on this "if"

        //     let shadow_arc = partial_view_arc.complement();
        //     let overlapped_shadow_edge = shadow_arc.most_overlapped_edge_of_self(square_arc);

        //     let shadow_line_from_center: TwoDifferentWorldPoints =
        //         TwoDifferentPoints::new_from_two_points_on_line(
        //             point2(0.0, 0.0),
        //             WorldPoint::unit_vector_from_angle(overlapped_shadow_edge.angle()).cast_unit(),
        //         );
        //     let point_in_shadow: WorldPoint =
        //         WorldPoint::unit_vector_from_angle(shadow_arc.center_angle()).cast_unit();

        //     let shadow_half_plane = HalfPlane::new_from_line_and_point_on_half_plane(
        //         shadow_line_from_center,
        //         point_in_shadow,
        //     );
        //     let square_shadow =
        //         world_half_plane_to_local_square_half_plane(shadow_half_plane, rel_square);

        //     let shadow_coverage_of_unit_square =
        //         square_shadow.coverage_of_centered_unit_square_with_tolerance(length_tolerance);

        //     match shadow_coverage_of_unit_square {
        //         RelativeIntervalLocation::MORE_THAN_FULL
        //         | RelativeIntervalLocation::EXACTLY_FULL => Self::NotVisible,
        //         RelativeIntervalLocation::PARTIALLY_FULL => {
        //             Self::PartiallyVisible(square_shadow.complement().try_into().unwrap())
        //         }
        //         RelativeIntervalLocation::EXACTLY_EMPTY
        //         | RelativeIntervalLocation::LESS_THAN_EMPTY => Self::FullyVisible,
        //     }
        // } else {
        //     Self::NotVisible
        // }
    }

    fn overlaps(&self, other: &Self, tolerance: f32) -> bool {
        match self {
            SquareVisibility::FullyVisible => match other {
                SquareVisibility::NotVisible => false,
                _ => true,
            },
            SquareVisibility::PartiallyVisible(v1) => match other {
                SquareVisibility::FullyVisible => true,
                SquareVisibility::PartiallyVisible(v2) => v1.half_plane()
                    .overlaps_other_inside_centered_unit_square_with_tolerance(&v2.half_plane(), tolerance)
                    .is_true(),

                SquareVisibility::NotVisible => false,
            },
            SquareVisibility::NotVisible => false,
        }
    }
    
    fn as_string(&self) -> String {
        use SquareVisibility::*;
        match self {
            FullyVisible => FULL_BLOCK.to_string().repeat(2) ,
            PartiallyVisible(partial) => {
                let fg_color = GREY;
                PartialVisibilityDrawable::from_shadowed_drawable_and_partial(
                    &SolidColorDrawable::new(fg_color),
                    *partial,
                )
                .to_glyphs()
                .to_clean_string()

                },
            NotVisible => "  ".to_string(),
        }
    }
    fn high_res_string(&self, output_diameter: u32) -> String {
        // TODO: Strong typing
        let dots_per_drawn_square_length = 4;
        let drawn_squares_per_world_square = output_diameter;
        let side_length_in_dots = 1 * drawn_squares_per_world_square * dots_per_drawn_square_length;
        let world_length_of_drawn_square = 1.0/drawn_squares_per_world_square as f32;
        let world_length_of_half_a_drawn_square = world_length_of_drawn_square/2.0;
        let dot_to_dot_world_distance = world_length_of_drawn_square /4.0;

        let world_position_of_top_left_drawn_square =  point2(-1.0, 1.0) * (0.5 -world_length_of_half_a_drawn_square);
        // start top left
        (0..drawn_squares_per_world_square).flat_map(|square_row| {
            (0..drawn_squares_per_world_square).flat_map(|square_col| {
                let drawn_square_center_in_world_coordinates = world_position_of_top_left_drawn_square + point2(square_col as f32, -(square_row as f32)) / drawn_squares_per_world_square as f32;

                let top_left_dot_rel_to_drawn_square_center = point2(-1, 1).to_f32() * 1.5 * dot_to_dot_world_distance;

                let bool_array_for_drawn_square = BoolArray2D::from_array([0,1,2,3].map(|dot_y| {
                    [0,1,2,3].map(|dot_x| {
                        let dot_position_rel_to_drawn_square_center_in_world_coordinates = top_left_dot_rel_to_drawn_square_center + (point2(1.0, 0.0)*dot_x as f32   + point2(0.0, -1.0)*dot_y as f32)*dot_to_dot_world_distance;
                        let dot_position_in_world_coord = drawn_square_center_in_world_coordinates +dot_position_rel_to_drawn_square_center_in_world_coordinates; 
                        let is_inside_half_plane = self.point_is_visible(dot_position_in_world_coord);
                        is_inside_half_plane
                    })

                }));
                 bool_array_for_drawn_square.chars().into_iter()
                
                

            }).chain("\n".chars()).collect_vec()
            
        }).collect()
    }

    fn about_equal(&self, other: Self) -> bool {
        match self {
            SquareVisibility::FullyVisible => other.is_fully_visible(),
            SquareVisibility::PartiallyVisible(v1) => other
                .visible_portion()
                .is_some_and(|v2| v1.half_plane().about_equal(v2.half_plane(), 1e-6)), // TODO: standardize tolerance
            SquareVisibility::NotVisible => other.is_not_visible(),
        }
    }
    fn about_complementary(&self, other: Self) -> bool {
        self.complement().about_equal(other)
    }

    fn is_visually_complementary_to(&self, other: Self) -> bool {
        self.as_string()
            .chars()
            .zip(other.as_string().chars())
            .all(|(c1, c2)| angle_block_char_complement(c1) == c2)
    }

    fn point_is_visible(&self, point: LocalSquarePoint) -> bool {
        assert!(point_is_in_centered_unit_square_with_tolerance(point, 0.0).is_at_least_partial(), "{:?}", point);
        match self {
            SquareVisibility::FullyVisible => true,
            SquareVisibility::PartiallyVisible(v) => {
                v.half_plane().at_least_partially_covers_point(point)
            }
            SquareVisibility::NotVisible => false,
        }
    }

    fn is_nearly_fully_visible(&self, tolerance_length: f32) -> bool {
        // self.faces()
        todo!()
    }

}
impl<T> QuarterTurnRotatable for SquareVisibility<T> where T: QuarterTurnRotatable + PartialSquareVisibilityOps {
    fn quarter_rotated_ccw(&self, quarter_turns_ccw: impl Into<NormalizedOrthoAngle>) -> Self {
        match self {
            SquareVisibility::PartiallyVisible(v) => {
                Self::from_partial_viz_type(v.quarter_rotated_ccw(quarter_turns_ccw))
            }
            _ => self.clone(),
        }
    }
}impl Debug for DefaultSquareVisibilityType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        use SquareVisibility::*;
        match self {
            FullyVisible => write!(f, "Fully Visible"),

            PartiallyVisible(v) => {
                write!(f, "Partially visible: {:#?}", v)
            }
            NotVisible => write!(f, "Not Visible"),
        }?;
        write!(
            f,
            "\n\
             chars: '{}'\n\
             {}",
            self.as_string(),
            self.high_res_string(7)
        )
    }
}


pub type LocalSquareVisibilityMap = HashMap<WorldStep, DefaultSquareVisibilityType>;

pub trait SquareVisibilityMapFunctions: ViewRoundable {
    fn combined_while_increasing_visibility(&self, other: &Self) -> Self;
    fn add_fully_visible_relative_square(&mut self, rel_square: impl Into<WorldStep>);
    fn new_empty() -> Self;
    fn new_with_only_center_visible() -> Self;
}

impl ViewRoundable for LocalSquareVisibilityMap {
    fn rounded_towards_full_visibility(&self, tolerance: f32) -> Self {
        self.iter()
            .map(|(&step, vis)| (step, vis.rounded_towards_full_visibility(tolerance)))
            .collect()
    }
}

impl SquareVisibilityMapFunctions for LocalSquareVisibilityMap {
    fn combined_while_increasing_visibility(&self, other: &Self) -> Self {
        let mut combined_vis_map = self.clone();
        other.iter().for_each(|entry_to_add| {
            let (key, value_to_add) = entry_to_add;
            let existing_entry = combined_vis_map.entry(*key);
            match existing_entry {
                std::collections::hash_map::Entry::Occupied(mut e) => {
                    e.insert(e.get().combined_increasing_visibility(value_to_add));
                }
                std::collections::hash_map::Entry::Vacant(e) => {
                    e.insert(*value_to_add);
                }
            }
        });
        combined_vis_map
    }

    fn add_fully_visible_relative_square(&mut self, rel_square: impl Into<WorldStep>) {
        self.insert(rel_square.into(), SquareVisibility::new_fully_visible());
    }
    fn new_empty() -> Self {
        Self::new()
    }
    fn new_with_only_center_visible() -> Self {
        let mut new_thing = Self::new_empty();
        new_thing.add_fully_visible_relative_square((0, 0));
        new_thing
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        glyph::glyph_constants::{FULL_BLOCK, SPACE},
        vec2,
    };
    use ntest::{assert_false, timeout};

    macro_rules! square_viz_tests {
($($name:ident: $type:ty,)*) => { $( mod $name {
            use super::*;

            #[test]
            fn test_square_visibility_knows_if_its_fully_visible() {
                let partial = <$type>::new_from_visible_half_plane(
                    HalfPlane::from_line_and_point_on_half_plane(
                        TwoDifferentPoints::from_two_points(point2(-5.0, 2.0), point2(5.0, 2.2928933)),
                        point2(-12.061038, -1.3054879),
                    ),
                );
                assert!(partial.is_fully_visible());
            }

            #[test]
            fn test_single_square_is_shadowed_correctly_on_diagonal() {
                let interval = PartialAngleInterval::from_degrees(0.0, 45.0).complement();
                let square_relative_to_center = vec2(1, 1);
                let visibility = <$type>::from_relative_square_and_view_arc(
                    interval,
                    square_relative_to_center,
                );
                let string = PartialVisibilityDrawable::from_square_visibility(visibility)
                    .to_glyphs()
                    .to_clean_string();
                assert_eq!(&string, "🭞🭚");
            }

            #[test]
            fn complementary_partial_squares_combine_to_full_visibility() {
                let line = TwoDifferentPoints::from_two_unordered_points_on_line(
                    point2(0.0, 0.0),
                    point2(1.0, 1.0),
                );
                let p1 = point2(0.0, 1.0);
                let p2 = point2(1.0, 0.0);

                let half_plane_1 = HalfPlane::from_line_and_point_on_half_plane(line, p1);
                let half_plane_2 = HalfPlane::from_line_and_point_on_half_plane(line, p2);
                assert!(half_plane_1.about_complementary(half_plane_2, 1e-6));

                let partial_1 = <$type>::new_from_visible_half_plane(half_plane_1);
                let partial_2 = <$type>::new_from_visible_half_plane(half_plane_2);

                let combined_partial = partial_1.combined_increasing_visibility(&partial_2);
                assert!(combined_partial.is_fully_visible());
            }
            #[test]
            fn test_partial_visibility_of_one_square__one_step_up() {
                let arc = PartialAngleInterval::from_degrees(90.0, 135.0);
                let square = WorldStep::new(0, 1);
                let partial = <$type>::from_relative_square_and_view_arc(arc, square);
                assert!(!partial.is_fully_visible());
                assert_eq!(
                    PartialVisibilityDrawable::from_square_visibility(partial)
                        .to_glyphs()
                        .to_clean_string(),
                    [FULL_BLOCK, SPACE].into_iter().collect::<String>()
                );
            }

            #[test]
            #[should_panic]
            fn test_one_shadow__should_fail_to_make_not_visible() {
                let non_vis = <$type>::new_partially_visible_from_visible_half_plane(
                    HalfPlaneCuttingLocalSquare::top_half_plane().extended(-0.6),
                );
            }
            #[test]
            fn test_one_shadow__fully_visible() {
                let vis =<$type>::new_fully_visible();

                assert!(!vis.is_only_partially_visible());
                assert!(vis.is_at_least_partially_visible());
                assert!(vis.is_fully_visible());
            }

            #[test]
            fn test_one_shadow__only_partially_visible() {
                let vis =<$type>::new_partially_visible_from_visible_half_plane(
                    HalfPlaneCuttingLocalSquare::top_half_plane().extended(0.5 - 1e-3),
                );
                assert!(vis.is_only_partially_visible());
                assert!(vis.is_at_least_partially_visible());
            }
            #[test]
            fn test_one_shadow__diagonal_partially_visible() {
                let line = TwoDifferentPoints::from_two_unordered_points_on_line(
                    point2(0.0, 0.0),
                    point2(1.0, 1.0),
                );
                let p1 = point2(0.0, 1.0);

                let half_plane_1 = HalfPlane::from_line_and_point_on_half_plane(line, p1);
                let partial_1 = <$type>::new_from_visible_half_plane(half_plane_1);
                assert!(partial_1.is_only_partially_visible());
            }
            #[test]
            fn test_one_shadow__almost_fully_visible() {
                let vis = <$type>::new_partially_visible_from_visible_half_plane(
                    HalfPlaneCuttingLocalSquare::top_half_plane().extended(0.5 - 1e-3),
                );

                assert!(!vis.is_nearly_fully_visible(0.0));
                assert!(!vis.is_nearly_fully_visible(1e-4));
                assert!(vis.is_nearly_fully_visible(1e-2));
            }
            #[test]
            fn test_square_visibility_overlap__simple_non_overlap() {
                let up = <$type>::new_partially_visible_from_visible_half_plane(
                    HalfPlaneCuttingLocalSquare::from_line_and_point_on_half_plane(
                        TwoPointsOnDifferentFacesOfCenteredUnitSquare::new_horizontal(0.4),
                        (0.0, 1.0).into(),
                    ),
                );
                let down = <$type>::new_partially_visible_from_visible_half_plane(
                    HalfPlaneCuttingLocalSquare::from_line_and_point_on_half_plane(
                        TwoPointsOnDifferentFacesOfCenteredUnitSquare::new_horizontal(0.3),
                        (0.0, -1.0).into(),
                    ),
                );
                assert_false!(up.overlaps(&down, 1e-5));
                assert_false!(down.overlaps(&up, 1e-5));
                assert_false!(up.overlaps(&up.complement(), 1e-5));
            }

            #[test]
            fn test_square_visibility_overlap__simple_overlap() {
                let vis1 = <$type>::new_partially_visible_from_visible_half_plane(
                    HalfPlaneCuttingLocalSquare::from_line_and_point_on_half_plane(
                        TwoPointsOnDifferentFacesOfCenteredUnitSquare::new_horizontal(-0.3),
                        (0.0, 1.0).into(),
                    ),
                );
                let vis2 = <$type>::new_partially_visible_from_visible_half_plane(
                    HalfPlaneCuttingLocalSquare::from_line_and_point_on_half_plane(
                        TwoPointsOnDifferentFacesOfCenteredUnitSquare::new_horizontal(0.2),
                        (0.0, -1.0).into(),
                    ),
                );
                assert!(vis1.overlaps(&vis2, 1e-5));
                assert!(vis2.overlaps(&vis1, 1e-5));
            }
            #[test]
            fn test_view_arc_source_is_not_visible_by_default() {
                assert!( <$type>::from_relative_square_and_view_arc(
                    AngleInterval::from_degrees(0.0, 45.0),
                    (0, 0)
                )
                .is_not_visible());
            }
            #[test]
            fn test_view_arc_source_is_not_visible_by_default__full_circle() {
                let arc =
                    <$type>::from_relative_square_and_view_arc(AngleInterval::FullCircle, (0, 0));
                assert!(arc.is_not_visible());
            }
            #[test]
            fn test_debug_draw_arc() {
                let arc =
                    <$type>::from_relative_square_and_view_arc(AngleInterval::FullCircle, (0, 0));
                dbg!(arc); // NOTE: keep this debug statement because it's part of the test
            }
            // TODO: find easier ways to generally get vectors pointing in cardinal directions with any type and unit



            
            #[test]
            fn test_square_visibility__if_visible_should_have_intersections_with_unit_square() {
                let hp =
                    LocalSquareHalfPlane::new_from_normal_vector_going_from_origin_to_inside_edge_of_border((0.5, 0.5).into()).extended(-0.1);
                let vis = <$type>::new_from_visible_half_plane(hp);
                assert!(vis.is_nearly_or_fully_visible(0.15));
                assert!(vis.is_only_partially_visible());
            }


            
        } )* }
    }

    square_viz_tests!(
        fromhalf_plane: SquareVisibilityFromOneHalfPlane,
        fromfovcones: SquareVisibilityFromFovCones,
    );

    
}
