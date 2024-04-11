use crate::{angled_blocks::angle_block_char_complement, fov_stuff::*, graphics::drawable::PartialVisibilityDrawable};

#[derive(PartialEq, Clone)]
pub enum SquareVisibilityFromOneLargeShadow {
    FullyVisible,
    // TODO: have more than one half plane (two?)
    PartiallyVisible(HalfPlaneCuttingLocalSquare),
    // PartiallyVisible(LocalSquareHalfPlaneWithBorderOnUnitSquare), // TODO
    NotVisible,
}
// not derived because error trace through macro is nightly only
impl Copy for SquareVisibilityFromOneLargeShadow {}
impl SquareVisibilityFromOneLargeShadow {
    pub(crate) fn visible_portion(&self) -> Option<HalfPlaneCuttingLocalSquare> {
        match self {
            Self::PartiallyVisible(v) => Some(*v),
            _ => None,
        }
    }
    pub fn complement(&self) -> Self {
        match self {
            SquareVisibilityFromOneLargeShadow::FullyVisible => Self::NotVisible,
            SquareVisibilityFromOneLargeShadow::PartiallyVisible(v) => {
                Self::PartiallyVisible(v.complement())
            }
            SquareVisibilityFromOneLargeShadow::NotVisible => Self::FullyVisible,
        }
    }
    pub fn where_border_touches_unit_square(&self) -> Vec<LocalSquarePoint> {
        if self.is_fully_visible() {
            return vec![];
        }
        self.visible_portion()
            .unwrap()
            .dividing_line()
            .ordered_line_intersections_with_centered_unit_square()
    }

    pub fn new_orthogonal_half_visible(which_half_visible: impl Into<OrthogonalDirection>) -> Self {
        Self::half_visible((-which_half_visible.into()).angle().into())
    }

    fn half_visible(shadow_direction: Angle<f32>) -> Self {
        // TODO: may be backwards
        let shadow_direction = standardize_angle_with_zero_mid(shadow_direction);
        let shadow_line = TwoPointsOnDifferentFacesOfCenteredUnitSquare::new_through_origin(
            LocalSquarePoint::unit_vector_from_angle(shadow_direction.turned_left()),
        );
        Self::new_partially_visible(
            HalfPlaneCuttingLocalSquare::new_from_line_and_point_on_half_plane(
                shadow_line,
                LocalSquarePoint::unit_vector_from_angle(shadow_direction),
            ),
        )
    }
}
impl ViewRoundable for SquareVisibilityFromOneLargeShadow {
    fn rounded_towards_full_visibility(&self, tolerance_length: f32) -> Self {
        if self.is_nearly_or_fully_visible(tolerance_length) {
            Self::new_fully_visible()
        } else {
            self.clone()
        }
    }
}impl SquareVisibilityOperations for SquareVisibilityFromOneLargeShadow {
    fn is_fully_visible(&self) -> bool {
        matches!(self, Self::FullyVisible)
    }
    fn is_not_visible(&self) -> bool {
        matches!(self, Self::NotVisible)
    }

    fn is_at_least_partially_visible(&self) -> bool {
        !self.is_not_visible()
    }
    fn is_only_partially_visible(&self) -> bool {
        matches!(self, Self::PartiallyVisible(_))
    }
    fn is_nearly_or_fully_visible(&self, tolerance: f32) -> bool {
        self.is_fully_visible() || self.is_just_barely_fully_visible(tolerance)
    }
    fn is_just_barely_fully_visible(&self, tolerance: f32) -> bool {
        self.visible_portion()
            .is_some_and(|v: HalfPlaneCuttingLocalSquare| {
                v.fully_covers_centered_unit_square_with_tolerance(tolerance)
                    .is_partial()
            })
    }
    fn new_fully_visible() -> Self {
        Self::FullyVisible
    }

    fn new_partially_visible(visible_portion: HalfPlaneCuttingLocalSquare) -> Self {
        SquareVisibilityFromOneLargeShadow::PartiallyVisible(visible_portion)
    }

    fn new_from_visible_half_plane(visible_portion: LocalSquareHalfPlane) -> Self {
        assert!(visible_portion.at_least_partially_covers_unit_square());
        if visible_portion
            .fully_covers_centered_unit_square()
            .is_at_least_partial()
        {
            Self::FullyVisible
        } else if visible_portion
            .complement()
            .fully_covers_centered_unit_square()
            .is_at_least_partial()
        {
            Self::NotVisible
        } else {
            // NOTE: possibility of overlap check misalignment with full coverage check above, leading to panic on unwrap.
            Self::PartiallyVisible(visible_portion.try_into().unwrap())
        }
    }
    fn new_top_half_visible() -> Self {
        Self::half_visible(Angle::degrees(270.0))
    }

    fn new_bottom_half_visible() -> Self {
        Self::half_visible(Angle::degrees(90.0))
    }
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
            1 => Self::PartiallyVisible(halfplane_cutting_world_square_to_halfplane_local_square(
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

                Self::PartiallyVisible(halfplane_cutting_world_square_to_halfplane_local_square(
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

    fn overlaps(&self, other: Self, tolerance: f32) -> bool {
        match self {
            SquareVisibilityFromOneLargeShadow::FullyVisible => match other {
                SquareVisibilityFromOneLargeShadow::NotVisible => false,
                _ => true,
            },
            SquareVisibilityFromOneLargeShadow::PartiallyVisible(v1) => match other {
                SquareVisibilityFromOneLargeShadow::FullyVisible => true,
                SquareVisibilityFromOneLargeShadow::PartiallyVisible(v2) => v1
                    .overlaps_other_inside_centered_unit_square_with_tolerance(&v2, tolerance)
                    .is_true(),

                SquareVisibilityFromOneLargeShadow::NotVisible => false,
            },
            SquareVisibilityFromOneLargeShadow::NotVisible => false,
        }
    }
    fn combined_increasing_visibility(&self, other: &Self) -> Self {
        match self {
            SquareVisibilityFromOneLargeShadow::FullyVisible => Self::FullyVisible,
            SquareVisibilityFromOneLargeShadow::PartiallyVisible(v1) => match other {
                SquareVisibilityFromOneLargeShadow::FullyVisible => *self,
                SquareVisibilityFromOneLargeShadow::PartiallyVisible(v2) => {
                    if v1.about_complementary(*v2, 1e-6) {
                        Self::FullyVisible
                    } else {
                        let depth_a = v1.depth_of_point_in_half_plane(point2(0.0, 0.0));
                        let depth_b = v2.depth_of_point_in_half_plane(point2(0.0, 0.0));

                        if depth_a > depth_b {
                            *self
                        } else {
                            *other
                        }
                    }
                }
                SquareVisibilityFromOneLargeShadow::NotVisible => *self,
            },
            SquareVisibilityFromOneLargeShadow::NotVisible => *other,
        }
    }
    fn as_string(&self) -> String {
        if self.is_fully_visible() {
            "  ".to_string()
        } else {
            let fg_color = GREY;
            PartialVisibilityDrawable::from_shadowed_drawable(
                &SolidColorDrawable::new(fg_color),
                *self,
            )
            .to_glyphs()
            .to_clean_string()
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
            SquareVisibilityFromOneLargeShadow::FullyVisible => other.is_fully_visible(),
            SquareVisibilityFromOneLargeShadow::PartiallyVisible(v1) => other
                .visible_portion()
                .is_some_and(|v2| v1.about_equal(v2, 1e-6)), // TODO: standardize tolerance
            SquareVisibilityFromOneLargeShadow::NotVisible => other.is_not_visible(),
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

    fn point_is_visible(&self, point: impl Into<LocalSquarePoint> + Copy) -> bool {
        assert!(point_is_in_centered_unit_square_with_tolerance(point, 0.0).is_at_least_partial(), "{:?}", point.into());
        match self {
            SquareVisibilityFromOneLargeShadow::FullyVisible => true,
            SquareVisibilityFromOneLargeShadow::PartiallyVisible(v) => {
                v.at_least_partially_covers_point(point)
            }
            SquareVisibilityFromOneLargeShadow::NotVisible => false,
        }
    }
}



impl QuarterTurnRotatable for SquareVisibilityFromOneLargeShadow {
    fn quarter_rotated_ccw(&self, quarter_turns_ccw: impl Into<NormalizedOrthoAngle>) -> Self {
        match self {
            SquareVisibilityFromOneLargeShadow::PartiallyVisible(v) => {
                Self::PartiallyVisible(v.quarter_rotated_ccw(quarter_turns_ccw))
            }
            _ => *self,
        }
    }
}impl Debug for SquareVisibilityFromOneLargeShadow {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        use SquareVisibilityFromOneLargeShadow::*;
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



