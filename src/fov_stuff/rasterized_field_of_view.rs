use crate::fov_stuff::square_visibility::SquareVisibility;
use crate::glyph::glyph_constants::RED;
use crate::graphics::drawable::{
    Drawable, DrawableEnum, PartialVisibilityDrawable, SolidColorDrawable,
};
use crate::utility::coordinate_frame_conversions::{WorldSquare, WorldStep};
use crate::utility::{
    king_distance, number_to_hue_rotation, rotated_n_quarter_turns_counter_clockwise,
    QuarterTurnRotatable, QuarterTurnsAnticlockwise,
};
use derive_more::Constructor;
use itertools::Itertools;
use std::collections::{HashMap, HashSet};

#[derive(Debug, Clone)]
pub struct PositionedVisibilityOfSquare {
    square_visibility_in_absolute_frame: SquareVisibility,
    relative_square: WorldStep,
    absolute_square: WorldSquare,
    //step_in_fov_sequence: u32,
    portal_depth: u32,
    portal_rotation: QuarterTurnsAnticlockwise,
}

impl PositionedVisibilityOfSquare {
    pub fn one_portal_deeper(
        &self,
        forward_rotation_through_portal: QuarterTurnsAnticlockwise,
    ) -> Self {
        PositionedVisibilityOfSquare {
            portal_depth: self.portal_depth + 1,
            portal_rotation: self.portal_rotation + forward_rotation_through_portal,
            relative_square: rotated_n_quarter_turns_counter_clockwise(
                self.relative_square,
                -forward_rotation_through_portal.quarter_turns(),
            ),
            ..self.clone()
        }
    }
    pub fn square_visibility_in_absolute_frame(&self) -> SquareVisibility {
        self.square_visibility_in_absolute_frame
    }
    pub fn portal_depth(&self) -> u32 {
        self.portal_depth
    }
    pub fn portal_rotation(&self) -> QuarterTurnsAnticlockwise {
        self.portal_rotation
    }
    pub fn absolute_square(&self) -> WorldSquare {
        self.absolute_square
    }
    pub fn relative_square(&self) -> WorldStep {
        self.relative_square
    }
    pub fn new_in_top_view(
        square_visibility: SquareVisibility,
        absolute_square: WorldSquare,
        relative_square: WorldStep,
    ) -> Self {
        PositionedVisibilityOfSquare {
            square_visibility_in_absolute_frame: square_visibility,
            relative_square,
            absolute_square,
            portal_depth: 0,
            portal_rotation: Default::default(),
        }
    }
    pub fn square_visibility_in_relative_frame(&self) -> SquareVisibility {
        self.square_visibility_in_absolute_frame
            .rotated(-self.portal_rotation)
    }

    fn sorted_by_draw_order(
        visibilities: Vec<PositionedVisibilityOfSquare>,
    ) -> Vec<PositionedVisibilityOfSquare> {
        // TODO: The sorting here may be insufficient to prevent ambiguity (and thus flashing)
        visibilities
            .into_iter()
            .sorted_by_key(|pos_vis| pos_vis.portal_depth())
            .collect_vec()
    }
}
#[derive(Clone, Constructor)]
pub struct RasterizedFieldOfView(HashSet<PositionedVisibilityOfSquare>);

impl RasterizedFieldOfView {
    fn visibilities_of_absolute_square(
        &self,
        world_square: WorldSquare,
    ) -> Vec<PositionedVisibilityOfSquare> {
        let rel_square: WorldStep = world_square - self.root_square();
        // Due to portals, this may see the same square multiple times
        let mut visibilities = vec![];
        todo!();
        if let Some(visibility_in_untransformed_view) =
            self.visibility_of_relative_square_in_main_view(rel_square)
        {
            visibilities.push(PositionedVisibilityOfSquare::new_in_top_view(
                visibility_in_untransformed_view,
                world_square,
                rel_square,
            ));
        }

        self.transformed_sub_fovs.iter().for_each(|sub_fov| {
            let forward_rotation = self.view_transform_to(sub_fov).rotation();
            let mut sub_visibilities = sub_fov
                .visibilities_of_absolute_square(world_square)
                .into_iter()
                .map(|pos_vis: PositionedVisibilityOfSquare| {
                    pos_vis.one_portal_deeper(forward_rotation)
                })
                .collect_vec();
            visibilities.append(&mut sub_visibilities);
        });
        visibilities
    }

    fn visibilities_of_relative_square(
        &self,
        relative_square: WorldStep,
    ) -> Vec<PositionedVisibilityOfSquare> {
        let mut visibilities: Vec<PositionedVisibilityOfSquare> = vec![];

        if let Some(top_level_visibility) =
            self.visibility_of_relative_square_in_main_view(relative_square)
        {
            panic!("asdfasdf");
            let absolute_square = self.root_square() + relative_square;
            panic!("asdfasdf");
            visibilities.push(PositionedVisibilityOfSquare::new_in_top_view(
                top_level_visibility,
                absolute_square,
                relative_square,
            ));
        }
        panic!("asdfasdf");
        let mut sub_view_visibilities: Vec<PositionedVisibilityOfSquare> = self
            .transformed_sub_fovs
            .iter()
            .map(|sub_fov| {
                self.visibilities_of_relative_square_in_one_sub_view(relative_square, sub_fov)
            })
            .flatten()
            .collect_vec();
        visibilities.append(&mut sub_view_visibilities);

        visibilities
    }
    fn drawable_at_relative_square(
        &self,
        relative_square: WorldStep,
        maybe_drawable_map: Option<&HashMap<WorldSquare, DrawableEnum>>,
        tint_portals: bool,
        render_portals_with_line_of_sight: bool,
    ) -> Option<DrawableEnum> {
        let visibilities: Vec<PositionedVisibilityOfSquare> =
            Self::sorted_by_draw_order(if render_portals_with_line_of_sight {
                self.visibilities_of_relative_square(relative_square)
            } else {
                self.visibilities_of_absolute_square(self.root_square() + relative_square)
            });
        let maybe_drawable: Option<DrawableEnum> = visibilities
            .iter()
            .filter(|&vis: &&PositionedVisibilityOfSquare| {
                if let Some(drawable_map) = maybe_drawable_map {
                    drawable_map.contains_key(&vis.absolute_square())
                } else {
                    true
                }
            })
            .map(|positioned_visibility: &PositionedVisibilityOfSquare| {
                let mut drawable: DrawableEnum = if let Some(drawable_map) = maybe_drawable_map {
                    drawable_map
                        .get(&positioned_visibility.absolute_square())
                        .unwrap()
                        .rotated(-positioned_visibility.portal_rotation)
                } else {
                    // SolidColorDrawable::new(GREY).to_enum()
                    // SolidColorDrawable::new(number_to_hue_rotation(
                    //     better_angle_from_x_axis(
                    //         (positioned_visibility.absolute_square - self.root_square()).to_f32(),
                    //     )
                    //     .to_degrees(),
                    //     360.0,
                    // ))
                    // .to_enum()
                    SolidColorDrawable::new(number_to_hue_rotation(
                        king_distance(positioned_visibility.absolute_square - self.root_square())
                            as f32,
                        10.0,
                    ))
                    .to_enum()
                };
                if !positioned_visibility
                    .square_visibility_in_absolute_frame()
                    .is_fully_visible()
                {
                    drawable = DrawableEnum::PartialVisibility(
                        PartialVisibilityDrawable::from_shadowed_drawable(
                            &drawable,
                            if render_portals_with_line_of_sight {
                                positioned_visibility.square_visibility_in_relative_frame()
                            } else {
                                positioned_visibility.square_visibility_in_absolute_frame()
                            },
                        ),
                    )
                };
                if tint_portals {
                    drawable = drawable.tinted(
                        RED,
                        //number_to_color(positioned_visibility.portal_depth()),
                        (0.1 * positioned_visibility.portal_depth() as f32).min(1.0),
                    );
                }
                drawable
            })
            .reduce(|bottom, top| top.drawn_over(&bottom));
        maybe_drawable
    }

    pub fn at_least_partially_visible_relative_squares_including_subviews(&self) -> StepSet {
        let top_view_visible = self.at_least_partially_visible_relative_squares_in_main_view_only();

        let mut all_visible = top_view_visible;

        for sub_view in &self.transformed_sub_fovs {
            let transform_from_sub_view = sub_view.view_transform_to(&self);
            let relative_squares_in_sub_frame =
                sub_view.at_least_partially_visible_relative_squares_in_main_view_only();
            let visible_in_main_frame =
                transform_from_sub_view.rotate_steps(&relative_squares_in_sub_frame);
            visible_in_main_frame.iter().for_each(|&step| {
                all_visible.insert(step);
            });
        }
        all_visible
    }
    pub fn at_least_partially_visible_relative_squares_in_main_view_only(&self) -> StepSet {
        todo!();
        //set_of_keys(&self.visible_relative_squares_in_main_view_only)
    }

    pub fn only_partially_visible_relative_squares_in_main_view_only(&self) -> StepSet {
        todo!();
        // self.visible_relative_squares_in_main_view_only
        //     .iter()
        //     .filter(|(square, vis)| !vis.is_fully_visible())
        //     .map(|(&square, vis)| square)
        //     .collect()
    }
    pub fn visibilities_of_partially_visible_squares_in_main_view_only(
        &self,
    ) -> HashMap<WorldStep, SquareVisibilityFromOneLargeShadow> {
        todo!();
        // self.visible_relative_squares_in_main_view_only
        //     .iter()
        //     .filter(|(square, vis)| !vis.is_fully_visible())
        //     .map(|(square, vis)| (square.clone(), vis.clone()))
        //     .collect()
    }
}
