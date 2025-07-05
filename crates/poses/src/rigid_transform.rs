use crate::*;

#[derive(Hash, Clone, Copy, Debug)]
pub struct RigidTransform {
    start_pose: SquareWithOrthogonalDirection,
    end_pose: SquareWithOrthogonalDirection,
}

fn ORIGIN_POSE() -> SquareWithOrthogonalDirection {
    SquareWithOrthogonalDirection::from_x_y_dir(0, 0, OrthogonalDirection::UP)
}

impl RigidTransform {
    pub fn from_start_and_end_poses(
        start: SquareWithOrthogonalDirection,
        end: SquareWithOrthogonalDirection,
    ) -> Self {
        RigidTransform {
            start_pose: start.into(),
            end_pose: end.into(),
        }
    }
    // Treats the start pose as the origin
    // TODO: move to a relative version of a rigidtransform
    pub fn new_relative_transform_from_start_to_end(
        start: SquareWithOrthogonalDirection,
        end: SquareWithOrthogonalDirection,
    ) -> Self {

        Self::from_start_and_end_poses(ORIGIN_POSE(), start.other_pose_absolute_to_relative(end))
    }
    pub fn from_rotation(r: impl Into<NormalizedOrthoAngle> + Copy) -> Self {
        let p = ORIGIN_POSE();
        Self::from_start_and_end_poses(p, p.quarter_rotated_ccw_in_place(r))
    }
    pub fn identity() -> Self {
        Self::from_start_and_end_poses((0, 0, UP), (0, 0, UP))
    }
    pub fn translation(&self) -> WorldStep {
        (self.end_pose - self.start_pose).square()
    }
    pub fn rotation(&self) -> NormalizedOrthoAngle {
        (self.end_pose - self.start_pose).angle()
    }
    // TODO: maybe te.st this if sus
    pub fn transform_relative_pose(
        &self,
        pose: RelativeSquareWithOrthogonalDir,
    ) -> RelativeSquareWithOrthogonalDir {
        let end_square = pose
            .square()
            .quarter_rotated_ccw(self.rotation().quarter_turns());

        let end_direction = self.rotation() + pose.angle();

        RelativeSquareWithOrthogonalDir::from_square_and_dir(end_square, end_direction)
    }
    pub fn inverse(&self) -> Self {
        Self {
            start_pose: self.end_pose,
            end_pose: self.start_pose,
        }
    }
    pub fn transform_octant(&self, octant: Octant) -> Octant {
        octant.with_n_quarter_turns_anticlockwise(self.rotation())
    }
    pub fn rotate_step(&self, step: WorldStep) -> WorldStep {
        self.rotation().rotate_vector(step)
    }
    pub fn rotate_steps(&self, steps: &StepSet) -> StepSet {
        steps
            .iter()
            .map(|&step: &WorldStep| self.rotation().rotate_vector(step))
            .collect()
    }
    pub fn transform_ray(
        &self,
        ray_start: WorldPoint,
        ray_direction: Angle<f32>,
    ) -> (WorldPoint, Angle<f32>) {
        let ray_start_relative_to_tf_start = ray_start - self.start_pose.square().to_f32();
        let dist_from_tf_start = ray_start_relative_to_tf_start.length();
        let start_tf_angle = self
            .start_pose
            .direction()
            .to_step::<WorldStep>()
            .to_f32()
            .better_angle_from_x_axis();

        let ray_angle_from_tf_start = start_tf_angle.angle_to(ray_direction);
        let end_tf_angle = self.end_pose.angle().to_float_angle();

        let new_ray_direction = end_tf_angle + ray_angle_from_tf_start;

        let new_ray_start = if dist_from_tf_start == 0.0 {
            self.end_pose.square().to_f32()
        } else {
            let tf_start_point: WorldMove = self.start_pose.direction().to_step();
            let position_angle_from_tf_start =
                tf_start_point.angle_to(ray_start_relative_to_tf_start);

            let position_angle_from_tf_end = end_tf_angle + position_angle_from_tf_start;

            self.end_pose.square().to_f32()
                + WorldMove::unit_vector_from_angle(position_angle_from_tf_end) * dist_from_tf_start
        };

        (new_ray_start, new_ray_direction)
    }
}

impl PartialEq for RigidTransform {
    fn eq(&self, other: &Self) -> bool {
        other.start_pose.apply_rigid_transform(*self) == other.end_pose
    }
}

impl Eq for RigidTransform {}

impl Default for RigidTransform {
    fn default() -> Self {
        RigidTransform::from_start_and_end_poses(
            SquareWithOrthogonalDirection::from_square_and_step(point2(0, 0), STEP_RIGHT),
            SquareWithOrthogonalDirection::from_square_and_step(point2(0, 0), STEP_RIGHT),
        )
    }
}
