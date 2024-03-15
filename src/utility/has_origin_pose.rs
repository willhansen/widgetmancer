use crate::utility::*;

pub(crate) trait HasOriginPose {
    const DEFAULT_FOV_ROOT_DIRECTION_STEP: WorldStep = STEP_UP;
    fn default_origin_pose_orientation() -> OrthoAngle {
        OrthoAngle::from_orthogonal_vector(Self::DEFAULT_FOV_ROOT_DIRECTION_STEP)
    }
    fn origin_pose(&self) -> WorldSquareWithOrthogonalDir;
    fn view_root_orientation_relative_to_default(&self) -> OrthoAngle {
        self.origin_pose().direction_in_quarter_turns() - Self::default_origin_pose_orientation()
    }
}
