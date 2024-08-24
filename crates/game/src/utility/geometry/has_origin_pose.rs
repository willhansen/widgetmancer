use crate::utility::*;

pub trait HasOriginPose {
    const DEFAULT_FOV_ROOT_DIRECTION: OrthogonalDirection = UP;
    fn default_origin_pose_orientation() -> NormalizedOrthoAngle {
        Self::DEFAULT_FOV_ROOT_DIRECTION.into()
    }
    fn origin_pose(&self) -> WorldSquareWithOrthogonalDir;
    fn view_root_orientation_relative_to_default(&self) -> NormalizedOrthoAngle {
        self.origin_pose().angle() - Self::default_origin_pose_orientation()
    }
}
