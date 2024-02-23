use crate::utility::QuarterTurnsCcw;

use super::{
    coordinate_frame_conversions::{WorldStep, STEP_UP},
    poses::SquareWithOrthogonalDir,
};

pub(crate) trait HasOriginPose {
    const DEFAULT_FOV_ROOT_DIRECTION_STEP: WorldStep = STEP_UP;
    fn default_origin_pose_orientation() -> QuarterTurnsCcw {
        QuarterTurnsCcw::from_vector(Self::DEFAULT_FOV_ROOT_DIRECTION_STEP)
    }
    fn origin_pose(&self) -> SquareWithOrthogonalDir;
    fn view_root_orientation_relative_to_default(&self) -> QuarterTurnsCcw {
        self.origin_pose().direction_in_quarter_turns() - Self::default_origin_pose_orientation()
    }
}