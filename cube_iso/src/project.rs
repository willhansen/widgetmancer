//! Projection P: the shared-vertical pseudo-isometric mapping.
//!
//! Top faces are drawn top-down exactly like the flat board renderer: one
//! world square is two terminal columns wide and one row tall (terminal cells
//! are about twice as tall as wide, so this keeps squares visually square).
//! A cube's wall toward the camera is a vertical extrusion below its top face.
//!
//! The defining rule is that **one terminal row is one world step along the
//! camera's forward axis or one step of altitude**. With no rotation
//! (`rotation == 0`) that is:
//!
//! ```text
//! col = 2 * (x - cam.x) + width/2
//! row =     (cam.y - y) - z + height/2
//! ```
//!
//! `row` grows downward (terminal order), world `y` grows north (up the
//! screen), and `z` grows up the screen. `cam.x`/`cam.y` follow the player so
//! the player stays near the middle of the screen; `z` is deliberately *not*
//! followed, which is what makes a fall visible.
//!
//! [`Camera::rotation`] is a count of 90-degree counter-clockwise quarter
//! turns applied to the world before projecting. Rotating turns a
//! north/south-facing structure into an east/west-facing one and back, which is
//! how a face whose depth is otherwise collapsed into the vertical axis can be
//! inspected: at any rotation, the faces aligned with the screen's *horizontal*
//! axis show their standoff directly.

/// A direction on the screen, independent of the view's world orientation.
/// Movement keys are interpreted in this frame so "up" always moves up.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ScreenDir {
    Up,
    Down,
    Left,
    Right,
}

/// The camera focuses on a continuous world `(x, y)`; altitude is not tracked
/// so a falling player slides down the screen. `rotation` is 0..=3 quarter
/// turns counter-clockwise.
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Camera {
    pub x: f32,
    pub y: f32,
    pub rotation: u8,
}

impl Camera {
    pub fn with_rotation(x: f32, y: f32, rotation: u8) -> Self {
        Camera {
            x,
            y,
            rotation: rotation % 4,
        }
    }

    /// Rotate a world offset into `(rx, ry)`, where `rx` grows to the screen's
    /// right and `ry` grows *into* the screen (up the screen after projection).
    fn rotated_offset(&self, x: f32, y: f32) -> (f32, f32) {
        rotate_ccw(x - self.x, y - self.y, self.rotation)
    }

    /// Project a continuous world point to integer `(col, row)` in a frame
    /// whose row 0 is the top. May be off-screen.
    pub fn project(&self, width: usize, height: usize, x: f32, y: f32, z: f32) -> (i32, i32) {
        let (rx, ry) = self.rotated_offset(x, y);
        let col = (2.0 * rx).round() as i32 + (width as i32) / 2;
        let row = (-ry - z).round() as i32 + (height as i32) / 2;
        (col, row)
    }

    /// The screen columns covering one world square's two terminal cells.
    pub fn project_double(
        &self,
        width: usize,
        height: usize,
        x: f32,
        y: f32,
        z: f32,
    ) -> (i32, i32, i32) {
        let (col, row) = self.project(width, height, x, y, z);
        (col, col + 1, row)
    }

    /// World step that appears to move **up** the screen.
    pub fn forward_step(&self) -> (i32, i32) {
        self.world_step_from_screen((0, 1))
    }

    /// World step that appears to move **down** the screen; the wall face the
    /// camera sees is on this side of a voxel.
    pub fn toward_camera_step(&self) -> (i32, i32) {
        self.world_step_from_screen((0, -1))
    }

    pub fn screen_right_step(&self) -> (i32, i32) {
        self.world_step_from_screen((1, 0))
    }

    pub fn screen_left_step(&self) -> (i32, i32) {
        self.world_step_from_screen((-1, 0))
    }

    /// Inverse-rotate a screen-axis vector back into a world step. Screen unit
    /// vectors rotate to unit world steps, so rounding is exact.
    fn world_step_from_screen(&self, screen: (i32, i32)) -> (i32, i32) {
        let (wx, wy) = rotate_ccw(screen.0 as f32, screen.1 as f32, (4 - self.rotation) % 4);
        (wx.round() as i32, wy.round() as i32)
    }

    /// World step for a screen direction, used to make input view-relative.
    pub fn world_step_for_screen_dir(&self, dir: ScreenDir) -> (i32, i32) {
        match dir {
            ScreenDir::Up => self.forward_step(),
            ScreenDir::Down => self.toward_camera_step(),
            ScreenDir::Left => self.screen_left_step(),
            ScreenDir::Right => self.screen_right_step(),
        }
    }

    /// Depth in the rotated frame (`|into-screen| + half |horizontal|`), for
    /// painter ordering and depth fog.
    pub fn depth(&self, x: f32, y: f32) -> f32 {
        let (rx, ry) = self.rotated_offset(x, y);
        ry.abs() + 0.5 * rx.abs()
    }
}

/// Rotate `(dx, dy)` counter-clockwise by `turns` quarter turns.
fn rotate_ccw(dx: f32, dy: f32, turns: u8) -> (f32, f32) {
    match turns % 4 {
        0 => (dx, dy),
        1 => (-dy, dx),
        2 => (-dx, -dy),
        _ => (dy, -dx),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn camera_target_projects_to_center() {
        let cam = Camera::with_rotation(5.0, 7.0, 0);
        let (col, row) = cam.project(80, 24, 5.0, 7.0, 0.0);
        assert_eq!((col, row), (40, 12));
    }

    #[test]
    fn north_is_up_and_east_is_right() {
        let cam = Camera::with_rotation(0.0, 0.0, 0);
        let (_, row_north) = cam.project(80, 24, 0.0, 1.0, 0.0);
        let (_, row_south) = cam.project(80, 24, 0.0, -1.0, 0.0);
        assert!(row_north < row_south, "north must be higher on screen");
        let (col_east, _) = cam.project(80, 24, 1.0, 0.0, 0.0);
        let (col_west, _) = cam.project(80, 24, -1.0, 0.0, 0.0);
        assert!(col_east > col_west, "east must be to the right");
    }

    #[test]
    fn one_altitude_step_equals_one_square_of_screen_vertical() {
        let cam = Camera::with_rotation(0.0, 0.0, 0);
        let (_, row_ground) = cam.project(80, 24, 0.0, 0.0, 0.0);
        let (_, row_up) = cam.project(80, 24, 0.0, 0.0, 1.0);
        let (_, row_north) = cam.project(80, 24, 0.0, 1.0, 0.0);
        assert_eq!(row_ground - row_up, 1);
        assert_eq!(row_ground - row_north, 1);
    }

    #[test]
    fn a_world_square_is_two_columns_wide() {
        let cam = Camera::with_rotation(0.0, 0.0, 0);
        let (left, right, _) = cam.project_double(80, 24, 1.0, 0.0, 0.0);
        assert_eq!(right - left, 1);
    }

    #[test]
    fn one_turn_puts_north_left_and_east_up() {
        let cam = Camera::with_rotation(0.0, 0.0, 1);
        let (col_north, _) = cam.project(80, 24, 0.0, 1.0, 0.0);
        let (col_south, _) = cam.project(80, 24, 0.0, -1.0, 0.0);
        assert!(
            col_north < col_south,
            "after one turn, north is to the left"
        );
        let (_, row_east) = cam.project(80, 24, 1.0, 0.0, 0.0);
        let (_, row_west) = cam.project(80, 24, -1.0, 0.0, 0.0);
        assert!(row_east < row_west, "after one turn, east is up");
    }

    #[test]
    fn direction_steps_cycle_with_rotation() {
        let cases = [
            (0, (0, 1), (0, -1), (1, 0)),
            (1, (1, 0), (-1, 0), (0, -1)),
            (2, (0, -1), (0, 1), (-1, 0)),
            (3, (-1, 0), (1, 0), (0, 1)),
        ];
        for (rotation, forward, toward, right) in cases {
            let cam = Camera::with_rotation(0.0, 0.0, rotation);
            assert_eq!(cam.forward_step(), forward, "forward at {rotation}");
            assert_eq!(cam.toward_camera_step(), toward, "toward at {rotation}");
            assert_eq!(cam.screen_right_step(), right, "right at {rotation}");
            assert_eq!(cam.screen_left_step(), (-right.0, -right.1));
        }
    }

    #[test]
    fn movement_is_view_relative() {
        assert_eq!(
            Camera::with_rotation(0.0, 0.0, 0).world_step_for_screen_dir(ScreenDir::Up),
            (0, 1),
            "with no rotation, up is north"
        );
        assert_eq!(
            Camera::with_rotation(0.0, 0.0, 1).world_step_for_screen_dir(ScreenDir::Up),
            (1, 0),
            "after one turn, up is east"
        );
        assert_eq!(
            Camera::with_rotation(0.0, 0.0, 1).world_step_for_screen_dir(ScreenDir::Right),
            (0, -1),
            "after one turn, right is south"
        );
    }

    #[test]
    fn depth_follows_the_rotated_frame() {
        // At rotation 1, east/west are the forward axis and north is sideways,
        // so forward geometry is "farther" than sideways geometry.
        let cam = Camera::with_rotation(0.0, 0.0, 1);
        assert!(cam.depth(5.0, 0.0) > cam.depth(0.0, 5.0));
        assert!(cam.depth(0.0, 5.0) > cam.depth(0.0, 0.0));
        assert_eq!(cam.depth(5.0, 0.0), cam.depth(-5.0, 0.0));
    }
}
