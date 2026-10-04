//! Projection P: the shared-vertical pseudo-isometric mapping.
//!
//! Top faces are drawn top-down exactly like the flat board renderer: one
//! world square is two terminal columns wide and one row tall (terminal cells
//! are about twice as tall as wide, so this keeps squares visually square).
//! A cube's south wall is a vertical extrusion below its top face.
//!
//! The defining rule is that **one terminal row is one world step north-south
//! or one step of altitude**:
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

/// The camera focuses on a continuous world `(x, y)`; altitude is not tracked
/// so a falling player slides down the screen.
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Camera {
    pub x: f32,
    pub y: f32,
}

impl Camera {
    pub fn at(x: f32, y: f32) -> Self {
        Camera { x, y }
    }
}

/// Project a continuous world point to integer `(col, row)` in a frame whose
/// row 0 is the top. May be off-screen.
pub fn project(cam: &Camera, width: usize, height: usize, x: f32, y: f32, z: f32) -> (i32, i32) {
    let col = (2.0 * (x - cam.x)).round() as i32 + (width as i32) / 2;
    let row = (cam.y - y - z).round() as i32 + (height as i32) / 2;
    (col, row)
}

/// The screen columns covering one world square's two terminal cells.
pub fn project_double(
    cam: &Camera,
    width: usize,
    height: usize,
    x: f32,
    y: f32,
    z: f32,
) -> (i32, i32, i32) {
    let (col, row) = project(cam, width, height, x, y, z);
    (col, col + 1, row)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn camera_target_projects_to_center() {
        let cam = Camera::at(5.0, 7.0);
        let (col, row) = project(&cam, 80, 24, 5.0, 7.0, 0.0);
        assert_eq!((col, row), (40, 12));
    }

    #[test]
    fn north_is_up_and_east_is_right() {
        let cam = Camera::at(0.0, 0.0);
        let (_, row_north) = project(&cam, 80, 24, 0.0, 1.0, 0.0);
        let (_, row_south) = project(&cam, 80, 24, 0.0, -1.0, 0.0);
        assert!(row_north < row_south, "north must be higher on screen");
        let (col_east, _) = project(&cam, 80, 24, 1.0, 0.0, 0.0);
        let (col_west, _) = project(&cam, 80, 24, -1.0, 0.0, 0.0);
        assert!(col_east > col_west, "east must be to the right");
    }

    #[test]
    fn one_altitude_step_equals_one_square_of_screen_vertical() {
        let cam = Camera::at(0.0, 0.0);
        let (_, row_ground) = project(&cam, 80, 24, 0.0, 0.0, 0.0);
        let (_, row_up) = project(&cam, 80, 24, 0.0, 0.0, 1.0);
        let (_, row_north) = project(&cam, 80, 24, 0.0, 1.0, 0.0);
        assert_eq!(row_ground - row_up, 1);
        assert_eq!(row_ground - row_north, 1);
    }

    #[test]
    fn a_world_square_is_two_columns_wide() {
        let cam = Camera::at(0.0, 0.0);
        let (left, right, _) = project_double(&cam, 80, 24, 1.0, 0.0, 0.0);
        assert_eq!(right - left, 1);
    }
}
