//! The voxel model for the prototype: four equal cubes floating in space,
//! arranged in a 2x2 grid with a gap between them, each carrying a few
//! platforms on its sides for testing the sidescroller.
//!
//! Coordinates are `(x, y, z)` integers: `x` is east(+)/west(-), `y` is
//! north(+)/south(-), and `z` is altitude. A voxel occupies `[z, z+1)`, so a
//! solid voxel at index `z` has its top surface at altitude `z + 1`. See
//! `project.rs` for how these map onto terminal cells.

use std::collections::{HashMap, HashSet};

/// Side length of each cube's square footprint, in world squares.
pub const CUBE_SIZE: i32 = 10;
/// Height of every cube, in voxels.
pub const CUBE_HEIGHT: i32 = 10;
/// Empty squares between adjacent cubes, in world squares. Must be at least
/// `CUBE_HEIGHT` so a cube's south wall does not overlap the cube to its south
/// in the shared-vertical projection; wider leaves a band of visible void.
pub const CUBE_GAP: i32 = 14;

const _: () = assert!(
    CUBE_HEIGHT <= CUBE_GAP,
    "wall would overlap the cube to the south"
);

pub type Voxel = (i32, i32, i32);

#[derive(Clone, Debug, Default)]
pub struct World {
    solid: HashSet<Voxel>,
    /// Highest solid voxel index per column, for O(1) support queries.
    top: HashMap<(i32, i32), i32>,
}

impl World {
    /// Four cubes at `(0,0)`, `(stride,0)`, `(0,stride)`, `(stride,stride)`
    /// where `stride = CUBE_SIZE + CUBE_GAP`, each with side platforms.
    pub fn four_cubes() -> Self {
        let mut world = World::default();
        let stride = CUBE_SIZE + CUBE_GAP;
        for ix in 0..2 {
            for iy in 0..2 {
                let (cx, cy) = (ix * stride, iy * stride);
                world.add_cube(cx, cy);
                world.add_side_platforms(cx, cy);
            }
        }
        world
    }

    fn put(&mut self, x: i32, y: i32, z: i32) {
        self.solid.insert((x, y, z));
        let entry = self.top.entry((x, y)).or_insert(z);
        if z > *entry {
            *entry = z;
        }
    }

    fn add_cube(&mut self, cx: i32, cy: i32) {
        for x in cx..cx + CUBE_SIZE {
            for y in cy..cy + CUBE_SIZE {
                for z in 0..CUBE_HEIGHT {
                    self.put(x, y, z);
                }
            }
        }
    }

    /// Ledges attached to the cube's south, east, and west faces. The south
    /// set is a descending staircase drifting east, so a player who walks off
    /// the south edge can step his way down. The east and west sets protrude
    /// horizontally (three squares) with a lower step, so the same is possible
    /// off those faces — and, unlike the south set, their standoff is visible
    /// in projection P because x is the screen's horizontal axis.
    fn add_side_platforms(&mut self, cx: i32, cy: i32) {
        // South staircase: (dy south of the south edge, x start offset, slab z).
        for (dy, x_start, z) in [(1, 3, 7), (2, 4, 5), (3, 5, 3), (4, 6, 1)] {
            let y = cy - dy;
            for x in cx + x_start..cx + x_start + 3 {
                self.put(x, y, z);
            }
        }
        // East and west pairs: (offset from the face, y start, slab z, x length).
        for (offset, y_start, z, length) in [(0, 2, 6, 3), (1, 3, 3, 3)] {
            let east_start = cx + CUBE_SIZE + offset;
            let west_start = cx - 1 - offset - (length - 1);
            for step in 0..length {
                for y in cy + y_start..cy + y_start + 5 {
                    self.put(east_start + step, y, z);
                    self.put(west_start + step, y, z);
                }
            }
        }
    }

    pub fn is_solid(&self, x: i32, y: i32, z: i32) -> bool {
        self.solid.contains(&(x, y, z))
    }

    /// Highest solid voxel index in a column, or `None` if the column is empty.
    pub fn top_voxel(&self, x: i32, y: i32) -> Option<i32> {
        self.top.get(&(x, y)).copied()
    }

    /// Altitude of the top surface in a column (the standable height), or
    /// `None` if the column is empty space.
    pub fn top_height(&self, x: i32, y: i32) -> Option<i32> {
        self.top_voxel(x, y).map(|z| z + 1)
    }

    /// Every column that contains at least one solid voxel.
    pub fn columns(&self) -> Vec<(i32, i32)> {
        self.top.keys().copied().collect()
    }

    /// A column whose top is a full-height cube (as opposed to a ledge slab).
    pub fn is_cube_top_column(&self, x: i32, y: i32) -> bool {
        self.top_voxel(x, y) == Some(CUBE_HEIGHT - 1)
    }

    /// Every full-height cube column; used to measure a ledge's standoff.
    pub fn cube_columns(&self) -> Vec<(i32, i32)> {
        self.top
            .iter()
            .filter(|(_, &z)| z == CUBE_HEIGHT - 1)
            .map(|(&square, _)| square)
            .collect()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn four_cubes_are_equal_and_separated() {
        let world = World::four_cubes();
        let stride = CUBE_SIZE + CUBE_GAP;
        // Cube interiors are solid to full height.
        for (cx, cy) in [(0, 0), (stride, 0), (0, stride), (stride, stride)] {
            assert_eq!(world.top_height(cx, cy), Some(CUBE_HEIGHT));
            assert_eq!(
                world.top_height(cx + CUBE_SIZE - 1, cy + CUBE_SIZE - 1),
                Some(CUBE_HEIGHT)
            );
        }
    }

    #[test]
    fn gap_columns_are_void() {
        let world = World::four_cubes();
        let mid = CUBE_SIZE + CUBE_GAP / 2;
        assert_eq!(world.top_height(mid, mid), None);
        assert_eq!(world.top_height(CUBE_SIZE, CUBE_SIZE), None);
    }

    #[test]
    fn side_platforms_are_thin_slabs_with_void_under_them() {
        let world = World::four_cubes();
        // First south step: y = -1, x in 3..6, single voxel at z = 7.
        assert_eq!(world.top_height(3, -1), Some(8));
        assert!(world.is_solid(3, -1, 7));
        assert!(!world.is_solid(3, -1, 6), "platform must be a thin slab");
        assert!(!world.is_solid(3, -1, 0));
    }

    #[test]
    fn east_and_west_ledges_protrude_and_are_thin_slabs() {
        let world = World::four_cubes();
        // East: 3 squares out (x = CUBE_SIZE..), surfaces 7 then 4.
        assert_eq!(world.top_height(CUBE_SIZE, 2), Some(7));
        assert_eq!(world.top_height(CUBE_SIZE + 2, 2), Some(7));
        // The lower step is exposed only where the upper slab doesn't cover it.
        assert_eq!(world.top_height(CUBE_SIZE + 3, 3), Some(4));
        assert!(world.is_solid(CUBE_SIZE, 2, 6));
        assert!(!world.is_solid(CUBE_SIZE, 2, 5), "must be a thin slab");
        // West is the mirror image.
        assert_eq!(world.top_height(-1, 2), Some(7));
        assert_eq!(world.top_height(-3, 2), Some(7));
        assert_eq!(world.top_height(-4, 3), Some(4));
        assert!(world.is_solid(-1, 2, 6));
        assert!(!world.is_solid(-1, 2, 5));
    }

    #[test]
    fn cube_columns_exclude_ledges() {
        let world = World::four_cubes();
        assert!(world.is_cube_top_column(0, 0));
        assert!(!world.is_cube_top_column(CUBE_SIZE, 2));
        assert_eq!(
            world.cube_columns().len(),
            4 * (CUBE_SIZE * CUBE_SIZE) as usize
        );
    }

    #[test]
    fn wall_fits_in_gap() {
        assert!(CUBE_HEIGHT <= CUBE_GAP);
    }
}
