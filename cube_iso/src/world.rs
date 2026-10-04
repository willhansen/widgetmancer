//! The voxel model for the prototype: four equal cubes floating in space,
//! arranged in a 2x2 grid with a gap between them.
//!
//! Coordinates are `(x, y, z)` integers: `x` is east(+)/west(-), `y` is
//! north(+)/south(-), and `z` is altitude. A "column" at `(x, y)` is either
//! the top of a cube (`z == CUBE_HEIGHT`) or empty void. See `project.rs`
//! for how these map onto terminal cells.

/// Side length of each cube's square footprint, in world squares.
pub const CUBE_SIZE: i32 = 4;
/// Height of every cube, in altitude steps. Must be `<= CUBE_GAP` so a cube's
/// south wall does not overlap the cube to its south in the shared-vertical
/// projection.
pub const CUBE_HEIGHT: i32 = 3;
/// Empty squares between adjacent cubes, in world squares. Wider than
/// `CUBE_HEIGHT` so a visible band of void separates a cube's south wall from
/// the cube below it.
pub const CUBE_GAP: i32 = 5;

const _: () = assert!(
    CUBE_HEIGHT <= CUBE_GAP,
    "wall would overlap the cube to the south"
);

/// One block of solid voxels: `CUBE_SIZE x CUBE_SIZE` in plan, `z in 0..CUBE_HEIGHT`.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Cube {
    pub min_x: i32,
    pub min_y: i32,
}

impl Cube {
    pub fn contains(&self, x: i32, y: i32) -> bool {
        x >= self.min_x
            && x < self.min_x + CUBE_SIZE
            && y >= self.min_y
            && y < self.min_y + CUBE_SIZE
    }

    pub fn squares(&self) -> impl Iterator<Item = (i32, i32)> + '_ {
        (self.min_x..self.min_x + CUBE_SIZE)
            .flat_map(move |x| (self.min_y..self.min_y + CUBE_SIZE).map(move |y| (x, y)))
    }
}

#[derive(Clone, Debug)]
pub struct World {
    pub cubes: Vec<Cube>,
}

impl World {
    /// The prototype layout: four cubes at `(0,0)`, `(stride,0)`, `(0,stride)`,
    /// `(stride,stride)` where `stride = CUBE_SIZE + CUBE_GAP`.
    pub fn four_cubes() -> Self {
        let stride = CUBE_SIZE + CUBE_GAP;
        let mut cubes = Vec::new();
        for ix in 0..2 {
            for iy in 0..2 {
                cubes.push(Cube {
                    min_x: ix * stride,
                    min_y: iy * stride,
                });
            }
        }
        World { cubes }
    }

    /// Altitude of the top solid surface at a column, or `None` if the column
    /// is empty space (the cube has no floor plane).
    pub fn top_height(&self, x: i32, y: i32) -> Option<i32> {
        self.cubes
            .iter()
            .any(|cube| cube.contains(x, y))
            .then_some(CUBE_HEIGHT)
    }

    /// Every square that shows a lit top face, across all cubes.
    pub fn all_top_squares(&self) -> Vec<(i32, i32)> {
        self.cubes.iter().flat_map(Cube::squares).collect()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn four_cubes_are_equal_and_separated() {
        let world = World::four_cubes();
        assert_eq!(world.cubes.len(), 4);
        let stride = CUBE_SIZE + CUBE_GAP;
        for cube in &world.cubes {
            assert_eq!(cube.min_x % stride, 0);
            assert_eq!(cube.min_y % stride, 0);
        }
    }

    #[test]
    fn gap_columns_are_void() {
        let world = World::four_cubes();
        // The middle of the two-by-two arrangement is empty.
        let mid = CUBE_SIZE + CUBE_GAP / 2;
        assert_eq!(world.top_height(mid, mid), None);
        // A cube interior is solid at full height.
        assert_eq!(world.top_height(0, 0), Some(CUBE_HEIGHT));
        assert_eq!(
            world.top_height(CUBE_SIZE - 1, CUBE_SIZE - 1),
            Some(CUBE_HEIGHT)
        );
        // Just past an edge is void.
        assert_eq!(world.top_height(CUBE_SIZE, 0), None);
    }

    #[test]
    fn top_squares_count_is_four_footprints() {
        let world = World::four_cubes();
        assert_eq!(
            world.all_top_squares().len(),
            4 * (CUBE_SIZE * CUBE_SIZE) as usize
        );
    }

    #[test]
    fn wall_fits_in_gap() {
        assert!(CUBE_HEIGHT <= CUBE_GAP);
    }
}
