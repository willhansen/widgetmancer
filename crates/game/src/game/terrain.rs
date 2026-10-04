//! Altitude-bearing terrain: a voxel set plus a per-column top cache.
//!
//! The board itself is a materialized **slab**: every on-board square owns one
//! voxel at index [`SLAB_VOXEL_Z`] (`-1`), so its top surface sits at altitude
//! `0`. Placed geometry — blocks and taller terrain — occupies indices `>= 0`,
//! stacked on the slab. Off-board squares have no slab and read as empty space.
//!
//! `column_tops` caches the highest occupied voxel index per column for an O(1)
//! `height_at`; every write keeps it at the maximum, so filling under an
//! existing voxel never lowers a column.

use std::collections::HashMap;

use utility::coordinate_frame_conversions::{
    BoardSize, SquareSet, VoxelSet, WorldSquare, WorldVoxel,
};

/// Altitude of the board slab's top surface (the standable floor).
pub const SLAB_TOP: i32 = 0;
/// Voxel index of the one-voxel-thick board slab; its top is [`SLAB_TOP`].
pub const SLAB_VOXEL_Z: i32 = SLAB_TOP - 1;

#[derive(Clone, Debug, Default)]
pub struct Terrain {
    voxels: VoxelSet,
    /// Highest occupied voxel index per column, for O(1) support queries.
    column_tops: HashMap<WorldSquare, i32>,
}

impl Terrain {
    /// Empty terrain; call [`Terrain::seed_board_slab`] once the board size is
    /// known to give the board its floor.
    pub fn new() -> Self {
        Self::default()
    }

    /// Replace any slab layer with one voxel per on-board square. Placed voxels
    /// (`z >= 0`) are untouched, so this is safe to call after a board resize.
    pub fn seed_board_slab(&mut self, board_size: BoardSize) {
        self.voxels.retain(|voxel| voxel.z != SLAB_VOXEL_Z);
        self.column_tops
            .retain(|_, &mut top| top != SLAB_VOXEL_Z);
        for y in 0..board_size.height as i32 {
            for x in 0..board_size.width as i32 {
                let square = WorldSquare::new(x, y);
                self.voxels.insert(WorldVoxel::new(x, y, SLAB_VOXEL_Z));
                let entry = self.column_tops.entry(square).or_insert(SLAB_VOXEL_Z);
                if SLAB_VOXEL_Z > *entry {
                    *entry = SLAB_VOXEL_Z;
                }
            }
        }
    }

    pub fn place_voxel(&mut self, voxel: WorldVoxel) {
        self.voxels.insert(voxel);
        let square = WorldSquare::new(voxel.x, voxel.y);
        let entry = self.column_tops.entry(square).or_insert(voxel.z);
        if voxel.z > *entry {
            *entry = voxel.z;
        }
    }

    /// Fill a column from altitude `0` up to `top_height` (exclusive), i.e. a
    /// solid column `top_height` voxels tall sitting on the slab.
    pub fn place_solid_column(&mut self, square: WorldSquare, top_height: u32) {
        assert!(top_height > 0, "a solid column must have positive height");
        for z in 0..top_height as i32 {
            self.place_voxel(WorldVoxel::new(square.x, square.y, z));
        }
    }

    pub fn is_solid_at(&self, x: i32, y: i32, z: i32) -> bool {
        self.voxels.contains(&WorldVoxel::new(x, y, z))
    }

    /// Highest occupied voxel index in a column, or `None` if the column is
    /// empty (off-board, with no placed voxels).
    pub fn top_voxel(&self, square: WorldSquare) -> Option<i32> {
        self.column_tops.get(&square).copied()
    }

    /// Altitude of the top surface in a column (the standable height), or
    /// `None` if the column is empty space. Bare board reads `Some(0)`.
    pub fn height_at(&self, square: WorldSquare) -> Option<i32> {
        self.top_voxel(square).map(|z| z + 1)
    }

    /// Every square solid at a given altitude. Altitude `0` is the gameplay
    /// "block" layer: the slab is at `-1`, so bare board is excluded.
    pub fn solid_squares_at_altitude(&self, z: i32) -> SquareSet {
        self.voxels
            .iter()
            .filter(|voxel| voxel.z == z)
            .map(|voxel| WorldSquare::new(voxel.x, voxel.y))
            .collect()
    }

    /// Placed voxels only (`z >= 0`), i.e. everything above the regenerated
    /// slab. This is what gets serialized.
    pub fn placed_voxels(&self) -> impl Iterator<Item = WorldVoxel> + '_ {
        self.voxels
            .iter()
            .copied()
            .filter(|voxel| voxel.z >= SLAB_TOP)
    }

    pub fn is_empty(&self) -> bool {
        self.voxels.is_empty()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use euclid::point2;

    fn board() -> Terrain {
        let mut terrain = Terrain::new();
        terrain.seed_board_slab(BoardSize::new(4, 3));
        terrain
    }

    #[test]
    fn slab_gives_every_board_square_a_floor() {
        let terrain = board();
        for y in 0..3 {
            for x in 0..4 {
                let square = point2(x, y);
                assert_eq!(terrain.height_at(square), Some(SLAB_TOP));
                assert!(terrain.is_solid_at(x, y, SLAB_VOXEL_Z));
                assert!(!terrain.is_solid_at(x, y, SLAB_TOP));
            }
        }
    }

    #[test]
    fn off_board_columns_are_void() {
        let terrain = board();
        assert_eq!(terrain.height_at(point2(-1, 0)), None);
        assert_eq!(terrain.height_at(point2(4, 0)), None);
        assert_eq!(terrain.height_at(point2(0, 3)), None);
        assert!(!terrain.is_solid_at(4, 0, SLAB_VOXEL_Z));
    }

    #[test]
    fn placing_a_voxel_raises_the_top_without_lowering_it() {
        let mut terrain = board();
        let square = point2(1, 1);
        terrain.place_voxel(WorldVoxel::new(1, 1, 5));
        assert_eq!(terrain.height_at(square), Some(6));
        terrain.place_voxel(WorldVoxel::new(1, 1, 2));
        assert_eq!(terrain.height_at(square), Some(6));
        assert!(terrain.is_solid_at(1, 1, 2));
    }

    #[test]
    fn solid_column_fills_from_the_slab_upward() {
        let mut terrain = board();
        let square = point2(2, 2);
        terrain.place_solid_column(square, 3);
        assert_eq!(terrain.height_at(square), Some(3));
        for z in 0..3 {
            assert!(terrain.is_solid_at(2, 2, z));
        }
        assert!(!terrain.is_solid_at(2, 2, 3));
    }

    #[test]
    fn block_layer_excludes_the_slab_and_collects_columns() {
        let mut terrain = board();
        assert!(terrain.solid_squares_at_altitude(SLAB_TOP).is_empty());
        terrain.place_solid_column(point2(0, 0), 1);
        terrain.place_solid_column(point2(3, 2), 2);
        let blocks = terrain.solid_squares_at_altitude(SLAB_TOP);
        assert_eq!(blocks.len(), 2);
        assert!(blocks.contains(&point2(0, 0)));
        assert!(blocks.contains(&point2(3, 2)));
    }

    #[test]
    fn placed_voxels_skip_the_slab() {
        let mut terrain = board();
        terrain.place_solid_column(point2(1, 1), 2);
        let placed: Vec<_> = terrain.placed_voxels().collect();
        assert_eq!(placed.len(), 2);
        assert!(placed.iter().all(|voxel| voxel.z >= SLAB_TOP));
    }

    #[test]
    fn reseeding_replaces_only_the_slab_layer() {
        let mut terrain = board();
        terrain.place_solid_column(point2(1, 1), 3);
        terrain.seed_board_slab(BoardSize::new(2, 2));
        assert_eq!(terrain.height_at(point2(1, 1)), Some(3), "placed voxels stay");
        assert_eq!(terrain.height_at(point2(3, 2)), None, "shrunk board loses its slab");
        assert!(terrain.is_solid_at(0, 0, SLAB_VOXEL_Z), "slab re-laid");
    }
}
