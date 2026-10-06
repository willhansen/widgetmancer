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

use rgb::RGB8;
use utility::coordinate_frame_conversions::{
    BoardSize, SquareSet, VoxelSet, WorldSquare, WorldVoxel,
};

/// Altitude of the board slab's top surface (the standable floor).
pub const SLAB_TOP: i32 = 0;
/// Voxel index of the one-voxel-thick board slab; its top is [`SLAB_TOP`].
pub const SLAB_VOXEL_Z: i32 = SLAB_TOP - 1;

/// Default tint for placed terrain that is not given an explicit material. A
/// neutral cool slate; the renderer derives the checker's light/dark shades and
/// the wall gradient from it.
pub const DEFAULT_TERRAIN_TINT: RGB8 = RGB8::new(96, 104, 128);

/// How a column's exposed faces are colored. `Floor` resolves to the board's
/// existing floor pattern at render time; `Tint` is a base color the renderer
/// shades into a checker of tops and a wall gradient.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum TerrainMaterial {
    Floor,
    Tint(RGB8),
}

#[derive(Clone, Debug, Default)]
pub struct Terrain {
    voxels: VoxelSet,
    /// Highest occupied voxel index per column, for O(1) support queries.
    column_tops: HashMap<WorldSquare, i32>,
    /// Explicit material overrides. Columns without an entry derive their
    /// material from whether they are built up (`material_at`).
    materials: HashMap<WorldSquare, TerrainMaterial>,
}

impl Terrain {
    /// Empty terrain; call [`Terrain::seed_board_slab`] once the board size is
    /// known to give the board its floor.
    pub fn new() -> Self {
        Self::default()
    }

    /// Replace any slab layer with one voxel per on-board square. Placed voxels
    /// (`z >= 0`) are untouched, so this is safe to call after a board resize.
    /// The convenience "make this a flat board" floor; a map with real gaps can
    /// instead [`Terrain::clear_floor`] and draw its own voxels.
    pub fn seed_board_slab(&mut self, board_size: BoardSize) {
        self.clear_floor();
        self.fill_floor_rect(board_size.width as i32, board_size.height as i32);
    }

    /// Place a floor voxel (`z = -1`) under every square of a `width x height`
    /// rect whose lower-left corner is the origin.
    pub fn fill_floor_rect(&mut self, width: i32, height: i32) {
        for y in 0..height {
            for x in 0..width {
                self.place_voxel(WorldVoxel::new(x, y, SLAB_VOXEL_Z));
            }
        }
    }

    /// Remove every floor voxel, leaving placed geometry. Columns left without
    /// any voxel become real void.
    pub fn clear_floor(&mut self) {
        self.voxels.retain(|voxel| voxel.z != SLAB_VOXEL_Z);
        self.column_tops
            .retain(|_, &mut top| top != SLAB_VOXEL_Z);
    }

    /// Columns whose top surface is the bare floor (no placed voxel above it).
    /// These are what render with the board's floor pattern.
    pub fn floor_squares(&self) -> SquareSet {
        self.column_tops
            .iter()
            .filter(|(_, &top)| top == SLAB_VOXEL_Z)
            .map(|(&square, _)| square)
            .collect()
    }

    /// Every column that contains any voxel. Anything else is void.
    pub fn occupied_squares(&self) -> SquareSet {
        self.column_tops.keys().copied().collect()
    }

    /// Every floor voxel (`z = -1`); what a snapshot records to reproduce a
    /// board that isn't the default full rect.
    pub fn slab_voxels(&self) -> Vec<WorldVoxel> {
        self.voxels
            .iter()
            .copied()
            .filter(|voxel| voxel.z == SLAB_VOXEL_Z)
            .collect()
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

    /// Like [`Terrain::place_solid_column`], but with an explicit material tint.
    pub fn place_solid_column_with_material(
        &mut self,
        square: WorldSquare,
        top_height: u32,
        material: TerrainMaterial,
    ) {
        self.place_solid_column(square, top_height);
        self.set_material(square, material);
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

    /// Every column that is exactly one voxel tall. These are the squares that
    /// render as flat gameplay "blocks"; taller columns render as material.
    pub fn single_height_block_squares(&self) -> SquareSet {
        self.column_tops
            .iter()
            .filter(|(_, &top)| top == SLAB_TOP)
            .map(|(&square, _)| square)
            .collect()
    }

    /// Every column that owns at least one voxel, with its highest voxel index.
    /// On-board squares always appear (their slab voxel), so this is also the
    /// set the render pass walks.
    pub fn columns(&self) -> Vec<(WorldSquare, i32)> {
        self.column_tops
            .iter()
            .map(|(&square, &top)| (square, top))
            .collect()
    }

    /// Highest top-surface altitude anywhere, or `0` for an empty/slab-only
    /// world. Drives the render gate: a board no taller than one voxel renders
    /// through the legacy flat path.
    pub fn max_top_altitude(&self) -> i32 {
        self.column_tops
            .values()
            .map(|&top| top + 1)
            .max()
            .unwrap_or(SLAB_TOP)
    }

    /// The column's material: an explicit override if set, otherwise `Floor` for
    /// the slab and the default terrain tint for any built-up column.
    pub fn material_at(&self, square: WorldSquare) -> TerrainMaterial {
        if let Some(&material) = self.materials.get(&square) {
            return material;
        }
        match self.top_voxel(square) {
            Some(top) if top >= SLAB_TOP => TerrainMaterial::Tint(DEFAULT_TERRAIN_TINT),
            _ => TerrainMaterial::Floor,
        }
    }

    pub fn set_material(&mut self, square: WorldSquare, material: TerrainMaterial) {
        self.materials.insert(square, material);
    }

    /// Explicit material overrides only (defaults are derived on read). What
    /// gets serialized.
    pub fn materials(&self) -> impl Iterator<Item = (WorldSquare, TerrainMaterial)> + '_ {
        self.materials.iter().map(|(&square, &material)| (square, material))
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
    fn max_top_altitude_tracks_the_tallest_column() {
        let mut terrain = board();
        assert_eq!(terrain.max_top_altitude(), SLAB_TOP, "bare slab is altitude 0");
        terrain.place_solid_column(point2(1, 1), 4);
        assert_eq!(terrain.max_top_altitude(), 4);
    }

    #[test]
    fn columns_include_bare_board_and_placed_columns() {
        let mut terrain = board();
        terrain.place_solid_column(point2(1, 1), 3);
        let columns = terrain.columns();
        assert_eq!(columns.len(), 4 * 3, "one per on-board square");
        assert!(columns.contains(&(point2(1, 1), 2)));
        assert!(columns.contains(&(point2(0, 0), SLAB_VOXEL_Z)));
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

    #[test]
    fn material_defaults_follow_whether_a_column_is_built_up() {
        let mut terrain = board();
        assert_eq!(terrain.material_at(point2(0, 0)), TerrainMaterial::Floor);
        terrain.place_solid_column(point2(1, 1), 3);
        assert_eq!(
            terrain.material_at(point2(1, 1)),
            TerrainMaterial::Tint(DEFAULT_TERRAIN_TINT)
        );
    }

    #[test]
    fn explicit_material_overrides_and_serializes() {
        let mut terrain = board();
        let tint = RGB8::new(200, 80, 40);
        terrain.place_solid_column_with_material(point2(1, 1), 2, TerrainMaterial::Tint(tint));
        assert_eq!(terrain.material_at(point2(1, 1)), TerrainMaterial::Tint(tint));
        let overrides: Vec<_> = terrain.materials().collect();
        assert_eq!(overrides, vec![(point2(1, 1), TerrainMaterial::Tint(tint))]);
    }

    #[test]
    fn clear_floor_makes_real_void_and_floor_squares_shrink() {
        let mut terrain = board();
        assert_eq!(terrain.floor_squares().len(), 4 * 3);
        assert_eq!(terrain.occupied_squares().len(), 4 * 3);
        terrain.clear_floor();
        assert!(terrain.floor_squares().is_empty());
        assert!(terrain.occupied_squares().is_empty());
        assert_eq!(terrain.height_at(point2(0, 0)), None, "now void");
    }

    #[test]
    fn fill_floor_rect_restores_a_partial_floor() {
        let mut terrain = Terrain::new();
        terrain.fill_floor_rect(3, 2);
        assert_eq!(terrain.floor_squares().len(), 6);
        assert_eq!(terrain.height_at(point2(2, 1)), Some(SLAB_TOP));
        assert_eq!(terrain.height_at(point2(3, 0)), None, "outside the rect");
    }
}
