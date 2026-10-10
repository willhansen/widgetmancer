//! The world as a voxel grid: a set of solid voxels plus its horizontal extent.
//!
//! `VoxelGrid` is the single source of truth for geometry. It owns the solid
//! `voxels` (a voxel at index `z` occupies `[z, z+1)`, so its top surface is at
//! altitude `z + 1`) and the `extent` that answers "is this square on the map?".
//! There is no separate board: a flat map is a grid whose floor layer is filled.
//!
//! A **floor layer** is a convenience a map opts into: one voxel at index
//! [`FLOOR_LAYER_Z`] (`-1`) per square of [`VoxelGrid::fill_floor_rect`], so its
//! top surface sits at altitude `FLOOR_TOP` (`0`). Placed geometry — walls and
//! taller stacks — occupies indices `>= 0`. Squares with no voxel read as void.
//!
//! `top_voxels` caches the highest occupied voxel index per square for an O(1)
//! surface query; every write keeps it at the maximum, so filling under an
//! existing voxel never lowers a square.

use std::collections::HashMap;

use rgb::RGB8;
use utility::coordinate_frame_conversions::{
    GridExtent, SquareSet, VoxelSet, WorldSquare, WorldVoxel,
};

/// Altitude of the floor layer's top surface (the standable floor).
pub const FLOOR_TOP: i32 = 0;
/// Voxel index of the one-voxel-thick floor layer; its top is [`FLOOR_TOP`].
pub const FLOOR_LAYER_Z: i32 = FLOOR_TOP - 1;

/// Default tint for placed voxels that are not given an explicit material. A
/// neutral cool slate; the renderer derives the checker's light/dark shades and
/// the wall gradient from it.
pub const DEFAULT_VOXEL_TINT: RGB8 = RGB8::new(96, 104, 128);

/// How a square's exposed faces are colored. `Floor` resolves to the grid's
/// existing floor pattern at render time; `Tint` is a base color the renderer
/// shades into a checker of tops and a wall gradient.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum VoxelMaterial {
    Floor,
    Tint(RGB8),
}

#[derive(Clone, Debug, Default)]
pub struct VoxelGrid {
    voxels: VoxelSet,
    /// Horizontal bounds; what "on the map" means without a separate board.
    extent: GridExtent,
    /// Highest occupied voxel index per square, for O(1) surface queries.
    top_voxels: HashMap<WorldSquare, i32>,
    /// Explicit material overrides. Squares without an entry derive their
    /// material from whether they are built up (`material_at`).
    materials: HashMap<WorldSquare, VoxelMaterial>,
}

impl VoxelGrid {
    /// Empty grid with zero extent; call [`VoxelGrid::seed_floor`] once the
    /// extent is known to give the map its floor.
    pub fn new() -> Self {
        Self::default()
    }

    pub fn extent(&self) -> GridExtent {
        self.extent
    }

    /// Set the horizontal bounds without touching voxels. Floors are laid
    /// separately with [`VoxelGrid::seed_floor`] or [`VoxelGrid::fill_floor_rect`].
    pub fn set_extent(&mut self, extent: GridExtent) {
        self.extent = extent;
    }

    /// Adopt `extent` and lay one floor voxel (`z = -1`) per square of it.
    /// Placed voxels (`z >= 0`) are untouched, so this is safe after a resize.
    /// The convenience "make this a flat map" floor; a map with real gaps can
    /// instead [`VoxelGrid::clear_floor`] and draw its own voxels.
    pub fn seed_floor(&mut self, extent: GridExtent) {
        self.extent = extent;
        self.clear_floor();
        self.fill_floor_rect(extent.width as i32, extent.height as i32);
    }

    /// Place a floor voxel (`z = -1`) under every square of a `width x height`
    /// rect whose lower-left corner is the origin.
    pub fn fill_floor_rect(&mut self, width: i32, height: i32) {
        for y in 0..height {
            for x in 0..width {
                self.place_voxel(WorldVoxel::new(x, y, FLOOR_LAYER_Z));
            }
        }
    }

    /// Remove every floor voxel, leaving placed geometry. Squares left without
    /// any voxel become real void.
    pub fn clear_floor(&mut self) {
        self.voxels.retain(|voxel| voxel.z != FLOOR_LAYER_Z);
        self.top_voxels
            .retain(|_, &mut top| top != FLOOR_LAYER_Z);
    }

    /// Squares whose surface is the bare floor (no placed voxel above it).
    /// These are what render with the grid's floor pattern.
    pub fn floor_squares(&self) -> SquareSet {
        self.top_voxels
            .iter()
            .filter(|(_, &top)| top == FLOOR_LAYER_Z)
            .map(|(&square, _)| square)
            .collect()
    }

    /// Every square that contains any voxel. Anything else is void.
    pub fn occupied_squares(&self) -> SquareSet {
        self.top_voxels.keys().copied().collect()
    }

    /// Every floor voxel (`z = -1`); what a snapshot records to reproduce a
    /// floor that isn't the default full rect.
    pub fn floor_voxels(&self) -> Vec<WorldVoxel> {
        self.voxels
            .iter()
            .copied()
            .filter(|voxel| voxel.z == FLOOR_LAYER_Z)
            .collect()
    }

    pub fn place_voxel(&mut self, voxel: WorldVoxel) {
        self.voxels.insert(voxel);
        let square = WorldSquare::new(voxel.x, voxel.y);
        let entry = self.top_voxels.entry(square).or_insert(voxel.z);
        if voxel.z > *entry {
            *entry = voxel.z;
        }
    }

    /// Fill a square from altitude `0` up to `top_height` (exclusive), i.e. a
    /// solid stack `top_height` voxels tall sitting on the floor.
    pub fn fill_column(&mut self, square: WorldSquare, top_height: u32) {
        assert!(top_height > 0, "a solid column must have positive height");
        for z in 0..top_height as i32 {
            self.place_voxel(WorldVoxel::new(square.x, square.y, z));
        }
    }

    /// Like [`VoxelGrid::fill_column`], but with an explicit material tint.
    pub fn fill_column_with_material(
        &mut self,
        square: WorldSquare,
        top_height: u32,
        material: VoxelMaterial,
    ) {
        self.fill_column(square, top_height);
        self.set_material(square, material);
    }

    pub fn is_solid_at(&self, x: i32, y: i32, z: i32) -> bool {
        self.voxels.contains(&WorldVoxel::new(x, y, z))
    }

    /// Highest occupied voxel index in a square, or `None` if the square is
    /// empty (off-grid, with no placed voxels).
    pub fn top_voxel(&self, square: WorldSquare) -> Option<i32> {
        self.top_voxels.get(&square).copied()
    }

    /// Altitude of the top surface in a square (the standable height), or
    /// `None` if the square is empty space. Bare floor reads `Some(0)`.
    pub fn surface_at(&self, square: WorldSquare) -> Option<i32> {
        self.top_voxel(square).map(|z| z + 1)
    }

    /// Every square solid at a given altitude. Altitude `0` is the gameplay
    /// "block" layer: the floor is at `-1`, so bare floor is excluded.
    pub fn solid_squares_at_altitude(&self, z: i32) -> SquareSet {
        self.voxels
            .iter()
            .filter(|voxel| voxel.z == z)
            .map(|voxel| WorldSquare::new(voxel.x, voxel.y))
            .collect()
    }

    /// Placed voxels only (`z >= 0`), i.e. everything above the regenerated
    /// floor. This is what gets serialized.
    pub fn placed_voxels(&self) -> impl Iterator<Item = WorldVoxel> + '_ {
        self.voxels
            .iter()
            .copied()
            .filter(|voxel| voxel.z >= FLOOR_TOP)
    }

    /// Every square that is exactly one voxel tall. These are the squares that
    /// render as flat gameplay "blocks"; taller stacks render as material.
    pub fn single_layer_squares(&self) -> SquareSet {
        self.top_voxels
            .iter()
            .filter(|(_, &top)| top == FLOOR_TOP)
            .map(|(&square, _)| square)
            .collect()
    }

    /// Every occupied square with its highest voxel index. On-grid squares
    /// always appear (their floor voxel), so this is also the set the render
    /// pass walks.
    pub fn surface_heights(&self) -> Vec<(WorldSquare, i32)> {
        self.top_voxels
            .iter()
            .map(|(&square, &top)| (square, top))
            .collect()
    }

    /// Highest top-surface altitude anywhere, or `0` for an empty/floor-only
    /// world. Drives the render gate: a grid no taller than one voxel renders
    /// through the legacy flat path.
    pub fn max_top_altitude(&self) -> i32 {
        self.top_voxels
            .values()
            .map(|&top| top + 1)
            .max()
            .unwrap_or(FLOOR_TOP)
    }

    /// The square's material: an explicit override if set, otherwise `Floor` for
    /// the floor layer and the default voxel tint for any built-up square.
    pub fn material_at(&self, square: WorldSquare) -> VoxelMaterial {
        if let Some(&material) = self.materials.get(&square) {
            return material;
        }
        match self.top_voxel(square) {
            Some(top) if top >= FLOOR_TOP => VoxelMaterial::Tint(DEFAULT_VOXEL_TINT),
            _ => VoxelMaterial::Floor,
        }
    }

    pub fn set_material(&mut self, square: WorldSquare, material: VoxelMaterial) {
        self.materials.insert(square, material);
    }

    /// Explicit material overrides only (defaults are derived on read). What
    /// gets serialized.
    pub fn materials(&self) -> impl Iterator<Item = (WorldSquare, VoxelMaterial)> + '_ {
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

    fn grid() -> VoxelGrid {
        let mut grid = VoxelGrid::new();
        grid.seed_floor(GridExtent::new(4, 3));
        grid
    }

    #[test]
    fn floor_layer_gives_every_grid_square_a_floor() {
        let grid = grid();
        for y in 0..3 {
            for x in 0..4 {
                let square = point2(x, y);
                assert_eq!(grid.surface_at(square), Some(FLOOR_TOP));
                assert!(grid.is_solid_at(x, y, FLOOR_LAYER_Z));
                assert!(!grid.is_solid_at(x, y, FLOOR_TOP));
            }
        }
    }

    #[test]
    fn off_grid_squares_are_void() {
        let grid = grid();
        assert_eq!(grid.surface_at(point2(-1, 0)), None);
        assert_eq!(grid.surface_at(point2(4, 0)), None);
        assert_eq!(grid.surface_at(point2(0, 3)), None);
        assert!(!grid.is_solid_at(4, 0, FLOOR_LAYER_Z));
    }

    #[test]
    fn placing_a_voxel_raises_the_surface_without_lowering_it() {
        let mut grid = grid();
        let square = point2(1, 1);
        grid.place_voxel(WorldVoxel::new(1, 1, 5));
        assert_eq!(grid.surface_at(square), Some(6));
        grid.place_voxel(WorldVoxel::new(1, 1, 2));
        assert_eq!(grid.surface_at(square), Some(6));
        assert!(grid.is_solid_at(1, 1, 2));
    }

    #[test]
    fn fill_column_stacks_from_the_floor_upward() {
        let mut grid = grid();
        let square = point2(2, 2);
        grid.fill_column(square, 3);
        assert_eq!(grid.surface_at(square), Some(3));
        for z in 0..3 {
            assert!(grid.is_solid_at(2, 2, z));
        }
        assert!(!grid.is_solid_at(2, 2, 3));
    }

    #[test]
    fn block_layer_excludes_the_floor_and_collects_squares() {
        let mut grid = grid();
        assert!(grid.solid_squares_at_altitude(FLOOR_TOP).is_empty());
        grid.fill_column(point2(0, 0), 1);
        grid.fill_column(point2(3, 2), 2);
        let blocks = grid.solid_squares_at_altitude(FLOOR_TOP);
        assert_eq!(blocks.len(), 2);
        assert!(blocks.contains(&point2(0, 0)));
        assert!(blocks.contains(&point2(3, 2)));
    }

    #[test]
    fn placed_voxels_skip_the_floor() {
        let mut grid = grid();
        grid.fill_column(point2(1, 1), 2);
        let placed: Vec<_> = grid.placed_voxels().collect();
        assert_eq!(placed.len(), 2);
        assert!(placed.iter().all(|voxel| voxel.z >= FLOOR_TOP));
    }

    #[test]
    fn max_top_altitude_tracks_the_tallest_stack() {
        let mut grid = grid();
        assert_eq!(grid.max_top_altitude(), FLOOR_TOP, "bare floor is altitude 0");
        grid.fill_column(point2(1, 1), 4);
        assert_eq!(grid.max_top_altitude(), 4);
    }

    #[test]
    fn surface_heights_include_bare_floor_and_placed_stacks() {
        let mut grid = grid();
        grid.fill_column(point2(1, 1), 3);
        let surface_heights = grid.surface_heights();
        assert_eq!(surface_heights.len(), 4 * 3, "one per on-grid square");
        assert!(surface_heights.contains(&(point2(1, 1), 2)));
        assert!(surface_heights.contains(&(point2(0, 0), FLOOR_LAYER_Z)));
    }

    #[test]
    fn reseeding_replaces_only_the_floor_layer() {
        let mut grid = grid();
        grid.fill_column(point2(1, 1), 3);
        grid.seed_floor(GridExtent::new(2, 2));
        assert_eq!(grid.surface_at(point2(1, 1)), Some(3), "placed voxels stay");
        assert_eq!(grid.surface_at(point2(3, 2)), None, "shrunk grid loses its floor");
        assert!(grid.is_solid_at(0, 0, FLOOR_LAYER_Z), "floor re-laid");
    }

    #[test]
    fn material_defaults_follow_whether_a_square_is_built_up() {
        let mut grid = grid();
        assert_eq!(grid.material_at(point2(0, 0)), VoxelMaterial::Floor);
        grid.fill_column(point2(1, 1), 3);
        assert_eq!(
            grid.material_at(point2(1, 1)),
            VoxelMaterial::Tint(DEFAULT_VOXEL_TINT)
        );
    }

    #[test]
    fn explicit_material_overrides_and_serializes() {
        let mut grid = grid();
        let tint = RGB8::new(200, 80, 40);
        grid.fill_column_with_material(point2(1, 1), 2, VoxelMaterial::Tint(tint));
        assert_eq!(grid.material_at(point2(1, 1)), VoxelMaterial::Tint(tint));
        let overrides: Vec<_> = grid.materials().collect();
        assert_eq!(overrides, vec![(point2(1, 1), VoxelMaterial::Tint(tint))]);
    }

    #[test]
    fn clear_floor_makes_real_void_and_floor_squares_shrink() {
        let mut grid = grid();
        assert_eq!(grid.floor_squares().len(), 4 * 3);
        assert_eq!(grid.occupied_squares().len(), 4 * 3);
        grid.clear_floor();
        assert!(grid.floor_squares().is_empty());
        assert!(grid.occupied_squares().is_empty());
        assert_eq!(grid.surface_at(point2(0, 0)), None, "now void");
    }

    #[test]
    fn fill_floor_rect_restores_a_partial_floor() {
        let mut grid = VoxelGrid::new();
        grid.fill_floor_rect(3, 2);
        assert_eq!(grid.floor_squares().len(), 6);
        assert_eq!(grid.surface_at(point2(2, 1)), Some(FLOOR_TOP));
        assert_eq!(grid.surface_at(point2(3, 0)), None, "outside the rect");
    }
}
