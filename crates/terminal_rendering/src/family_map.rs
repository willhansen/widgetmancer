//! Baked snap-family selection: which glyph family best renders a floating
//! square at each offset, scored by *measured coverage error* (symmetric
//! difference against the true square on the sampling lattice), not the
//! center-offset proxy. Baked offline over the fundamental domain because
//! the coverage oracle is too slow to run per frame; see the baker test
//! (`family_map` in tests/floating_square_coherence.rs).
//!
//! Sign symmetry: every snap grid is sign-symmetric off exact ties, so the
//! map covers only [0, 0.5)^2 and lookups fold by absolute value. Ties
//! differ from the live proxy by family-priority order, which is fine:
//! what matters is that every cell of a square folds identically.

use crate::family_map_table::FAMILY_BY_OFFSET;
use utility::FVector;

/// Cells per axis over [0, 0.5); one cell = 1/48 world units (half the
/// coverage sample step, so cell centers never alias glyph boundaries).
pub const FAMILY_MAP_RES: usize = 24;
pub const FAMILY_MAP_CELLS_PER_UNIT: f32 = FAMILY_MAP_RES as f32 * 2.0;

/// Index into `SnapFamily::ALL` / `snap_family_names()` for the family the
/// baked map picked for offset `o`.
///
/// The fold takes the *fractional* offset by half-up rounding, which is
/// exactly translation-invariant, so every cell of a square (offsets
/// differing by integers) lands on the same map cell — cross-cell family
/// agreement, the invariant the whole design rests on. Plain `abs()`
/// would send neighbor cells to different bins. Offsets are already in
/// [-0.5, 0.5) from `world_point_to_world_square`'s half-up rounding, but
/// callers may pass anything.
fn fold_component(x: f32) -> usize {
    let frac = (x - (x + 0.5).floor()).abs();
    ((frac * FAMILY_MAP_CELLS_PER_UNIT) as usize).min(FAMILY_MAP_RES - 1)
}

/// Index into `SnapFamily::ALL` / `snap_family_names()` for the family the
/// baked map picked for offset `o`.
pub fn family_index_for_offset(o: FVector) -> usize {
    FAMILY_BY_OFFSET[fold_component(o.x)][fold_component(o.y)] as usize
}
