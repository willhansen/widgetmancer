//! Compares rendering *approaches* on the evaluation metrics the
//! floating_square_debug tool's animate view displays per approach row:
//! area error (rendered area vs the sampled ideal's), per-character
//! coverage error (`per_char_coverage_error`: sum over half-cells of
//! |rendered - ideal| filled area), and jaggedness (total edge step
//! length). Approaches are not fitted to these metrics; this test measures
//! them over an offset grid and asserts loose sanity bounds so a
//! regression screams, while the printed table is the real comparison.

use terminal_rendering::coverage::{
    actual_sample, assign_colors, charwise_neighborhood, charwise_protrusion_squared_neighborhood,
    charwise_shaped_neighborhood, displacement_sensitivity, fill_centroid, jaggedness,
    per_char_coverage_error, rendered_neighborhood, rendered_neighborhood_forced, FillGrid,
    DISPLACEMENT_DELTA, SX, SY,
};
use terminal_rendering::DoubleChar;
use utility::coordinate_frame_conversions::{WorldPoint, WorldSquare};

fn sampled(grid: &[[DoubleChar; 3]; 3], center: WorldSquare) -> FillGrid {
    let owners = assign_colors(grid);
    let origin = euclid::point2(center.x as f32 - 1.5, center.y as f32 - 1.5);
    FillGrid::sample(origin, |wx, wy| actual_sample(grid, &owners, center, wx, wy))
}

fn sampled_ideal(center: WorldSquare, pos: WorldPoint) -> FillGrid {
    let origin = euclid::point2(center.x as f32 - 1.5, center.y as f32 - 1.5);
    FillGrid::sample(origin, |wx, wy| {
        (
            (wx - pos.x).abs() <= 0.5 && (wy - pos.y).abs() <= 0.5,
            Some(0),
        )
    })
}

fn area_of(actual: &FillGrid) -> f32 {
    actual.cells.iter().flatten().filter(|&&b| b).count() as f32 / (SX * SY) as f32
}

struct Stats {
    area_err: f32,
    center_err: f32,
    per_char_err: f32,
    jaggedness: f32,
}

fn measure(
    name: &str,
    neighborhood: impl Fn(WorldPoint) -> ([[DoubleChar; 3]; 3], WorldSquare),
) -> (f32, f32, f32, f32, f32, f32) {
    let (mut max_area, mut max_center, mut max_pc, mut max_jag, mut max_disp) =
        (0.0f32, 0.0f32, 0.0f32, 0.0f32, 0.0f32);
    let (mut sum_area, mut sum_center, mut sum_pc, mut sum_jag, mut sum_disp) =
        (0.0f32, 0.0f32, 0.0f32, 0.0f32, 0.0f32);
    for xi in 0..16 {
        for yi in 0..16 {
            let pos = euclid::point2(xi as f32 / 16.0, yi as f32 / 16.0);
            let (grid, center) = neighborhood(pos);
            let actual = sampled(&grid, center);
            let ideal = sampled_ideal(center, pos);
            let s = Stats {
                area_err: area_of(&actual) - area_of(&ideal),
                center_err: match (fill_centroid(&actual), fill_centroid(&ideal)) {
                    (Some(a), Some(b)) => (a.x - b.x).hypot(a.y - b.y),
                    _ => f32::INFINITY,
                },
                per_char_err: per_char_coverage_error(&grid, center, pos),
                jaggedness: jaggedness(&actual),
            };
            let (disp, _) = displacement_sensitivity(&neighborhood, pos, DISPLACEMENT_DELTA);
            max_area = max_area.max(s.area_err.abs());
            max_center = max_center.max(s.center_err);
            max_pc = max_pc.max(s.per_char_err);
            max_jag = max_jag.max(s.jaggedness);
            max_disp = max_disp.max(disp);
            sum_area += s.area_err.abs();
            sum_center += s.center_err;
            sum_pc += s.per_char_err;
            sum_jag += s.jaggedness;
            sum_disp += disp;
        }
    }
    let n = 256.0;
    println!(
        "{name:<20} mean(|area| {:.3} center {:.3} per-char {:.3} jagged {:.3} disp {:.3})  \
         max(|area| {:.3} center {:.3} per-char {:.3} jagged {:.3} disp {:.3})",
        sum_area / n,
        sum_center / n,
        sum_pc / n,
        sum_jag / n,
        sum_disp / n,
        max_area,
        max_center,
        max_pc,
        max_jag,
        max_disp,
    );
    (max_area, max_center, max_pc, max_jag, sum_disp / n, max_disp)
}

#[test]
fn test_approach_comparison_metrics() {
    let auto = measure("family-snapped (auto)", rendered_neighborhood);
    let charwise = measure("charwise", charwise_neighborhood);
    let shaped = measure("charwise + protrusion", charwise_shaped_neighborhood);
    let squared = measure(
        "charwise + protrusion²",
        charwise_protrusion_squared_neighborhood,
    );
    for f in 0..4 {
        measure(&format!("forced family {f}"), move |pos| {
            rendered_neighborhood_forced(pos, f)
        });
    }

    // sanity bounds, deliberately loose: the printed table above is the
    // comparison; these only catch blowups
    let (auto_area, auto_center, _, auto_jag, _, _) = auto;
    let (fit_area, fit_center, _, fit_jag, _, _) = charwise;
    assert!(auto_area <= 0.35, "family-snapped area err {auto_area}");
    assert!(auto_center <= 0.25, "family-snapped center err {auto_center}");
    // the coherence property: the auto pick keeps edges straight
    assert!(auto_jag <= 0.3, "family-snapped jaggedness {auto_jag}");
    assert!(fit_area <= 0.35, "charwise area err {fit_area}");
    assert!(fit_center <= 0.25, "charwise center err {fit_center}");
    // jagged by design, but not chaotic
    assert!(fit_jag <= 8.0, "charwise jaggedness {fit_jag}");
    let (shaped_area, shaped_center, _, shaped_jag, _, _) = shaped;
    // same loose bounds as plain charwise, except area: the protrusion
    // penalty deliberately refuses thin glyphs that spike far past the
    // true edge (a full-height sliver protrudes by the whole uncovered
    // height), leaving a notch instead — so its worst-case |area| error
    // is structurally higher. It must stay a trade, not a blowup.
    assert!(shaped_area <= 0.45, "shaped area err {shaped_area}");
    assert!(shaped_center <= 0.25, "shaped center err {shaped_center}");
    assert!(shaped_jag <= 8.0, "shaped jaggedness {shaped_jag}");
    let (sq_area, sq_center, _, sq_jag, _, _) = squared;
    // the quadratic tolerates shallow overshoot (d < 1/W2) so its worst
    // |area| sits between plain charwise and the linear penalty
    assert!(sq_area <= 0.45, "squared area err {sq_area}");
    assert!(sq_center <= 0.25, "squared center err {sq_center}");
    assert!(sq_jag <= 8.0, "squared jaggedness {sq_jag}");
    // displacement sensitivity: a tiny nudge must never add a huge xor
    for (name, (_, _, _, _, mean_disp, max_disp)) in [
        ("family-snapped", auto),
        ("charwise", charwise),
        ("charwise + protrusion", shaped),
        ("charwise + protrusion²", squared),
    ] {
        assert!(max_disp <= 0.5, "{name} displacement gain {max_disp}");
        assert!(mean_disp >= 0.0, "{name} negative mean displacement");
    }
}
