//! Compares rendering *approaches* on the evaluation metrics the
//! floating_square_debug tool's animate view displays per approach row:
//! area error (rendered area vs the sampled ideal's), per-character
//! coverage error (`per_char_coverage_error`: sum over half-cells of
//! |rendered - ideal| filled area), and jaggedness (total edge step
//! length). Approaches are not fitted to these metrics; this test measures
//! them over an offset grid and asserts loose sanity bounds so a
//! regression screams, while the printed table is the real comparison.

use terminal_rendering::coverage::{
    actual_sample, assign_colors, charwise_neighborhood, fill_centroid, jaggedness,
    per_char_coverage_error, rendered_neighborhood, rendered_neighborhood_forced, FillGrid, SX,
    SY,
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
) -> (f32, f32, f32, f32) {
    let (mut max_area, mut max_center, mut max_pc, mut max_jag) = (0.0f32, 0.0f32, 0.0f32, 0.0f32);
    let (mut sum_area, mut sum_center, mut sum_pc, mut sum_jag) = (0.0f32, 0.0f32, 0.0f32, 0.0f32);
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
            max_area = max_area.max(s.area_err.abs());
            max_center = max_center.max(s.center_err);
            max_pc = max_pc.max(s.per_char_err);
            max_jag = max_jag.max(s.jaggedness);
            sum_area += s.area_err.abs();
            sum_center += s.center_err;
            sum_pc += s.per_char_err;
            sum_jag += s.jaggedness;
        }
    }
    let n = 256.0;
    println!(
        "{name:<20} mean(|area| {:.3} center {:.3} per-char {:.3} jagged {:.3})  \
         max(|area| {:.3} center {:.3} per-char {:.3} jagged {:.3})",
        sum_area / n,
        sum_center / n,
        sum_pc / n,
        sum_jag / n,
        max_area,
        max_center,
        max_pc,
        max_jag,
    );
    (max_area, max_center, max_pc, max_jag)
}

#[test]
fn test_approach_comparison_metrics() {
    let auto = measure("family-snapped (auto)", rendered_neighborhood);
    let charwise = measure("charwise", charwise_neighborhood);
    for f in 0..4 {
        measure(&format!("forced family {f}"), move |pos| {
            rendered_neighborhood_forced(pos, f)
        });
    }

    // sanity bounds, deliberately loose: the printed table above is the
    // comparison; these only catch blowups
    let (auto_area, auto_center, _, auto_jag) = auto;
    let (fit_area, fit_center, _, fit_jag) = charwise;
    assert!(auto_area <= 0.35, "family-snapped area err {auto_area}");
    assert!(auto_center <= 0.25, "family-snapped center err {auto_center}");
    // the coherence property: the auto pick keeps edges straight
    assert!(auto_jag <= 0.3, "family-snapped jaggedness {auto_jag}");
    assert!(fit_area <= 0.35, "charwise area err {fit_area}");
    assert!(fit_center <= 0.25, "charwise center err {fit_center}");
    // jagged by design, but not chaotic
    assert!(fit_jag <= 8.0, "charwise jaggedness {fit_jag}");
}
