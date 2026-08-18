//! Visual coherence tests for the floating square renderer.
//!
//! The game draws a floating unit square by picking a block glyph per
//! terminal half-cell (`characters_for_full_square_with_2d_offset`, reached
//! via `OffsetSquareDrawable`). Each half-cell is snapped independently, so
//! the cells of one square can disagree about where the square's edges are,
//! tearing the silhouette (seen at e.g. pos=(2.363, -0.816)).
//!
//! These tests render the square at evenly spaced positions along a line
//! (the reported position is the middle sample) and assert edge coherence:
//! a square's top/bottom/left/right edges must be straight (same position
//! in every column/row) and its fill must be hole-free. Any single glyph
//! family (eighths, hextants, quadrants) applied consistently satisfies
//! this, so a failure means the renderer mixed families within one square.
//!
//! Report layout: first a horizontal strip of the small (character-grid)
//! views of the square at every position, monochrome (uniform grey on the
//! checkerboard); below it the same strip with each half-cell glyph in its
//! own color; below that a coherent reference rendering (the true square
//! glyphized via hextants — one valid coherent family, not the only one)
//! at the same zoom, colored per piece. Columns (positions) with errors
//! are marked with `^^^`, and each failed position gets a zoomed-in row
//! (sampled bitmaps of actual vs ideal coverage) at the bottom. A
//! dark-grey background checkerboard marks the character cells.
//! Set NO_COLOR=1 for plain output.
//!
//! Sampling note: metric sample points sit at half-sample offsets
//! ((j+0.5)/16, (i+0.5)/24), which can never coincide with the
//! half/third/eighth glyph boundaries, so coverage metrics never alias.
//!
//! Run with output visible:
//!   cargo test -p terminal_rendering --test floating_square_coherence -- --nocapture

use euclid::vec2;
use terminal_rendering::coverage::{
    actual_sample, assign_colors, cell_bg, glyph_filled, rendered_neighborhood, FillGrid, Rgb,
    Style, BITMAP_W, DOT_COLOR, IDEAL_COLOR, NX, NY, PALETTE, PX_W, SX, SY,
};
use terminal_rendering::glyph_constants::*;
use terminal_rendering::hextant_blocks::hextant_array_to_char;
use terminal_rendering::*;
use utility::coordinate_frame_conversions::{WorldPoint, WorldSquare};

const GLYPH_W: usize = 6; // small views: 3 world squares = 6 half-cell chars

const MARKER_COLOR: Rgb = Rgb(255, 220, 80);

#[derive(Default)]
struct Metrics {
    top_spread: f32,
    bottom_spread: f32,
    left_spread: f32,
    right_spread: f32,
    holes: usize,
    area: f32,
    /// per display-column flag: top or bottom edge deviates >1/8 from ideal
    ragged_columns: Vec<bool>,
}

impl Metrics {
    fn measure(actual: &FillGrid, pos: WorldPoint) -> Self {
        let mut m = Metrics::default();
        let mut tops = Vec::new();
        let mut bottoms = Vec::new();
        let mut column_info = Vec::new(); // (top, bottom) per sample column
        for j in 0..NX {
            let filled_rows: Vec<usize> = (0..NY).filter(|&i| actual.filled(j, i)).collect();
            if filled_rows.is_empty() {
                column_info.push(None);
                continue;
            }
            let (lo, hi) = (*filled_rows.first().unwrap(), *filled_rows.last().unwrap());
            m.holes += (hi - lo + 1) - filled_rows.len();
            let (top, bottom) = (actual.wy(hi), actual.wy(lo));
            tops.push(top);
            bottoms.push(bottom);
            column_info.push(Some((top, bottom)));
        }
        let spread = |v: &Vec<f32>| {
            v.iter().cloned().fold(f32::NEG_INFINITY, f32::max)
                - v.iter().cloned().fold(f32::INFINITY, f32::min)
        };
        m.top_spread = spread(&tops);
        m.bottom_spread = spread(&bottoms);

        let mut lefts = Vec::new();
        let mut rights = Vec::new();
        for i in 0..NY {
            let filled_cols: Vec<usize> = (0..NX).filter(|&j| actual.filled(j, i)).collect();
            if filled_cols.is_empty() {
                continue;
            }
            lefts.push(actual.wx(*filled_cols.first().unwrap()));
            rights.push(actual.wx(*filled_cols.last().unwrap()));
        }
        m.left_spread = spread(&lefts);
        m.right_spread = spread(&rights);

        let filled_count: usize = actual.cells.iter().flatten().filter(|&&b| b).count();
        m.area = filled_count as f32 / (SX * SY) as f32;

        let (ideal_top, ideal_bottom) = (pos.y + 0.5, pos.y - 0.5);
        m.ragged_columns = (0..PX_W)
            .map(|px| {
                [2 * px, 2 * px + 1].iter().any(|&j| {
                    column_info[j].is_some_and(|(top, bottom)| {
                        (top - ideal_top).abs() > 0.125 || (bottom - ideal_bottom).abs() > 0.125
                    })
                })
            })
            .collect();
        m
    }

    fn failures(&self) -> Vec<String> {
        let mut out = Vec::new();
        // without this the spread checks below pass vacuously on an empty
        // or degenerate render (no filled columns -> no spread)
        if self.area == 0.0 {
            out.push("no fill at all".to_string());
        } else if (self.area - 1.0).abs() > 0.3 {
            // 0.3 exceeds the worst-case quantization error of any single
            // glyph family (~0.23: x edges to 1/16, y edges to 1/6)
            out.push(format!("area {:.3} too far from 1.0", self.area));
        }
        for (name, spread) in [
            ("top", self.top_spread),
            ("bottom", self.bottom_spread),
            ("left", self.left_spread),
            ("right", self.right_spread),
        ] {
            if spread > 1e-6 {
                out.push(format!("{name}-edge spread {spread:.3}"));
            }
        }
        if self.holes > 0 {
            out.push(format!("{} hole(s) in the fill", self.holes));
        }
        out
    }
}

struct PositionAnalysis {
    pos: WorldPoint,
    grid: [[DoubleChar; 3]; 3],
    owners: [[[Option<usize>; 2]; 3]; 3],
    center: WorldSquare,
    actual: FillGrid,
    ideal: FillGrid,
    metrics: Metrics,
    failures: Vec<String>,
}

fn analyze(pos: WorldPoint) -> PositionAnalysis {
    let (grid, center) = rendered_neighborhood(pos);
    let owners = assign_colors(&grid);
    let origin = euclid::point2(center.x as f32 - 1.5, center.y as f32 - 1.5);
    let actual = FillGrid::sample(origin, |wx, wy| actual_sample(&grid, &owners, center, wx, wy));
    let ideal = FillGrid::sample(origin, |wx, wy| {
        (
            (wx - pos.x).abs() <= 0.5 && (wy - pos.y).abs() <= 0.5,
            Some(0),
        )
    });
    let metrics = Metrics::measure(&actual, pos);
    let failures = metrics.failures();
    PositionAnalysis {
        pos,
        grid,
        owners,
        center,
        actual,
        ideal,
        metrics,
        failures,
    }
}

/// Small view builder: the 3x3 world squares as 6 half-cell chars per row
/// over the cell checkerboard. `cell` resolves each half-cell to the char
/// to show and its fg color.
fn small_view_lines(
    style: &Style,
    cell: impl Fn(i32, i32, usize) -> (char, Rgb),
) -> Vec<String> {
    [1i32, 0, -1]
        .iter()
        .map(|&dy| {
            let mut line = String::new();
            for dx in -1..=1i32 {
                for half in 0..2 {
                    line.push_str(
                        &style.bg(cell_bg((dx + 1) as usize * 2 + half, (1 - dy) as usize)),
                    );
                    let (c, color) = cell(dx, dy, half);
                    line.push_str(&format!("{}{c}", style.fg(color)));
                }
            }
            line.push_str(style.reset());
            line
        })
        .collect()
}

/// Small view, monochrome: the square in uniform grey.
fn plain_view_lines(grid: &[[DoubleChar; 3]; 3], style: &Style) -> Vec<String> {
    small_view_lines(style, |dx, dy, half| {
        let c = grid[(dx + 1) as usize][(dy + 1) as usize][half];
        if c == SPACE {
            ('·', DOT_COLOR)
        } else {
            (c, IDEAL_COLOR)
        }
    })
}

/// Small view, colored: each glyph in its assigned color.
fn colored_view_lines(
    grid: &[[DoubleChar; 3]; 3],
    owners: &[[[Option<usize>; 2]; 3]; 3],
    style: &Style,
) -> Vec<String> {
    small_view_lines(style, |dx, dy, half| {
        let c = grid[(dx + 1) as usize][(dy + 1) as usize][half];
        match owners[(dx + 1) as usize][(dy + 1) as usize][half] {
            Some(idx) => (c, PALETTE[idx]),
            None => ('·', DOT_COLOR),
        }
    })
}

/// The reference rendering of one half-cell: the true unit square's
/// coverage of the cell, glyphized coherently as a hextant. Each 2x3
/// sub-cell is filled iff at least half (>= 8 of 16) of its samples are
/// covered; the 4x4 sub-samples can alias exactly onto the square's edge,
/// which is what makes the exact-half rule load-bearing.
fn correct_view_char(pos: WorldPoint, square: WorldSquare, half: usize) -> Option<char> {
    let cell_left = square.x as f32 - 0.5 + 0.5 * half as f32;
    let cell_bottom = square.y as f32 - 0.5;
    let mut array = [[false; 2]; 3];
    let mut any = false;
    for row in 0..3 {
        for col in 0..2 {
            let mut count = 0;
            for sy in 0..4 {
                for sx in 0..4 {
                    let fx = (col as f32 + (sx as f32 + 0.5) / 4.0) / 2.0;
                    let fy = ((2 - row) as f32 + (sy as f32 + 0.5) / 4.0) / 3.0;
                    let wx = cell_left + fx * 0.5;
                    let wy = cell_bottom + fy;
                    if (wx - pos.x).abs() <= 0.5 && (wy - pos.y).abs() <= 0.5 {
                        count += 1;
                    }
                }
            }
            array[row][col] = count >= 8;
            any |= array[row][col];
        }
    }
    any.then(|| hextant_array_to_char(array))
}

/// Small view of the reference rendering, colored per piece. Pieces keep
/// the color of the same cell in the actual view; pieces the actual render
/// is missing entirely are gray.
fn correct_view_lines(a: &PositionAnalysis, style: &Style) -> Vec<String> {
    small_view_lines(style, |dx, dy, half| {
        let square = a.center + vec2(dx, dy);
        match correct_view_char(a.pos, square, half) {
            Some(c) => {
                let color = a.owners[(dx + 1) as usize][(dy + 1) as usize][half]
                    .map(|idx| PALETTE[idx])
                    .unwrap_or(IDEAL_COLOR);
                (c, color)
            }
            None => ('·', DOT_COLOR),
        }
    })
}

/// Pad a possibly ANSI-styled line to a visible width.
fn pad(line: &str, visible_len: usize, width: usize) -> String {
    format!("{}{}", line, " ".repeat(width.saturating_sub(visible_len)))
}

/// Zoomed-in row for one position: glyph view, sampled actual coverage,
/// sampled ideal coverage, deviation markers, metrics.
fn zoomed_report(idx: usize, count: usize, a: &PositionAnalysis, style: &Style) -> String {
    let frac = fraction_part(a.pos);
    let status = if a.failures.is_empty() { "pass" } else { "FAIL" };
    let mut out = format!(
        "── [{idx}/{count}] pos=({:.3}, {:.3})  frac=({:+.3}, {:+.3})  ── {status} ──\n",
        a.pos.x, a.pos.y, frac.x, frac.y
    );
    out.push_str(&format!(
        "  char grid: world squares x={}..={}, y={}..={}   (background checkerboard = character cells)\n",
        a.center.x - 1,
        a.center.x + 1,
        a.center.y - 1,
        a.center.y + 1
    ));
    out.push_str(&format!(
        "  {:GLYPH_W$}  {:BITMAP_W$}  {}\n",
        "glyphs", "actual coverage", "ideal (true square)"
    ));

    let glyphs = colored_view_lines(&a.grid, &a.owners, style);
    let actual_lines = a.actual.bitmap_pane(&PALETTE, style);
    let ideal_lines = a.ideal.bitmap_pane(&[IDEAL_COLOR], style);
    let height = actual_lines.len().max(glyphs.len());
    for row in 0..height {
        let g = glyphs.get(row).map(String::as_str).unwrap_or("");
        let ac = actual_lines.get(row).map(String::as_str).unwrap_or("");
        let id = ideal_lines.get(row).map(String::as_str).unwrap_or("");
        out.push_str(&format!(
            "  {}  {}  {}\n",
            pad(g, if row < glyphs.len() { GLYPH_W } else { 0 }, GLYPH_W),
            pad(ac, if row < actual_lines.len() { BITMAP_W } else { 0 }, BITMAP_W),
            id
        ));
    }

    let markers: String = a
        .metrics
        .ragged_columns
        .iter()
        .map(|&ragged| if ragged { '^' } else { ' ' })
        .collect();
    if markers.trim().len() > 0 {
        out.push_str(&format!(
            "  {:GLYPH_W$}  {}{markers}{}  <-- columns >1/8 off the true edge\n",
            "",
            style.fg(MARKER_COLOR),
            style.reset()
        ));
    }
    out.push_str(&format!(
        "  edge spreads: top {:.3}  bottom {:.3}  left {:.3}  right {:.3}   holes: {}   area {:.3} (err {:+.3})\n",
        a.metrics.top_spread,
        a.metrics.bottom_spread,
        a.metrics.left_spread,
        a.metrics.right_spread,
        a.metrics.holes,
        a.metrics.area,
        a.metrics.area - 1.0,
    ));
    out
}

/// Renders the square along a line through the reported tearing position
/// (2.363, -0.816) and asserts the silhouette stays rectangular: straight
/// edges, no holes, area ~1. FAILS at several positions — that is the
/// demonstration of the bug, deliberately left red until the renderer
/// picks one glyph family per square instead of per half-cell.
#[test]
fn test_square_silhouette_stays_rectangular_along_motion_line() {
    let base = euclid::point2(2.363f32, -0.816f32);
    let step = vec2(0.06f32, 0.03f32);
    let positions: Vec<WorldPoint> = (-4..=4)
        .map(|i| base + step * i as f32)
        .collect();
    let analyses: Vec<PositionAnalysis> = positions.iter().map(|&p| analyze(p)).collect();
    let style = Style::from_env();

    let mut report = String::from(
        "\nfloating square along a line; the reported case is [5].\n\
         A square must have a straight top/bottom/left/right edge and no holes.\n\n",
    );

    // strips of small views, one 6-wide view per position
    let n = analyses.len();
    report.push_str("          ");
    for k in 0..n {
        report.push_str(&format!("{:^7}", format!("[{}]", k + 1)));
    }
    report.push('\n');
    let strips: Vec<(&str, Vec<Vec<String>>)> = vec![
        (
            "plain",
            analyses
                .iter()
                .map(|a| plain_view_lines(&a.grid, &style))
                .collect(),
        ),
        (
            "colored",
            analyses
                .iter()
                .map(|a| colored_view_lines(&a.grid, &a.owners, &style))
                .collect(),
        ),
        (
            "correct",
            analyses.iter().map(|a| correct_view_lines(a, &style)).collect(),
        ),
    ];
    for (label, views) in &strips {
        for row in 0..3 {
            let label = if row == 1 { *label } else { "" };
            report.push_str(&format!("  {:7} ", label));
            report.push_str(
                &views
                    .iter()
                    .map(|v| v[row].as_str())
                    .collect::<Vec<_>>()
                    .join(" "),
            );
            report.push('\n');
        }
        report.push('\n');
    }

    // mark the columns (positions) with errors
    report.push_str("          ");
    for a in &analyses {
        if a.failures.is_empty() {
            report.push_str("       ");
        } else {
            report.push_str(&format!("{} ^^^^  {}", style.fg(MARKER_COLOR), style.reset()));
        }
    }
    report.push_str("\n\n");

    // legend
    for (k, a) in analyses.iter().enumerate() {
        let frac = fraction_part(a.pos);
        let status = if a.failures.is_empty() {
            "pass".to_string()
        } else {
            format!("FAIL: {}", a.failures.join(", "))
        };
        report.push_str(&format!(
            "  [{}] pos=({:.3}, {:.3})  frac=({:+.3}, {:+.3})  {status}\n",
            k + 1,
            a.pos.x,
            a.pos.y,
            frac.x,
            frac.y
        ));
    }

    // zoomed-in rows for the failed positions only
    let failed: Vec<(usize, &PositionAnalysis)> = analyses
        .iter()
        .enumerate()
        .filter(|(_, a)| !a.failures.is_empty())
        .collect();
    if !failed.is_empty() {
        report.push_str("\nzoomed views of failed positions:\n\n");
        for (k, a) in &failed {
            report.push_str(&zoomed_report(k + 1, n, a, &style));
            report.push('\n');
        }
    }

    // the report rides the panic payload on failure; printing it too
    // would show it twice under --nocapture
    if failed.is_empty() {
        println!("{report}");
    }
    assert!(
        failed.is_empty(),
        "square silhouette is not rectangular at {}/{} sampled positions:\n{}\n\nfull report:\n{}",
        failed.len(),
        n,
        failed
            .iter()
            .map(|(k, a)| format!(
                "  [{}/{}] pos=({:.3}, {:.3}): {}",
                k + 1,
                n,
                a.pos.x,
                a.pos.y,
                a.failures.join(", ")
            ))
            .collect::<Vec<_>>()
            .join("\n"),
        report
    );
}

/// Pin the coverage oracle: every metric above flows through
/// `glyph_filled`, so a flipped axis or bit order here would silently
/// invalidate the other tests.
#[test]
fn test_glyph_filled_coverage_model() {
    let f = glyph_filled;
    // trivial cases
    assert!(!f(SPACE, 0.5, 0.5));
    assert!(f(FULL_BLOCK, 0.01, 0.99));
    // half and quadrant blocks: right/left and up/down not swapped
    assert!(f('▐', 0.75, 0.5) && !f('▐', 0.25, 0.5));
    assert!(f('▌', 0.25, 0.5) && !f('▌', 0.75, 0.5));
    assert!(f('▀', 0.5, 0.75) && !f('▀', 0.5, 0.25));
    assert!(f('▄', 0.5, 0.25) && !f('▄', 0.5, 0.75));
    assert!(f('▖', 0.25, 0.25) && !f('▖', 0.75, 0.25) && !f('▖', 0.25, 0.75));
    assert!(f('▝', 0.75, 0.75) && !f('▝', 0.25, 0.75) && !f('▝', 0.75, 0.25));
    // eighth blocks: fill measured from the named edge, boundary excluded
    assert!(f(EIGHTH_BLOCKS_FROM_RIGHT[2], 0.9, 0.5));
    assert!(!f(EIGHTH_BLOCKS_FROM_RIGHT[2], 0.5, 0.5));
    assert!(!f(EIGHTH_BLOCKS_FROM_RIGHT[2], 0.75, 0.5));
    assert!(f(EIGHTH_BLOCKS_FROM_LEFT[4], 0.4, 0.5) && !f(EIGHTH_BLOCKS_FROM_LEFT[4], 0.6, 0.5));
    assert!(f(EIGHTH_BLOCKS_FROM_BOTTOM[1], 0.5, 0.1) && !f(EIGHTH_BLOCKS_FROM_BOTTOM[1], 0.5, 0.5));
    assert!(f(EIGHTH_BLOCKS_FROM_TOP[7], 0.5, 0.9) && !f(EIGHTH_BLOCKS_FROM_TOP[7], 0.5, 0.1));
    // third blocks
    assert!(f(UPPER_TWO_THIRD_BLOCK, 0.5, 0.9) && f(UPPER_TWO_THIRD_BLOCK, 0.5, 0.5));
    assert!(!f(UPPER_TWO_THIRD_BLOCK, 0.5, 0.1));
    assert!(f(LOWER_ONE_THIRD_BLOCK, 0.5, 0.1) && !f(LOWER_ONE_THIRD_BLOCK, 0.5, 0.5));
    // sextants: '🬀' (U+1FB00) is the top-left sub-cell only
    assert!(f('🬀', 0.25, 0.9));
    assert!(!f('🬀', 0.75, 0.9) && !f('🬀', 0.25, 0.5) && !f('🬀', 0.25, 0.1));
    // cross-check the sextant arm against the crate's array->char direction
    // (independent logic path through the same bit convention)
    let only_bottom_right = hextant_array_to_char([[false, false], [false, false], [false, true]]);
    assert!(f(only_bottom_right, 0.75, 0.1));
    assert!(!f(only_bottom_right, 0.25, 0.1) && !f(only_bottom_right, 0.75, 0.9));
    let full_sextant = hextant_array_to_char([[true; 2]; 3]);
    assert!(f(full_sextant, 0.25, 0.9) && f(full_sextant, 0.75, 0.1));
}

/// The 1D path (`characters_for_full_square_with_looping_1d_offset`, used by
/// shockwave animations) should move the rendered region monotonically with
/// the offset. This one is expected to pass already; it guards the path the
/// coherence fix must not regress.
#[test]
fn test_1d_offset_rendering_moves_monotonically() {
    // vertical: as the square moves up, the top edge inside the cell sinks
    let mut prev_top: Option<f32> = None;
    for step in 0..=99 {
        let t = step as f32 / 100.0;
        let chars = characters_for_full_square_with_looping_1d_offset(STEP_UP.into(), t);
        let top = (0..SY)
            .filter(|&i| glyph_filled(chars[0], 0.5, (i as f32 + 0.5) / SY as f32))
            .map(|i| (i as f32 + 0.5) / SY as f32)
            .last();
        if let (Some(prev), Some(top)) = (prev_top, top) {
            assert!(
                top <= prev + 1e-6,
                "vertical top edge jumped up: t={t:.2} edge {top:.3} after {prev:.3}"
            );
        }
        prev_top = top.or(prev_top);
    }

    // horizontal: as the square moves right, both edges move right
    let mut prev_edges: Option<(f32, f32)> = None;
    for step in 0..=99 {
        let t = step as f32 / 100.0;
        let chars = characters_for_full_square_with_looping_1d_offset(STEP_RIGHT.into(), t);
        let filled_x: Vec<f32> = (0..2 * SX)
            .map(|k| -0.5 + (k as f32 + 0.5) / (2 * SX) as f32)
            .filter(|&wx| {
                let half = if wx < 0.0 { 0 } else { 1 };
                let fx = (wx - (-0.5 + 0.5 * half as f32)) * 2.0;
                glyph_filled(chars[half], fx, 0.5)
            })
            .collect();
        if filled_x.is_empty() {
            continue;
        }
        let edges = (*filled_x.first().unwrap(), *filled_x.last().unwrap());
        if let Some(prev) = prev_edges {
            assert!(
                edges.0 >= prev.0 - 1e-6 && edges.1 >= prev.1 - 1e-6,
                "horizontal edges moved left: t={t:.2} {edges:?} after {prev:?}"
            );
        }
        prev_edges = Some(edges);
    }
}

