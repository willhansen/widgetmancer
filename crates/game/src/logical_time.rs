//! Injectable logical time for the simulation and render paths (roadmap W.A).
//!
//! `std::time::Instant` is opaque and cannot be reconstructed from a snapshot,
//! so a loaded game had to rebase its clock to `Instant::now()`, and any draw
//! path that read the wall clock made frames irreproducible. `LogicalTime` is a
//! monotonic `Duration` newtype that is `Copy`, comparable, and constructible
//! from serialized data. The only place allowed to read the wall clock is the
//! driver loop in `lib.rs` (and the input thread), which converts elapsed real
//! time into a `LogicalTime` tick.

use std::fmt;
use std::ops::{Add, AddAssign, Sub};
use std::time::Duration;

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug, Default, Hash)]
pub struct LogicalTime(Duration);

impl LogicalTime {
    pub const ZERO: LogicalTime = LogicalTime(Duration::ZERO);

    /// Sentinel for an animation whose start time has not yet been resolved to
    /// a frame time. `Graphics::resolve_animation_start_times` replaces it on
    /// the first frame the animation is present.
    pub const UNSET: LogicalTime = LogicalTime(Duration::MAX);

    pub fn from_duration(duration: Duration) -> Self {
        LogicalTime(duration)
    }

    pub fn from_secs_f32(seconds: f32) -> Self {
        LogicalTime(Duration::from_secs_f32(seconds.max(0.0)))
    }

    pub fn as_duration(self) -> Duration {
        self.0
    }

    pub fn as_secs_f32(self) -> f32 {
        self.0.as_secs_f32()
    }

    pub fn is_set(self) -> bool {
        self != LogicalTime::UNSET
    }

    /// Mirrors `Instant::duration_since`: panics if `earlier` is later.
    pub fn duration_since(self, earlier: LogicalTime) -> Duration {
        self.0.checked_sub(earlier.0).unwrap_or_else(|| {
            panic!(
                "LogicalTime::duration_since: earlier is later ({earlier:?} > {self:?})"
            )
        })
    }

    pub fn saturating_duration_since(self, earlier: LogicalTime) -> Duration {
        self.0.saturating_sub(earlier.0)
    }
}

impl Add<Duration> for LogicalTime {
    type Output = LogicalTime;
    fn add(self, rhs: Duration) -> LogicalTime {
        LogicalTime(self.0 + rhs)
    }
}

impl Sub<Duration> for LogicalTime {
    type Output = LogicalTime;
    fn sub(self, rhs: Duration) -> LogicalTime {
        LogicalTime(self.0 - rhs)
    }
}

impl AddAssign<Duration> for LogicalTime {
    fn add_assign(&mut self, rhs: Duration) {
        self.0 += rhs;
    }
}

impl Sub<LogicalTime> for LogicalTime {
    type Output = Duration;
    fn sub(self, rhs: LogicalTime) -> Duration {
        self.duration_since(rhs)
    }
}

impl fmt::Display for LogicalTime {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:.6}s", self.as_secs_f32())
    }
}

#[cfg(test)]
mod tests {
    use std::path::{Path, PathBuf};

    /// Wall-clock reads must stay at the driver seam so frames are
    /// reproducible (roadmap W.A). `lib.rs` is the driver + input thread;
    /// `portal_playground` runs its own local, non-snapshot game.
    const ALLOWED: &[&str] = &[
        "src/lib.rs",
        "src/bin/portal_playground.rs",
        // This audit test contains the literal it searches for.
        "src/logical_time.rs",
    ];

    fn collect_rs_files(dir: &Path, out: &mut Vec<PathBuf>) {
        for entry in std::fs::read_dir(dir).expect("read_dir") {
            let path = entry.expect("dir entry").path();
            if path.is_dir() {
                collect_rs_files(&path, out);
            } else if path.extension().is_some_and(|ext| ext == "rs") {
                out.push(path);
            }
        }
    }

    #[test]
    fn wall_clock_reads_are_confined_to_the_driver_seam() {
        let src = Path::new(env!("CARGO_MANIFEST_DIR")).join("src");
        let mut files = Vec::new();
        collect_rs_files(&src, &mut files);

        let mut offenders = Vec::new();
        for file in files {
            let relative = file
                .strip_prefix(Path::new(env!("CARGO_MANIFEST_DIR")))
                .expect("strip prefix")
                .to_string_lossy()
                .replace('\\', "/");
            if ALLOWED.contains(&relative.as_str()) {
                continue;
            }
            let contents = std::fs::read_to_string(&file).expect("read source");
            for (line_number, line) in contents.lines().enumerate() {
                let trimmed = line.trim_start();
                if trimmed.starts_with("//") {
                    continue;
                }
                if line.contains("Instant::now()") {
                    offenders.push(format!("{relative}:{}", line_number + 1));
                }
            }
        }

        assert!(
            offenders.is_empty(),
            "wall-clock reads outside the allowed driver seam: {offenders:?}"
        );
    }
}
