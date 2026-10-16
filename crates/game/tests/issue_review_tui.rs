//! End-to-end tests for `issue_review`, driven through a virtual terminal: the
//! binary runs with a pty on its stdin/stdout and the test types at it.

#![cfg(feature = "debug-tools")]

mod support;

use std::path::{Path, PathBuf};
use std::thread;
use std::time::{Duration, Instant};

use support::pty::Pty;

const TIMEOUT: Duration = Duration::from_secs(10);
const ALT_SCREEN_ON: &[u8] = b"\x1b[?1049h";
const ALT_SCREEN_OFF: &str = "\x1b[?1049l";
/// Ctrl+S: save the note and advance.
const SAVE: &[u8] = b"\x13";

fn repo_issues() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR")).join("../../issues")
}

fn temp_dir(tag: &str) -> PathBuf {
    let dir = std::env::temp_dir().join(format!(
        "issue_review_tui_{tag}_{}",
        std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .unwrap()
            .as_nanos()
    ));
    std::fs::create_dir_all(&dir).unwrap();
    dir
}

fn copy_dir(from: &Path, to: &Path) {
    std::fs::create_dir_all(to).unwrap();
    for entry in std::fs::read_dir(from).unwrap() {
        let entry = entry.unwrap();
        let source = entry.path();
        let destination = to.join(entry.file_name());
        if source.is_dir() {
            copy_dir(&source, &destination);
        } else {
            std::fs::copy(&source, &destination).unwrap();
        }
    }
}

/// A temp `issues/` holding copies of the named real issues.
fn fixture(tag: &str, numbers: &[&str]) -> PathBuf {
    let base = temp_dir(tag);
    let issues = base.join("issues");
    for number in numbers {
        copy_dir(&repo_issues().join(number), &issues.join(number));
    }
    issues
}

fn spawn_review(issues_dir: &Path, target: Option<&str>) -> Pty {    let mut args: Vec<String> = Vec::new();
    if let Some(target) = target {
        args.push(target.to_string());
    }
    args.push("--issues-dir".to_string());
    args.push(issues_dir.to_str().unwrap().to_string());
    let args: Vec<&str> = args.iter().map(String::as_str).collect();
    Pty::spawn(
        Path::new(env!("CARGO_BIN_EXE_issue_review")),
        &args,
        &[],
        // Never touch the host repo's index even though nothing moves now.
        &["GIT_DIR", "GIT_WORK_TREE"],
        120,
        40,
    )
    .expect("spawn issue_review")
}

/// Wait until the output stops growing for `quiet`, so the initial frame has
/// settled before a test measures a redraw.
fn wait_until_quiet(pty: &Pty, quiet: Duration, timeout: Duration) -> bool {
    let start = Instant::now();
    let mut last_len = pty.output().len();
    let mut last_change = Instant::now();
    while start.elapsed() < timeout {
        thread::sleep(Duration::from_millis(10));
        let len = pty.output().len();
        if len != last_len {
            last_len = len;
            last_change = Instant::now();
        } else if last_change.elapsed() >= quiet {
            return true;
        }
    }
    false
}

/// Wait until the output grows past `before` bytes.
fn wait_for_growth(pty: &Pty, before: usize, timeout: Duration) -> bool {
    let start = Instant::now();
    while start.elapsed() < timeout {
        if pty.output().len() > before {
            return true;
        }
        thread::sleep(Duration::from_millis(10));
    }
    false
}

#[test]
fn default_sweep_appends_a_note_to_one_issue_and_skips_the_empty_other() {
    let issues = fixture("sweep", &["0022", "0027"]);
    let noted_path = issues.join("0022/issue.md");
    let skipped_path = issues.join("0027/issue.md");
    let skipped_before = std::fs::read_to_string(&skipped_path).unwrap();

    let mut pty = spawn_review(&issues, None);
    assert!(
        pty.wait_for(ALT_SCREEN_ON, TIMEOUT),
        "did not enter the alternate screen"
    );
    assert!(
        wait_until_quiet(&pty, Duration::from_millis(200), TIMEOUT),
        "the initial frame never settled"
    );
    // Typing must redraw the editor. Guard against the regression where edits
    // landed in the buffer but the screen never refreshed.
    let before = pty.output().len();
    pty.send(b"a").unwrap();
    assert!(
        wait_for_growth(&pty, before, TIMEOUT),
        "typing produced no redraw"
    );
    // 0022 is first: finish the note and save. 0027 is next: save an empty note.
    pty.send(b" note").unwrap();
    pty.send(SAVE).unwrap();
    pty.send(SAVE).unwrap();
    let status = pty.wait(TIMEOUT).unwrap().expect("child exited");
    let out = pty.output_lossy();

    let noted = std::fs::read_to_string(&noted_path).unwrap();
    let skipped_after = std::fs::read_to_string(&skipped_path).unwrap();
    std::fs::remove_dir_all(issues.parent().unwrap()).ok();

    assert!(status.success(), "non-zero exit; output:\n{out}");
    assert!(out.contains(ALT_SCREEN_OFF), "did not leave the alternate screen");
    assert!(
        noted.contains("## Appended note (") && noted.contains("a note"),
        "0022 issue.md was not appended:\n{noted}"
    );
    assert_eq!(
        skipped_before, skipped_after,
        "0027 issue.md must be untouched for an empty note"
    );
    assert!(out.contains("issue 0022: appended"), "summary:\n{out}");
    assert!(out.contains("issue 0027: no note"), "summary:\n{out}");
}

#[test]
fn a_single_target_reviews_just_that_issue() {
    let issues = fixture("single", &["0022", "0027"]);
    let noted_path = issues.join("0027/issue.md");
    let untouched_path = issues.join("0022/issue.md");
    let untouched_before = std::fs::read_to_string(&untouched_path).unwrap();

    let mut pty = spawn_review(&issues, Some("0027"));
    assert!(pty.wait_for(ALT_SCREEN_ON, TIMEOUT));
    pty.send(b"looks good").unwrap();
    pty.send(SAVE).unwrap();
    let status = pty.wait(TIMEOUT).unwrap().expect("child exited");
    let out = pty.output_lossy();

    let noted = std::fs::read_to_string(&noted_path).unwrap();
    let untouched_after = std::fs::read_to_string(&untouched_path).unwrap();
    std::fs::remove_dir_all(issues.parent().unwrap()).ok();

    assert!(status.success(), "non-zero exit; output:\n{out}");
    assert!(noted.contains("looks good"));
    assert_eq!(untouched_before, untouched_after, "0022 must be untouched");
    assert!(out.contains("issue 0027: appended"), "summary:\n{out}");
}
