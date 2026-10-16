//! Tests for the pty harness itself (`tests/support/pty.rs`), so the harness is
//! trusted before it is used to judge the tool's behaviour.

mod support;

use std::path::Path;
use std::time::Duration;

use support::pty::Pty;

const SH: &str = "/bin/sh";
const TIMEOUT: Duration = Duration::from_secs(5);

fn spawn(script: &str) -> Pty {
    Pty::spawn(Path::new(SH), &["-c", script], &[], &[], 120, 40).expect("spawn pty")
}

#[test]
fn round_trips_stdin_to_the_child() {
    let mut pty = spawn("read line; printf 'GOT:%s' \"$line\"");
    pty.send(b"hello\n").unwrap();
    let status = pty.wait(TIMEOUT).unwrap().expect("child exited");
    assert!(status.success());
    assert!(
        pty.output_lossy().contains("GOT:hello"),
        "child output: {:?}",
        pty.output_lossy()
    );
}

#[test]
fn reports_the_exit_status() {
    let mut pty = spawn("exit 7");
    let status = pty.wait(TIMEOUT).unwrap().expect("child exited");
    assert_eq!(status.code(), Some(7));
}

#[test]
fn applies_the_requested_window_size() {
    let mut pty = spawn("stty size");
    let _ = pty.wait(TIMEOUT).unwrap();
    assert!(
        pty.output_lossy().contains("40 120"),
        "stty size output: {:?}",
        pty.output_lossy()
    );
}

#[test]
fn clears_listed_environment_variables() {
    let mut pty = Pty::spawn(
        Path::new(SH),
        &["-c", "printf 'GD=[%s]' \"$GIT_DIR\""],
        &[],
        &["GIT_DIR"],
        120,
        40,
    )
    .unwrap();
    let _ = pty.wait(TIMEOUT).unwrap();
    assert!(
        pty.output_lossy().contains("GD=[]"),
        "child output: {:?}",
        pty.output_lossy()
    );
}

#[test]
fn sets_environment_variables() {
    let mut pty = Pty::spawn(
        Path::new(SH),
        &["-c", "printf 'V=%s' \"$FOO\""],
        &[("FOO", "bar")],
        &[],
        120,
        40,
    )
    .unwrap();
    let _ = pty.wait(TIMEOUT).unwrap();
    assert!(pty.output_lossy().contains("V=bar"));
}

#[test]
fn wait_for_reports_a_marker_and_times_out_without_one() {
    let mut pty = spawn("printf ready; sleep 30");
    assert!(pty.wait_for(b"ready", TIMEOUT), "marker should appear");
    assert!(
        !pty.wait_for(b"never-printed", Duration::from_millis(100)),
        "absent marker must time out"
    );
}
