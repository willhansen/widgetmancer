//! A minimal pseudo-terminal harness for driving a terminal program under test.
//!
//! The program runs with a pty slave on its stdin/stdout/stderr, so it sees a
//! real tty. The test writes to the master (as if typing) with [`Pty::send`] and
//! reads the program's output from an accumulating buffer via [`Pty::output`].
//!
//! Deliberately hand-rolled on `libc` (no pty crate dependency). The harness is
//! itself covered by `tests/pty_harness.rs`.

use std::fs::File;
use std::io::{self, ErrorKind, Read, Write};
use std::os::unix::io::FromRawFd;
use std::os::unix::process::CommandExt;
use std::path::Path;
use std::process::{Child, Command, ExitStatus, Stdio};
use std::sync::{Arc, Mutex};
use std::thread::{self, JoinHandle};
use std::time::{Duration, Instant};

pub struct Pty {
    master: File,
    child: Child,
    output: Arc<Mutex<Vec<u8>>>,
    reader: Option<JoinHandle<()>>,
}

impl Pty {
    /// Spawn `program` on a fresh pty sized `cols`×`rows`. `envs` are set, and
    /// the names in `clear_env` are removed, on the child.
    pub fn spawn(
        program: &Path,
        args: &[&str],
        envs: &[(&str, &str)],
        clear_env: &[&str],
        cols: u16,
        rows: u16,
    ) -> io::Result<Pty> {
        let mut master_fd: libc::c_int = -1;
        let mut slave_fd: libc::c_int = -1;
        let mut winsize = libc::winsize {
            ws_row: rows,
            ws_col: cols,
            ws_xpixel: 0,
            ws_ypixel: 0,
        };
        let opened = unsafe {
            libc::openpty(
                &mut master_fd,
                &mut slave_fd,
                std::ptr::null_mut(),
                std::ptr::null(),
                &winsize,
            )
        };
        if opened != 0 {
            return Err(io::Error::last_os_error());
        }
        // openpty does not always apply `winp` on every platform; set it too.
        unsafe {
            libc::ioctl(slave_fd, libc::TIOCSWINSZ, &winsize);
        }

        let slave = unsafe { File::from_raw_fd(slave_fd) };
        let master = unsafe { File::from_raw_fd(master_fd) };

        let mut command = Command::new(program);
        command
            .args(args)
            .stdin(Stdio::from(slave.try_clone()?))
            .stdout(Stdio::from(slave.try_clone()?))
            .stderr(Stdio::from(slave.try_clone()?));
        for (key, value) in envs {
            command.env(key, value);
        }
        for key in clear_env {
            command.env_remove(key);
        }
        unsafe {
            command.pre_exec(|| {
                // Own session, pty as controlling terminal, so job-control
                // style programs behave. Failures are non-fatal.
                libc::setsid();
                libc::ioctl(0, libc::TIOCSCTTY, 0);
                Ok(())
            });
        }

        let child = command.spawn()?;
        drop(slave);

        // Read the child's output on a thread, so the test can write while it
        // drains. Treat EIO (Linux reports it once the slave closes) as EOF.
        let output = Arc::new(Mutex::new(Vec::new()));
        let sink = Arc::clone(&output);
        let mut reader = master.try_clone()?;
        let reader = thread::spawn(move || {
            let mut buffer = [0u8; 4096];
            loop {
                match reader.read(&mut buffer) {
                    Ok(0) => break,
                    Ok(n) => sink.lock().unwrap().extend_from_slice(&buffer[..n]),
                    Err(error) if error.kind() == ErrorKind::Interrupted => continue,
                    Err(_) => break,
                }
            }
        });

        Ok(Pty {
            master,
            child,
            output,
            reader: Some(reader),
        })
    }

    /// Write bytes to the child's stdin (the pty master), as if typed.
    pub fn send(&mut self, bytes: &[u8]) -> io::Result<()> {
        self.master.write_all(bytes)?;
        self.master.flush()
    }

    /// A snapshot of everything the child has written so far.
    pub fn output(&self) -> Vec<u8> {
        self.output.lock().unwrap().clone()
    }

    /// The output decoded lossily to a string.
    pub fn output_lossy(&self) -> String {
        String::from_utf8_lossy(&self.output()).into_owned()
    }

    /// Wait until `marker` appears in the output, or `timeout` elapses.
    pub fn wait_for(&self, marker: &[u8], timeout: Duration) -> bool {
        let start = Instant::now();
        loop {
            if contains(&self.output(), marker) {
                return true;
            }
            if start.elapsed() >= timeout {
                return false;
            }
            thread::sleep(Duration::from_millis(10));
        }
    }

    /// Wait for the child to exit, up to `timeout`. `None` on timeout (the
    /// child is then killed on drop).
    pub fn wait(&mut self, timeout: Duration) -> io::Result<Option<ExitStatus>> {
        let start = Instant::now();
        loop {
            if let Some(status) = self.child.try_wait()? {
                self.join_reader();
                return Ok(Some(status));
            }
            if start.elapsed() >= timeout {
                let _ = self.child.kill();
                let _ = self.child.wait();
                self.join_reader();
                return Ok(None);
            }
            thread::sleep(Duration::from_millis(10));
        }
    }

    fn join_reader(&mut self) {
        if let Some(reader) = self.reader.take() {
            let _ = reader.join();
        }
    }
}

impl Drop for Pty {
    fn drop(&mut self) {
        let _ = self.child.kill();
        let _ = self.child.wait();
        self.join_reader();
    }
}

fn contains(haystack: &[u8], needle: &[u8]) -> bool {
    if needle.is_empty() {
        return true;
    }
    haystack
        .windows(needle.len())
        .any(|window| window == needle)
}
