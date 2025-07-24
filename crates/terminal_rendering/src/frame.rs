use crate::glyph_constants::named_colors;
use crate::*;

use fancy_regex::Regex;
use itertools::Itertools;
use std::fmt::Display;
use std::fs::File;
use std::io::Write;
use std::path::PathBuf;

#[derive(PartialEq, Clone)]
pub struct Frame {
    pub grid: Vec<Vec<Glyph>>,
}

impl Frame {
    pub fn blank(width: usize, height: usize) -> Self {
        Frame {
            grid: (0..height)
                .map(|_row| {
                    (0..width)
                        .map(|_col| Glyph::solid_color(named_colors::BLACK))
                        .collect()
                })
                .collect(),
        }
    }
    pub fn save_to_file(&self, path: PathBuf) {
        File::create(path)
            .unwrap()
            .write_all(&self.string_for_regular_display().as_bytes());
    }
    pub fn from_plain_string(s: &str) -> Self {
        let lines = s.lines().collect_vec();
        let height = lines.len();
        let width = lines.iter().map(|l| l.len()).max().unwrap();
        let mut frame = Frame::blank(width, height);
        lines.into_iter().enumerate().for_each(|(row, line)| {
            line.chars()
                .enumerate()
                .for_each(|(col, char)| frame.grid[row][col] = Glyph::from_char(char))
        });
        frame
    }
    pub fn width(&self) -> usize {
        self.grid[0].len()
    }
    pub fn height(&self) -> usize {
        self.grid.len()
    }

    pub fn row_to_y(&self, row: usize) -> usize {
        self.height() - row - 1
    }
    pub fn y_to_row(&self, y: usize) -> usize {
        // Symmetric transform
        self.row_to_y(y)
    }

    pub fn get_xy(&self, frame_point: [usize; 2]) -> Glyph {
        let [x, y] = frame_point;
        assert!(
            x < self.width() && y < self.height(),
            "x: {x}, y: {y}, width: {width}, height: {height}",
            width = self.width(),
            height = self.height()
        );
        let col = x;
        let row = self.y_to_row(y);
        self.grid[row][col]
    }

    pub fn blit(&mut self, other: &Self, rowcol: [usize; 2]) {
        for other_row in 0..other.height() {
            let row = rowcol[0] + other_row;
            if row >= self.height() {
                break;
            }
            for other_col in 0..other.width() {
                let col = rowcol[1] + other_col;
                if col >= self.width() {
                    break;
                }
                self.grid[row][col] = other.grid[other_row][other_col];
            }
        }
    }
    pub fn draw_text(&mut self, txt: String, rowcol: [usize; 2]) {
        for (i, char) in txt.chars().enumerate() {
            let row = rowcol[0];
            let col = rowcol[1] + i;
            if col >= self.width() {
                break;
            }
            self.grid[row][col] = Glyph::from_char(char);
        }
    }

    pub fn framed(&self) -> String {
        //╭╮╯╰─│
        format!(
            "\
╭{horiz}╮
{contents}
╰{horiz}╯\
",
            horiz = "─".repeat(self.width()),
            contents = self
                .string_for_regular_display()
                .lines()
                .map(|row| format!("│{row}│"))
                .join("\n")
        )
    }
    pub fn string_for_regular_display(&self) -> String {
        self.non_raw_render_string(true)
    }
    fn non_raw_render_string(&self, _colored: bool) -> String {
        raw_display_string_to_regular_display_string(self.simple_raw_display_string())
    }
    pub fn uncolored_regular_string(&self) -> String {
        self.grid
            .iter()
            .map(|row| row.iter().map(|s| s.character).join(""))
            .join("\n")
    }
    fn simple_raw_display_string(&self) -> String {
        String::from_utf8(self.bytes_for_raw_display_over(&None)).unwrap()
    }
    fn raw_string(&self, maybe_old_frame: &Option<Frame>, colored: bool) -> String {
        let mut output = String::new();
        let is_simple_draw_case = maybe_old_frame.is_none() && colored;

        let mut prev_written_row_col: Option<[usize; 2]> = None;
        let mut prev_written_glyph: Option<Glyph> = None;
        for row in 0..self.height() {
            for col in 0..self.width() {
                let new_glyph = self.grid[row][col];

                if let Some(old_frame) = maybe_old_frame {
                    let old_glyph = old_frame.grid[row][col];
                    let this_square_is_same = new_glyph == old_glyph;
                    if this_square_is_same {
                        continue;
                    }
                }
                let just_next_horizontally = prev_written_row_col
                    .is_some_and(|[prev_row, prev_col]| row == prev_row && prev_col == col - 1);

                let should_do_linewrap = prev_written_row_col.is_some_and(|[prev_row, prev_col]| {
                    prev_row + 1 == row && col == 0 && prev_col + 1 == self.width()
                });
                let down_right = prev_written_row_col
                    .is_some_and(|[prev_row, prev_col]| prev_row + 1 == row && prev_col + 1 == col);

                if just_next_horizontally {
                    // Do nothing
                } else if should_do_linewrap {
                    if is_simple_draw_case {
                        output += &Glyph::color_reset_string();
                    }
                    output += "\n\r";
                } else if down_right {
                    output += "\n";
                } else {
                    output +=
                        &termion::cursor::Goto((col + 1) as u16, (row + 1) as u16).to_string();
                }

                output += &if colored {
                    if should_do_linewrap {
                        new_glyph.to_string_after(None)
                    } else {
                        new_glyph.to_string_after(prev_written_glyph)
                    }
                } else {
                    new_glyph.character.to_string()
                };
                prev_written_glyph = Some(new_glyph);
                prev_written_row_col = Some([row, col]);
            }
        }
        if is_simple_draw_case {
            output += &Glyph::color_reset_string();
        }
        output
    }

    pub fn bytes_for_raw_display_over(&self, maybe_old_frame: &Option<Frame>) -> Vec<u8> {
        self.raw_string(maybe_old_frame, true).into_bytes()
    }
    pub fn string_for_raw_display_over(&self, maybe_old_frame: &Option<Frame>) -> String {
        self.raw_string(maybe_old_frame, true)
    }
    // Debug string with readable strings instead of escape codes
    pub fn readable_string(&self) -> String {
        display_string_to_readable_string(self.simple_raw_display_string())
    }
}

pub fn display_string_to_readable_string(display_string: String) -> String {
    let a_b = [
        [r"\\u\{1b\}\[38;2(;([0-9]+))\1\1m", "BG<$2>"],
        [r"\\u\{1b\}\[48;2(;([0-9]+))\1\1m", "FG<$2>"],
        [r"\\u\{1b\}\[48;2;255;0;0m", "FG<r  >"],
        [r"\\u\{1b\}\[38;2;255;0;255m", "BG<r b>"],
        [r"\\n", "\n"],
    ];

    let mut hay = display_string.escape_debug().to_string();
    for [a, b] in a_b.into_iter() {
        hay = Regex::new(a).unwrap().replace_all(&hay, b).to_string();
    }
    hay.to_string()
}

fn string_for_goto_top_left() -> String {
    termion::cursor::Goto(1, 1).to_string()
}

// These do not account for efficient rendering by ignoring screen characters that are already
// correct
fn raw_display_string_to_regular_display_string(naive_raw_display_string: String) -> String {
    naive_raw_display_string
        .strip_prefix(&string_for_goto_top_left())
        .unwrap()
        .replace("\r", "")
}
fn regular_display_string_to_raw_display_string(regular_display_string: String) -> String {
    string_for_goto_top_left() + &regular_display_string.replace("\n", "\n\r")
}

impl Display for Frame {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.framed())
    }
}
impl std::fmt::Debug for Frame {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&format!(
            "\n{}\n\nRaw Render String:\n\n{}\n\nNon-Raw Render String:\n\n{}",
            self.framed(),
            display_string_to_readable_string(self.simple_raw_display_string()),
            display_string_to_readable_string(self.non_raw_render_string(true))
        ))
    }
}

impl From<Vec<Vec<DoubleGlyph>>> for Frame {
    fn from(value: Vec<Vec<DoubleGlyph>>) -> Self {
        Frame {
            grid: value
                .into_iter()
                .map(|row| row.into_iter().flat_map(|dg| dg.into_iter()).collect_vec())
                .collect_vec(),
        }
    }
}
#[cfg(test)]
mod tests {
    use crate::glyph_constants::named_colors;

    use super::*;
    use pretty_assertions::assert_eq;

    fn small_color_frame() -> Frame {
        use named_colors::*;
        Frame {
            grid: vec![
                vec![Glyph::solid_color(RED), Glyph::solid_color(GREEN)],
                vec![Glyph::solid_color(BLUE), Glyph::solid_color(YELLOW)],
            ],
        }
    }
    fn small_letter_frame() -> Frame {
        Frame {
            grid: vec![
                vec![Glyph::from_char('a'), Glyph::from_char('b')],
                vec![Glyph::from_char('c'), Glyph::from_char('d')],
            ],
        }
    }
    #[test]
    fn test_correct_newlines_in_raw_render() {
        let frame = small_letter_frame();
        let raw = frame.raw_string(&None, false);
        dbg!(&frame, &raw);
        assert_eq!(raw.matches("\n").count(), 1);
    }
    #[ignore]
    #[test]
    fn test_naive_raw_to_regular_conversions() {
        let ab = regular_display_string_to_raw_display_string;
        let ba = raw_display_string_to_regular_display_string;
        let e = |s: String| s.escape_debug().to_string();
        macro_rules! t {
            ($a:expr, $b:expr) => {
                assert_eq!(e($a.clone()), e($b.clone()), "{}\n\n{}", $a, $b);
            };
        }
        [small_letter_frame(), small_color_frame()]
            .iter()
            .for_each(|frame| {
                let regular = frame.non_raw_render_string(false);
                let raw = frame.raw_string(&None, false);

                let a = regular;
                let b = raw;

                t!(ba(b.clone()), a.clone());
                t!(ab(a.clone()), b.clone());
                t!(ba(ab(a.clone())), a.clone());
                t!(ab(ba(b.clone())), b.clone());
            })
    }
    #[test]
    fn test_render_string_directly_below() {
        let frame_a = Frame::from_plain_string(
            "aaa111
aaa111
",
        );
        let frame_b = Frame::from_plain_string(
            "aaa211
aaa211
",
        );
        let frame_c = Frame::from_plain_string(
            "aaa211
aaa121
",
        );
        assert_eq!(6, frame_a.width());
        assert_eq!(2, frame_a.height());

        let directly_below = frame_b.raw_string(&Some(frame_a.clone()), false);
        let correct = 
            termion::cursor::Goto(4, 1).to_string()
                + "2"
                + &termion::cursor::Goto(4, 2).to_string()
                + "2";

        assert_eq!(
            correct,
            directly_below,
            "\n\nA:\n\n{:?}\n\nB:\n\n{:?}\n\nDiff:\n\n{:?}\n\nCorrect:\n\n{correct:?}",
            frame_a.clone(),
            frame_b,
            directly_below
        );

        let below_and_next = frame_c.raw_string(&Some(frame_a.clone()), false);
        let correct = termion::cursor::Goto(4, 1).to_string() + "2\n2";
        assert_eq!(
            correct,
            below_and_next,
            "\n\nA:\n\n{frame_a:?}\n\nC:\n\n{frame_c:?}\n\nDiff:\n\n{below_and_next:?}\n\nCorrect:\n\n{correct:?}"
        );
    }
}
