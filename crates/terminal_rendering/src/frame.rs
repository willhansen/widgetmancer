use crate::glyph_constants::named_colors;
use crate::*;

use itertools::Itertools;
use rgb::RGB8;
use std::fmt::Display;
use std::fs::File;
use std::io::Write;
use std::path::PathBuf;

#[derive(PartialEq, Clone)]
pub struct Frame {
    pub grid: Vec<Vec<GlyphWithTransparency>>,
}

impl Frame {
    pub fn blank(width: usize, height: usize) -> Self {
        Self {
            grid: (0..height)
                .map(|_row| {
                    (0..width)
                        .map(|_col| Glyph::solid_color(named_colors::BLACK).into())
                        .collect::<Vec<GlyphWithTransparency>>()
                })
                .collect_vec(),
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
                .for_each(|(col, char)| frame.grid[row][col] = Glyph::from_char(char).into())
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

    pub fn get_xy(&self, frame_point: [usize; 2]) -> GlyphWithTransparency {
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
            if row < self.height() && col < self.width() {
                self.grid[row][col] = Glyph::from_char(char).into();
            }
        }
    }
    pub fn glyphs(&mut self) -> impl Iterator<Item = &mut GlyphWithTransparency> {
        self.grid.iter_mut().flat_map(|row| row.iter_mut())
    }

    pub fn bg_only(&self) -> Self {
        let mut f = self.clone();
        f.glyphs().for_each(|glyph| {
            glyph.character = ' ';
            *glyph.fg_color_mut() = named_colors::BLACK.into();
        });
        f
    }
    pub fn fg_only(&self) -> Self {
        let mut f = self.clone();
        f.glyphs().for_each(|glyph| {
            *glyph.bg_color_mut() = named_colors::BLACK.into();
        });
        f
    }
    pub fn fg_colors(&self) -> Self {
        let mut f = self.clone();
        f.glyphs().for_each(|glyph| {
            glyph.character = ' ';
            *glyph.bg_color_mut() = glyph.fg_color();
            *glyph.fg_color_mut() = named_colors::BLACK.into();
        });
        f
    }
    pub fn characters_only(&self) -> Self {
        let mut f = self.clone();
        f.glyphs().for_each(|glyph| {
            *glyph.fg_color_mut() = named_colors::WHITE.into();
            *glyph.bg_color_mut() = named_colors::BLACK.into();
        });
        f
    }

    // TODO: actually use.  Probably need to parse render strings to frames from tests
    pub fn diff_from(&self, other: &Self) -> Self {
        assert_eq!(self.width(), other.width());
        assert_eq!(self.height(), other.height());
        let mut out = self.clone();

        for row in 0..self.height() {
            for col in 0..self.width() {
                if out.grid[row][col].bg_color() == other.grid[row][col].bg_color() {
                    *out.grid[row][col].bg_color_mut() = Glyph::default_bg_color.into();
                }
                if out.grid[row][col].fg_color() == other.grid[row][col].fg_color() {
                    *out.grid[row][col].fg_color_mut() = Glyph::default_fg_color.into();
                }
                if out.grid[row][col].character == other.grid[row][col].character {
                    out.grid[row][col].character = ' ';
                }
            }
        }
        out
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
    pub fn parse_regular_display_string(input_string: String) -> Self {
        let three_bytes_regex = regex::Regex::new(r";2;([0-9]+);([0-9]+);([0-9]+)").unwrap();
        let escape_regex = regex::Regex::new("\u{1b}\\[(.*?)m").unwrap();
        const fg_set: &str = "38";
        const bg_set: &str = "48";
        const fg_reset: &str = "39";
        const bg_reset: &str = "49";
        let parse_3_byte_color_string = |param_string: &str| {
            let rgb_array: [u8; 3] = three_bytes_regex
                .captures(param_string)
                .unwrap()
                .iter()
                .skip(1)
                .map(|cap| cap.unwrap().as_str().parse::<u8>().unwrap())
                .collect_vec()
                .try_into()
                .unwrap();
            RGB8::from(rgb_array)
        };
        // regular display strings reset colors every line, so we can split easily
        let grid = input_string
            .lines()
            .map(|line| {
                let mut glyphs_out = Vec::new();

                let mut fg: Option<RGB8> = None;
                let mut bg: Option<RGB8> = None;

                let mut found_color_command_iter = escape_regex.captures_iter(line).peekable();
                while let Some(found_color_command) = found_color_command_iter.next() {
                    // let color_command = &line[found_color_command.range()];
                    let color_command = &found_color_command[1];

                    let end = if let Some(next_capture) = found_color_command_iter.peek() {
                        next_capture.get(0).unwrap().start()
                    } else {
                        line.len()
                    };

                    let text_until_next = &line[found_color_command.get(0).unwrap().end()..end];
                    let utf8_until_next = String::from_utf8(text_until_next.into()).unwrap();

                    // modify current colors based on command
                    let cmd_code = &color_command[..2];
                    let rest_of_cmd = &color_command[2..];
                    match cmd_code {
                        fg_set => fg = Some(parse_3_byte_color_string(rest_of_cmd)),
                        bg_set => bg = Some(parse_3_byte_color_string(rest_of_cmd)),
                        fg_reset => fg = None,
                        bg_reset => bg = None,
                        x => panic!("Invalid escape code: {x:?}"),
                    }

                    utf8_until_next
                        .chars()
                        .for_each(|c| glyphs_out.push(Glyph::new(c, fg.unwrap(), bg.unwrap())));
                }
                glyphs_out
            })
            .collect_vec();
        grid.into()
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
                let new_glyph: Glyph = self.grid[row][col].into();

                if let Some(old_frame) = maybe_old_frame {
                    let old_glyph: Glyph = old_frame.grid[row][col].into();
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
    pub fn set_by_double_wide_grid(
        &mut self,
        row: usize,
        wide_col: usize,
        val: DoubleGlyphWithTransparency,
    ) {
        let left_narrow_col = wide_col * 2;
        self.grid[row][left_narrow_col] = val[0];
        self.grid[row][left_narrow_col + 1] = val[1];
    }
}

pub fn display_string_to_readable_string(display_string: String) -> String {
    let a_b = [
        [r"\\u\{1b\}\[38", "FG"],
        [r"\\u\{1b\}\[48", "BG"],
        [r";2(;([0-9]+))\1\1m", "<$2>"],
        [r";2(;([0-9]+))\1\1m", "<$2>"],
        [r";2;255;0;0m", "<r  >"],
        [r";2;0;255;0m", "< g >"],
        [r";2;255;255;0m", "<rg >"],
        [r";2;0;0;255m", "<  b>"],
        [r";2;255;0;255m", "<r b>"],
        [r";2;0;255;255m", "< gb>"],
        [r";2;255;255;255m", "<rgb>"],
        [r";2;255;0;0m", "<r  >"],
        [r";2;0;255;0m", "< g >"],
        [r";2;255;255;0m", "<rg >"],
        [r";2;0;0;255m", "<  b>"],
        [r";2;255;0;255m", "<r b>"],
        [r";2;0;255;255m", "< gb>"],
        [r";2;255;255;255m", "<rgb>"],
        [r"\\n", "\n"],
    ];

    let mut hay = display_string.escape_debug().to_string();
    for [a, b] in a_b.into_iter() {
        hay = fancy_regex::Regex::new(a)
            .unwrap()
            .replace_all(&hay, b)
            .to_string();
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
        let s = horiz_concat_strings(
            &[
                ("Full", self),
                ("BG Colors", &self.bg_only()),
                ("FG Colors", &self.fg_colors()),
                ("Characters", &self.characters_only()),
            ]
            .map(|(title, frame)| {
                let full_title: String = title.to_string()
                    + &" ".repeat((frame.width() + 2).saturating_sub(title.len()));
                format!("{full_title}\n{}", frame.framed())
            }),
            5,
        );

        // f.write_str(&format!(
        //     "\n{s}\n\nReadable:\n\n{}\n\nRaw:\n\n{}",
        //     display_string_to_readable_string(self.non_raw_render_string(true)),
        //     self.simple_raw_display_string().to_debug(),
        // ))
        f.write_str(&format!("\n{s}\n"))
    }
}

impl From<Vec<Vec<DoubleGlyph>>> for Frame {
    fn from(value: Vec<Vec<DoubleGlyph>>) -> Self {
        value
            .into_iter()
            .map(|row| row.into_iter().flat_map(|dg| dg.into_iter()).collect_vec())
            .collect_vec()
            .into()
    }
}
impl From<Vec<Vec<GlyphWithTransparency>>> for Frame {
    fn from(value: Vec<Vec<GlyphWithTransparency>>) -> Self {
        assert!(value.len() > 0);
        assert!(value[0].len() > 0);
        assert!(value.iter().map(|row| row.len()).all_equal());
        Frame { grid: value }
    }
}
impl From<Vec<Vec<Glyph>>> for Frame {
    fn from(value: Vec<Vec<Glyph>>) -> Self {
        value
            .iter()
            .map(|row| {
                row.iter()
                    .map(|&g| GlyphWithTransparency::from_glyph(g))
                    .collect_vec()
            })
            .collect_vec()
            .into()
    }
}
#[cfg(test)]
mod tests {
    use crate::glyph_constants::named_colors;

    use super::*;
    use pretty_assertions::assert_eq;

    fn small_color_frame() -> Frame {
        use named_colors::*;
        vec![
            vec![Glyph::solid_color(RED), Glyph::solid_color(GREEN)],
            vec![Glyph::solid_color(BLUE), Glyph::solid_color(YELLOW)],
        ]
        .into()
    }
    fn small_letter_frame() -> Frame {
        vec![
            vec![Glyph::from_char('a'), Glyph::from_char('b')],
            vec![Glyph::from_char('c'), Glyph::from_char('d')],
        ]
        .into()
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
        let correct = termion::cursor::Goto(4, 1).to_string()
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
    #[test]
    fn test_frame_to_regular_display_string_back_to_frame() {
        let mut frame = Frame::from_plain_string(
            "abc
def
ghi",
        );
        dbg!(&frame);
        let b = Frame::parse_regular_display_string(frame.string_for_regular_display());
        dbg!(&b);
        assert_eq!(frame, b);

        *frame.grid[0][2].bg_color_mut() = named_colors::WHITE.into();
        *frame.grid[0][2].fg_color_mut() = named_colors::RED.into();

        *frame.grid[1][0].bg_color_mut() = named_colors::BLUE.into();
        *frame.grid[2][2].fg_color_mut() = named_colors::GREEN.into();

        let b = Frame::parse_regular_display_string(frame.string_for_regular_display());
        assert_eq!(frame, b);
    }
    #[test]
    fn test_regex_lazy() {
        let hay = "\u{1b}[48;2;0;0;0m\u{1b}[38;2;255;255;255mabc\u{1b}[49m\u{1b}[39m";
        let regex = regex::Regex::new("\u{1b}\\[(.*?)m").unwrap();
        dbg!(&regex);
        let captures = regex.captures(&hay);
        dbg!(&captures);
        let contents = regex
            .captures_iter(&hay)
            .map(|nth| nth[1].to_string())
            .collect_vec();
        assert_eq!(contents, vec!["48;2;0;0;0", "38;2;255;255;255", "49", "39"]);
    }
}
