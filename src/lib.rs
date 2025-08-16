pub mod glyph;
use std::collections::HashMap;
use std::ops::Add;
use std::ops::Mul;
use std::ops::Sub;

use geometry2::FPoint;
use geometry2::FPointExt;
use geometry2::IPoint;
pub use glyph::*;

pub mod glyph_with_transparency;
pub use glyph_with_transparency::*;

pub mod screen;
use itertools::Itertools;
use ordered_float::OrderedFloat;
pub use screen::*;

pub mod frame;
pub use frame::*;

use utility::geometry2::IPointExt;
pub use utility::*;

#[derive(Hash, Debug, Copy, Clone, Eq, PartialEq)]
pub enum ConcatVBias {
    Top,
    Middle,
    Bottom,
}
pub trait MultilineStringExt: ToString {
    fn lines_including_trailing_newline(&self) -> std::str::Split<'_, char>;
    fn is_rectangular(&self) -> bool {
        self.lines_including_trailing_newline()
            .map(|line| line.visible_len())
            .all_equal()
    }
    fn visible_len(&self) -> usize {
        self.without_control_sequences().chars().count()
    }
    fn width(&self) -> usize {
        self.lines_including_trailing_newline()
            .map(|line| line.visible_len())
            .max()
            .unwrap()
    }
    fn without_control_sequences(&self) -> String {
        let control_char_regex = regex_static::static_regex!(r"\u{1b}\[(.*?)m");
        let x = control_char_regex.split(&self.to_string()).join("");
        x
    }
    fn height(&self) -> usize {
        self.lines_including_trailing_newline().count()
    }
    // dir is 0=right, 1=up, 2=left, 3=down
    fn pad_to_rectangle(&self) -> String {
        let w = self.width();
        self.lines_including_trailing_newline()
            .map(|line| line.to_string() + &" ".repeat(w - line.visible_len()))
            .join("\n")
    }
    fn pad_side(&self, dir: i32, n: usize) -> String {
        assert!(
            self.is_rectangular(),
            "frame: \n{}\n\n{:?}",
            self.to_string().replace(" ", "."),
            self.without_control_sequences()
                .lines_including_trailing_newline()
                .map(|l| l.visible_len())
                .collect_vec()
        );
        assert!(dir >= 0 && dir < 4);

        match dir {
            0 => self
                .lines_including_trailing_newline()
                .map(|line| line.to_string() + &" ".repeat(n))
                .join("\n"),
            1 => (" ".repeat(self.width()) + "\n").repeat(n) + &self.to_string(),
            2 => self
                .lines_including_trailing_newline()
                .map(|line| " ".repeat(n) + line)
                .join("\n"),
            3 => self.to_string() + &("\n".to_owned() + &" ".repeat(self.width())).repeat(n),
            _ => panic!("invalid direction: {dir}"),
        }
    }
    fn pad_to_height_with_vbias(&self, height: usize, vbias: ConcatVBias) -> String {
        let dheight = height - self.height();
        match vbias {
            ConcatVBias::Top => self.pad_side(3, dheight),
            ConcatVBias::Middle => self
                .pad_side(1, dheight / 2)
                .pad_side(3, dheight / 2 + dheight % 2),
            ConcatVBias::Bottom => self.pad_side(1, dheight),
        }
    }
    fn linewise_prefix(&self, prefix: impl Into<String>) -> String {
        let prefix = prefix.into();
        self.lines_including_trailing_newline().map(|s| prefix.clone() + s).join("\n")
    }
    fn indent(&self) -> String {
        self.linewise_prefix("\t")
    }
    fn framed(&self) -> String {
        //╭╮╯╰─│
        format!("\

╭{horiz}╮
{contents}
╰{horiz}╯\
",
            horiz = "─".repeat(self.width()),
            contents = self
                .lines_including_trailing_newline()
                .map(|row| format!("│{row}│"))
                .join("\n")
        )
    }
}

impl MultilineStringExt for String {
    fn lines_including_trailing_newline(&self) -> std::str::Split<'_, char> {
        self.split('\n')
    }
}
impl MultilineStringExt for &str {
    fn lines_including_trailing_newline(&self) -> std::str::Split<'_, char> {
        self.split('\n')
    }
}

pub fn horiz_concat_strings(strings: &[String], spaces: usize) -> String {
    horiz_concat_strings_with_vbias(strings, spaces, ConcatVBias::Top)
}

pub fn horiz_concat_strings_with_vbias(
    strings: &[String],
    spaces: usize,
    vbias: ConcatVBias,
) -> String {
    let max_height = strings.iter().map(|col| col.height()).max().unwrap();
    let out_strings = strings
        .iter()
        .map(|col| {
            col.pad_to_rectangle()
                .pad_to_height_with_vbias(max_height, vbias)
        })
        .collect_vec();
    horiz_concat_equal_height_strings(&out_strings, spaces)
}
pub fn horiz_concat_equal_height_strings(strings: &[String], spaces: usize) -> String {
    assert!(strings.iter().map(|col| col.height()).all_equal());
    let mut out = String::new();
    let num_cols = strings.len();
    let height = strings[0].height();
    let mut columns = strings
        .into_iter()
        .map(|s| s.lines().collect_vec())
        .collect_vec();
    (0..height)
        .map(|row| {
            (0..num_cols)
                .map(|col| columns[col][row])
                .join(&" ".repeat(spaces))
        })
        .join("\n")
}

pub fn char_point_to_local_braille_square(char_point: FPoint) -> IPoint {
    [
        (char_point[0].mul(2.0).add(0.5).round() as i32).rem_euclid(2),
        (char_point[1].mul(4.0).add(1.5).round() as i32).rem_euclid(4),
    ]
}

pub fn draw_points_in_character_grid(points: &[FPoint]) -> String {
    let char_map: HashMap<IPoint, char> = points
        .iter()
        .map(|&p| (p.rounded(), char_point_to_local_braille_square(p)))
        // .inspect(|p| {
        // println!("{}, {}", p.0.to_string(), p.1.to_string());
        // })
        .sorted()
        .group_by(|(char_pos, local_braille_pos)| *char_pos)
        .into_iter()
        .map(|(char_pos, pos_pair)| {
            let local_squares = pos_pair
                .into_iter()
                .map(|(char_pos, local_pos)| local_pos)
                .collect_vec();
            (
                char_pos,
                local_braille_squares_to_braille_char2(local_squares),
            )
        })
        .collect();
    char_map_to_string(char_map)
}

pub fn bargraph(data: Vec<f32>, height: u32) -> String {
    const blocks: [char; 8] = ['▁', '▂', '▃', '▄', '▅', '▆', '▇', '█'];
    const full_block: char = '█';
    let max = data
        .iter()
        .map(|&x| OrderedFloat(x))
        .max()
        .unwrap()
        .into_inner();
    assert!(data.iter().all(|&x| x >= 0.0));

    let col_func = |val: f32| -> String {
        let normalized_val = val / max;
        let height_in_blocks = normalized_val * height as f32;
        let full_blocks = height_in_blocks.floor() as usize;
        let remainder_in_eighths = (height_in_blocks - full_blocks as f32) * 8.0;
        let full_eighths = remainder_in_eighths.round() as u32;
        (if full_eighths > 0 {
            blocks[(full_eighths - 1) as usize].to_string()
        } else {
            "".to_string()
        }) + &format!("\n{full_block}").repeat(full_blocks)
    };

    let columns = data.iter().map(|x| col_func(*x) + "\n-").collect_vec();
    let graph = horiz_concat_strings_with_vbias(&columns, 0, ConcatVBias::Bottom);

    format!("{graph}\n\nMax: {max}")
}

#[cfg(test)]
mod tests {
    use std::f32::consts::TAU;

    use pretty_assertions::{assert_eq, assert_ne};

    use super::*;

    #[test]
    fn test_horiz_concat() {
        let a = "a";
        let b = " \nb\n ";
        let c = "\nc\n";
        let s = [a, b, c].map(|x| x.to_string());

        let l = [
            (ConcatVBias::Top, "a  \n bc\n   "),
            (ConcatVBias::Middle, "   \nabc\n   "),
            (ConcatVBias::Bottom, "   \n bc\na  "),
        ];

        for (bias, good) in l {
            let r = horiz_concat_strings_with_vbias(&s, 0, bias);
            dbg!(&r, &good);
            assert_eq!(
                r,
                good,
                "\nr:\n{:?}\n\ngood:\n{:?}",
                r.replace(" ", "."),
                good.replace(" ", ".")
            );
        }
    }
    #[test]
    fn test_string_pad() {
        assert_eq!("a".pad_side(0, 2), "a  ".to_string());
        assert_eq!("a".pad_side(0, 2).pad_side(1, 1), "   \na  ".to_string());
        assert_eq!("a".pad_side(1, 2), " \n \na".to_string());
        assert_eq!("a".pad_side(2, 2), "  a".to_string());
        assert_eq!("a".pad_side(2, 2).pad_side(0, 2), "  a  ".to_string());
    }
    #[test]
    fn test_print_braille_points_simple() {
        let p = (0..20)
            .map(|t| [t as f32 * 0.2, t as f32 * 0.1])
            .collect_vec();
        let out_string = draw_points_in_character_grid(&p);
        let correct_string = "   ⢀⠄
 ⢀⠔⠁ 
⠐⠁   ";
        println!("{}", out_string);
        println!("out:\n{:?}\n\ncorrect:\n{:?}", out_string, correct_string);
        assert_eq!(&out_string, correct_string);
    }
    #[test]
    fn test_print_braille_points() {
        let p = (0..100)
            .map(|t| [t as f32 / 3.0, (t as f32 * TAU / 100.0).sin() * 3.0])
            .collect_vec();
        let out_string = draw_points_in_character_grid(&p);

        let correct_string = "     ⢀⣠⠤⠴⠤⠤⣀                      
   ⢠⠔⠉      ⠙⠢⡀                   
 ⢀⠜⠁          ⠈⢢⡀                 
⠐⠁              ⠘⢤               ⡠
                  ⠙⢄           ⣠⠊ 
                    ⠑⢤⡀     ⢀⡠⠚   
                      ⠈⠑⠒⠲⠒⠚⠉     ";
        println!("out:\n{}\n\ncorrect:\n{}", out_string, correct_string);
        assert_eq!(&out_string, correct_string);
    }
}
