#![feature(file_create_new)]
#![feature(return_position_impl_trait_in_trait)]
#![feature(iter_array_chunks)]
#![feature(const_for)]
#![feature(box_syntax, box_patterns)]

use defaultmap::DefaultHashMap;
use derive_more::{Add, AddAssign, Sub, SubAssign, Sum};
use itertools::Itertools;
use owo_colors::OwoColorize;
use reqwest::blocking::Client;
use std::collections::{HashMap, HashSet};
use std::fmt::{Debug, Display};
use std::fs::{read_to_string, File};
use std::hash::Hash;
use std::io::Write;
use std::ops::Mul;
use std::process::{Command, Stdio};
use std::{env, io};

#[cfg(debug_assertions)]
const DEBUG: bool = true;

#[cfg(not(debug_assertions))]
const DEBUG: bool = false;

pub fn load_input(day: u8) -> String {
    if DEBUG {
        read_to_string(format!("src/bin/{}.sample.txt", day)).unwrap()
    } else {
        let url = format!("https://adventofcode.com/2022/day/{}/input", day);
        let path = format!("src/bin/{}.input.txt", day);
        match read_to_string(&path) {
            Ok(x) => x,
            Err(e) => {
                println!("{e:?}");
                print!("Downloading input... ");
                io::stdout().flush().unwrap();
                let input = Client::new()
                    .get(url)
                    .header(
                        "cookie",
                        format!("session={}", env::var("AOC_SESSION").unwrap()),
                    )
                    .header(
                        "user-agent",
                        "github.com/danielhuang/aoc-2022 - hello@danielh.cc",
                    )
                    .send()
                    .unwrap()
                    .error_for_status()
                    .unwrap()
                    .text()
                    .unwrap();
                File::create_new(&path)
                    .unwrap()
                    .write_all(input.as_bytes())
                    .unwrap();
                println!("done!");
                input
            }
        }
    }
}

#[derive(
    Debug, Clone, Copy, PartialEq, Eq, Add, AddAssign, Sub, SubAssign, Sum, Hash, PartialOrd, Ord,
)]
pub struct Coordinate2D(pub i64, pub i64);
impl Coordinate2D {
    pub const ADJACENT: [Self; 4] = [Self(0, 1), Self(1, 0), Self(0, -1), Self(-1, 0)];

    pub const ADJACENT_CORNERS: [Self; 8] = [
        Self(0, 1),
        Self(1, 0),
        Self(0, -1),
        Self(-1, 0),
        Self(1, 1),
        Self(-1, 1),
        Self(1, -1),
        Self(-1, -1),
    ];

    pub fn x(self) -> i64 {
        self.0
    }

    pub fn y(self) -> i64 {
        self.1
    }

    pub fn adjacent(self) -> [Self; 4] {
        Self::ADJACENT.map(|x| self + x)
    }

    pub fn adjacent_corners(self) -> [Self; 8] {
        Self::ADJACENT_CORNERS.map(|x| self + x)
    }

    pub fn rotate(self) -> Self {
        Self(self.1, -self.0)
    }

    pub fn rotate3(self) -> Self {
        Self(-self.1, self.0)
    }

    pub fn manhat(self) -> i64 {
        self.0.abs() + self.1.abs()
    }

    pub fn zero(self) -> bool {
        self == Self(0, 0)
    }
}

impl Mul<i64> for Coordinate2D {
    type Output = Coordinate2D;

    fn mul(self, rhs: i64) -> Self::Output {
        Coordinate2D(self.0 * rhs, self.1 * rhs)
    }
}

pub fn print_grid<T: Clone + Debug>(grid: &DefaultHashMap<Coordinate2D, T>) {
    let min_x = grid.keys().map(|x| x.0).min().unwrap();
    let max_x = grid.keys().map(|x| x.0).max().unwrap();
    let min_y = grid.keys().map(|x| x.1).min().unwrap();
    let max_y = grid.keys().map(|x| x.1).max().unwrap();

    for x in min_x..=max_x {
        for y in min_y..=max_y {
            let c = Coordinate2D(x, y);
            let data = format!("{:?}", grid[c]).chars().next().unwrap();
            print!("{}", data);
        }
        println!();
    }
}

pub fn print_hashmap<T: Clone + Debug>(grid: &HashMap<Coordinate2D, T>) {
    let min_x = grid.keys().map(|x| x.0).min().unwrap();
    let max_x = grid.keys().map(|x| x.0).max().unwrap();
    let min_y = grid.keys().map(|x| x.1).min().unwrap();
    let max_y = grid.keys().map(|x| x.1).max().unwrap();

    println!("printing map (len={})", grid.len());
    for y in min_y..=max_y {
        for x in min_x..=max_x {
            let c = Coordinate2D(x, y);
            let data = if let Some(x) = grid.get(&c) {
                format!("{:?}          ", x)
                    .chars()
                    .take(3)
                    .collect::<String>()
            } else {
                "   ".to_string()
            };
            print!("{data} ");
        }
        println!();
    }
    println!();
}

pub fn print_hashset(grid: &HashSet<Coordinate2D>) {
    let min_x = grid.iter().map(|x| x.0).min().unwrap();
    let max_x = grid.iter().map(|x| x.0).max().unwrap();
    let min_y = grid.iter().map(|x| x.1).min().unwrap();
    let max_y = grid.iter().map(|x| x.1).max().unwrap();

    for y in min_y..=max_y {
        for x in min_x..=max_x {
            let c = Coordinate2D(x, y);
            print!("{}", if grid.contains(&c) { "█" } else { " " });
        }
        println!();
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Add, AddAssign, Sub, SubAssign, Sum, Hash)]
pub struct Coordinate3D(pub i64, pub i64, pub i64);

impl Mul<i64> for Coordinate3D {
    type Output = Coordinate3D;

    fn mul(self, rhs: i64) -> Self::Output {
        Coordinate3D(self.0 * rhs, self.1 * rhs, self.2 * rhs)
    }
}

#[derive(Clone, Copy, Debug)]
pub struct Matrix3 {
    pub cols: [Coordinate3D; 3],
}

impl Mul<Coordinate3D> for Matrix3 {
    type Output = Coordinate3D;

    fn mul(self, rhs: Coordinate3D) -> Self::Output {
        self.cols[0] * rhs.0 + self.cols[1] * rhs.1 + self.cols[2] * rhs.2
    }
}

pub fn cp(x: impl Display) {
    if DEBUG {
        println!("value: {} (debug mode, not copying)", x.blue().bold());
    } else if env::var("AOC_COPY_CLIPBOARD").is_ok() {
        let mut cmd = Command::new("xclip")
            .arg("-sel")
            .arg("clip")
            .stdin(Stdio::piped())
            .spawn()
            .unwrap();
        cmd.stdin
            .as_mut()
            .unwrap()
            .write_all(x.to_string().as_bytes())
            .unwrap();
        cmd.stdin.take().unwrap();
        cmd.wait().unwrap();
        println!("value: {} (copied to clipboard)", x.green().bold());
    } else {
        println!(
            "value: {} (set AOC_COPY_CLIPBOARD=1 to enable copy)",
            x.green().bold()
        );
    }
}

pub trait CollectionExt<T> {
    fn min_c(&self) -> T;
    fn max_c(&self) -> T;
    fn iter_c(&self) -> impl Iterator<Item = T>;
}

impl<T: Clone + Ord, C: IntoIterator<Item = T> + Clone> CollectionExt<T> for C {
    fn min_c(&self) -> T {
        self.clone().into_iter().min().unwrap()
    }

    fn max_c(&self) -> T {
        self.clone().into_iter().max().unwrap()
    }

    fn iter_c(&self) -> impl Iterator<Item = T> {
        self.clone().into_iter()
    }
}

pub fn collect_2d<T>(s: impl IntoIterator<Item = impl IntoIterator<Item = T>>) -> Vec<Vec<T>> {
    s.into_iter().map(|x| x.into_iter().collect()).collect()
}

pub fn transpose_vec<T: Default + Clone + Ord>(s: Vec<Vec<T>>) -> Vec<Vec<T>> {
    assert!(s.iter().map(|x| x.len()).all_equal());
    assert!(!s.is_empty());
    assert!(!s[0].is_empty());
    let mut result = vec![vec![T::default(); s.len()]; s[0].len()];
    for (i, row) in s.iter_c().enumerate() {
        for (j, x) in row.iter_c().enumerate() {
            result[j][i] = x;
        }
    }
    result
}

pub fn transpose<T: Default + Clone + Ord>(
    s: impl IntoIterator<Item = impl IntoIterator<Item = T>>,
) -> Vec<Vec<T>> {
    transpose_vec(collect_2d(s))
}

pub trait Intify {
    fn int(&self) -> i64;
    fn uint(&self) -> usize;
}

impl<T: Display> Intify for T {
    fn int(&self) -> i64 {
        self.to_string().parse().unwrap()
    }

    fn uint(&self) -> usize {
        self.to_string().parse().unwrap()
    }
}

pub fn set_n<T: Eq + Hash + Clone>(
    a: impl IntoIterator<Item = T>,
    b: impl IntoIterator<Item = T>,
) -> HashSet<T> {
    let a: HashSet<_> = a.into_iter().collect();
    let b: HashSet<_> = b.into_iter().collect();
    a.intersection(&b).cloned().collect()
}

pub fn set_u<T: Eq + Hash + Clone>(
    a: impl IntoIterator<Item = T>,
    b: impl IntoIterator<Item = T>,
) -> HashSet<T> {
    let a: HashSet<_> = a.into_iter().collect();
    let b: HashSet<_> = b.into_iter().collect();
    a.union(&b).cloned().collect()
}

use mimalloc::MiMalloc;

#[global_allocator]
static GLOBAL: MiMalloc = MiMalloc;

pub fn grab_nums<const N: usize>(s: &str) -> [i64; N] {
    s.split(|c: char| !c.is_numeric() && c != '-')
        .map(|x| x.trim().int())
        .collect_vec()
        .try_into()
        .unwrap()
}

pub fn grab_unums<const N: usize>(s: &str) -> [usize; N] {
    s.split(|c: char| !c.is_numeric())
        .map(|x| x.trim().uint())
        .collect_vec()
        .try_into()
        .unwrap()
}
