#![allow(incomplete_features)]
#![feature(file_create_new)]
#![feature(return_position_impl_trait_in_trait)]
#![feature(iter_array_chunks)]
#![feature(const_for)]
#![feature(box_patterns)]

pub use defaultmap::DefaultHashMap;
use derive_more::Neg;
pub use derive_more::{Add, AddAssign, Sub, SubAssign, Sum};
pub use itertools::Itertools;
pub use num::*;
pub use owo_colors::OwoColorize;
pub use pathfinding::directed::count_paths::count_paths;
pub use pathfinding::prelude::*;
pub use reqwest::blocking::Client;
use serde::{Deserialize, Serialize};
use serde_json::Value;
pub use std::any::Any;
use std::cmp::Ordering;
pub use std::collections::*;
use std::env::args;
pub use std::fmt::{Debug, Display};
use std::fs::metadata;
pub use std::fs::{read_to_string, File};
pub use std::hash::Hash;
pub use std::io::Write;
pub use std::iter::from_fn;
pub use std::ops::Mul;
use std::ops::{Div, RangeBounds};
pub use std::process::{Command, Stdio};
use std::str::FromStr;
use std::sync::atomic::AtomicUsize;
use std::sync::Mutex;
use std::time::{Duration, Instant};
pub use std::{env, io};

#[cfg(debug_assertions)]
pub const DEBUG: bool = true;

#[cfg(not(debug_assertions))]
pub const DEBUG: bool = false;

fn fetch(url: &str) -> String {
    Client::new()
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
        .unwrap()
}

static SUBMITTED: Mutex<bool> = Mutex::new(false);

static START_TS: Mutex<Option<Instant>> = Mutex::new(None);

pub fn load_input(day: u8) -> String {
    let exe = args().next().unwrap();
    let exe_day = exe.rsplit('/').next().unwrap().int() as u8;

    if day != exe_day {
        println!(
            "{}",
            "[!] Warning: Specified day does not match the executable name [!]"
                .red()
                .bold()
        );
    }

    let input = if DEBUG {
        read_to_string(format!("src/bin/{}.sample.txt", day)).unwrap()
    } else {
        let url = format!("https://adventofcode.com/2022/day/{}/input", day);
        let path = format!("target/{}.input.txt", day);
        let input = match read_to_string(&path) {
            Ok(x) => x,
            Err(e) => {
                println!("{e:?}");
                print!("Downloading input... ");
                io::stdout().flush().unwrap();
                let input = fetch(&url);
                File::create_new(&path)
                    .unwrap()
                    .write_all(input.as_bytes())
                    .unwrap();
                println!("done!");
                input
            }
        };
        let submitted_path = format!("target/{}.submitted", day);
        let submitted = match metadata(&submitted_path) {
            Ok(_) => true,
            Err(_) => {
                let page = fetch(&format!("https://adventofcode.com/2022/day/{}", day));
                if page.contains(
                    "Both parts of this puzzle are complete! They provide two gold stars: **",
                ) {
                    if let Err(e) = File::create_new(submitted_path) {
                        dbg!(&e);
                    }
                    true
                } else {
                    false
                }
            }
        };
        *SUBMITTED.lock().unwrap() = submitted;
        input
    };

    *START_TS.lock().unwrap() = Some(Instant::now());
    input
}

#[derive(
    Clone, Copy, PartialEq, Eq, Add, AddAssign, Sub, SubAssign, Sum, Hash, PartialOrd, Ord, Neg,
)]
pub struct Coordinate2D(pub i64, pub i64);
impl Coordinate2D {
    pub const ADJACENT: [Self; 4] = [Self(0, 1), Self(1, 0), Self(-1, 0), Self(0, -1)];

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

    pub fn rotate_left(self) -> Self {
        Self(self.1, -self.0)
    }

    pub fn rotate_right(self) -> Self {
        Self(-self.1, self.0)
    }

    pub fn manhat(self) -> i64 {
        self.0.abs() + self.1.abs()
    }

    pub fn manhat_diag(self) -> i64 {
        self.1.abs().max(self.0.abs())
    }

    pub fn zero(self) -> bool {
        self == Self(0, 0)
    }

    pub fn go_straight(self, unit: Self) -> impl Iterator<Item = Self> {
        let mut pos = self;
        from_fn(move || {
            pos += unit;
            Some(pos)
        })
    }

    pub fn is_aligned(self) -> bool {
        self.0 == 0 || self.1 == 0
    }

    pub fn is_aligned_with(self, other: Self) -> bool {
        self.0 == other.0 || self.1 == other.1
    }

    pub fn is_exactly_diagonal(self) -> bool {
        self.1.abs() == self.0.abs()
    }

    pub fn is_exactly_diagonal_with(self, other: Self) -> bool {
        (other - self).is_exactly_diagonal()
    }

    pub fn goto(self, dest: Self) -> impl Iterator<Item = Self> {
        let unit = (dest - self).normalize();
        std::iter::once(self).chain(self.go_straight(unit).take((dest - self).manhat() as _))
    }

    pub fn goto_diag(self, dest: Self) -> impl Iterator<Item = Self> {
        let unit = (dest - self).normalize_diag();
        std::iter::once(self).chain(
            self.go_straight(unit)
                .take((dest - self).manhat_diag() as _),
        )
    }

    pub fn normalize(self) -> Self {
        assert!(self.is_aligned());
        self / self.manhat()
    }

    pub fn normalize_diag(self) -> Self {
        assert!(self.is_exactly_diagonal());
        self / self.manhat_diag()
    }

    pub fn up(self, n: i64) -> Self {
        self + Self(0, -1) * n
    }

    pub fn down(self, n: i64) -> Self {
        self + Self(0, 1) * n
    }

    pub fn left(self, n: i64) -> Self {
        self + Self(-1, 0) * n
    }

    pub fn right(self, n: i64) -> Self {
        self + Self(1, 0) * n
    }

    pub fn swap_coords(self) -> Self {
        Coordinate2D(self.1, self.0)
    }
}

impl Debug for Coordinate2D {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({}, {})", self.0, self.1)
    }
}

impl Mul<i64> for Coordinate2D {
    type Output = Coordinate2D;

    fn mul(self, rhs: i64) -> Self::Output {
        Coordinate2D(self.0 * rhs, self.1 * rhs)
    }
}

impl Div<i64> for Coordinate2D {
    type Output = Coordinate2D;

    fn div(self, rhs: i64) -> Self::Output {
        Coordinate2D(self.0 / rhs, self.1 / rhs)
    }
}

pub type Grid2D<T> = DefaultHashMap<Coordinate2D, T>;

pub fn parse_grid<T: Clone>(s: &str, mut f: impl FnMut(char) -> T, default: T) -> Grid2D<T> {
    let mut grid = DefaultHashMap::new(default);

    for (y, line) in s.lines().enumerate() {
        for (x, c) in line.chars().enumerate() {
            grid[Coordinate2D(x as _, y as _)] = f(c);
        }
    }

    grid
}

pub fn parse_hashset(s: &str, mut f: impl FnMut(char) -> bool) -> HashSet<Coordinate2D> {
    let mut grid = HashSet::new();

    for (y, line) in s.lines().enumerate() {
        for (x, c) in line.chars().enumerate() {
            if f(c) {
                grid.insert(Coordinate2D(x as _, y as _));
            }
        }
    }

    grid
}

pub fn print_grid<T: Clone + Debug + Any>(grid: &Grid2D<T>) {
    let min_x = grid.keys().map(|x| x.0).min().unwrap();
    let max_x = grid.keys().map(|x| x.0).max().unwrap();
    let min_y = grid.keys().map(|x| x.1).min().unwrap();
    let max_y = grid.keys().map(|x| x.1).max().unwrap();

    println!("printing grid (len={})", grid.len());

    for y in min_y..=max_y {
        for x in min_x..=max_x {
            let c = Coordinate2D(x, y);
            let data = format!("{:?}", grid[c]);
            if data.starts_with('\'') && data.ends_with('\'') {
                print!("{}", data.chars().rev().nth(1).unwrap());
            } else if data.starts_with('\"') && data.ends_with('\"') {
                let mut data = data.chars().skip(1).collect_vec();
                data.pop();
                print!("{}", data.into_iter().collect_string());
            } else {
                print!("{}", data.chars().next().unwrap());
            }
        }
        println!();
    }
    println!();
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
    println!("printing set (len={})", grid.len());

    if grid.is_empty() {
        return;
    }

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

    println!();
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Add, AddAssign, Sub, SubAssign, Sum, Hash, Neg)]
pub struct Coordinate3D(pub i64, pub i64, pub i64);

impl Mul<i64> for Coordinate3D {
    type Output = Coordinate3D;

    fn mul(self, rhs: i64) -> Self::Output {
        Coordinate3D(self.0 * rhs, self.1 * rhs, self.2 * rhs)
    }
}

impl Div<i64> for Coordinate3D {
    type Output = Coordinate3D;

    fn div(self, rhs: i64) -> Self::Output {
        Coordinate3D(self.0 / rhs, self.1 / rhs, self.2 / rhs)
    }
}

impl Coordinate3D {
    pub const ADJACENT: [Coordinate3D; 6] = [
        Coordinate3D(-1, 0, 0),
        Coordinate3D(1, 0, 0),
        Coordinate3D(0, -1, 0),
        Coordinate3D(0, 1, 0),
        Coordinate3D(0, 0, -1),
        Coordinate3D(0, 0, 1),
    ];

    pub fn cross(self, other: Self) -> Self {
        let Self(a, b, c) = self;
        let Self(x, y, z) = other;
        Self(b * z - c * y, c * x - a * z, a * y - b * x)
    }

    pub fn manhat(self) -> i64 {
        self.0.abs() + self.1.abs() + self.2.abs()
    }

    pub fn manhat_diag(self) -> i64 {
        self.0.abs().max(self.1.abs()).max(self.2.abs())
    }

    pub fn adjacent(self) -> [Self; 6] {
        Self::ADJACENT.map(|x| x + self)
    }
}

pub fn all_rotations() -> Vec<Matrix3> {
    let base = [
        Coordinate3D(1, 0, 0),
        Coordinate3D(0, 1, 0),
        Coordinate3D(0, 0, 1),
        Coordinate3D(-1, 0, 0),
        Coordinate3D(0, -1, 0),
        Coordinate3D(0, 0, -1),
    ];
    let mut result = vec![];
    for &v1 in &base {
        for &v2 in &base {
            if v1 != v2 && v1 * -1 != v2 {
                let Coordinate3D(a, b, c) = v1;
                let Coordinate3D(x, y, z) = v2;
                let cross = Coordinate3D(b * z - c * y, c * x - a * z, a * y - b * x);
                result.push(Matrix3 {
                    cols: [v1, v2, cross],
                })
            }
        }
    }
    assert_eq!(result.len(), 24);
    result
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

static COPIES: Mutex<usize> = Mutex::new(0);

pub fn cp(x: impl Display) {
    let elapsed = START_TS.lock().unwrap().unwrap().elapsed();
    let elapsed = format!("{:?}", elapsed);

    let mut copies = COPIES.lock().unwrap();
    if *copies >= 2 {
        println!("value: {}", x.red().bold());
        panic!("already copied twice");
    }
    *copies += 1;

    if DEBUG {
        println!(
            "value: {} (debug mode, not copying) took {}",
            x.blue().bold(),
            elapsed.yellow()
        );
    } else if env::var("AOC_COPY_CLIPBOARD").is_ok() {
        if !*SUBMITTED.lock().unwrap() {
            // Copy it twice to work around a bug.
            for _ in 0..2 {
                let mut cmd = Command::new("xclip")
                    .arg("-sel")
                    .arg("clip")
                    .stdin(Stdio::piped())
                    .spawn()
                    .unwrap();
                let mut stdin = cmd.stdin.take().unwrap();
                stdin.write_all(x.to_string().as_bytes()).unwrap();
                stdin.flush().unwrap();
                drop(stdin);
                cmd.wait().unwrap();
            }
            println!("value: {} (copied to clipboard)", x.green().bold());
        } else {
            println!(
                "value: {} (already submitted, not copying) took {}",
                x.green().bold(),
                elapsed.yellow()
            );
        }
    } else {
        println!(
            "value: {} (set AOC_COPY_CLIPBOARD=1 to enable copy) took {}",
            x.green().bold(),
            elapsed.yellow()
        );
    }

    *START_TS.lock().unwrap() = Some(Instant::now());
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

pub fn transpose_vec<T: Default + Clone>(s: Vec<Vec<T>>) -> Vec<Vec<T>> {
    assert!(s.iter().map(|x| x.len()).all_equal());
    assert!(!s.is_empty());
    assert!(!s[0].is_empty());
    let mut result = vec![vec![T::default(); s.len()]; s[0].len()];
    for (i, row) in s.iter().cloned().enumerate() {
        for (j, x) in row.iter().cloned().enumerate() {
            result[j][i] = x;
        }
    }
    result
}

pub fn transpose<T: Default + Clone>(
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
        .filter_map(|x| {
            if x.is_empty() {
                None
            } else {
                Some(x.trim().int())
            }
        })
        .collect_vec()
        .try_into()
        .unwrap()
}

pub fn grab_unums<const N: usize>(s: &str) -> [usize; N] {
    s.split(|c: char| !c.is_numeric())
        .filter_map(|x| {
            if x.is_empty() {
                None
            } else {
                Some(x.trim().uint())
            }
        })
        .collect_vec()
        .try_into()
        .unwrap()
}

pub trait ExtraItertools: Iterator + Sized {
    fn collect_set(self) -> HashSet<Self::Item>
    where
        Self::Item: Eq + Hash,
    {
        self.collect()
    }

    fn collect_string(self) -> String
    where
        String: FromIterator<Self::Item>,
    {
        self.collect()
    }

    fn test(
        self,
        mut pass: impl FnMut(&Self::Item) -> bool,
        mut fail: impl FnMut(&Self::Item) -> bool,
    ) -> bool {
        for item in self {
            if pass(&item) {
                return true;
            }
            if fail(&item) {
                return false;
            }
        }
        unreachable!("the iterator does not pass or fail");
    }
}

impl<T: Iterator + Sized> ExtraItertools for T {}

pub fn freqs<T: Hash + Eq>(i: impl IntoIterator<Item = T>) -> DefaultHashMap<T, usize> {
    let mut result = DefaultHashMap::new(0);
    for x in i {
        result[x] += 1;
    }
    result
}

pub trait SignedExt: Signed {
    fn with_abs(self, f: impl FnOnce(Self) -> Self) -> Self {
        f(self.abs()) * self.signum()
    }
}

impl<T: Signed> SignedExt for T {}

#[derive(Debug, PartialEq, Eq, Clone, Serialize, Deserialize)]
#[serde(untagged)]
pub enum Snailfish {
    Num(i64),
    Array(Vec<Snailfish>),
}

impl Snailfish {
    pub fn from_value(v: &Value) -> Self {
        match v {
            Value::Number(x) => Self::Num(x.as_i64().unwrap()),
            Value::Array(x) => Self::Array(x.iter().map(Self::from_value).collect_vec()),
            _ => unreachable!("invalid"),
        }
    }
}

impl FromStr for Snailfish {
    type Err = serde_json::Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        serde_json::from_str(s)
    }
}

impl PartialOrd for Snailfish {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Snailfish {
    fn cmp(&self, other: &Self) -> Ordering {
        match (self, other) {
            (Snailfish::Num(a), Snailfish::Num(b)) => a.cmp(b),
            (Snailfish::Num(a), Snailfish::Array(b)) => vec![Snailfish::Num(*a)].cmp(b),
            (Snailfish::Array(a), Snailfish::Num(b)) => a.cmp(&vec![Snailfish::Num(*b)]),
            (Snailfish::Array(a), Snailfish::Array(b)) => a.cmp(b),
        }
    }
}

#[derive(Copy, Clone, PartialEq, Eq, Hash, Debug)]
pub enum IntervalEdge {
    Start,
    End,
}

#[derive(Default, Debug, Clone)]
pub struct Intervals {
    bounds: BTreeMap<i64, IntervalEdge>,
}

impl Intervals {
    fn remove_between(&mut self, range: impl RangeBounds<i64>) {
        let (start, end) = (range.start_bound(), range.end_bound());
        match (start, end) {
            (Bound::Excluded(s), Bound::Excluded(e)) if s == e => {
                return;
            }
            (Bound::Included(s) | Bound::Excluded(s), Bound::Included(e) | Bound::Excluded(e))
                if s > e =>
            {
                return;
            }
            _ => {}
        }
        for to_remove in self.bounds.range(range).map(|x| *x.0).collect_vec() {
            self.bounds.remove(&to_remove);
        }
    }

    pub fn add(&mut self, start: i64, end: i64) {
        match (self.is_inside(start), self.is_inside(end)) {
            (true, true) => {
                self.remove_between((start + 1)..=end);
            }
            (true, false) => {
                self.remove_between((start + 1)..=end);
                self.bounds.insert(end, IntervalEdge::End);
            }
            (false, true) => {
                self.remove_between((start + 1)..=end);
                self.bounds.insert(start, IntervalEdge::Start);
            }
            (false, false) => {
                self.remove_between((start + 1)..=end);
                self.bounds.insert(start, IntervalEdge::Start);
                self.bounds.insert(end, IntervalEdge::End);
            }
        }
    }

    pub fn remove(&mut self, start: i64, end: i64) {
        match (self.is_inside(start), self.is_inside(end)) {
            (true, true) => {
                self.remove_between(start..end);
                self.bounds.insert(start, IntervalEdge::End);
                self.bounds.insert(end, IntervalEdge::Start);
            }
            (true, false) => {
                self.remove_between(start..end);
                self.bounds.insert(start, IntervalEdge::End);
            }
            (false, true) => {
                self.remove_between(start..end);
                self.bounds.insert(end, IntervalEdge::Start);
            }
            (false, false) => {
                self.remove_between(start..end);
            }
        }
    }

    pub fn remove_one(&mut self, x: i64) {
        self.remove(x, x + 1);
    }

    pub fn is_inside(&self, x: i64) -> bool {
        if let Some(edge) = self.bounds.range(..=x).next_back() {
            edge.1 == &IntervalEdge::Start
        } else {
            false
        }
    }

    pub fn covered_size(&self) -> i64 {
        let mut total = 0;
        for (left, right) in self.bounds.iter().tuple_windows() {
            match left.1 {
                IntervalEdge::Start => {
                    total += right.0 - left.0;
                }
                IntervalEdge::End => {}
            }
        }
        total
    }
}

pub fn bfs2<T: Clone + Hash + Eq, I: IntoIterator<Item = T>>(
    start: T,
    mut find_nexts: impl FnMut(usize, T) -> I,
) -> impl Iterator<Item = (usize, T)> {
    let mut edge = VecDeque::new();
    let mut seen = HashSet::new();

    seen.insert(start.clone());
    edge.push_back(start);

    let mut i = 0;

    from_fn(move || {
        let mut result = vec![];
        for _ in 0..edge.len() {
            let item = edge.pop_front()?;
            let nexts = find_nexts(i, item.clone());
            for next in nexts {
                seen.insert(next.clone());
                edge.push_back(next);
            }
            result.push((i, item));
        }
        i += 1;
        if result.is_empty() {
            return None;
        }
        Some(result)
    })
    .flatten()
}

pub fn sometimes() -> bool {
    static PREV: Mutex<Option<Instant>> = Mutex::new(None);
    static COUNT: AtomicUsize = AtomicUsize::new(0);

    let count = COUNT.fetch_add(1, std::sync::atomic::Ordering::SeqCst);

    let mut s = PREV.lock().unwrap();
    let result = s.is_none() || s.is_some_and(|x| x.elapsed() > Duration::from_millis(250));
    if result {
        println!("sometimes count: {count}");
        *s = Some(Instant::now());
    }
    result
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct Bounds {
    pub min_x: i64,
    pub max_x: i64,
    pub min_y: i64,
    pub max_y: i64,
}

impl Bounds {
    pub fn width(&self) -> i64 {
        self.max_x - self.min_x + 1
    }

    pub fn height(&self) -> i64 {
        self.max_y - self.min_y + 1
    }

    pub fn area(&self) -> i64 {
        self.width() * self.height()
    }
}

pub fn bounds(i: impl IntoIterator<Item = Coordinate2D>) -> Bounds {
    let mut i = i.into_iter();

    let first = i.next().expect("must have at least one point");
    let mut bounds = Bounds {
        min_x: first.0,
        max_x: first.0,
        min_y: first.1,
        max_y: first.1,
    };

    for c in i {
        bounds.min_x = bounds.min_x.min(c.0);
        bounds.max_x = bounds.max_x.max(c.0);
        bounds.min_y = bounds.min_y.min(c.1);
        bounds.max_y = bounds.max_y.max(c.1);
    }

    bounds
}
