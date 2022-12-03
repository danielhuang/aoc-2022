use aoc_2022::{cp, load_input};
use itertools::Itertools;

fn main() {
    let input = load_input(1);
    let input: Vec<_> = input
        .split("\n\n")
        .map(|x| {
            x.split_whitespace()
                .map(|x| x.parse::<i32>().unwrap())
                .collect_vec()
        })
        .collect_vec();

    let x = input
        .into_iter()
        .map(|x| x.into_iter().sum::<i32>())
        .sorted()
        .rev()
        .collect_vec();

    cp(x[0]);
    cp(x[0] + x[1] + x[2]);
}
