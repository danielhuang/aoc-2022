use aoc_2022::*;
use itertools::Itertools;

fn main() {
    let input = load_input(2);

    let input = input
        .lines()
        .map(|x| x.split_once(' ').unwrap())
        .map(|(a, b)| {
            (
                a.chars().exactly_one().unwrap(),
                b.chars().exactly_one().unwrap(),
            )
        })
        .collect_vec();

    let mut score = 0;

    for (opponent_shape, own_shape) in input.clone() {
        score += match (own_shape, opponent_shape) {
            ('X', 'A') => 3,
            ('Y', 'A') => 6,
            ('Z', 'A') => 0,
            ('X', 'B') => 0,
            ('Y', 'B') => 3,
            ('Z', 'B') => 6,
            ('X', 'C') => 6,
            ('Y', 'C') => 0,
            ('Z', 'C') => 3,
            _ => unreachable!(),
        };
        score += match own_shape {
            'X' => 1,
            'Y' => 2,
            'Z' => 3,
            _ => unreachable!(),
        }
    }

    cp(score);

    let mut score = 0;

    for (opponent_shape, own) in input {
        let own_shape = match (opponent_shape, own) {
            ('A', 'X') => 'C',
            ('B', 'X') => 'A',
            ('C', 'X') => 'B',
            (opponent_shape, 'Y') => opponent_shape,
            ('A', 'Z') => 'B',
            ('B', 'Z') => 'C',
            ('C', 'Z') => 'A',
            _ => unreachable!(),
        };
        score += match own {
            'X' => 0,
            'Y' => 3,
            'Z' => 6,
            _ => unreachable!(),
        };
        score += match own_shape {
            'A' => 1,
            'B' => 2,
            'C' => 3,
            _ => unreachable!(),
        }
    }

    cp(score);
}
