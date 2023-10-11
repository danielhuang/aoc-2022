use aoc_2022::*;
use rayon::prelude::{IndexedParallelIterator, IntoParallelRefIterator, ParallelIterator};

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
struct State {
    ores: i32,
    clays: i32,
    obsidians: i32,
    geodes: i32,
    ore_robots: i32,
    clay_robots: i32,
    obsidian_robots: i32,
    geode_robots: i32,
}

impl State {
    fn tick(&mut self) {
        self.ores += self.ore_robots;
        self.clays += self.clay_robots;
        self.obsidians += self.obsidian_robots;
        self.geodes += self.geode_robots;
    }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
struct Blueprint {
    ore_cost_ore: i32,
    clay_cost_ore: i32,
    obsidian_cost_ore: i32,
    obsidian_cost_clay: i32,
    geode_cost_ore: i32,
    geode_cost_obsidian: i32,
}

fn geodes_possible(blueprint: Blueprint, time: i32) -> i32 {
    let state = init();
    let mut costs: HashMap<State, i32> = HashMap::new();
    costs.insert(init(), 0);

    let mut max_geodes = 0;
    let reach = dijkstra_reach(&state, |prev, cost| {
        {
            let mut state = prev.clone();
            if assume_free_robots(state.clone(), time - cost) < max_geodes {
                return vec![];
            }
            let mut cost = cost;
            while cost < time {
                state.tick();
                cost += 1;
            }
            if state.geodes > max_geodes {
                max_geodes = state.geodes;
            }
        }

        let mut result = vec![];

        let mut state = prev.clone();
        let mut time = 1;
        while state.ores < blueprint.ore_cost_ore {
            time += 1;
            state.tick();
        }
        state.ores -= blueprint.ore_cost_ore;
        state.tick();
        state.ore_robots += 1;
        result.push((state, time));

        let mut state = prev.clone();
        let mut time = 1;
        while state.ores < blueprint.clay_cost_ore {
            time += 1;
            state.tick();
        }
        state.ores -= blueprint.clay_cost_ore;
        state.tick();
        state.clay_robots += 1;
        result.push((state, time));

        let mut state = prev.clone();
        if (state.ores >= blueprint.obsidian_cost_ore || state.ore_robots > 0)
            && (state.clays >= blueprint.obsidian_cost_clay || state.clay_robots > 0)
        {
            let mut time = 1;
            while state.ores < blueprint.obsidian_cost_ore
                || state.clays < blueprint.obsidian_cost_clay
            {
                time += 1;
                state.tick();
            }
            state.ores -= blueprint.obsidian_cost_ore;
            state.clays -= blueprint.obsidian_cost_clay;
            state.tick();
            state.obsidian_robots += 1;
            result.push((state, time));
        }

        let mut state = prev.clone();
        if (state.ores >= blueprint.geode_cost_ore || state.ore_robots > 0)
            && (state.obsidians >= blueprint.geode_cost_obsidian || state.obsidian_robots > 0)
        {
            let mut time = 1;
            while state.ores < blueprint.geode_cost_ore
                || state.obsidians < blueprint.geode_cost_obsidian
            {
                time += 1;
                state.tick();
            }
            state.ores -= blueprint.geode_cost_ore;
            state.obsidians -= blueprint.geode_cost_obsidian;
            state.tick();
            state.geode_robots += 1;
            result.push((state, time));
        }

        result
            .into_iter()
            .filter(|(state, _)| {
                state.obsidian_robots <= blueprint.geode_cost_obsidian
                    && state.ore_robots
                        <= (blueprint
                            .geode_cost_ore
                            .max(blueprint.ore_cost_ore)
                            .max(blueprint.clay_cost_ore)
                            .max(blueprint.obsidian_cost_ore))
                    && state.clay_robots <= blueprint.obsidian_cost_clay
            })
            .collect_vec()
    });

    let mut max_geodes = 0;

    for item in reach {
        let mut state = item.node;
        let mut cost = item.total_cost;
        if cost > time {
            break;
        }
        while cost < time {
            state.tick();
            cost += 1;
        }
        if state.geodes > max_geodes {
            max_geodes = state.geodes;
        }
    }

    max_geodes
}

fn assume_free_robots(state: State, time_left: i32) -> i32 {
    state.geodes + state.geode_robots * time_left + (0..time_left).sum::<i32>()
}

fn init() -> State {
    State {
        ores: 0,
        clays: 0,
        obsidians: 0,
        geodes: 0,
        ore_robots: 1,
        clay_robots: 0,
        obsidian_robots: 0,
        geode_robots: 0,
    }
}

fn main() {
    let input = load_input(19);

    let input = if DEBUG {
        input.split("\n\n").collect_vec()
    } else {
        input.lines().collect_vec()
    };

    let mut v = vec![];

    for line in input {
        let [id, ore_cost_ore, clay_cost_ore, obsidian_cost_ore, obsidian_cost_clay, geode_cost_ore, geode_cost_obsidian] =
            grab_nums(line).map(|x| x as i32);

        let blueprint = Blueprint {
            ore_cost_ore,
            clay_cost_ore,
            obsidian_cost_ore,
            obsidian_cost_clay,
            geode_cost_ore,
            geode_cost_obsidian,
        };

        v.push((blueprint, id));
    }

    let part1: i32 = v
        .par_iter()
        .map(|(blueprint, id)| geodes_possible(*blueprint, 24) * id)
        .sum();

    cp(part1);

    let part2: i32 = v
        .par_iter()
        .take(3)
        .map(|(blueprint, _)| geodes_possible(*blueprint, 32))
        .product();

    cp(part2);
}
