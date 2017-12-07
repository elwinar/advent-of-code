use std::fs::File;
use std::io::Read;

fn main() {
    println!("Day 6");

    let input = vec![0, 2, 7, 0];
    first_star(input.clone());
    second_star(input.clone());

    let input = File::open("6.input")
        .map_err(|err| err.to_string())
        .and_then(|mut file| {
            let mut contents = String::new();
            file.read_to_string(&mut contents)
                .map_err(|err| err.to_string())
                .map(|_| contents)
        })
        .expect("unable to read input file")
        .split_whitespace()
        .map(|elem| elem.parse::<u32>().expect("unable to parse number"))
        .collect::<Vec<u32>>();
    first_star(input.clone());
    second_star(input.clone());
}

fn first_star(input: Vec<u32>) {
    println!("First star");

    let mut step = 0;
    let mut configurations: Vec<Vec<u32>> = Vec::new();
    let mut state = input.clone();

    loop {
        step += 1;
        configurations.push(state.clone());

        let mut index;
        let count;
        {
            match state
                .iter()
                .rev()
                .enumerate()
                .max_by(|a, b| a.1.cmp(b.1))
                .expect("computing maximum position") {
                (i, c) => {
                    index = state.len() - i - 1;
                    count = *c;
                }
            }
        }
        state[index] = 0;
        for _ in 0..count {
            index = (index + 1) % state.len();
            state[index] += 1;
        }

        if configurations.contains(&state) {
            break;
        }
    }

    println!("output={}", step);
}

fn second_star(input: Vec<u32>) {
    println!("Second star");

    let mut step = 0;
    let mut configurations: Vec<Vec<u32>> = Vec::new();
    let mut state = input.clone();

    loop {
        step += 1;
        configurations.push(state.clone());

        let mut index;
        let count;
        {
            match state
                .iter()
                .rev()
                .enumerate()
                .max_by(|a, b| a.1.cmp(b.1))
                .expect("computing maximum position") {
                (i, c) => {
                    index = state.len() - i - 1;
                    count = *c;
                }
            }
        }
        state[index] = 0;
        for _ in 0..count {
            index = (index + 1) % state.len();
            state[index] += 1;
        }

        match configurations.iter().position(|x| x == &state) {
            None => continue,
            Some(x) => {
                step = configurations.len() - x;
                break;
            }
        };
    }

    println!("output={}", step);
}
