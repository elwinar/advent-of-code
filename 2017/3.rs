fn main() {
    println!("Day 3");

    first_star(1);
    first_star(9);
    first_star(10);
    first_star(12);
    first_star(23);
    first_star(1024);
    first_star(289326);

    second_star(1);
    second_star(2);
    second_star(9);
    second_star(10);
    second_star(25);
    second_star(1024);
    second_star(289326);
}

fn first_star(input: i32) {
    println!("First star");
    println!("input={}", input);

    let distance = ((input as f32 - 1.).sqrt() as i32 + 1) / 2;
    println!("distance={}", distance);

    let delta = input - (distance * 2 - 1).pow(2);
    println!("delta={}", delta);

    let step = (delta % std::cmp::max(distance * 2, 1) - distance) % std::cmp::max(distance * 2, 1);
    println!("step={}", step);

    let manhattan = distance + step.abs();
    println!("manhattan={}", manhattan);

    println!();
}

fn second_star(input: i32) {
    println!("Second star");
    println!("input={}", input);

    let size = ((input as f32).sqrt() as i32 / 2 * 2 + 5) as usize;
    let mut sq = vec![vec![0; size]; size];

    let mut i = size / 2;
    let mut j = i;
    let mut step = 2;
    let mut w = 1;
    sq[i][j] = w;

    'search: loop {
        i += 1;
        j += 1;
        for _ in 0..step {
            j -= 1;
            w = fill(&mut sq, i, j);
            if w > input {
                break 'search;
            }
        }

        for _ in 0..step {
            i -= 1;
            w = fill(&mut sq, i, j);
            if w > input {
                break 'search;
            }
        }

        for _ in 0..step {
            j += 1;
            w = fill(&mut sq, i, j);
            if w > input {
                break 'search;
            }
        }

        for _ in 0..step {
            i += 1;
            w = fill(&mut sq, i, j);
            if w > input {
                break 'search;
            }
        }

        step += 2;
    }

    println!("written={}", w);

    println!();
}

fn fill(sq: &mut Vec<Vec<i32>>, i: usize, j: usize) -> i32 {
    sq[i][j] = sq[i - 1][j - 1] + sq[i][j - 1] + sq[i + 1][j - 1] + sq[i - 1][j] +
        sq[i + 1][j] + sq[i - 1][j + 1] + sq[i][j + 1] + sq[i + 1][j + 1];
    return sq[i][j];
}
