use std::fs::File;
use std::io::Read;

fn main() {
    println!("Day 5");

    let input = vec![0, 3, 0, 1, -3];
    first_star(&mut input.clone());
    second_star(&mut input.clone());

    let input = File::open("5.input")
        .map_err(|err| err.to_string())
        .and_then(|mut file| {
            let mut contents = String::new();
            file.read_to_string(&mut contents)
                .map_err(|err| err.to_string())
                .map(|_| contents)
        })
        .expect("unable to read input file")
        .lines()
        .map(|line| line.parse::<i32>().expect("unable to parse number"))
        .collect::<Vec<i32>>();
    first_star(&mut input.clone());
    second_star(&mut input.clone());

}

fn first_star(input: &mut Vec<i32>) {
    println!("First star");

    let mut step = 0;
    let mut position = 0;

    while position < input.len() {
        let offset = input[position];
        input[position] = offset + 1;
        position = ((position as i32) + offset) as usize;
        step += 1;
    }

    println!("output={}", step);
}

fn second_star(input: &mut Vec<i32>) {
    println!("Second star");

    let mut step = 0;
    let mut position = 0;

    while position < input.len() {
        let offset = input[position];
        input[position] = if offset >= 3 { offset - 1 } else { offset + 1 };
        position = ((position as i32) + offset) as usize;
        step += 1;
    }

    println!("output={}", step);
}
