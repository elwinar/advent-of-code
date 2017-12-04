use std::fs::File;
use std::io::Read;
use std::iter::FromIterator;

fn main() {
    println!("Day 4");

    let input = File::open("4.input")
        .map_err(|err| err.to_string())
        .and_then(|mut file| {
            let mut contents = String::new();
            file.read_to_string(&mut contents)
                .map_err(|err| err.to_string())
                .map(|_| contents)
        })
        .expect("unable to read input file");
    first_star(&input);

    second_star(&input);
}

fn first_star(input: &String) {
    println!("First star");

    let output: i32 = input
        .lines()
        .map(|line| {
            let mut words = line.split_whitespace().collect::<Vec<&str>>();
            words.sort();
            let length = words.len();
            words.dedup();
            let uniq = words.len();
            if length == uniq { 1 } else { 0 }
        })
        .sum();

    println!("output={}", output);
}

fn second_star(input: &String) {
    println!("First star");

    let output: i32 = input
        .lines()
        .map(|line| {
            let mut words = line.split_whitespace()
                .map(|word| {
                    let mut chars = word.chars().collect::<Vec<char>>();
                    chars.sort();
                    String::from_iter(chars)
                })
                .collect::<Vec<String>>();
            words.sort();
            let length = words.len();
            words.dedup();
            let uniq = words.len();
            if length == uniq { 1 } else { 0 }
        })
        .sum();

    println!("output={}", output);
}
