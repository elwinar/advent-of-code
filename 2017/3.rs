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
    second_star(9);
    second_star(10);
    second_star(12);
    second_star(23);
    second_star(1024);
    second_star(289326);
}

fn first_star(input: i32) {
    println!("First star");
    println!("input={}", input);

    let distance = ((input as f64 - 1.).sqrt() as i32 + 1) / 2;
    println!("distance={}", distance);

    let delta = input - (distance * 2 - 1).pow(2);
    println!("delta={}", delta);

    let step = (delta % std::cmp::max(distance * 2, 1) - distance) % std::cmp::max(distance * 2, 1);
    println!("step={}", step);

    let manhattan = distance + step.abs();
    println!("manhattan={}", manhattan);

    println!();
}
