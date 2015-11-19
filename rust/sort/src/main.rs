use std::env;


fn main() {

    let args: Vec<String> = env::args().collect();
    let cnt: u32 = match args.len() {
        1 => 100,
        _ => match args[1].parse::<u32>().ok() {
            None => 100,
            Some(cnt) => cnt,
        },
    };

    println!("args {}", cnt);
}
