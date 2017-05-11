extern crate rand;
extern crate time;

use std::env;
use time::*;

mod jngmisc;

use jngmisc::*;


fn main() {

    let args: Vec<String> = env::args().collect();
    let default_word = String::from("abc");
    let word: String = match args.len() {
        n if n <= 1 => default_word,
        _ => match args[1].parse::<String>().ok() {
            None => default_word,
            Some(word) => word,
        },
    };

    println!("\n\nGenerating all permutations for word '{}' ...",
             word);

    let start = precise_time_ns();
    let perms = permutations::for_string_rec(word);
    let end = precise_time_ns();
    let duration_us = (end - start) / 1000;

    println!("Permutations ({}) :\n{:?}\ntook {}us.",
             perms.len(), perms, duration_us);

}
