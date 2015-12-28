extern crate rand;
extern crate time;

use std::env;
use rand::Rng;
use time::*;

mod jngmisc;

use jngmisc::sort::*;


fn gen_random_vec(cnt: u64) -> Vec<u64> {
    let mut rng = rand::thread_rng();
    let mut nums: Vec<u64> = Vec::new();
    for _ in 0..cnt {
        nums.push(rng.gen_range(1, cnt+1));
    }
    return nums;
}

fn run_sort_fn<F>(sortfn: F, name: String, cnt: u64) where
    F: Fn(Vec<u64>) -> Vec<u64> {
        let nums = gen_random_vec(cnt);

        println!("\nRunning sort {} func for {} random numbers :\n{:?} ...",
                 name, cnt, nums);

        let start = precise_time_ns();
        let sorted_nums = sortfn(nums);
        let end = precise_time_ns();
        let duration_us = (end - start) / 1000;

        println!("Sorted {} nums:\n{:?}\nusing {} func in {}us\n",
                 sorted_nums.len(), sorted_nums, name, duration_us);
    }

fn main() {

    let args: Vec<String> = env::args().collect();
    let cnt: u64 = match args.len() {
        n if n <= 1 => 100,
        _ => match args[1].parse::<u64>().ok() {
            None => 100,
            Some(cnt) => cnt,
        },
    };

    run_sort_fn(quicksort::quicksort_rec, "quicksort".to_string(), cnt);
    run_sort_fn(mergesort::mergesort_rec, "mergesort".to_string(), cnt);

}
