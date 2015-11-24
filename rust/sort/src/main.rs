extern crate rand;

use std::env;

use rand::Rng;

mod jngmisc;

use jngmisc::sort::quicksort;

fn gen_random_vec(cnt: u64) -> Vec<u64> {
    let mut rng = rand::thread_rng();
    let mut nums: Vec<u64> = Vec::new();
    for _ in 0..cnt-1 {
        nums.push(rng.gen_range(0, cnt));
    }
    return nums;
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

    let nums = gen_random_vec(cnt);
    println!("Running sort funcs for {} random numbers :\n{:?}\n...",
             cnt, nums);

    let sorted_nums = quicksort::quicksort_rec(nums);

    println!("Sorted:\n{:?}",
             sorted_nums);

}
