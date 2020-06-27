use rand::prelude::*;
use std::env;

fn gen_random_vec(cnt: isize) -> Vec<isize> {
    let mut rng = rand::thread_rng();
    let mut nums: Vec<isize> = Vec::new();
    for _ in 0..cnt {
        nums.push(rng.gen_range(1, cnt + 1));
    }
    return nums;
}

fn partition<T: std::cmp::PartialOrd>(v: &mut [T]) -> usize {
    let len = v.len();
    let pivot_index = len / 2;
    let last_index = len - 1;

    v.swap(pivot_index, last_index);

    let mut store_index = 0;
    for i in 0..last_index {
        if v[i] < v[last_index] {
            v.swap(i, store_index);
            store_index += 1;
        }
    }

    v.swap(store_index, len - 1);
    store_index
}

fn quick_sort<T: std::cmp::PartialOrd>(v: &mut [T]) {
    let len = v.len();
    if len >= 2 {
        let pivot_index = partition(v);
        quick_sort(&mut v[0..pivot_index]);
        quick_sort(&mut v[pivot_index + 1..len]);
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let cnt: isize = match args.len() {
        n if n <= 1 => 100,
        _ => match args[1].parse::<isize>().ok() {
            None => 100,
            Some(cnt) => cnt,
        },
    };

    let mut nums = gen_random_vec(cnt);
    let mnums = nums.as_mut_slice();

    quick_sort(mnums);

    println!("{:?}", mnums);
}
