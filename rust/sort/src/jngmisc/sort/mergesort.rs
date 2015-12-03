extern crate rand;

use rand::Rng;


fn merge(mut left: Vec<u64>, mut right: Vec<u64>) -> Vec<u64> {
    let mut merged = Vec::new();
    while !left.is_empty() && !right.is_empty() {
        if left.last() >= right.last() {
            merged.push(left.pop().unwrap());
        } else {
            merged.push(right.pop().unwrap());
        }
    }
    while !left.is_empty() {
        merged.push(left.pop().unwrap());
    }
    while !right.is_empty() {
        merged.push(right.pop().unwrap());
    }
    merged.reverse();
    return merged;
}

pub fn mergesort_rec(nums: Vec<u64>) -> Vec<u64> {

    return match nums.len() {
        cnt if cnt <= 1 => nums,
        cnt => {
            let mut left = Vec::new();
            let mut right = Vec::new();
            let middle = cnt / 2;
            for i in (0..middle).rev() { left.push(nums[i]); }
            for i in (middle..cnt).rev() { right.push(nums[i]); }
            left = mergesort_rec(left);
            right = mergesort_rec(right);
            return merge(left, right);
        },
    };

}
