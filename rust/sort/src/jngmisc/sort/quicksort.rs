extern crate rand;

use rand::Rng;


pub fn quicksort_rec(nums: Vec<u64>) -> Vec<u64> {

    return match nums.len() {
        cnt if cnt <= 1 => nums,
        cnt => {
            let mut left = Vec::new();
            let mut right = Vec::new();
            let pivot = nums[0];
            for i in 1..cnt {
                match nums[i] {
                    num if num < pivot => left.push(num),
                    num => right.push(num),
                }
            }
            let mut left_sorted = quicksort_rec(left);
            let mut right_sorted = quicksort_rec(right);
            left_sorted.push(pivot);
            left_sorted.append(&mut right_sorted);
            return left_sorted;
        },
    };

}
