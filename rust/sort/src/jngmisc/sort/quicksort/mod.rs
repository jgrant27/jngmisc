extern crate rand;

use rand::Rng;



pub fn quicksort_rec(nums: Vec<u64>) -> Vec<u64> {

    if nums.len() <= 1 {
        return nums;
    } else {
        let mut left: Vec<u64> = Vec::new();
        let mut right: Vec<u64> = Vec::new();
        let pivot = nums[0];
        for i in 1..nums.len() {
            if nums[i] < pivot {
                left.push(nums[i]);
            } else {
                right.push(nums[i]);
            }
        }
        let mut left_sorted = quicksort_rec(left);
        let mut right_sorted = quicksort_rec(right);
        left_sorted.push(pivot);
        left_sorted.append(&mut right_sorted);
        return left_sorted;
    }

}
