use std::env;

fn collatz(mut nums: Vec<u64>) -> Vec<u64> {
    let n = nums[nums.len()-1];
    if 1 == n {
        nums
    } else {
        if 0 == n % 2 {
            nums.push(n / 2);
        } else {
            nums.push(3 * n + 1);
        }
        collatz(nums)
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let n = args[1].parse::<u64>().unwrap();
    for i in 1..=n {
        let res = collatz(vec![i]);
        println!("{:?}", res);
        println!("{} steps", res.len());
    }
}
