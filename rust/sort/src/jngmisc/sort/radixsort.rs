pub fn radixsort(nums: Vec<u64>) -> Vec<u64> {

    let mut zeros = &nums[1..50];
    let mut ones = &nums[51..100];

    for i in 0..63 {
        let mut new_zeros = Vec::new();
        let mut new_ones = Vec::new();
        for j in 0..zeros.len()-1 {
            let num = zeros[j];
            let bit = num.rotate_right(i) & 1;
            if bit == 0 {
                new_zeros.push(zeros[j]);
            } else {
                new_ones.push(zeros[j]);
            }
        }
        for k in 0..ones.len()-1 {
            let num = ones[k];
            let bit = num.rotate_right(k) & 1;
            if bit == 0 {
                new_zeros.push(ones[k]);
            } else {
                new_ones.push(ones[k]);
            }
        }
        zeros = new_zeros;
        ones = new_ones;
    }

    zeros.append(&mut ones);
    zeros

}
