pub fn radixsort(mut nums: Vec<u64>) -> Vec<u64> {

    let half_len = nums.len() / 2;
    let mut ones = nums.split_off(half_len);
    let mut zeros = nums;

    for i in 0..64 {
        let mut new_zeros = Vec::with_capacity(half_len);
        let mut new_ones = Vec::with_capacity(half_len);
        for j in 0..zeros.len() {
            let bit = zeros[j].rotate_right(i) & 1;
            if bit == 0 {
                new_zeros.push(zeros[j]);
            } else {
                new_ones.push(zeros[j]);
            }
        }
        for k in 0..ones.len() {
            let bit = ones[k].rotate_right(i) & 1;
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
