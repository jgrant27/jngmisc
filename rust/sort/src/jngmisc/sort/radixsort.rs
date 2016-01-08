pub fn process_bucket(source_bucket: &mut Vec<u64>,
                      bit_index: u32,
                      new_zeros: &mut Vec<u64>,
                      new_ones: &mut Vec<u64>) {

    for i in 0..source_bucket.len() {
        let number = source_bucket[i];

        match number.rotate_right(bit_index) & 1 {
          0 => new_zeros.push(number),
          _ => new_ones.push(number),
        };
    }

}

pub fn radixsort(mut nums: Vec<u64>) -> Vec<u64> {

    let half_len = nums.len() / 2;
    let mut ones = nums.split_off(half_len);
    let mut zeros = nums;

    for i in 0..16 {
        let mut new_zeros: Vec<u64> = Vec::with_capacity(half_len);
        let mut new_ones: Vec<u64> = Vec::with_capacity(half_len);

        process_bucket(&mut zeros, i, &mut new_zeros, &mut new_ones);
        process_bucket(&mut ones, i, &mut new_zeros, &mut new_ones);
        zeros = new_zeros;
        ones = new_ones;
    }

    zeros.append(&mut ones);
    zeros

}
