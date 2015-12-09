extern crate rand;


fn for_string_rec_aux(word: String,
                      new_word: &mut String,
                      used: &mut Vec<bool>,
                      curr_ind: usize,
                      perms: &mut Vec<String>) {
    if curr_ind == word.len() {
        //perms.push(word.clone());
        println!("{}", new_word);
    } else {
        *used.get_mut(curr_ind).unwrap() = true;
        new_word.char_at(curr_ind) = word.char_at(curr_ind);
        for i in 0..used.len() {
            for_string_rec_aux(word, new_word,
                               used, curr_ind+1, perms);
        }
        *used.get_mut(curr_ind).unwrap() = false;
    }
}

pub fn for_string_rec(word: String) -> Vec<String> {
    let mut perms = Vec::new();
    let mut used = Vec::new();
    let mut new_word = String::from("abcd");
    for _ in 0..word.len() {
        used.push(false);
    }
    for_string_rec_aux(word, &mut new_word,
                       &mut used, 0, &mut perms);
    return perms;
}
