extern crate rand;


fn for_string_rec_aux(word: &String,
                      new_word: String,
                      used: &mut Vec<bool>,
                      perms: &mut Vec<String>) {
    if new_word.len() == word.len() {
        perms.push(new_word);
    } else {
        for i in 0..used.len() {
            if *used.get_mut(i).unwrap() { continue; }

            let mut newer_word = new_word.clone();
            newer_word.push(word.chars().nth(i).unwrap());
            *used.get_mut(i).unwrap() = true;
            for_string_rec_aux(word, newer_word, used, perms);
            *used.get_mut(i).unwrap() = false;
        }
    }
}

pub fn for_string_rec(word: String) -> Vec<String> {
    let mut perms = Vec::new();
    let mut used = Vec::new();
    let new_word = String::new();
    for _ in 0..word.len() { used.push(false); }
    for_string_rec_aux(&word, new_word, &mut used, &mut perms);
    return perms;
}
