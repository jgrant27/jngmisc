//
// Copyright (c) 2013, Justin Grant <justin at imagine27 dot com>
// All rights reserved.

// Redistribution and use in source and binary forms, with or without modification,
// are permitted provided that the following conditions are met:

// Redistributions of source code must retain the above copyright notice, this list
// of conditions and the following disclaimer.
// Redistributions in binary form must reproduce the above copyright notice, this
// list of conditions and the following disclaimer in the documentation and/or
// other materials provided with the distribution.
// Neither the name of the <ORGANIZATION> nor the names of its contributors may be
// used to endorse or promote products derived from this software without specific
// prior written permission.
//
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
// ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
// WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
// DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR
// ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
// (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
// LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
// THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
// NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE,
// EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
//

pub type PalFunc = Box<dyn Fn(&str) -> (usize, usize)>;

fn start_end(lengths: &[usize]) -> (usize, usize) {
    // Given a slice of palindrome lengths,
    // returns the start and end indexes of the longest
    // palindrome.
    let mut max = 0;
    let mut im = 0;
    if !lengths.is_empty() {
        max = *lengths.iter().max().unwrap();
        im = lengths.iter().position(|&n| n == max).unwrap();
    };
    //println!("{:?} {} {}", lengths, im, max);
    let s = std::cmp::max(0, im / 2 - max / 2);
    (s, s + max)
}

// Finds the lengths of palindromes in a string.
// O(n^2) time complexity. O(n) space complexity.
pub fn pals_naive(text: &str) -> (usize, usize) {
    let tlen: usize = text.len();
    let llen: usize = if tlen > 1 { 2 * tlen + 1 } else { 0 };
    let mut lengths = vec![0; llen];
    let mut i: usize = 0;
    while i < llen {
        let mut start: usize = i / 2;
        let mut end: usize = start + i % 2;
        while start > 0 && end < tlen && &text[start - 1..start] == &text[end..end + 1] {
            start -= 1;
            end += 1;
            lengths[i] = end - start;
        }
        lengths[i] = end - start;
        i += 1;
    }
    start_end(&lengths)
}

// Finds the lengths of palindromes in a string.
// O(n) time complexity. O(n) space complexity.
pub fn pals_fast(text: &str) -> (usize, usize) {
    let mut lengths = vec![0; 2 * text.len() + 1];
    let mut d;
    let mut s;
    let mut j;
    let mut e;
    let mut plen = 0;
    let mut k = 0;
    let mut i = 0;

    while i < text.len() {
        // is the string so far a palindrome ?
        if i > plen && text[i - plen - 1..i - plen] == text[i..i + 1] {
            // yes, so we extend the current found palindrome length
            // and keep checking forward else ...
            plen += 2;
            i += 1;
            continue;
        }
        // ... we have reached the end of the current found palindrome
        // so we set the current palindrome length etc. ...
        lengths[k] = plen;
        k += 1;
        s = if k < 2 { 0 } else { k - 2 };
        e = if s < plen { plen } else { s - plen };
        // ... then backtrack over the last found palindrome to see ...
        let mut b = true;
        j = s;
        while j > e {
            j -= 1;
            d = j - e;
            // ... if we have a reflection of the same length ...
            if lengths[j] == d {
                // ... yes, so we set new palindrome length ...
                plen = d;
                // ... and stop backtracking.
                b = false;
                break;
            }
            // ... no, so we take the smaller length,
            // update the lengths and continue backtracking.
            lengths[k] = std::cmp::min(d, lengths[j]);
            k += 1;
        }
        // if we are back-tracking then reset palindrome length
        // and move on.
        if b {
            plen = 1;
            i += 1;
        }
    }
    // we're done scanning the text so we perform
    // one last backtrack.
    lengths[k] = plen;
    k += 1;
    s = if k < 2 { 0 } else { k - 2 };
    e = if s < (2 * text.len() + 1 - k) {
        s
    } else {
        s - (2 * text.len() + 1 - k)
    };
    let mut i = s;
    while i > e {
        d = i - e - 1;
        lengths[k] = std::cmp::min(d, lengths[i]);
        k += 1;
        i -= 1;
    }
    start_end(&lengths)
}

pub fn find_longest_pal(func: &PalFunc, text: &str) -> String {
    let (i1, i2) = func(text);
    if i1 < i2 && text.len() > 0 {
        text[i1..i2].to_string()
    } else {
        String::new()
    }
}
