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

extern crate palindromes;

type PalFuncPair = (String, palindromes::PalFunc);

const WORD_CNT: usize = 1000;

fn assert_find(funcpair: &PalFuncPair, instr: &str, expstr: &str) -> bool {
    let fname = &funcpair.0;
    let func = &funcpair.1;
    let start = std::time::Instant::now();
    let longest = palindromes::find_longest_pal(func, instr);
    let dur = std::time::Instant::now() - start;
    let success = expstr == longest;
    let result = if success { "PASS" } else { "FAIL" };
    let maxilen = std::cmp::min(24, instr.len());
    let maxelen = std::cmp::min(50, expstr.len());
    let llen = std::cmp::min(10, longest.len());
    println!(
        "{}\t{}\tAsserted >>>{}<<< (found >>>{}<<< length {}) in >>>{} ...<<< {:?}ms",
        &result,
        fname,
        &expstr[0..maxelen],
        //&longest[0..llen],
        longest,
        longest.len(),
        &instr[0..maxilen],
        dur
    );
    //assert!(success);
    success
}

fn sanity_test(funcpair: PalFuncPair) {
    println!("Sanity test ...\n");
    assert_find(&funcpair, "eat a banana bob !", "anana");
    assert_find(&funcpair, "lol", "lol");
    assert_find(&funcpair, "", "");
    assert_find(&funcpair, "A876BC110115438776E0FC16BFF7B24537", "11011");
    // assert_find 49 chars of the first chromosome of the human genome;
    assert_find(
        &funcpair,
        "AATTCTTTGATTGATAATTTTTTCTTCTCAGTCTTTTATCTTGTCTCTTC",
        "TTTTTT",
    );
    assert_find(&funcpair, "TTTTTT", "TTTTTT");
    assert_find(&funcpair, "So my mom and dad said that they would find me a palindrome that is better than a man a plan a canal panama.  I'm not sure that is possible though and don't expect a tattarrattat on my door any time soon. tattarrattattattarrattat", &"tattarrattattattarrattat");
    println!("\n");
}

fn big_test(funcpair: PalFuncPair) {
    println!(
        "Running test to find longest palindromes ({} words) ...\n",
        WORD_CNT
    );

    let paltxt = " amanaplanacanalpanama ".repeat(WORD_CNT);
    println!("\nBig test {} ...\n", WORD_CNT);
    assert_find(&funcpair, &paltxt, &paltxt);
    println!("\n");
}

#[test]
#[ignore]
pub fn sanity_test_naive() {
    sanity_test(("pals_naive".to_string(), Box::new(palindromes::pals_naive)));
}

#[test]
#[ignore]
pub fn big_test_naive() {
    big_test(("pals_naive".to_string(), Box::new(palindromes::pals_naive)));
}

#[test]
pub fn sanity_test_fast() {
    sanity_test(("pals_fast".to_string(), Box::new(palindromes::pals_fast)));
}

#[test]
#[ignore]
pub fn big_test_fast() {
    big_test(("pals_fast".to_string(), Box::new(palindromes::pals_fast)));
}
