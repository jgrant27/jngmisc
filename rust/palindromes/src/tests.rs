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

#[crate_id(name = "tests", vers = "1.0")];

extern mod palindromes;
extern mod extra;


fn assert_find(funcs:&~[(&~str, extern fn(&~str) -> (uint, uint))], 
               instr:&~str, expstr:&~str) -> bool {
  funcs.iter().fold(true, |res, pair:&(&~str, extern fn(&~str) -> (uint, uint))| {
    let fname = pair.first();
    let func = pair.second();
    let start = extra::time::precise_time_ns();
    let longest = palindromes::find_longest_pal(func, instr);
    let dur = (extra::time::precise_time_ns() - start) as f64 / 1000000.0;
    let success = expstr.eq(&longest);
    let result = if success { "PASS" } else { "FAIL" };
    let maxilen = std::num::min(300, instr.len()); 
    let maxelen = std::num::min(10, expstr.len()); 
    std::io::println(format!("{:s}\t{:s}\tAsserted >>>{:s}<<< (length {:u}) in >>>{:s} ...<<< {:.9f}ms",
                     result, *fname, expstr.slice(0, maxelen), longest.len(),
                     instr.slice(0, maxilen), dur));
    res && success
  })
}

pub fn sanity_tests() {
  std::io::println(format!("Sanity tests ...\n"));

  let funcs = ~[(&~"pals_naive", palindromes::pals_naive), 
                (&~"pals_fast", palindromes::pals_fast)];
  
  assert_find(&funcs, &~"eat a banana bob !", &~"anana");
  assert_find(&funcs, &~"lol", &~"lol");
  assert_find(&funcs, &~"", &~"");
  assert_find(&funcs, &~"A876BC110115438776E0FC16BFF7B24537", &~"11011");
  // assert_find 49 chars of the first chromosome of the human genome;
  assert_find(&funcs, &~"AATTCTTTGATTGATAATTTTTTCTTCTCAGTCTTTTATCTTGTCTCTTC", &~"TTTTTT");
  assert_find(&funcs, &~"TTTTTT", &~"TTTTTT");
  assert_find(&funcs, &~"So my mom and dad said that they would find me a palindrome that is better than a man a plan a canal panama.  I'm not sure that is possible though and don't expect a tattarrattat on my door any time soon. tattarrattattattarrattat", &~"tattarrattattattarrattat");
  println("\n");
}

pub fn big_tests(wordcnt:uint) {
  std::io::println(format!("Running tests to find longest palindromes ({:u} words) ...\n", wordcnt));

  let funcs = ~[(&~"pals_naive", palindromes::pals_naive), 
                (&~"pals_fast", palindromes::pals_fast)];

  let paltxt = " amanaplanacanalpanama ".repeat(wordcnt);
  std::io::println(format!("\nBig tests {:u} ...\n", wordcnt));
  assert_find(&funcs, &paltxt, &paltxt);
  println("\n");
}