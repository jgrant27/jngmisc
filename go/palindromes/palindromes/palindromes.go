//
// Copyright (c) 2009, Justin Grant <justin at imagine27 dot com>
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


package palindromes


import (
  . "../utils";
//  "fmt";
)




// Finds the lengths of palindromes in a string.
// O(n) time complexity. O(n) space complexity.
func PalindromesFast(text string) (rlens []int) {
  tlen := len(text);
  lengths := make([]int, 0);
  var i int = 0;
  var plen int = 0;

	if tlen > 1 {

		for i < tlen {
			if i > plen && text[i - plen - 1] == text[i] {
				plen += 2;
				i++;
				continue;
			}

			lengths = append(lengths, plen);

			start := len(lengths) - 2;
			end := start - plen;

			if (plen > 0) {
				for j := start;  j > end; j-- {
					ind := j;
					d := ind - end - 1;
					if ind >= 0 && ind < len(lengths) {
						if lengths[ind] == d {
							plen = d;
							break;
						}
						lengths = append(lengths, MinInt(d, lengths[ind]));
					} else {
						plen = 1;
						i++;
					}
				}
			} else {
				plen = 1;
				i++;
			}
		}
    plen = 1;
    i++;

		lengths = append(lengths, plen);

		nlen := len(lengths);
		start := nlen - 2;
		end := start - (2 * tlen + 1 - nlen);
		if (start - end > 0) {
			for j := start;  j > end; j-- {
				ind := j;
				d := ind - end - 1;
				//fmt.Printf("d %d , ind %d\n", d, ind)
				if ind >= 0 && ind < len(lengths) {
					lengths = append(lengths, MinInt(d, lengths[ind]));
				}
			}
		}
	}

  return lengths;
}


// Finds the lengths of palindromes in a string.
// O(n^2) time complexity. O(n) space complexity.
func PalindromesNaive(text string) (rlens []int) {
  tlen := len(text);
	var llen int = 0;
	if tlen > 1 {
		llen = 2 * tlen + 1;
	}
  lengths := make([]int, llen);

	for i := 0;  i < llen; i++ {
		start := i / 2;
		end := start + i % 2;

		for start > 0 && end < tlen && text[start - 1] == text[end] {
			start -= 1;
			end += 1;
			lengths[i] = end - start;
		}

		lengths[i] = end - start;
	}

  return lengths;
}


// Given a slice of palindrome lengths,
// returns the start and end indexes of the longest
// palindrome.
func LongestPalindrome(lens []int) (i1 int, i2 int){
  max := Max(lens);
  im := IndexOf(lens, max);
  s := im / 2 - max / 2;

  return s, s + max;
}
