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
  . "../utils"
  //"fmt"
)

func LargestStartEnd(lengths []int) (int, int) {
  // Given a slice of palindrome lengths,
  // returns the start and end indexes of the longest
  // palindrome.
  max := Max(lengths)
  im := IndexOf(lengths, max)
  s := MaxInt(0, im/2-max/2)
  return s, s + max
}

// Finds the lengths of palindromes in a string.
// O(n) time complexity. O(n) space complexity.
func PalindromesFast(text string) (int, int) {
  lengths := make([]int, 2*len(text)+1)
  i, j, d, s, e, plen, llen, k := 0, 0, 0, 0, 0, 0, 0, 0
  for i < len(text) {
		// is the string so far a palindrome ?
    if i > plen && text[i-plen-1] == text[i] {
			// yes, so we extend the current found palindrome length
			// and keep checking forward else ...
      plen += 2
      i++
      continue
    }
		// ... we have reached the end of the current found palindrome
		// so we set the current palindrome length etc. ...
    lengths[k] = plen
    k++
    s = k - 2
    e = s - plen
		// ... then backtrack over the last found palindrome to see ...
    b := true
    for j = s; j > e; j-- {
      d = j - e - 1
			// ... if we have a reflection of the same length ...
      if lengths[j] == d {
				// ... yes, so we set new palindrome length ...
        plen = d
				// ... and stop backtracking.
        b = false
        break
      }
			// ... no, so we take the smaller length,
			// update the lengths and continue backtracking.
      lengths[k] = MinInt(d, lengths[j])
      k++
    }
		// if we we're backtracking then reset palindrome length
		// and move on.
    if b {
      plen = 1
      i++
    }
  }
	// we're done scanning the text so we perform 
	// one last backtrack.
  lengths[k] = plen
  k++
  llen = k
  s = llen - 2
  e = s - (2*len(text) + 1 - llen)
  for i := s; i > e; i-- {
    d = i - e - 1
    lengths[k] = MinInt(d, lengths[i])
    k++
  }
  return LargestStartEnd(lengths)
}

// Finds the lengths of palindromes in a string.
// O(n^2) time complexity. O(n) space complexity.
func PalindromesNaive(text string) (int, int) {
  llen, tlen := 0, len(text)
  if tlen > 1 {
    llen = 2*tlen + 1
  }
  lengths := make([]int, llen)
  for i := 0; i < llen; i++ {
    start := i / 2
    end := start + i%2
    for start > 0 && end < tlen && text[start-1] == text[end] {
      start -= 1
			end += 1
      lengths[i] = end - start
    }
    lengths[i] = end - start
  }
  return LargestStartEnd(lengths)
}
