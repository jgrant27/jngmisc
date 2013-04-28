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

package tests

import (
  . "../palindromes"
  . "../utils"
  "fmt"
  "strings"
  "time"
)

func PrintDuration(f func()) {
  start := time.Now()
  f()
  end := time.Now()
  fmt.Printf("%v elapsed\n", end.Sub(start))
}

func AssertFind(funcs [](func(string) (int, int)), instr string, expstr string) {
	for i:=0; i < len(funcs); i++ {
		f := funcs[i]
		fname := GetFunctionName(f)
		longest := FindLongestPal(f, instr)
		result := "FAIL"
		if longest == expstr {
			result = "PASS"
		}
		fmt.Printf(
			"%s\t(%s)\tAsserted >>>%s<<< in >>>%s ...<<< \tlength %d\n",
			result, fname, 
			expstr[0:MinInt(len(expstr), 50)],
			instr[0:MinInt(len(instr), 24)], 
			len(longest))
	}
}

func SanityTests() {
	funcs := [](func(string) (int, int)) {PalindromesFast, PalindromesNaive}
  fmt.Printf("\nSanity tests ...\n")
  AssertFind(funcs, "eat a banana bob !", "anana")
  AssertFind(funcs, "lol", "lol")
  AssertFind(funcs, "", "")
  AssertFind(funcs, "A876BC110115438776E0FC16BFF7B24537", "11011")
   // AssertFind 49 chars of the first chromosome of the human genome
  AssertFind(funcs, "AATTCTTTGATTGATAATTTTTTCTTCTCAGTCTTTTATCTTGTCTCTTC", "TTTTTT")
  AssertFind(funcs, "TTTTTT", "TTTTTT")
  AssertFind(funcs, "So my mom and dad said that they would find me a palindrome that is better than a man a plan a canal panama.  I'm not sure that is possible though and don't expect a tattarrattat on my door any time soon. tattarrattattattarrattat", "tattarrattattattarrattat")
}

func BigTest(wordcnt int) {
	funcs := [](func(string) (int, int)) {PalindromesFast, PalindromesNaive}
  paltxt := strings.Repeat(" amanaplanacanalpanama ", wordcnt)
  fmt.Printf("\nBig tests (%d) ...\n", wordcnt)
  PrintDuration(func() { AssertFind(funcs, paltxt, paltxt) })
}
