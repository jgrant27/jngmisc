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


package main

import (
  "fmt";
  "os";
  "io";
  "bytes";
  . "./palindromes";
)

func main() {
  text := bytes.NewBufferString("");
  io.Copyn(text, os.Stdin, 1024 * 1024 * 512); // copies up to 512MB of input
  strn := text.String();
  lens1 := PalindromesNaive(strn);
  //lens2 := PalindromesFast(strn);
	//PalindromesNaive(strn);
	//PalindromesFast(strn);
  //fmt.Printf("naive : %s\n", lens1);
  //fmt.Printf("fast  : %s\n", lens2);
  i1, i2 := LongestPalindrome(lens1);
  longest := strn[i1:i2];
  fmt.Printf("%d lines of '%s'\n", -1, "line");
  //fmt.Printf("longest : '%s ...'\n", longest[0:22]);
  fmt.Printf("longest : '%s'\n", longest);
}
