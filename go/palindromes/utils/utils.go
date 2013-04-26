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


package utils


import (
)



// Util functions


// Given two integers returns the smallest
func MinInt(i1 int, i2 int) (mi int) {
  var val int = i2;
  if i1 < i2 { val = i1; }

  return val;
}

// Returns the smallest value in a slice of integers
func Min(sl []int) (mv int) {
  var vi = -1;
  for i := 0; i < len(sl); i++ {
    if (i == 0 || sl[i] < vi) { vi = sl[i]; }
  }
  return vi;
}

// Returns the largest value in a slice of integers
func Max(sl []int) (mv int) {
  var vi = -1;
  for i := 0; i < len(sl); i++ {
    if (sl[i] > vi) { vi = sl[i]; }
  }
  return vi;
}

// Given a slice of integers and an integer, returns the index
// of the integer in the slice.
func IndexOf(sl []int, si int) (rind int) {
  var ind int = -1;
  for i := 0; i < len(sl); i++ {
    if (sl[i] == si) { ind = i; break; }
  }
  return ind;
}

func Abs(num int) (rnum int) {
  if (num < 0) { return (-1 * num); }
  return num;
}

