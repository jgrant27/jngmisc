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
  "fmt";
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

// This implementation of range sucks.
func RangeFor(start int, end int, step int) (ra []int) {
  var val []int;
  var i int = 0;

  if (start < end) {
    if (step < 1) { return val; }
    if (step > end - start) {
      val = make([]int, 1);
      val[0] = start;
      return val;
    }
    size := (end - start) / step;
    if (end - start) % step > 0 { size++; }
    if (end - start >= 1) {
      val = make([]int, size);
      for j := start; j < end; j += step {
        val[i] = j;
        i++;
      }
    }
  } else if (start > end) {
    if (step > -1) { return val; }
    if (Abs(step) > start - end) {
      val = make([]int, 1);
      val[0] = start;
      return val;
    }
    size := (start - end) / Abs(step);
    if (start - end >= 1) {
      val = make([]int, size);
      for j := start; j > end; j += step {
        val[i] = j;
        i++;
      }
    }
  }

  return val;
}


func RangeTest() {

  fmt.Printf("\n\nUp\n");
  // Up
  rng := RangeFor(0, 7, 3);
  fmt.Printf("%s\n", rng);
  rng2 := RangeFor(1, 7, 3);
  fmt.Printf("%s\n", rng2);
  rng3 := RangeFor(2, -3, -1);
  fmt.Printf("%s\n", rng3);
  rng4 := RangeFor(-4, 5, 2);
  fmt.Printf("%s\n", rng4);
  rng6 := RangeFor(1, 2, 4);
  fmt.Printf("%s\n", rng6);
  rng7 := RangeFor(-2, -1, 5);
  fmt.Printf("%s\n", rng7);
  rng10 := RangeFor(0, 1, 4);
  fmt.Printf("%s\n", rng10);

  fmt.Printf("\n\nDown\n");
  // Down
  rng5 := RangeFor(2, 1, -5);
  fmt.Printf("%s\n", rng5);
  rng8 := RangeFor(-1, -2, -4);
  fmt.Printf("%s\n", rng8);
  rng9 := RangeFor(1, 0, -5);
  fmt.Printf("%s\n", rng9);
  rng11 := RangeFor(1, -3, -1);
  fmt.Printf("%s\n", rng11);
  rng12 := RangeFor(5, 2, -1);
  fmt.Printf("%s\n", rng12);
  rng13 := RangeFor(0, -2, -1);
  fmt.Printf("%s\n", rng13);
  rng14 := RangeFor(1, -1, -1);
  fmt.Printf("%s\n", rng14);

}

