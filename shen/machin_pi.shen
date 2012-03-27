\**\
\* Copyright (c) 2012, Justin Grant <justin at imagine27 dot com> *\
\* All rights reserved. *\
\**\
\* Redistribution and use in source and binary forms, with or without modification,  *\
\* are permitted provided that the following conditions are met: *\
\**\
\* Redistributions of source code must retain the above copyright notice, this list  *\
\* of conditions and the following disclaimer. *\
\* Redistributions in binary form must reproduce the above copyright notice, this  *\
\* list of conditions and the following disclaimer in the documentation and/or  *\
\* other materials provided with the distribution. *\
\* Neither the name of the <ORGANIZATION> nor the names of its contributors may be  *\
\* used to endorse or promote products derived from this software without specific  *\
\* prior written permission. *\
\**\
\* THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND  *\
\* ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED *\
\* WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE  *\
\* DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR  *\
\* ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES  *\
\* (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; *\
\* LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY  *\
\* THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING  *\
\* NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE,  *\
\* EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. *\
\**\

\*
  Accurately calculates N digits of Pi using Machin's formula
  with fixed point arithmetic and variable guards digits. 

  Depends on the maths library -->
    http://www.shenlanguage.org/library.html
*\

(tc +)

(define arccot-
  {number --> number --> number --> number --> number --> number} 
  X N XPOWER    0 _ -> 0
  X N XPOWER TERM 1 -> (+ (arccot- X (+ N 2) (floor (/ XPOWER X)) 
                                     (floor (/ XPOWER N)) 0) 
                          (floor (/ XPOWER N)))
  X N XPOWER TERM 0 -> (- (arccot- X (+ N 2) (floor (/ XPOWER X))
                                      (floor (/ XPOWER N)) 1) 
                          (floor (/ XPOWER N))))

(define arccot
  {number --> number --> number}
  X UNITY -> (let XPOWER (floor (/ UNITY X))
                  (arccot- (* X X) 1 XPOWER (floor XPOWER) 1)))

(define machin-pi
  {number --> number} 
  DIGITS -> (let GUARD (+ 10 (ceiling (log' DIGITS 10)))
                 UNITY (expt 10 (+ DIGITS GUARD))
                 (floor (/ (* 4 (- (* 4 (arccot 5 UNITY))
                                   (arccot 239 UNITY)))
                           (expt 10 GUARD)))))



\*
(1+) (time (machin-pi 100))

run time: 2.56899999999996 secs
31415926535...4350265344N : number
*\