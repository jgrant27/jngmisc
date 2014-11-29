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


(tc +)


\**\
\* Selection Sort *\
\**\


(define remove-first-aux
  { boolean --> number --> (list number) --> (list number) --> (list number) }
  _       _   Clean []     -> (reverse Clean)
  Removed Num Clean [X|XS] -> (let NewRemoved (if (= X Num) true Removed)
				   NewClean (if (and (= X Num) (not Removed)) Clean (cons X Clean))
				    (remove-first-aux NewRemoved Num NewClean XS)))
(define remove-first
  { number --> (list number) --> (list number) }
  Num Lst -> (remove-first-aux false Num [] Lst))

(define minimum-aux
  { number --> (list number) --> number }
  Min []     -> Min
  Min [X|XS] -> (let Min (if (< X Min ) X Min) (minimum-aux Min XS)))
(define minimum
  { (list number) --> number }
  [X|XS] -> (minimum-aux X XS))

(define selection-sort-aux
  { (list number) --> (list number) --> (list number) }
  Sorted []       -> (reverse Sorted)
  Sorted Unsorted -> (let Min (minimum Unsorted)
			  (selection-sort-aux (cons Min Sorted) (remove-first Min Unsorted))))
(define selection-sort
  { (list number) --> (list number) }
  Unsorted -> (selection-sort-aux [] Unsorted))


\* Tests *\

(define test
  { number  --> number }
  N ->
  (let Unsorted [8 3 4 1 2 5 7 6 0 9]
       Sorted   (selection-sort Unsorted)
    (do
     (output "unsorted list ... ~A~%" Unsorted)
     (output "sorted list ... ~A~%" Sorted)
     1)))



\* (19+) (test 1) *\
\* unsorted list ... [8 3 4 1 2 5 7 6 0 9] *\
\* sorted list ... [0 1 2 3 4 5 6 7 8 9] *\
\* 1 : number *\
