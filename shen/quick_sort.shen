 \* Copyright (c) 2012, Justin Grant <justin at imagine27 dot com> *\
 \* All rights reserved. *\

 \* Redistribution and use in source and binary forms, with or without modification,  *\
 \* are permitted provided that the following conditions are met: *\

 \* Redistributions of source code must retain the above copyright notice, this list  *\
 \* of conditions and the following disclaimer. *\
 \* Redistributions in binary form must reproduce the above copyright notice, this  *\
 \* list of conditions and the following disclaimer in the documentation and/or  *\
 \* other materials provided with the distribution. *\
 \* Neither the name of the <ORGANIZATION> nor the names of its contributors may be  *\
 \* used to endorse or promote products derived from this software without specific  *\
 \* prior written permission. *\

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


\* A quick sort implementation that takes a list of any type, *\
\* a left and right function each taking two arguments of any type. *\
\* These two functions are used to perform left and right partitioning *\
\* on the list. *\

(tc +)

(define filter
  {(A --> boolean) --> (list A) --> (list A)}
  _  []      -> []
  T? [A | B] -> (append [A] (filter T? B)) where (T? A)
  T? [_ | B] -> (filter T? B))

(define quick-sort-generic
  {(list A) --> (A --> A --> boolean) --> (A --> A --> boolean) --> (list A)}
  [] _ _ -> []
  [A | B] L? R? -> (append (quick-sort-generic (filter (R? A) B) L? R?)
			   [A]
			   (quick-sort-generic (filter (L? A) B) L? R?)))

\* *** NUMBERS *** *\

\* descending with duplicates *\
(quick-sort-generic [3 1 2 7 9 6 6 3 0] <= >)
\* [9 7 6 6 3 3 2 1 0] : (list number) *\

\* ascending with duplicates *\
(quick-sort-generic [3 1 2 7 9 6 6 3 0] >= <)
\* [0 1 2 3 3 6 6 7 9] : (list number)  *\

\* ascending unique numbers *\
(quick-sort-generic [3 1 2 7 9 6 6 3 0] > <)
\* [0 1 2 3 6 7 9] : (list number) *\



\* *** LISTS *** *\

\* descending by length *\
(quick-sort-rec [[2 1 6] [] [1 1 9 0] [0]] 
		(/. X Y (<= (length X) (length Y)))
		(/. X Y (> (length X) (length Y))))
\* [[1 1 9 0] [2 1 6] [0] []] : (list (list number)) *\

\* ascending by length *\
(quick-sort-rec [[2 1 6] [] [1 1 9 0] [0]] 
		(/. X Y (> (length X) (length Y)))
		(/. X Y (<= (length X) (length Y))))
\* [[] [0] [2 1 6] [1 1 9 0]] : (list (list number)) *\
