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
\* This code depends on the standard vector library *\
\* which can be found at http://shenlanguage.org/library.html *\
\**\
\* (cd "/home/jgrant") *\
\* (load "vectors.shen") *\
\**\


\* Vector utils *\

(define range-aux
  { number --> number --> (vector number) --> (vector number) }
  X Y Z -> (if (< Y X)
               Z
               (range-aux X (- Y 1) (@v Y Z))))
(define range 
  { number --> number --> (vector number) }
  X Y -> (range-aux X Y <>))

(define vector-init-aux
  { number --> number --> (vector number) --> (vector number) }
  X Y Z -> (if (< Y 1)
               Z
               (vector-init-aux X (- Y 1) (@v X Z))))
(define vector-init
  { number --> number --> (vector number) }
  X Y -> (vector-init-aux X Y <>))


\**\
\* Disjoint set data type (weighted and using path compression) demonstrating  *\
\* (m + n) lg* n worst-case find time *\
\**\

(datatype disjoint-set
  Count : number ; Ids : (vector number) ; Sizes : (vector number);
  =================================================================
  [Count Ids Sizes] : disjoint-set;)

\* Create a new disjoint-set type *\
(define new
  { number --> disjoint-set }
  N -> [N (range 1 N) (vector-init 1 N)])

\* Return the number of disjoint sets *\
(define count
  { disjoint-set --> number }
  [Count Ids Sizes] -> Count)

\* Return id of root object *\
(define find-root
  { disjoint-set --> number --> number }
  [Count Ids Sizes] P -> (let Parent 
			   \* Path Compression *\
			   (<-vector Ids (<-vector Ids P))
			   (if (= P Parent)
			       P
			       (find-root [Count Ids Sizes] Parent))))

\* Are objects P and Q in the set ? *\
(define connected
  { disjoint-set --> number --> number --> boolean }
  UF P Q -> (= (find-root UF P) (find-root UF Q)))

\* Replace sets containing P and Q with their union *\
(define quick-union
  { disjoint-set --> number --> number --> disjoint-set }
  [Count Ids Sizes] P Q 
  -> (let UF      [Count Ids Sizes]
	  I       (find-root UF P)
	  J       (find-root UF Q)
	  SizeI   (<-vector Sizes I)
	  SizeJ   (<-vector Sizes J)
          SizeSum (+ SizeI SizeJ)
	  CIds    (vector-copy Ids)
	  CSizes  (vector-copy Sizes)
       (if (= I J)
	   [Count CIds CSizes]
	   \* Always make smaller root point to larger one *\
	   (do (if (< SizeI SizeJ)
		   (do (vector-> CIds I J) (vector-> CSizes J SizeSum))
		   (do (vector-> CIds J I) (vector-> CSizes I SizeSum)))
	       [(- Count 1) CIds CSizes]))))


\* Tests *\

(define create-unions
  { disjoint-set --> (list (list number)) --> disjoint-set }
  UF []           -> UF
  UF [[P Q] | XS] -> (create-unions (quick-union UF P Q) XS))

(define test
  { number  --> number }
  N -> 
  (let UF (new N)
    (do 
     (output "creating union find with ~A objects ...~%" N)
     (output "DONE~%~A~%" UF)
     (output "All objects are disconnected :~%")
     (output "1 and 9 connected ? ~A~%" (connected UF 1 9))
     (output "4 and 6 connected ? ~A~%" (connected UF 4 6))
     (output "3 and 1 connected ? ~A~%" (connected UF 3 1))
     (output "7 and 8 connected ? ~A~%" (connected UF 7 8))
     (output "... creating unions ... ~%")
     (let Unions [[4 1] [8 2] [7 3] [8 5] [3 4] [5 9] [5 1] [10 4] [6 1]]
	  UFPrime (create-unions UF Unions)
       (time 
	(do
	 (output "DONE~%~A~%" UFPrime)
	 (output "All objects should be connected as there is only 1 group :~%")
	 (output "1 and 9 connected ? ~A~%" (connected UFPrime 1 9))
	 (output "4 and 6 connected ? ~A~%" (connected UFPrime 4 6))
	 (output "3 and 1 connected ? ~A~%" (connected UFPrime 3 1))
	 (output "7 and 8 connected ? ~A~%" (connected UFPrime 7 8)))))
     1)))
       


\**\
\* (132+) (test 10) *\
\* creating union find with 10 objects and creating unions ...  *\
\**\
\* run time: 0.0 secs *\
\* DONE *\
\* [1 <4 8 7 7 8 8 8 8 8 8> <1 1 1 2 1 1 4 10 1 1>] *\
\* All nodes should be connected as there is only 1 group : *\
\* 1 and 9 connected ? true *\
\* 4 and 6 connected ? true *\
\* 3 and 1 connected ? true *\
\* 7 and 8 connected ? true *\
\* 1 : number *\
\**\
