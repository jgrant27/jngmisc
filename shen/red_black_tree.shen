\**\
\* Copyright (c) 2011, Justin Grant <justin at imagine27 dot com> *\
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

(datatype tree-node
	  Key : number; Val : B;
	  ======================
	  [Key Val] : tree-node;)

(datatype color
	  if (element? Color [red black])
	  _______________________________
	  Color : color;)

(datatype tree
	  if (empty? Tree)
	  ________________
	  Tree : tree;

	  Color : color; LTree : tree; TreeNode : tree-node; RTree : tree;
	  ================================================================
	  [Color LTree TreeNode RTree] : tree;)

(define node-key
    {tree-node --> number}
    [Key Val] -> Key)

(define make-tree-black
    {tree --> tree}
    [Color A X B] -> [black A X B])

(define member 
    {tree-node --> tree --> boolean}
    X NIL -> false
    X [Color A Y B] -> (if (< (node-key X) (node-key Y))
			   (member X A)
			   (if (< (node-key Y) (node-key X))
			       (member X B)
			       true)))

(define balance
    {tree --> tree}
    [black [red [red A X B] Y C] Z D] -> [red [black A X B] Y [black C Z D]]
    [black [red A X [red B Y C]] Z D] -> [red [black A X B] Y [black C Z D]]
    [black A X [red [red B Y C] Z D]] -> [red [black A X B] Y [black C Z D]]
    [black A X [red B Y [red C Z D]]] -> [red [black A X B] Y [black C Z D]]
    S -> S)

(define insert-
    {tree-node --> tree --> tree}
    X [] -> [red [] X []]
    X [Color A Y B] -> (if (< (node-key X) (node-key Y))
			   (balance [Color (insert- X A) Y B])
			   (if (< (node-key Y) (node-key X))
			       (balance [Color A Y (insert- X B)])
			       [Color A Y B])))

(define insert
	{tree-node --> tree --> tree}
	X S -> (make-tree-black (insert- X S)))

(define range 
    { number --> number --> (list number) --> (list number) }
    X Y Z -> (if (> X Y)
                 Z
                 (range X (- Y 1) (cons Y Z))))

(define insert-number-list-into-tree-
    { ((list number) * tree) --> ((list number) * tree) }
    (@p Nums Tree) -> (if (empty? Nums)
                          (@p Nums Tree)
                          (insert-number-list-into-tree-
                           (@p (tail Nums) 
                               (insert [(head Nums) (head Nums)] Tree)))))

(define insert-number-list-into-tree
    { (list number) --> tree --> tree }
    Nums Tree -> (snd (insert-number-list-into-tree-
                       (@p Nums Tree))))


\* test functions *\



(define run-tests
  { A --> (list A) }
    _ -> (let Tree (insert-number-list-into-tree [11 14 15 2 7 1 5 8] [])
	      Count 10000
	      Nums (range 1 Count [])
	   (do  
	    (output "tree: ~A ~%12 is a member ? ~A~%8 is a member ? ~A~%"
		    Tree
		    (member [12 12] Tree) 
		    (member [8 8] Tree))
	    (output "~%Creating tree with ~A elements ..." Count)
	    (let Tree (time (insert-number-list-into-tree Nums []))
	      (do
		(output "~%Performing lookups in tree with ~A elements ...~%" Count)
		(time (output "666 in tree ? ~A" (member [666 666] Tree)))
		(time (output "-1 in tree ? " (member [-1 -1] Tree)))))
	    NIL)))

(run-tests NIL)


\* tree: [black *\
\*        [red [black [red [] [1 1] []] [2 2] [red [] [5 5] []]] [7 7] *\
\*         [black [red [] [8 8] []] [11 11] []]] *\
\*        [14 14] [black [] [15 15] []]]  *\
\* 12 is a member ? false *\
\* 8 is a member ? true *\


\* output *\

\* Creating tree with 100000 elements ... *\
\* Evaluation took: *\
\*   0.600 seconds of real time *\
\*   0.577388 seconds of total run time (0.506373 user, 0.071015 system) *\
\*   [ Run times consist of 0.177 seconds GC time, and 0.401 seconds non-GC time. ] *\
\*   96.17% CPU *\
\*   1,254,956,000 processor cycles *\
\*   168,580,336 bytes consed *\
\*    *\
\*  *\
\* Performing lookups in tree with 100000 elements ... *\
\* 666 in tree ? true *\
\* Evaluation took: *\
\*   0.000 seconds of real time *\
\*   0.000033 seconds of total run time (0.000029 user, 0.000004 system) *\
\*   100.00% CPU *\
\*   64,176 processor cycles *\
\*   0 bytes consed *\
\*    *\
\* -1 in tree ?  *\
\* Evaluation took: *\
