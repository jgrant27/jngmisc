;;
;; Copyright (c) 2010, Justin Grant <justin at imagine27 dot com>
;; All rights reserved.

;; Redistribution and use in source and binary forms, with or without modification,
;; are permitted provided that the following conditions are met:

;; Redistributions of source code must retain the above copyright notice, this list
;; of conditions and the following disclaimer.
;; Redistributions in binary form must reproduce the above copyright notice, this
;; list of conditions and the following disclaimer in the documentation and/or
;; other materials provided with the distribution.
;; Neither the name of the <ORGANIZATION> nor the names of its contributors may be
;; used to endorse or promote products derived from this software without specific
;; prior written permission.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
;; ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
;; DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR
;; ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
;; (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
;; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE,
;; EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;


;;;; Demonstrates binary tree-traversal in Common Lisp.

;; #S(BTNODE
;;    :VAL 5
;;    :LEFT #S(BTNODE
;;             :VAL 3
;;             :LEFT #S(BTNODE
;;                      :VAL 2
;;                      :LEFT #S(BTNODE :VAL 1 :LEFT NIL :RIGHT NIL)
;;                      :RIGHT NIL)
;;             :RIGHT #S(BTNODE :VAL 4 :LEFT NIL :RIGHT NIL))
;;    :RIGHT #S(BTNODE
;;              :VAL 7
;;              :LEFT #S(BTNODE :VAL 6 :LEFT NIL :RIGHT NIL)
;;              :RIGHT #S(BTNODE
;;                        :VAL 8
;;                        :LEFT NIL
;;                        :RIGHT #S(BTNODE :VAL 9 :LEFT NIL :RIGHT NIL))))

;; naive functions :
;; PRE-ORDER                   : 5 3 2 1 4 7 6 8 9
;; IN-ORDER                    : 1 2 3 4 5 6 7 8 9
;; POST-ORDER                  : 1 2 4 3 6 9 8 7 5

;; traverse-tree function :
;; PRE-ORDER                   : 5 3 2 1 4 7 6 8 9
;; IN-ORDER                    : 1 2 3 4 5 6 7 8 9
;; POST-ORDER                  : 1 2 4 3 6 9 8 7 5
;; REVERSE-PRE-ORDER           : 5 7 8 9 6 3 4 2 1
;; REVERSE-IN-ORDER            : 9 8 7 6 5 4 3 2 1
;; REVERSE-POST-ORDER          : 9 8 6 7 4 1 2 3 5

;; level order function :
;; TOP-TO-BOTTOM LEFT-TO-RIGHT : 5 3 7 2 4 6 8 1 9
;; TOP-TO-BOTTOM RIGHT-TO-LEFT : 5 7 3 8 6 4 2 9 1
;; BOTTOM-TO-TOP LEFT-TO-RIGHT : 1 9 2 4 6 8 3 7 5
;; BOTTOM-TO-TOP RIGHT-TO-LEFT : 9 1 8 6 4 2 7 3 5

;; level order function recursive :
;; TOP-TO-BOTTOM LEFT-TO-RIGHT : 5 3 7 2 4 6 8 1 9
;; TOP-TO-BOTTOM RIGHT-TO-LEFT : 5 7 3 8 6 4 2 9 1
;; BOTTOM-TO-TOP LEFT-TO-RIGHT : 1 9 2 4 6 8 3 7 5
;; BOTTOM-TO-TOP RIGHT-TO-LEFT : 9 1 8 6 4 2 7 3 5

;; level order function (odd only) :
;; TOP-TO-BOTTOM LEFT-TO-RIGHT : 5 3 7 1 9
;; TOP-TO-BOTTOM RIGHT-TO-LEFT : 5 7 3 9 1
;; BOTTOM-TO-TOP LEFT-TO-RIGHT : 1 9 3 7 5
;; BOTTOM-TO-TOP RIGHT-TO-LEFT : 9 1 7 3 5

;; level order function (leaves only) :
;; TOP-TO-BOTTOM LEFT-TO-RIGHT : 4 6 1 9
;; TOP-TO-BOTTOM RIGHT-TO-LEFT : 6 4 9 1
;; BOTTOM-TO-TOP LEFT-TO-RIGHT : 1 9 4 6
;; BOTTOM-TO-TOP RIGHT-TO-LEFT : 9 1 6 4


;; A struct representing a node in a binary tree
(defstruct (btnode)
  (val nil)
  (left nil)
  (right nil))

;; naive traversal functions
(defun traverse-pre-order (node fun)
  (if node
      (progn
        (apply fun (list node))
        (if (btnode-left node)
            (traverse-pre-order (btnode-left node) fun))
        (if (btnode-right node)
            (traverse-pre-order (btnode-right node) fun)))))

(defun traverse-in-order (node fun)
  (if node
      (progn
        (if (btnode-left node)
            (traverse-in-order (btnode-left node) fun))
        (apply fun (list node))
        (if (btnode-right node)
            (traverse-in-order (btnode-right node) fun)))))

(defun traverse-post-order (node fun)
  (if node
      (progn
        (if (btnode-left node)
            (traverse-post-order (btnode-left node) fun))
        (if (btnode-right node)
            (traverse-post-order (btnode-right node) fun))
        (apply fun (list node)))))


;; a better traversal function that shows how easily
;; the traversal order can be parameterized instead
;; of having to write a new function for each order.

(defun traverse-tree (node fun order)
  (map 'list #'(lambda (f)
                 (let ((n (if f
                              (apply f (list node))
                              node)))
                   (if (eq node n)
                       (apply fun (list node))
                       (when n
                         (traverse-tree n fun order)))))
       order))

;;
;; level-order/breadth-first traversal
;;
(defmacro with-breadth-first-traversal (&body body)
  `(macrolet ((bft-init ((&optional &key test) &body body)
             `(let* ((res   (list node))
                     (queue (list node))
                     (btot  (eql vorder 'bottom-to-top))
                     (rtol  (eql horder 'right-to-left))
                     (rtol  (if btot (not rtol) rtol)))
                ,@body))
           (bft-loop-init (&body body)
              `(let* ((node     (car queue))
                      (lcnode   (btnode-left  node))
                      (rcnode   (btnode-right node))
                      (children (when rcnode
                                  (cons rcnode nil)))
                      (children (if lcnode
                                    (cons lcnode children) children))
                      (children (if rtol (reverse children) children))
                      (nqueue   (append (rest queue) children))
                      (nres     (append res children)))
                 ,@body)))
     ,@body))

;; Define callable variations of breadth first traversal functions
(with-breadth-first-traversal

  (defun traverse-tree-by-level
      (node fun &optional horder vorder)
    (bft-init ()
          (loop while queue do
               (bft-loop-init
                  (setf queue nqueue)
                  (setf res   nres)))
          (mapcar fun (if btot (reverse res) res))))

  (defmacro traverse-tree-by-level-recursive-m
      (node fun &optional horder vorder &key test)
    (declare (ignore node fun horder vorder))
    `(bft-init (:test ,test)
           (labels ((traverse (res queue)
                      (if (not queue)
                          (if ,test (delete-if-not ,test res) res)
                          (bft-loop-init (traverse nres nqueue)))))
             (let ((res (traverse res queue)))
               (mapcar fun (if btot (reverse res) res))))))

  (defun traverse-tree-by-level-recursive
      (node fun &optional horder vorder)
    (traverse-tree-by-level-recursive-m node fun horder vorder))

  (defun traverse-tree-by-level-only-odd
      (node fun &optional horder vorder)
    (traverse-tree-by-level-recursive-m
     node fun horder vorder
     :test #'(lambda (node) (and node (oddp (btnode-val node))))))

  (defun traverse-tree-by-level-only-leaves
      (node fun &optional horder vorder)
    (traverse-tree-by-level-recursive-m
     node fun horder vorder
     :test #'(lambda (node)
               (and node (not (or (btnode-left node)
                                  (btnode-right node)))))))
  
  )


;;utils

(defun print-node (n) (format t "~A " (btnode-val n)))

(defun test()
  (let ((root
         (make-btnode
          :val 5
          :left (make-btnode
                 :val 3
                 :left (make-btnode :val 2 :left (make-btnode :val 1))
                 :right (make-btnode :val 4))
          :right (make-btnode
                  :val 7
                  :left (make-btnode :val 6)
                  :right (make-btnode :val 8 :right (make-btnode :val 9))))))

    (format t "~A~%" root)

    (format t "~%naive functions :~%")
    (map 'list #'(lambda (fun)
                   (let* ((fname (string fun))
                          (pos (1+ (position #\- fname)))
                          (order (subseq fname pos)))
                     (format t "~27A : " order))
                   (apply fun (list root #'print-node))
                   (format t "~%"))
         '(traverse-pre-order traverse-in-order traverse-post-order))
    (format t "~%")

    (format t "traverse-tree function : ~%")
    (map 'list #'(lambda (order)
                   (format t "~27A : " (car order))
                   (traverse-tree root #'print-node (cdr order))
                   (format t "~%"))
         '((pre-order          nil btnode-left btnode-right)
           (in-order           btnode-left nil btnode-right)
           (post-order         btnode-left btnode-right nil)
           (reverse-pre-order  nil btnode-right btnode-left)
           (reverse-in-order   btnode-right nil btnode-left)
           (reverse-post-order btnode-right btnode-left nil)))
    (format t "~%")

    (defun run-bfs-test-variations (variations)
      (let ((orders (mapcar #'(lambda (x)
                                (mapcar #'(lambda (y) (cons x y))
                                        '(left-to-right right-to-left)))
                            '(top-to-bottom bottom-to-top))))
        (mapcar #'(lambda (test)
                    (format t (car test))
                    (map 'list #'(lambda (fnames)
                                   (format t "~13A ~A : " (car fnames) (cdr fnames))
                                   (apply (cadr test) (list root #'print-node
                                                            (cdr fnames) (car fnames)))
                                   (format t "~%"))
                         (apply #'append orders))
                    (format t "~%"))
                variations)))
    
    (run-bfs-test-variations
     '(("level order function : ~%"               traverse-tree-by-level)
       ("level order function recursive : ~%"     traverse-tree-by-level-recursive)
       ("level order function (odd only) : ~%"    traverse-tree-by-level-only-odd)
       ("level order function (leaves only) : ~%" traverse-tree-by-level-only-leaves)))
    
    ))
