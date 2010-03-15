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


;;;; This demonstrates tree-traversal in Common Lisp.


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


      (format t "~%simple functions :~%")
      (map 'list #'(lambda (fun)
                     (let* ((fname (string fun))
                            (pos (1+ (position #\- fname)))
                            (order (subseq fname pos)))
                       (format t "~20A : " order))
                     (apply fun (list root #'print-node))
                     (format t "~%"))
           '(traverse-pre-order traverse-in-order traverse-post-order))
      (format t "~%")

      (format t "~%traverse-tree function : ~%")
      (map 'list #'(lambda (order)
                     (format t "~20A : " (car order))
                     (traverse-tree root #'print-node (cdr order))
                     (format t "~%"))
           '((pre-order          nil btnode-left btnode-right)
             (in-order           btnode-left nil btnode-right)    
             (post-order         btnode-left btnode-right nil)    
             (reverse-pre-order  nil btnode-right btnode-left)    
             (reverse-in-order   btnode-right nil btnode-left)    
             (reverse-post-order btnode-right btnode-left nil)))
      
      )
    )

(test)
