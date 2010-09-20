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


;;;; This demonstrates tree-traversal(represented as lists) in Common Lisp.


;; naive traversal functions
(defun traverse-pre-order (node fun)
  (when node
    (apply fun (list (car node)))
    (when (second node)
      (traverse-pre-order (second node) fun))
    (when (third node)
      (traverse-pre-order (third node) fun))))

(defun traverse-in-order (node fun)
  (when node
    (when (second node)
      (traverse-in-order (second node) fun))
    (apply fun (list (car node)))
    (when (third node)
      (traverse-in-order (third node) fun))))

(defun traverse-post-order (node fun)
  (when node
    (when (second node)
      (traverse-post-order (second node) fun))
    (when (third node)
      (traverse-post-order (third node) fun))
    (apply fun (list (car node)))))



;; a better traversal function that shows how easily
;; the traversal order can be parameterized instead
;; of having to write a new function for each order.

(defun traverse-tree (node fun order)
  (map 'list #'(lambda (f)
                 (let ((n (if f
                              (apply f (list node)) 
                              node)))
                   (if (equal node n)
                       (apply fun (list (car node)))
                       (when n
                         (traverse-tree n fun order)))))
       order))


(defun print-node (n) (format t "~A " n))

(defun test()
    (let ((root '(5 (3 (2 (1)) (4)) (7 (6) (8 nil (9))))))      

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
           '((pre-order          nil second third)
             (in-order           second nil third)    
             (post-order         second third nil)    
             (reverse-pre-order  nil third second)    
             (reverse-in-order   third nil second)    
             (reverse-post-order third second nil)))
      )
    nil)

(test)
