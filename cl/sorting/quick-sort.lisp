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


(proclaim '(optimize 
            (speed 0) (compilation-speed 0) (safety 3) (debug 3)))

;; alternative
(defun quick-sort-generic (sequence cfun &optional result-type)
  "Picks the pivot at random and therefore avoids the edge-case
   where the list is already sorted and the performance degrades to O(n^2)."
  (if (<= (length sequence) 1)
      (copy-seq sequence)
      (flet ((partition (fun rem-seq)
	       (list (remove-if-not fun rem-seq) (remove-if fun rem-seq))))
	(let* ((result-type (or result-type 'vector))
	       (pivot-ind (random (length sequence)))
	       (pivot-val (elt sequence pivot-ind))
	       (rem-seq
		(remove pivot-val sequence :start pivot-ind :end (+ 1 pivot-ind)))
	       (part (partition (lambda (x) 
				  (apply cfun (list x pivot-val))) rem-seq)))
	  (concatenate result-type
		       (quick-sort-generic (car part) cfun result-type) 
		       (list pivot-val)
		       (quick-sort-generic (cadr part) cfun result-type))))))


;; test functions

(defmacro do-sort (fun args type cnt)
  `(progn
     (format t "~%started quick-sort (~A) ...~%" ,type)
     (finish-output)
     (let ((sub (if (>= ,cnt 10) 10 ,cnt))
	   #-:ECL-READ-WRITE-LOCK
	   (snums (time (apply ,fun ,args)))
           #+:ECL-READ-WRITE-LOCK
	   (snums (apply ,fun ,args)))
       (format t "quick-sorted ~A items (first ~A shown) : ~%~A ~%~%~%" 
	       (length snums) sub (subseq snums 0 sub)))))

;; tests
(defun test-sort()
  (setf *random-state* (make-random-state t))
  (let* ((cnt (floor 1E4))
	 (rnumsa (make-array cnt))
	 (rnumsl (list (random (1+ cnt))))
	 (rstr (list (+ 64 (random 27)))))
    (dotimes (n cnt) (setf (svref rnumsa n) (random (1+ cnt))))
    (dotimes (n (1- cnt)) (nconc rnumsl (list (random (1+ cnt)))))
    ;; letters only
    (dotimes (n (1- cnt)) (nconc rstr (list (+ 64 (random 27)))))
    
    ;; Sort numbers in descending order.
    (time
    (do-sort 'quick-sort-generic 
      (list rnumsa #'>) "generic, array" cnt))

    ;; Sort numbers in descending order (list).    
    (time
     (do-sort 'quick-sort-generic 
       (list rnumsl #'> 'list) "generic, list" cnt))

    ;; Sort string in descending order by char int vals
    (time 
     (do-sort 'quick-sort-generic
       (list (map 'string #'(lambda (x) (code-char x)) rstr)
    	     #'(lambda (x y) (> (char-int x) (char-int y)))
    	     'string) "generic, string" cnt))))
       

