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


(defun merge-sort-arr(arr cfun)
  "Implements the well-known merge-sort sorting algorithm using arrays
   O(n log n) time, O(n) space"

  (labels ((merge-aux (left right res)
             (if (not (and (> (length left) 0) (> (length right) 0)))
                 (list left right res)
                 (if (apply cfun (list (svref left 0) (svref right 0)))
                     (progn (vector-push (svref left 0) res)
                            (merge-aux (subseq left 1 (length left)) right res))
                     (progn (vector-push (svref right 0) res)
                            (merge-aux left (subseq right 1 (length right)) res)))))

           (merge-segments (left right)
             (declare (type simple-array left right))
             (destructuring-bind (left right res) 
                 (merge-aux left right 
                            (make-array (+ (length left) (length right))
                                        :element-type 'fixnum
                                        :fill-pointer 0))
               (if (> (length left) 0)
                   (concatenate 'vector res left)
                   (concatenate 'vector res right)))))

    (let ((len (length arr)))
      (when (> len 0)
        (if (= 1 len) 
            arr
            (let* ((mid (floor len 2))
                   (left (merge-sort-arr (subseq arr 0 mid) cfun))
                   (right (merge-sort-arr (subseq arr mid len) cfun)))
             (if (apply cfun (list (svref left (- (length left) 1)) (svref right 0)))
                 (concatenate 'vector left right)
                 (merge-segments left right))))))))


(defun merge-sort-lst(lst cfun)
  "Implements the well-known merge-sort sorting algorithm using lists
   O(n log n) time, O(n) space"

  (labels ((merge-aux (left right res)
             (if (not (and (> (length left) 0)
                           (> (length right) 0)))
                 (list left right res)
                 (if (apply cfun (list (first left) (first right)))
                     (merge-aux (rest left) right
                                (nconc res (list (first left))))
                     (merge-aux left (rest right)
                                (nconc res (list (first right)))))))

           (merge-segments (left right)
             (destructuring-bind (left right res) 
                 (merge-aux left right nil)
               (if (> (length left) 0)
                   (nconc res left)
                   (nconc res right)))))

    (let ((len (length lst)))
      (when (> len 0)
        (if (= 1 len) 
            lst
            (let* ((mid (floor len 2))
                   (left (merge-sort-lst (subseq lst 0 mid) cfun))
                   (right (merge-sort-lst (subseq lst mid len) cfun)))
              (if (apply cfun (list (first (last left)) (first right)))
                  (nconc left right)
                  (merge-segments left right))))))))

;; fastest
(defun merge-sort-gen (sequence cfun &optional result-type)
   (let ((split (floor (length sequence) 2))
	 (result-type (if (eq nil result-type) 'vector result-type)))
     (if (zerop split)
       (copy-seq sequence)
       (merge result-type (merge-sort-gen (subseq sequence 0 split) cfun result-type)
                          (merge-sort-gen (subseq sequence split)   cfun result-type)
                          cfun))))


(defmacro do-sort (fun args type cnt)
  `(progn
     (format t "~%started merge-sort (~A) ...~%" ,type)
     (finish-output)
     (let ((sub (if (>= cnt 10) 10 cnt))
	   #-:ECL-READ-WRITE-LOCK
	   (snums (time (apply ,fun ,args)))
           #+:ECL-READ-WRITE-LOCK
	   (snums (apply ,fun ,args)))
       (format t "merge-sorted ~A items (first ~A shown) : ~%~A ~%~%~%" 
	       (length snums) sub (subseq snums 0 sub)))))
  

(defun range (start end)
  (loop for i from start below end collect (random i)))

;; tests
(defun test-sort()
  (setf *random-state* (make-random-state t))
  (let* ((cnt (floor 1E4))
	 (rnumsa (make-array cnt))
	 (rnumsl (list (random cnt))))
    (dotimes (n cnt) (setf (svref rnumsa n) (random cnt)))
    (dotimes (n (- cnt 1)) (nconc rnumsl (list (random cnt))))
    
    ;; Sort numbers in descending order (array). (single thread)    
    (time
     (do-sort 'merge-sort-arr 
       (list rnumsa #'>) "array" cnt))
    
    ;; Sort numbers in descending order (list). (single thread)    
    (time
     (do-sort 'merge-sort-lst 
       (list rnumsl #'>) "list" cnt))
    
    ;; Sort numbers in descending order (generic). (single thread)    
    (time
     (do-sort 'merge-sort-gen 
       (list rnumsl #'>) "generic" cnt))
    
    ;; Filter numbers with factors of 3 then sort multiples of 9 first in desc.
    (time
     (do-sort 'merge-sort-lst 
       (list 
	(map 'list #'(lambda (x) (when (= 0 (mod x 3)) x))
	     (delete #'(lambda (x) (not (and (> x 0) (= 0 (mod x 3)))))
		     rnumsl :test #'(lambda (f n) (apply f (list n)))))
	#'(lambda (x y) 
	    (and (= 0 (mod x 9)) (>= x 9)
		 (or (not (= 0 (mod y 9)))
		     (and (= 0 (mod y 9)) (>= y 9)
			  (> x y)))))) "generic custom" cnt))
     ))
  
  
