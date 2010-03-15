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



(defun range (start end)
  (loop for i from start below end collect (random i)))

(setf *random-state* (make-random-state t))
(let* ((cnt (floor 1E4))
       (sub 100))

  ;; examples using arrays

  (let ((rnums (make-array cnt)))
    (dotimes (n cnt) (setf (svref rnums n) (random cnt)))
  
    ;; Sort numbers in descending order (array). (single thread)    
    (format t "~%started array seq merge-sort ...~%")
    (finish-output)
    (let ((snums (time (merge-sort-arr rnums #'>))))
      (format t "merge-sorted ~A items (first ~A shown) : ~%~A ~%~%~%" 
              (length snums) sub (subseq snums 0 sub)))
    )

  ;; examples using lists
  (let ((rnums (list (random cnt))))
    (format t "~%building random list ~A ...~%" cnt)
    (dotimes (n (- cnt 1)) (nconc rnums (list (random cnt))))
    (format t "~%sorting ~A ...~%" (subseq rnums 0 sub))
    (finish-output)
    
    ;; Filter numbers with factors of 3 then sort multiples of 9 first in desc.
    (format t "merge-sorted ~A items (first ~A shown) : ~%~A ~%~%~%" 
            cnt sub
            (time
             (subseq
              (map 'list #'(lambda (x) (when (= 0 (mod x 3)) x))
                   (merge-sort-lst
                    (delete #'(lambda (x) (not (and (> x 0) (= 0 (mod x 3)))))
                            rnums :test #'(lambda (f n) (apply f (list n))))
                    #'(lambda (x y) 
                        (and (= 0 (mod x 9)) (>= x 9)
                             (or (not (= 0 (mod y 9)))
                                 (and (= 0 (mod y 9)) (>= y 9)
                                      (> x y)))))))
              0 sub)))
    )
  
  )
