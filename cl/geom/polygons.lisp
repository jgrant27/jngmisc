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


(defun polygon-area (pts)
  "Calculates the signed area of a polygon given a list of points.
   Assumes that the last point is connected to the first point.
   http://mathworld.wolfram.com/PolygonArea.html"
  
  (labels ((matrix-det (pt1 pt2)
		       (- (* (first pt1) (second pt2)) 
			  (* (first pt2) (second pt1)))))
    (destructuring-bind (iarea last-pt) 
	(reduce #'(lambda (res pt)
		    (list (+ (first res) (matrix-det (second res) pt)) pt))
		(rest pts) :initial-value (list 0 (first pts)))
      (* 0.5 (+ iarea (matrix-det last-pt (first pts)))))))


;;
;; a square with sides of length 5
;; 
;; * (time (polygon-area '((0 0) (5 0) (5 5) (0 5) (0 0))))
;;
;; Evaluation took:
;;   0.000 seconds of real time
;;   0.000020 seconds of total run time (0.000020 user, 0.000000 system)
;;   100.00% CPU
;;   13,697 processor cycles
;;   0 bytes consed
;;  
;; 25.0
;;

;;
;; a right angled triangle with sides x and y of length 5
;;
;; * (time (polygon-area '((0 0) (5 0) (0 5))))
;;
;; Evaluation took:
;;   0.000 seconds of real time
;;   0.000011 seconds of total run time (0.000010 user, 0.000001 system)
;;   100.00% CPU
;;   8,248 processor cycles
;;   0 bytes consed
;;  
;; 12.5
;;
