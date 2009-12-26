;;
;; Copyright (c) 2009, Justin Grant <justin at imagine27 dot com>
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


(defun longest-pals-fast (text)
  "O(n) time & space complexity"

  (declare (optimize (speed 3) 
                     (compilation-speed 0) 
                     (space 0) (debug 0)))

  (labels ((final-centers (n tcenters centers)
             (cond
               ((<= n 0) centers)
               (t
                (let ((n (- n 1)))
                  (final-centers 
                   n
                   (rest tcenters)
                   (cons 
                    (min n (first tcenters))
                    centers))))))
           
           (ext-centers (text n centers tcenters cdist)
             (cond
               ((eq 0 cdist)
                (ext-tail text (+ 1 n) 1 centers))
               ((eq (- cdist 1) (first tcenters))
                (ext-tail text n (first tcenters) centers))
               (t
                (ext-centers text n 
                             (cons 
                               (min (- cdist 1) (first tcenters))
                              centers)
                             (rest tcenters) (- cdist 1)))))
    
           (ext-tail (text n curr-tail centers)
             (cond 
               ((> n (- (length text) 1))
                (final-centers curr-tail centers 
                               (cons curr-tail centers)))
                ((eql (- n curr-tail) 0)
                 (ext-centers text n 
                              (cons curr-tail centers)
                              centers curr-tail))
                ((eq (elt text n) (elt text (- n curr-tail 1)))
                 (ext-tail text (+ 1 n) (+ 2 curr-tail) centers))
                (t
                 (ext-centers text n
                              (cons curr-tail centers)
                              centers curr-tail))))
    
           (pal-around-centers (text)
             (ext-tail text 0 0 '())))
    
    (pal-around-centers text)))


(defun longest-pals-naive (text)
  "O(n^2) time complexity, O(n) space complexity"
  
  (declare (optimize (speed 3) 
                     (compilation-speed 0) 
                     (space 0) (debug 0)))

  (let* ((afirst 0)
         (alast (- (length text) 1))
         (len (+ 1 (* 2 (+ 1 alast))))
         (positions (make-array len :element-type 'fixnum)))
    (declare (type (simple-array fixnum (*)) positions))
    
    (labels ((ext-pal-around (start end)
               (if (or (< start 0) (> end alast)
                       (not (eql (elt text start) (elt text end))))
                   (- end start 1)
                   (ext-pal-around (- start 1) (+ 1 end))))
             
             (pal-len-around (position)
               (let* ((pos (floor position 2))
                      (nstart (- (+ afirst pos) 1))
                      (nend (if (evenp position) 
                                (+ afirst pos)
                                (+ afirst pos 1))))
                 (ext-pal-around nstart nend))))

      (dotimes (n len)
        (setf (aref positions n) (pal-len-around n)))
      positions)))


(defun longest-pal-str (text seq)
  (if (not (> (length text) 0))
      ""
      (let* ((len (reduce #'max seq))
             ;; A list will have it's elements in reverse
             (pos (if (eql 'cons (type-of seq)) 
                      ;; ... so we search in reverse ...
                      (- (length seq) (position len seq :from-end t)) 
                      ;; ... a simple array 
                      (position len seq))) 
             (start (truncate (- (/ pos 2) (/ len 2))))
             (end   (truncate (+ (/ pos 2) (/ len 2)))))
        (declare (fixnum start end))
        (subseq text start end))))


(defun longest-pal-str-test (text)
  (let* ((pals (longest-pals-fast text))
        (str (longest-pal-str text pals)))
    (format t "~A~%~A~%" pals str) str))


(defun small-test ()
  (let ((text "a boob ool"))
    (format t "~% ~A ~% ~A ~%" 
            (longest-pals-naive text)
            (longest-pals-fast text))
    (format t "~% '~A' ~% '~A' ~%" 
            (longest-pal-str text (longest-pals-naive text))
            (longest-pal-str text (longest-pals-fast text)))))


(defun run-tests ()  
  (let ((success (and
                  (not (eql (longest-pal-str-test "bob says yabbadabbadooo !") "bob"))
                  (equal (longest-pal-str-test "eat a banana bob !") "anana")
                  (not (eql (longest-pal-str-test "eat a banana bob !") "bob"))
                  (equal (longest-pal-str-test "lol") "lol")
                  (not (eql (longest-pal-str-test "lol") "bob"))
                  (equal (longest-pal-str-test "") "")
                  (not (eql (longest-pal-str-test "") "bob"))
                  (equal (longest-pal-str-test "A876BC110115438776E0FC16BFF7B24537") "11011")
                  ;; the first 49 chars of the first chromosome of the human genome
                  (equal (longest-pal-str-test "AATTCTTTGATTGATAATTTTTTCTTCTCAGTCTTTTATCTTGTCTCTTC") "TTTTTT")
                  (equal (longest-pal-str-test "TTTTTT") "TTTTTT"))))
    (if success
        (format t "~%All tests succeeded.~%")
        (format t "~%Test/s failed !~%"))))
  

(defun big-test ()
  (let* ((cnt (truncate 1e3))
         (line " amanaplanacanalpanama ")
         (text (with-output-to-string (stream)
                 (dotimes (n cnt) (format stream "~A" line)))))
    (format t "~%~A X '~A' ~%" cnt line)
    (format t "naive : ~%")
    (time (longest-pal-str text (longest-pals-naive text)))
    (format t "fast : ~%")
    (time (longest-pal-str text (longest-pals-fast text)))
    nil))

