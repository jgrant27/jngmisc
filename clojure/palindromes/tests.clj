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

(set! *warn-on-reflection* true)

(load "palindromes")

(ns i27.palindromes
  (:use clojure.test))


;; Utils

(defmacro deftests-assert-eql-aux [name fnstr fnames strn res & [negate]]
  (doseq [fnpal fnames]
    (eval
     (let [tname (symbol (str "test-strn-longest-pal-" (gensym)))
           pal (gensym) truncate (gensym) strcnt (gensym)
           len (gensym) trunclen 40 stime (gensym)]
       `(deftest ~tname
          (let [~strcnt (count ~strn)
                ~truncate (> ~strcnt ~trunclen)
                ~len (if ~truncate ~trunclen ~strcnt)
                ~stime (System/currentTimeMillis)
                ~pal (~fnstr ~strn (~fnpal ~strn))
                ttime# (- (System/currentTimeMillis) ~stime)]
            (println
             (format (str " %-20s %9dms    text      : %-30s\n" "%35s  expected  : %-30s")
                     ~name ttime#
                     (str "'" (subs ~strn 0 ~len) (when ~truncate " ... ") "'") " "
                     (str "'" ~res "'")))
            ~(if (true? negate)
               `(is (not (= ~res ~pal)))
               `(is (= ~res ~pal)))))))))

(defmacro deftests-lpn-assert-eql [strn res]
  `(deftests-assert-eql-aux "test-assert-eql" 
     longest-pal-str (longest-pals-naive longest-pals-fast) ~strn ~res false))

(defmacro deftests-lpn-assert-not-eql [strn res]
  `(deftests-assert-eql-aux "test-assert-not-eql" 
     longest-pal-str (longest-pals-naive longest-pals-fast) ~strn ~res true))


;;;; Tests

;;; Sanity checks
(deftests-lpn-assert-not-eql "bob says yabbadabbadooo !" "bob")
(deftests-lpn-assert-eql     "eat a banana bob !" "anana")
(deftests-lpn-assert-not-eql "eat a banana bob !" "bob")
(deftests-lpn-assert-eql     "lol" "lol")
(deftests-lpn-assert-not-eql "lol" "bob")
(deftests-lpn-assert-eql     "" "")
(deftests-lpn-assert-not-eql "" "bob")
(deftests-lpn-assert-eql     "A876BC110115438776E0FC16BFF7B24537" "11011")
;; the first 49 chars of the first chromosome of the human genome
(deftests-lpn-assert-eql     "AATTCTTTGATTGATAATTTTTTCTTCTCAGTCTTTTATCTTGTCTCTTC" "TTTTTT")
(deftests-lpn-assert-eql "TTTTTT" "TTTTTT")


;(run-tests 'i27.palindromes)


(defn big-test []
  (let [lcnt 1E3
        line " amanaplanacanalpanama "
        sb (StringBuilder.)]
    (def +big-test-strn+ (do (dotimes [x lcnt] (.append sb line)) (.toString sb)))
    (println (format "\n%d X '%s' " (int lcnt) line))
    (print (format "naive : "))
    (time (longest-pal-str +big-test-strn+ (longest-pals-naive +big-test-strn+)))
    (print (format "fast  : "))
    (time (longest-pal-str +big-test-strn+ (longest-pals-fast +big-test-strn+))))
  (println)
  nil)


;(let [strn (slurp "/Users/jgrant/human_genome/01hgp10a_abbr.txt")] ;;; ~ 280MB
;(let [strn (slurp "/Users/jgrant/Desktop/228.txt")] ;;; ~ 700KB
; (time (print (format "'%s'    - " (longest-pal-str strn (longest-pals-naive strn)))))
; (time (print (format "'%s'    - " (longest-pal-str strn (longest-pals-fast strn))))))

