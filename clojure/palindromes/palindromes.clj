;;
;; Copyright (c) 2009, Justin Grant <justin at imagine27 dot com>
;; All rights reserved.

;; Redistribution and use in source and binary forms, with or without modification
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
;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE
;; EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;

(set! *warn-on-reflection* true)


(ns i27.palindromes
  (:import java.util.ArrayList))




(defn longest-pals-naive
  "O(n^2) time complexity, O(n) space complexity"
  [text]

  (let* [afirst 0
         alast (dec (count text))
         positions (transient [])]

        (letfn [(ext-pal-around
                 [start end]
                 (if (or (< start 0) (> end alast)
                         (not (= (nth text start) (nth text end))))
                   (- end start 1)
                   (recur (dec start) (inc end))))

                (pal-len-around
                 [position]
                 (let [pos (long (/ position 2))
                       nstart (dec (+ afirst pos))
                       nend (cond (even? position) (+ afirst pos)
                                  (odd? position) (+ afirst pos 1))]
                   (ext-pal-around nstart nend)))]

          (dotimes [n (* 2 (inc alast))]
            (conj! positions (pal-len-around n)))
          (persistent! positions))))


(defn longest-pals-fast
  "O(n) time & space complexity"
  [text]

  (letfn [(final-centers
           [n tcenters centers]
           (cond
            (<= n 1)
            centers
            true
            (let [n (dec n)]
              (recur n
                     (rest tcenters)
                     (cons (min (first tcenters) n)
                           centers)))))

          (ext-centers
           [strn n centers tcenters cdist]
           (cond
            (= 0 cdist)
            #(ext-tail strn (inc n) 1 centers)
            (= (dec cdist) (first tcenters))
            #(ext-tail strn n (first tcenters) centers)
            true
            #(ext-centers strn n
                          (cons (min (first tcenters) (dec cdist))
                                centers)
                          (rest tcenters) (dec cdist))))

          (ext-tail
           [strn n curr-tail centers]
           (cond
            (> n (dec (count strn)))
            #(final-centers curr-tail centers
                            (cons curr-tail centers))
            (= (- n curr-tail) 0)
            #(ext-centers strn n
                          (cons curr-tail centers)
                          centers curr-tail)
            (= (nth strn n) (nth strn (- n curr-tail 1)))
            #(ext-tail strn (inc n) (+ 2 curr-tail) centers)
            true
            #(ext-centers strn n
                          (cons curr-tail centers)
                          centers curr-tail)))

          (pal-around-centers
           [strn]
           (reverse (trampoline #(ext-tail strn 0 0 ()))))]

    (pal-around-centers text)))


(defn longest-pal-str [strn seq]
  (if (empty? seq)
    ""
    (let* [len (reduce max seq)
           pos (.indexOf (ArrayList. seq) len)
           start (- (/ pos 2) (/ len 2))
           end (+ (/ pos 2) (/ len 2))]
          (subs strn start end))))

