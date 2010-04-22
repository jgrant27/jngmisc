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


(defn- swap [a i j]
  (assoc a i (nth a j) j (nth a i)))
 
(defn- sift [a pred k l]
  (loop [a a x k y (inc (* 2 k))]
    (if (< (inc (* 2 x)) l)
      (let [ch (if (and (< y (dec l)) (pred (nth a y) (nth a (inc y))))
                 (inc y)
                 y)]
        (if (pred (nth a x) (nth a ch))
          (recur (swap a x ch) ch (inc (* 2 ch)))
          a))
      a)))

(defn- heapify[pred a len]
  (reduce (fn [c term] (sift (swap c term 0) pred 0 term))
          (reduce (fn [c i] (sift c pred i len))
                  (vec a)
                  (range (dec (int (/ len 2))) -1 -1))
          (range (dec len) 0 -1)))


(defn heap-sort
  ([a pred]
   (let [len (count a)]
     (heapify pred a len)))
  ([a]
     (heap-sort a <)))


(comment
(println (heap-sort '(3 2 5 9 1 2 6)))
;; (1 2 2 3 5 6 9)
)