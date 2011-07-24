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


;; Implementation of bucket sort

(set! *warn-on-reflection* true)


(defn insertion-sort
  "runs in linear time when only a few items are being sorted.
   Otherwise O(n^2) time."
  [items & cfun]
  (let [cfun (if cfun cfun <=)]
    (letfn [(insert
             ([raw x] (insert [] raw x))
             ([sorted [y & raw] x]
                (if (nil? y) (conj sorted x)
                    (if (cfun x y)
                      (concat sorted [x,y] raw)
                      (recur (conj sorted y) raw x )))))]
      (reduce insert [] items))))


(defn bucket-sort
  "Runs in O(n) time."
  [items]
  (let [len (count items)
        mx (apply max items)
        bucket-size (inc (int (/ mx len)))
        buckets (reduce (fn [v n] (conj v [])) []
                        (range (+ bucket-size (/ mx bucket-size))))]
    (letfn [(distrib-nums [v n]
                          (let [ind (int (/ n bucket-size))
                                bucket (nth v ind)]
                            (assoc! v ind (conj bucket n))))]
      (let [pre-buckets (persistent!
                         (reduce distrib-nums (transient buckets) items))]
        (apply concat (map (fn [bucket]
                             (when (> (count bucket) 0)
                               (insertion-sort bucket))) pre-buckets))))))


(defn bucket-sort-lam
  "Runs in O(n) time."
  [items lam]
  (let [len (count items)
        mitems (map (fn [n] [(lam n) n]) items)
        mx (reduce (fn [[fm rm :as m] [fn rn :as n]]
                     (if (> fm fn) m n))
                   (first mitems) (rest mitems))
        bucket-size (inc (int (/ (first mx) len)))
        buckets (reduce (fn [v n] (conj v [])) []
                        (range (+ bucket-size (/ (first mx) bucket-size))))]
    (letfn [(distrib-nums [v n]
                          (let [ind (int (/ (first n) bucket-size))
                                bucket (nth v ind)]
                            (assoc! v ind (conj bucket (last n)))))]
      (let [pre-buckets (persistent!
                         (reduce distrib-nums (transient buckets) mitems))]
        (apply concat (map (fn [bucket]
                             (when (> (count bucket) 0)
                               (insertion-sort bucket))) pre-buckets))))))


(let [cnt 1e6
      rnums (map (fn [n] (int (rand cnt))) (range cnt))]
  ;; warmup
  (take 10 (bucket-sort rnums))
  (take 10 (bucket-sort-lam rnums (fn [x] (+ 10 cnt x))))
  (take 10 (sort rnums))
  ;; run
  (println (time (take 10 (bucket-sort rnums))))
  (println (time (take 10 (bucket-sort-lam rnums (fn [x] (+ 10 cnt x))))))
  (println (time (take 10 (sort rnums))))
  )



