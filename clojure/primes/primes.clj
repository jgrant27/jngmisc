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

;;;; example macro usage with primes



(defn divides? [candidate-divisor dividend]
  (zero? (rem dividend candidate-divisor)))

(defn prime? [num]
  (when (> num 1)
    (every? (fn [x] (not (divides? x num)))
            (range 2 (inc (int (Math/sqrt num)))))))

(defn primes-from [number]
  (filter prime? (iterate inc number)))

(defn primes-in-range [start end]
  (for [x (primes-from start) :while (<= x end)] x))

(defmacro do-primes [var start end & body]
  `(doseq [~var (primes-in-range ~start ~end)] ~@body))

;;;; example use
;;(do-primes i 100 200
;;           (print (format "%d " i)))
;;(println)


(defn lazy-primes []
  (letfn [(enqueue [sieve n step]
                   (let [m (+ n step)]
                     (if (sieve m)
                       (recur sieve m step)
                       (assoc sieve m step))))
          (next-sieve [sieve candidate]
                      (if-let [step (sieve candidate)]
                        (-> sieve
                            (dissoc candidate)
                            (enqueue candidate step))
                        (enqueue sieve candidate (+ candidate candidate))))
          (next-primes [sieve candidate]
                       (if (sieve candidate)
                         (recur (next-sieve sieve candidate) (+ candidate 2))
                         (cons candidate
                               (lazy-seq (next-primes (next-sieve sieve candidate)
                                                      (+ candidate 2))))))]
    (cons 2 (lazy-seq (next-primes {} 3)))))

(def lazy-primes (memoize lazy-primes))

(println (time (nth (lazy-primes) 1E6)))
(println (time (nth (lazy-primes) 1E6)))
