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


(import 'java.lang.Math)
(import 'java.math.MathContext)
(import 'java.math.BigDecimal)


(defn sb-pi 
  "Calculates PI digits using the Salamin-Brent algorithm
   and Java's BigDecimal class."
  [places]

  (let [digits (.intValue (+ 10 places)) ;; add some guard digits
        round-mode BigDecimal/ROUND_DOWN]

    (letfn [(big-sqrt[#^BigDecimal num]
             "Calculates square root using Newton's method."
             (letfn [(big-sqrt-int 
                      [#^BigDecimal num #^BigDecimal x0 #^BigDecimal x1]
                      "aux function for calculating square root"
                      (let [#^BigDecimal x0new x1
                            #^BigDecimal x1new (.divide num x0new digits round-mode)
                            #^BigDecimal xsum (+ x1new x0new)
                            #^BigDecimal x1tot (.divide xsum 2M digits round-mode)]
                        (if (= x0 x1)
                          x1tot
                          (recur num x1 x1tot))))]
               (big-sqrt-int
                num 0M (BigDecimal/valueOf
                        (Math/sqrt (. num doubleValue))))))
            (sb-pi-int [#^BigDecimal a #^BigDecimal b 
                        #^BigDecimal t #^BigDecimal x #^BigDecimal y]
             "aux function for calculating PI"
             (let
                 [#^BigDecimal y1 a
                  #^BigDecimal absum (+ a b)
                  #^BigDecimal a1 (-> absum (.divide 2M digits round-mode))
                  #^BigDecimal b1 (big-sqrt (* b y1))
                  #^BigDecimal ydiff (- y1 a1)
                  #^BigDecimal t1 (- t (* x ydiff ydiff))
                  #^BigDecimal x1 (* x 2M)]               
               (if (== a b)
                 (let [#^BigDecimal absum1 (+ a1 b1)
                       #^BigDecimal absqrd (* absum1 absum1)
                       #^BigDecimal u (* t1 4M)]
                   (-> absqrd
                       (.divide u digits round-mode)
                       (.setScale places round-mode)))
                 (recur a1 b1 t1 x1 y1))))]
      
      (sb-pi-int 1M (-> 1M (.divide #^BigDecimal (big-sqrt 2M) digits round-mode))
                       (/ 1M 4M) 1M nil))))


(time (println (sb-pi (Integer/parseInt (second *command-line-args*)))))
