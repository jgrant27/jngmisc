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


;; Misc. geometric math functions

(ns i27.geom.misc
  (:import [java.lang Math]))


(def pi (* 4 (Math/atan 1)))

(defn dtor [deg] (* deg (/ pi 180)))

(defn rtod [rad] (* rad (/ 180 pi)))

(defn pass [val] val)

(defn cross-product
  "Calculate the cross product given 3 points in the cartesian plane."
  [[x1 y1 :as pt1] [x2 y2 :as pt2] [x3 y3 :as pt3]]
  (- (* (- x3 x1) (- y2 y1)) (* (- x2 x1) (- y3 y1))))

(defn cartesian-to-polar
  "Converts a point in cartesian co-ordinates to it's polar equivalent.
   Optionally return the angle in degrees instead of radians"
  [[x1 y1 :as pt] & degs]
  (let [^Double x (Double. x1) ^Double y (Double. y1)
        cfun (if (first degs) rtod pass)]
	(vector
	 (Math/sqrt (+ (* x x) (* y y)))
	 (if (= x 0)
	     (cond
	       (> y 0) (cfun (/ pi 2))
	       (< y 0) (cfun (* pi (/ 3 2)))
	       (= y 0) 0)
	     (let [at (Math/atan (/ y x))]
		  (cond
		    (and (> x 0) (>= y 0)) (cfun at)
		    (and (> x 0) (< ~y 0))  (cfun (+ (* 2 pi) at))
		    (< x 0)                 (cfun (+ pi at))))))))


(defn cartesian-to-polar-seq
  "Converts a seq of cartesian co-ordinates to their polar equivalents.
   Optionally return the angle in degrees instead of radians"
  [cpts & degs]
    (map (fn [pt] (cartesian-to-polar pt (first degs))) cpts))

(defn polar-to-cartesian
  "Converts a point in polar to it's cartesian equivalent.
   Optionally take the angle in degrees instead of radians"
  [pt & degs]
  (let [mag (first pt) angle (last pt)
        cfun (if (first degs) dtor pass)]
       (vector (Math/round (* mag (Math/cos (cfun angle))))
               (Math/round (* mag (Math/sin (cfun angle)))))))

(defn polar-to-cartesian-seq
  "Converts a seq of polar co-ords to their cartesian equivalents."
  [ppts & degs]
  (map (fn [pt] (polar-to-cartesian pt (first degs))) ppts))

(defn angle [[x1 y1 :as pt1] [x2 y2 :as pt2]]
  "Returns the polar angle between point 1 and point 2"
  (last (cartesian-to-polar [(- x2 x1) (- y2 y1)])))

