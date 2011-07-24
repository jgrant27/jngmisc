;;
;; Copyright (c) 2011, Justin Grant <justin at imagine27 dot com>
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


(ns convexhull
    (:require 
	      [goog.Timer :as timer]
	      [goog.events :as events]
	      [goog.events.EventType :as event-type]
	      [goog.dom.classes :as classes]
	      [goog.dom :as dom]
	      [goog.graphics.Stroke :as Stroke]
	      [goog.graphics.SolidFill :as SolidFill]
	      [goog.graphics.Path :as Path]
	      [goog.graphics :as graphics]))


;; Misc. geometric math functions

(def pi (* 4 (Math/atan 1)))

(defn dtor [deg] (* deg (/ pi 180)))

(defn rtod [rad] (* rad (/ 180 pi)))

(defn pass [val] val)

(defn cross-product
  "Calculate the cross product given 3 points in the cartesian plane."
  [[x1 y1 :as pt1] [x2 y2 :as pt2] [x3 y3 :as pt3]]
  (- (* (- x3 x1) (- y2 y1))
     (* (- x2 x1) (- y3 y1))))

(defn cartesian-to-polar
  "Converts a point in cartesian co-ordinates to it's polar equivalent.
   Optionally return the angle in degrees instead of radians"
  [[x y :as pt] & degs]
  (let [cfun (if (first degs) rtod pass)]
       (vector
        (Math/sqrt (+ (* x x) (* y y)))
        (if (not (= x 0))
          (let [at (Math/atan (/ y x))]
            (cond
             (and (> x 0) (>= y 0)) (cfun at)
             (and (> x 0) (< y 0))  (cfun (+ (* 2 pi) at))
             (< x 0)                (cfun (+ pi at))))
          (cond
           (> y 0) (cfun (/ pi 2))
           (< y 0) (cfun (* pi (/ 3 2)))
           (= y 0) 0)))))

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


;; Calculates the convex hull of a set of points using the Graham scan
;; algorithm.

(defn find-start 
    [[x1 y1 :as pt1] [x2 y2 :as pt2]]
    (cond (< y1 y2) pt1
	  (= y1 y2) (if (< x1 x2) pt1 pt2)
	  true pt2))

(defn presort-points [pts]
  "Presorts the cartesian points in descending order by angle.
   The point with lowest y value is last in the vector."
    (let* [pt (reduce find-start pts)
	  npts (remove (fn [[x y :as cpt]]
			   (and (= (first pt) x) (= (last pt) y))) pts)]
			   (conj (apply vector
					(sort (fn [pt1 pt2]
						  (> (angle pt pt1) (angle pt pt2))) npts))
				 pt)))

(defn popstack [stk pt]
      (if (not (> (cross-product
		   (last (pop stk)) (last stk) pt) 0))
	  stk (recur (pop stk) pt)))

(defn scan [res pt]
      (conj (popstack res pt) pt))

(defn subvec 
    [v start end]
    (let [fsec (take-last (- (count v) start) v)]
	 (take (- end start) fsec)))

(defn convex-hull
  "Calculates the convex hull given a set of points in the cartesian plane.
   Uses the Graham scan algorithm. Performance is O(n lg n) time."
  [pts]
  (when (>= (count pts) 3)
    (let [start (apply vector
                       (reverse (subvec pts (- (count pts) 3) (count pts))))
	 other (reverse (subvec pts 0 (- (count pts) 3)))]
	 (reduce scan start other))))

(defn find-quad
    [[[x1 y1 :as pt1] [x3 y3 :as pt3]
    [x2 y2 :as pt2] [x4 y4 :as pt4] :as quad] [xc yc :as cpt]]
    [(if (> x1 xc) cpt pt1) (if (> y3 yc) cpt pt3)
    (if (< x2 xc) cpt pt2) (if (< y4 yc) cpt pt4)])

(defn elim-points
  "Prepare data with Aki-Toussaint heuristics before passing to convex hull function.
   Performance is O(n) time regardless of the convex hull algorithm used."
  [pts]
  (when (>= (count pts) 4)
    (let [poly (distinct (reduce find-quad (take 4 pts) pts))
	 ;; generate the line segments that form the convex polygon
	 lines (conj (reduce (fn [lines pt] (conj lines [(last (last lines)) pt]))
			     [[(first poly) (second poly)]] (rest (rest poly)))
		     [(last poly) (first poly)])
	 ;; filter out all points that fall inside the convex polygon
	 pts (filter
	      (fn [tpt]
		  (not (reduce (fn [b v] (and b (neg? v))) true
			       (map (fn [pt]
					(cross-product tpt (first pt) (last pt)))
				    lines))))
                 pts)]
        pts)))

(defn range
    [start end v]
    (if (> start end)
	(reverse v)
	(recur start (dec end) (conj v end))))

;; generative functions

(defn ^:export smalltest
  "Some simple tests"
  []
  ;; descending order of angle from point with lowest y then x val - [1 2]
  (let [pts
        [[3 9] [7 6] [9 9] [2 1] [4 2] [0 8] [1 3] [5 7] [6 0] [4 9] [6 0]]] ;; 7 points
	(let [cvxhull (convex-hull (presort-points pts))]
	     cvxhull)))

(defn ^:export randomset
  "Test on large random data sets"
  [pts heurp]
  (let [epts (if heurp (elim-points pts) pts)
        spts (presort-points epts)
        cvxhull (convex-hull spts)]
	cvxhull))

;;;; UI stuff

(def edge-stroke (graphics/Stroke. 1 "#444"))

(def blue-edge-stroke (graphics/Stroke. 1 "#66b"))

(def green-edge-stroke (graphics/Stroke. 1 "#0f0"))

(def white-fill (graphics/SolidFill. "#fff"))

(def blue-fill (graphics/SolidFill. "#66b"))

(def green-fill (graphics/SolidFill. "#0f0"))

(def trans-fill (graphics/SolidFill. "#0f0" 0.001))

(def g
  (doto (graphics/createGraphics "440" "440")
    (.render (dom/getElement "graph"))))

(defn draw-graph 
    []
    (let [canvas-size (. g (getPixelSize))]
	 (.drawRect g 0 0 
		    (.width canvas-size) (.height canvas-size) 
		    edge-stroke white-fill)))

(defn scale-coord
    [coord]
  (+ 20 (* 4 coord)))

(defn draw-points
    [points stroke fill]
    (doseq  [[x y :as pt] points]
	    (.drawEllipse g (scale-coord x) (scale-coord y) 
		       3 3 stroke fill)))

(defn draw-convex-hull
    [points stroke fill]
    (let [path (graphics/Path.)
	  [xs ys :as start] (first points)]
	 (.moveTo path (scale-coord xs) (scale-coord ys))
	 (doall (map (fn [[x y :as pt]]
			 (.lineTo path (scale-coord x) (scale-coord y)))
		     (rest points)))
	 (.lineTo path (scale-coord xs) (scale-coord ys))
    (.drawPath g path stroke fill)))

(defn print-points
    [points el]
    (doseq [pair points]
	   (dom/append el
		       (str " [" (first pair) " " (second pair) "]"))))

(defn ^:export rundemo
  []
  (let [cnt 1E2
        rpts (apply 
       	     vector 
       	     (map (fn [n] 
       		      [(rand-int (inc cnt))
       		       (rand-int (inc cnt))])
       		  (range 1 cnt [])))
       text-input-title (dom/getElement "text-input-title")
       text-input (dom/getElement "text-input")
       text-results-status (dom/getElement "text-results-status")
       text-results (dom/getElement "text-results")]
       (draw-graph) 
       ;; draw all points
       (dom/setTextContent 
	text-input-title 
	(str "Random generation of " cnt " points...")) 
       (draw-points rpts blue-edge-stroke blue-fill)
       (print-points rpts text-input)
       ;; calc hull
       (dom/setTextContent 
	text-results-status 
	(str "Calculating convex hull ...")) 
       (let [;;r1 (largetest rpts false)
	     r2 (randomset rpts true)]
	     (dom/append text-results-status (str " done.\n")) 
	     ;; update the results
	     (print-points r2 text-results)
	     (dom/append
	      text-results-status
	      (str "Convex hull has " (count r2) " points.\n"))
	     ;; draw hull points
	     (draw-points r2 green-edge-stroke green-fill)
	     ;; draw hull
	     (draw-convex-hull r2 green-edge-stroke trans-fill)
	     ;; return the results
	     [rpts r2])))

;; Auto-update
(defn ^:export poll
  []
  (let [timer (goog.Timer. 15000)]
    (do (rundemo)
        (. timer (start))
        (events/listen timer goog.Timer/TICK rundemo))))
