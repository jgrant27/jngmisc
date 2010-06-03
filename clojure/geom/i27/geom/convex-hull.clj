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


;; Calculates the convex hull of a set of points using the Graham scan
;; algorithm.

(set! *warn-on-reflection* true)

(ns i27.geom.convex-hull
  (:use [i27.geom.misc :only (angle cross-product)]))


(defn presort-points [pts]
  "Presorts the cartesian points in descending order by angle.
   The point with lowest y value is last in the vector."
  (letfn [(find-start [[x1 y1 :as pt1] [x2 y2 :as pt2]]
                      (cond (< y1 y2) pt1
                            (= y1 y2) (if (< x1 x2) pt1 pt2)
                            true pt2))]
    (let* [pt (reduce find-start pts)
           npts (remove (fn [[x y :as cpt]]
                          (and (= (first pt) x) (= (last pt) y))) pts)]
          (conj (apply vector
                       (sort (fn [pt1 pt2]
                               (> (angle pt pt1) (angle pt pt2))) npts))
                pt))))


(defn convex-hull
  "Calculates the convex hull given a set of points in the cartesian plane.
   Uses the Graham scan algorithm. Performance is O(n lg n) time."
  [pts]
  (when (>= (count pts) 3)
    (let [start (apply vector
                       (reverse (subvec pts (- (count pts) 3) (count pts))))
          other (reverse (subvec pts 0 (- (count pts) 3)))]
      (letfn [(popstack [stk pt]
                        (if (not (> (cross-product
                                     (last (pop stk)) (last stk) pt) 0))
                          stk (recur (pop stk) pt)))
              (scan [res pt]
                    (conj (popstack res pt) pt))]
        (reduce scan start other)))))


(defn elim-points
  "Prepare data with Aki-Toussaint heuristics before passing to convex hull function.
   Performance is O(n) time regardless of the convex hull algorithm used."
  [pts]
  (when (>= (count pts) 4)
    (letfn [(find-quad
             [[[x1 y1 :as pt1] [x3 y3 :as pt3]
               [x2 y2 :as pt2] [x4 y4 :as pt4] :as quad] [xc yc :as cpt]]
             [(if (> x1 xc) cpt pt1) (if (> y3 yc) cpt pt3)
              (if (< x2 xc) cpt pt2) (if (< y4 yc) cpt pt4)])]
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
        pts))))




(defn small-test
  "Some simple tests"
  []
  ;; descending order of angle from point with lowest y then x val - [1 2]
  (let [pt-cols
        [[[7 8] [5 5] [6 4] [1 2] [2 9] [2 5] [4 7]] ;; 4 points
         [[7 8] [1 2] [2 9] [2 5] [4 7]] ;; 3 points
         [[3 9] [7 6] [9 9] [2 1] [4 2] [0 8] [1 3] [5 7] [6 0] [4 9] [6 0]]]] ;; 7 points
    (doseq [col (map (fn [pts]
                       (println "calculating convex hull for" (count pts) "points ...")
                       (let [cvxhull (time (convex-hull (presort-points pts)))]
                         (print
                          (format "%s - " (apply vector (take 10 cvxhull))))
                         (println (count cvxhull) "points")))
                     pt-cols)])))


(defn large-test
  "Test on large random data sets"
  [pts heurp]
  (time
   (do
     (when heurp (println "heuristic elimination of points ..."))
     (let [epts (if heurp (time (elim-points pts)) pts)]
       (println "sorting" (count epts) "points ...")
       (let [spts (time (presort-points epts))]
         (println "calculating convex hull for" (count epts) "points ...")
         (let [cvxhull (time (convex-hull spts))]
           (print (apply vector (take 5 cvxhull)) "- ")
           (println (count cvxhull) "points")))))))


;; run tests
(let [cnt (long 1E6)]
  (println "\n\ngenerating" cnt "random points ...")
  (let [rpts (time (apply vector (map (fn [n] [(rand-int (inc cnt))
                                               (rand-int (inc cnt))])
                                      (range 0 cnt))))]
    (large-test rpts false)
    (large-test rpts true)))
