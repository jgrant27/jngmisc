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



;; A molecule viewer using Penumbra/OpenGL
;;
;; keys
;;   m   - toggle molecule random motion
;;   f   - toggle fullscreen
;;   esc - exit
;;
;; controls
;;   left button down and move rotates 

(set! *warn-on-reflection* true)

(ns i27.molecule-viewer
  (:use 
   [penumbra opengl geometry])
  (:require 
   [penumbra.app :as app]
   [penumbra.text :as text]
   [clojure.contrib.generic.math-functions :as math]))

;; Element defs
(def elements
     {:colors
      '{H [0.8 0.8 0.8 1]
        O [0.8 0.1 0.0 1]
        C [0.2 0.2 0.2 1]}
      :sizes
      '{H 0.7
        O 1.0
        C 1.2}})

;; Some sample molecule defs
(def molecules
     {:water
      '[[O  0.000 0.000 0.000]
        [H -0.900 0.000 0.000]
        [H  0.000 1.000 0.000]]
      :ethanol
      '[[C -0.426  -0.115  -0.147]
        [O -0.599   1.244  -0.481]
        [H -0.750  -0.738  -0.981]
        [H -1.022  -0.351   0.735]
        [H -1.642   1.434  -0.689]
        [C  1.047  -0.383   0.147]
        [H  1.370   0.240   0.981]
        [H  1.642  -0.147  -0.735]
        [H  1.180  -1.434   0.405]]
      :benzene
      '[[C -0.750  0.000 0.000]
        [C  0.750  0.000 0.000]
        [C  0.750  0.000 0.000]
        [C -0.375  0.750 0.000]
        [C  0.375  0.750 0.000]
        [C -0.375 -0.750 0.000]
        [C  0.375 -0.750 0.000]
        ]
      :methane
      '[[C  0.000  0.000  0.000]
        [H -0.500  0.500 -0.500]
        [H  0.500  0.500 -0.500]
        [H  0.500 -0.500 -0.500]
        [H -0.500 -0.500 -0.500]]})

(defn sphere-vertices
  [d lod]
  (for [theta (range 0 361 (/ 360 lod))]
    (for [phi (range -90 91 (/ 180 (/ lod 2)))]
      (cartesian [theta phi d]))))

(defn sphere-geometry [d lod]
  (doseq [arcs (partition 2 1 (sphere-vertices d lod))]
    (draw-quad-strip
     (doseq [[a b] (map list (first arcs) (second arcs))]
       (apply normal a) (apply vertex a)
       (apply normal b) (apply vertex b)))))

(defn draw-atom [element x y z]
  (push-matrix
   (material :front :ambient-and-diffuse (get (:colors elements) element))
   (translate x y z)
   (sphere-geometry (get (:sizes elements) element) 40.0)))

(defn draw-molecule [mol]
  (doseq [a mol]
          (draw-atom (nth a 0) (nth a 1) (nth a 2) (nth a 3))))
                     
(defn reshape [[x y w h] state]
  (viewport 0 0 w h)
  (gl-matrix-mode :projection)
  (load-identity)
  (let [h (/ h w)]
    (gl-frustum -1 1 (- h) h 9 50))
  (gl-matrix-mode :modelview)
  (load-identity)
  (gl-translate 0 0 -40)
  state)

(defn key-press [key state]
  (cond
   (= key :escape) (System/exit 0)
   (= key "f") (do (app/fullscreen! (not (:fullscreen state)))
                   (assoc state :fullscreen (not (:fullscreen state))))
   (= key "m") (assoc state :motion (not (:motion state)))))

(defn mouse-drag [[dx dy] _ button state]
  (assoc state 
    :rotx (+ (:rotx state) dy )
    :roty (+ (:roty state) dx )
    :light-phi (+ (* dy (:one-degree state)) (:light-phi state))
    :light-theta (+ (* dx (:one-degree state)) (:light-theta state))))

(defn lim-between [val bot top]
  (max bot (min top val)))

(defn rand-interval [bot top]
  (+ (* (- top bot) (/ (rand 100000) 100000.0)) bot))

(defn update [[delta time] state]
  (merge
   state
   
   {:counter (+ 1 (:counter state))}

   (let [interval 5.0]
     (when (> (- time (:last-update state)) interval)
       (println (format "%f FPS" (/ (:counter state) interval)))
       {:counter 0 :last-update time}))
   
   (if (:motion state)
     (let [rotx-vel
           (lim-between (+ (:rotx-vel state) (rand-interval -0.1 0.1)) -2.0 2.0)
           roty-vel
           (lim-between (+ (:roty-vel state) (rand-interval -0.1 0.1)) -2.0 2.0)
           rotz-vel
           (lim-between (+ (:rotz-vel state) (rand-interval -0.1 0.1)) -2.0 2.0)]
       {:rotx (+ (:rotx state) rotx-vel) :rotx-vel rotx-vel
        :roty (+ (:roty state) roty-vel) :roty-vel roty-vel
        :rotz (+ (:rotz state) rotz-vel) :rotz-vel rotz-vel
        :light-phi (+ (* rotx-vel (:one-degree state)) (:light-phi state))
        :light-theta (+ (* rotx-vel (:one-degree state)) (:light-theta state))}))))

(defn display [[delta time] state]
  (clear)
  (rotate (:rotx state) 1 0 0)
  (rotate (:roty state) 0 1 0)
  (rotate (:rotz state) 0 0 1)
  (light 0 :position [(* (:light-dist state) (Math/cos (:light-theta state))
                         (Math/sin (:light-phi state)))
                      (* (:light-dist state) (Math/sin (:light-theta state))
                         (Math/sin (:light-phi state)))
                      (* (:light-dist state) (Math/cos (:light-phi state)))
                      0])
  (light 0 :diffuse [(:light-r state) (:light-g state) (:light-b state) 1])
  (call-display-list (:molecule state))
  (app/repaint!))

(defn init [state]
  (app/resizable! true)
  (app/vsync! true)
  (app/title! "Molecule Viewer")
  (enable :lighting)
  (enable :light0)
  (enable :depth-test)  
  (assoc state 
    :molecule (create-display-list 
               (draw-molecule (:ethanol molecules)))))

(defn start []
  (app/start*
   {:display display :reshape reshape :update update
    :key-press key-press :mouse-drag mouse-drag :init init} 
   {:fullscreen false
    :motion false
    :spin-speed 5.0
    :slices 40
    :model-type 'solid
    :curr-mol nil
    :rotx 0.0
    :roty 0.0
    :rotz 0.0
    :rotx-vel 0.0
    :roty-vel 0.0
    :rotz-vel 0.0
    :show-light-source nil
    :one-degree 0.0174
    :light-dist 5
    :light-theta 0.0
    :light-phi 0.0
    :light-r 0.8
    :light-g 0.8
    :light-b 0.8
    :last-update 0.0
    :counter 0}))


