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


;;;;
;;;; Pong!
;;;;
;;;; Justin Grant
;;;; 2009/09/12

(set! *warn-on-reflection* true)

(ns i27.games.pong
  (:gen-class)
  (:import (java.awt Color Toolkit Font GraphicsEnvironment Graphics2D Point)
           (java.awt.image BufferStrategy BufferedImage)
           (java.awt.event ActionListener MouseMotionListener KeyListener
                           MouseEvent KeyEvent)
           (javax.swing JFrame Timer WindowConstants)))

(defstruct ball :h :w :x :y :sx :sy)
(defn new-ball [& [h w x y sx sy]] (atom (struct ball h w x y sx sy)))
(defn set-ball-size [b h w] (swap! b assoc :h h :w w))
(defn set-ball-speed [b sx sy] (swap! b assoc :sx sx :sy sy))
(defn set-ball-position [b x y] (swap! b assoc :x x :y y))

(defstruct paddle :h :w :x :y)
(defn new-paddle [& [h w x y]] (atom (struct paddle h w x y)))
(defn set-paddle-size [p h w] (swap! p assoc :h h :w w))
(defn set-paddle-position [p x y] (swap! p assoc :x x :y y))

(defstruct game :h :w :timer :score :started :my)
(defn new-game [& [h w timer score started my]]
  (atom (struct game h w timer score started my)))
(defn set-game-size [g h w] (swap! g assoc :h h :w w))
(defn set-game-timer [g t] (swap! g assoc :timer t))
(defn set-game-score [g s] (swap! g assoc :score s))
(defn set-game-mouse-y [g my] (swap! g assoc :my my))
(defn stop-game [g]
  (swap! g assoc :started false) (let [#^Timer t (@g :timer)] (.stop t)))
(defn start-game [g]
  (swap! g assoc :started true) (let [#^Timer t (@g :timer)] (.start t)))
(defn reset-game [g b p c]
  (set-ball-size b (* (@g :h) 0.0335) (* (@g :h) 0.0335))
  (set-ball-position b
                     (- (/ (@g :w) 2) (/ (@b :w) 2))
                     (- (/ (@g :h) 2) (/ (@b :h) 2)))
  (set-ball-speed b 15 15)
  (set-paddle-size p (* (@b :h) 5) (@b :w))
  (set-paddle-position p 35 (- (/ (@g :h) 2) (/ (@p :h) 2)))
  (set-paddle-size c (@p :h) (@p :w))
  (set-paddle-position c (- (@g :w) (@p :x) (@p :w)) (@p :y))
  (set-game-score g 0))

(defn pong-frame [g b p c f1 f2]
  (proxy [JFrame ActionListener MouseMotionListener KeyListener] []
    (paint [grf]
           (let [#^JFrame me this
                 #^BufferStrategy bs (.getBufferStrategy me)
                 #^Graphics2D gr (if (not= nil bs) (. bs getDrawGraphics) nil)]
             (when (not= nil gr)
               (do
                 (.setColor gr Color/BLACK)
                 (.fillRect gr 0 0 (@g :w) (@g :h))
                 (.setColor gr Color/WHITE)
                 (.setFont gr f1)
                 (.drawString gr (str "SCORE " (@g :score)) 20 20)
                 (.fillRect gr (@p :x) (@p :y) (@p :w) (@p :h))
                 (.fillRect gr (@c :x) (@c :y) (@c :w) (@c :h))
                 (if (@g :started)
                   (.fillRect gr (@b :x) (@b :y) (@b :w) (@b :h))
                   (do
                     (.setFont gr f2)
                     (.drawString gr "PONG!"
                                  (- (/ (@g :w) 2) 46) (- (/ (@g :h) 2) 16))
                     (.setFont gr f1)
                     (.drawString gr "PRESS 'S' TO START, 'Q' TO QUIT"
                                  (- (/ (@g :w) 2) 200) (+ (/ (@g :h) 2) 30))))
                 (. gr dispose)
                 (. bs show)))))

    (mouseMoved [#^MouseEvent e]
                (set-game-mouse-y g (.getY e))
                (if (> (+ (@g :my) (/ (@p :h) 2)) (@g :h))
                  (set-game-mouse-y g (- (@g :h) (/ (@p :h) 2))))
                (if (< (@g :my) (/ (@p :h) 2))
                  (set-game-mouse-y g (/ (@p :h) 2)))
                (set-paddle-position p (@p :x) (- (@g :my) (/ (@p :h) 2)))
                (let [#^JFrame me this] (.repaint me)))

    (mouseDragged [e])

    (keyPressed [#^KeyEvent e]
                (when (and (not (@g :started)) (= (. e getKeyChar) \s))
                  (reset-game g b p c) (start-game g))
                (when (= (. e getKeyChar) \q) (.dispose this))
                )

    (keyReleased [e])

    (keyTyped [e])

    (actionPerformed [e]
                     ;; update ball position
                     (set-ball-position
                      b (+ (@b :x) (@b :sx)) (+ (@b :y) (@b :sy)))
                     ;; update ball y direction
                     (when (or (<= (@b :y) 0) (>= (+ (@b :y) (@b :h)) (@g :h)))
                       (set-ball-speed b (@b :sx) (* -1 (@b :sy))))
                     ;; check if player returns ball
                     (when (and (<= (@b :x) (+ (@p :x) (@p :w)))
                                (>= (+ (@b :y) (@b :h)) (@p :y))
                                (<= (@b :y) (+ (@p :y) (@p :h)))
                                (> (@b :x) (@p :x)))
                       (set-ball-speed b (* -1 (@b :sx)) (@b :sy))
                       (set-game-score g (inc (@g :score)))
                       (set-ball-speed b (+ 1 (@b :sx)) (@b :sy))) ; game gets faster
                     ;; check when computer returns ball
                     (when (and (>= (+ (@b :x) (@b :w)) (@c :x))
                                (>= (+ (@b :y) (@b :h)) (@c :y))
                                (<= (@b :y) (+ (@c :y) (@p :h)))
                                (< (@b :x) (+ (@c :x) (@p :w))))
                       (set-ball-speed b (* -1 (@b :sx)) (@b :sy)))
                     ;; computer 'AI'
                     (if (< (@b :sx) 0)
                       (when (not (= (+ (@c :y) (/ (@p :h) 2)) (/ (@g :h) 2)))
                         (if (> (+ (@c :y) (/ (@p :h) 2)) (/ (@g :h) 2))
                           (set-paddle-position
                            c (@c :x) (- (@c :y) (* -1 (@b :sx))))
                           (set-paddle-position
                            c (@c :x) (+ (@c :y) (* -1 (@b :sx))))))
                       (if (<= (+ (@b :y) (/ (@b :h) 2)) (+ (@c :y) (/ (@p :h) 2)))
                         (set-paddle-position c (@c :x) (- (@c :y) (@b :sx)))
                         (set-paddle-position c (@c :x) (+ (@c :y) (@b :sx)))))
                     ;; computer paddle bound checks
                     (when (< (@c :y) 0) (set-paddle-position c (@c :x) 0))
                     (when (> (+ (@c :y) (@p :h)) (@g :h))
                       (set-paddle-position c (@c :x) (- (@g :h) (@p :h))))
                     ;; check game over
                     (when (or (< (+ (@b :x) (@b :w)) 0)
                               (> (+ (@b :x) (@b :w)) (@g :w)))
                       (set-paddle-position p (@p :x)
                                            (- (/ (@g :h) 2) (/ (@p :h) 2)))
                       (stop-game g))
                     (let [#^JFrame me this]
                       (.repaint me)
                       (.setDefaultCloseOperation me WindowConstants/DISPOSE_ON_CLOSE)))))

(defn -main []
  (let [tk (. Toolkit getDefaultToolkit)
        cur (. tk createCustomCursor
               (BufferedImage. 3 3 BufferedImage/TYPE_INT_ARGB)
               (Point. 0 0) "null")
        ge (GraphicsEnvironment/getLocalGraphicsEnvironment)
        gd (. ge getDefaultScreenDevice)
        thegame (new-game (.. tk getScreenSize height)
                          (.. tk getScreenSize width))
        theball (new-ball)
        theplayer (new-paddle)
        thecomputer (new-paddle)
        #^JFrame screen (pong-frame
                         thegame theball theplayer thecomputer
                         (Font. "Courier New" Font/BOLD 24)
                         (Font. "Courier New" Font/BOLD 44))]
    (set-game-timer thegame (Timer. 20 screen))
    (if (not (. screen isDisplayable)) (. screen setUndecorated true))
    (.setVisible screen true)
    (. (.getContentPane screen) setBackground Color/BLACK)
    (. (.getContentPane screen) setIgnoreRepaint true)
    (doto screen
      (.setResizable false)
      (.setBackground Color/BLACK) (.setIgnoreRepaint true)
      (.addMouseMotionListener screen) (.addKeyListener screen))
    (. gd setFullScreenWindow screen)
    (. screen setCursor cur)
    (. screen createBufferStrategy 2)
    (reset-game thegame theball theplayer thecomputer)))


