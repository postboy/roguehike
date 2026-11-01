(ns r.c
  (:require [lanterna.screen :as s]
            [roul.random :as r]
            [clojure.math :as m]
            [clojure.edn :as e])
  (:gen-class))

(def a ref-set)
(def o str)
(def t s/put-string)
(def A range)
(def J ref)
(def K get-in)

(def obstacle? #{\0 \O \W \T \@ \=})

(def world-size 150)
(def world-map (vec (for [_ (range world-size)]
                      (vec (for [_ (range world-size)]
                             (r/rand-nth-weighted
                                [[\space 150]
                                [\. 20] [\, 15] [\` 15]
                                [\* 40]
                                [\" 5]
                                [\o 5]
                                [\w 5]
                                [\t 5]

                                [\0 5] [\O 5]
                                [\W 5]
                                [\T 5]
                                [\@ 5]
                                [\= 1]]))))))

(def player-x (ref 75))
(def player-y (ref 148))
(def render-center-x (ref @player-x))
(def render-center-y (ref @player-y))
(def render-delta-x (ref 0))
(def render-delta-y (ref 0))
(def cur-altitude (ref 3))
(def cur-energy (ref 100))
(def canvas-cols (ref 0))
(def canvas-rows (ref 0))
(def screen (ref nil))

(defn recenter []
  (dosync
   (ref-set render-center-x @player-x)
   (ref-set render-delta-x 0)
   (ref-set render-center-y @player-y)
   (ref-set render-delta-y 0)))

(defn move [shift only-clamber]
  (dosync
   (let [[x y] (mapv + [@player-x @player-y] shift)
         ; modular arithmetics to wrap around the map
         dest (get-in world-map [(mod x world-size) (mod y world-size)])]
     (if (not (and (obstacle? dest) (= 0 only-clamber)))
       (let [[new-delta-x new-delta-y] (mapv + [@render-delta-x @render-delta-y] shift)
             ; must be in sync with arrows to summit
             new-altitude (max 0 (- 75
                                 ; distance to top
                                 ; decrement here is required for in-game top to be an area, not a single square
                                 (max 0 (dec (m/round (m/sqrt (+ (m/pow (- x 75) 2)
                                                                 (m/pow (- y 75) 2))))))))
             clamber-modifier (if (obstacle? dest) 6 1)
             step-cost (if (> new-altitude @cur-altitude) (* clamber-modifier 3)
                                                          (if (< new-altitude @cur-altitude) (* clamber-modifier 2)
                                                                                             clamber-modifier))]
         (if (>= @cur-energy step-cost)
           (do (ref-set player-x x)
               (ref-set player-y y)
               (ref-set render-delta-x new-delta-x)
               (ref-set render-delta-y new-delta-y)
               (ref-set cur-altitude new-altitude)
               (ref-set cur-energy (- @cur-energy step-cost)))))))))

(defn render-screen []
  ;(println (inc @player-x) (inc @player-y))
  (dosync
   (let [status-bar-row (dec @canvas-rows)
         canvas-center-x (quot @canvas-cols 2)
         canvas-center-y (quot status-bar-row 2)
         shift-x (- @canvas-cols 2)
         shift-y (- status-bar-row 2)]
     ; when we're stepping on the edge, we need to re-center so we can see what's over the edge
     ; we can find ourselves over the edge after resize that shrinks a window
     (when (>= 0 (+ canvas-center-x @render-delta-x))
       (ref-set render-center-x (- @render-center-x shift-x))
       (ref-set render-delta-x (+ @render-delta-x shift-x)))
     (when (<= (dec @canvas-cols) (+ canvas-center-x @render-delta-x))
       (ref-set render-center-x (+ @render-center-x shift-x))
       (ref-set render-delta-x (- @render-delta-x shift-x)))
     ; same logic plus taking status bar into account
     (when (>= 0 (+ canvas-center-y @render-delta-y))
       (ref-set render-center-y (- @render-center-y shift-y))
       (ref-set render-delta-y (+ @render-delta-y shift-y)))
     (when (<= (dec status-bar-row) (+ canvas-center-y @render-delta-y))
       (ref-set render-center-y (+ @render-center-y shift-y))
       (ref-set render-delta-y (- @render-delta-y shift-y)))
     ; draw the world
     (doseq [x (range @canvas-cols)
             y (range status-bar-row)]
       ; render center will be in center of the canvas, so move everything accordingly
       ; modular arithmetics to wrap around the map
       (s/put-string @screen x y (str (get-in world-map
         [(mod (+ (- @render-center-x (quot @canvas-cols 2)) x) world-size)
          (mod (+ (- @render-center-y (quot (dec @canvas-rows) 2)) y) world-size)]
         ))))
     ; draw the player
     (s/put-string @screen (+ canvas-center-x @render-delta-x) (+ canvas-center-y @render-delta-y) "i")
     (s/move-cursor @screen (+ canvas-center-x @render-delta-x) (+ canvas-center-y @render-delta-y))
     ; clear and set the status bar
     (s/put-string @screen 0 status-bar-row (apply str (repeat @canvas-cols" ")))
     (s/put-string @screen 0 status-bar-row
     ; "NRG 100 | ALT 50/50 | ^ | ", so status message should be shorter than 55 symbols to
     ; fit in 80 symbols of standard terminal
     ; 2 is deliberate hardcode because maximum status message length depends on this
                             (format (str "NRG %3d | ALT %2d/%2d |%s%s%s")
                                     @cur-energy @cur-altitude 75
                                     ; inc/dec to be in sync with get-altitude
                                     (if (= @cur-altitude 75) "T"
                                                              (if (> @player-x 76) "<"
                                                                                   " "))
                                     (if (= @cur-altitude 75) "O"
                                                              (if (< @player-y 74) "v"
                                                                                   (if (> @player-y 76) "^"
                                                                                                        " ")))
                                     (if (= @cur-altitude 75) "P"
                                                              (if (< @player-x 74) ">"
                                                                                   " "))))))
   (s/redraw @screen))

(defn -main []
  ; Windows can't live without Swing, but on *nix it's better to use standard terminal
  (dosync (ref-set screen (s/get-screen))
  (s/start @screen)
  ; for some reason, this works better than setting :resize-listener argument to get-screen
  (s/add-resize-listener @screen (fn [cols rows]
                                   (dosync (ref-set canvas-cols cols)
                                           (ref-set canvas-rows rows))
                                   (recenter)
                                   ; for some reason, (redraw) inside (render-screen) is not enough
                                   (s/redraw @screen)
                                   (render-screen)))
  (let [[cols rows] (s/get-size @screen)]
    (ref-set canvas-cols cols)
    (ref-set canvas-rows rows)))
  (loop []
    (render-screen)
    (case (s/get-key-blocking @screen)
      \q (do (s/stop @screen)
        (dosync (ref-set screen nil))) ; hacky way to quit
      \r (let [location (if (= @cur-altitude 75) " on top of the mountain""")]
                (dosync
                  (ref-set cur-energy (min 100 (+ @cur-energy 5)))))
      \h (move [-1 0] 0) ; left
      \H (move [-1 0] 1)
      \j (move [0 1] 0) ; down
      \J (move [0 1] 1)
      \k (move [0 -1] 0) ; up
      \K (move [0 -1] 1)
      \l (move [1 0] 0) ; right
      \L (move [1 0] 1)
      \y (move [-1 -1] 0) ; up-left
      \Y (move [-1 -1] 1)
      \u (move [1 -1] 0) ; up-right
      \U (move [1 -1] 1)
      \b (move [-1 1] 0) ; down-left
      \B (move [-1 1] 1)
      \n (move [1 1] 0) ; down-right
      \N (move [1 1] 1)
      nil)
    (if (some? @screen) ; hacky way to quit
        (recur))))
