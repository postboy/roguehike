(ns r.c
  (:require [lanterna.screen :as s]
            [roul.random :as r]
            [clojure.math :as m]
            [clojure.edn :as e])
  (:gen-class))

(def a ref-set)
(def o str)

(defn obstacle? [square] (#{\0 \O \W \T \@ \=} square))

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
(def status-message (ref "You're standing at foot of the mountain."))
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

(defn move [shift clamber]
  (dosync
   (let [[x y] (mapv + [@player-x @player-y] shift)
         ; modular arithmetics to wrap around the map
         dest (get-in world-map [(mod x world-size) (mod y world-size)])]
     (if (and (obstacle? dest) (= 0 clamber))
       (ref-set status-message"Can't walk there, only clamber: path is obstructed.")
       (let [[new-delta-x new-delta-y] (mapv + [@render-delta-x @render-delta-y] shift)
             ; must be in sync with arrows to summit
             new-altitude (max 0 (- 75
                                 ; distance to top
                                 ; decrement here is required for in-game top to be an area, not a single square
                                 (max 0 (dec (m/round (m/sqrt (+ (m/pow (- x 75) 2)
                                                                    (m/pow (- y 75) 2))))))))
             clamber-modifier (if (obstacle? dest) 6 1)
             verb (if (= clamber-modifier 6) "clamber""walk")
             step-cost (cond (> new-altitude @cur-altitude) (* clamber-modifier 3)
                             (< new-altitude @cur-altitude) (* clamber-modifier 2)
                             :else (* clamber-modifier 1))]
         (if (< @cur-energy step-cost)
           (ref-set status-message (str "You're too tired to "verb". You need a rest."))
           (do (ref-set player-x x)
               (ref-set player-y y)
               (ref-set render-delta-x new-delta-x)
               (ref-set render-delta-y new-delta-y)
               (ref-set cur-altitude new-altitude)
               (ref-set cur-energy (- @cur-energy step-cost))
               ; warn about being outside of the map but allow to go there anyway
               (cond (nil? (get-in world-map [x y])) (ref-set status-message"You are about to leave wilderness. Press q to quit.")
                     (< @cur-altitude 75) (ref-set status-message (str "You "verb"."))
                     :else (ref-set status-message (str "You "verb" on top of the mountain."))))))))))

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
         )) {:fg :white :bg :black}))
     ; draw the player
     (s/put-string @screen (+ canvas-center-x @render-delta-x) (+ canvas-center-y @render-delta-y) "i" {:fg :white :bg :black})
     (s/move-cursor @screen (+ canvas-center-x @render-delta-x) (+ canvas-center-y @render-delta-y))
     ; clear and set the status bar
     (s/put-string @screen 0 status-bar-row (apply str (repeat @canvas-cols" ")) {:fg :black :bg :white})
     (s/put-string @screen 0 status-bar-row
     ; "NRG 100 | ALT 50/50 | ^ | ", so status message should be shorter than 55 symbols to
     ; fit in 80 symbols of standard terminal
     ; 2 is deliberate hardcode because maximum status message length depends on this
                             (format (str "NRG %3d | ALT %2d/%2d |%s%s%s| %s")
                                     @cur-energy @cur-altitude 75
                                     ; inc/dec to be in sync with get-altitude
                                     (cond (= @cur-altitude 75) "T"
                                           (> @player-x 76) "<"
                                           :else " ")
                                     (cond (= @cur-altitude 75) "O"
                                           (< @player-y 74) "v"
                                           (> @player-y 76) "^"
                                           :else " ")
                                     (cond (= @cur-altitude 75) "P"
                                           (< @player-x 74) ">"
                                           :else " ")
                                     @status-message)
                             {:fg :black :bg :white})))
   (s/redraw @screen))

(defn -main [& args]
  ; Windows can't live without Swing, but on *nix it's better to use standard terminal
  (dosync (ref-set screen (s/get-screen (keyword (or (first args)
                                                     (if (re-matches #"Windows.*" (System/getProperty"os.name")) "auto""unix")))
                                        (e/read-string (or (second args) "{}"))))
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
      \c (recenter)
      (\r \5) (let [location (if (= @cur-altitude 75) " on top of the mountain""")]
                (dosync
                  (ref-set cur-energy (min 100 (+ @cur-energy 5)))
                  (if (= @cur-energy 100)
                    (ref-set status-message (str "You're fully rested"location"."))
                    (ref-set status-message (str "You rest for a while"location".")))))
      (\h \4) (move [-1 0] 0) ; left
      (:left \H) (move [-1 0] 1)
      (\j \2) (move [0 1] 0) ; down
      (:down \J) (move [0 1] 1)
      (\k \8) (move [0 -1] 0) ; up
      (:up \K) (move [0 -1] 1)
      (\l \6) (move [1 0] 0) ; right
      (:right \L) (move [1 0] 1)
      (\y \7) (move [-1 -1] 0) ; up-left
      (:home \Y) (move [-1 -1] 1)
      (\u \9) (move [1 -1] 0) ; up-right
      (:page-up \U) (move [1 -1] 1)
      (\b \1) (move [-1 1] 0) ; down-left
      (:end \B) (move [-1 1] 1)
      (\n \3) (move [1 1] 0) ; down-right
      (:page-down \N) (move [1 1] 1)
      nil)
    (when (some? @screen) ; hacky way to quit
        (recur))))
