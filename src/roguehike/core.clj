(ns roguehike.core
  (:require [lanterna.screen :as s]
            [clojure.math :as math])
  (:gen-class))

(def map-symbols (vec (concat (repeat 150 " ")
                              (repeat 20 ".") (repeat 15 ",") (repeat 15 "`")
                              (repeat 40 "*")
                              (repeat 5 "\"")
                              (repeat 5 "o")
                              (repeat 5 "w")
                              (repeat 5 "t")

                              (repeat 5 "0") (repeat 5 "O")
                              (repeat 5 "W")
                              (repeat 5 "T")
                              (repeat 5 "@")
                              ["="])))

(defn obstacle? [square] (not (#{" " "." "," "`" "*" "\"" "o" "w" "t"} square)))

(def world-cols 100)
(def world-rows 100)
(def summit-x (quot world-cols 2))
(def summit-y (quot world-rows 2))
(def max-altitude (quot (+ world-cols world-rows) 4))
(def max-stamina 100)
(def step-up-cost 3)
(def step-down-cost 2)
(def step-straight-cost 1)
(def stamina-from-rest 7)
(def world-map
  (vec (for [_ (range world-rows)]
         (vec (for [_ (range world-cols)]
                (rand-nth map-symbols))))))

(def player-x (ref summit-x))
(def player-y (ref (- world-rows 2)))
(def render-delta-x (ref 0))
(def render-delta-y (ref 0))
(def render-center-x (ref @player-x))
(def render-center-y (ref @player-y))
(def status-message (ref "You're standing at foot of the mountain."))

(defn get-altitude [x y]
  (max 0 (- max-altitude
            ; distance to top
            ; decrement here is required for in-game top to be an area, not a single square
            (max 0 (dec (math/round (math/sqrt (+ (math/pow (- x summit-x) 2)
                                                  (math/pow (- y summit-y) 2)))))))))

(def cur-altitude (ref (get-altitude @player-x @player-y)))
(def cur-stamina (ref max-stamina))
(def canvas-cols (ref 0))
(def canvas-rows (ref 0))
(def screen (ref nil))

; render center will be in center of the canvas, so move everything accordingly
(defn screen-to-world [screen-x screen-y]
  (let [status-bar-row (dec @canvas-rows)
        canvas-center-x (quot @canvas-cols 2)
        canvas-center-y (quot status-bar-row 2)
        ; modular arithmetics to wrap around the map
        corrected-world-x (mod (+ (- @render-center-x canvas-center-x) screen-x) world-cols)
        corrected-world-y (mod (+ (- @render-center-y canvas-center-y) screen-y) world-rows)]
    [corrected-world-x corrected-world-y]))

(defn rest-turn []
  (dosync
   (ref-set cur-stamina (min max-stamina (+ @cur-stamina stamina-from-rest)))
   (if (= @cur-stamina max-stamina)
     (if (< @cur-altitude max-altitude)
       (ref-set status-message "You're fully rested.")
       (ref-set status-message "You're fully rested on top of the mountain."))
     (if (< @cur-altitude max-altitude)
       (ref-set status-message "You rest for a while.")
       (ref-set status-message "You rest for a while on top of the mountain.")))))

(defn move [shift]
  (dosync
   (let [[x y] (mapv + [@player-x @player-y] shift)
         ; modular arithmetics to wrap around the map
         dest (get-in world-map [(mod x world-cols) (mod y world-rows)])]
     (if (obstacle? dest)
       (ref-set status-message "You cannot walk there: path is obstructed.")
       (let [[new-delta-x new-delta-y] (mapv + [@render-delta-x @render-delta-y] shift)
             old-altitude @cur-altitude
             new-altitude (get-altitude x y)
             step-cost (if (> new-altitude old-altitude)
                         step-up-cost
                         (if (< new-altitude old-altitude) step-down-cost step-straight-cost))]
         (if (< @cur-stamina step-cost)
           (ref-set status-message "You're too tired to walk. You need a rest.")
           (do (ref-set player-x x)
               (ref-set player-y y)
               (ref-set render-delta-x new-delta-x)
               (ref-set render-delta-y new-delta-y)
               (ref-set cur-altitude new-altitude)
               (ref-set cur-stamina (- @cur-stamina step-cost))
               ; warn about being outside of the map but allow to go there anyway
               (if (nil? (get-in world-map [x y]))
                 (ref-set status-message "You are about to leave wilderness. Press q to quit.")
                 (if (< @cur-altitude max-altitude)
                   (ref-set status-message "You walk.")
                   (ref-set status-message "You walk on top of the mountain."))))))))))

(defn recenter []
  (dosync
   (ref-set render-center-x @player-x)
   (ref-set render-delta-x 0)
   (ref-set render-center-y @player-y)
   (ref-set render-delta-y 0)))

(defn parse-input []
  (let [k (s/get-key-blocking @screen)]
    (case k
      \q (do (s/stop @screen)
             ; hacky way to quit
             (dosync (ref-set screen nil)))
      (\5 \r) (rest-turn)
      (\4 \h) (move [-1 0]) ; left
      (\2 \j) (move [0 1]) ; down
      (\8 \k) (move [0 -1]) ; up
      (\6 \l) (move [1 0]) ; right
      (\7 \y) (move [-1 -1]) ; up-left
      (\9 \u) (move [1 -1]) ; up-right
      (\1 \b) (move [-1 1]) ; down-left
      (\3 \n) (move [1 1]) ; down-right
      \c (recenter)
      nil)))

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
       (s/put-string @screen x y (get-in world-map (screen-to-world x y)) {:fg :white :bg :black}))
     ; draw the player
     (s/put-string @screen (+ canvas-center-x @render-delta-x) (+ canvas-center-y @render-delta-y) "i" {:fg :white :bg :black})
     (s/move-cursor @screen (+ canvas-center-x @render-delta-x) (+ canvas-center-y @render-delta-y))
     ; clear and set the status bar
     (s/put-string @screen 0 status-bar-row (apply str (repeat @canvas-cols " ")) {:fg :black :bg :white})
     (let [alt-width (count (str max-altitude))
           arrow-left (if (> @player-x summit-x) "<" " ")
           arrow-up-down (if (< @player-y summit-y) "v" (if (> @player-y summit-y) "^" " "))
           arrow-right (if (< @player-x summit-x) ">" " ")
           string (format (str "STA %3d | ALT %" alt-width "d/%" alt-width "d |%s%s%s| %s")
                          @cur-stamina @cur-altitude max-altitude arrow-left arrow-up-down arrow-right @status-message)]
       (s/put-string @screen 0 status-bar-row string {:fg :black :bg :white})))
   (s/redraw @screen)))

(defn handle-resize [cols rows]
  (dosync (ref-set canvas-cols cols)
          (ref-set canvas-rows rows))
  (recenter)
  ; for some reason, (redraw) inside (render-screen) is not enough
  (s/redraw @screen)
  (render-screen))

(defn create-screen [terminal-type resized-fn]
  (dosync (ref-set screen (s/get-screen terminal-type)))
  (s/start @screen)
  ; for some reason, this works better than setting :resize-listener argument
  ; to get-screen
  (s/add-resize-listener @screen resized-fn)
  (let [[cols rows] (s/get-size @screen)]
    (dosync (ref-set canvas-cols cols)
            (ref-set canvas-rows rows))))

(defn game-loop []
  (render-screen)
  (parse-input)
  ; hacky way to quit
  (when (some? @screen)
    (recur)))

(defn -main [& args]
  ; Windows can't live without Swing, but on *nix it's better to use standard terminal
  (let [terminal-type (keyword (or (first args)
                                   (if (re-matches #"Windows.*" (System/getProperty "os.name"))
                                     "auto"
                                     "unix")))]
    (create-screen terminal-type handle-resize)
    (game-loop)))
