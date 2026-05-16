(ns roguehike.core
  (:gen-class)
  (:require [lanterna.screen :as s]
            [clojure.math :as math]))

(def map-symbols (vec (concat (repeat 30 " ")
                              (repeat 10 ".")
                              (repeat 8 "*")
                              ["o" "O" "0" "w" "W" "t" "T"])))

(def walkable-object? #{" " "." "*" "o" "w" "t"})

(def world-cols 100)
(def world-rows 100)
(def summit-x (quot world-cols 2))
(def summit-y (quot world-rows 2))
(def max-height (quot (+ world-cols world-rows) 4))
(def max-stamina 100)
(def step-up-cost 3)
(def step-down-cost 2)
(def step-straight-cost 1)
(def stamina-from-rest 7)
(def world-map
  (vec (for [_ (range world-rows)]
         (vec (for [_ (range world-cols)]
                (rand-nth map-symbols))))))

(def player-x (ref 0))
(def player-y (ref 0))
(def render-center-x (ref 0))
(def render-center-y (ref 0))
(def status-message (ref "You're standing at the foot of the mountain."))
(def cur-height (ref 0))
(def cur-stamina (ref max-stamina))
(def canvas-cols (ref 0))
(def canvas-rows (ref 0))
(def screen (ref nil))

; calculate coordinates shift after moving dir from current position
(defn coords-shift [dir]
  (case dir
    :left       [-1 0]
    :right      [1 0]
    :up         [0 -1]
    :down       [0 1]
    :up-left    [-1 -1]
    :up-right   [1 -1]
    :down-left  [-1 1]
    :down-right [1 1]))

; render center will be in center of the canvas, so move everything accordingly
(defn screen-to-world [screen-x screen-y]
  (let [canvas-center-x (quot @canvas-cols 2)
        canvas-center-y (quot @canvas-rows 2)
        ; modular arithmetics to wrap around the map          
        corrected-world-x (mod (+ (- @render-center-x canvas-center-x) screen-x) world-cols)
        corrected-world-y (mod (+ (- @render-center-y canvas-center-y) screen-y) world-rows)]
    [corrected-world-x corrected-world-y]))

; does bounds checking via map and ensures the player doesn't walk through
; solid objects, so a player might not actually end up moving
(defn walkable? [x y]
  (let [dest (get-in world-map [x y])]
    (and (some? dest) (walkable-object? dest))))

(defn in-world-bounds? [x y]
  (and (>= x 0)
       (< x world-cols)
       (>= y 0)
       (< y world-rows)))

(defn rest-turn []
  (dosync
   (ref-set cur-stamina (min max-stamina (+ @cur-stamina stamina-from-rest)))
   (if (= @cur-stamina max-stamina)
     (ref-set status-message "You're fully rested.")
     (ref-set status-message "You rest for a while."))))

(defn move [dir]
  (dosync
   (let [[x y] (mapv + [@player-x @player-y] (coords-shift dir))]
     (if (not (in-world-bounds? x y))
       (ref-set status-message "You are about to leave wilderness. Press q to quit.")
       (if (not (walkable? x y))
         (ref-set status-message "You cannot walk there: path is obstructed.")
         (let [old-height @cur-height
               new-height (max 0 (- max-height
                                  ; distance to summit
                                    (max 0 (dec (math/round (math/sqrt (+ (math/pow (- x summit-x) 2)
                                                                          (math/pow (- y summit-y) 2))))))))
               step-cost (if (> new-height old-height)
                           step-up-cost
                           (if (< new-height old-height) step-down-cost step-straight-cost))]
           (if (< @cur-stamina step-cost)
             (ref-set status-message "You're too tired to walk. You need a rest.")
             (do (ref-set player-x x)
                 (ref-set player-y y)
                 (ref-set render-center-x x)
                 (ref-set render-center-y y)
                 (ref-set cur-height new-height)
                 (ref-set cur-stamina (- @cur-stamina step-cost))
                 (ref-set status-message "You walk.")))))))))

; get a key from the user and execute their command
(defn parse-input []
  (let [k (s/get-key-blocking @screen)]
    (case k
      \q (do (s/stop @screen)
             ; hacky way to quit
             (dosync (ref-set screen nil)))
      (\5 \r) (rest-turn)
      (\4 \h) (move :left)
      (\2 \j) (move :down)
      (\8 \k) (move :up)
      (\6 \l) (move :right)
      (\7 \y) (move :up-left)
      (\9 \u) (move :up-right)
      (\1 \b) (move :down-left)
      (\3 \n) (move :down-right)
      nil)))

(defn render-screen []
  ;(println (inc @player-x) (inc @player-y))
  (dosync
   (let [status-bar-row (dec @canvas-rows)]
     ; draw the world
     (doseq [x (range @canvas-cols)
             y (range status-bar-row)]
       (s/put-string @screen x y (get-in world-map (screen-to-world x y))))
     ; draw the player in center of the canvas
     (let [canvas-center-x (quot @canvas-cols 2)
           canvas-center-y (quot @canvas-rows 2)]
       (s/put-string @screen canvas-center-x canvas-center-y "i")
       (s/move-cursor @screen canvas-center-x canvas-center-y))
     ; draw the status bar
     (s/put-string @screen 0 status-bar-row (apply str (repeat @canvas-cols " ")))
     ; insert at the end of status bar
     (let [st-width (count (str max-stamina))
           he-width (count (str max-height))
           string (format (str "| Stamina: %" st-width "d/%" st-width "d | Height: %" he-width "d/%" he-width "d | %s")
                          @cur-stamina max-stamina @cur-height max-height @status-message)]
       (s/put-string @screen 0 status-bar-row string)))
   (s/redraw @screen)))

(defn handle-resize [cols rows]
  (dosync (ref-set canvas-cols cols)
          (ref-set canvas-rows rows))
  ; for some reason, (redraw) inside (render-screen) is not enough
  (s/redraw @screen)
  ; we need to re-render the screen
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
  ; first argument is terminal type: auto/swing/text/unix/cygwin
  (create-screen (keyword (or (first args) "auto")) handle-resize)
  (game-loop))
