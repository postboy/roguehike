(ns roguehike.core
  (:gen-class)
  (:require [lanterna.screen :as s]))

; World/screen state
; map instead of vector seems excessive but probably will be useful in the
; future
(def world (ref {}))
(def world-cols 100)
(def world-rows 100)
(def player-x (ref 0))
(def player-y (ref 0))
(def canvas-cols (ref 0))
(def canvas-rows (ref 0))
(def screen (ref nil))

(def map-symbols [" " " " " " " " " " " " " " " " " " " " " " " " " " " " " " " " " " " " " " " " " " " " " " " " " " " " " " " " " " " "
                  "." "." "." "." "." "." "." "." "." "."
                  "o" "O" "w" "W" "t" "T"])

(def walkable-object? #{" " "." "o" "w" "t"})

; Rendering
; player will be in center of the canvas, so move everything accordingly
(defn screen-to-world
  [screen-x screen-y]
  (let [center-x (quot @canvas-cols 2)
        center-y (quot @canvas-rows 2)
        ; modular arithmetics to wrap around the mountain map          
        corrected-world-x (mod (+ (- @player-x center-x) screen-x) world-cols)
        corrected-world-y (mod (+ (- @player-y center-y) screen-y) world-rows)]
    [corrected-world-x corrected-world-y]))

; World creation
(defn create-world []
  ((fn [world col row]
     (if (= row world-rows)
       world
       (let [symbol (rand-nth map-symbols)]
         (cond
           ; go to next row
           (= col world-cols) (recur world 0 (inc row))
           ; add square
           :else (recur (-> world
                            (assoc [col row] (str symbol)))
                        (inc col) row)))))
   {} 0 0))

(defn create-initial-world []
  (dosync (ref-set world (create-world))))

; Input/command handling
(defn calc-screen-coords
  "Calculate the new screen coordinates after moving dir from current position."
  [dir]
  (let [center-x (quot @canvas-cols 2)
        center-y (quot @canvas-rows 2)]
    (case dir
      :left       [(dec center-x) center-y]
      :right      [(inc center-x) center-y]
      :up         [center-x (dec center-y)]
      :down       [center-x (inc center-y)]
      :up-left    [(dec center-x) (dec center-y)]
      :up-right   [(inc center-x) (dec center-y)]
      :down-left  [(dec center-x) (inc center-y)]
      :down-right [(inc center-x) (inc center-y)])))

(defn parse-input
  "Get a key from the user and return what command they want (if any).
   The returned value is a vector of [command-type data], where data is any
   extra metadata that might be needed (like the direction for a :move command)."
  []
  (let [k (s/get-key-blocking @screen)]
    (case k
      \q [:quit nil]
      \4 [:move :left]
      \2 [:move :down]
      \8 [:move :up]
      \6 [:move :right]
      \7 [:move :up-left]
      \9 [:move :up-right]
      \1 [:move :down-left]
      \3 [:move :down-right]
      \h [:move :left]
      \j [:move :down]
      \k [:move :up]
      \l [:move :right]
      \y [:move :up-left]
      \u [:move :up-right]
      \b [:move :down-left]
      \n [:move :down-right]
      [nil nil])))

(defn walkable?
  "Does bounds checking via map and ensures the player doesn't walk through
   solid objects, so a player might not actually end up moving."
  [x y]
  (let [dest (@world [x y])]
    (and (some? dest) (walkable-object? dest))))

(defmulti handle-command
  (fn [command _] command))

(defmethod handle-command nil [_ _]
  nil)

(defmethod handle-command :move [_ dir]
  (dosync
   (let [[x y] (apply screen-to-world (calc-screen-coords dir))]
     (when (walkable? x y)
       (ref-set player-x x)
       (ref-set player-y y)))))

(defn render-screen
  []
  ;(println (inc @player-x) (inc @player-y))
  (dosync
   ; draw the world
   (doseq [x (range @canvas-cols)
           y (range @canvas-rows)]
     (s/put-string @screen x y (@world (screen-to-world x y))))
   ; draw the player in center of the canvas
   (let [center-x (quot @canvas-cols 2)
         center-y (quot @canvas-rows 2)]
     ; if player is on the rope then draw them differently
     (s/put-string @screen center-x center-y "i")
                   ;(if (= "|" (get-rendered-square center-x center-y)) "$" "1"))
     (s/move-cursor @screen center-x center-y)))
  (s/redraw @screen))

(defn handle-resize [cols rows]
  (dosync (ref-set canvas-cols cols)
          (ref-set canvas-rows rows))
  ; for some reason, (redraw) inside (render-screen) is not enough
  (s/redraw @screen)
  ; we need to re-render the screen
  (render-screen))

(defn create-screen
  [terminal-type resized-fn]
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
  (let [[command data] (parse-input)]
    (if (= command :quit)
      (s/stop @screen)
      (do
        (handle-command command data)
        (recur)))))

(defn -main [& args]
  ; first argument is terminal type: auto/swing/text/unix/cygwin
  (create-screen (keyword (or (first args) "auto")) handle-resize)
  (create-initial-world)
  (game-loop))
