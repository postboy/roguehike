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
(def status-message (ref "You're standing at the foot of the mountain."))
(def cur-height (ref 0))
(def cur-stamina (ref max-stamina))
(def canvas-cols (ref 0))
(def canvas-rows (ref 0))
(def screen (ref nil))

; player will be in center of the canvas, so move everything accordingly
(defn screen-to-world [screen-x screen-y]
  (let [center-x (quot @canvas-cols 2)
        center-y (quot @canvas-rows 2)
        ; modular arithmetics to wrap around the mountain map          
        corrected-world-x (mod (+ (- @player-x center-x) screen-x) world-cols)
        corrected-world-y (mod (+ (- @player-y center-y) screen-y) world-rows)]
    [corrected-world-x corrected-world-y]))

; calculate the new screen coordinates after moving dir from current position
(defn calc-screen-coords [dir]
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

; does bounds checking via map and ensures the player doesn't walk through
; solid objects, so a player might not actually end up moving
(defn walkable? [x y]
  (let [dest (get-in world-map [x y])]
    (and (some? dest) (walkable-object? dest))))

(defmulti handle-command
  (fn [command _] command))

(defmethod handle-command nil [_ _]
  nil)

(defmethod handle-command :rest-turn [_ _]
  (dosync
   (ref-set cur-stamina (min max-stamina (+ @cur-stamina stamina-from-rest)))
   (if (= @cur-stamina max-stamina)
     (ref-set status-message "You're fully rested.")
     (ref-set status-message "You rest for a while."))))

(defmethod handle-command :move [_ dir]
  (dosync
   (let [[x y] (apply screen-to-world (calc-screen-coords dir))]
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
               (ref-set cur-height new-height)
               (ref-set cur-stamina (- @cur-stamina step-cost))
               (ref-set status-message "You walk."))))))))

; Get a key from the user and return what command they want (if any).
; The returned value is a vector of [command-type data], where data is any
; extra metadata that might be needed (like the direction for a :move command).
(defn parse-input []
  (let [k (s/get-key-blocking @screen)]
    (case k
      \q [:quit nil]
      (\5 \r) [:rest-turn nil]
      (\4 \h) [:move :left]
      (\2 \j) [:move :down]
      (\8 \k) [:move :up]
      (\6 \l) [:move :right]
      (\7 \y) [:move :up-left]
      (\9 \u) [:move :up-right]
      (\1 \b) [:move :down-left]
      (\3 \n) [:move :down-right]
      [nil nil])))

(defn render-screen []
  ;(println (inc @player-x) (inc @player-y))
  (dosync
   (let [status-bar-row (dec @canvas-rows)]
     ; draw the world
     (doseq [x (range @canvas-cols)
             y (range status-bar-row)]
       (s/put-string @screen x y (get-in world-map (screen-to-world x y))))
     ; draw the player in center of the canvas
     (let [center-x (quot @canvas-cols 2)
           center-y (quot @canvas-rows 2)]
       (s/put-string @screen center-x center-y "i")
       (s/move-cursor @screen center-x center-y))
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
  (let [[command data] (parse-input)]
    (if (= command :quit)
      (s/stop @screen)
      (do
        (handle-command command data)
        (recur)))))

(defn -main [& args]
  ; first argument is terminal type: auto/swing/text/unix/cygwin
  (create-screen (keyword (or (first args) "auto")) handle-resize)
  (game-loop))
