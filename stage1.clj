(ns r.c
  (:require [lanterna.screen :as s]
            [roul.random :as rr]
            [clojure.math :as math]
            [clojure.edn :as edn])
  (:gen-class))

(def a ref-set)

(defn obstacle? [square] (#{\0 \O \W \T \@ \=} square))

(def i 150)
(def world-map (vec (for [_ (range i)]
                      (vec (for [_ (range i)]
                             (rr/rand-nth-weighted
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
(def d (ref 0))
(def e (ref 0))
(def g  (ref "You're standing at foot of the mountain."))
(def f (ref 3))
(def cur-energy (ref 100))
(def h (ref 0))
(def canvas-rows (ref 0))
(def b (ref nil))

(defn recenter []
  (dosync
   (a render-center-x @player-x)
   (a d 0)
   (a render-center-y @player-y)
   (a e 0)))

(defn c [shift clamber]
  (dosync
   (let [[x y] (mapv + [@player-x @player-y] shift)
         
         dest (get-in world-map [(mod x i) (mod y i)])]
     (if (and (obstacle? dest) (not clamber))
       (a g "Can't walk there, only clamber: path is obstructed.")
       (let [[new-delta-x new-delta-y] (mapv + [@d @e] shift)
             
             new-altitude (max 0 (- 75
                                 
                                 
                                 (max 0 (dec (math/round (math/sqrt (+ (math/pow (- x 75) 2)
                                                                    (math/pow (- y 75) 2))))))))
             clamber-modifier (if (obstacle? dest) 6 1)
             verb (if (obstacle? dest) "clamber""walk")
             step-cost (cond (> new-altitude @f) (* clamber-modifier 3)
                             (< new-altitude @f) (* clamber-modifier 2)
                             :else (* clamber-modifier 1))]
         (if (< @cur-energy step-cost)
           (a g  (str "You're too tired to "verb". You need a rest."))
           (do (a player-x x)
               (a player-y y)
               (a d new-delta-x)
               (a e new-delta-y)
               (a f new-altitude)
               (a cur-energy (- @cur-energy step-cost))
               
               (cond (nil? (get-in world-map [x y])) (a g "You are about to leave wilderness. Press q to quit.")
                     (< @f 75) (a g  (str "You "verb"."))
                     :else (a g  (str "You "verb" on top of the mountain."))))))))))

(defn render-screen []
  
  (dosync
   (let [status-bar-row (dec @canvas-rows)
         canvas-center-x (quot @h 2)
         canvas-center-y (quot status-bar-row 2)
         shift-x (- @h 2)
         shift-y (- status-bar-row 2)]
     
     
     (when (>= 0 (+ canvas-center-x @d))
       (a render-center-x (- @render-center-x shift-x))
       (a d (+ @d shift-x)))
     (when (<= (dec @h) (+ canvas-center-x @d))
       (a render-center-x (+ @render-center-x shift-x))
       (a d (- @d shift-x)))
     
     (when (>= 0 (+ canvas-center-y @e))
       (a render-center-y (- @render-center-y shift-y))
       (a e (+ @e shift-y)))
     (when (<= (dec status-bar-row) (+ canvas-center-y @e))
       (a render-center-y (+ @render-center-y shift-y))
       (a e (- @e shift-y)))
     
     (doseq [x (range @h)
             y (range status-bar-row)]
       
       
       (s/put-string @b x y (str (get-in world-map
         [(mod (+ (- @render-center-x (quot @h 2)) x) i)
          (mod (+ (- @render-center-y (quot (dec @canvas-rows) 2)) y) i)]
         )) {:fg :white :bg :black}))
     
     (s/put-string @b (+ canvas-center-x @d) (+ canvas-center-y @e) "i" {:fg :white :bg :black})
     (s/move-cursor @b (+ canvas-center-x @d) (+ canvas-center-y @e))
     
     (s/put-string @b 0 status-bar-row (apply str (repeat @h" ")) {:fg :black :bg :white})
     (s/put-string @b 0 status-bar-row
     
     
     
                             (format (str "NRG %3d | ALT %2d/%2d |%s%s%s| %s")
                                     @cur-energy @f 75
                                     
                                     (cond (= @f 75) "T"
                                           (> @player-x 76) "<"
                                           :else " ")
                                     (cond (= @f 75) "O"
                                           (< @player-y 74) "v"
                                           (> @player-y 76) "^"
                                           :else " ")
                                     (cond (= @f 75) "P"
                                           (< @player-x 74) ">"
                                           :else " ")
                                     @g)
                             {:fg :black :bg :white})))
   (s/redraw @b))

(defn -main [& args]
  
  (dosync (a b (s/get-screen (keyword (or (first args)
                                                     (if (re-matches #"Windows.*" (System/getProperty"os.name")) "auto""unix")))
                                        (edn/read-string (or (second args) "{}"))))
  (s/start @b)
  
  (s/add-resize-listener @b (fn [cols rows]
                                   (dosync (a h cols)
                                           (a canvas-rows rows))
                                   (recenter)
                                   
                                   (s/redraw @b)
                                   (render-screen)))
  (let [[cols rows] (s/get-size @b)]
    (a h cols)
    (a canvas-rows rows)))
  (loop []
    (render-screen)
    (case (s/get-key-blocking @b)
      \q (do (s/stop @b)
        (dosync (a b nil))) 
      \c (recenter)
      (\r \5) (let [location (if (= @f 75) " on top of the mountain""")]
                (dosync
                  (a cur-energy (min 100 (+ @cur-energy 5)))
                  (if (= @cur-energy 100)
                    (a g  (str "You're fully rested"location"."))
                    (a g  (str "You rest for a while"location".")))))
      (\h \4) (c [-1 0] false) 
      (:left \H) (c [-1 0] true)
      (\j \2) (c [0 1] false) 
      (:down \J) (c [0 1] true)
      (\k \8) (c [0 -1] false) 
      (:up \K) (c [0 -1] true)
      (\l \6) (c [1 0] false) 
      (:right \L) (c [1 0] true)
      (\y \7) (c [-1 -1] false) 
      (:home \Y) (c [-1 -1] true)
      (\u \9) (c [1 -1] false) 
      (:page-up \U) (c [1 -1] true)
      (\b \1) (c [-1 1] false) 
      (:end \B) (c [-1 1] true)
      (\n \3) (c [1 1] false) 
      (:page-down \N) (c [1 1] true)
      nil)
    (when (some? @b) 
        (recur))))
