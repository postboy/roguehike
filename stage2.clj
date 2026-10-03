(ns roguehike.core
(:require[lanterna.screen :as s]
[roul.random :as rr]
[clojure.math :as math]
[clojure.edn :as edn])
(:gen-class))

(def a ref-set)

(def map-symbols[[\space 150]
[\. 20][\, 15][\` 15]
[\* 40]
[\" 5]
[\o 5]
[\w 5]
[\t 5]

[\0 5][\O 5]
[\W 5]
[\T 5]
[\@ 5]
[\= 1]])

(defn obstacle?[square](not(#{\space\.\,\`\*\"\o\w\t}square)))

(def world-size 150)
(def summit-coord(quot world-size 2))
(def max-altitude(quot(+ world-size world-size)4))
(def max-energy 100)

(def world-map(vec(for[_(range world-size)]
(vec(for[_(range world-size)]
(rr/rand-nth-weighted map-symbols))))))


(defn get-altitude[x y]
(max 0(- max-altitude


(max 0(dec(math/round(math/sqrt(+(math/pow(- x summit-coord)2)
(math/pow(- y summit-coord)2)))))))))

(def player-x(ref summit-coord))
(def player-y(ref(- world-size 2)))
(def render-center-x(ref@player-x))
(def render-center-y(ref@player-y))
(def render-delta-x(ref 0))
(def render-delta-y(ref 0))
(def status-message(ref"You're standing at foot of the mountain."))
(def cur-altitude(ref(get-altitude@player-x@player-y)))
(def cur-energy(ref max-energy))
(def canvas-cols(ref 0))
(def canvas-rows(ref 0))
(def b(ref nil))

(defn recenter[]
(dosync
(a render-center-x@player-x)
(a render-delta-x 0)
(a render-center-y@player-y)
(a render-delta-y 0)))

(defn rest-turn[]
(let[location(if(=@cur-altitude max-altitude)" on top of the mountain""")]
(dosync
(a cur-energy(min max-energy(+@cur-energy 5)))
(if(=@cur-energy max-energy)
(a status-message(str"You're fully rested"location"."))
(a status-message(str"You rest for a while"location"."))))))

(defn move[shift clamber]
(dosync
(let[[x y](mapv +[@player-x@player-y]shift)

dest(get-in world-map[(mod x world-size)(mod y world-size)])]
(if(and(obstacle? dest)(not clamber))
(a status-message"Can't walk there, only clamber: path is obstructed.")
(let[[new-delta-x new-delta-y](mapv +[@render-delta-x@render-delta-y]shift)
new-altitude(get-altitude x y)
clamber-modifier(if(obstacle? dest)6 1)
verb(if(obstacle? dest)"clamber""walk")
step-cost(cond(> new-altitude@cur-altitude)(* clamber-modifier 3)
(< new-altitude@cur-altitude)(* clamber-modifier 2)
:else(* clamber-modifier 1))]
(if(<@cur-energy step-cost)
(a status-message(str"You're too tired to "verb". You need a rest."))
(do(a player-x x)
(a player-y y)
(a render-delta-x new-delta-x)
(a render-delta-y new-delta-y)
(a cur-altitude new-altitude)
(a cur-energy(-@cur-energy step-cost))

(cond(nil?(get-in world-map[x y]))(a status-message"You are about to leave wilderness. Press q to quit.")
(<@cur-altitude max-altitude)(a status-message(str"You "verb"."))
:else(a status-message(str"You "verb" on top of the mountain."))))))))))


(defn screen-to-world[screen-x screen-y]
(let[status-bar-row(dec@canvas-rows)
canvas-center-x(quot@canvas-cols 2)
canvas-center-y(quot status-bar-row 2)

corrected-world-x(mod(+(-@render-center-x canvas-center-x)screen-x)world-size)
corrected-world-y(mod(+(-@render-center-y canvas-center-y)screen-y)world-size)]
[corrected-world-x corrected-world-y]))

(defn render-screen[]

(dosync
(let[status-bar-row(dec@canvas-rows)
canvas-center-x(quot@canvas-cols 2)
canvas-center-y(quot status-bar-row 2)
shift-x(-@canvas-cols 2)
shift-y(- status-bar-row 2)]


(when(>= 0(+ canvas-center-x@render-delta-x))
(a render-center-x(-@render-center-x shift-x))
(a render-delta-x(+@render-delta-x shift-x)))
(when(<=(dec@canvas-cols)(+ canvas-center-x@render-delta-x))
(a render-center-x(+@render-center-x shift-x))
(a render-delta-x(-@render-delta-x shift-x)))

(when(>= 0(+ canvas-center-y@render-delta-y))
(a render-center-y(-@render-center-y shift-y))
(a render-delta-y(+@render-delta-y shift-y)))
(when(<=(dec status-bar-row)(+ canvas-center-y@render-delta-y))
(a render-center-y(+@render-center-y shift-y))
(a render-delta-y(-@render-delta-y shift-y)))

(doseq[x(range@canvas-cols)
y(range status-bar-row)]
(s/put-string@b x y(str(get-in world-map(screen-to-world x y))){:fg :white :bg :black}))

(s/put-string@b(+ canvas-center-x@render-delta-x)(+ canvas-center-y@render-delta-y)"i"{:fg :white :bg :black})
(s/move-cursor@b(+ canvas-center-x@render-delta-x)(+ canvas-center-y@render-delta-y))

(s/put-string@b 0 status-bar-row(apply str(repeat@canvas-cols" ")){:fg :black :bg :white})
(let[alt-width 2 

arrow-left(cond(=@cur-altitude max-altitude)"T"
(>@player-x(inc summit-coord))"<"
:else" ")
arrow-up-down(cond(=@cur-altitude max-altitude)"O"
(<@player-y(dec summit-coord))"v"
(>@player-y(inc summit-coord))"^"
:else" ")
arrow-right(cond(=@cur-altitude max-altitude)"P"
(<@player-x(dec summit-coord))">"
:else" ")


string(format(str"NRG %3d | ALT %"alt-width"d/%"alt-width"d |%s%s%s| %s")
@cur-energy@cur-altitude max-altitude arrow-left arrow-up-down arrow-right@status-message)]
(s/put-string@b 0 status-bar-row string{:fg :black :bg :white})))
(s/redraw@b)))

(defn parse-input[]
(case(s/get-key-blocking@b)
\q(do(s/stop@b)
(dosync(a b nil)))
\c(recenter)
(\r\5)(rest-turn)
(\h\4)(move[-1 0]false)
(:left\H)(move[-1 0]true)
(\j\2)(move[0 1]false)
(:down\J)(move[0 1]true)
(\k\8)(move[0 -1]false)
(:up\K)(move[0 -1]true)
(\l\6)(move[1 0]false)
(:right\L)(move[1 0]true)
(\y\7)(move[-1 -1]false)
(:home\Y)(move[-1 -1]true)
(\u\9)(move[1 -1]false)
(:page-up\U)(move[1 -1]true)
(\b\1)(move[-1 1]false)
(:end\B)(move[-1 1]true)
(\n\3)(move[1 1]false)
(:page-down\N)(move[1 1]true)
nil))

(defn game-loop[]
(render-screen)
(parse-input)
(when(some?@b)
(recur)))

(defn handle-resize[cols rows]
(dosync(a canvas-cols cols)
(a canvas-rows rows))
(recenter)

(s/redraw@b)
(render-screen))

(defn -main[& args]

(let[terminal-type(keyword(or(first args)
(if(re-matches #"Windows.*"(System/getProperty"os.name"))"auto""unix")))
options(edn/read-string(or(second args)"{}"))]
(dosync(a b(s/get-screen terminal-type options))
(s/start@b)

(s/add-resize-listener@b handle-resize)
(let[[cols rows](s/get-size@b)]
(a canvas-cols cols)
(a canvas-rows rows)))
(game-loop)))
