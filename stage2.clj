(ns r.c
(:require[lanterna.screen :as s]
[roul.random :as rr]
[clojure.math :as p]
[clojure.edn :as edn])
(:gen-class))

(def a ref-set)
(def o str)

(defn obstacle?[square](#{\0\O\W\T\@\=}square))

(def i 150)
(def world-map(vec(for[_(range i)]
(vec(for[_(range i)]
(rr/rand-nth-weighted
[[\space 150]
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
[\= 1]]))))))

(def m(ref 75))
(def n(ref 148))
(def k(ref@m))
(def l(ref@n))
(def d(ref 0))
(def e(ref 0))
(def g(ref"You're standing at foot of the mountain."))
(def f(ref 3))
(def j(ref 100))
(def h(ref 0))
(def canvas-rows(ref 0))
(def b(ref nil))

(defn recenter[]
(dosync
(a k@m)
(a d 0)
(a l@n)
(a e 0)))

(defn c[shift clamber]
(dosync
(let[[x y](mapv +[@m@n]shift)

dest(get-in world-map[(mod x i)(mod y i)])]
(if(and(obstacle? dest)(= 0 clamber))
(a g "Can't walk there, only clamber: path is obstructed.")
(let[[new-delta-x new-delta-y](mapv +[@d@e]shift)

new-altitude(max 0(- 75


(max 0(dec(p/round(p/sqrt(+(p/pow(- x 75)2)
(p/pow(- y 75)2))))))))
clamber-modifier(if(obstacle? dest)6 1)
verb(if(obstacle? dest)"clamber""walk")
step-cost(cond(> new-altitude@f)(* clamber-modifier 3)
(< new-altitude@f)(* clamber-modifier 2)
:else(* clamber-modifier 1))]
(if(<@j step-cost)
(a g(o"You're too tired to "verb". You need a rest."))
(do(a m x)
(a n y)
(a d new-delta-x)
(a e new-delta-y)
(a f new-altitude)
(a j(-@j step-cost))

(cond(nil?(get-in world-map[x y]))(a g "You are about to leave wilderness. Press q to quit.")
(<@f 75)(a g(o"You "verb"."))
:else(a g(o"You "verb" on top of the mountain."))))))))))

(defn render-screen[]

(dosync
(let[z(dec@canvas-rows)
canvas-center-x(quot@h 2)
canvas-center-y(quot z 2)
x(-@h 2)
y(- z 2)]


(when(>= 0(+ canvas-center-x@d))
(a k(-@k x))
(a d(+@d x)))
(when(<=(dec@h)(+ canvas-center-x@d))
(a k(+@k x))
(a d(-@d x)))

(when(>= 0(+ canvas-center-y@e))
(a l(-@l y))
(a e(+@e y)))
(when(<=(dec z)(+ canvas-center-y@e))
(a l(+@l y))
(a e(-@e y)))

(doseq[x(range@h)
y(range z)]


(s/put-string@b x y(o(get-in world-map
[(mod(+(-@k(quot@h 2))x)i)
(mod(+(-@l(quot(dec@canvas-rows)2))y)i)]
)){:fg :white :bg :black}))

(s/put-string@b(+ canvas-center-x@d)(+ canvas-center-y@e)"i"{:fg :white :bg :black})
(s/move-cursor@b(+ canvas-center-x@d)(+ canvas-center-y@e))

(s/put-string@b 0 z(apply o(repeat@h" ")){:fg :black :bg :white})
(s/put-string@b 0 z



(format(o"NRG %3d | ALT %2d/%2d |%s%s%s| %s")
@j@f 75

(cond(=@f 75)"T"
(>@m 76)"<"
:else" ")
(cond(=@f 75)"O"
(<@n 74)"v"
(>@n 76)"^"
:else" ")
(cond(=@f 75)"P"
(<@m 74)">"
:else" ")
@g)
{:fg :black :bg :white})))
(s/redraw@b))

(defn -main[& args]

(dosync(a b(s/get-screen(keyword(or(first args)
(if(re-matches #"Windows.*"(System/getProperty"os.name"))"auto""unix")))
(edn/read-string(or(second args)"{}"))))
(s/start@b)

(s/add-resize-listener@b(fn[x y]
(dosync(a h x)
(a canvas-rows y))
(recenter)

(s/redraw@b)
(render-screen)))
(let[[x y](s/get-size@b)]
(a h x)
(a canvas-rows y)))
(loop[]
(render-screen)
(case(s/get-key-blocking@b)
\q(do(s/stop@b)
(dosync(a b nil)))
\c(recenter)
(\r\5)(let[z(if(=@f 75)" on top of the mountain""")]
(dosync
(a j(min 100(+@j 5)))
(if(=@j 100)
(a g(o"You're fully rested"z"."))
(a g(o"You rest for a while"z".")))))
(\h\4)(c[-1 0]0)
(:left\H)(c[-1 0]1)
(\j\2)(c[0 1]0)
(:down\J)(c[0 1]1)
(\k\8)(c[0 -1]0)
(:up\K)(c[0 -1]1)
(\l\6)(c[1 0]0)
(:right\L)(c[1 0]1)
(\y\7)(c[-1 -1]0)
(:home\Y)(c[-1 -1]1)
(\u\9)(c[1 -1]0)
(:page-up\U)(c[1 -1]1)
(\b\1)(c[-1 1]0)
(:end\B)(c[-1 1]1)
(\n\3)(c[1 1]0)
(:page-down\N)(c[1 1]1)
nil)
(when(some?@b)
(recur))))
