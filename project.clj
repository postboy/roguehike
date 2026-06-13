(defproject roguehike "0.3"
  :description "A minimalistic roguelike hiking game. Go climb a mountain here!"
  :url "https://github.com/postboy/roguehike"
  :license {:name "MIT/X11"}
  :dependencies [[org.clojure/clojure "1.11.0"]
                 [clojure-lanterna "0.9.7"]]
  :profiles {:uberjar {:aot :all}}
  :main roguehike.core)
