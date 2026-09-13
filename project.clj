(defproject roguehike "0.4"
  :description "A minimalistic roguelike hiking game. Go climb a mountain here!"
  :url "https://github.com/postboy/roguehike"
  :license {:name "MIT/X11"}
  :dependencies [[org.clojure/clojure "1.12.6"]
                 [clojure-lanterna "0.9.7"]
                 [roul "0.2.0"]]
  :profiles {:uberjar {:aot :all}}
  :main roguehike.core)
