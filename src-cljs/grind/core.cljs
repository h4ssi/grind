(ns grind.core
  (:require [clojure.browser.repl :as repl]))

(defn ctx []
  (.. js/document (getElementById "grind") (getContext "2d")))

(defn ^:export greet [n]
  (str "Hello " n))



(repl/connect "http://localhost:9000/repl")

(let [c (ctx)]
  (set! (.-fillStyle c) "rgb(200,0,0)")
  (.fillRect c 10 10 100 100))
