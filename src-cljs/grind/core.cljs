(ns grind.core
  (:require [clojure.browser.repl :as repl]))

(defn ctx []
  (.. js/document (getElementById "grind") (getContext "2d")))

(defn ^:export greet [n]
  (str "Hello " n))



(repl/connect "http://localhost:9000/repl")

(defn draw [x]
(let [c (ctx)]
  (.clearRect c 0 0 200 200)
  (set! (.-fillStyle c) "rgb(200,0,0)")
  (.fillRect c (+ 10 x) 10 100 100)))

(defn frame [t]
  ;(.log js/console t)
  (draw (* 10 (.sin js/Math (* 0.001 t))))
  (.requestAnimationFrame js/window frame))

(.requestAnimationFrame js/window frame)
